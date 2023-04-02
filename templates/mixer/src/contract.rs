use cosmwasm_std::{
    attr, entry_point, to_binary, BankMsg, Binary, Coin, CosmosMsg, Deps, DepsMut, Env, Event,
    MessageInfo, Response, StdError, StdResult,
};

use crate::error::ContractError;
use crate::msg::{
    ConfigResponse, DepositMsg, ExecuteMsg, InstantiateMsg, MerkleRootResponse,
    MerkleTreeInfoResponse, QueryMsg, WithdrawMsg,
};

use crate::utils::{element_encoder, truncate_and_pad};
use crate::zeroes::zeroes;

use crate::state::{
    mixer_read, mixer_write, nullifier_read, nullifier_write, read_root, save_root, save_subtree,
    MerkleTree, Mixer,
};

const VK_BYTES: &[u8; 360] = include_bytes!("../../../bn254/x5/verifying_key.bin");

#[entry_point]
pub fn instantiate(
    deps: DepsMut,
    _env: Env,
    info: MessageInfo,
    msg: InstantiateMsg,
) -> Result<Response, ContractError> {
    // Validation 1. Check if the funds are sent with this message
    if !info.funds.is_empty() {
        return Err(ContractError::UnnecessaryFunds {});
    }

    // Initialize the "Mixer"
    let merkle_tree: MerkleTree =
        MerkleTree { levels: msg.merkletree_levels, current_root_index: 0, next_index: 0 };
    let native_token_denom = msg.native_token_denom;

    let deposit_size = msg.deposit_size;

    let mixer: Mixer = Mixer { native_token_denom, deposit_size, merkle_tree };
    mixer_write(deps.storage, &mixer)?;

    for i in 0..msg.merkletree_levels {
        save_subtree(deps.storage, i as u32, &zeroes(i));
    }

    save_root(deps.storage, 0_u32, &zeroes(msg.merkletree_levels));

    Ok(Response::new().add_attribute("action", "instantiate").add_attribute("owner", info.sender))
}

#[entry_point]
pub fn execute(
    deps: DepsMut,
    _env: Env,
    info: MessageInfo,
    msg: ExecuteMsg,
) -> Result<Response, ContractError> {
    match msg {
        // Deposit the "native" tokens with commitment
        ExecuteMsg::Deposit(msg) => deposit(deps, info, msg),
        // Withdraw either "native" tokens
        ExecuteMsg::Withdraw(msg) => withdraw(deps, info, msg),
    }
}

pub fn deposit(
    deps: DepsMut,
    info: MessageInfo,
    msg: DepositMsg,
) -> Result<Response, ContractError> {
    let mut mixer = mixer_read(deps.storage)?;

    let sent_tokens: Vec<Coin> =
        info.funds.into_iter().filter(|x| x.denom == mixer.native_token_denom).collect();
    if sent_tokens.is_empty() || sent_tokens[0].amount < mixer.deposit_size {
        return Err(ContractError::InsufficientFunds {});
    }

    // Handle the "deposit"
    let commitment_bytes = element_encoder(msg.commitment.as_slice());

    // insert commitment into merke_tree
    let inserted_index = mixer.merkle_tree.insert(deps.api, commitment_bytes, deps.storage)?;
    mixer_write(deps.storage, &mixer)?;
    return Ok(Response::new().add_event(Event::new("mixer-deposit").add_attributes(vec![
        attr("action", "deposit"),
        attr("inserted_index", inserted_index.to_string()),
        attr("commitment", msg.commitment.to_base64()),
    ])));
}

pub fn withdraw(
    deps: DepsMut,
    info: MessageInfo,
    msg: WithdrawMsg,
) -> Result<Response, ContractError> {
    let recipient = msg.recipient;
    let relayer = msg.relayer;
    let fee = msg.fee;
    let refund = msg.refund;
    let root_bytes = element_encoder(msg.root.as_slice());
    let nullifier_hash_bytes = element_encoder(msg.nullifier_hash.as_slice());
    let proof_bytes_vec = msg.proof_bytes.to_vec();

    let mixer = mixer_read(deps.storage)?;

    // Validations
    let sent_funds = info.funds;
    if !refund.is_zero() && (sent_funds.len() != 1 || sent_funds[0].amount != refund) {
        return Err(ContractError::Std(StdError::GenericErr {
            msg: "Sent insufficent refund".to_string(),
        }));
    }

    let merkle_tree = mixer.merkle_tree;
    if !merkle_tree.is_known_root(root_bytes, deps.storage) {
        return Err(ContractError::Std(StdError::GenericErr {
            msg: "Root is not known".to_string(),
        }));
    }

    if nullifier_read(deps.storage, &nullifier_hash_bytes) {
        return Err(ContractError::Std(StdError::GenericErr {
            msg: "Nullifier is known".to_string(),
        }));
    }

    // Format the public input bytes
    let recipient_bytes = truncate_and_pad(recipient.as_bytes());
    let relayer_bytes = truncate_and_pad(relayer.as_bytes());

    // limit arbitrary data bytes to 96 bytes
    let mut arbitrary_data_bytes = Vec::new();
    arbitrary_data_bytes.extend_from_slice(&recipient_bytes);
    arbitrary_data_bytes.extend_from_slice(&relayer_bytes);
    arbitrary_data_bytes.extend_from_slice(&fee.to_le_bytes());
    arbitrary_data_bytes.extend_from_slice(&refund.to_le_bytes());

    let arbitrary_input =
        deps.api.curve_hash(&arbitrary_data_bytes, 1).map_err(|_| ContractError::HashError)?;

    // Join the public input bytes
    let mut bytes = Vec::new();
    bytes.extend_from_slice(&msg.nullifier_hash);
    bytes.extend_from_slice(&msg.root);
    bytes.extend_from_slice(&arbitrary_input);

    // Verify the proof
    let result = deps
        .api
        .groth16_verify(&bytes, &proof_bytes_vec, VK_BYTES, 1)
        .map_err(|_| ContractError::VerifyError)?;

    if !result {
        return Err(ContractError::Std(StdError::GenericErr {
            msg: "Invalid withdraw proof".to_string(),
        }));
    }

    // Set used nullifier to true after successful verification
    nullifier_write(deps.storage, &element_encoder(msg.nullifier_hash.as_slice()));

    // Send the funds
    let mut msgs: Vec<CosmosMsg> = vec![];

    // Send the funds to "recipient"
    let amt_to_recipient = match mixer.deposit_size.checked_sub(fee) {
        Ok(v) => v,
        Err(e) => return Err(ContractError::Std(StdError::GenericErr { msg: e.to_string() })),
    };

    if !amt_to_recipient.is_zero() {
        msgs.push(CosmosMsg::Bank(BankMsg::Send {
            to_address: recipient.clone(),
            amount: vec![Coin {
                denom: mixer.native_token_denom.clone(),
                amount: amt_to_recipient,
            }],
        }));
    }
    if !fee.is_zero() {
        msgs.push(CosmosMsg::Bank(BankMsg::Send {
            to_address: relayer,
            amount: vec![Coin { denom: mixer.native_token_denom, amount: fee }],
        }));
    }

    if !refund.is_zero() {
        msgs.push(CosmosMsg::Bank(BankMsg::Send {
            to_address: recipient.clone(),
            amount: sent_funds,
        }));
    }

    Ok(Response::new().add_messages(msgs).add_event(Event::new("mixer-withdraw").add_attributes(
        vec![
            attr("action", "withdraw"),
            attr("recipient", recipient),
            attr("root", msg.root.to_base64()),
            attr("nullifier_hash", msg.nullifier_hash.to_base64()),
        ],
    )))
}

#[entry_point]
pub fn query(deps: Deps, _env: Env, msg: QueryMsg) -> StdResult<Binary> {
    match msg {
        QueryMsg::Config {} => to_binary(&get_config(deps)?),
        QueryMsg::MerkleTreeInfo {} => to_binary(&get_merkle_tree_info(deps)?),
        QueryMsg::MerkleRoot { id } => to_binary(&get_merkle_root(deps, id)?),
    }
}

fn get_config(deps: Deps) -> StdResult<ConfigResponse> {
    let mixer = mixer_read(deps.storage)?;
    let native_token_denom = mixer.native_token_denom;

    let deposit_size = mixer.deposit_size.to_string();
    Ok(ConfigResponse { native_token_denom, deposit_size })
}

fn get_merkle_tree_info(deps: Deps) -> StdResult<MerkleTreeInfoResponse> {
    let mixer = mixer_read(deps.storage)?;
    Ok(MerkleTreeInfoResponse {
        levels: mixer.merkle_tree.levels,
        current_root_index: mixer.merkle_tree.current_root_index,
        next_index: mixer.merkle_tree.next_index,
    })
}

fn get_merkle_root(deps: Deps, id: u32) -> StdResult<MerkleRootResponse> {
    let root = read_root(deps.storage, id);
    let root_binary = Binary::from(root.as_slice());
    Ok(MerkleRootResponse { root: root_binary })
}
