use cosmwasm_schema::{cw_serde, QueryResponses};
use cosmwasm_std::{Binary, Uint128};

#[cw_serde]
pub struct InstantiateMsg {
    pub deposit_size: Uint128,
    pub merkletree_levels: u32,
    pub native_token_denom: String,
    pub curve: u8,
    pub vk_raw: Binary,
}

#[cw_serde]
pub enum ExecuteMsg {
    Deposit(DepositMsg),
    Withdraw(WithdrawMsg),
}

#[cw_serde]
pub struct DepositMsg {
    pub commitment: Binary,
}

#[cw_serde]
pub struct WithdrawMsg {
    pub proof_bytes: Binary,
    pub root: Binary,
    pub nullifier_hash: Binary,
    pub recipient: String,
    pub relayer: String,
    pub fee: Uint128,
    pub refund: Uint128,
}

#[cw_serde]
#[derive(QueryResponses)]
pub enum QueryMsg {
    #[returns(ConfigResponse)]
    Config {},
    #[returns(MerkleTreeInfoResponse)]
    MerkleTreeInfo {},
    #[returns(MerkleRootResponse)]
    MerkleRoot { id: u32 },
}

#[cw_serde]
pub struct ConfigResponse {
    pub native_token_denom: String,
    pub deposit_size: String,
}

#[cw_serde]
pub struct MerkleTreeInfoResponse {
    pub levels: u32,
    pub current_root_index: u32,
    pub next_index: u32,
}

#[cw_serde]
pub struct MerkleRootResponse {
    pub root: Binary,
}
