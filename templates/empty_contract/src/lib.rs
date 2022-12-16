#[cfg(not(feature = "library"))]
use cosmwasm_schema::{cw_serde, QueryResponses};
use cosmwasm_std::{
    entry_point, to_binary, Addr, Binary, CanonicalAddr, Deps, DepsMut, Env, MessageInfo, Response,
    StdError, StdResult, Storage,
};
use cosmwasm_storage::{singleton, singleton_read};
use thiserror::Error;

#[derive(Error, Debug)]
pub enum ContractError {
    #[error("{0}")]
    Std(#[from] StdError),

    #[error("Unauthorized")]
    Unauthorized {},
    // Add any other custom errors you like here.
    // Look at https://docs.rs/thiserror/1.0.21/thiserror/ for details.
}

#[cw_serde]
pub struct InstantiateMsg {}

#[cw_serde]
pub enum ExecuteMsg {
    ChangeOwner { owner: Addr },
}

#[cw_serde]
#[derive(QueryResponses)]
pub enum QueryMsg {
    #[returns(ConfigResponse)]
    Config {},
}

#[cw_serde]
pub struct ConfigResponse {
    pub owner: Addr,
}

#[cw_serde]
pub struct Config {
    pub owner: CanonicalAddr,
}

pub fn store_config(storage: &mut dyn Storage, config: &Config) -> StdResult<()> {
    singleton(storage, KEY_CONFIG).save(config)
}

pub fn read_config(storage: &dyn Storage) -> StdResult<Config> {
    singleton_read(storage, KEY_CONFIG).load()
}

static KEY_CONFIG: &[u8] = b"config";

// Note, you can use StdResult in some functions where you do not
// make use of the custom errors
#[entry_point]
pub fn instantiate(
    deps: DepsMut,
    _env: Env,
    info: MessageInfo,
    _: InstantiateMsg,
) -> StdResult<Response> {
    store_config(
        deps.storage,
        &Config { owner: deps.api.addr_canonicalize(info.sender.as_str())? },
    )?;
    Ok(Response::default())
}

// And declare a custom Error variant for the ones where you will want to make use of it
#[entry_point]
pub fn execute(
    deps: DepsMut,
    _env: Env,
    info: MessageInfo,
    msg: ExecuteMsg,
) -> Result<Response, ContractError> {
    match msg {
        ExecuteMsg::ChangeOwner { owner } => execute_change_owner(deps, info, owner),
    }
}

#[entry_point]
pub fn query(deps: Deps, _env: Env, msg: QueryMsg) -> StdResult<Binary> {
    match msg {
        QueryMsg::Config {} => to_binary(&query_config(deps)?),
    }
}

pub fn execute_change_owner(
    deps: DepsMut,
    info: MessageInfo,
    owner: Addr,
) -> Result<Response, ContractError> {
    let state: Config = read_config(deps.storage)?;
    let current_owner = deps.api.addr_humanize(&state.owner)?;
    if current_owner.ne(&info.sender) {
        return Err(ContractError::Unauthorized {});
    }

    store_config(deps.storage, &Config { owner: deps.api.addr_canonicalize(owner.as_str())? })?;

    Ok(Response::default())
}

pub fn query_config(deps: Deps) -> StdResult<ConfigResponse> {
    let state: Config = read_config(deps.storage)?;
    let resp = ConfigResponse { owner: deps.api.addr_humanize(&state.owner)? };

    Ok(resp)
}

#[test]
fn test_init() {
    use cosmwasm_std::testing::{mock_dependencies, mock_env, mock_info};
    let mut deps = mock_dependencies();
    // instantiate an empty contract
    let instantiate_msg = InstantiateMsg {};
    let info = mock_info(&String::from("anyone"), &[]);
    let res = instantiate(deps.as_mut(), mock_env(), info, instantiate_msg).unwrap();
    assert_eq!(0, res.messages.len());
    let query_msg = QueryMsg::Config {};
    let res: ConfigResponse =
        cosmwasm_std::from_binary(&query(deps.as_ref(), mock_env(), query_msg).unwrap()).unwrap();
    assert_eq!(res.owner, "anyone");
}
