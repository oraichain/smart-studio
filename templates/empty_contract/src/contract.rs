#[cfg(not(feature = "library"))]
use cosmwasm_std::{
    entry_point, to_binary, Addr, Binary, Deps, DepsMut, Env, MessageInfo, Response, StdResult,
};

use crate::error::ContractError;
use crate::msg::{ConfigResponse, ExecuteMsg, InstantiateMsg, QueryMsg};
use crate::state::{read_config, store_config, Config};

// Note, you can use StdResult in some functions where you do not
// make use of the custom errors
#[cfg_attr(not(feature = "library"), entry_point)]
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
#[cfg_attr(not(feature = "library"), entry_point)]
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

#[cfg_attr(not(feature = "library"), entry_point)]
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
    let state = read_config(deps.storage)?;
    let current_owner = deps.api.addr_humanize(&state.owner)?;
    if current_owner.ne(&info.sender) {
        return Err(ContractError::Unauthorized {});
    }

    store_config(deps.storage, &Config { owner: deps.api.addr_canonicalize(owner.as_str())? })?;

    Ok(Response::default())
}

pub fn query_config(deps: Deps) -> StdResult<ConfigResponse> {
    let state = read_config(deps.storage)?;
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
