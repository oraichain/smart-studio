use crate::error::ContractError;
use crate::msg::{HandleMsg, InitMsg, QueryMsg};
use cosmwasm_std::{
    Binary, Env, HandleResponse, InitResponse, MessageInfo,
    StdResult, Deps, DepsMut, to_binary
};

// Note, you can use StdResult in some functions where you do not
// make use of the custom errors
pub fn init(
    _deps: DepsMut,
    _env: Env,
    _info: MessageInfo,
    _: InitMsg,
) -> StdResult<InitResponse> {
    Ok(InitResponse::default())
}

// And declare a custom Error variant for the ones where you will want to make use of it
pub fn handle(
    _: DepsMut,
    _env: Env,
    _: MessageInfo,
    _: HandleMsg,
) -> Result<HandleResponse, ContractError> {
    Ok(HandleResponse::default())
}

pub fn query(
    deps: Deps,
    _env: Env,
    msg: QueryMsg,
) -> StdResult<Binary> {
    match msg {
        QueryMsg::Get { input } => to_binary(&query_data(deps, input)?),
    }
}

fn query_data(
    deps: Deps,
    input: String,
) -> StdResult<String> { 
    Ok(String::new())
}