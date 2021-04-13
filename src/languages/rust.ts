export default `

/// Serializes the given data structure as a JSON byte vector
pub fn to_binary<T>(data: &T) -> StdResult<Binary> whereT: Serialize + ?Sized

///
/// { pub \`messages\`: Vec<CosmosMsg<T>>, pub \`attributes\`: Vec<Attribute>, pub \`data\`: Option<Binary> }
pub struct HandleResponse

/// MessageInfo is sent with \`init\`, \`handle\`, and \`migrate\` calls, but not with queries.
/// It contains the essential info for authorization - identity of the call, and payment
pub struct MessageInfo

/// The return type for init, handle and query. Since the error type cannot be serialized to JSON,
/// this is only available within the contract and its unit tests.
///
/// The prefix "Std" means "the standard result within the standard library". This is not the only
/// result/error type in cosmwasm-std.
pub type StdResult

/// Holds all external dependencies of the contract and is immutable.
///
/// { pub \`storage\`: Storage, pub \`api\`: Api, pub \`querier\`: QuerierWrapper }
pub struct Deps

/// Holds all external dependencies of the contract and is mutable.
///
/// { pub \`storage\`: Storage, pub \`api\`: Api, pub \`querier\`: QuerierWrapper }
pub struct DepsMut

/// Binary is a wrapper around Vec<u8> to add base64 de/serialization
/// with serde. It also adds some helper methods to help encode inline.
///
/// This is only needed as serde-json-{core,wasm} has a horrible encoding for Vec<u8>
pub struct Binary

///
/// { pub \`block\`: BlockInfo, pub \`contract\`: ContractInfo }
pub struct Env

///
/// { pub \`messages\`: Vec<CosmosMsg<T>>, pub \`attributes\`: Vec<Attribute> }
pub struct InitResponse

`;
