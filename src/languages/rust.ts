export default `

/// Serializes the given data structure as a JSON byte vector
pub fn to_binary<T>(data: &T) -> StdResult<Binary> whereT: Serialize + ?Sized

/// - pub \`messages\`: Vec<CosmosMsg<T>>
/// - pub \`attributes\`: Vec<Attribute> 
/// - pub \`data\`: Option<Binary> 
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
/// - pub \`storage\`: Storage
/// - pub \`api\`: Api 
/// - pub \`querier\`: QuerierWrapper 
pub struct Deps

/// Holds all external dependencies of the contract and is mutable.
///
/// - pub \`storage\`: Storage
/// - pub \`api\`: Api
/// - pub \`querier\`: QuerierWrapper 
pub struct DepsMut

/// Binary is a wrapper around Vec<u8> to add base64 de/serialization
/// with serde. It also adds some helper methods to help encode inline.
///
/// This is only needed as serde-json-{core,wasm} has a horrible encoding for Vec<u8>
pub struct Binary

/// - pub \`block\`: BlockInfo
/// - pub \`contract\`: ContractInfo 
pub struct Env

/// - pub \`messages\`: Vec<CosmosMsg<T>>
/// - pub \`attributes\`: Vec<Attribute> 
pub struct InitResponse

/// KV is a Key-Value pair, returned from our iterators
pub type KV

/// HumanAddr is a String type
pub struct HumanAddr

/// Creates a new Attribute.
pub fn attr<K: ToString, V: ToString>(key: K, value: V) -> Attribute

/// Structured error type for init, handle and query.
///
/// This can be serialized and passed over the Wasm/VM boundary, which allows us to use structured
/// error types in e.g. integration tests. In that process backtraces are stripped off.
///
/// The prefix "Std" means "the standard error within the standard library". This is not the only
/// result/error type in cosmwasm-std.
///
/// When new cases are added, they should describe the problem rather than what was attempted (e.g.
/// InvalidBase64 is preferred over Base64DecodingErr). In the long run this allows us to get rid of
/// the duplication in "StdError::FooErr".
///
/// Checklist for adding a new error:
/// - Add enum case
/// - Add creator function in std_error_helpers.rs
pub enum StdError

/// We assign these to integers to provide a stable API for passing over FFI (to wasm and Go)
pub enum Order 

/// Api are callbacks to system functions implemented outside of the wasm modules.
/// Currently it just supports address conversion but we could add eg. crypto functions here.
///
/// This is a trait to allow mocks in the test code. Its members have a read-only
/// reference to the Api instance to allow accessing configuration.
/// Implementations must not have mutable state, such that an instance can freely
/// be copied and shared between threads without affecting the behaviour.
/// Given an Api instance, all members should return the same value when called with the same
/// arguments. In particular this means the result must not depend in the state of the chain.
/// If you need to access chaim state, you probably want to use the Querier.
/// Side effects (such as logging) are allowed.
///
/// We can use feature flags to opt-in to non-essential methods
/// for backwards compatibility in systems that don't have them all.
pub trait Api 

/// - pub \`height\`: u64
/// - pub \`time\`: u64
/// - pub \`time_nanos\`: u64
/// - pub \`chain_id\`: String
pub struct BlockInfo 
`;
