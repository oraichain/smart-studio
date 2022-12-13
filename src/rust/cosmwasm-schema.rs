mod casing {
pub fn to_snake_case(name: &str) -> String {
}

#[cfg(test)]
mod tests {
}
}
mod export {
//! Export schema to file

use std::fs::write;
use std::path::Path;

use schemars::schema::RootSchema;

use crate::casing::to_snake_case;

// Exports a schema, auto-generating filename based on the metadata title of the generated schema.
pub fn export_schema(schema: &RootSchema, out_dir: &Path) {
}

// use this if you want to override the auto-detected name of the object.
// very useful when creating an alias for a type-alias.
pub fn export_schema_with_title(schema: &RootSchema, out_dir: &Path, title: &str) {
}

/// Writes schema to file. Overwrites existing file.
/// Panics on any error writing out the schema.
fn write_schema(schema: &RootSchema, out_dir: &Path, title: &str) {
}
}
mod idl {
//! The Cosmwasm IDL (Interface Description Language)

use std::collections::BTreeMap;

use schemars::schema::RootSchema;
use thiserror::Error;

/// The version of the CosmWasm IDL.
///
/// Follows Semantic Versioning 2.0.0: <https://semver.org/>
// To determine if a change is breaking, assume consumers allow unknown fields and bump accordingly.
pub const IDL_VERSION: &str = "1.0.0";

/// Rust representation of a contract's API.
pub struct Api {
    pub contract_name: String,
    pub contract_version: String,
    pub instantiate: RootSchema,
    pub execute: Option<RootSchema>,
    pub query: Option<RootSchema>,
    pub migrate: Option<RootSchema>,
    pub sudo: Option<RootSchema>,
    /// A mapping of query variants to response types
    pub responses: Option<BTreeMap<String, RootSchema>>,
}

impl Api {
    pub fn render(self) -> JsonApi {
}
}

/// A JSON representation of a contract's API.
#[derive(serde::Serialize)]
pub struct JsonApi {
    contract_name: String,
    contract_version: String,
    idl_version: String,
    instantiate: RootSchema,
    execute: Option<RootSchema>,
    query: Option<RootSchema>,
    migrate: Option<RootSchema>,
    sudo: Option<RootSchema>,
    responses: Option<BTreeMap<String, RootSchema>>,
}

impl JsonApi {
    pub fn to_string(&self) -> Result<String, EncodeError> {
}

    pub fn to_schema_files(&self) -> Result<Vec<(String, String)>, EncodeError> {
}

    pub fn to_writer(&self, writer: impl std::io::Write) -> Result<(), EncodeError> {
}
}

#[derive(Error, Debug)]
pub enum EncodeError {
    #[error("{0}")]
    JsonError(#[from] serde_json::Error),
}

#[cfg(test)]
mod tests {
}
}
mod query_response {
use std::collections::{BTreeMap, BTreeSet};

use schemars::{schema::RootSchema, JsonSchema};
use thiserror::Error;

pub use cosmwasm_schema_derive::QueryResponses;

/// A trait for tying QueryMsg variants (different contract queries) to their response types.
/// This is mostly useful for the generated contracted API description when using `cargo schema`.
///
/// Using the derive macro is the preferred way of implementing this trait.
///
/// # Examples
/// ```
/// use cosmwasm_schema::QueryResponses;
/// use schemars::JsonSchema;
///
/// #[derive(JsonSchema)]
/// struct AccountInfo {
///     IcqHandle: String,
/// }
///
/// #[derive(JsonSchema, QueryResponses)]
/// enum QueryMsg {
///     #[returns(Vec<String>)]
///     Denoms {},
///     #[returns(AccountInfo)]
///     AccountInfo { account: String },
/// }
/// ```
///
/// You can compose multiple queries using `#[query_responses(nested)]`. This might be useful
/// together with `#[serde(untagged)]`. If the `nested` flag is set, no `returns` attributes
/// are necessary on the enum variants. Instead, the response types are collected from the
/// nested enums.
///
/// ```
/// # use cosmwasm_schema::QueryResponses;
/// # use schemars::JsonSchema;
/// #[derive(JsonSchema, QueryResponses)]
/// #[query_responses(nested)]
/// #[serde(untagged)]
/// enum QueryMsg {
///     MsgA(QueryA),
///     MsgB(QueryB),
/// }
///
/// #[derive(JsonSchema, QueryResponses)]
/// enum QueryA {
///     #[returns(Vec<String>)]
///     Denoms {},
/// }
///
/// #[derive(JsonSchema, QueryResponses)]
/// enum QueryB {
///     #[returns(AccountInfo)]
///     AccountInfo { account: String },
/// }
///
/// # #[derive(JsonSchema)]
/// # struct AccountInfo {
/// #     IcqHandle: String,
/// # }
/// ```
pub trait QueryResponses: JsonSchema {
    fn response_schemas() -> Result<BTreeMap<String, RootSchema>, IntegrityError> {
}

    fn response_schemas_impl() -> BTreeMap<String, RootSchema>;
}

/// Combines multiple response schemas into one. Panics if there are name collisions.
/// Used internally in the implementation of [`QueryResponses`] when using `#[query_responses(nested)]`
pub fn combine_subqueries<const N: usize, T>(
    subqueries: [BTreeMap<String, RootSchema>; N],
) -> BTreeMap<String, RootSchema> {
}

#[derive(Debug, Error, PartialEq, Eq)]
pub enum IntegrityError {
    #[error("the structure of the QueryMsg schema was unexpected")]
    InvalidQueryMsgSchema,
    #[error("external reference in schema found, but they are not supported")]
    ExternalReference { reference: String },
    #[error(
        "inconsistent queries - QueryMsg schema has {query_msg:?}, but query responses have {responses:?}"
    )]
    InconsistentQueries {
        query_msg: BTreeSet<String>,
        responses: BTreeSet<String>,
    },
}

#[cfg(test)]
mod tests {
}
}
mod remove {
use std::{fs, io, path};

fn is_regular_file(path: &path::Path) -> Result<bool, io::Error> {
}

fn is_hidden(path: &path::Path) -> bool {
}

fn is_json(path: &path::Path) -> bool {
}

pub fn remove_schemas(schemas_dir: &path::Path) -> Result<(), io::Error> {
}

#[cfg(test)]
mod tests {
}
}
mod schema_for {
/// Generates a [`RootSchema`](crate::schemars::schema::RootSchema) for the given type using default settings.
///
/// The type must implement [`JsonSchema`](crate::schemars::JsonSchema).
///
/// The schema version is strictly `draft-07`.
///
/// # Example
/// ```
/// use cosmwasm_schema::schema_for;
/// use schemars::JsonSchema;
///
/// #[derive(JsonSchema)]
/// struct MyStruct {
///     foo: i32,
/// }
///
/// let my_schema = schema_for!(MyStruct);
/// ```
#[macro_export]
macro_rules! schema_for {
    ($type:ty) => {
        $crate::schemars::gen::SchemaGenerator::new($crate::schemars::gen::SchemaSettings::draft07()).into_root_schema_for::<$type>()
    };
    ($_:expr) => {
        compile_error!("The argument to `schema_for!` is not a type.")
    };
}
}

pub use export::{export_schema, export_schema_with_title};
pub use idl::{Api, IDL_VERSION};
pub use query_response::{combine_subqueries, IntegrityError, QueryResponses};
pub use remove::remove_schemas;

// Re-exports
/// An attribute macro that annotates types with things they need to be properly (de)serialized
/// for use in CosmWasm contract messages and/or responses, and also for schema generation.
///
/// This derives things like `serde::Serialize` or `schemars::JsonSchema`, makes sure
/// variants are `snake_case` in the resulting JSON, and so forth.
///
/// # Example
/// ```
/// use cosmwasm_schema::{cw_serde, QueryResponses};
///
/// #[cw_serde]
/// pub struct InstantiateMsg {
///     owner: String,
/// }
///
/// #[cw_serde]
/// #[derive(QueryResponses)]
/// pub enum QueryMsg {
///     #[returns(Vec<String>)]
///     Denoms {},
///     #[returns(String)]
///     AccountName { account: String },
/// }
/// ```
pub use cosmwasm_schema_derive::cw_serde;
/// Generates an [`Api`](crate::Api) for the contract. The body describes the message
/// types exported in the schema and allows setting contract name and version overrides.
///
/// The only obligatory field is `instantiate` - to set the InstantiateMsg type.
///
/// # Available fields
/// See [`write_api`](crate::write_api).
///
/// # Example
/// ```
/// use cosmwasm_schema::{cw_serde, generate_api};
///
/// #[cw_serde]
/// struct InstantiateMsg;
///
/// #[cw_serde]
/// struct MigrateMsg;
///
/// let api = generate_api! {
///     name: "cw20",
///     instantiate: InstantiateMsg,
///     migrate: MigrateMsg,
/// }.render();
/// ```
pub use cosmwasm_schema_derive::generate_api;
/// Takes care of generating the interface description file for a contract. The body describes
/// the message types included and allows setting contract name and version overrides.
///
/// The only obligatory field is `instantiate` - to set the InstantiateMsg type.
///
/// # Available fields
/// - `name` - contract name, crate name by default
/// - `version` - contract version, crate version by default
/// - `instantiate` - instantiate msg type
/// - `query` - query msg type, empty by default
/// - `execute` - execute msg type, empty by default
/// - `migrate` - migrate msg type, empty by default
/// - `sudo` - sudo msg type, empty by default
///
/// # Example
/// ```
/// use cosmwasm_schema::{cw_serde, write_api};
///
/// #[cw_serde]
/// struct InstantiateMsg;
///
/// #[cw_serde]
/// struct MigrateMsg;
///
/// write_api! {
///     name: "cw20",
///     instantiate: InstantiateMsg,
///     migrate: MigrateMsg,
/// };
/// ```
pub use cosmwasm_schema_derive::write_api;

// For use in macro expansions
pub use schemars;
pub use serde;
