mod casing {
pub fn to_snake_case(name: &str) -> String {
    let mut out = String::new();
    for (index, ch) in name.char_indices() {
        if index != 0 && ch.is_uppercase() {
            out.push('_');
        }
        out.push(ch.to_ascii_lowercase());
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn to_snake_case_leaves_snake_case_untouched() {
        assert_eq!(to_snake_case(""), "");
        assert_eq!(to_snake_case("a"), "a");
        assert_eq!(to_snake_case("abc"), "abc");
        assert_eq!(to_snake_case("a_bc"), "a_bc");
    }

    #[test]
    fn to_snake_case_works_for_camel_case() {
        assert_eq!(to_snake_case("Foobar"), "foobar");
        assert_eq!(to_snake_case("FooBar"), "foo_bar");
        assert_eq!(to_snake_case("ABC"), "a_b_c");
    }
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
    let title = schema
        .schema
        .metadata
        .as_ref()
        .map(|b| b.title.clone().unwrap_or_else(|| "untitled".to_string()))
        .unwrap_or_else(|| "unknown".to_string());
    write_schema(schema, out_dir, &title);
}

// use this if you want to override the auto-detected name of the object.
// very useful when creating an alias for a type-alias.
pub fn export_schema_with_title(schema: &RootSchema, out_dir: &Path, title: &str) {
    let mut schema = schema.clone();
    // set the title explicitly on the schema's metadata
    if let Some(metadata) = &mut schema.schema.metadata {
        metadata.title = Some(title.to_string());
    }
    write_schema(&schema, out_dir, title);
}

/// Writes schema to file. Overwrites existing file.
/// Panics on any error writing out the schema.
fn write_schema(schema: &RootSchema, out_dir: &Path, title: &str) {
    // first, we set the title as we wish
    let path = out_dir.join(format!("{}.json", to_snake_case(title)));
    let json = serde_json::to_string_pretty(schema).unwrap();
    write(&path, json + "\n").unwrap();
    println!("Created {}", path.to_str().unwrap());
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
        let mut json_api = JsonApi {
            contract_name: self.contract_name,
            contract_version: self.contract_version,
            idl_version: IDL_VERSION.to_string(),
            instantiate: self.instantiate,
            execute: self.execute,
            query: self.query,
            migrate: self.migrate,
            sudo: self.sudo,
            responses: self.responses,
        };

        if let Some(metadata) = &mut json_api.instantiate.schema.metadata {
            metadata.title = Some("InstantiateMsg".to_string());
        }
        if let Some(execute) = &mut json_api.execute {
            if let Some(metadata) = &mut execute.schema.metadata {
                metadata.title = Some("ExecuteMsg".to_string());
            }
        }
        if let Some(query) = &mut json_api.query {
            if let Some(metadata) = &mut query.schema.metadata {
                metadata.title = Some("QueryMsg".to_string());
            }
        }
        if let Some(migrate) = &mut json_api.migrate {
            if let Some(metadata) = &mut migrate.schema.metadata {
                metadata.title = Some("MigrateMsg".to_string());
            }
        }
        if let Some(sudo) = &mut json_api.sudo {
            if let Some(metadata) = &mut sudo.schema.metadata {
                metadata.title = Some("SudoMsg".to_string());
            }
        }

        json_api
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
        serde_json::to_string_pretty(&self).map_err(Into::into)
    }

    pub fn to_schema_files(&self) -> Result<Vec<(String, String)>, EncodeError> {
        let mut result = vec![(
            "instantiate.json".to_string(),
            serde_json::to_string_pretty(&self.instantiate)?,
        )];

        if let Some(execute) = &self.execute {
            result.push((
                "execute.json".to_string(),
                serde_json::to_string_pretty(&execute)?,
            ));
        }
        if let Some(query) = &self.execute {
            result.push((
                "query.json".to_string(),
                serde_json::to_string_pretty(&query)?,
            ));
        }
        if let Some(migrate) = &self.execute {
            result.push((
                "migrate.json".to_string(),
                serde_json::to_string_pretty(&migrate)?,
            ));
        }
        if let Some(sudo) = &self.execute {
            result.push((
                "sudo.json".to_string(),
                serde_json::to_string_pretty(&sudo)?,
            ));
        }
        if let Some(responses) = &self.responses {
            for (name, response) in responses {
                result.push((
                    format!("response_to_{}.json", name),
                    serde_json::to_string_pretty(&response)?,
                ));
            }
        }

        Ok(result)
    }

    pub fn to_writer(&self, writer: impl std::io::Write) -> Result<(), EncodeError> {
        serde_json::to_writer_pretty(writer, self).map_err(Into::into)
    }
}

#[derive(Error, Debug)]
pub enum EncodeError {
    #[error("{0}")]
    JsonError(#[from] serde_json::Error),
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn version_is_semver() {
        semver::Version::parse(IDL_VERSION).unwrap();
    }
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
        let response_schemas = Self::response_schemas_impl();

        Ok(response_schemas)
    }

    fn response_schemas_impl() -> BTreeMap<String, RootSchema>;
}

/// Combines multiple response schemas into one. Panics if there are name collisions.
/// Used internally in the implementation of [`QueryResponses`] when using `#[query_responses(nested)]`
pub fn combine_subqueries<const N: usize, T>(
    subqueries: [BTreeMap<String, RootSchema>; N],
) -> BTreeMap<String, RootSchema> {
    let sub_count = subqueries.iter().flatten().count();
    let map: BTreeMap<_, _> = subqueries.into_iter().flatten().collect();
    if map.len() != sub_count {
        panic!(
            "name collision in subqueries for {}",
            std::any::type_name::<T>()
        )
    }
    map
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
    use schemars::schema_for;

    use super::*;

    #[derive(Debug, JsonSchema)]
    #[serde(rename_all = "snake_case")]
    #[allow(dead_code)]
    pub enum GoodMsg {
        BalanceFor { account: String },
        AccountIdFor(String),
        Supply {},
        Liquidity,
        AccountCount(),
    }

    impl QueryResponses for GoodMsg {
        fn response_schemas_impl() -> BTreeMap<String, RootSchema> {
            BTreeMap::from([
                ("balance_for".to_string(), schema_for!(u128)),
                ("account_id_for".to_string(), schema_for!(u128)),
                ("supply".to_string(), schema_for!(u128)),
                ("liquidity".to_string(), schema_for!(u128)),
                ("account_count".to_string(), schema_for!(u128)),
            ])
        }
    }

    #[test]
    fn good_msg_works() {
        let response_schemas = GoodMsg::response_schemas().unwrap();
        assert_eq!(
            response_schemas,
            BTreeMap::from([
                ("balance_for".to_string(), schema_for!(u128)),
                ("account_id_for".to_string(), schema_for!(u128)),
                ("supply".to_string(), schema_for!(u128)),
                ("liquidity".to_string(), schema_for!(u128)),
                ("account_count".to_string(), schema_for!(u128))
            ])
        );
    }

    #[derive(Debug, JsonSchema)]
    #[serde(rename_all = "snake_case")]
    #[allow(dead_code)]
    pub enum EmptyMsg {}

    impl QueryResponses for EmptyMsg {
        fn response_schemas_impl() -> BTreeMap<String, RootSchema> {
            BTreeMap::from([])
        }
    }

    #[test]
    fn empty_msg_works() {
        let response_schemas = EmptyMsg::response_schemas().unwrap();
        assert_eq!(response_schemas, BTreeMap::from([]));
    }

    #[derive(Debug, JsonSchema)]
    #[serde(rename_all = "kebab-case")]
    #[allow(dead_code)]
    pub enum BadMsg {
        BalanceFor { account: String },
    }

    impl QueryResponses for BadMsg {
        fn response_schemas_impl() -> BTreeMap<String, RootSchema> {
            BTreeMap::from([("balance_for".to_string(), schema_for!(u128))])
        }
    }

    #[derive(Debug, JsonSchema)]
    #[serde(rename_all = "snake_case")]
    #[allow(dead_code)]
    pub enum ExtMsg {
        Extension {},
    }

    #[derive(Debug, JsonSchema)]
    #[serde(untagged, rename_all = "snake_case")]
    #[allow(dead_code)]
    pub enum UntaggedMsg {
        Good(GoodMsg),
        Ext(ExtMsg),
        Empty(EmptyMsg),
    }

    impl QueryResponses for UntaggedMsg {
        fn response_schemas_impl() -> BTreeMap<String, RootSchema> {
            BTreeMap::from([
                ("balance_for".to_string(), schema_for!(u128)),
                ("account_id_for".to_string(), schema_for!(u128)),
                ("supply".to_string(), schema_for!(u128)),
                ("liquidity".to_string(), schema_for!(u128)),
                ("account_count".to_string(), schema_for!(u128)),
                ("extension".to_string(), schema_for!(())),
            ])
        }
    }

    #[test]
    fn untagged_msg_works() {
        let response_schemas = UntaggedMsg::response_schemas().unwrap();
        assert_eq!(
            response_schemas,
            BTreeMap::from([
                ("balance_for".to_string(), schema_for!(u128)),
                ("account_id_for".to_string(), schema_for!(u128)),
                ("supply".to_string(), schema_for!(u128)),
                ("liquidity".to_string(), schema_for!(u128)),
                ("account_count".to_string(), schema_for!(u128)),
                ("extension".to_string(), schema_for!(())),
            ])
        );
    }
}
}
mod remove {
use std::{fs, io, path};

fn is_regular_file(path: &path::Path) -> Result<bool, io::Error> {
    Ok(path.symlink_metadata()?.is_file())
}

fn is_hidden(path: &path::Path) -> bool {
    match path.file_name() {
        Some(name) => name.to_os_string().to_string_lossy().starts_with('.'),
        None => false, // a path without filename is no .*
    }
}

fn is_json(path: &path::Path) -> bool {
    match path.file_name() {
        Some(name) => name.to_os_string().to_string_lossy().ends_with(".json"),
        None => false, // a path without filename is no *.json
    }
}

pub fn remove_schemas(schemas_dir: &path::Path) -> Result<(), io::Error> {
    let file_paths = fs::read_dir(schemas_dir)?
        .filter_map(Result::ok) // skip read errors on entries
        .map(|entry| entry.path())
        .filter(|path| is_regular_file(path).unwrap_or(false)) // skip directories and symlinks
        .filter(|path| !is_hidden(path)) // skip hidden
        .filter(|path| is_json(path)) // skip non JSON
        ;

    for file_path in file_paths {
        println!("Removing {:?} …", file_path);
        fs::remove_file(file_path)?;
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::ffi::OsStr;
    use std::path::Path;

    #[test]
    fn is_hidden_works() {
        assert!(!is_hidden(Path::new("/foo")));
        assert!(!is_hidden(Path::new("/foo/bar")));
        assert!(!is_hidden(Path::new("/foo/bar.txt")));
        assert!(!is_hidden(Path::new("~foo")));
        assert!(!is_hidden(Path::new("foo")));

        assert!(is_hidden(Path::new("/.foo")));
        assert!(is_hidden(Path::new("/foo/.bar")));
        assert!(is_hidden(Path::new("/foo/.bar.txt")));
        assert!(is_hidden(Path::new(".foo")));

        // no filename
        assert!(!is_hidden(Path::new("/")));
        assert!(!is_hidden(Path::new("")));

        // invalid UTF-8
        #[cfg(any(unix, target_os = "redox"))]
        {
            use std::os::unix::ffi::OsStrExt;
            let non_hidden = OsStr::from_bytes(&[0x66, 0x6f, 0x80, 0x6f]); // fo�o
            assert!(!is_hidden(Path::new(non_hidden)));
            let hidden = OsStr::from_bytes(&[0x2e, 0x66, 0x6f, 0x80, 0x6f]); // .fo�o
            assert!(is_hidden(Path::new(hidden)));
        }
    }

    #[test]
    fn is_json_works() {
        assert!(!is_json(Path::new("/foo")));
        assert!(!is_json(Path::new("/foo/bar")));
        assert!(!is_json(Path::new("/foo/bar.txt")));
        assert!(!is_json(Path::new("~foo")));
        assert!(!is_json(Path::new("foo")));
        assert!(!is_json(Path::new("foo.json5")));

        assert!(is_json(Path::new("/.json")));
        assert!(is_json(Path::new("/foo/.bar.json")));
        assert!(is_json(Path::new("/foo/bar.json")));
        assert!(is_json(Path::new("foo.json")));

        // no filename
        assert!(!is_json(Path::new("/")));
        assert!(!is_json(Path::new("")));

        // invalid UTF-8
        #[cfg(any(unix, target_os = "redox"))]
        {
            use std::os::unix::ffi::OsStrExt;
            let non_hidden = OsStr::from_bytes(&[0x66, 0x6f, 0x80, 0x6f]); // fo�o
            assert!(!is_json(Path::new(non_hidden)));
            let hidden = OsStr::from_bytes(&[0x66, 0x6f, 0x80, 0x6f, 0x2e, 0x6a, 0x73, 0x6f, 0x6e]); // fo�o.json
            assert!(is_json(Path::new(hidden)));
        }
    }
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
