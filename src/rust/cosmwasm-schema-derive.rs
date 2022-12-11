mod cw_serde {
use syn::{parse_quote, DeriveInput};

pub fn cw_serde_impl(input: DeriveInput) -> DeriveInput {
    match input.data {
        syn::Data::Struct(_) => parse_quote! {
            #[derive(
                ::cosmwasm_schema::serde::Serialize,
                ::cosmwasm_schema::serde::Deserialize,
                ::std::clone::Clone,
                ::std::fmt::Debug,
                ::std::cmp::PartialEq,
                ::cosmwasm_schema::schemars::JsonSchema
            )]
            #[allow(clippy::derive_partial_eq_without_eq)] // Allow users of `#[cw_serde]` to not implement Eq without clippy complaining
            #[serde(deny_unknown_fields, crate = "::cosmwasm_schema::serde")]
            #[schemars(crate = "::cosmwasm_schema::schemars")]
            #input
        },
        syn::Data::Enum(_) => parse_quote! {
            #[derive(
                ::cosmwasm_schema::serde::Serialize,
                ::cosmwasm_schema::serde::Deserialize,
                ::std::clone::Clone,
                ::std::fmt::Debug,
                ::std::cmp::PartialEq,
                ::cosmwasm_schema::schemars::JsonSchema
            )]
            #[allow(clippy::derive_partial_eq_without_eq)] // Allow users of `#[cw_serde]` to not implement Eq without clippy complaining
            #[serde(deny_unknown_fields, rename_all = "snake_case", crate = "::cosmwasm_schema::serde")]
            #[schemars(crate = "::cosmwasm_schema::schemars")]
            #input
        },
        syn::Data::Union(_) => panic!("unions are not supported"),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn structs() {
        let expanded = cw_serde_impl(parse_quote! {
            pub struct InstantiateMsg {
                pub verifier: String,
                pub beneficiary: String,
            }
        });

        let expected = parse_quote! {
            #[derive(
                ::cosmwasm_schema::serde::Serialize,
                ::cosmwasm_schema::serde::Deserialize,
                ::std::clone::Clone,
                ::std::fmt::Debug,
                ::std::cmp::PartialEq,
                ::cosmwasm_schema::schemars::JsonSchema
            )]
            #[allow(clippy::derive_partial_eq_without_eq)]
            #[serde(deny_unknown_fields, crate = "::cosmwasm_schema::serde")]
            #[schemars(crate = "::cosmwasm_schema::schemars")]
            pub struct InstantiateMsg {
                pub verifier: String,
                pub beneficiary: String,
            }
        };

        assert_eq!(expanded, expected);
    }

    #[test]
    fn empty_struct() {
        let expanded = cw_serde_impl(parse_quote! {
            pub struct InstantiateMsg {}
        });

        let expected = parse_quote! {
            #[derive(
                ::cosmwasm_schema::serde::Serialize,
                ::cosmwasm_schema::serde::Deserialize,
                ::std::clone::Clone,
                ::std::fmt::Debug,
                ::std::cmp::PartialEq,
                ::cosmwasm_schema::schemars::JsonSchema
            )]
            #[allow(clippy::derive_partial_eq_without_eq)]
            #[serde(deny_unknown_fields, crate = "::cosmwasm_schema::serde")]
            #[schemars(crate = "::cosmwasm_schema::schemars")]
            pub struct InstantiateMsg {}
        };

        assert_eq!(expanded, expected);
    }

    #[test]
    fn enums() {
        let expanded = cw_serde_impl(parse_quote! {
            pub enum SudoMsg {
                StealFunds {
                    recipient: String,
                    amount: Vec<Coin>,
                },
            }
        });

        let expected = parse_quote! {
            #[derive(
                ::cosmwasm_schema::serde::Serialize,
                ::cosmwasm_schema::serde::Deserialize,
                ::std::clone::Clone,
                ::std::fmt::Debug,
                ::std::cmp::PartialEq,
                ::cosmwasm_schema::schemars::JsonSchema
            )]
            #[allow(clippy::derive_partial_eq_without_eq)]
            #[serde(deny_unknown_fields, rename_all = "snake_case", crate = "::cosmwasm_schema::serde")]
            #[schemars(crate = "::cosmwasm_schema::schemars")]
            pub enum SudoMsg {
                StealFunds {
                    recipient: String,
                    amount: Vec<Coin>,
                },
            }
        };

        assert_eq!(expanded, expected);
    }

    #[test]
    #[should_panic(expected = "unions are not supported")]
    fn unions() {
        cw_serde_impl(parse_quote! {
            pub union SudoMsg {
                x: u32,
                y: u32,
            }
        });
    }
}
}
mod generate_api {
use std::collections::BTreeMap;

use proc_macro2::TokenStream;
use quote::quote;
use syn::{
    parse::{Parse, ParseStream},
    parse_quote, Block, ExprStruct, Ident, Path, Token,
};

pub fn write_api_impl(input: Options) -> Block {
    let api_object = generate_api_impl(&input);
    let name = input.name;

    parse_quote! {
        {
            #[cfg(target_arch = "wasm32")]
            compile_error!("can't compile schema generator for the `wasm32` arch\nhint: are you trying to compile a smart contract without specifying `--lib`?");
            use ::std::env;
            use ::std::fs::{create_dir_all, write};

            use ::cosmwasm_schema::{remove_schemas, Api, QueryResponses};

            let mut out_dir = env::current_dir().unwrap();
            out_dir.push("schema");
            create_dir_all(&out_dir).unwrap();
            remove_schemas(&out_dir).unwrap();

            let api = #api_object.render();


            let path = out_dir.join(concat!(#name, ".json"));

            let json = api.to_string().unwrap();
            write(&path, json + "\n").unwrap();
            println!("Exported the full API as {}", path.to_str().unwrap());

            let raw_dir = out_dir.join("raw");
            create_dir_all(&raw_dir).unwrap();

            for (filename, json) in api.to_schema_files().unwrap() {
                let path = raw_dir.join(filename);

                write(&path, json + "\n").unwrap();
                println!("Exported {}", path.to_str().unwrap());
            }
        }
    }
}

pub fn generate_api_impl(input: &Options) -> ExprStruct {
    let Options {
        name,
        version,
        instantiate,
        execute,
        query,
        migrate,
        sudo,
        responses,
    } = input;

    parse_quote! {
        ::cosmwasm_schema::Api {
            contract_name: #name.to_string(),
            contract_version: #version.to_string(),
            instantiate: ::cosmwasm_schema::schema_for!(#instantiate),
            execute: #execute,
            query: #query,
            migrate: #migrate,
            sudo: #sudo,
            responses: #responses,
        }
    }
}

#[derive(Debug)]
enum Value {
    Type(syn::Path),
    Str(syn::LitStr),
}

impl Value {
    fn unwrap_type(self) -> syn::Path {
        if let Self::Type(p) = self {
            p
        } else {
            panic!("expected a type");
        }
    }

    fn unwrap_str(self) -> syn::LitStr {
        if let Self::Str(s) = self {
            s
        } else {
            panic!("expected a string literal");
        }
    }
}

impl Parse for Value {
    fn parse(input: ParseStream) -> syn::parse::Result<Self> {
        if let Ok(p) = input.parse::<syn::Path>() {
            Ok(Self::Type(p))
        } else {
            Ok(Self::Str(input.parse::<syn::LitStr>()?))
        }
    }
}

#[derive(Debug)]
struct Pair((Ident, Value));

impl Parse for Pair {
    fn parse(input: ParseStream) -> syn::parse::Result<Self> {
        let k = input.parse::<syn::Ident>()?;
        input.parse::<Token![:]>()?;
        let v = input.parse::<Value>()?;

        Ok(Self((k, v)))
    }
}

#[derive(Debug)]
pub struct Options {
    name: TokenStream,
    version: TokenStream,
    instantiate: Path,
    execute: TokenStream,
    query: TokenStream,
    migrate: TokenStream,
    sudo: TokenStream,
    responses: TokenStream,
}

impl Parse for Options {
    fn parse(input: ParseStream) -> syn::parse::Result<Self> {
        let pairs = input.parse_terminated::<Pair, Token![,]>(Pair::parse)?;
        let mut map: BTreeMap<_, _> = pairs.into_iter().map(|p| p.0).collect();

        let name = if let Some(name_override) = map.remove(&parse_quote!(name)) {
            let name_override = name_override.unwrap_str();
            quote! {
                #name_override
            }
        } else {
            quote! {
                ::std::env!("CARGO_PKG_NAME")
            }
        };

        let version = if let Some(version_override) = map.remove(&parse_quote!(version)) {
            let version_override = version_override.unwrap_str();
            quote! {
                #version_override
            }
        } else {
            quote! {
                ::std::env!("CARGO_PKG_VERSION")
            }
        };

        let instantiate = map
            .remove(&parse_quote!(instantiate))
            .unwrap()
            .unwrap_type();

        let execute = match map.remove(&parse_quote!(execute)) {
            Some(ty) => {
                let ty = ty.unwrap_type();
                quote! {Some(::cosmwasm_schema::schema_for!(#ty))}
            }
            None => quote! { None },
        };

        let (query, responses) = match map.remove(&parse_quote!(query)) {
            Some(ty) => {
                let ty = ty.unwrap_type();
                (
                    quote! {Some(::cosmwasm_schema::schema_for!(#ty))},
                    quote! { Some(<#ty as QueryResponses>::response_schemas().unwrap()) },
                )
            }
            None => (quote! { None }, quote! { None }),
        };

        let migrate = match map.remove(&parse_quote!(migrate)) {
            Some(ty) => {
                let ty = ty.unwrap_type();
                quote! {Some(::cosmwasm_schema::schema_for!(#ty))}
            }
            None => quote! { None },
        };

        let sudo = match map.remove(&parse_quote!(sudo)) {
            Some(ty) => {
                let ty = ty.unwrap_type();
                quote! {Some(::cosmwasm_schema::schema_for!(#ty))}
            }
            None => quote! { None },
        };

        if let Some((invalid_option, _)) = map.into_iter().next() {
            panic!("unknown generate_api option: {}", invalid_option);
        }

        Ok(Self {
            name,
            version,
            instantiate,
            execute,
            query,
            migrate,
            sudo,
            responses,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn api_object_minimal() {
        assert_eq!(
            generate_api_impl(&parse_quote! {
                instantiate: InstantiateMsg,
            }),
            parse_quote! {
                ::cosmwasm_schema::Api {
                    contract_name: ::std::env!("CARGO_PKG_NAME").to_string(),
                    contract_version: ::std::env!("CARGO_PKG_VERSION").to_string(),
                    instantiate: ::cosmwasm_schema::schema_for!(InstantiateMsg),
                    execute: None,
                    query: None,
                    migrate: None,
                    sudo: None,
                    responses: None,
                }
            }
        );
    }

    #[test]
    fn api_object_name_vesion_override() {
        assert_eq!(
            generate_api_impl(&parse_quote! {
                name: "foo",
                version: "bar",
                instantiate: InstantiateMsg,
            }),
            parse_quote! {
                ::cosmwasm_schema::Api {
                    contract_name: "foo".to_string(),
                    contract_version: "bar".to_string(),
                    instantiate: ::cosmwasm_schema::schema_for!(InstantiateMsg),
                    execute: None,
                    query: None,
                    migrate: None,
                    sudo: None,
                    responses: None,
                }
            }
        );
    }

    #[test]
    fn api_object_all_msgs() {
        assert_eq!(
            generate_api_impl(&parse_quote! {
                instantiate: InstantiateMsg,
                execute: ExecuteMsg,
                query: QueryMsg,
                migrate: MigrateMsg,
                sudo: SudoMsg,
            }),
            parse_quote! {
                ::cosmwasm_schema::Api {
                    contract_name: ::std::env!("CARGO_PKG_NAME").to_string(),
                    contract_version: ::std::env!("CARGO_PKG_VERSION").to_string(),
                    instantiate: ::cosmwasm_schema::schema_for!(InstantiateMsg),
                    execute: Some(::cosmwasm_schema::schema_for!(ExecuteMsg)),
                    query: Some(::cosmwasm_schema::schema_for!(QueryMsg)),
                    migrate: Some(::cosmwasm_schema::schema_for!(MigrateMsg)),
                    sudo: Some(::cosmwasm_schema::schema_for!(SudoMsg)),
                    responses: Some(<QueryMsg as QueryResponses>::response_schemas().unwrap()),
                }
            }
        );
    }

    #[test]
    #[should_panic(expected = "unknown generate_api option: asd")]
    fn invalid_option() {
        let _options: Options = parse_quote! {
            instantiate: InstantiateMsg,
            asd: Asd,
        };
    }
}
}
mod query_responses {
mod context {
use std::collections::HashSet;

use syn::{Ident, ItemEnum, Meta, NestedMeta};

const ATTR_PATH: &str = "query_responses";

pub struct Context {
    /// If the enum we're trying to derive QueryResponses for collects other QueryMsgs,
    /// setting this flag will derive the implementation appropriately, collecting all
    /// KV pairs from the nested enums rather than expecting `#[return]` annotations.
    pub is_nested: bool,
    /// Disable infering the `JsonSchema` trait bound for chosen type parameters.
    pub no_bounds_for: HashSet<Ident>,
}

pub fn get_context(input: &ItemEnum) -> Context {
    let params = input
        .attrs
        .iter()
        .filter(|attr| matches!(attr.path.get_ident(), Some(id) if *id == ATTR_PATH))
        .flat_map(|attr| {
            if let Meta::List(l) = attr.parse_meta().unwrap() {
                l.nested
            } else {
                panic!("{} attribute must contain a meta list", ATTR_PATH);
            }
        })
        .map(|nested_meta| {
            if let NestedMeta::Meta(m) = nested_meta {
                m
            } else {
                panic!("no literals allowed in QueryResponses params")
            }
        });

    let mut ctx = Context {
        is_nested: false,
        no_bounds_for: HashSet::new(),
    };

    for param in params {
        match param.path().get_ident().unwrap().to_string().as_str() {
            "no_bounds_for" => {
                if let Meta::List(l) = param {
                    for item in l.nested {
                        match item {
                            NestedMeta::Meta(Meta::Path(p)) => {
                                ctx.no_bounds_for.insert(p.get_ident().unwrap().clone());
                            }
                            _ => panic!("`no_bounds_for` only accepts a list of type params"),
                        }
                    }
                } else {
                    panic!("expected a list for `no_bounds_for`")
                }
            }
            "nested" => ctx.is_nested = true,
            path => panic!("unrecognized QueryResponses param: {}", path),
        }
    }

    ctx
}
}

use syn::{
    parse_quote, Expr, ExprTuple, Generics, ItemEnum, ItemImpl, Type, TypeParamBound, Variant,
};

use self::context::Context;

pub fn query_responses_derive_impl(input: ItemEnum) -> ItemImpl {
    let ctx = context::get_context(&input);

    if ctx.is_nested {
        let ident = input.ident;
        let subquery_calls = input.variants.into_iter().map(parse_subquery);

        // Handle generics if the type has any
        let (_, type_generics, where_clause) = input.generics.split_for_impl();
        let impl_generics = impl_generics(
            &ctx,
            &input.generics,
            &[parse_quote! {::cosmwasm_schema::QueryResponses}],
        );

        let subquery_len = subquery_calls.len();
        parse_quote! {
            #[automatically_derived]
            #[cfg(not(target_arch = "wasm32"))]
            impl #impl_generics ::cosmwasm_schema::QueryResponses for #ident #type_generics #where_clause {
                fn response_schemas_impl() -> ::std::collections::BTreeMap<String, ::cosmwasm_schema::schemars::schema::RootSchema> {
                    let subqueries = [
                        #( #subquery_calls, )*
                    ];
                    ::cosmwasm_schema::combine_subqueries::<#subquery_len, #ident #type_generics>(subqueries)
                }
            }
        }
    } else {
        let ident = input.ident;
        let mappings = input.variants.into_iter().map(parse_query);
        let mut queries: Vec<_> = mappings.clone().map(|(q, _)| q).collect();
        queries.sort();
        let mappings = mappings.map(parse_tuple);

        // Handle generics if the type has any
        let (_, type_generics, where_clause) = input.generics.split_for_impl();
        let impl_generics = impl_generics(&ctx, &input.generics, &[]);

        parse_quote! {
            #[automatically_derived]
            #[cfg(not(target_arch = "wasm32"))]
            impl #impl_generics ::cosmwasm_schema::QueryResponses for #ident #type_generics #where_clause {
                fn response_schemas_impl() -> ::std::collections::BTreeMap<String, ::cosmwasm_schema::schemars::schema::RootSchema> {
                    ::std::collections::BTreeMap::from([
                        #( #mappings, )*
                    ])
                }
            }
        }
    }
}

/// Takes a list of generics from the type definition and produces a list of generics
/// for the expanded `impl` block, adding trait bounds like `JsonSchema` as appropriate.
fn impl_generics(ctx: &Context, generics: &Generics, bounds: &[TypeParamBound]) -> Generics {
    let mut impl_generics = generics.to_owned();
    for param in impl_generics.type_params_mut() {
        // remove the default type if present, as those are invalid in
        // a trait implementation
        param.default = None;

        if !ctx.no_bounds_for.contains(&param.ident) {
            param
                .bounds
                .push(parse_quote! {::cosmwasm_schema::schemars::JsonSchema});
            param.bounds.extend(bounds.to_owned());
        }
    }

    impl_generics
}

/// Extract the query -> response mapping out of an enum variant.
fn parse_query(v: Variant) -> (String, Expr) {
    let query = to_snake_case(&v.ident.to_string());
    let response_ty: Type = v
        .attrs
        .iter()
        .find(|a| a.path.get_ident().unwrap() == "returns")
        .unwrap_or_else(|| panic!("missing return type for query: {}", v.ident))
        .parse_args()
        .unwrap_or_else(|_| panic!("return for {} must be a type", v.ident));

    (
        query,
        parse_quote!(::cosmwasm_schema::schema_for!(#response_ty)),
    )
}

/// Extract the nested query  -> response mapping out of an enum variant.
fn parse_subquery(v: Variant) -> Expr {
    let submsg = match v.fields {
        syn::Fields::Named(_) => panic!("a struct variant is not a valid subquery"),
        syn::Fields::Unnamed(fields) => {
            if fields.unnamed.len() != 1 {
                panic!("invalid number of subquery parameters");
            }

            fields.unnamed[0].ty.clone()
        }
        syn::Fields::Unit => panic!("a unit variant is not a valid subquery"),
    };
    parse_quote!(<#submsg as ::cosmwasm_schema::QueryResponses>::response_schemas_impl())
}

fn parse_tuple((q, r): (String, Expr)) -> ExprTuple {
    parse_quote! {
        (#q.to_string(), #r)
    }
}

fn to_snake_case(input: &str) -> String {
    // this was stolen from serde for consistent behavior
    let mut snake = String::new();
    for (i, ch) in input.char_indices() {
        if i > 0 && ch.is_uppercase() {
            snake.push('_');
        }
        snake.push(ch.to_ascii_lowercase());
    }
    snake
}

#[cfg(test)]
mod tests {
    use syn::parse_quote;

    use super::*;

    #[test]
    fn happy_path() {
        let input: ItemEnum = parse_quote! {
            #[derive(Serialize, Deserialize, Clone, Debug, PartialEq, JsonSchema, QueryResponses)]
            #[serde(rename_all = "snake_case")]
            pub enum QueryMsg {
                #[returns(some_crate::AnotherType)]
                Supply {},
                #[returns(SomeType)]
                Balance {},
            }
        };

        assert_eq!(
            query_responses_derive_impl(input),
            parse_quote! {
                #[automatically_derived]
                #[cfg(not(target_arch = "wasm32"))]
                impl ::cosmwasm_schema::QueryResponses for QueryMsg {
                    fn response_schemas_impl() -> ::std::collections::BTreeMap<String, ::cosmwasm_schema::schemars::schema::RootSchema> {
                        ::std::collections::BTreeMap::from([
                            ("supply".to_string(), ::cosmwasm_schema::schema_for!(some_crate::AnotherType)),
                            ("balance".to_string(), ::cosmwasm_schema::schema_for!(SomeType)),
                        ])
                    }
                }
            }
        );
    }

    #[test]
    fn empty_query_msg() {
        let input: ItemEnum = parse_quote! {
            #[derive(Serialize, Deserialize, Clone, Debug, PartialEq, JsonSchema, QueryResponses)]
            #[serde(rename_all = "snake_case")]
            pub enum QueryMsg {}
        };

        assert_eq!(
            query_responses_derive_impl(input),
            parse_quote! {
                #[automatically_derived]
                #[cfg(not(target_arch = "wasm32"))]
                impl ::cosmwasm_schema::QueryResponses for QueryMsg {
                    fn response_schemas_impl() -> ::std::collections::BTreeMap<String, ::cosmwasm_schema::schemars::schema::RootSchema> {
                        ::std::collections::BTreeMap::from([])
                    }
                }
            }
        );
    }

    #[test]
    fn generics() {
        let input: ItemEnum = parse_quote! {
            #[derive(Serialize, Deserialize, Clone, Debug, PartialEq, JsonSchema, QueryResponses)]
            #[serde(rename_all = "snake_case")]
            pub enum QueryMsg<T> {
                #[returns(bool)]
                Foo,
                #[returns(u32)]
                Bar(T),
            }
        };

        let input2: ItemEnum = parse_quote! {
            #[derive(Serialize, Deserialize, Clone, Debug, PartialEq, JsonSchema, QueryResponses)]
            #[serde(rename_all = "snake_case")]
            pub enum QueryMsg<T: std::fmt::Debug + SomeTrait> {
                #[returns(bool)]
                Foo,
                #[returns(u32)]
                Bar { data: T },
            }
        };

        let input3: ItemEnum = parse_quote! {
            #[derive(Serialize, Deserialize, Clone, Debug, PartialEq, JsonSchema, QueryResponses)]
            #[serde(rename_all = "snake_case")]
            pub enum QueryMsg<T>
                where T: std::fmt::Debug + SomeTrait,
            {
                #[returns(bool)]
                Foo,
                #[returns(u32)]
                Bar { data: T },
            }
        };

        let result = query_responses_derive_impl(input);

        assert_eq!(
            result,
            parse_quote! {
                #[automatically_derived]
                #[cfg(not(target_arch = "wasm32"))]
                impl<T: ::cosmwasm_schema::schemars::JsonSchema> ::cosmwasm_schema::QueryResponses for QueryMsg<T> {
                    fn response_schemas_impl() -> ::std::collections::BTreeMap<String, ::cosmwasm_schema::schemars::schema::RootSchema> {
                        ::std::collections::BTreeMap::from([
                            ("foo".to_string(), ::cosmwasm_schema::schema_for!(bool)),
                            ("bar".to_string(), ::cosmwasm_schema::schema_for!(u32)),
                        ])
                    }
                }
            }
        );
        assert_eq!(
            query_responses_derive_impl(input2),
            parse_quote! {
                #[automatically_derived]
                #[cfg(not(target_arch = "wasm32"))]
                impl<T: std::fmt::Debug + SomeTrait + ::cosmwasm_schema::schemars::JsonSchema> ::cosmwasm_schema::QueryResponses for QueryMsg<T> {
                    fn response_schemas_impl() -> ::std::collections::BTreeMap<String, ::cosmwasm_schema::schemars::schema::RootSchema> {
                        ::std::collections::BTreeMap::from([
                            ("foo".to_string(), ::cosmwasm_schema::schema_for!(bool)),
                            ("bar".to_string(), ::cosmwasm_schema::schema_for!(u32)),
                        ])
                    }
                }
            }
        );
        let a = query_responses_derive_impl(input3);
        assert_eq!(
            a,
            parse_quote! {
                #[automatically_derived]
                #[cfg(not(target_arch = "wasm32"))]
                impl<T: ::cosmwasm_schema::schemars::JsonSchema> ::cosmwasm_schema::QueryResponses for QueryMsg<T>
                    where T: std::fmt::Debug + SomeTrait,
                {
                    fn response_schemas_impl() -> ::std::collections::BTreeMap<String, ::cosmwasm_schema::schemars::schema::RootSchema> {
                        ::std::collections::BTreeMap::from([
                            ("foo".to_string(), ::cosmwasm_schema::schema_for!(bool)),
                            ("bar".to_string(), ::cosmwasm_schema::schema_for!(u32)),
                        ])
                    }
                }
            }
        );
    }

    #[test]
    #[should_panic(expected = "missing return type for query: Supply")]
    fn missing_return() {
        let input: ItemEnum = parse_quote! {
            #[derive(Serialize, Deserialize, Clone, Debug, PartialEq, JsonSchema, QueryResponses)]
            #[serde(rename_all = "snake_case")]
            pub enum QueryMsg {
                Supply {},
                #[returns(SomeType)]
                Balance {},
            }
        };

        query_responses_derive_impl(input);
    }

    #[test]
    #[should_panic(expected = "return for Supply must be a type")]
    fn invalid_return() {
        let input: ItemEnum = parse_quote! {
            #[derive(Serialize, Deserialize, Clone, Debug, PartialEq, JsonSchema, QueryResponses)]
            #[serde(rename_all = "snake_case")]
            pub enum QueryMsg {
                #[returns(1)]
                Supply {},
                #[returns(SomeType)]
                Balance {},
            }
        };

        query_responses_derive_impl(input);
    }

    #[test]
    fn parse_query_works() {
        let variant = parse_quote! {
            #[returns(Foo)]
            GetFoo {}
        };

        assert_eq!(
            parse_tuple(parse_query(variant)),
            parse_quote! {
                ("get_foo".to_string(), ::cosmwasm_schema::schema_for!(Foo))
            }
        );

        let variant = parse_quote! {
            #[returns(some_crate::Foo)]
            GetFoo {}
        };

        assert_eq!(
            parse_tuple(parse_query(variant)),
            parse_quote! { ("get_foo".to_string(), ::cosmwasm_schema::schema_for!(some_crate::Foo)) }
        );
    }

    #[test]
    fn to_snake_case_works() {
        assert_eq!(to_snake_case("SnakeCase"), "snake_case");
        assert_eq!(to_snake_case("Wasm123AndCo"), "wasm123_and_co");
    }

    #[test]
    fn nested_works() {
        let input: ItemEnum = parse_quote! {
            #[derive(Serialize, Deserialize, Clone, Debug, PartialEq, JsonSchema, QueryResponses)]
            #[serde(untagged)]
            #[query_responses(nested)]
            pub enum ContractQueryMsg {
                Cw1(QueryMsg1),
                Whitelist(whitelist::QueryMsg),
                Cw1WhitelistContract(QueryMsg),
            }
        };
        let result = query_responses_derive_impl(input);
        assert_eq!(
            result,
            parse_quote! {
                #[automatically_derived]
                #[cfg(not(target_arch = "wasm32"))]
                impl ::cosmwasm_schema::QueryResponses for ContractQueryMsg {
                    fn response_schemas_impl() -> ::std::collections::BTreeMap<String, ::cosmwasm_schema::schemars::schema::RootSchema> {
                        let subqueries = [
                            <QueryMsg1 as ::cosmwasm_schema::QueryResponses>::response_schemas_impl(),
                            <whitelist::QueryMsg as ::cosmwasm_schema::QueryResponses>::response_schemas_impl(),
                            <QueryMsg as ::cosmwasm_schema::QueryResponses>::response_schemas_impl(),
                        ];
                        ::cosmwasm_schema::combine_subqueries::<3usize, ContractQueryMsg>(subqueries)
                    }
                }
            }
        );
    }

    #[test]
    fn nested_empty() {
        let input: ItemEnum = parse_quote! {
            #[derive(Serialize, Deserialize, Clone, Debug, PartialEq, JsonSchema, QueryResponses)]
            #[serde(untagged)]
            #[query_responses(nested)]
            pub enum EmptyMsg {}
        };
        let result = query_responses_derive_impl(input);
        assert_eq!(
            result,
            parse_quote! {
                #[automatically_derived]
                #[cfg(not(target_arch = "wasm32"))]
                impl ::cosmwasm_schema::QueryResponses for EmptyMsg {
                    fn response_schemas_impl() -> ::std::collections::BTreeMap<String, ::cosmwasm_schema::schemars::schema::RootSchema> {
                        let subqueries = [];
                        ::cosmwasm_schema::combine_subqueries::<0usize, EmptyMsg>(subqueries)
                    }
                }
            }
        );
    }

    #[test]
    #[should_panic(expected = "invalid number of subquery parameters")]
    fn nested_too_many_params() {
        let input: ItemEnum = parse_quote! {
            #[derive(Serialize, Deserialize, Clone, Debug, PartialEq, JsonSchema, QueryResponses)]
            #[serde(untagged)]
            #[query_responses(nested)]
            pub enum ContractQueryMsg {
                Msg1(QueryMsg1, QueryMsg2),
                Whitelist(whitelist::QueryMsg),
            }
        };
        query_responses_derive_impl(input);
    }

    #[test]
    #[should_panic(expected = "a struct variant is not a valid subquery")]
    fn nested_mixed() {
        let input: ItemEnum = parse_quote! {
            #[derive(Serialize, Deserialize, Clone, Debug, PartialEq, JsonSchema, QueryResponses)]
            #[serde(untagged)]
            #[query_responses(nested)]
            pub enum ContractQueryMsg {
                Cw1(cw1::QueryMsg),
                Test {
                    mixed: bool,
                }
            }
        };
        query_responses_derive_impl(input);
    }

    #[test]
    #[should_panic(expected = "a unit variant is not a valid subquery")]
    fn nested_unit_variant() {
        let input: ItemEnum = parse_quote! {
            #[derive(Serialize, Deserialize, Clone, Debug, PartialEq, JsonSchema, QueryResponses)]
            #[serde(untagged)]
            #[query_responses(nested)]
            pub enum ContractQueryMsg {
                Cw1(cw1::QueryMsg),
                Whitelist,
            }
        };
        query_responses_derive_impl(input);
    }
}
}

use quote::ToTokens;
use syn::{parse_macro_input, DeriveInput, ItemEnum};

#[proc_macro_derive(QueryResponses, attributes(returns, query_responses))]
pub fn query_responses_derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as ItemEnum);

    let expanded = query_responses::query_responses_derive_impl(input).into_token_stream();

    proc_macro::TokenStream::from(expanded)
}

#[proc_macro]
pub fn write_api(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as generate_api::Options);

    let expanded = generate_api::write_api_impl(input).into_token_stream();

    proc_macro::TokenStream::from(expanded)
}

#[proc_macro]
pub fn generate_api(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as generate_api::Options);

    let expanded = generate_api::generate_api_impl(&input).into_token_stream();

    proc_macro::TokenStream::from(expanded)
}

#[proc_macro_attribute]
pub fn cw_serde(
    _attr: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let expanded = cw_serde::cw_serde_impl(input).into_token_stream();

    proc_macro::TokenStream::from(expanded)
}
