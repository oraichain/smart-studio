mod cw_serde {
use syn::{parse_quote, DeriveInput};

pub fn cw_serde_impl(input: DeriveInput) -> DeriveInput {
}

#[cfg(test)]
mod tests {
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
}

pub fn generate_api_impl(input: &Options) -> ExprStruct {
}

#[derive(Debug)]
enum Value {
    Type(syn::Path),
    Str(syn::LitStr),
}

impl Value {
    fn unwrap_type(self) -> syn::Path {
}

    fn unwrap_str(self) -> syn::LitStr {
}
}

impl Parse for Value {
    fn parse(input: ParseStream) -> syn::parse::Result<Self> {
}
}

#[derive(Debug)]
struct Pair((Ident, Value));

impl Parse for Pair {
    fn parse(input: ParseStream) -> syn::parse::Result<Self> {
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
}
}

#[cfg(test)]
mod tests {
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
}
}

use syn::{
    parse_quote, Expr, ExprTuple, Generics, ItemEnum, ItemImpl, Type, TypeParamBound, Variant,
};

use self::context::Context;

pub fn query_responses_derive_impl(input: ItemEnum) -> ItemImpl {
}

/// Takes a list of generics from the type definition and produces a list of generics
/// for the expanded `impl` block, adding trait bounds like `JsonSchema` as appropriate.
fn impl_generics(ctx: &Context, generics: &Generics, bounds: &[TypeParamBound]) -> Generics {
}

/// Extract the query -> response mapping out of an enum variant.
fn parse_query(v: Variant) -> (String, Expr) {
}

/// Extract the nested query  -> response mapping out of an enum variant.
fn parse_subquery(v: Variant) -> Expr {
}

fn parse_tuple((q, r): (String, Expr)) -> ExprTuple {
}

fn to_snake_case(input: &str) -> String {
}

#[cfg(test)]
mod tests {
}
}

use quote::ToTokens;
use syn::{parse_macro_input, DeriveInput, ItemEnum};

#[proc_macro_derive(QueryResponses, attributes(returns, query_responses))]
pub fn query_responses_derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
}

#[proc_macro]
pub fn write_api(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
}

#[proc_macro]
pub fn generate_api(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
}

#[proc_macro_attribute]
pub fn cw_serde(
    _attr: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
}
