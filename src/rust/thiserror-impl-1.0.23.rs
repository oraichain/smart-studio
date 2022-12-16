#![allow(clippy::blocks_in_if_conditions, clippy::range_plus_one)]

extern crate proc_macro;

mod ast {
use crate::attr::{self, Attrs};
use proc_macro2::Span;
use syn::{
    Data, DataEnum, DataStruct, DeriveInput, Error, Fields, Generics, Ident, Index, Member, Result,
    Type,
};

pub enum Input<'a> {
    Struct(Struct<'a>),
    Enum(Enum<'a>),
}

pub struct Struct<'a> {
    pub original: &'a DeriveInput,
    pub attrs: Attrs<'a>,
    pub ident: Ident,
    pub generics: &'a Generics,
    pub fields: Vec<Field<'a>>,
}

pub struct Enum<'a> {
    pub original: &'a DeriveInput,
    pub attrs: Attrs<'a>,
    pub ident: Ident,
    pub generics: &'a Generics,
    pub variants: Vec<Variant<'a>>,
}

pub struct Variant<'a> {
    pub original: &'a syn::Variant,
    pub attrs: Attrs<'a>,
    pub ident: Ident,
    pub fields: Vec<Field<'a>>,
}

pub struct Field<'a> {
    pub original: &'a syn::Field,
    pub attrs: Attrs<'a>,
    pub member: Member,
    pub ty: &'a Type,
}

impl<'a> Input<'a> {
    pub fn from_syn(node: &'a DeriveInput) -> Result<Self> {
}
}

impl<'a> Struct<'a> {
    fn from_syn(node: &'a DeriveInput, data: &'a DataStruct) -> Result<Self> {
}
}

impl<'a> Enum<'a> {
    fn from_syn(node: &'a DeriveInput, data: &'a DataEnum) -> Result<Self> {
}
}

impl<'a> Variant<'a> {
    fn from_syn(node: &'a syn::Variant, span: Span) -> Result<Self> {
}
}

impl<'a> Field<'a> {
    fn multiple_from_syn(fields: &'a Fields, span: Span) -> Result<Vec<Self>> {
}

    fn from_syn(i: usize, node: &'a syn::Field, span: Span) -> Result<Self> {
}
}

impl Attrs<'_> {
    pub fn span(&self) -> Option<Span> {
}
}
}
mod attr {
use proc_macro2::{Delimiter, Group, Span, TokenStream, TokenTree};
use quote::{format_ident, quote, ToTokens};
use std::iter::FromIterator;
use syn::parse::{Nothing, ParseStream};
use syn::{
    braced, bracketed, parenthesized, token, Attribute, Error, Ident, Index, LitInt, LitStr,
    Result, Token,
};

pub struct Attrs<'a> {
    pub display: Option<Display<'a>>,
    pub source: Option<&'a Attribute>,
    pub backtrace: Option<&'a Attribute>,
    pub from: Option<&'a Attribute>,
    pub transparent: Option<Transparent<'a>>,
}

#[derive(Clone)]
pub struct Display<'a> {
    pub original: &'a Attribute,
    pub fmt: LitStr,
    pub args: TokenStream,
    pub has_bonus_display: bool,
}

#[derive(Copy, Clone)]
pub struct Transparent<'a> {
    pub original: &'a Attribute,
    pub span: Span,
}

pub fn get(input: &[Attribute]) -> Result<Attrs> {
}

fn parse_error_attribute<'a>(attrs: &mut Attrs<'a>, attr: &'a Attribute) -> Result<()> {
}

fn parse_token_expr(input: ParseStream, mut begin_expr: bool) -> Result<TokenStream> {
}

fn require_empty_attribute(attr: &Attribute) -> Result<()> {
}

impl ToTokens for Display<'_> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
}
}
}
mod expand {
use crate::ast::{Enum, Field, Input, Struct};
use proc_macro2::TokenStream;
use quote::{format_ident, quote, quote_spanned, ToTokens};
use syn::spanned::Spanned;
use syn::{Data, DeriveInput, Member, PathArguments, Result, Type, Visibility};

pub fn derive(node: &DeriveInput) -> Result<TokenStream> {
}

fn impl_struct(input: Struct) -> TokenStream {
}

fn impl_enum(input: Enum) -> TokenStream {
}

fn fields_pat(fields: &[Field]) -> TokenStream {
}

fn from_initializer(from_field: &Field, backtrace_field: Option<&Field>) -> TokenStream {
}

fn type_is_option(ty: &Type) -> bool {
}

fn spanned_error_trait(input: &DeriveInput) -> TokenStream {
}
}
mod fmt {
use crate::ast::Field;
use crate::attr::Display;
use proc_macro2::TokenTree;
use quote::{format_ident, quote_spanned};
use std::collections::HashSet as Set;
use syn::ext::IdentExt;
use syn::parse::{ParseStream, Parser};
use syn::{Ident, Index, LitStr, Member, Result, Token};

impl Display<'_> {
    // Transform `"error {var}"` to `"error {}", var`.
    pub fn expand_shorthand(&mut self, fields: &[Field]) {
}
}

fn explicit_named_args(input: ParseStream) -> Result<Set<Ident>> {
}

fn take_int(read: &mut &str) -> String {
}

fn take_ident(read: &mut &str) -> Ident {
}
}
mod prop {
use crate::ast::{Enum, Field, Struct, Variant};
use syn::{Member, Type};

impl Struct<'_> {
    pub(crate) fn from_field(&self) -> Option<&Field> {
}

    pub(crate) fn source_field(&self) -> Option<&Field> {
}

    pub(crate) fn backtrace_field(&self) -> Option<&Field> {
}
}

impl Enum<'_> {
    pub(crate) fn has_source(&self) -> bool {
}

    pub(crate) fn has_backtrace(&self) -> bool {
}

    pub(crate) fn has_display(&self) -> bool {
}
}

impl Variant<'_> {
    pub(crate) fn from_field(&self) -> Option<&Field> {
}

    pub(crate) fn source_field(&self) -> Option<&Field> {
}

    pub(crate) fn backtrace_field(&self) -> Option<&Field> {
}
}

impl Field<'_> {
    pub(crate) fn is_backtrace(&self) -> bool {
}
}

fn from_field<'a, 'b>(fields: &'a [Field<'b>]) -> Option<&'a Field<'b>> {
}

fn source_field<'a, 'b>(fields: &'a [Field<'b>]) -> Option<&'a Field<'b>> {
}

fn backtrace_field<'a, 'b>(fields: &'a [Field<'b>]) -> Option<&'a Field<'b>> {
}

fn type_is_backtrace(ty: &Type) -> bool {
}
}
mod valid {
use crate::ast::{Enum, Field, Input, Struct, Variant};
use crate::attr::Attrs;
use quote::ToTokens;
use std::collections::BTreeSet as Set;
use syn::{Error, GenericArgument, Member, PathArguments, Result, Type};

impl Input<'_> {
    pub(crate) fn validate(&self) -> Result<()> {
}
}

impl Struct<'_> {
    fn validate(&self) -> Result<()> {
}
}

impl Enum<'_> {
    fn validate(&self) -> Result<()> {
}
}

impl Variant<'_> {
    fn validate(&self) -> Result<()> {
}
}

impl Field<'_> {
    fn validate(&self) -> Result<()> {
}
}

fn check_non_field_attrs(attrs: &Attrs) -> Result<()> {
}

fn check_field_attrs(fields: &[Field]) -> Result<()> {
}

fn same_member(one: &Field, two: &Field) -> bool {
}

fn contains_non_static_lifetime(ty: &Type) -> bool {
}
}

use proc_macro::TokenStream;
use syn::{parse_macro_input, DeriveInput};

#[proc_macro_derive(Error, attributes(backtrace, error, from, source))]
pub fn derive_error(input: TokenStream) -> TokenStream {
}
