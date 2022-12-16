//! A wrapper around the procedural macro API of the compiler's [`proc_macro`]
//! crate. This library serves two purposes:
//!
//! [`proc_macro`]: https://doc.rust-lang.org/proc_macro/
//!
//! - **Bring proc-macro-like functionality to other contexts like build.rs and
//!   main.rs.** Types from `proc_macro` are entirely specific to procedural
//!   macros and cannot ever exist in code outside of a procedural macro.
//!   Meanwhile `proc_macro2` types may exist anywhere including non-macro code.
//!   By developing foundational libraries like [syn] and [quote] against
//!   `proc_macro2` rather than `proc_macro`, the procedural macro ecosystem
//!   becomes easily applicable to many other use cases and we avoid
//!   reimplementing non-macro equivalents of those libraries.
//!
//! - **Make procedural macros unit testable.** As a consequence of being
//!   specific to procedural macros, nothing that uses `proc_macro` can be
//!   executed from a unit test. In order for helper libraries or components of
//!   a macro to be testable in isolation, they must be implemented using
//!   `proc_macro2`.
//!
//! [syn]: https://github.com/dtolnay/syn
//! [quote]: https://github.com/dtolnay/quote
//!
//! # Usage
//!
//! The skeleton of a typical procedural macro typically looks like this:
//!
//! ```
//! extern crate proc_macro;
//!
//! # const IGNORE: &str = stringify! {
//! #[proc_macro_derive(MyDerive)]
//! # };
//! # #[cfg(wrap_proc_macro)]
//! pub fn my_derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
//!     let input = proc_macro2::TokenStream::from(input);
//!
//!     let output: proc_macro2::TokenStream = {
//!         /* transform input */
//!         # input
//!     };
//!
//!     proc_macro::TokenStream::from(output)
//! }
//! ```
//!
//! If parsing with [Syn], you'll use [`parse_macro_input!`] instead to
//! propagate parse errors correctly back to the compiler when parsing fails.
//!
//! [`parse_macro_input!`]: https://docs.rs/syn/1.0/syn/macro.parse_macro_input.html
//!
//! # Unstable features
//!
//! The default feature set of proc-macro2 tracks the most recent stable
//! compiler API. Functionality in `proc_macro` that is not yet stable is not
//! exposed by proc-macro2 by default.
//!
//! To opt into the additional APIs available in the most recent nightly
//! compiler, the `procmacro2_semver_exempt` config flag must be passed to
//! rustc. We will polyfill those nightly-only APIs back to Rust 1.31.0. As
//! these are unstable APIs that track the nightly compiler, minor versions of
//! proc-macro2 may make breaking changes to them at any time.
//!
//! ```sh
//! RUSTFLAGS='--cfg procmacro2_semver_exempt' cargo build
//! ```
//!
//! Note that this must not only be done for your crate, but for any crate that
//! depends on your crate. This infectious nature is intentional, as it serves
//! as a reminder that you are outside of the normal semver guarantees.
//!
//! Semver exempt methods are marked as such in the proc-macro2 documentation.
//!
//! # Thread-Safety
//!
//! Most types in this crate are `!Sync` because the underlying compiler
//! types make use of thread-local memory, meaning they cannot be accessed from
//! a different thread.

// Proc-macro2 types in rustdoc of other crates get linked to here.
#![doc(html_root_url = "https://docs.rs/proc-macro2/1.0.6")]
#![cfg_attr(any(proc_macro_span, super_unstable), feature(proc_macro_span))]
#![cfg_attr(super_unstable, feature(proc_macro_raw_ident, proc_macro_def_site))]

#[cfg(use_proc_macro)]
extern crate proc_macro;

use std::cmp::Ordering;
use std::fmt;
use std::hash::{Hash, Hasher};
use std::iter::FromIterator;
use std::marker;
use std::ops::RangeBounds;
#[cfg(procmacro2_semver_exempt)]
use std::path::PathBuf;
use std::rc::Rc;
use std::str::FromStr;

#[macro_use]
mod strnom {
//! Adapted from [`nom`](https://github.com/Geal/nom).

use crate::fallback::LexError;
use std::str::{Bytes, CharIndices, Chars};
use unicode_xid::UnicodeXID;

#[derive(Copy, Clone, Eq, PartialEq)]
pub struct Cursor<'a> {
    pub rest: &'a str,
    #[cfg(span_locations)]
    pub off: u32,
}

impl<'a> Cursor<'a> {
    #[cfg(not(span_locations))]
    pub fn advance(&self, amt: usize) -> Cursor<'a> {
}
    #[cfg(span_locations)]
    pub fn advance(&self, amt: usize) -> Cursor<'a> {
}

    pub fn find(&self, p: char) -> Option<usize> {
}

    pub fn starts_with(&self, s: &str) -> bool {
}

    pub fn is_empty(&self) -> bool {
}

    pub fn len(&self) -> usize {
}

    pub fn as_bytes(&self) -> &'a [u8] {
}

    pub fn bytes(&self) -> Bytes<'a> {
}

    pub fn chars(&self) -> Chars<'a> {
}

    pub fn char_indices(&self) -> CharIndices<'a> {
}
}

pub type PResult<'a, O> = Result<(Cursor<'a>, O), LexError>;

pub fn whitespace(input: Cursor) -> PResult<()> {
}

pub fn block_comment(input: Cursor) -> PResult<&str> {
}

pub fn skip_whitespace(input: Cursor) -> Cursor {
}

fn is_whitespace(ch: char) -> bool {
}

pub fn word_break(input: Cursor) -> PResult<()> {
}

macro_rules! named {
    ($name:ident -> $o:ty, $submac:ident!( $($args:tt)* )) => {
        fn $name<'a>(i: Cursor<'a>) -> $crate::strnom::PResult<'a, $o> {
}
    };
}

macro_rules! alt {
    ($i:expr, $e:ident | $($rest:tt)*) => {
        alt!($i, call!($e) | $($rest)*)
    };

    ($i:expr, $subrule:ident!( $($args:tt)*) | $($rest:tt)*) => {
        match $subrule!($i, $($args)*) {
            res @ Ok(_) => res,
            _ => alt!($i, $($rest)*)
        }
    };

    ($i:expr, $subrule:ident!( $($args:tt)* ) => { $gen:expr } | $($rest:tt)+) => {
        match $subrule!($i, $($args)*) {
            Ok((i, o)) => Ok((i, $gen(o))),
            Err(LexError) => alt!($i, $($rest)*)
        }
    };

    ($i:expr, $e:ident => { $gen:expr } | $($rest:tt)*) => {
        alt!($i, call!($e) => { $gen } | $($rest)*)
    };

    ($i:expr, $e:ident => { $gen:expr }) => {
        alt!($i, call!($e) => { $gen })
    };

    ($i:expr, $subrule:ident!( $($args:tt)* ) => { $gen:expr }) => {
        match $subrule!($i, $($args)*) {
            Ok((i, o)) => Ok((i, $gen(o))),
            Err(LexError) => Err(LexError),
        }
    };

    ($i:expr, $e:ident) => {
        alt!($i, call!($e))
    };

    ($i:expr, $subrule:ident!( $($args:tt)*)) => {
        $subrule!($i, $($args)*)
    };
}

macro_rules! do_parse {
    ($i:expr, ( $($rest:expr),* )) => {
        Ok(($i, ( $($rest),* )))
    };

    ($i:expr, $e:ident >> $($rest:tt)*) => {
        do_parse!($i, call!($e) >> $($rest)*)
    };

    ($i:expr, $submac:ident!( $($args:tt)* ) >> $($rest:tt)*) => {
        match $submac!($i, $($args)*) {
            Err(LexError) => Err(LexError),
            Ok((i, _)) => do_parse!(i, $($rest)*),
        }
    };

    ($i:expr, $field:ident : $e:ident >> $($rest:tt)*) => {
        do_parse!($i, $field: call!($e) >> $($rest)*)
    };

    ($i:expr, $field:ident : $submac:ident!( $($args:tt)* ) >> $($rest:tt)*) => {
        match $submac!($i, $($args)*) {
            Err(LexError) => Err(LexError),
            Ok((i, o)) => {
                let $field = o;
                do_parse!(i, $($rest)*)
            },
        }
    };
}

macro_rules! peek {
    ($i:expr, $submac:ident!( $($args:tt)* )) => {
        match $submac!($i, $($args)*) {
            Ok((_, o)) => Ok(($i, o)),
            Err(LexError) => Err(LexError),
        }
    };
}

macro_rules! call {
    ($i:expr, $fun:expr $(, $args:expr)*) => {
        $fun($i $(, $args)*)
    };
}

macro_rules! option {
    ($i:expr, $f:expr) => {
        match $f($i) {
            Ok((i, o)) => Ok((i, Some(o))),
            Err(LexError) => Ok(($i, None)),
        }
    };
}

macro_rules! take_until_newline_or_eof {
    ($i:expr,) => {{
        if $i.len() == 0 {
            Ok(($i, ""))
        } else {
            match $i.find('\n') {
                Some(i) => Ok(($i.advance(i), &$i.rest[..i])),
                None => Ok(($i.advance($i.len()), &$i.rest[..$i.len()])),
            }
        }
    }};
}

macro_rules! tuple {
    ($i:expr, $($rest:tt)*) => {
        tuple_parser!($i, (), $($rest)*)
    };
}

/// Do not use directly. Use `tuple!`.
macro_rules! tuple_parser {
    ($i:expr, ($($parsed:tt),*), $e:ident, $($rest:tt)*) => {
        tuple_parser!($i, ($($parsed),*), call!($e), $($rest)*)
    };

    ($i:expr, (), $submac:ident!( $($args:tt)* ), $($rest:tt)*) => {
        match $submac!($i, $($args)*) {
            Err(LexError) => Err(LexError),
            Ok((i, o)) => tuple_parser!(i, (o), $($rest)*),
        }
    };

    ($i:expr, ($($parsed:tt)*), $submac:ident!( $($args:tt)* ), $($rest:tt)*) => {
        match $submac!($i, $($args)*) {
            Err(LexError) => Err(LexError),
            Ok((i, o)) => tuple_parser!(i, ($($parsed)* , o), $($rest)*),
        }
    };

    ($i:expr, ($($parsed:tt),*), $e:ident) => {
        tuple_parser!($i, ($($parsed),*), call!($e))
    };

    ($i:expr, (), $submac:ident!( $($args:tt)* )) => {
        $submac!($i, $($args)*)
    };

    ($i:expr, ($($parsed:expr),*), $submac:ident!( $($args:tt)* )) => {
        match $submac!($i, $($args)*) {
            Err(LexError) => Err(LexError),
            Ok((i, o)) => Ok((i, ($($parsed),*, o)))
        }
    };

    ($i:expr, ($($parsed:expr),*)) => {
        Ok(($i, ($($parsed),*)))
    };
}

macro_rules! not {
    ($i:expr, $submac:ident!( $($args:tt)* )) => {
        match $submac!($i, $($args)*) {
            Ok((_, _)) => Err(LexError),
            Err(LexError) => Ok(($i, ())),
        }
    };
}

macro_rules! tag {
    ($i:expr, $tag:expr) => {
        if $i.starts_with($tag) {
            Ok(($i.advance($tag.len()), &$i.rest[..$tag.len()]))
        } else {
            Err(LexError)
        }
    };
}

macro_rules! punct {
    ($i:expr, $punct:expr) => {
        $crate::strnom::punct($i, $punct)
    };
}

/// Do not use directly. Use `punct!`.
pub fn punct<'a>(input: Cursor<'a>, token: &'static str) -> PResult<'a, &'a str> {
}

macro_rules! preceded {
    ($i:expr, $submac:ident!( $($args:tt)* ), $submac2:ident!( $($args2:tt)* )) => {
        match tuple!($i, $submac!($($args)*), $submac2!($($args2)*)) {
            Ok((remaining, (_, o))) => Ok((remaining, o)),
            Err(LexError) => Err(LexError),
        }
    };

    ($i:expr, $submac:ident!( $($args:tt)* ), $g:expr) => {
        preceded!($i, $submac!($($args)*), call!($g))
    };
}

macro_rules! delimited {
    ($i:expr, $submac:ident!( $($args:tt)* ), $($rest:tt)+) => {
        match tuple_parser!($i, (), $submac!($($args)*), $($rest)*) {
            Err(LexError) => Err(LexError),
            Ok((i1, (_, o, _))) => Ok((i1, o))
        }
    };
}

macro_rules! map {
    ($i:expr, $submac:ident!( $($args:tt)* ), $g:expr) => {
        match $submac!($i, $($args)*) {
            Err(LexError) => Err(LexError),
            Ok((i, o)) => Ok((i, call!(o, $g)))
        }
    };

    ($i:expr, $f:expr, $g:expr) => {
        map!($i, call!($f), $g)
    };
}
}
mod fallback {
#[cfg(span_locations)]
use std::cell::RefCell;
#[cfg(span_locations)]
use std::cmp;
use std::fmt;
use std::iter;
use std::ops::RangeBounds;
#[cfg(procmacro2_semver_exempt)]
use std::path::Path;
use std::path::PathBuf;
use std::str::FromStr;
use std::vec;

use crate::strnom::{block_comment, skip_whitespace, whitespace, word_break, Cursor, PResult};
use crate::{Delimiter, Punct, Spacing, TokenTree};
use unicode_xid::UnicodeXID;

#[derive(Clone)]
pub struct TokenStream {
    inner: Vec<TokenTree>,
}

#[derive(Debug)]
pub struct LexError;

impl TokenStream {
    pub fn new() -> TokenStream {
}

    pub fn is_empty(&self) -> bool {
}
}

#[cfg(span_locations)]
fn get_cursor(src: &str) -> Cursor {
}

#[cfg(not(span_locations))]
fn get_cursor(src: &str) -> Cursor {
}

impl FromStr for TokenStream {
    type Err = LexError;

    fn from_str(src: &str) -> Result<TokenStream, LexError> {
}
}

impl fmt::Display for TokenStream {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
}
}

impl fmt::Debug for TokenStream {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
}
}

#[cfg(use_proc_macro)]
impl From<proc_macro::TokenStream> for TokenStream {
    fn from(inner: proc_macro::TokenStream) -> TokenStream {
}
}

#[cfg(use_proc_macro)]
impl From<TokenStream> for proc_macro::TokenStream {
    fn from(inner: TokenStream) -> proc_macro::TokenStream {
}
}

impl From<TokenTree> for TokenStream {
    fn from(tree: TokenTree) -> TokenStream {
}
}

impl iter::FromIterator<TokenTree> for TokenStream {
    fn from_iter<I: IntoIterator<Item = TokenTree>>(streams: I) -> Self {
}
}

impl iter::FromIterator<TokenStream> for TokenStream {
    fn from_iter<I: IntoIterator<Item = TokenStream>>(streams: I) -> Self {
}
}

impl Extend<TokenTree> for TokenStream {
    fn extend<I: IntoIterator<Item = TokenTree>>(&mut self, streams: I) {
}
}

impl Extend<TokenStream> for TokenStream {
    fn extend<I: IntoIterator<Item = TokenStream>>(&mut self, streams: I) {
}
}

pub type TokenTreeIter = vec::IntoIter<TokenTree>;

impl IntoIterator for TokenStream {
    type Item = TokenTree;
    type IntoIter = TokenTreeIter;

    fn into_iter(self) -> TokenTreeIter {
}
}

#[derive(Clone, PartialEq, Eq)]
pub struct SourceFile {
    path: PathBuf,
}

impl SourceFile {
    /// Get the path to this source file as a string.
    pub fn path(&self) -> PathBuf {
}

    pub fn is_real(&self) -> bool {
}
}

impl fmt::Debug for SourceFile {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
}
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct LineColumn {
    pub line: usize,
    pub column: usize,
}

#[cfg(span_locations)]
thread_local! {
    static SOURCE_MAP: RefCell<SourceMap> = RefCell::new(SourceMap {
        // NOTE: We start with a single dummy file which all call_site() and
        // def_site() spans reference.
        files: vec![{
            #[cfg(procmacro2_semver_exempt)]
            {
                FileInfo {
                    name: "<unspecified>".to_owned(),
                    span: Span { lo: 0, hi: 0 },
                    lines: vec![0],
                }
            }

            #[cfg(not(procmacro2_semver_exempt))]
            {
                FileInfo {
                    span: Span { lo: 0, hi: 0 },
                    lines: vec![0],
                }
            }
        }],
    });
}

#[cfg(span_locations)]
struct FileInfo {
    #[cfg(procmacro2_semver_exempt)]
    name: String,
    span: Span,
    lines: Vec<usize>,
}

#[cfg(span_locations)]
impl FileInfo {
    fn offset_line_column(&self, offset: usize) -> LineColumn {
}

    fn span_within(&self, span: Span) -> bool {
}
}

/// Computesthe offsets of each line in the given source string.
#[cfg(span_locations)]
fn lines_offsets(s: &str) -> Vec<usize> {
}

#[cfg(span_locations)]
struct SourceMap {
    files: Vec<FileInfo>,
}

#[cfg(span_locations)]
impl SourceMap {
    fn next_start_pos(&self) -> u32 {
}

    fn add_file(&mut self, name: &str, src: &str) -> Span {
}

    fn fileinfo(&self, span: Span) -> &FileInfo {
}
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Span {
    #[cfg(span_locations)]
    lo: u32,
    #[cfg(span_locations)]
    hi: u32,
}

impl Span {
    #[cfg(not(span_locations))]
    pub fn call_site() -> Span {
}

    #[cfg(span_locations)]
    pub fn call_site() -> Span {
}

    #[cfg(procmacro2_semver_exempt)]
    pub fn def_site() -> Span {
}

    #[cfg(procmacro2_semver_exempt)]
    pub fn resolved_at(&self, _other: Span) -> Span {
}

    #[cfg(procmacro2_semver_exempt)]
    pub fn located_at(&self, other: Span) -> Span {
}

    #[cfg(procmacro2_semver_exempt)]
    pub fn source_file(&self) -> SourceFile {
}

    #[cfg(span_locations)]
    pub fn start(&self) -> LineColumn {
}

    #[cfg(span_locations)]
    pub fn end(&self) -> LineColumn {
}

    #[cfg(not(span_locations))]
    pub fn join(&self, _other: Span) -> Option<Span> {
}

    #[cfg(span_locations)]
    pub fn join(&self, other: Span) -> Option<Span> {
}
}

impl fmt::Debug for Span {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
}
}

pub fn debug_span_field_if_nontrivial(debug: &mut fmt::DebugStruct, span: Span) {
}

#[derive(Clone)]
pub struct Group {
    delimiter: Delimiter,
    stream: TokenStream,
    span: Span,
}

impl Group {
    pub fn new(delimiter: Delimiter, stream: TokenStream) -> Group {
}

    pub fn delimiter(&self) -> Delimiter {
}

    pub fn stream(&self) -> TokenStream {
}

    pub fn span(&self) -> Span {
}

    pub fn span_open(&self) -> Span {
}

    pub fn span_close(&self) -> Span {
}

    pub fn set_span(&mut self, span: Span) {
}
}

impl fmt::Display for Group {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
}
}

impl fmt::Debug for Group {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
}
}

#[derive(Clone)]
pub struct Ident {
    sym: String,
    span: Span,
    raw: bool,
}

impl Ident {
    fn _new(string: &str, raw: bool, span: Span) -> Ident {
}

    pub fn new(string: &str, span: Span) -> Ident {
}

    pub fn new_raw(string: &str, span: Span) -> Ident {
}

    pub fn span(&self) -> Span {
}

    pub fn set_span(&mut self, span: Span) {
}
}

#[inline]
fn is_ident_start(c: char) -> bool {
}

#[inline]
fn is_ident_continue(c: char) -> bool {
}

fn validate_ident(string: &str) {
}

impl PartialEq for Ident {
    fn eq(&self, other: &Ident) -> bool {
}
}

impl<T> PartialEq<T> for Ident
where
    T: ?Sized + AsRef<str>,
{
    fn eq(&self, other: &T) -> bool {
}
}

impl fmt::Display for Ident {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
}
}

impl fmt::Debug for Ident {
    // Ident(proc_macro), Ident(r#union)
    #[cfg(not(procmacro2_semver_exempt))]
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
}

    // Ident {
    //     sym: proc_macro,
    //     span: bytes(128..138)
    // }
    #[cfg(procmacro2_semver_exempt)]
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
}
}

#[derive(Clone)]
pub struct Literal {
    text: String,
    span: Span,
}

macro_rules! suffixed_numbers {
    ($($name:ident => $kind:ident,)*) => ($(
        pub fn $name(n: $kind) -> Literal {
}
    )*)
}

macro_rules! unsuffixed_numbers {
    ($($name:ident => $kind:ident,)*) => ($(
        pub fn $name(n: $kind) -> Literal {
}
    )*)
}

impl Literal {
    fn _new(text: String) -> Literal {
}

    suffixed_numbers! {
        u8_suffixed => u8,
        u16_suffixed => u16,
        u32_suffixed => u32,
        u64_suffixed => u64,
        u128_suffixed => u128,
        usize_suffixed => usize,
        i8_suffixed => i8,
        i16_suffixed => i16,
        i32_suffixed => i32,
        i64_suffixed => i64,
        i128_suffixed => i128,
        isize_suffixed => isize,

        f32_suffixed => f32,
        f64_suffixed => f64,
    }

    unsuffixed_numbers! {
        u8_unsuffixed => u8,
        u16_unsuffixed => u16,
        u32_unsuffixed => u32,
        u64_unsuffixed => u64,
        u128_unsuffixed => u128,
        usize_unsuffixed => usize,
        i8_unsuffixed => i8,
        i16_unsuffixed => i16,
        i32_unsuffixed => i32,
        i64_unsuffixed => i64,
        i128_unsuffixed => i128,
        isize_unsuffixed => isize,
    }

    pub fn f32_unsuffixed(f: f32) -> Literal {
}

    pub fn f64_unsuffixed(f: f64) -> Literal {
}

    pub fn string(t: &str) -> Literal {
}

    pub fn character(t: char) -> Literal {
}

    pub fn byte_string(bytes: &[u8]) -> Literal {
}

    pub fn span(&self) -> Span {
}

    pub fn set_span(&mut self, span: Span) {
}

    pub fn subspan<R: RangeBounds<usize>>(&self, _range: R) -> Option<Span> {
}
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
}
}

impl fmt::Debug for Literal {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
}
}

fn token_stream(mut input: Cursor) -> PResult<TokenStream> {
}

#[cfg(not(span_locations))]
fn spanned<'a, T>(
    input: Cursor<'a>,
    f: fn(Cursor<'a>) -> PResult<'a, T>,
) -> PResult<'a, (T, crate::Span)> {
}

#[cfg(span_locations)]
fn spanned<'a, T>(
    input: Cursor<'a>,
    f: fn(Cursor<'a>) -> PResult<'a, T>,
) -> PResult<'a, (T, crate::Span)> {
}

fn token_tree(input: Cursor) -> PResult<TokenTree> {
}

named!(token_kind -> TokenTree, alt!(
    map!(group, |g| TokenTree::Group(crate::Group::_new_stable(g)))
    |
    map!(literal, |l| TokenTree::Literal(crate::Literal::_new_stable(l))) // must be before symbol
    |
    map!(op, TokenTree::Punct)
    |
    symbol_leading_ws
));

named!(group -> Group, alt!(
    delimited!(
        punct!("("),
        token_stream,
        punct!(")")
    ) => { |ts| Group::new(Delimiter::Parenthesis, ts) }
    |
    delimited!(
        punct!("["),
        token_stream,
        punct!("]")
    ) => { |ts| Group::new(Delimiter::Bracket, ts) }
    |
    delimited!(
        punct!("{"),
        token_stream,
        punct!("}")
    ) => { |ts| Group::new(Delimiter::Brace, ts) }
));

fn symbol_leading_ws(input: Cursor) -> PResult<TokenTree> {
}

fn symbol(input: Cursor) -> PResult<TokenTree> {
}

fn symbol_not_raw(input: Cursor) -> PResult<&str> {
}

fn literal(input: Cursor) -> PResult<Literal> {
}

named!(literal_nocapture -> (), alt!(
    string
    |
    byte_string
    |
    byte
    |
    character
    |
    float
    |
    int
));

named!(string -> (), alt!(
    quoted_string
    |
    preceded!(
        punct!("r"),
        raw_string
    ) => { |_| () }
));

named!(quoted_string -> (), do_parse!(
    punct!("\"") >>
    cooked_string >>
    tag!("\"") >>
    option!(symbol_not_raw) >>
    (())
));

fn cooked_string(input: Cursor) -> PResult<()> {
}

named!(byte_string -> (), alt!(
    delimited!(
        punct!("b\""),
        cooked_byte_string,
        tag!("\"")
    ) => { |_| () }
    |
    preceded!(
        punct!("br"),
        raw_string
    ) => { |_| () }
));

fn cooked_byte_string(mut input: Cursor) -> PResult<()> {
}

fn raw_string(input: Cursor) -> PResult<()> {
}

named!(byte -> (), do_parse!(
    punct!("b") >>
    tag!("'") >>
    cooked_byte >>
    tag!("'") >>
    (())
));

fn cooked_byte(input: Cursor) -> PResult<()> {
}

named!(character -> (), do_parse!(
    punct!("'") >>
    cooked_char >>
    tag!("'") >>
    (())
));

fn cooked_char(input: Cursor) -> PResult<()> {
}

macro_rules! next_ch {
    ($chars:ident @ $pat:pat $(| $rest:pat)*) => {
        match $chars.next() {
            Some((_, ch)) => match ch {
                $pat $(| $rest)*  => ch,
                _ => return false,
            },
            None => return false
        }
    };
}

fn backslash_x_char<I>(chars: &mut I) -> bool
where
    I: Iterator<Item = (usize, char)>,
{
}

fn backslash_x_byte<I>(chars: &mut I) -> bool
where
    I: Iterator<Item = (usize, u8)>,
{
}

fn backslash_u<I>(chars: &mut I) -> bool
where
    I: Iterator<Item = (usize, char)>,
{
}

fn float(input: Cursor) -> PResult<()> {
}

fn float_digits(input: Cursor) -> PResult<()> {
}

fn int(input: Cursor) -> PResult<()> {
}

fn digits(mut input: Cursor) -> PResult<()> {
}

fn op(input: Cursor) -> PResult<Punct> {
}

fn op_char(input: Cursor) -> PResult<char> {
}

fn doc_comment(input: Cursor) -> PResult<Vec<TokenTree>> {
}

named!(doc_comment_contents -> (&str, bool), alt!(
    do_parse!(
        punct!("//!") >>
        s: take_until_newline_or_eof!() >>
        ((s, true))
    )
    |
    do_parse!(
        option!(whitespace) >>
        peek!(tag!("/*!")) >>
        s: block_comment >>
        ((s, true))
    )
    |
    do_parse!(
        punct!("///") >>
        not!(tag!("/")) >>
        s: take_until_newline_or_eof!() >>
        ((s, false))
    )
    |
    do_parse!(
        option!(whitespace) >>
        peek!(tuple!(tag!("/**"), not!(tag!("*")))) >>
        s: block_comment >>
        ((s, false))
    )
));
}

#[cfg(not(wrap_proc_macro))]
use crate::fallback as imp;
#[path = "wrapper.rs"]
#[cfg(wrap_proc_macro)]
mod imp {
use std::fmt;
use std::iter;
use std::ops::RangeBounds;
use std::panic::{self, PanicInfo};
#[cfg(super_unstable)]
use std::path::PathBuf;
use std::str::FromStr;

use crate::{fallback, Delimiter, Punct, Spacing, TokenTree};

#[derive(Clone)]
pub enum TokenStream {
    Compiler(DeferredTokenStream),
    Fallback(fallback::TokenStream),
}

// Work around https://github.com/rust-lang/rust/issues/65080.
// In `impl Extend<TokenTree> for TokenStream` which is used heavily by quote,
// we hold on to the appended tokens and do proc_macro::TokenStream::extend as
// late as possible to batch together consecutive uses of the Extend impl.
#[derive(Clone)]
pub struct DeferredTokenStream {
    stream: proc_macro::TokenStream,
    extra: Vec<proc_macro::TokenTree>,
}

pub enum LexError {
    Compiler(proc_macro::LexError),
    Fallback(fallback::LexError),
}

fn nightly_works() -> bool {
}

fn mismatch() -> ! {
}

impl DeferredTokenStream {
    fn new(stream: proc_macro::TokenStream) -> Self {
}

    fn is_empty(&self) -> bool {
}

    fn evaluate_now(&mut self) {
}

    fn into_token_stream(mut self) -> proc_macro::TokenStream {
}
}

impl TokenStream {
    pub fn new() -> TokenStream {
}

    pub fn is_empty(&self) -> bool {
}

    fn unwrap_nightly(self) -> proc_macro::TokenStream {
}

    fn unwrap_stable(self) -> fallback::TokenStream {
}
}

impl FromStr for TokenStream {
    type Err = LexError;

    fn from_str(src: &str) -> Result<TokenStream, LexError> {
}
}

impl fmt::Display for TokenStream {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
}
}

impl From<proc_macro::TokenStream> for TokenStream {
    fn from(inner: proc_macro::TokenStream) -> TokenStream {
}
}

impl From<TokenStream> for proc_macro::TokenStream {
    fn from(inner: TokenStream) -> proc_macro::TokenStream {
}
}

impl From<fallback::TokenStream> for TokenStream {
    fn from(inner: fallback::TokenStream) -> TokenStream {
}
}

// Assumes nightly_works().
fn into_compiler_token(token: TokenTree) -> proc_macro::TokenTree {
}

impl From<TokenTree> for TokenStream {
    fn from(token: TokenTree) -> TokenStream {
}
}

impl iter::FromIterator<TokenTree> for TokenStream {
    fn from_iter<I: IntoIterator<Item = TokenTree>>(trees: I) -> Self {
}
}

impl iter::FromIterator<TokenStream> for TokenStream {
    fn from_iter<I: IntoIterator<Item = TokenStream>>(streams: I) -> Self {
}
}

impl Extend<TokenTree> for TokenStream {
    fn extend<I: IntoIterator<Item = TokenTree>>(&mut self, streams: I) {
}
}

impl Extend<TokenStream> for TokenStream {
    fn extend<I: IntoIterator<Item = TokenStream>>(&mut self, streams: I) {
}
}

impl fmt::Debug for TokenStream {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
}
}

impl From<proc_macro::LexError> for LexError {
    fn from(e: proc_macro::LexError) -> LexError {
}
}

impl From<fallback::LexError> for LexError {
    fn from(e: fallback::LexError) -> LexError {
}
}

impl fmt::Debug for LexError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
}
}

#[derive(Clone)]
pub enum TokenTreeIter {
    Compiler(proc_macro::token_stream::IntoIter),
    Fallback(fallback::TokenTreeIter),
}

impl IntoIterator for TokenStream {
    type Item = TokenTree;
    type IntoIter = TokenTreeIter;

    fn into_iter(self) -> TokenTreeIter {
}
}

impl Iterator for TokenTreeIter {
    type Item = TokenTree;

    fn next(&mut self) -> Option<TokenTree> {
}

    fn size_hint(&self) -> (usize, Option<usize>) {
}
}

impl fmt::Debug for TokenTreeIter {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
}
}

#[derive(Clone, PartialEq, Eq)]
#[cfg(super_unstable)]
pub enum SourceFile {
    Compiler(proc_macro::SourceFile),
    Fallback(fallback::SourceFile),
}

#[cfg(super_unstable)]
impl SourceFile {
    fn nightly(sf: proc_macro::SourceFile) -> Self {
}

    /// Get the path to this source file as a string.
    pub fn path(&self) -> PathBuf {
}

    pub fn is_real(&self) -> bool {
}
}

#[cfg(super_unstable)]
impl fmt::Debug for SourceFile {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
}
}

#[cfg(any(super_unstable, feature = "span-locations"))]
pub struct LineColumn {
    pub line: usize,
    pub column: usize,
}

#[derive(Copy, Clone)]
pub enum Span {
    Compiler(proc_macro::Span),
    Fallback(fallback::Span),
}

impl Span {
    pub fn call_site() -> Span {
}

    #[cfg(super_unstable)]
    pub fn def_site() -> Span {
}

    #[cfg(super_unstable)]
    pub fn resolved_at(&self, other: Span) -> Span {
}

    #[cfg(super_unstable)]
    pub fn located_at(&self, other: Span) -> Span {
}

    pub fn unwrap(self) -> proc_macro::Span {
}

    #[cfg(super_unstable)]
    pub fn source_file(&self) -> SourceFile {
}

    #[cfg(any(super_unstable, feature = "span-locations"))]
    pub fn start(&self) -> LineColumn {
}

    #[cfg(any(super_unstable, feature = "span-locations"))]
    pub fn end(&self) -> LineColumn {
}

    pub fn join(&self, other: Span) -> Option<Span> {
}

    #[cfg(super_unstable)]
    pub fn eq(&self, other: &Span) -> bool {
}

    fn unwrap_nightly(self) -> proc_macro::Span {
}
}

impl From<proc_macro::Span> for crate::Span {
    fn from(proc_span: proc_macro::Span) -> crate::Span {
}
}

impl From<fallback::Span> for Span {
    fn from(inner: fallback::Span) -> Span {
}
}

impl fmt::Debug for Span {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
}
}

pub fn debug_span_field_if_nontrivial(debug: &mut fmt::DebugStruct, span: Span) {
}

#[derive(Clone)]
pub enum Group {
    Compiler(proc_macro::Group),
    Fallback(fallback::Group),
}

impl Group {
    pub fn new(delimiter: Delimiter, stream: TokenStream) -> Group {
}

    pub fn delimiter(&self) -> Delimiter {
}

    pub fn stream(&self) -> TokenStream {
}

    pub fn span(&self) -> Span {
}

    pub fn span_open(&self) -> Span {
}

    pub fn span_close(&self) -> Span {
}

    pub fn set_span(&mut self, span: Span) {
}

    fn unwrap_nightly(self) -> proc_macro::Group {
}
}

impl From<fallback::Group> for Group {
    fn from(g: fallback::Group) -> Self {
}
}

impl fmt::Display for Group {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
}
}

impl fmt::Debug for Group {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
}
}

#[derive(Clone)]
pub enum Ident {
    Compiler(proc_macro::Ident),
    Fallback(fallback::Ident),
}

impl Ident {
    pub fn new(string: &str, span: Span) -> Ident {
}

    pub fn new_raw(string: &str, span: Span) -> Ident {
}

    pub fn span(&self) -> Span {
}

    pub fn set_span(&mut self, span: Span) {
}

    fn unwrap_nightly(self) -> proc_macro::Ident {
}
}

impl PartialEq for Ident {
    fn eq(&self, other: &Ident) -> bool {
}
}

impl<T> PartialEq<T> for Ident
where
    T: ?Sized + AsRef<str>,
{
    fn eq(&self, other: &T) -> bool {
}
}

impl fmt::Display for Ident {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
}
}

impl fmt::Debug for Ident {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
}
}

#[derive(Clone)]
pub enum Literal {
    Compiler(proc_macro::Literal),
    Fallback(fallback::Literal),
}

macro_rules! suffixed_numbers {
    ($($name:ident => $kind:ident,)*) => ($(
        pub fn $name(n: $kind) -> Literal {
}
    )*)
}

macro_rules! unsuffixed_integers {
    ($($name:ident => $kind:ident,)*) => ($(
        pub fn $name(n: $kind) -> Literal {
}
    )*)
}

impl Literal {
    suffixed_numbers! {
        u8_suffixed => u8,
        u16_suffixed => u16,
        u32_suffixed => u32,
        u64_suffixed => u64,
        u128_suffixed => u128,
        usize_suffixed => usize,
        i8_suffixed => i8,
        i16_suffixed => i16,
        i32_suffixed => i32,
        i64_suffixed => i64,
        i128_suffixed => i128,
        isize_suffixed => isize,

        f32_suffixed => f32,
        f64_suffixed => f64,
    }

    unsuffixed_integers! {
        u8_unsuffixed => u8,
        u16_unsuffixed => u16,
        u32_unsuffixed => u32,
        u64_unsuffixed => u64,
        u128_unsuffixed => u128,
        usize_unsuffixed => usize,
        i8_unsuffixed => i8,
        i16_unsuffixed => i16,
        i32_unsuffixed => i32,
        i64_unsuffixed => i64,
        i128_unsuffixed => i128,
        isize_unsuffixed => isize,
    }

    pub fn f32_unsuffixed(f: f32) -> Literal {
}

    pub fn f64_unsuffixed(f: f64) -> Literal {
}

    pub fn string(t: &str) -> Literal {
}

    pub fn character(t: char) -> Literal {
}

    pub fn byte_string(bytes: &[u8]) -> Literal {
}

    pub fn span(&self) -> Span {
}

    pub fn set_span(&mut self, span: Span) {
}

    pub fn subspan<R: RangeBounds<usize>>(&self, range: R) -> Option<Span> {
}

    fn unwrap_nightly(self) -> proc_macro::Literal {
}
}

impl From<fallback::Literal> for Literal {
    fn from(s: fallback::Literal) -> Literal {
}
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
}
}

impl fmt::Debug for Literal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
}
}
}

/// An abstract stream of tokens, or more concretely a sequence of token trees.
///
/// This type provides interfaces for iterating over token trees and for
/// collecting token trees into one stream.
///
/// Token stream is both the input and output of `#[proc_macro]`,
/// `#[proc_macro_attribute]` and `#[proc_macro_derive]` definitions.
#[derive(Clone)]
pub struct TokenStream {
    inner: imp::TokenStream,
    _marker: marker::PhantomData<Rc<()>>,
}

/// Error returned from `TokenStream::from_str`.
pub struct LexError {
    inner: imp::LexError,
    _marker: marker::PhantomData<Rc<()>>,
}

impl TokenStream {
    fn _new(inner: imp::TokenStream) -> TokenStream {
}

    fn _new_stable(inner: fallback::TokenStream) -> TokenStream {
}

    /// Returns an empty `TokenStream` containing no token trees.
    pub fn new() -> TokenStream {
}

    /// Checks if this `TokenStream` is empty.
    pub fn is_empty(&self) -> bool {
}
}

/// `TokenStream::default()` returns an empty stream,
/// i.e. this is equivalent with `TokenStream::new()`.
impl Default for TokenStream {
    fn default() -> Self {
}
}

/// Attempts to break the string into tokens and parse those tokens into a token
/// stream.
///
/// May fail for a number of reasons, for example, if the string contains
/// unbalanced delimiters or characters not existing in the language.
///
/// NOTE: Some errors may cause panics instead of returning `LexError`. We
/// reserve the right to change these errors into `LexError`s later.
impl FromStr for TokenStream {
    type Err = LexError;

    fn from_str(src: &str) -> Result<TokenStream, LexError> {
}
}

#[cfg(use_proc_macro)]
impl From<proc_macro::TokenStream> for TokenStream {
    fn from(inner: proc_macro::TokenStream) -> TokenStream {
}
}

#[cfg(use_proc_macro)]
impl From<TokenStream> for proc_macro::TokenStream {
    fn from(inner: TokenStream) -> proc_macro::TokenStream {
}
}

impl From<TokenTree> for TokenStream {
    fn from(token: TokenTree) -> Self {
}
}

impl Extend<TokenTree> for TokenStream {
    fn extend<I: IntoIterator<Item = TokenTree>>(&mut self, streams: I) {
}
}

impl Extend<TokenStream> for TokenStream {
    fn extend<I: IntoIterator<Item = TokenStream>>(&mut self, streams: I) {
}
}

/// Collects a number of token trees into a single stream.
impl FromIterator<TokenTree> for TokenStream {
    fn from_iter<I: IntoIterator<Item = TokenTree>>(streams: I) -> Self {
}
}
impl FromIterator<TokenStream> for TokenStream {
    fn from_iter<I: IntoIterator<Item = TokenStream>>(streams: I) -> Self {
}
}

/// Prints the token stream as a string that is supposed to be losslessly
/// convertible back into the same token stream (modulo spans), except for
/// possibly `TokenTree::Group`s with `Delimiter::None` delimiters and negative
/// numeric literals.
impl fmt::Display for TokenStream {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
}
}

/// Prints token in a form convenient for debugging.
impl fmt::Debug for TokenStream {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
}
}

impl fmt::Debug for LexError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
}
}

/// The source file of a given `Span`.
///
/// This type is semver exempt and not exposed by default.
#[cfg(procmacro2_semver_exempt)]
#[derive(Clone, PartialEq, Eq)]
pub struct SourceFile {
    inner: imp::SourceFile,
    _marker: marker::PhantomData<Rc<()>>,
}

#[cfg(procmacro2_semver_exempt)]
impl SourceFile {
    fn _new(inner: imp::SourceFile) -> Self {
}

    /// Get the path to this source file.
    ///
    /// ### Note
    ///
    /// If the code span associated with this `SourceFile` was generated by an
    /// external macro, this may not be an actual path on the filesystem. Use
    /// [`is_real`] to check.
    ///
    /// Also note that even if `is_real` returns `true`, if
    /// `--remap-path-prefix` was passed on the command line, the path as given
    /// may not actually be valid.
    ///
    /// [`is_real`]: #method.is_real
    pub fn path(&self) -> PathBuf {
}

    /// Returns `true` if this source file is a real source file, and not
    /// generated by an external macro's expansion.
    pub fn is_real(&self) -> bool {
}
}

#[cfg(procmacro2_semver_exempt)]
impl fmt::Debug for SourceFile {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
}
}

/// A line-column pair representing the start or end of a `Span`.
///
/// This type is semver exempt and not exposed by default.
#[cfg(span_locations)]
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct LineColumn {
    /// The 1-indexed line in the source file on which the span starts or ends
    /// (inclusive).
    pub line: usize,
    /// The 0-indexed column (in UTF-8 characters) in the source file on which
    /// the span starts or ends (inclusive).
    pub column: usize,
}

/// A region of source code, along with macro expansion information.
#[derive(Copy, Clone)]
pub struct Span {
    inner: imp::Span,
    _marker: marker::PhantomData<Rc<()>>,
}

impl Span {
    fn _new(inner: imp::Span) -> Span {
}

    fn _new_stable(inner: fallback::Span) -> Span {
}

    /// The span of the invocation of the current procedural macro.
    ///
    /// Identifiers created with this span will be resolved as if they were
    /// written directly at the macro call location (call-site hygiene) and
    /// other code at the macro call site will be able to refer to them as well.
    pub fn call_site() -> Span {
}

    /// A span that resolves at the macro definition site.
    ///
    /// This method is semver exempt and not exposed by default.
    #[cfg(procmacro2_semver_exempt)]
    pub fn def_site() -> Span {
}

    /// Creates a new span with the same line/column information as `self` but
    /// that resolves symbols as though it were at `other`.
    ///
    /// This method is semver exempt and not exposed by default.
    #[cfg(procmacro2_semver_exempt)]
    pub fn resolved_at(&self, other: Span) -> Span {
}

    /// Creates a new span with the same name resolution behavior as `self` but
    /// with the line/column information of `other`.
    ///
    /// This method is semver exempt and not exposed by default.
    #[cfg(procmacro2_semver_exempt)]
    pub fn located_at(&self, other: Span) -> Span {
}

    /// Convert `proc_macro2::Span` to `proc_macro::Span`.
    ///
    /// This method is available when building with a nightly compiler, or when
    /// building with rustc 1.29+ *without* semver exempt features.
    ///
    /// # Panics
    ///
    /// Panics if called from outside of a procedural macro. Unlike
    /// `proc_macro2::Span`, the `proc_macro::Span` type can only exist within
    /// the context of a procedural macro invocation.
    #[cfg(wrap_proc_macro)]
    pub fn unwrap(self) -> proc_macro::Span {
}

    // Soft deprecated. Please use Span::unwrap.
    #[cfg(wrap_proc_macro)]
    #[doc(hidden)]
    pub fn unstable(self) -> proc_macro::Span {
}

    /// The original source file into which this span points.
    ///
    /// This method is semver exempt and not exposed by default.
    #[cfg(procmacro2_semver_exempt)]
    pub fn source_file(&self) -> SourceFile {
}

    /// Get the starting line/column in the source file for this span.
    ///
    /// This method requires the `"span-locations"` feature to be enabled.
    #[cfg(span_locations)]
    pub fn start(&self) -> LineColumn {
}

    /// Get the ending line/column in the source file for this span.
    ///
    /// This method requires the `"span-locations"` feature to be enabled.
    #[cfg(span_locations)]
    pub fn end(&self) -> LineColumn {
}

    /// Create a new span encompassing `self` and `other`.
    ///
    /// Returns `None` if `self` and `other` are from different files.
    ///
    /// Warning: the underlying [`proc_macro::Span::join`] method is
    /// nightly-only. When called from within a procedural macro not using a
    /// nightly compiler, this method will always return `None`.
    ///
    /// [`proc_macro::Span::join`]: https://doc.rust-lang.org/proc_macro/struct.Span.html#method.join
    pub fn join(&self, other: Span) -> Option<Span> {
}

    /// Compares two spans to see if they're equal.
    ///
    /// This method is semver exempt and not exposed by default.
    #[cfg(procmacro2_semver_exempt)]
    pub fn eq(&self, other: &Span) -> bool {
}
}

/// Prints a span in a form convenient for debugging.
impl fmt::Debug for Span {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
}
}

/// A single token or a delimited sequence of token trees (e.g. `[1, (), ..]`).
#[derive(Clone)]
pub enum TokenTree {
    /// A token stream surrounded by bracket delimiters.
    Group(Group),
    /// An identifier.
    Ident(Ident),
    /// A single punctuation character (`+`, `,`, `$`, etc.).
    Punct(Punct),
    /// A literal character (`'a'`), string (`"hello"`), number (`2.3`), etc.
    Literal(Literal),
}

impl TokenTree {
    /// Returns the span of this tree, delegating to the `span` method of
    /// the contained token or a delimited stream.
    pub fn span(&self) -> Span {
}

    /// Configures the span for *only this token*.
    ///
    /// Note that if this token is a `Group` then this method will not configure
    /// the span of each of the internal tokens, this will simply delegate to
    /// the `set_span` method of each variant.
    pub fn set_span(&mut self, span: Span) {
}
}

impl From<Group> for TokenTree {
    fn from(g: Group) -> TokenTree {
}
}

impl From<Ident> for TokenTree {
    fn from(g: Ident) -> TokenTree {
}
}

impl From<Punct> for TokenTree {
    fn from(g: Punct) -> TokenTree {
}
}

impl From<Literal> for TokenTree {
    fn from(g: Literal) -> TokenTree {
}
}

/// Prints the token tree as a string that is supposed to be losslessly
/// convertible back into the same token tree (modulo spans), except for
/// possibly `TokenTree::Group`s with `Delimiter::None` delimiters and negative
/// numeric literals.
impl fmt::Display for TokenTree {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
}
}

/// Prints token tree in a form convenient for debugging.
impl fmt::Debug for TokenTree {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
}
}

/// A delimited token stream.
///
/// A `Group` internally contains a `TokenStream` which is surrounded by
/// `Delimiter`s.
#[derive(Clone)]
pub struct Group {
    inner: imp::Group,
}

/// Describes how a sequence of token trees is delimited.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Delimiter {
    /// `( ... )`
    Parenthesis,
    /// `{ ... }`
    Brace,
    /// `[ ... ]`
    Bracket,
    /// `Ø ... Ø`
    ///
    /// An implicit delimiter, that may, for example, appear around tokens
    /// coming from a "macro variable" `$var`. It is important to preserve
    /// operator priorities in cases like `$var * 3` where `$var` is `1 + 2`.
    /// Implicit delimiters may not survive roundtrip of a token stream through
    /// a string.
    None,
}

impl Group {
    fn _new(inner: imp::Group) -> Self {
}

    fn _new_stable(inner: fallback::Group) -> Self {
}

    /// Creates a new `Group` with the given delimiter and token stream.
    ///
    /// This constructor will set the span for this group to
    /// `Span::call_site()`. To change the span you can use the `set_span`
    /// method below.
    pub fn new(delimiter: Delimiter, stream: TokenStream) -> Group {
}

    /// Returns the delimiter of this `Group`
    pub fn delimiter(&self) -> Delimiter {
}

    /// Returns the `TokenStream` of tokens that are delimited in this `Group`.
    ///
    /// Note that the returned token stream does not include the delimiter
    /// returned above.
    pub fn stream(&self) -> TokenStream {
}

    /// Returns the span for the delimiters of this token stream, spanning the
    /// entire `Group`.
    ///
    /// ```text
    /// pub fn span(&self) -> Span {
    ///            ^^^^^^^
    /// ```
    pub fn span(&self) -> Span {
}

    /// Returns the span pointing to the opening delimiter of this group.
    ///
    /// ```text
    /// pub fn span_open(&self) -> Span {
    ///                 ^
    /// ```
    pub fn span_open(&self) -> Span {
}

    /// Returns the span pointing to the closing delimiter of this group.
    ///
    /// ```text
    /// pub fn span_close(&self) -> Span {
    ///                        ^
    /// ```
    pub fn span_close(&self) -> Span {
}

    /// Configures the span for this `Group`'s delimiters, but not its internal
    /// tokens.
    ///
    /// This method will **not** set the span of all the internal tokens spanned
    /// by this group, but rather it will only set the span of the delimiter
    /// tokens at the level of the `Group`.
    pub fn set_span(&mut self, span: Span) {
}
}

/// Prints the group as a string that should be losslessly convertible back
/// into the same group (modulo spans), except for possibly `TokenTree::Group`s
/// with `Delimiter::None` delimiters.
impl fmt::Display for Group {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
}
}

impl fmt::Debug for Group {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
}
}

/// An `Punct` is an single punctuation character like `+`, `-` or `#`.
///
/// Multicharacter operators like `+=` are represented as two instances of
/// `Punct` with different forms of `Spacing` returned.
#[derive(Clone)]
pub struct Punct {
    op: char,
    spacing: Spacing,
    span: Span,
}

/// Whether an `Punct` is followed immediately by another `Punct` or followed by
/// another token or whitespace.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Spacing {
    /// E.g. `+` is `Alone` in `+ =`, `+ident` or `+()`.
    Alone,
    /// E.g. `+` is `Joint` in `+=` or `'` is `Joint` in `'#`.
    ///
    /// Additionally, single quote `'` can join with identifiers to form
    /// lifetimes `'ident`.
    Joint,
}

impl Punct {
    /// Creates a new `Punct` from the given character and spacing.
    ///
    /// The `ch` argument must be a valid punctuation character permitted by the
    /// language, otherwise the function will panic.
    ///
    /// The returned `Punct` will have the default span of `Span::call_site()`
    /// which can be further configured with the `set_span` method below.
    pub fn new(op: char, spacing: Spacing) -> Punct {
}

    /// Returns the value of this punctuation character as `char`.
    pub fn as_char(&self) -> char {
}

    /// Returns the spacing of this punctuation character, indicating whether
    /// it's immediately followed by another `Punct` in the token stream, so
    /// they can potentially be combined into a multicharacter operator
    /// (`Joint`), or it's followed by some other token or whitespace (`Alone`)
    /// so the operator has certainly ended.
    pub fn spacing(&self) -> Spacing {
}

    /// Returns the span for this punctuation character.
    pub fn span(&self) -> Span {
}

    /// Configure the span for this punctuation character.
    pub fn set_span(&mut self, span: Span) {
}
}

/// Prints the punctuation character as a string that should be losslessly
/// convertible back into the same character.
impl fmt::Display for Punct {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
}
}

impl fmt::Debug for Punct {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
}
}

/// A word of Rust code, which may be a keyword or legal variable name.
///
/// An identifier consists of at least one Unicode code point, the first of
/// which has the XID_Start property and the rest of which have the XID_Continue
/// property.
///
/// - The empty string is not an identifier. Use `Option<Ident>`.
/// - A lifetime is not an identifier. Use `syn::Lifetime` instead.
///
/// An identifier constructed with `Ident::new` is permitted to be a Rust
/// keyword, though parsing one through its [`Parse`] implementation rejects
/// Rust keywords. Use `input.call(Ident::parse_any)` when parsing to match the
/// behaviour of `Ident::new`.
///
/// [`Parse`]: https://docs.rs/syn/1.0/syn/parse/trait.Parse.html
///
/// # Examples
///
/// A new ident can be created from a string using the `Ident::new` function.
/// A span must be provided explicitly which governs the name resolution
/// behavior of the resulting identifier.
///
/// ```
/// use proc_macro2::{Ident, Span};
///
/// fn main() {
///     let call_ident = Ident::new("calligraphy", Span::call_site());
///
///     println!("{}", call_ident);
/// }
/// ```
///
/// An ident can be interpolated into a token stream using the `quote!` macro.
///
/// ```
/// use proc_macro2::{Ident, Span};
/// use quote::quote;
///
/// fn main() {
///     let ident = Ident::new("demo", Span::call_site());
///
///     // Create a variable binding whose name is this ident.
///     let expanded = quote! { let #ident = 10; };
///
///     // Create a variable binding with a slightly different name.
///     let temp_ident = Ident::new(&format!("new_{}", ident), Span::call_site());
///     let expanded = quote! { let #temp_ident = 10; };
/// }
/// ```
///
/// A string representation of the ident is available through the `to_string()`
/// method.
///
/// ```
/// # use proc_macro2::{Ident, Span};
/// #
/// # let ident = Ident::new("another_identifier", Span::call_site());
/// #
/// // Examine the ident as a string.
/// let ident_string = ident.to_string();
/// if ident_string.len() > 60 {
///     println!("Very long identifier: {}", ident_string)
/// }
/// ```
#[derive(Clone)]
pub struct Ident {
    inner: imp::Ident,
    _marker: marker::PhantomData<Rc<()>>,
}

impl Ident {
    fn _new(inner: imp::Ident) -> Ident {
}

    /// Creates a new `Ident` with the given `string` as well as the specified
    /// `span`.
    ///
    /// The `string` argument must be a valid identifier permitted by the
    /// language, otherwise the function will panic.
    ///
    /// Note that `span`, currently in rustc, configures the hygiene information
    /// for this identifier.
    ///
    /// As of this time `Span::call_site()` explicitly opts-in to "call-site"
    /// hygiene meaning that identifiers created with this span will be resolved
    /// as if they were written directly at the location of the macro call, and
    /// other code at the macro call site will be able to refer to them as well.
    ///
    /// Later spans like `Span::def_site()` will allow to opt-in to
    /// "definition-site" hygiene meaning that identifiers created with this
    /// span will be resolved at the location of the macro definition and other
    /// code at the macro call site will not be able to refer to them.
    ///
    /// Due to the current importance of hygiene this constructor, unlike other
    /// tokens, requires a `Span` to be specified at construction.
    ///
    /// # Panics
    ///
    /// Panics if the input string is neither a keyword nor a legal variable
    /// name. If you are not sure whether the string contains an identifier and
    /// need to handle an error case, use
    /// <a href="https://docs.rs/syn/1.0/syn/fn.parse_str.html"><code
    ///   style="padding-right:0;">syn::parse_str</code></a><code
    ///   style="padding-left:0;">::&lt;Ident&gt;</code>
    /// rather than `Ident::new`.
    pub fn new(string: &str, span: Span) -> Ident {
}

    /// Same as `Ident::new`, but creates a raw identifier (`r#ident`).
    ///
    /// This method is semver exempt and not exposed by default.
    #[cfg(procmacro2_semver_exempt)]
    pub fn new_raw(string: &str, span: Span) -> Ident {
}

    fn _new_raw(string: &str, span: Span) -> Ident {
}

    /// Returns the span of this `Ident`.
    pub fn span(&self) -> Span {
}

    /// Configures the span of this `Ident`, possibly changing its hygiene
    /// context.
    pub fn set_span(&mut self, span: Span) {
}
}

impl PartialEq for Ident {
    fn eq(&self, other: &Ident) -> bool {
}
}

impl<T> PartialEq<T> for Ident
where
    T: ?Sized + AsRef<str>,
{
    fn eq(&self, other: &T) -> bool {
}
}

impl Eq for Ident {}

impl PartialOrd for Ident {
    fn partial_cmp(&self, other: &Ident) -> Option<Ordering> {
}
}

impl Ord for Ident {
    fn cmp(&self, other: &Ident) -> Ordering {
}
}

impl Hash for Ident {
    fn hash<H: Hasher>(&self, hasher: &mut H) {
}
}

/// Prints the identifier as a string that should be losslessly convertible back
/// into the same identifier.
impl fmt::Display for Ident {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
}
}

impl fmt::Debug for Ident {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
}
}

/// A literal string (`"hello"`), byte string (`b"hello"`), character (`'a'`),
/// byte character (`b'a'`), an integer or floating point number with or without
/// a suffix (`1`, `1u8`, `2.3`, `2.3f32`).
///
/// Boolean literals like `true` and `false` do not belong here, they are
/// `Ident`s.
#[derive(Clone)]
pub struct Literal {
    inner: imp::Literal,
    _marker: marker::PhantomData<Rc<()>>,
}

macro_rules! suffixed_int_literals {
    ($($name:ident => $kind:ident,)*) => ($(
        /// Creates a new suffixed integer literal with the specified value.
        ///
        /// This function will create an integer like `1u32` where the integer
        /// value specified is the first part of the token and the integral is
        /// also suffixed at the end. Literals created from negative numbers may
        /// not survive rountrips through `TokenStream` or strings and may be
        /// broken into two tokens (`-` and positive literal).
        ///
        /// Literals created through this method have the `Span::call_site()`
        /// span by default, which can be configured with the `set_span` method
        /// below.
        pub fn $name(n: $kind) -> Literal {
}
    )*)
}

macro_rules! unsuffixed_int_literals {
    ($($name:ident => $kind:ident,)*) => ($(
        /// Creates a new unsuffixed integer literal with the specified value.
        ///
        /// This function will create an integer like `1` where the integer
        /// value specified is the first part of the token. No suffix is
        /// specified on this token, meaning that invocations like
        /// `Literal::i8_unsuffixed(1)` are equivalent to
        /// `Literal::u32_unsuffixed(1)`. Literals created from negative numbers
        /// may not survive rountrips through `TokenStream` or strings and may
        /// be broken into two tokens (`-` and positive literal).
        ///
        /// Literals created through this method have the `Span::call_site()`
        /// span by default, which can be configured with the `set_span` method
        /// below.
        pub fn $name(n: $kind) -> Literal {
}
    )*)
}

impl Literal {
    fn _new(inner: imp::Literal) -> Literal {
}

    fn _new_stable(inner: fallback::Literal) -> Literal {
}

    suffixed_int_literals! {
        u8_suffixed => u8,
        u16_suffixed => u16,
        u32_suffixed => u32,
        u64_suffixed => u64,
        u128_suffixed => u128,
        usize_suffixed => usize,
        i8_suffixed => i8,
        i16_suffixed => i16,
        i32_suffixed => i32,
        i64_suffixed => i64,
        i128_suffixed => i128,
        isize_suffixed => isize,
    }

    unsuffixed_int_literals! {
        u8_unsuffixed => u8,
        u16_unsuffixed => u16,
        u32_unsuffixed => u32,
        u64_unsuffixed => u64,
        u128_unsuffixed => u128,
        usize_unsuffixed => usize,
        i8_unsuffixed => i8,
        i16_unsuffixed => i16,
        i32_unsuffixed => i32,
        i64_unsuffixed => i64,
        i128_unsuffixed => i128,
        isize_unsuffixed => isize,
    }

    /// Creates a new unsuffixed floating-point literal.
    ///
    /// This constructor is similar to those like `Literal::i8_unsuffixed` where
    /// the float's value is emitted directly into the token but no suffix is
    /// used, so it may be inferred to be a `f64` later in the compiler.
    /// Literals created from negative numbers may not survive rountrips through
    /// `TokenStream` or strings and may be broken into two tokens (`-` and
    /// positive literal).
    ///
    /// # Panics
    ///
    /// This function requires that the specified float is finite, for example
    /// if it is infinity or NaN this function will panic.
    pub fn f64_unsuffixed(f: f64) -> Literal {
}

    /// Creates a new suffixed floating-point literal.
    ///
    /// This constructor will create a literal like `1.0f64` where the value
    /// specified is the preceding part of the token and `f64` is the suffix of
    /// the token. This token will always be inferred to be an `f64` in the
    /// compiler. Literals created from negative numbers may not survive
    /// rountrips through `TokenStream` or strings and may be broken into two
    /// tokens (`-` and positive literal).
    ///
    /// # Panics
    ///
    /// This function requires that the specified float is finite, for example
    /// if it is infinity or NaN this function will panic.
    pub fn f64_suffixed(f: f64) -> Literal {
}

    /// Creates a new unsuffixed floating-point literal.
    ///
    /// This constructor is similar to those like `Literal::i8_unsuffixed` where
    /// the float's value is emitted directly into the token but no suffix is
    /// used, so it may be inferred to be a `f64` later in the compiler.
    /// Literals created from negative numbers may not survive rountrips through
    /// `TokenStream` or strings and may be broken into two tokens (`-` and
    /// positive literal).
    ///
    /// # Panics
    ///
    /// This function requires that the specified float is finite, for example
    /// if it is infinity or NaN this function will panic.
    pub fn f32_unsuffixed(f: f32) -> Literal {
}

    /// Creates a new suffixed floating-point literal.
    ///
    /// This constructor will create a literal like `1.0f32` where the value
    /// specified is the preceding part of the token and `f32` is the suffix of
    /// the token. This token will always be inferred to be an `f32` in the
    /// compiler. Literals created from negative numbers may not survive
    /// rountrips through `TokenStream` or strings and may be broken into two
    /// tokens (`-` and positive literal).
    ///
    /// # Panics
    ///
    /// This function requires that the specified float is finite, for example
    /// if it is infinity or NaN this function will panic.
    pub fn f32_suffixed(f: f32) -> Literal {
}

    /// String literal.
    pub fn string(string: &str) -> Literal {
}

    /// Character literal.
    pub fn character(ch: char) -> Literal {
}

    /// Byte string literal.
    pub fn byte_string(s: &[u8]) -> Literal {
}

    /// Returns the span encompassing this literal.
    pub fn span(&self) -> Span {
}

    /// Configures the span associated for this literal.
    pub fn set_span(&mut self, span: Span) {
}

    /// Returns a `Span` that is a subset of `self.span()` containing only
    /// the source bytes in range `range`. Returns `None` if the would-be
    /// trimmed span is outside the bounds of `self`.
    ///
    /// Warning: the underlying [`proc_macro::Literal::subspan`] method is
    /// nightly-only. When called from within a procedural macro not using a
    /// nightly compiler, this method will always return `None`.
    ///
    /// [`proc_macro::Literal::subspan`]: https://doc.rust-lang.org/proc_macro/struct.Literal.html#method.subspan
    pub fn subspan<R: RangeBounds<usize>>(&self, range: R) -> Option<Span> {
}
}

impl fmt::Debug for Literal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
}
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
}
}

/// Public implementation details for the `TokenStream` type, such as iterators.
pub mod token_stream {
    use std::fmt;
    use std::marker;
    use std::rc::Rc;

    pub use crate::TokenStream;
    use crate::{imp, TokenTree};

    /// An iterator over `TokenStream`'s `TokenTree`s.
    ///
    /// The iteration is "shallow", e.g. the iterator doesn't recurse into
    /// delimited groups, and returns whole groups as token trees.
    #[derive(Clone)]
    pub struct IntoIter {
        inner: imp::TokenTreeIter,
        _marker: marker::PhantomData<Rc<()>>,
    }

    impl Iterator for IntoIter {
        type Item = TokenTree;

        fn next(&mut self) -> Option<TokenTree> {
}
    }

    impl fmt::Debug for IntoIter {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
}
    }

    impl IntoIterator for TokenStream {
        type Item = TokenTree;
        type IntoIter = IntoIter;

        fn into_iter(self) -> IntoIter {
}
    }
}
