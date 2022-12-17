//! [![github]](https://github.com/dtolnay/thiserror)&ensp;[![crates-io]](https://crates.io/crates/thiserror)&ensp;[![docs-rs]](https://docs.rs/thiserror)
//!
//! [github]: https://studio.orai.network/assets/img/github.svg
//! [crates-io]: https://studio.orai.network/assets/img/crates-io.svg
//! [docs-rs]: https://studio.orai.network/assets/img/docs-rs.svg
//!
//! <br>
//!
//! This library provides a convenient derive macro for the standard library's
//! [`std::error::Error`] trait.
//!
//! [`std::error::Error`]: https://doc.rust-lang.org/std/error/trait.Error.html
//!
//! <br>
//!
//! # Example
//!
//! ```rust
//! # use std::io;
//! use thiserror::Error;
//!
//! #[derive(Error, Debug)]
//! pub enum DataStoreError {
//!     #[error("data store disconnected")]
//!     Disconnect(#[from] io::Error),
//!     #[error("the data for key `{0}` is not available")]
//!     Redaction(String),
//!     #[error("invalid header (expected {expected:?}, found {found:?})")]
//!     InvalidHeader {
//!         expected: String,
//!         found: String,
//!     },
//!     #[error("unknown data store error")]
//!     Unknown,
//! }
//! ```
//!
//! <br>
//!
//! # Details
//!
//! - Thiserror deliberately does not appear in your public API. You get the
//!   same thing as if you had written an implementation of `std::error::Error`
//!   by hand, and switching from handwritten impls to thiserror or vice versa
//!   is not a breaking change.
//!
//! - Errors may be enums, structs with named fields, tuple structs, or unit
//!   structs.
//!
//! - A `Display` impl is generated for your error if you provide
//!   `#[error("...")]` messages on the struct or each variant of your enum, as
//!   shown above in the example.
//!
//!   The messages support a shorthand for interpolating fields from the error.
//!
//!     - `#[error("{var}")]`&ensp;⟶&ensp;`write!("{}", self.var)`
//!     - `#[error("{0}")]`&ensp;⟶&ensp;`write!("{}", self.0)`
//!     - `#[error("{var:?}")]`&ensp;⟶&ensp;`write!("{:?}", self.var)`
//!     - `#[error("{0:?}")]`&ensp;⟶&ensp;`write!("{:?}", self.0)`
//!
//!   These shorthands can be used together with any additional format args,
//!   which may be arbitrary expressions. For example:
//!
//!   ```rust
//!   # use std::i32;
//!   # use thiserror::Error;
//!   #
//!   #[derive(Error, Debug)]
//!   pub enum Error {
//!       #[error("invalid rdo_lookahead_frames {0} (expected < {})", i32::MAX)]
//!       InvalidLookahead(u32),
//!   }
//!   ```
//!
//!   If one of the additional expression arguments needs to refer to a field of
//!   the struct or enum, then refer to named fields as `.var` and tuple fields
//!   as `.0`.
//!
//!   ```rust
//!   # use thiserror::Error;
//!   #
//!   # fn first_char(s: &String) -> char {
//!   #     s.chars().next().unwrap()
//!   # }
//!   #
//!   # #[derive(Debug)]
//!   # struct Limits {
//!   #     lo: usize,
//!   #     hi: usize,
//!   # }
//!   #
//!   #[derive(Error, Debug)]
//!   pub enum Error {
//!       #[error("first letter must be lowercase but was {:?}", first_char(.0))]
//!       WrongCase(String),
//!       #[error("invalid index {idx}, expected at least {} and at most {}", .limits.lo, .limits.hi)]
//!       OutOfBounds { idx: usize, limits: Limits },
//!   }
//!   ```
//!
//! - A `From` impl is generated for each variant containing a `#[from]`
//!   attribute.
//!
//!   Note that the variant must not contain any other fields beyond the source
//!   error and possibly a backtrace. A backtrace is captured from within the
//!   `From` impl if there is a field for it.
//!
//!   ```rust
//!   # const IGNORE: &str = stringify! {
//!   #[derive(Error, Debug)]
//!   pub enum MyError {
//!       Io {
//!           #[from]
//!           source: io::Error,
//!           backtrace: Backtrace,
//!       },
//!   }
//!   # };
//!   ```
//!
//! - The Error trait's `source()` method is implemented to return whichever
//!   field has a `#[source]` attribute or is named `source`, if any. This is
//!   for identifying the underlying lower level error that caused your error.
//!
//!   The `#[from]` attribute always implies that the same field is `#[source]`,
//!   so you don't ever need to specify both attributes.
//!
//!   Any error type that implements `std::error::Error` or dereferences to `dyn
//!   std::error::Error` will work as a source.
//!
//!   ```rust
//!   # use std::fmt::{self, Display};
//!   # use thiserror::Error;
//!   #
//!   #[derive(Error, Debug)]
//!   pub struct MyError {
//!       msg: String,
//!       #[source]  // optional if field name is `source`
//!       source: anyhow::Error,
//!   }
//!   #
//!   # impl Display for MyError {
//!   #     fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
//!   #         unimplemented!()
//!   #     }
//!   # }
//!   ```
//!
//! - The Error trait's `backtrace()` method is implemented to return whichever
//!   field has a type named `Backtrace`, if any.
//!
//!   ```rust
//!   # const IGNORE: &str = stringify! {
//!   use std::backtrace::Backtrace;
//!
//!   #[derive(Error, Debug)]
//!   pub struct MyError {
//!       msg: String,
//!       backtrace: Backtrace,  // automatically detected
//!   }
//!   # };
//!   ```
//!
//! - Errors may use `error(transparent)` to forward the source and Display
//!   methods straight through to an underlying error without adding an
//!   additional message. This would be appropriate for enums that need an
//!   "anything else" variant.
//!
//!   ```
//!   # use thiserror::Error;
//!   #
//!   #[derive(Error, Debug)]
//!   pub enum MyError {
//!       # /*
//!       ...
//!       # */
//!
//!       #[error(transparent)]
//!       Other(#[from] anyhow::Error),  // source and Display delegate to anyhow::Error
//!   }
//!   ```
//!
//! - See also the [`anyhow`] library for a convenient single error type to use
//!   in application code.
//!
//!   [`anyhow`]: https://github.com/dtolnay/anyhow

mod aserror {
use std::error::Error;

pub trait AsDynError {
    fn as_dyn_error(&self) -> &(dyn Error + 'static);
}

impl<T: Error + 'static> AsDynError for T {
    #[inline]
    fn as_dyn_error(&self) -> &(dyn Error + 'static) {
}
}

impl AsDynError for dyn Error + 'static {
    #[inline]
    fn as_dyn_error(&self) -> &(dyn Error + 'static) {
}
}

impl AsDynError for dyn Error + Send + 'static {
    #[inline]
    fn as_dyn_error(&self) -> &(dyn Error + 'static) {
}
}

impl AsDynError for dyn Error + Send + Sync + 'static {
    #[inline]
    fn as_dyn_error(&self) -> &(dyn Error + 'static) {
}
}
}
mod display {
use std::fmt::Display;
use std::path::{self, Path, PathBuf};

pub trait DisplayAsDisplay {
    fn as_display(&self) -> Self;
}

impl<T: Display> DisplayAsDisplay for &T {
    fn as_display(&self) -> Self {
}
}

pub trait PathAsDisplay {
    fn as_display(&self) -> path::Display<'_>;
}

impl PathAsDisplay for Path {
    fn as_display(&self) -> path::Display<'_> {
}
}

impl PathAsDisplay for PathBuf {
    fn as_display(&self) -> path::Display<'_> {
}
}
}

pub use thiserror_impl::*;

// Not public API.
#[doc(hidden)]
pub mod private {
    pub use crate::aserror::AsDynError;
    pub use crate::display::{DisplayAsDisplay, PathAsDisplay};
}
