#![cfg_attr(feature = "backtraces", feature(backtrace))]

// Exposed on all platforms

mod addresses {
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use sha2::{
    digest::{Digest, Update},
    Sha256,
};
use std::borrow::Cow;
use std::fmt;
use std::ops::Deref;

use crate::{binary::Binary, HexBinary};

/// A human readable address.
///
/// In Cosmos, this is typically bech32 encoded. But for multi-chain smart contracts no
/// assumptions should be made other than being UTF-8 encoded and of reasonable length.
///
/// This type represents a validated address. It can be created in the following ways
/// 1. Use `Addr::unchecked(input)`
/// 2. Use `let checked: Addr = deps.api.addr_validate(input)?`
/// 3. Use `let checked: Addr = deps.api.addr_humanize(canonical_addr)?`
/// 4. Deserialize from JSON. This must only be done from JSON that was validated before
///    such as a contract's state. `Addr` must not be used in messages sent by the user
///    because this would result in unvalidated instances.
///
/// This type is immutable. If you really need to mutate it (Really? Are you sure?), create
/// a mutable copy using `let mut mutable = Addr::to_string()` and operate on that `String`
/// instance.
#[derive(
    Serialize, Deserialize, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, JsonSchema,
)]
pub struct Addr(String);

impl Addr {
    /// Creates a new `Addr` instance from the given input without checking the validity
    /// of the input. Since `Addr` must always contain valid addresses, the caller is
    /// responsible for ensuring the input is valid.
    ///
    /// Use this in cases where the address was validated before or in test code.
    /// If you see this in contract code, it should most likely be replaced with
    /// `let checked: Addr = deps.api.addr_humanize(canonical_addr)?`.
    ///
    /// ## Examples
    ///
    /// ```
    /// # use cosmwasm_std::{Addr};
    /// let address = Addr::unchecked("foobar");
    /// assert_eq!(address, "foobar");
    /// ```
    pub fn unchecked(input: impl Into<String>) -> Addr {}

    #[inline]
    pub fn as_str(&self) -> &str {}

    /// Returns the UTF-8 encoded address string as a byte array.
    ///
    /// This is equivalent to `address.as_str().as_bytes()`.
    #[inline]
    pub fn as_bytes(&self) -> &[u8] {}

    /// Utility for explicit conversion to `String`.
    #[inline]
    pub fn into_string(self) -> String {}
}

impl fmt::Display for Addr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {}
}

impl AsRef<str> for Addr {
    #[inline]
    fn as_ref(&self) -> &str {}
}

/// Implement `Addr == &str`
impl PartialEq<&str> for Addr {
    fn eq(&self, rhs: &&str) -> bool {}
}

/// Implement `&str == Addr`
impl PartialEq<Addr> for &str {
    fn eq(&self, rhs: &Addr) -> bool {}
}

/// Implement `Addr == String`
impl PartialEq<String> for Addr {
    fn eq(&self, rhs: &String) -> bool {}
}

/// Implement `String == Addr`
impl PartialEq<Addr> for String {
    fn eq(&self, rhs: &Addr) -> bool {}
}

// Addr->String is a safe conversion.
// However, the opposite direction is unsafe and must not be implemented.

impl From<Addr> for String {
    fn from(addr: Addr) -> Self {}
}

impl From<&Addr> for String {
    fn from(addr: &Addr) -> Self {}
}

impl From<Addr> for Cow<'_, Addr> {
    fn from(addr: Addr) -> Self {}
}

impl<'a> From<&'a Addr> for Cow<'a, Addr> {
    fn from(addr: &'a Addr) -> Self {}
}

/// A blockchain address in its binary form.
///
/// The specific implementation is up to the underlying chain and CosmWasm as well as
/// contracts should not make assumptions on that data. In Ethereum for example, an
/// `Addr` would contain a user visible address like 0x14d3cc818735723ab86eaf9502376e847a64ddad
/// and the corresponding `CanonicalAddr` would store the 20 bytes 0x14, 0xD3, ..., 0xAD.
/// In Cosmos, the bech32 format is used for `Addr`s and the `CanonicalAddr` holds the
/// encoded bech32 data without the checksum. Typical sizes are 20 bytes for externally
/// owned addresses and 32 bytes for module addresses (such as x/wasm contract addresses).
/// That being said, a chain might decide to use any size other than 20 or 32 bytes.
///
/// The safe way to obtain a valid `CanonicalAddr` is using `Api::addr_canonicalize`. In
/// addition to that there are many unsafe ways to convert any binary data into an instance.
/// So the type shoud be treated as a marker to express the intended data type, not as
/// a validity guarantee of any sort.
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, Hash, JsonSchema)]
pub struct CanonicalAddr(pub Binary);

/// Implement `CanonicalAddr == Binary`
impl PartialEq<Binary> for CanonicalAddr {
    fn eq(&self, rhs: &Binary) -> bool {}
}

/// Implement `Binary == CanonicalAddr`
impl PartialEq<CanonicalAddr> for Binary {
    fn eq(&self, rhs: &CanonicalAddr) -> bool {}
}

/// Implement `CanonicalAddr == HexBinary`
impl PartialEq<HexBinary> for CanonicalAddr {
    fn eq(&self, rhs: &HexBinary) -> bool {}
}

/// Implement `HexBinary == CanonicalAddr`
impl PartialEq<CanonicalAddr> for HexBinary {
    fn eq(&self, rhs: &CanonicalAddr) -> bool {}
}

impl From<&[u8]> for CanonicalAddr {
    fn from(source: &[u8]) -> Self {}
}

// Array reference
impl<const LENGTH: usize> From<&[u8; LENGTH]> for CanonicalAddr {
    fn from(source: &[u8; LENGTH]) -> Self {}
}

// Owned array
impl<const LENGTH: usize> From<[u8; LENGTH]> for CanonicalAddr {
    fn from(source: [u8; LENGTH]) -> Self {}
}

// Owned vector -> CanonicalAddr
impl From<Vec<u8>> for CanonicalAddr {
    fn from(source: Vec<u8>) -> Self {}
}

// CanonicalAddr -> Owned vector
impl From<CanonicalAddr> for Vec<u8> {
    fn from(source: CanonicalAddr) -> Vec<u8> {}
}

// Owned Binary -> CanonicalAddr
impl From<Binary> for CanonicalAddr {
    fn from(source: Binary) -> Self {}
}

// CanonicalAddr -> Owned Binary
impl From<CanonicalAddr> for Binary {
    fn from(source: CanonicalAddr) -> Binary {}
}

// Owned HexBinary -> CanonicalAddr
impl From<HexBinary> for CanonicalAddr {
    fn from(source: HexBinary) -> Self {}
}

// CanonicalAddr -> Owned HexBinary
impl From<CanonicalAddr> for HexBinary {
    fn from(source: CanonicalAddr) -> HexBinary {}
}

/// Just like Vec<u8>, CanonicalAddr is a smart pointer to [u8].
/// This implements `*canonical_address` for us and allows us to
/// do `&*canonical_address`, returning a `&[u8]` from a `&CanonicalAddr`.
/// With [deref coercions](https://doc.rust-lang.org/1.22.1/book/first-edition/deref-coercions.html#deref-coercions),
/// this allows us to use `&canonical_address` whenever a `&[u8]` is required.
impl Deref for CanonicalAddr {
    type Target = [u8];

    fn deref(&self) -> &Self::Target {}
}

impl CanonicalAddr {
    pub fn as_slice(&self) -> &[u8] {}
}

impl fmt::Display for CanonicalAddr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {}
}

#[derive(Debug)]
pub enum Instantiate2AddressError {
    /// Checksum must be 32 bytes
    InvalidChecksumLength,
    /// Salt must be between 1 and 64 bytes
    InvalidSaltLength,
}

/// Creates a contract address using the predictable address format introduced with
/// wasmd 0.29. When using instantiate2, this is a way to precompute the address.
/// When using instantiate, the contract address will use a different algorithm and
/// cannot be pre-computed as it contains inputs from the chain's state at the time of
/// message execution.
///
/// The predicable address format of instantiate2 is stable. But bear in mind this is
/// a powerful tool that requires multiple software components to work together smoothly.
/// It should be used carefully and tested thoroughly to avoid the loss of funds.
///
/// This method operates on [`CanonicalAddr`] to be implemented without chain interaction.
/// The typical usage looks like this:
///
/// ```
/// # use cosmwasm_std::{
/// #     HexBinary,
/// #     Storage, Api, Querier, DepsMut, Deps, entry_point, Env, StdError, MessageInfo,
/// #     Response, QueryResponse,
/// # };
/// # type ExecuteMsg = ();
/// use cosmwasm_std::instantiate2_address;
///
/// #[entry_point]
/// pub fn execute(
///     deps: DepsMut,
///     env: Env,
///     info: MessageInfo,
///     msg: ExecuteMsg,
/// ) -> Result<Response, StdError> {}
/// ```
pub fn instantiate2_address(
    checksum: &[u8],
    creator: &CanonicalAddr,
    salt: &[u8],
    msg: Option<&[u8]>,
) -> Result<CanonicalAddr, Instantiate2AddressError> {}

/// The "Basic Address" Hash from
/// https://github.com/cosmos/cosmos-sdk/blob/v0.45.8/docs/architecture/adr-028-public-key-addresses.md
fn hash(ty: &str, key: &[u8]) -> Vec<u8> {}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::HexBinary;
    use hex_literal::hex;
    use std::collections::hash_map::DefaultHasher;
    use std::collections::HashSet;
    use std::hash::{Hash, Hasher};

    #[test]
    fn addr_unchecked_works() {}

    #[test]
    fn addr_as_str_works() {}

    #[test]
    fn addr_as_bytes_works() {}

    #[test]
    fn addr_implements_display() {}

    #[test]
    fn addr_implements_as_ref_for_str() {}

    #[test]
    fn addr_implements_partial_eq_with_str() {}

    #[test]
    fn addr_implements_partial_eq_with_string() {}

    #[test]
    fn addr_implements_into_string() {}

    // Test CanonicalAddr as_slice() for each CanonicalAddr::from input type
    #[test]
    fn canonical_addr_from_slice() {}

    #[test]
    fn canonical_addr_implements_partial_eq_with_binary() {}

    #[test]
    fn canonical_addr_implements_partial_eq_with_hex_binary() {}

    #[test]
    fn canonical_addr_implements_from_array() {}

    #[test]
    fn canonical_addr_implements_from_and_to_vector() {}

    #[test]
    fn canonical_addr_implements_from_and_to_binary() {}

    #[test]
    fn canonical_addr_implements_from_and_to_hex_binary() {}

    #[test]
    fn canonical_addr_len() {}

    #[test]
    fn canonical_addr_is_empty() {}

    #[test]
    fn canonical_addr_implements_display() {}

    #[test]
    fn canonical_addr_implements_deref() {}

    #[test]
    fn canonical_addr_implements_hash() {}

    /// This requires Hash and Eq to be implemented
    #[test]
    fn canonical_addr_can_be_used_in_hash_set() {}

    // helper to show we can handle Addr and &Addr equally
    fn flexible<'a>(a: impl Into<Cow<'a, Addr>>) -> String {}

    #[test]
    fn addr_into_cow() {}

    #[test]
    fn instantiate2_address_works() {}

    #[test]
    fn instantiate2_address_works_for_cosmjs_testvectors() {}

    #[test]
    fn hash_works() {}
}
}
mod assertions {
//! A module containing an assertion framework for CosmWasm contracts.
//! The methods in here never panic but return errors instead.

/// Quick check for a guard. If the condition (first argument) is false,
/// then return the second argument `x` wrapped in `Err(x)`.
///
/// ```
/// # enum ContractError {
/// #     DelegatePerm {},
/// # }
/// #
/// # struct Permissions {
/// #     delegate: bool,
/// # }
/// #
/// # fn body() -> Result<(), ContractError> {}
/// ```
#[macro_export]
macro_rules! ensure {
    ($cond:expr, $e:expr) => {
        if !($cond) {
            return Err(std::convert::From::from($e));
        }
    };
}

/// Quick check for a guard. Like `assert_eq!`, but rather than panic,
/// it returns the third argument `x` wrapped in `Err(x)`.
///
/// ```
/// # use cosmwasm_std::{MessageInfo, Addr};
/// #
/// # enum ContractError {
/// #     Unauthorized {},
/// # }
/// # struct Config {
/// #     admin: String,
/// # }
/// #
/// # fn body() -> Result<(), ContractError> {}
/// ```
#[macro_export]
macro_rules! ensure_eq {
    ($a:expr, $b:expr, $e:expr) => {
        // Not implemented via `ensure!` because the caller would have to import both macros.
        if !($a == $b) {
            return Err(std::convert::From::from($e));
        }
    };
}

/// Quick check for a guard. Like `assert_ne!`, but rather than panic,
/// it returns the third argument `x` wrapped in Err(x).
///
/// ```
/// # enum ContractError {
/// #     NotAVoter {},
/// # }
/// #
/// # fn body() -> Result<(), ContractError> {}
/// ```
#[macro_export]
macro_rules! ensure_ne {
    ($a:expr, $b:expr, $e:expr) => {
        // Not implemented via `ensure!` because the caller would have to import both macros.
        if !($a != $b) {
            return Err(std::convert::From::from($e));
        }
    };
}

#[cfg(test)]
mod tests {
    use crate::StdError;

    #[test]
    fn ensure_works() {}

    #[test]
    fn ensure_can_infer_error_type() {}

    #[test]
    fn ensure_can_convert_into() {}

    #[test]
    fn ensure_eq_works() {}

    #[test]
    fn ensure_eq_gets_precedence_right() {}

    #[test]
    fn ensure_ne_works() {}

    #[test]
    fn ensure_ne_gets_precedence_right() {}
}
}
mod binary {
use std::fmt;
use std::ops::Deref;

use schemars::JsonSchema;
use serde::{de, ser, Deserialize, Deserializer, Serialize};

use crate::errors::{StdError, StdResult};

/// Binary is a wrapper around Vec<u8> to add base64 de/serialization
/// with serde. It also adds some helper methods to help encode inline.
///
/// This is only needed as serde-json-{core,wasm} has a horrible encoding for Vec<u8>.
/// See also <https://github.com/CosmWasm/cosmwasm/blob/main/docs/MESSAGE_TYPES.md>.
#[derive(Clone, Default, PartialEq, Eq, Hash, PartialOrd, Ord, JsonSchema)]
pub struct Binary(#[schemars(with = "String")] pub Vec<u8>);

impl Binary {
    /// take an (untrusted) string and decode it into bytes.
    /// fails if it is not valid base64
    pub fn from_base64(encoded: &str) -> StdResult<Self> {}

    /// encode to base64 string (guaranteed to be success as we control the data inside).
    /// this returns normalized form (with trailing = if needed)
    pub fn to_base64(&self) -> String {}

    pub fn as_slice(&self) -> &[u8] {}

    /// Copies content into fixed-sized array.
    ///
    /// # Examples
    ///
    /// Copy to array of explicit length
    ///
    /// ```
    /// # use cosmwasm_std::Binary;
    /// let binary = Binary::from(&[0xfb, 0x1f, 0x37]);
    /// let array: [u8; 3] = binary.to_array().unwrap();
    /// assert_eq!(array, [0xfb, 0x1f, 0x37]);
    /// ```
    ///
    /// Copy to integer
    ///
    /// ```
    /// # use cosmwasm_std::Binary;
    /// let binary = Binary::from(&[0x8b, 0x67, 0x64, 0x84, 0xb5, 0xfb, 0x1f, 0x37]);
    /// let num = u64::from_be_bytes(binary.to_array().unwrap());
    /// assert_eq!(num, 10045108015024774967);
    /// ```
    pub fn to_array<const LENGTH: usize>(&self) -> StdResult<[u8; LENGTH]> {}
}

impl fmt::Display for Binary {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {}
}

impl fmt::Debug for Binary {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {}
}

impl From<&[u8]> for Binary {
    fn from(binary: &[u8]) -> Self {}
}

/// Just like Vec<u8>, Binary is a smart pointer to [u8].
/// This implements `*binary` for us and allows us to
/// do `&*binary`, returning a `&[u8]` from a `&Binary`.
/// With [deref coercions](https://doc.rust-lang.org/1.22.1/book/first-edition/deref-coercions.html#deref-coercions),
/// this allows us to use `&binary` whenever a `&[u8]` is required.
impl Deref for Binary {
    type Target = [u8];

    fn deref(&self) -> &Self::Target {}
}

// Reference
impl<const LENGTH: usize> From<&[u8; LENGTH]> for Binary {
    fn from(source: &[u8; LENGTH]) -> Self {}
}

// Owned
impl<const LENGTH: usize> From<[u8; LENGTH]> for Binary {
    fn from(source: [u8; LENGTH]) -> Self {}
}

impl From<Vec<u8>> for Binary {
    fn from(vec: Vec<u8>) -> Self {}
}

impl From<Binary> for Vec<u8> {
    fn from(original: Binary) -> Vec<u8> {}
}

/// Implement `encoding::Binary == std::vec::Vec<u8>`
impl PartialEq<Vec<u8>> for Binary {
    fn eq(&self, rhs: &Vec<u8>) -> bool {}
}

/// Implement `std::vec::Vec<u8> == encoding::Binary`
impl PartialEq<Binary> for Vec<u8> {
    fn eq(&self, rhs: &Binary) -> bool {}
}

/// Implement `Binary == &[u8]`
impl PartialEq<&[u8]> for Binary {
    fn eq(&self, rhs: &&[u8]) -> bool {}
}

/// Implement `&[u8] == Binary`
impl PartialEq<Binary> for &[u8] {
    fn eq(&self, rhs: &Binary) -> bool {}
}

/// Implement `Binary == &[u8; LENGTH]`
impl<const LENGTH: usize> PartialEq<&[u8; LENGTH]> for Binary {
    fn eq(&self, rhs: &&[u8; LENGTH]) -> bool {}
}

/// Implement `&[u8; LENGTH] == Binary`
impl<const LENGTH: usize> PartialEq<Binary> for &[u8; LENGTH] {
    fn eq(&self, rhs: &Binary) -> bool {}
}

/// Implement `Binary == [u8; LENGTH]`
impl<const LENGTH: usize> PartialEq<[u8; LENGTH]> for Binary {
    fn eq(&self, rhs: &[u8; LENGTH]) -> bool {}
}

/// Implement `[u8; LENGTH] == Binary`
impl<const LENGTH: usize> PartialEq<Binary> for [u8; LENGTH] {
    fn eq(&self, rhs: &Binary) -> bool {}
}

/// Serializes as a base64 string
impl Serialize for Binary {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: ser::Serializer,
    {}
}

/// Deserializes as a base64 string
impl<'de> Deserialize<'de> for Binary {
    fn deserialize<D>(deserializer: D) -> Result<Binary, D::Error>
    where
        D: Deserializer<'de>,
    {}
}

struct Base64Visitor;

impl<'de> de::Visitor<'de> for Base64Visitor {
    type Value = Binary;

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {}

    fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
    where
        E: de::Error,
    {}
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::errors::StdError;
    use crate::serde::{from_slice, to_vec};
    use std::collections::hash_map::DefaultHasher;
    use std::collections::HashSet;
    use std::hash::{Hash, Hasher};

    #[test]
    fn encode_decode() {}

    #[test]
    fn encode_decode_non_ascii() {}

    #[test]
    fn to_array_works() {}

    #[test]
    fn from_valid_string() {}

    // this accepts input without a trailing = but outputs normal form
    #[test]
    fn from_shortened_string() {}

    #[test]
    fn from_invalid_string() {}

    #[test]
    fn from_slice_works() {}

    #[test]
    fn from_fixed_length_array_works() {}

    #[test]
    fn from_owned_fixed_length_array_works() {}

    #[test]
    fn from_literal_works() {}

    #[test]
    fn from_vec_works() {}

    #[test]
    fn into_vec_works() {}

    #[test]
    fn serialization_works() {}

    #[test]
    fn deserialize_from_valid_string() {}

    #[test]
    fn deserialize_from_invalid_string() {}

    #[test]
    fn binary_implements_debug() {}

    #[test]
    fn binary_implements_deref() {}

    #[test]
    fn binary_implements_hash() {}

    /// This requires Hash and Eq to be implemented
    #[test]
    fn binary_can_be_used_in_hash_set() {}

    #[test]
    fn binary_implements_partial_eq_with_vector() {}

    #[test]
    fn binary_implements_partial_eq_with_slice_and_array() {}
}
}
mod coin {
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use std::fmt;

use crate::math::Uint128;

#[derive(Serialize, Deserialize, Clone, Default, Debug, PartialEq, Eq, JsonSchema)]
pub struct Coin {
    pub denom: String,
    pub amount: Uint128,
}

impl Coin {
    pub fn new(amount: u128, denom: impl Into<String>) -> Self {}
}

impl fmt::Display for Coin {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {}
}

/// A shortcut constructor for a set of one denomination of coins
///
/// # Examples
///
/// ```
/// # use cosmwasm_std::{coins, BankMsg, CosmosMsg, Response, SubMsg};
/// # use cosmwasm_std::testing::{mock_env, mock_info};
/// # let env = mock_env();
/// # let info = mock_info("sender", &[]);
/// let tip = coins(123, "ucosm");
///
/// let mut response: Response = Default::default();
/// response.messages = vec![SubMsg::new(BankMsg::Send {
///   to_address: info.sender.into(),
///   amount: tip,
/// })];
/// ```
pub fn coins(amount: u128, denom: impl Into<String>) -> Vec<Coin> {}

/// A shorthand constructor for Coin
///
/// # Examples
///
/// ```
/// # use cosmwasm_std::{coin, BankMsg, CosmosMsg, Response, SubMsg};
/// # use cosmwasm_std::testing::{mock_env, mock_info};
/// # let env = mock_env();
/// # let info = mock_info("sender", &[]);
/// let tip = vec![
///     coin(123, "ucosm"),
///     coin(24, "ustake"),
/// ];
///
/// let mut response: Response = Default::default();
/// response.messages = vec![SubMsg::new(BankMsg::Send {
///     to_address: info.sender.into(),
///     amount: tip,
/// })];
/// ```
pub fn coin(amount: u128, denom: impl Into<String>) -> Coin {}

/// has_coins returns true if the list of coins has at least the required amount
pub fn has_coins(coins: &[Coin], required: &Coin) -> bool {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn coin_implements_display() {}

    #[test]
    fn coin_works() {}

    #[test]
    fn coins_works() {}

    #[test]
    fn has_coins_matches() {}
}
}
mod conversion {
/// Converts an input of type usize to u32.
///
/// On 32 bit platforms such as wasm32 this is just a safe cast.
/// On other platforms the conversion panics for values larger than
/// `u32::MAX`.
#[inline]
pub fn force_to_u32(input: usize) -> u32 {}
}
mod deps {
use std::marker::PhantomData;

use crate::query::CustomQuery;
use crate::results::Empty;
use crate::traits::{Api, Querier, Storage};
use crate::QuerierWrapper;

/// Holds all external dependencies of the contract.
/// Designed to allow easy dependency injection at runtime.
/// This cannot be copied or cloned since it would behave differently
/// for mock storages and a bridge storage in the VM.
pub struct OwnedDeps<S: Storage, A: Api, Q: Querier, C: CustomQuery = Empty> {
    pub storage: S,
    pub api: A,
    pub querier: Q,
    pub custom_query_type: PhantomData<C>,
}

pub struct DepsMut<'a, C: CustomQuery = Empty> {
    pub storage: &'a mut dyn Storage,
    pub api: &'a dyn Api,
    pub querier: QuerierWrapper<'a, C>,
}

#[derive(Clone)]
pub struct Deps<'a, C: CustomQuery = Empty> {
    pub storage: &'a dyn Storage,
    pub api: &'a dyn Api,
    pub querier: QuerierWrapper<'a, C>,
}

// Use custom implementation on order to implement Copy in case `C` is not `Copy`.
// See "There is a small difference between the two: the derive strategy will also
// place a Copy bound on type parameters, which isn’t always desired."
// https://doc.rust-lang.org/std/marker/trait.Copy.html
impl<'a, C: CustomQuery> Copy for Deps<'a, C> {}

impl<S: Storage, A: Api, Q: Querier, C: CustomQuery> OwnedDeps<S, A, Q, C> {
    pub fn as_ref(&'_ self) -> Deps<'_, C> {}

    pub fn as_mut(&'_ mut self) -> DepsMut<'_, C> {}
}

impl<'a, C: CustomQuery> DepsMut<'a, C> {
    pub fn as_ref(&'_ self) -> Deps<'_, C> {}

    pub fn branch(&'_ mut self) -> DepsMut<'_, C> {}
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::testing::{mock_dependencies, MockApi, MockQuerier, MockStorage};
    use serde::{Deserialize, Serialize};

    // ensure we can call these many times, eg. as sub-calls
    fn execute(mut deps: DepsMut) {}
    fn execute2(_deps: DepsMut) {}

    fn query(deps: Deps) {}
    fn query2(_deps: Deps) {}

    #[test]
    fn ensure_easy_reuse() {}

    #[test]
    fn deps_implements_copy() {}
}
}
mod errors {
mod recover_pubkey_error {
#[cfg(not(target_arch = "wasm32"))]
use cosmwasm_crypto::CryptoError;
#[cfg(feature = "backtraces")]
use std::backtrace::Backtrace;
use std::fmt::Debug;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum RecoverPubkeyError {
    #[error("Invalid hash format")]
    InvalidHashFormat,
    #[error("Invalid signature format")]
    InvalidSignatureFormat,
    #[error("Invalid recovery parameter. Supported values: 0 and 1.")]
    InvalidRecoveryParam,
    #[error("Unknown error: {error_code}")]
    UnknownErr {
        error_code: u32,
        #[cfg(feature = "backtraces")]
        backtrace: Backtrace,
    },
}

impl RecoverPubkeyError {
    pub fn unknown_err(error_code: u32) -> Self {}
}

impl PartialEq<RecoverPubkeyError> for RecoverPubkeyError {
    fn eq(&self, rhs: &RecoverPubkeyError) -> bool {}
}

#[cfg(not(target_arch = "wasm32"))]
impl From<CryptoError> for RecoverPubkeyError {
    fn from(original: CryptoError) -> Self {}
}
}
mod std_error {
#[cfg(feature = "backtraces")]
use std::backtrace::Backtrace;
use std::fmt;
use thiserror::Error;

use crate::errors::{RecoverPubkeyError, VerificationError};

/// Structured error type for init, execute and query.
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
#[derive(Error, Debug)]
pub enum StdError {
    #[error("Verification error: {source}")]
    VerificationErr {
        source: VerificationError,
        #[cfg(feature = "backtraces")]
        backtrace: Backtrace,
    },
    #[error("Recover pubkey error: {source}")]
    RecoverPubkeyErr {
        source: RecoverPubkeyError,
        #[cfg(feature = "backtraces")]
        backtrace: Backtrace,
    },
    /// Whenever there is no specific error type available
    #[error("Generic error: {msg}")]
    GenericErr {
        msg: String,
        #[cfg(feature = "backtraces")]
        backtrace: Backtrace,
    },
    #[error("Invalid Base64 string: {msg}")]
    InvalidBase64 {
        msg: String,
        #[cfg(feature = "backtraces")]
        backtrace: Backtrace,
    },
    #[error("Invalid data size: expected={expected} actual={actual}")]
    InvalidDataSize {
        expected: u64,
        actual: u64,
        #[cfg(feature = "backtraces")]
        backtrace: Backtrace,
    },
    #[error("Invalid hex string: {msg}")]
    InvalidHex {
        msg: String,
        #[cfg(feature = "backtraces")]
        backtrace: Backtrace,
    },
    /// Whenever UTF-8 bytes cannot be decoded into a unicode string, e.g. in String::from_utf8 or str::from_utf8.
    #[error("Cannot decode UTF8 bytes into string: {msg}")]
    InvalidUtf8 {
        msg: String,
        #[cfg(feature = "backtraces")]
        backtrace: Backtrace,
    },
    #[error("{kind} not found")]
    NotFound {
        kind: String,
        #[cfg(feature = "backtraces")]
        backtrace: Backtrace,
    },
    #[error("Error parsing into type {target_type}: {msg}")]
    ParseErr {
        /// the target type that was attempted
        target_type: String,
        msg: String,
        #[cfg(feature = "backtraces")]
        backtrace: Backtrace,
    },
    #[error("Error serializing type {source_type}: {msg}")]
    SerializeErr {
        /// the source type that was attempted
        source_type: String,
        msg: String,
        #[cfg(feature = "backtraces")]
        backtrace: Backtrace,
    },
    #[error("Overflow: {source}")]
    Overflow {
        source: OverflowError,
        #[cfg(feature = "backtraces")]
        backtrace: Backtrace,
    },
    #[error("Divide by zero: {source}")]
    DivideByZero {
        source: DivideByZeroError,
        #[cfg(feature = "backtraces")]
        backtrace: Backtrace,
    },
    #[error("Conversion error: ")]
    ConversionOverflow {
        #[from]
        source: ConversionOverflowError,
        #[cfg(feature = "backtraces")]
        backtrace: Backtrace,
    },
}

impl StdError {
    pub fn verification_err(source: VerificationError) -> Self {}

    pub fn recover_pubkey_err(source: RecoverPubkeyError) -> Self {}

    pub fn generic_err(msg: impl Into<String>) -> Self {}

    pub fn invalid_base64(msg: impl ToString) -> Self {}

    pub fn invalid_data_size(expected: usize, actual: usize) -> Self {}

    pub fn invalid_hex(msg: impl ToString) -> Self {}

    pub fn invalid_utf8(msg: impl ToString) -> Self {}

    pub fn not_found(kind: impl Into<String>) -> Self {}

    pub fn parse_err(target: impl Into<String>, msg: impl ToString) -> Self {}

    pub fn serialize_err(source: impl Into<String>, msg: impl ToString) -> Self {}

    pub fn overflow(source: OverflowError) -> Self {}

    pub fn divide_by_zero(source: DivideByZeroError) -> Self {}
}

impl PartialEq<StdError> for StdError {
    fn eq(&self, rhs: &StdError) -> bool {}
}

impl From<std::str::Utf8Error> for StdError {
    fn from(source: std::str::Utf8Error) -> Self {}
}

impl From<std::string::FromUtf8Error> for StdError {
    fn from(source: std::string::FromUtf8Error) -> Self {}
}

impl From<VerificationError> for StdError {
    fn from(source: VerificationError) -> Self {}
}

impl From<RecoverPubkeyError> for StdError {
    fn from(source: RecoverPubkeyError) -> Self {}
}

impl From<OverflowError> for StdError {
    fn from(source: OverflowError) -> Self {}
}

impl From<DivideByZeroError> for StdError {
    fn from(source: DivideByZeroError) -> Self {}
}

/// The return type for init, execute and query. Since the error type cannot be serialized to JSON,
/// this is only available within the contract and its unit tests.
///
/// The prefix "Std" means "the standard result within the standard library". This is not the only
/// result/error type in cosmwasm-std.
pub type StdResult<T> = core::result::Result<T, StdError>;

#[derive(Error, Debug, PartialEq, Eq)]
pub enum OverflowOperation {
    Add,
    Sub,
    Mul,
    Pow,
    Shr,
    Shl,
}

impl fmt::Display for OverflowOperation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {}
}

#[derive(Error, Debug, PartialEq, Eq)]
#[error("Cannot {operation} with {operand1} and {operand2}")]
pub struct OverflowError {
    pub operation: OverflowOperation,
    pub operand1: String,
    pub operand2: String,
}

impl OverflowError {
    pub fn new(
        operation: OverflowOperation,
        operand1: impl ToString,
        operand2: impl ToString,
    ) -> Self {}
}

/// The error returned by [`TryFrom`] conversions that overflow, for example
/// when converting from [`Uint256`] to [`Uint128`].
///
/// [`TryFrom`]: std::convert::TryFrom
/// [`Uint256`]: crate::Uint256
/// [`Uint128`]: crate::Uint128
#[derive(Error, Debug, PartialEq, Eq)]
#[error("Error converting {source_type} to {target_type} for {value}")]
pub struct ConversionOverflowError {
    pub source_type: &'static str,
    pub target_type: &'static str,
    pub value: String,
}

impl ConversionOverflowError {
    pub fn new(
        source_type: &'static str,
        target_type: &'static str,
        value: impl Into<String>,
    ) -> Self {}
}

#[derive(Error, Debug, PartialEq, Eq)]
#[error("Cannot devide {operand} by zero")]
pub struct DivideByZeroError {
    pub operand: String,
}

impl DivideByZeroError {
    pub fn new(operand: impl ToString) -> Self {}
}

#[derive(Error, Debug, PartialEq, Eq)]
pub enum CheckedMultiplyRatioError {
    #[error("Denominator must not be zero")]
    DivideByZero,

    #[error("Multiplication overflow")]
    Overflow,
}

#[derive(Error, Debug, PartialEq, Eq)]
pub enum CheckedFromRatioError {
    #[error("Denominator must not be zero")]
    DivideByZero,

    #[error("Overflow")]
    Overflow,
}

#[derive(Error, Debug, PartialEq, Eq)]
#[error("Round up operation failed because of overflow")]
pub struct RoundUpOverflowError;

#[cfg(test)]
mod tests {
    use super::*;
    use std::str;

    // constructors

    // example of reporting contract errors with format!
    #[test]
    fn generic_err_owned() {}

    // example of reporting static contract errors
    #[test]
    fn generic_err_ref() {}

    #[test]
    fn invalid_base64_works_for_strings() {}

    #[test]
    fn invalid_base64_works_for_errors() {}

    #[test]
    fn invalid_data_size_works() {}

    #[test]
    fn invalid_hex_works_for_strings() {}

    #[test]
    fn invalid_hex_works_for_errors() {}

    #[test]
    fn invalid_utf8_works_for_strings() {}

    #[test]
    fn invalid_utf8_works_for_errors() {}

    #[test]
    fn not_found_works() {}

    #[test]
    fn parse_err_works() {}

    #[test]
    fn serialize_err_works() {}

    #[test]
    fn underflow_works_for_u128() {}

    #[test]
    fn overflow_works_for_i64() {}

    #[test]
    fn divide_by_zero_works() {}

    #[test]
    fn implements_debug() {}

    #[test]
    fn implements_display() {}

    #[test]
    fn implements_partial_eq() {}

    #[test]
    fn from_std_str_utf8error_works() {}

    #[test]
    fn from_std_string_fromutf8error_works() {}
}
}
mod system_error {
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

use crate::Binary;

/// SystemError is used for errors inside the VM and is API friendly (i.e. serializable).
///
/// This is used on return values for Querier as a nested result: Result<StdResult<T>, SystemError>
/// The first wrap (SystemError) will trigger if the contract address doesn't exist,
/// the QueryRequest is malformated, etc. The second wrap will be an error message from
/// the contract itself.
///
/// Such errors are only created by the VM. The error type is defined in the standard library, to ensure
/// the contract understands the error format without creating a dependency on cosmwasm-vm.
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, JsonSchema)]
#[serde(rename_all = "snake_case")]
#[non_exhaustive]
pub enum SystemError {
    InvalidRequest {
        error: String,
        request: Binary,
    },
    InvalidResponse {
        error: String,
        response: Binary,
    },
    NoSuchContract {
        /// The address that was attempted to query
        addr: String,
    },
    Unknown {},
    UnsupportedRequest {
        kind: String,
    },
}

impl std::error::Error for SystemError {}

impl std::fmt::Display for SystemError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {}
}
}
mod verification_error {
#[cfg(feature = "backtraces")]
use std::backtrace::Backtrace;
use std::fmt::Debug;
use thiserror::Error;

#[cfg(not(target_arch = "wasm32"))]
use cosmwasm_crypto::CryptoError;

#[derive(Error, Debug)]
pub enum VerificationError {
    #[error("Batch error")]
    BatchErr,
    #[error("Generic error")]
    GenericErr,
    #[error("Invalid hash format")]
    InvalidHashFormat,
    #[error("Invalid signature format")]
    InvalidSignatureFormat,
    #[error("Invalid public key format")]
    InvalidPubkeyFormat,
    #[error("Invalid recovery parameter. Supported values: 0 and 1.")]
    InvalidRecoveryParam,
    #[error("Unknown error: {error_code}")]
    UnknownErr {
        error_code: u32,
        #[cfg(feature = "backtraces")]
        backtrace: Backtrace,
    },
}

impl VerificationError {
    pub fn unknown_err(error_code: u32) -> Self {}
}

impl PartialEq<VerificationError> for VerificationError {
    fn eq(&self, rhs: &VerificationError) -> bool {}
}

#[cfg(not(target_arch = "wasm32"))]
impl From<CryptoError> for VerificationError {
    fn from(original: CryptoError) -> Self {}
}

#[cfg(test)]
mod tests {
    use super::*;

    // constructors
    #[test]
    fn unknown_err_works() {}
}
}

pub use recover_pubkey_error::RecoverPubkeyError;
pub use std_error::{
    CheckedFromRatioError, CheckedMultiplyRatioError, ConversionOverflowError, DivideByZeroError,
    OverflowError, OverflowOperation, RoundUpOverflowError, StdError, StdResult,
};
pub use system_error::SystemError;
pub use verification_error::VerificationError;
}
mod hex_binary {
use std::fmt;
use std::ops::Deref;

use schemars::JsonSchema;
use serde::{de, ser, Deserialize, Deserializer, Serialize};

use crate::{Binary, StdError, StdResult};

/// This is a wrapper around Vec<u8> to add hex de/serialization
/// with serde. It also adds some helper methods to help encode inline.
///
/// This is similar to `cosmwasm_std::Binary` but uses hex.
/// See also <https://github.com/CosmWasm/cosmwasm/blob/main/docs/MESSAGE_TYPES.md>.
#[derive(Clone, Default, PartialEq, Eq, Hash, PartialOrd, Ord, JsonSchema)]
pub struct HexBinary(#[schemars(with = "String")] Vec<u8>);

impl HexBinary {
    pub fn from_hex(input: &str) -> StdResult<Self> {}

    pub fn to_hex(&self) -> String {}

    pub fn as_slice(&self) -> &[u8] {}

    /// Copies content into fixed-sized array.
    ///
    /// # Examples
    ///
    /// Copy to array of explicit length
    ///
    /// ```
    /// # use cosmwasm_std::HexBinary;
    /// let data = HexBinary::from(&[0xfb, 0x1f, 0x37]);
    /// let array: [u8; 3] = data.to_array().unwrap();
    /// assert_eq!(array, [0xfb, 0x1f, 0x37]);
    /// ```
    ///
    /// Copy to integer
    ///
    /// ```
    /// # use cosmwasm_std::HexBinary;
    /// let data = HexBinary::from(&[0x8b, 0x67, 0x64, 0x84, 0xb5, 0xfb, 0x1f, 0x37]);
    /// let num = u64::from_be_bytes(data.to_array().unwrap());
    /// assert_eq!(num, 10045108015024774967);
    /// ```
    pub fn to_array<const LENGTH: usize>(&self) -> StdResult<[u8; LENGTH]> {}
}

impl fmt::Display for HexBinary {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {}
}

impl fmt::Debug for HexBinary {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {}
}

impl From<&[u8]> for HexBinary {
    fn from(binary: &[u8]) -> Self {}
}

/// Just like Vec<u8>, HexBinary is a smart pointer to [u8].
/// This implements `*data` for us and allows us to
/// do `&*data`, returning a `&[u8]` from a `&HexBinary`.
/// With [deref coercions](https://doc.rust-lang.org/1.22.1/book/first-edition/deref-coercions.html#deref-coercions),
/// this allows us to use `&data` whenever a `&[u8]` is required.
impl Deref for HexBinary {
    type Target = [u8];

    fn deref(&self) -> &Self::Target {}
}

// Reference
impl<const LENGTH: usize> From<&[u8; LENGTH]> for HexBinary {
    fn from(source: &[u8; LENGTH]) -> Self {}
}

// Owned
impl<const LENGTH: usize> From<[u8; LENGTH]> for HexBinary {
    fn from(source: [u8; LENGTH]) -> Self {}
}

impl From<Vec<u8>> for HexBinary {
    fn from(vec: Vec<u8>) -> Self {}
}

impl From<HexBinary> for Vec<u8> {
    fn from(original: HexBinary) -> Vec<u8> {}
}

impl From<Binary> for HexBinary {
    fn from(original: Binary) -> Self {}
}

impl From<HexBinary> for Binary {
    fn from(original: HexBinary) -> Binary {}
}

/// Implement `HexBinary == std::vec::Vec<u8>`
impl PartialEq<Vec<u8>> for HexBinary {
    fn eq(&self, rhs: &Vec<u8>) -> bool {}
}

/// Implement `std::vec::Vec<u8> == HexBinary`
impl PartialEq<HexBinary> for Vec<u8> {
    fn eq(&self, rhs: &HexBinary) -> bool {}
}

/// Implement `HexBinary == &[u8]`
impl PartialEq<&[u8]> for HexBinary {
    fn eq(&self, rhs: &&[u8]) -> bool {}
}

/// Implement `&[u8] == HexBinary`
impl PartialEq<HexBinary> for &[u8] {
    fn eq(&self, rhs: &HexBinary) -> bool {}
}

/// Implement `HexBinary == [u8; LENGTH]`
impl<const LENGTH: usize> PartialEq<[u8; LENGTH]> for HexBinary {
    fn eq(&self, rhs: &[u8; LENGTH]) -> bool {}
}

/// Implement `[u8; LENGTH] == HexBinary`
impl<const LENGTH: usize> PartialEq<HexBinary> for [u8; LENGTH] {
    fn eq(&self, rhs: &HexBinary) -> bool {}
}

/// Implement `HexBinary == &[u8; LENGTH]`
impl<const LENGTH: usize> PartialEq<&[u8; LENGTH]> for HexBinary {
    fn eq(&self, rhs: &&[u8; LENGTH]) -> bool {}
}

/// Implement `&[u8; LENGTH] == HexBinary`
impl<const LENGTH: usize> PartialEq<HexBinary> for &[u8; LENGTH] {
    fn eq(&self, rhs: &HexBinary) -> bool {}
}

/// Serializes as a hex string
impl Serialize for HexBinary {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: ser::Serializer,
    {}
}

/// Deserializes as a hex string
impl<'de> Deserialize<'de> for HexBinary {
    fn deserialize<D>(deserializer: D) -> Result<HexBinary, D::Error>
    where
        D: Deserializer<'de>,
    {}
}

struct HexVisitor;

impl<'de> de::Visitor<'de> for HexVisitor {
    type Value = HexBinary;

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {}

    fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
    where
        E: de::Error,
    {}
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::{from_slice, to_vec, StdError};
    use std::collections::hash_map::DefaultHasher;
    use std::collections::HashSet;
    use std::hash::{Hash, Hasher};

    #[test]
    fn from_hex_works() {}

    #[test]
    fn to_hex_works() {}

    #[test]
    fn to_array_works() {}

    #[test]
    fn from_slice_works() {}

    #[test]
    fn from_fixed_length_array_works() {}

    #[test]
    fn from_owned_fixed_length_array_works() {}

    #[test]
    fn from_literal_works() {}

    #[test]
    fn from_vec_works() {}

    #[test]
    fn into_vec_works() {}

    #[test]
    fn from_binary_works() {}

    #[test]
    fn into_binary_works() {}

    #[test]
    fn serialization_works() {}

    #[test]
    fn deserialize_from_valid_string() {}

    #[test]
    fn deserialize_from_invalid_string() {}

    #[test]
    fn hex_binary_implements_debug() {}

    #[test]
    fn hex_binary_implements_deref() {}

    #[test]
    fn hex_binary_implements_hash() {}

    /// This requires Hash and Eq to be implemented
    #[test]
    fn hex_binary_can_be_used_in_hash_set() {}

    #[test]
    fn hex_binary_implements_partial_eq_with_vector() {}

    #[test]
    fn hex_binary_implements_partial_eq_with_slice_and_array() {}
}
}
mod ibc {
#![cfg(feature = "stargate")]
// The CosmosMsg variants are defined in results/cosmos_msg.rs
// The rest of the IBC related functionality is defined here

use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use std::cmp::{Ord, Ordering, PartialOrd};

#[cfg(feature = "ibc3")]
use crate::addresses::Addr;
use crate::binary::Binary;
use crate::coin::Coin;
use crate::errors::StdResult;
use crate::results::{Attribute, CosmosMsg, Empty, Event, SubMsg};
use crate::serde::to_binary;
use crate::timestamp::Timestamp;

/// These are messages in the IBC lifecycle. Only usable by IBC-enabled contracts
/// (contracts that directly speak the IBC protocol via 6 entry points)
#[non_exhaustive]
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, JsonSchema)]
#[serde(rename_all = "snake_case")]
pub enum IbcMsg {
    /// Sends bank tokens owned by the contract to the given address on another chain.
    /// The channel must already be established between the ibctransfer module on this chain
    /// and a matching module on the remote chain.
    /// We cannot select the port_id, this is whatever the local chain has bound the ibctransfer
    /// module to.
    Transfer {
        /// exisiting channel to send the tokens over
        channel_id: String,
        /// address on the remote chain to receive these tokens
        to_address: String,
        /// packet data only supports one coin
        /// https://github.com/cosmos/cosmos-sdk/blob/v0.40.0/proto/ibc/applications/transfer/v1/transfer.proto#L11-L20
        amount: Coin,
        /// when packet times out, measured on remote chain
        timeout: IbcTimeout,
    },
    /// Sends an IBC packet with given data over the existing channel.
    /// Data should be encoded in a format defined by the channel version,
    /// and the module on the other side should know how to parse this.
    SendPacket {
        channel_id: String,
        data: Binary,
        /// when packet times out, measured on remote chain
        timeout: IbcTimeout,
    },
    /// This will close an existing channel that is owned by this contract.
    /// Port is auto-assigned to the contract's IBC port
    CloseChannel { channel_id: String },
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, JsonSchema)]
pub struct IbcEndpoint {
    pub port_id: String,
    pub channel_id: String,
}

/// In IBC each package must set at least one type of timeout:
/// the timestamp or the block height. Using this rather complex enum instead of
/// two timeout fields we ensure that at least one timeout is set.
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, JsonSchema)]
#[serde(rename_all = "snake_case")]
pub struct IbcTimeout {
    // use private fields to enforce the use of constructors, which ensure that at least one is set
    block: Option<IbcTimeoutBlock>,
    timestamp: Option<Timestamp>,
}

impl IbcTimeout {
    pub fn with_block(block: IbcTimeoutBlock) -> Self {}

    pub fn with_timestamp(timestamp: Timestamp) -> Self {}

    pub fn with_both(block: IbcTimeoutBlock, timestamp: Timestamp) -> Self {}

    pub fn block(&self) -> Option<IbcTimeoutBlock> {}

    pub fn timestamp(&self) -> Option<Timestamp> {}
}

impl From<Timestamp> for IbcTimeout {
    fn from(timestamp: Timestamp) -> IbcTimeout {}
}

impl From<IbcTimeoutBlock> for IbcTimeout {
    fn from(original: IbcTimeoutBlock) -> IbcTimeout {}
}

// These are various messages used in the callbacks

/// IbcChannel defines all information on a channel.
/// This is generally used in the hand-shake process, but can be queried directly.
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, JsonSchema)]
#[non_exhaustive]
pub struct IbcChannel {
    pub endpoint: IbcEndpoint,
    pub counterparty_endpoint: IbcEndpoint,
    pub order: IbcOrder,
    /// Note: in ibcv3 this may be "", in the IbcOpenChannel handshake messages
    pub version: String,
    /// The connection upon which this channel was created. If this is a multi-hop
    /// channel, we only expose the first hop.
    pub connection_id: String,
}

impl IbcChannel {
    /// Construct a new IbcChannel.
    pub fn new(
        endpoint: IbcEndpoint,
        counterparty_endpoint: IbcEndpoint,
        order: IbcOrder,
        version: impl Into<String>,
        connection_id: impl Into<String>,
    ) -> Self {}
}

/// IbcOrder defines if a channel is ORDERED or UNORDERED
/// Values come from https://github.com/cosmos/cosmos-sdk/blob/v0.40.0/proto/ibc/core/channel/v1/channel.proto#L69-L80
/// Naming comes from the protobuf files and go translations.
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, JsonSchema)]
pub enum IbcOrder {
    #[serde(rename = "ORDER_UNORDERED")]
    Unordered,
    #[serde(rename = "ORDER_ORDERED")]
    Ordered,
}

/// IBCTimeoutHeight Height is a monotonically increasing data type
/// that can be compared against another Height for the purposes of updating and
/// freezing clients.
/// Ordering is (revision_number, timeout_height)
#[derive(Serialize, Deserialize, Copy, Clone, Debug, PartialEq, Eq, JsonSchema)]
pub struct IbcTimeoutBlock {
    /// the version that the client is currently on
    /// (eg. after reseting the chain this could increment 1 as height drops to 0)
    pub revision: u64,
    /// block height after which the packet times out.
    /// the height within the given revision
    pub height: u64,
}

impl IbcTimeoutBlock {
    pub fn is_zero(&self) -> bool {}
}

impl PartialOrd for IbcTimeoutBlock {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {}
}

impl Ord for IbcTimeoutBlock {
    fn cmp(&self, other: &Self) -> Ordering {}
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, JsonSchema)]
#[non_exhaustive]
pub struct IbcPacket {
    /// The raw data sent from the other side in the packet
    pub data: Binary,
    /// identifies the channel and port on the sending chain.
    pub src: IbcEndpoint,
    /// identifies the channel and port on the receiving chain.
    pub dest: IbcEndpoint,
    /// The sequence number of the packet on the given channel
    pub sequence: u64,
    pub timeout: IbcTimeout,
}

impl IbcPacket {
    /// Construct a new IbcPacket.
    pub fn new(
        data: impl Into<Binary>,
        src: IbcEndpoint,
        dest: IbcEndpoint,
        sequence: u64,
        timeout: IbcTimeout,
    ) -> Self {}
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, JsonSchema)]
#[non_exhaustive]
pub struct IbcAcknowledgement {
    pub data: Binary,
    // we may add more info here in the future (meta-data from the acknowledgement)
    // there have been proposals to extend this type in core ibc for future versions
}

impl IbcAcknowledgement {
    pub fn new(data: impl Into<Binary>) -> Self {}

    pub fn encode_json(data: &impl Serialize) -> StdResult<Self> {}
}

/// The message that is passed into `ibc_channel_open`
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, JsonSchema)]
#[serde(rename_all = "snake_case")]
pub enum IbcChannelOpenMsg {
    /// The ChanOpenInit step from https://github.com/cosmos/ibc/tree/master/spec/core/ics-004-channel-and-packet-semantics#channel-lifecycle-management
    OpenInit { channel: IbcChannel },
    /// The ChanOpenTry step from https://github.com/cosmos/ibc/tree/master/spec/core/ics-004-channel-and-packet-semantics#channel-lifecycle-management
    OpenTry {
        channel: IbcChannel,
        counterparty_version: String,
    },
}

impl IbcChannelOpenMsg {
    pub fn new_init(channel: IbcChannel) -> Self {}

    pub fn new_try(channel: IbcChannel, counterparty_version: impl Into<String>) -> Self {}

    pub fn channel(&self) -> &IbcChannel {}

    pub fn counterparty_version(&self) -> Option<&str> {}
}

impl From<IbcChannelOpenMsg> for IbcChannel {
    fn from(msg: IbcChannelOpenMsg) -> IbcChannel {}
}

/// Note that this serializes as "null".
#[cfg(not(feature = "ibc3"))]
pub type IbcChannelOpenResponse = ();
/// This serializes either as "null" or a JSON object.
#[cfg(feature = "ibc3")]
pub type IbcChannelOpenResponse = Option<Ibc3ChannelOpenResponse>;

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, JsonSchema)]
pub struct Ibc3ChannelOpenResponse {
    /// We can set the channel version to a different one than we were called with
    pub version: String,
}

/// The message that is passed into `ibc_channel_connect`
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, JsonSchema)]
#[serde(rename_all = "snake_case")]
pub enum IbcChannelConnectMsg {
    /// The ChanOpenAck step from https://github.com/cosmos/ibc/tree/master/spec/core/ics-004-channel-and-packet-semantics#channel-lifecycle-management
    OpenAck {
        channel: IbcChannel,
        counterparty_version: String,
    },
    /// The ChanOpenConfirm step from https://github.com/cosmos/ibc/tree/master/spec/core/ics-004-channel-and-packet-semantics#channel-lifecycle-management
    OpenConfirm { channel: IbcChannel },
}

impl IbcChannelConnectMsg {
    pub fn new_ack(channel: IbcChannel, counterparty_version: impl Into<String>) -> Self {}

    pub fn new_confirm(channel: IbcChannel) -> Self {}

    pub fn channel(&self) -> &IbcChannel {}

    pub fn counterparty_version(&self) -> Option<&str> {}
}

impl From<IbcChannelConnectMsg> for IbcChannel {
    fn from(msg: IbcChannelConnectMsg) -> IbcChannel {}
}

/// The message that is passed into `ibc_channel_close`
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, JsonSchema)]
#[serde(rename_all = "snake_case")]
pub enum IbcChannelCloseMsg {
    /// The ChanCloseInit step from https://github.com/cosmos/ibc/tree/master/spec/core/ics-004-channel-and-packet-semantics#channel-lifecycle-management
    CloseInit { channel: IbcChannel },
    /// The ChanCloseConfirm step from https://github.com/cosmos/ibc/tree/master/spec/core/ics-004-channel-and-packet-semantics#channel-lifecycle-management
    CloseConfirm { channel: IbcChannel }, // pub channel: IbcChannel,
}

impl IbcChannelCloseMsg {
    pub fn new_init(channel: IbcChannel) -> Self {}

    pub fn new_confirm(channel: IbcChannel) -> Self {}

    pub fn channel(&self) -> &IbcChannel {}
}

impl From<IbcChannelCloseMsg> for IbcChannel {
    fn from(msg: IbcChannelCloseMsg) -> IbcChannel {}
}

/// The message that is passed into `ibc_packet_receive`
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, JsonSchema)]
#[non_exhaustive]
pub struct IbcPacketReceiveMsg {
    pub packet: IbcPacket,
    #[cfg(feature = "ibc3")]
    pub relayer: Addr,
}

impl IbcPacketReceiveMsg {
    #[cfg(not(feature = "ibc3"))]
    pub fn new(packet: IbcPacket) -> Self {}

    #[cfg(feature = "ibc3")]
    pub fn new(packet: IbcPacket, relayer: Addr) -> Self {}
}

/// The message that is passed into `ibc_packet_ack`
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, JsonSchema)]
#[non_exhaustive]
pub struct IbcPacketAckMsg {
    pub acknowledgement: IbcAcknowledgement,
    pub original_packet: IbcPacket,
    #[cfg(feature = "ibc3")]
    pub relayer: Addr,
}

impl IbcPacketAckMsg {
    #[cfg(not(feature = "ibc3"))]
    pub fn new(acknowledgement: IbcAcknowledgement, original_packet: IbcPacket) -> Self {}

    #[cfg(feature = "ibc3")]
    pub fn new(
        acknowledgement: IbcAcknowledgement,
        original_packet: IbcPacket,
        relayer: Addr,
    ) -> Self {}
}

/// The message that is passed into `ibc_packet_timeout`
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, JsonSchema)]
#[non_exhaustive]
pub struct IbcPacketTimeoutMsg {
    pub packet: IbcPacket,
    #[cfg(feature = "ibc3")]
    pub relayer: Addr,
}

impl IbcPacketTimeoutMsg {
    #[cfg(not(feature = "ibc3"))]
    pub fn new(packet: IbcPacket) -> Self {}

    #[cfg(feature = "ibc3")]
    pub fn new(packet: IbcPacket, relayer: Addr) -> Self {}
}

/// This is the return value for the majority of the ibc handlers.
/// That are able to dispatch messages / events on their own,
/// but have no meaningful return value to the calling code.
///
/// Callbacks that have return values (like receive_packet)
/// or that cannot redispatch messages (like the handshake callbacks)
/// will use other Response types
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, JsonSchema)]
#[non_exhaustive]
pub struct IbcBasicResponse<T = Empty> {
    /// Optional list of messages to pass. These will be executed in order.
    /// If the ReplyOn member is set, they will invoke this contract's `reply` entry point
    /// after execution. Otherwise, they act like "fire and forget".
    /// Use `SubMsg::new` to create messages with the older "fire and forget" semantics.
    pub messages: Vec<SubMsg<T>>,
    /// The attributes that will be emitted as part of a `wasm` event.
    ///
    /// More info about events (and their attributes) can be found in [*Cosmos SDK* docs].
    ///
    /// [*Cosmos SDK* docs]: https://docs.cosmos.network/v0.42/core/events.html
    pub attributes: Vec<Attribute>,
    /// Extra, custom events separate from the main `wasm` one. These will have
    /// `wasm-` prepended to the type.
    ///
    /// More info about events can be found in [*Cosmos SDK* docs].
    ///
    /// [*Cosmos SDK* docs]: https://docs.cosmos.network/v0.42/core/events.html
    pub events: Vec<Event>,
}

// Custom imlementation in order to implement it for all `T`, even if `T` is not `Default`.
impl<T> Default for IbcBasicResponse<T> {
    fn default() -> Self {}
}

impl<T> IbcBasicResponse<T> {
    pub fn new() -> Self {}

    /// Add an attribute included in the main `wasm` event.
    pub fn add_attribute(mut self, key: impl Into<String>, value: impl Into<String>) -> Self {}

    /// This creates a "fire and forget" message, by using `SubMsg::new()` to wrap it,
    /// and adds it to the list of messages to process.
    pub fn add_message(mut self, msg: impl Into<CosmosMsg<T>>) -> Self {}

    /// This takes an explicit SubMsg (creates via eg. `reply_on_error`)
    /// and adds it to the list of messages to process.
    pub fn add_submessage(mut self, msg: SubMsg<T>) -> Self {}

    /// Adds an extra event to the response, separate from the main `wasm` event
    /// that is always created.
    ///
    /// The `wasm-` prefix will be appended by the runtime to the provided type
    /// of event.
    pub fn add_event(mut self, event: Event) -> Self {}

    /// Bulk add attributes included in the main `wasm` event.
    ///
    /// Anything that can be turned into an iterator and yields something
    /// that can be converted into an `Attribute` is accepted.
    ///
    /// ## Examples
    ///
    /// ```
    /// use cosmwasm_std::{attr, IbcBasicResponse};
    ///
    /// let attrs = vec![
    ///     ("action", "reaction"),
    ///     ("answer", "42"),
    ///     ("another", "attribute"),
    /// ];
    /// let res: IbcBasicResponse = IbcBasicResponse::new().add_attributes(attrs.clone());
    /// assert_eq!(res.attributes, attrs);
    /// ```
    pub fn add_attributes<A: Into<Attribute>>(
        mut self,
        attrs: impl IntoIterator<Item = A>,
    ) -> Self {}

    /// Bulk add "fire and forget" messages to the list of messages to process.
    ///
    /// ## Examples
    ///
    /// ```
    /// use cosmwasm_std::{CosmosMsg, IbcBasicResponse};
    ///
    /// fn make_response_with_msgs(msgs: Vec<CosmosMsg>) -> IbcBasicResponse {}
    /// ```
    pub fn add_messages<M: Into<CosmosMsg<T>>>(self, msgs: impl IntoIterator<Item = M>) -> Self {}

    /// Bulk add explicit SubMsg structs to the list of messages to process.
    ///
    /// ## Examples
    ///
    /// ```
    /// use cosmwasm_std::{SubMsg, IbcBasicResponse};
    ///
    /// fn make_response_with_submsgs(msgs: Vec<SubMsg>) -> IbcBasicResponse {}
    /// ```
    pub fn add_submessages(mut self, msgs: impl IntoIterator<Item = SubMsg<T>>) -> Self {}

    /// Bulk add custom events to the response. These are separate from the main
    /// `wasm` event.
    ///
    /// The `wasm-` prefix will be appended by the runtime to the provided types
    /// of events.
    pub fn add_events(mut self, events: impl IntoIterator<Item = Event>) -> Self {}
}

// This defines the return value on packet response processing.
// This "success" case should be returned even in application-level errors,
// Where the acknowledgement bytes contain an encoded error message to be returned to
// the calling chain. (Returning ContractResult::Err will abort processing of this packet
// and not inform the calling chain).
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, JsonSchema)]
#[non_exhaustive]
pub struct IbcReceiveResponse<T = Empty> {
    /// The bytes we return to the contract that sent the packet.
    /// This may represent a success or error of exection
    pub acknowledgement: Binary,
    /// Optional list of messages to pass. These will be executed in order.
    /// If the ReplyOn member is set, they will invoke this contract's `reply` entry point
    /// after execution. Otherwise, they act like "fire and forget".
    /// Use `call` or `msg.into()` to create messages with the older "fire and forget" semantics.
    pub messages: Vec<SubMsg<T>>,
    /// The attributes that will be emitted as part of a "wasm" event.
    ///
    /// More info about events (and their attributes) can be found in [*Cosmos SDK* docs].
    ///
    /// [*Cosmos SDK* docs]: https://docs.cosmos.network/v0.42/core/events.html
    pub attributes: Vec<Attribute>,
    /// Extra, custom events separate from the main `wasm` one. These will have
    /// `wasm-` prepended to the type.
    ///
    /// More info about events can be found in [*Cosmos SDK* docs].
    ///
    /// [*Cosmos SDK* docs]: https://docs.cosmos.network/v0.42/core/events.html
    pub events: Vec<Event>,
}

// Custom imlementation in order to implement it for all `T`, even if `T` is not `Default`.
impl<T> Default for IbcReceiveResponse<T> {
    fn default() -> Self {}
}

impl<T> IbcReceiveResponse<T> {
    pub fn new() -> Self {}

    /// Set the acknowledgement for this response.
    pub fn set_ack(mut self, ack: impl Into<Binary>) -> Self {}

    /// Add an attribute included in the main `wasm` event.
    pub fn add_attribute(mut self, key: impl Into<String>, value: impl Into<String>) -> Self {}

    /// This creates a "fire and forget" message, by using `SubMsg::new()` to wrap it,
    /// and adds it to the list of messages to process.
    pub fn add_message(mut self, msg: impl Into<CosmosMsg<T>>) -> Self {}

    /// This takes an explicit SubMsg (creates via eg. `reply_on_error`)
    /// and adds it to the list of messages to process.
    pub fn add_submessage(mut self, msg: SubMsg<T>) -> Self {}

    /// Adds an extra event to the response, separate from the main `wasm` event
    /// that is always created.
    ///
    /// The `wasm-` prefix will be appended by the runtime to the provided type
    /// of event.
    pub fn add_event(mut self, event: Event) -> Self {}

    /// Bulk add attributes included in the main `wasm` event.
    ///
    /// Anything that can be turned into an iterator and yields something
    /// that can be converted into an `Attribute` is accepted.
    ///
    /// ## Examples
    ///
    /// ```
    /// use cosmwasm_std::{attr, IbcReceiveResponse};
    ///
    /// let attrs = vec![
    ///     ("action", "reaction"),
    ///     ("answer", "42"),
    ///     ("another", "attribute"),
    /// ];
    /// let res: IbcReceiveResponse = IbcReceiveResponse::new().add_attributes(attrs.clone());
    /// assert_eq!(res.attributes, attrs);
    /// ```
    pub fn add_attributes<A: Into<Attribute>>(
        mut self,
        attrs: impl IntoIterator<Item = A>,
    ) -> Self {}

    /// Bulk add "fire and forget" messages to the list of messages to process.
    ///
    /// ## Examples
    ///
    /// ```
    /// use cosmwasm_std::{CosmosMsg, IbcReceiveResponse};
    ///
    /// fn make_response_with_msgs(msgs: Vec<CosmosMsg>) -> IbcReceiveResponse {}
    /// ```
    pub fn add_messages<M: Into<CosmosMsg<T>>>(self, msgs: impl IntoIterator<Item = M>) -> Self {}

    /// Bulk add explicit SubMsg structs to the list of messages to process.
    ///
    /// ## Examples
    ///
    /// ```
    /// use cosmwasm_std::{SubMsg, IbcReceiveResponse};
    ///
    /// fn make_response_with_submsgs(msgs: Vec<SubMsg>) -> IbcReceiveResponse {}
    /// ```
    pub fn add_submessages(mut self, msgs: impl IntoIterator<Item = SubMsg<T>>) -> Self {}

    /// Bulk add custom events to the response. These are separate from the main
    /// `wasm` event.
    ///
    /// The `wasm-` prefix will be appended by the runtime to the provided types
    /// of events.
    pub fn add_events(mut self, events: impl IntoIterator<Item = Event>) -> Self {}
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json_wasm::to_string;

    #[test]
    // added this to check json format for go compat, as I was unsure how some messages are snake encoded
    fn serialize_msg() {}

    #[test]
    fn ibc_timeout_serialize() {}

    #[test]
    #[allow(clippy::eq_op)]
    fn ibc_timeout_block_ord() {}

    #[test]
    fn ibc_packet_serialize() {}
}
}
mod import_helpers {
/// Returns the four most significant bytes
#[allow(dead_code)] // only used in Wasm builds
#[inline]
pub fn from_high_half(data: u64) -> u32 {}

/// Returns the four least significant bytes
#[allow(dead_code)] // only used in Wasm builds
#[inline]
pub fn from_low_half(data: u64) -> u32 {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn from_high_half_works() {}

    #[test]
    fn from_low_haf_works() {}
}
}
#[cfg(feature = "iterator")]
mod iterator {
use crate::errors::StdError;

/// A record of a key-value storage that is created through an iterator API.
/// The first element (key) is always raw binary data. The second element
/// (value) is binary by default but can be changed to a custom type. This
/// allows contracts to reuse the type when deserializing database records.
pub type Record<V = Vec<u8>> = (Vec<u8>, V);

#[derive(Copy, Clone)]
// We assign these to integers to provide a stable API for passing over FFI (to wasm and Go)
pub enum Order {
    Ascending = 1,
    Descending = 2,
}

impl TryFrom<i32> for Order {
    type Error = StdError;

    fn try_from(value: i32) -> Result<Self, Self::Error> {}
}

impl From<Order> for i32 {
    fn from(original: Order) -> i32 {}
}
}
mod math {
mod decimal {
use forward_ref::{forward_ref_binop, forward_ref_op_assign};
use schemars::JsonSchema;
use serde::{de, ser, Deserialize, Deserializer, Serialize};
use std::cmp::Ordering;
use std::fmt::{self, Write};
use std::ops::{Add, AddAssign, Div, DivAssign, Mul, MulAssign, Rem, RemAssign, Sub, SubAssign};
use std::str::FromStr;
use thiserror::Error;

use crate::errors::{
    CheckedFromRatioError, CheckedMultiplyRatioError, DivideByZeroError, OverflowError,
    OverflowOperation, RoundUpOverflowError, StdError,
};

use super::Fraction;
use super::Isqrt;
use super::{Uint128, Uint256};

/// A fixed-point decimal value with 18 fractional digits, i.e. Decimal(1_000_000_000_000_000_000) == 1.0
///
/// The greatest possible value that can be represented is 340282366920938463463.374607431768211455 (which is (2^128 - 1) / 10^18)
#[derive(Copy, Clone, Default, Debug, PartialEq, Eq, PartialOrd, Ord, JsonSchema)]
pub struct Decimal(#[schemars(with = "String")] Uint128);

#[derive(Error, Debug, PartialEq, Eq)]
#[error("Decimal range exceeded")]
pub struct DecimalRangeExceeded;

impl Decimal {
    const DECIMAL_FRACTIONAL: Uint128 = Uint128::new(1_000_000_000_000_000_000u128); // 1*10**18
    const DECIMAL_FRACTIONAL_SQUARED: Uint128 =
        Uint128::new(1_000_000_000_000_000_000_000_000_000_000_000_000u128); // (1*10**18)**2 = 1*10**36

    /// The number of decimal places. Since decimal types are fixed-point rather than
    /// floating-point, this is a constant.
    pub const DECIMAL_PLACES: u32 = 18; // This needs to be an even number.
    /// The largest value that can be represented by this decimal type.
    pub const MAX: Self = Self(Uint128::MAX);
    /// The smallest value that can be represented by this decimal type.
    pub const MIN: Self = Self(Uint128::MIN);

    /// Creates a Decimal(value)
    /// This is equivalent to `Decimal::from_atomics(value, 18)` but usable in a const context.
    pub const fn new(value: Uint128) -> Self {}

    /// Creates a Decimal(Uint128(value))
    /// This is equivalent to `Decimal::from_atomics(value, 18)` but usable in a const context.
    pub const fn raw(value: u128) -> Self {}

    /// Create a 1.0 Decimal
    #[inline]
    pub const fn one() -> Self {}

    /// Create a 0.0 Decimal
    #[inline]
    pub const fn zero() -> Self {}

    /// Convert x% into Decimal
    pub fn percent(x: u64) -> Self {}

    /// Convert permille (x/1000) into Decimal
    pub fn permille(x: u64) -> Self {}

    /// Creates a decimal from a number of atomic units and the number
    /// of decimal places. The inputs will be converted internally to form
    /// a decimal with 18 decimal places. So the input 123 and 2 will create
    /// the decimal 1.23.
    ///
    /// Using 18 decimal places is slightly more efficient than other values
    /// as no internal conversion is necessary.
    ///
    /// ## Examples
    ///
    /// ```
    /// # use cosmwasm_std::{Decimal, Uint128};
    /// let a = Decimal::from_atomics(Uint128::new(1234), 3).unwrap();
    /// assert_eq!(a.to_string(), "1.234");
    ///
    /// let a = Decimal::from_atomics(1234u128, 0).unwrap();
    /// assert_eq!(a.to_string(), "1234");
    ///
    /// let a = Decimal::from_atomics(1u64, 18).unwrap();
    /// assert_eq!(a.to_string(), "0.000000000000000001");
    /// ```
    pub fn from_atomics(
        atomics: impl Into<Uint128>,
        decimal_places: u32,
    ) -> Result<Self, DecimalRangeExceeded> {}

    /// Returns the ratio (numerator / denominator) as a Decimal
    pub fn from_ratio(numerator: impl Into<Uint128>, denominator: impl Into<Uint128>) -> Self {}

    /// Returns the ratio (numerator / denominator) as a Decimal
    pub fn checked_from_ratio(
        numerator: impl Into<Uint128>,
        denominator: impl Into<Uint128>,
    ) -> Result<Self, CheckedFromRatioError> {}

    pub const fn is_zero(&self) -> bool {}

    /// A decimal is an integer of atomic units plus a number that specifies the
    /// position of the decimal dot. So any decimal can be expressed as two numbers.
    ///
    /// ## Examples
    ///
    /// ```
    /// # use cosmwasm_std::{Decimal, Uint128};
    /// # use std::str::FromStr;
    /// // Value with whole and fractional part
    /// let a = Decimal::from_str("1.234").unwrap();
    /// assert_eq!(a.decimal_places(), 18);
    /// assert_eq!(a.atomics(), Uint128::new(1234000000000000000));
    ///
    /// // Smallest possible value
    /// let b = Decimal::from_str("0.000000000000000001").unwrap();
    /// assert_eq!(b.decimal_places(), 18);
    /// assert_eq!(b.atomics(), Uint128::new(1));
    /// ```
    #[inline]
    pub const fn atomics(&self) -> Uint128 {}

    /// The number of decimal places. This is a constant value for now
    /// but this could potentially change as the type evolves.
    ///
    /// See also [`Decimal::atomics()`].
    #[inline]
    pub const fn decimal_places(&self) -> u32 {}

    /// Rounds value down after decimal places.
    pub fn floor(&self) -> Self {}

    /// Rounds value up after decimal places. Panics on overflow.
    pub fn ceil(&self) -> Self {}

    /// Rounds value up after decimal places. Returns OverflowError on overflow.
    pub fn checked_ceil(&self) -> Result<Self, RoundUpOverflowError> {}

    pub fn checked_add(self, other: Self) -> Result<Self, OverflowError> {}

    pub fn checked_sub(self, other: Self) -> Result<Self, OverflowError> {}

    /// Multiplies one `Decimal` by another, returning an `OverflowError` if an overflow occurred.
    pub fn checked_mul(self, other: Self) -> Result<Self, OverflowError> {}

    /// Raises a value to the power of `exp`, panics if an overflow occurred.
    pub fn pow(self, exp: u32) -> Self {}

    /// Raises a value to the power of `exp`, returning an `OverflowError` if an overflow occurred.
    pub fn checked_pow(self, exp: u32) -> Result<Self, OverflowError> {}

    pub fn checked_div(self, other: Self) -> Result<Self, CheckedFromRatioError> {}

    pub fn checked_rem(self, other: Self) -> Result<Self, DivideByZeroError> {}

    /// Returns the approximate square root as a Decimal.
    ///
    /// This should not overflow or panic.
    pub fn sqrt(&self) -> Self {}

    /// Lower precision means more aggressive rounding, but less risk of overflow.
    /// Precision *must* be a number between 0 and 9 (inclusive).
    ///
    /// Returns `None` if the internal multiplication overflows.
    fn sqrt_with_precision(&self, precision: u32) -> Option<Self> {}

    pub const fn abs_diff(self, other: Self) -> Self {}

    pub fn saturating_add(self, other: Self) -> Self {}

    pub fn saturating_sub(self, other: Self) -> Self {}

    pub fn saturating_mul(self, other: Self) -> Self {}

    pub fn saturating_pow(self, exp: u32) -> Self {}
}

impl Fraction<Uint128> for Decimal {
    #[inline]
    fn numerator(&self) -> Uint128 {}

    #[inline]
    fn denominator(&self) -> Uint128 {}

    /// Returns the multiplicative inverse `1/d` for decimal `d`.
    ///
    /// If `d` is zero, none is returned.
    fn inv(&self) -> Option<Self> {}
}

impl FromStr for Decimal {
    type Err = StdError;

    /// Converts the decimal string to a Decimal
    /// Possible inputs: "1.23", "1", "000012", "1.123000000"
    /// Disallowed: "", ".23"
    ///
    /// This never performs any kind of rounding.
    /// More than DECIMAL_PLACES fractional digits, even zeros, result in an error.
    fn from_str(input: &str) -> Result<Self, Self::Err> {}
}

impl fmt::Display for Decimal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {}
}

impl Add for Decimal {
    type Output = Self;

    fn add(self, other: Self) -> Self {}
}
forward_ref_binop!(impl Add, add for Decimal, Decimal);

impl AddAssign for Decimal {
    fn add_assign(&mut self, rhs: Decimal) {}
}
forward_ref_op_assign!(impl AddAssign, add_assign for Decimal, Decimal);

impl Sub for Decimal {
    type Output = Self;

    fn sub(self, other: Self) -> Self {}
}
forward_ref_binop!(impl Sub, sub for Decimal, Decimal);

impl SubAssign for Decimal {
    fn sub_assign(&mut self, rhs: Decimal) {}
}
forward_ref_op_assign!(impl SubAssign, sub_assign for Decimal, Decimal);

impl Mul for Decimal {
    type Output = Self;

    #[allow(clippy::suspicious_arithmetic_impl)]
    fn mul(self, other: Self) -> Self {}
}
forward_ref_binop!(impl Mul, mul for Decimal, Decimal);

impl MulAssign for Decimal {
    fn mul_assign(&mut self, rhs: Decimal) {}
}
forward_ref_op_assign!(impl MulAssign, mul_assign for Decimal, Decimal);

/// Both d*u and u*d with d: Decimal and u: Uint128 returns an Uint128. There is no
/// specific reason for this decision other than the initial use cases we have. If you
/// need a Decimal result for the same calculation, use Decimal(d*u) or Decimal(u*d).
impl Mul<Decimal> for Uint128 {
    type Output = Self;

    #[allow(clippy::suspicious_arithmetic_impl)]
    fn mul(self, rhs: Decimal) -> Self::Output {}
}

impl Mul<Uint128> for Decimal {
    type Output = Uint128;

    fn mul(self, rhs: Uint128) -> Self::Output {}
}

impl Div for Decimal {
    type Output = Self;

    fn div(self, other: Self) -> Self {}
}
forward_ref_binop!(impl Div, div for Decimal, Decimal);

impl DivAssign for Decimal {
    fn div_assign(&mut self, rhs: Decimal) {}
}
forward_ref_op_assign!(impl DivAssign, div_assign for Decimal, Decimal);

impl Div<Uint128> for Decimal {
    type Output = Self;

    fn div(self, rhs: Uint128) -> Self::Output {}
}

impl DivAssign<Uint128> for Decimal {
    fn div_assign(&mut self, rhs: Uint128) {}
}

impl Rem for Decimal {
    type Output = Self;

    /// # Panics
    ///
    /// This operation will panic if `rhs` is zero
    #[inline]
    fn rem(self, rhs: Self) -> Self {}
}
forward_ref_binop!(impl Rem, rem for Decimal, Decimal);

impl RemAssign<Decimal> for Decimal {
    fn rem_assign(&mut self, rhs: Decimal) {}
}
forward_ref_op_assign!(impl RemAssign, rem_assign for Decimal, Decimal);

impl<A> std::iter::Sum<A> for Decimal
where
    Self: Add<A, Output = Self>,
{
    fn sum<I: Iterator<Item = A>>(iter: I) -> Self {}
}

/// Serializes as a decimal string
impl Serialize for Decimal {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: ser::Serializer,
    {}
}

/// Deserializes as a base64 string
impl<'de> Deserialize<'de> for Decimal {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {}
}

struct DecimalVisitor;

impl<'de> de::Visitor<'de> for DecimalVisitor {
    type Value = Decimal;

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {}

    fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
    where
        E: de::Error,
    {}
}

impl PartialEq<&Decimal> for Decimal {
    fn eq(&self, rhs: &&Decimal) -> bool {}
}

impl PartialEq<Decimal> for &Decimal {
    fn eq(&self, rhs: &Decimal) -> bool {}
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{from_slice, to_vec};

    fn dec(input: &str) -> Decimal {}

    #[test]
    fn decimal_new() {}

    #[test]
    fn decimal_raw() {}

    #[test]
    fn decimal_one() {}

    #[test]
    fn decimal_zero() {}

    #[test]
    fn decimal_percent() {}

    #[test]
    fn decimal_permille() {}

    #[test]
    fn decimal_from_atomics_works() {}

    #[test]
    fn decimal_from_ratio_works() {}

    #[test]
    #[should_panic(expected = "Denominator must not be zero")]
    fn decimal_from_ratio_panics_for_zero_denominator() {}

    #[test]
    #[should_panic(expected = "Multiplication overflow")]
    fn decimal_from_ratio_panics_for_mul_overflow() {}

    #[test]
    fn decimal_checked_from_ratio_does_not_panic() {}

    #[test]
    fn decimal_implements_fraction() {}

    #[test]
    fn decimal_from_str_works() {}

    #[test]
    fn decimal_from_str_errors_for_broken_whole_part() {}

    #[test]
    fn decimal_from_str_errors_for_broken_fractinal_part() {}

    #[test]
    fn decimal_from_str_errors_for_more_than_18_fractional_digits() {}

    #[test]
    fn decimal_from_str_errors_for_invalid_number_of_dots() {}

    #[test]
    fn decimal_from_str_errors_for_more_than_max_value() {}

    #[test]
    fn decimal_atomics_works() {}

    #[test]
    fn decimal_decimal_places_works() {}

    #[test]
    fn decimal_is_zero_works() {}

    #[test]
    fn decimal_inv_works() {}

    #[test]
    #[allow(clippy::op_ref)]
    fn decimal_add_works() {}

    #[test]
    #[should_panic(expected = "attempt to add with overflow")]
    fn decimal_add_overflow_panics() {}

    #[test]
    fn decimal_add_assign_works() {}

    #[test]
    #[allow(clippy::op_ref)]
    fn decimal_sub_works() {}

    #[test]
    #[should_panic(expected = "attempt to subtract with overflow")]
    fn decimal_sub_overflow_panics() {}

    #[test]
    fn decimal_sub_assign_works() {}

    #[test]
    #[allow(clippy::op_ref)]
    fn decimal_implements_mul() {}

    #[test]
    fn decimal_mul_assign_works() {}

    #[test]
    #[should_panic(expected = "attempt to multiply with overflow")]
    fn decimal_mul_overflow_panics() {}

    #[test]
    fn decimal_checked_mul() {}

    #[test]
    fn decimal_checked_mul_overflow() {}

    #[test]
    // in this test the Decimal is on the right
    fn uint128_decimal_multiply() {}

    #[test]
    // in this test the Decimal is on the left
    fn decimal_uint128_multiply() {}

    #[test]
    #[allow(clippy::op_ref)]
    fn decimal_implements_div() {}

    #[test]
    fn decimal_div_assign_works() {}

    #[test]
    #[should_panic(expected = "Division failed - multiplication overflow")]
    fn decimal_div_overflow_panics() {}

    #[test]
    #[should_panic(expected = "Division failed - denominator must not be zero")]
    fn decimal_div_by_zero_panics() {}

    #[test]
    fn decimal_uint128_division() {}

    #[test]
    #[should_panic(expected = "attempt to divide by zero")]
    fn decimal_uint128_divide_by_zero() {}

    #[test]
    fn decimal_uint128_div_assign() {}

    #[test]
    #[should_panic(expected = "attempt to divide by zero")]
    fn decimal_uint128_div_assign_by_zero() {}

    #[test]
    fn decimal_uint128_sqrt() {}

    /// sqrt(2) is an irrational number, i.e. all 18 decimal places should be used.
    #[test]
    fn decimal_uint128_sqrt_is_precise() {}

    #[test]
    fn decimal_uint128_sqrt_does_not_overflow() {}

    #[test]
    fn decimal_uint128_sqrt_intermediate_precision_used() {}

    #[test]
    fn decimal_checked_pow() {}

    #[test]
    fn decimal_checked_pow_overflow() {}

    #[test]
    fn decimal_to_string() {}

    #[test]
    fn decimal_iter_sum() {}

    #[test]
    fn decimal_serialize() {}

    #[test]
    fn decimal_deserialize() {}

    #[test]
    fn decimal_abs_diff_works() {}

    #[test]
    #[allow(clippy::op_ref)]
    fn decimal_rem_works() {}

    #[test]
    fn decimal_rem_assign_works() {}

    #[test]
    #[should_panic(expected = "divisor of zero")]
    fn decimal_rem_panics_for_zero() {}

    #[test]
    fn decimal_checked_methods() {}

    #[test]
    fn decimal_pow_works() {}

    #[test]
    #[should_panic]
    fn decimal_pow_overflow_panics() {}

    #[test]
    fn decimal_saturating_works() {}

    #[test]
    fn decimal_rounding() {}

    #[test]
    #[should_panic(expected = "attempt to ceil with overflow")]
    fn decimal_ceil_panics() {}

    #[test]
    fn decimal_checked_ceil() {}

    #[test]
    fn decimal_partial_eq() {}
}
}
mod decimal256 {
use forward_ref::{forward_ref_binop, forward_ref_op_assign};
use schemars::JsonSchema;
use serde::{de, ser, Deserialize, Deserializer, Serialize};
use std::cmp::Ordering;
use std::fmt::{self, Write};
use std::ops::{Add, AddAssign, Div, DivAssign, Mul, MulAssign, Rem, RemAssign, Sub, SubAssign};
use std::str::FromStr;
use thiserror::Error;

use crate::errors::{
    CheckedFromRatioError, CheckedMultiplyRatioError, DivideByZeroError, OverflowError,
    OverflowOperation, RoundUpOverflowError, StdError,
};
use crate::{Decimal, Uint512};

use super::Fraction;
use super::Isqrt;
use super::Uint256;

/// A fixed-point decimal value with 18 fractional digits, i.e. Decimal256(1_000_000_000_000_000_000) == 1.0
///
/// The greatest possible value that can be represented is
/// 115792089237316195423570985008687907853269984665640564039457.584007913129639935
/// (which is (2^256 - 1) / 10^18)
#[derive(Copy, Clone, Default, Debug, PartialEq, Eq, PartialOrd, Ord, JsonSchema)]
pub struct Decimal256(#[schemars(with = "String")] Uint256);

#[derive(Error, Debug, PartialEq, Eq)]
#[error("Decimal256 range exceeded")]
pub struct Decimal256RangeExceeded;

impl Decimal256 {
    const DECIMAL_FRACTIONAL: Uint256 = // 1*10**18
        Uint256::from_be_bytes([
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 13, 224, 182,
            179, 167, 100, 0, 0,
        ]);
    const DECIMAL_FRACTIONAL_SQUARED: Uint256 = // 1*10**36
        Uint256::from_be_bytes([
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 192, 151, 206, 123, 201, 7, 21, 179,
            75, 159, 16, 0, 0, 0, 0,
        ]);

    /// The number of decimal places. Since decimal types are fixed-point rather than
    /// floating-point, this is a constant.
    pub const DECIMAL_PLACES: u32 = 18;
    /// The largest value that can be represented by this decimal type.
    pub const MAX: Self = Self(Uint256::MAX);
    /// The smallest value that can be represented by this decimal type.
    pub const MIN: Self = Self(Uint256::MIN);

    /// Creates a Decimal256 from Uint256
    /// This is equivalent to `Decimal256::from_atomics(value, 18)` but usable in a const context.
    pub const fn new(value: Uint256) -> Self {}

    /// Creates a Decimal256 from u128
    /// This is equivalent to `Decimal256::from_atomics(value, 18)` but usable in a const context.
    pub const fn raw(value: u128) -> Self {}

    /// Create a 1.0 Decimal256
    #[inline]
    pub const fn one() -> Self {}

    /// Create a 0.0 Decimal256
    #[inline]
    pub const fn zero() -> Self {}

    /// Convert x% into Decimal256
    pub fn percent(x: u64) -> Self {}

    /// Convert permille (x/1000) into Decimal256
    pub fn permille(x: u64) -> Self {}

    /// Creates a decimal from a number of atomic units and the number
    /// of decimal places. The inputs will be converted internally to form
    /// a decimal with 18 decimal places. So the input 123 and 2 will create
    /// the decimal 1.23.
    ///
    /// Using 18 decimal places is slightly more efficient than other values
    /// as no internal conversion is necessary.
    ///
    /// ## Examples
    ///
    /// ```
    /// # use cosmwasm_std::{Decimal256, Uint256};
    /// let a = Decimal256::from_atomics(1234u64, 3).unwrap();
    /// assert_eq!(a.to_string(), "1.234");
    ///
    /// let a = Decimal256::from_atomics(1234u128, 0).unwrap();
    /// assert_eq!(a.to_string(), "1234");
    ///
    /// let a = Decimal256::from_atomics(1u64, 18).unwrap();
    /// assert_eq!(a.to_string(), "0.000000000000000001");
    ///
    /// let a = Decimal256::from_atomics(Uint256::MAX, 18).unwrap();
    /// assert_eq!(a, Decimal256::MAX);
    /// ```
    pub fn from_atomics(
        atomics: impl Into<Uint256>,
        decimal_places: u32,
    ) -> Result<Self, Decimal256RangeExceeded> {}

    /// Returns the ratio (numerator / denominator) as a Decimal256
    pub fn from_ratio(numerator: impl Into<Uint256>, denominator: impl Into<Uint256>) -> Self {}

    /// Returns the ratio (numerator / denominator) as a Decimal256
    pub fn checked_from_ratio(
        numerator: impl Into<Uint256>,
        denominator: impl Into<Uint256>,
    ) -> Result<Self, CheckedFromRatioError> {}

    pub const fn is_zero(&self) -> bool {}

    /// A decimal is an integer of atomic units plus a number that specifies the
    /// position of the decimal dot. So any decimal can be expressed as two numbers.
    ///
    /// ## Examples
    ///
    /// ```
    /// # use cosmwasm_std::{Decimal256, Uint256};
    /// # use std::str::FromStr;
    /// // Value with whole and fractional part
    /// let a = Decimal256::from_str("1.234").unwrap();
    /// assert_eq!(a.decimal_places(), 18);
    /// assert_eq!(a.atomics(), Uint256::from(1234000000000000000u128));
    ///
    /// // Smallest possible value
    /// let b = Decimal256::from_str("0.000000000000000001").unwrap();
    /// assert_eq!(b.decimal_places(), 18);
    /// assert_eq!(b.atomics(), Uint256::from(1u128));
    /// ```
    #[inline]
    pub const fn atomics(&self) -> Uint256 {}

    /// The number of decimal places. This is a constant value for now
    /// but this could potentially change as the type evolves.
    ///
    /// See also [`Decimal256::atomics()`].
    #[inline]
    pub const fn decimal_places(&self) -> u32 {}

    /// Rounds value down after decimal places.
    pub fn floor(&self) -> Self {}

    /// Rounds value up after decimal places. Panics on overflow.
    pub fn ceil(&self) -> Self {}

    /// Rounds value up after decimal places. Returns OverflowError on overflow.
    pub fn checked_ceil(&self) -> Result<Self, RoundUpOverflowError> {}

    pub fn checked_add(self, other: Self) -> Result<Self, OverflowError> {}

    pub fn checked_sub(self, other: Self) -> Result<Self, OverflowError> {}

    /// Multiplies one `Decimal256` by another, returning an `OverflowError` if an overflow occurred.
    pub fn checked_mul(self, other: Self) -> Result<Self, OverflowError> {}

    /// Raises a value to the power of `exp`, panics if an overflow occurred.
    pub fn pow(self, exp: u32) -> Self {}

    /// Raises a value to the power of `exp`, returning an `OverflowError` if an overflow occurred.
    pub fn checked_pow(self, exp: u32) -> Result<Self, OverflowError> {}

    pub fn checked_div(self, other: Self) -> Result<Self, CheckedFromRatioError> {}

    pub fn checked_rem(self, other: Self) -> Result<Self, DivideByZeroError> {}

    /// Returns the approximate square root as a Decimal256.
    ///
    /// This should not overflow or panic.
    pub fn sqrt(&self) -> Self {}

    /// Lower precision means more aggressive rounding, but less risk of overflow.
    /// Precision *must* be a number between 0 and 9 (inclusive).
    ///
    /// Returns `None` if the internal multiplication overflows.
    fn sqrt_with_precision(&self, precision: u32) -> Option<Self> {}

    pub fn abs_diff(self, other: Self) -> Self {}

    pub fn saturating_add(self, other: Self) -> Self {}

    pub fn saturating_sub(self, other: Self) -> Self {}

    pub fn saturating_mul(self, other: Self) -> Self {}

    pub fn saturating_pow(self, exp: u32) -> Self {}
}

impl Fraction<Uint256> for Decimal256 {
    #[inline]
    fn numerator(&self) -> Uint256 {}

    #[inline]
    fn denominator(&self) -> Uint256 {}

    /// Returns the multiplicative inverse `1/d` for decimal `d`.
    ///
    /// If `d` is zero, none is returned.
    fn inv(&self) -> Option<Self> {}
}

impl From<Decimal> for Decimal256 {
    fn from(input: Decimal) -> Self {}
}

impl FromStr for Decimal256 {
    type Err = StdError;

    /// Converts the decimal string to a Decimal256
    /// Possible inputs: "1.23", "1", "000012", "1.123000000"
    /// Disallowed: "", ".23"
    ///
    /// This never performs any kind of rounding.
    /// More than DECIMAL_PLACES fractional digits, even zeros, result in an error.
    fn from_str(input: &str) -> Result<Self, Self::Err> {}
}

impl fmt::Display for Decimal256 {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {}
}

impl Add for Decimal256 {
    type Output = Self;

    fn add(self, other: Self) -> Self {}
}
forward_ref_binop!(impl Add, add for Decimal256, Decimal256);

impl AddAssign for Decimal256 {
    fn add_assign(&mut self, rhs: Decimal256) {}
}
forward_ref_op_assign!(impl AddAssign, add_assign for Decimal256, Decimal256);

impl Sub for Decimal256 {
    type Output = Self;

    fn sub(self, other: Self) -> Self {}
}
forward_ref_binop!(impl Sub, sub for Decimal256, Decimal256);

impl SubAssign for Decimal256 {
    fn sub_assign(&mut self, rhs: Decimal256) {}
}
forward_ref_op_assign!(impl SubAssign, sub_assign for Decimal256, Decimal256);

impl Mul for Decimal256 {
    type Output = Self;

    #[allow(clippy::suspicious_arithmetic_impl)]
    fn mul(self, other: Self) -> Self {}
}
forward_ref_binop!(impl Mul, mul for Decimal256, Decimal256);

impl MulAssign for Decimal256 {
    fn mul_assign(&mut self, rhs: Self) {}
}
forward_ref_op_assign!(impl MulAssign, mul_assign for Decimal256, Decimal256);

/// Both d*u and u*d with d: Decimal256 and u: Uint256 returns an Uint256. There is no
/// specific reason for this decision other than the initial use cases we have. If you
/// need a Decimal256 result for the same calculation, use Decimal256(d*u) or Decimal256(u*d).
impl Mul<Decimal256> for Uint256 {
    type Output = Self;

    #[allow(clippy::suspicious_arithmetic_impl)]
    fn mul(self, rhs: Decimal256) -> Self::Output {}
}

impl Mul<Uint256> for Decimal256 {
    type Output = Uint256;

    fn mul(self, rhs: Uint256) -> Self::Output {}
}

impl Div for Decimal256 {
    type Output = Self;

    fn div(self, other: Self) -> Self {}
}
forward_ref_binop!(impl Div, div for Decimal256, Decimal256);

impl DivAssign for Decimal256 {
    fn div_assign(&mut self, rhs: Decimal256) {}
}
forward_ref_op_assign!(impl DivAssign, div_assign for Decimal256, Decimal256);

impl Div<Uint256> for Decimal256 {
    type Output = Self;

    fn div(self, rhs: Uint256) -> Self::Output {}
}

impl DivAssign<Uint256> for Decimal256 {
    fn div_assign(&mut self, rhs: Uint256) {}
}

impl Rem for Decimal256 {
    type Output = Self;

    /// # Panics
    ///
    /// This operation will panic if `rhs` is zero
    #[inline]
    fn rem(self, rhs: Self) -> Self {}
}
forward_ref_binop!(impl Rem, rem for Decimal256, Decimal256);

impl RemAssign<Decimal256> for Decimal256 {
    fn rem_assign(&mut self, rhs: Decimal256) {}
}
forward_ref_op_assign!(impl RemAssign, rem_assign for Decimal256, Decimal256);

impl<A> std::iter::Sum<A> for Decimal256
where
    Self: Add<A, Output = Self>,
{
    fn sum<I: Iterator<Item = A>>(iter: I) -> Self {}
}

/// Serializes as a decimal string
impl Serialize for Decimal256 {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: ser::Serializer,
    {}
}

/// Deserializes as a base64 string
impl<'de> Deserialize<'de> for Decimal256 {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {}
}

struct Decimal256Visitor;

impl<'de> de::Visitor<'de> for Decimal256Visitor {
    type Value = Decimal256;

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {}

    fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
    where
        E: de::Error,
    {}
}

impl PartialEq<&Decimal256> for Decimal256 {
    fn eq(&self, rhs: &&Decimal256) -> bool {}
}

impl PartialEq<Decimal256> for &Decimal256 {
    fn eq(&self, rhs: &Decimal256) -> bool {}
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::errors::StdError;
    use crate::{from_slice, to_vec};

    fn dec(input: &str) -> Decimal256 {}

    #[test]
    fn decimal256_new() {}

    #[test]
    fn decimal256_raw() {}

    #[test]
    fn decimal256_one() {}

    #[test]
    fn decimal256_zero() {}

    #[test]
    fn decimal256_percent() {}

    #[test]
    fn decimal256_permille() {}

    #[test]
    fn decimal256_from_atomics_works() {}

    #[test]
    fn decimal256_from_ratio_works() {}

    #[test]
    #[should_panic(expected = "Denominator must not be zero")]
    fn decimal256_from_ratio_panics_for_zero_denominator() {}

    #[test]
    #[should_panic(expected = "Multiplication overflow")]
    fn decimal256_from_ratio_panics_for_mul_overflow() {}

    #[test]
    fn decimal256_checked_from_ratio_does_not_panic() {}

    #[test]
    fn decimal256_implements_fraction() {}

    #[test]
    fn decimal256_implements_from_decimal() {}

    #[test]
    fn decimal256_from_str_works() {}

    #[test]
    fn decimal256_from_str_errors_for_broken_whole_part() {}

    #[test]
    fn decimal256_from_str_errors_for_broken_fractinal_part() {}

    #[test]
    fn decimal256_from_str_errors_for_more_than_36_fractional_digits() {}

    #[test]
    fn decimal256_from_str_errors_for_invalid_number_of_dots() {}

    #[test]
    fn decimal256_from_str_errors_for_more_than_max_value() {}

    #[test]
    fn decimal256_atomics_works() {}

    #[test]
    fn decimal256_decimal_places_works() {}

    #[test]
    fn decimal256_is_zero_works() {}

    #[test]
    fn decimal256_inv_works() {}

    #[test]
    #[allow(clippy::op_ref)]
    fn decimal256_add_works() {}

    #[test]
    #[should_panic(expected = "attempt to add with overflow")]
    fn decimal256_add_overflow_panics() {}

    #[test]
    fn decimal256_add_assign_works() {}

    #[test]
    #[allow(clippy::op_ref)]
    fn decimal256_sub_works() {}

    #[test]
    #[should_panic(expected = "attempt to subtract with overflow")]
    fn decimal256_sub_overflow_panics() {}

    #[test]
    fn decimal256_sub_assign_works() {}

    #[test]
    #[allow(clippy::op_ref)]
    fn decimal256_implements_mul() {}

    #[test]
    fn decimal256_mul_assign_works() {}

    #[test]
    #[should_panic(expected = "attempt to multiply with overflow")]
    fn decimal256_mul_overflow_panics() {}

    #[test]
    fn decimal256_checked_mul() {}

    #[test]
    fn decimal256_checked_mul_overflow() {}

    #[test]
    // in this test the Decimal256 is on the right
    fn uint128_decimal_multiply() {}

    #[test]
    // in this test the Decimal256 is on the left
    fn decimal256_uint128_multiply() {}

    #[test]
    #[allow(clippy::op_ref)]
    fn decimal256_implements_div() {}

    #[test]
    fn decimal256_div_assign_works() {}

    #[test]
    #[should_panic(expected = "Division failed - multiplication overflow")]
    fn decimal256_div_overflow_panics() {}

    #[test]
    #[should_panic(expected = "Division failed - denominator must not be zero")]
    fn decimal256_div_by_zero_panics() {}

    #[test]
    fn decimal256_uint128_division() {}

    #[test]
    #[should_panic(expected = "attempt to divide by zero")]
    fn decimal256_uint128_divide_by_zero() {}

    #[test]
    fn decimal256_uint128_div_assign() {}

    #[test]
    #[should_panic(expected = "attempt to divide by zero")]
    fn decimal256_uint128_div_assign_by_zero() {}

    #[test]
    fn decimal256_uint128_sqrt() {}

    /// sqrt(2) is an irrational number, i.e. all 36 decimal places should be used.
    #[test]
    fn decimal256_uint128_sqrt_is_precise() {}

    #[test]
    fn decimal256_uint128_sqrt_does_not_overflow() {}

    #[test]
    fn decimal256_uint128_sqrt_intermediate_precision_used() {}

    #[test]
    fn decimal256_checked_pow() {}

    #[test]
    fn decimal256_checked_pow_overflow() {}

    #[test]
    fn decimal256_to_string() {}

    #[test]
    fn decimal256_iter_sum() {}

    #[test]
    fn decimal256_serialize() {}

    #[test]
    fn decimal256_deserialize() {}

    #[test]
    fn decimal256_abs_diff_works() {}

    #[test]
    #[allow(clippy::op_ref)]
    fn decimal256_rem_works() {}

    #[test]
    fn decimal_rem_assign_works() {}

    #[test]
    #[should_panic(expected = "division by zero")]
    fn decimal256_rem_panics_for_zero() {}

    #[test]
    fn decimal256_checked_methods() {}

    #[test]
    fn decimal256_pow_works() {}

    #[test]
    #[should_panic]
    fn decimal256_pow_overflow_panics() {}

    #[test]
    fn decimal256_saturating_works() {}

    #[test]
    fn decimal256_rounding() {}

    #[test]
    #[should_panic(expected = "attempt to ceil with overflow")]
    fn decimal256_ceil_panics() {}

    #[test]
    fn decimal256_checked_ceil() {}

    #[test]
    fn decimal256_partial_eq() {}
}
}
mod fraction {
/// A fraction `p`/`q` with integers `p` and `q`.
///
/// `p` is called the numerator and `q` is called the denominator.
pub trait Fraction<T>: Sized {
    /// Returns the numerator `p`
    fn numerator(&self) -> T;
    /// Returns the denominator `q`
    fn denominator(&self) -> T;

    /// Returns the multiplicative inverse `q/p` for fraction `p/q`.
    ///
    /// If `p` is zero, None is returned.
    fn inv(&self) -> Option<Self>;
}
}
mod isqrt {
use std::{cmp, ops};

use crate::{Uint128, Uint256, Uint512, Uint64};

/// A trait for calculating the
/// [integer square root](https://en.wikipedia.org/wiki/Integer_square_root).
pub trait Isqrt {
    /// The [integer square root](https://en.wikipedia.org/wiki/Integer_square_root).
    fn isqrt(self) -> Self;
}

impl<I> Isqrt for I
where
    I: Unsigned
        + ops::Add<I, Output = I>
        + ops::Div<I, Output = I>
        + ops::Shr<u32, Output = I>
        + cmp::PartialOrd
        + Copy
        + From<u8>,
{
    /// Algorithm adapted from
    /// [Wikipedia](https://en.wikipedia.org/wiki/Integer_square_root#Example_implementation_in_C).
    fn isqrt(self) -> Self {}
}

/// Marker trait for types that represent unsigned integers.
pub trait Unsigned {}
impl Unsigned for u8 {}
impl Unsigned for u16 {}
impl Unsigned for u32 {}
impl Unsigned for u64 {}
impl Unsigned for u128 {}
impl Unsigned for Uint64 {}
impl Unsigned for Uint128 {}
impl Unsigned for Uint256 {}
impl Unsigned for Uint512 {}
impl Unsigned for usize {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn isqrt_primitives() {}

    #[test]
    fn isqrt_uint64() {}

    #[test]
    fn isqrt_uint128() {}

    #[test]
    fn isqrt_uint256() {}

    #[test]
    fn isqrt_uint512() {}
}
}
mod uint128 {
use forward_ref::{forward_ref_binop, forward_ref_op_assign};
use schemars::JsonSchema;
use serde::{de, ser, Deserialize, Deserializer, Serialize};
use std::fmt::{self};
use std::ops::{
    Add, AddAssign, Div, DivAssign, Mul, MulAssign, Rem, RemAssign, Shr, ShrAssign, Sub, SubAssign,
};
use std::str::FromStr;

use crate::errors::{
    CheckedMultiplyRatioError, DivideByZeroError, OverflowError, OverflowOperation, StdError,
};
use crate::{ConversionOverflowError, Uint256, Uint64};

/// A thin wrapper around u128 that is using strings for JSON encoding/decoding,
/// such that the full u128 range can be used for clients that convert JSON numbers to floats,
/// like JavaScript and jq.
///
/// # Examples
///
/// Use `from` to create instances of this and `u128` to get the value out:
///
/// ```
/// # use cosmwasm_std::Uint128;
/// let a = Uint128::from(123u128);
/// assert_eq!(a.u128(), 123);
///
/// let b = Uint128::from(42u64);
/// assert_eq!(b.u128(), 42);
///
/// let c = Uint128::from(70u32);
/// assert_eq!(c.u128(), 70);
/// ```
#[derive(Copy, Clone, Default, Debug, PartialEq, Eq, PartialOrd, Ord, JsonSchema)]
pub struct Uint128(#[schemars(with = "String")] u128);

impl Uint128 {
    pub const MAX: Self = Self(u128::MAX);
    pub const MIN: Self = Self(u128::MIN);

    /// Creates a Uint128(value).
    ///
    /// This method is less flexible than `from` but can be called in a const context.
    pub const fn new(value: u128) -> Self {}

    /// Creates a Uint128(0)
    #[inline]
    pub const fn zero() -> Self {}

    /// Creates a Uint128(1)
    #[inline]
    pub const fn one() -> Self {}

    /// Returns a copy of the internal data
    pub const fn u128(&self) -> u128 {}

    /// Returns a copy of the number as big endian bytes.
    pub const fn to_be_bytes(self) -> [u8; 16] {}

    /// Returns a copy of the number as little endian bytes.
    pub const fn to_le_bytes(self) -> [u8; 16] {}

    pub const fn is_zero(&self) -> bool {}

    pub fn pow(self, exp: u32) -> Self {}

    /// Returns `self * numerator / denominator`.
    ///
    /// Due to the nature of the integer division involved, the result is always floored.
    /// E.g. 5 * 99/100 = 4.
    pub fn multiply_ratio<A: Into<u128>, B: Into<u128>>(
        &self,
        numerator: A,
        denominator: B,
    ) -> Uint128 {}

    /// Returns `self * numerator / denominator`.
    ///
    /// Due to the nature of the integer division involved, the result is always floored.
    /// E.g. 5 * 99/100 = 4.
    pub fn checked_multiply_ratio<A: Into<u128>, B: Into<u128>>(
        &self,
        numerator: A,
        denominator: B,
    ) -> Result<Uint128, CheckedMultiplyRatioError> {}

    /// Multiplies two u128 values without overflow, producing an
    /// [`Uint256`].
    ///
    /// # Examples
    ///
    /// ```
    /// use cosmwasm_std::Uint128;
    ///
    /// let a = Uint128::MAX;
    /// let result = a.full_mul(2u32);
    /// assert_eq!(result.to_string(), "680564733841876926926749214863536422910");
    /// ```
    pub fn full_mul(self, rhs: impl Into<u128>) -> Uint256 {}

    pub fn checked_add(self, other: Self) -> Result<Self, OverflowError> {}

    pub fn checked_sub(self, other: Self) -> Result<Self, OverflowError> {}

    pub fn checked_mul(self, other: Self) -> Result<Self, OverflowError> {}

    pub fn checked_pow(self, exp: u32) -> Result<Self, OverflowError> {}

    pub fn checked_div(self, other: Self) -> Result<Self, DivideByZeroError> {}

    pub fn checked_div_euclid(self, other: Self) -> Result<Self, DivideByZeroError> {}

    pub fn checked_rem(self, other: Self) -> Result<Self, DivideByZeroError> {}

    #[inline]
    pub fn wrapping_add(self, other: Self) -> Self {}

    #[inline]
    pub fn wrapping_sub(self, other: Self) -> Self {}

    #[inline]
    pub fn wrapping_mul(self, other: Self) -> Self {}

    #[inline]
    pub fn wrapping_pow(self, other: u32) -> Self {}

    pub fn saturating_add(self, other: Self) -> Self {}

    pub fn saturating_sub(self, other: Self) -> Self {}

    pub fn saturating_mul(self, other: Self) -> Self {}

    pub fn saturating_pow(self, exp: u32) -> Self {}

    pub const fn abs_diff(self, other: Self) -> Self {}
}

// `From<u{128,64,32,16,8}>` is implemented manually instead of
// using `impl<T: Into<u128>> From<T> for Uint128` because
// of the conflict with `TryFrom<&str>` as described here
// https://stackoverflow.com/questions/63136970/how-do-i-work-around-the-upstream-crates-may-add-a-new-impl-of-trait-error

impl From<Uint64> for Uint128 {
    fn from(val: Uint64) -> Self {}
}

impl From<u128> for Uint128 {
    fn from(val: u128) -> Self {}
}

impl From<u64> for Uint128 {
    fn from(val: u64) -> Self {}
}

impl From<u32> for Uint128 {
    fn from(val: u32) -> Self {}
}

impl From<u16> for Uint128 {
    fn from(val: u16) -> Self {}
}

impl From<u8> for Uint128 {
    fn from(val: u8) -> Self {}
}

impl TryFrom<Uint128> for Uint64 {
    type Error = ConversionOverflowError;

    fn try_from(value: Uint128) -> Result<Self, Self::Error> {}
}

impl TryFrom<&str> for Uint128 {
    type Error = StdError;

    fn try_from(val: &str) -> Result<Self, Self::Error> {}
}

impl FromStr for Uint128 {
    type Err = StdError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {}
}

impl From<Uint128> for String {
    fn from(original: Uint128) -> Self {}
}

impl From<Uint128> for u128 {
    fn from(original: Uint128) -> Self {}
}

impl fmt::Display for Uint128 {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {}
}

impl Add<Uint128> for Uint128 {
    type Output = Self;

    fn add(self, rhs: Self) -> Self {}
}

impl<'a> Add<&'a Uint128> for Uint128 {
    type Output = Self;

    fn add(self, rhs: &'a Uint128) -> Self {}
}

impl Sub<Uint128> for Uint128 {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self {}
}
forward_ref_binop!(impl Sub, sub for Uint128, Uint128);

impl SubAssign<Uint128> for Uint128 {
    fn sub_assign(&mut self, rhs: Uint128) {}
}
forward_ref_op_assign!(impl SubAssign, sub_assign for Uint128, Uint128);

impl Mul<Uint128> for Uint128 {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {}
}
forward_ref_binop!(impl Mul, mul for Uint128, Uint128);

impl MulAssign<Uint128> for Uint128 {
    fn mul_assign(&mut self, rhs: Self) {}
}
forward_ref_op_assign!(impl MulAssign, mul_assign for Uint128, Uint128);

impl Div<Uint128> for Uint128 {
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {}
}

impl<'a> Div<&'a Uint128> for Uint128 {
    type Output = Self;

    fn div(self, rhs: &'a Uint128) -> Self::Output {}
}

impl Shr<u32> for Uint128 {
    type Output = Self;

    fn shr(self, rhs: u32) -> Self::Output {}
}

impl<'a> Shr<&'a u32> for Uint128 {
    type Output = Self;

    fn shr(self, rhs: &'a u32) -> Self::Output {}
}

impl AddAssign<Uint128> for Uint128 {
    fn add_assign(&mut self, rhs: Uint128) {}
}

impl<'a> AddAssign<&'a Uint128> for Uint128 {
    fn add_assign(&mut self, rhs: &'a Uint128) {}
}

impl DivAssign<Uint128> for Uint128 {
    fn div_assign(&mut self, rhs: Self) {}
}

impl<'a> DivAssign<&'a Uint128> for Uint128 {
    fn div_assign(&mut self, rhs: &'a Uint128) {}
}

impl Rem for Uint128 {
    type Output = Self;

    /// # Panics
    ///
    /// This operation will panic if `rhs` is zero.
    #[inline]
    fn rem(self, rhs: Self) -> Self {}
}
forward_ref_binop!(impl Rem, rem for Uint128, Uint128);

impl RemAssign<Uint128> for Uint128 {
    fn rem_assign(&mut self, rhs: Uint128) {}
}
forward_ref_op_assign!(impl RemAssign, rem_assign for Uint128, Uint128);

impl ShrAssign<u32> for Uint128 {
    fn shr_assign(&mut self, rhs: u32) {}
}

impl<'a> ShrAssign<&'a u32> for Uint128 {
    fn shr_assign(&mut self, rhs: &'a u32) {}
}

impl Serialize for Uint128 {
    /// Serializes as an integer string using base 10
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: ser::Serializer,
    {}
}

impl<'de> Deserialize<'de> for Uint128 {
    /// Deserialized from an integer string using base 10
    fn deserialize<D>(deserializer: D) -> Result<Uint128, D::Error>
    where
        D: Deserializer<'de>,
    {}
}

struct Uint128Visitor;

impl<'de> de::Visitor<'de> for Uint128Visitor {
    type Value = Uint128;

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {}

    fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
    where
        E: de::Error,
    {}
}

impl<A> std::iter::Sum<A> for Uint128
where
    Self: Add<A, Output = Self>,
{
    fn sum<I: Iterator<Item = A>>(iter: I) -> Self {}
}

impl PartialEq<&Uint128> for Uint128 {
    fn eq(&self, rhs: &&Uint128) -> bool {}
}

impl PartialEq<Uint128> for &Uint128 {
    fn eq(&self, rhs: &Uint128) -> bool {}
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{from_slice, to_vec};

    #[test]
    fn uint128_zero_works() {}

    #[test]
    fn uint128_one_works() {}

    #[test]
    fn uint128_convert_into() {}

    #[test]
    fn uint128_convert_from() {}

    #[test]
    fn uint128_implements_display() {}

    #[test]
    fn uint128_display_padding_works() {}

    #[test]
    fn uint128_to_be_bytes_works() {}

    #[test]
    fn uint128_to_le_bytes_works() {}

    #[test]
    fn uint128_is_zero_works() {}

    #[test]
    fn uint128_json() {}

    #[test]
    fn uint128_compare() {}

    #[test]
    #[allow(clippy::op_ref)]
    fn uint128_math() {}

    #[test]
    #[should_panic]
    fn uint128_add_overflow_panics() {}

    #[test]
    #[allow(clippy::op_ref)]
    fn uint128_sub_works() {}

    #[test]
    #[should_panic]
    fn uint128_sub_overflow_panics() {}

    #[test]
    fn uint128_sub_assign_works() {}

    #[test]
    #[allow(clippy::op_ref)]
    fn uint128_mul_works() {}

    #[test]
    fn uint128_mul_assign_works() {}

    #[test]
    fn uint128_pow_works() {}

    #[test]
    #[should_panic]
    fn uint128_pow_overflow_panics() {}

    #[test]
    fn uint128_multiply_ratio_works() {}

    #[test]
    fn uint128_multiply_ratio_does_not_overflow_when_result_fits() {}

    #[test]
    #[should_panic]
    fn uint128_multiply_ratio_panicks_on_overflow() {}

    #[test]
    #[should_panic(expected = "Denominator must not be zero")]
    fn uint128_multiply_ratio_panics_for_zero_denominator() {}

    #[test]
    fn uint128_checked_multiply_ratio_does_not_panic() {}

    #[test]
    fn sum_works() {}

    #[test]
    fn uint128_methods() {}

    #[test]
    fn uint128_wrapping_methods() {}

    #[test]
    #[allow(clippy::op_ref)]
    fn uint128_implements_rem() {}

    #[test]
    #[should_panic(expected = "divisor of zero")]
    fn uint128_rem_panics_for_zero() {}

    #[test]
    #[allow(clippy::op_ref)]
    fn uint128_rem_works() {}

    #[test]
    fn uint128_rem_assign_works() {}

    #[test]
    fn uint128_abs_diff_works() {}

    #[test]
    fn uint128_partial_eq() {}
}
}
mod uint256 {
use forward_ref::{forward_ref_binop, forward_ref_op_assign};
use schemars::JsonSchema;
use serde::{de, ser, Deserialize, Deserializer, Serialize};
use std::fmt;
use std::ops::{
    Add, AddAssign, Div, DivAssign, Mul, MulAssign, Rem, RemAssign, Shl, Shr, ShrAssign, Sub,
    SubAssign,
};
use std::str::FromStr;

use crate::errors::{
    CheckedMultiplyRatioError, ConversionOverflowError, DivideByZeroError, OverflowError,
    OverflowOperation, StdError,
};
use crate::{Uint128, Uint512, Uint64};

/// This module is purely a workaround that lets us ignore lints for all the code
/// the `construct_uint!` macro generates.
#[allow(clippy::all)]
mod uints {
    uint::construct_uint! {
        pub struct U256(4);
    }
}

/// Used internally - we don't want to leak this type since we might change
/// the implementation in the future.
use uints::U256;

/// An implementation of u256 that is using strings for JSON encoding/decoding,
/// such that the full u256 range can be used for clients that convert JSON numbers to floats,
/// like JavaScript and jq.
///
/// # Examples
///
/// Use `from` to create instances out of primitive uint types or `new` to provide big
/// endian bytes:
///
/// ```
/// # use cosmwasm_std::Uint256;
/// let a = Uint256::from(258u128);
/// let b = Uint256::new([
///     0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
///     0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
///     0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
///     0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 1u8, 2u8,
/// ]);
/// assert_eq!(a, b);
/// ```
#[derive(Copy, Clone, Default, Debug, PartialEq, Eq, PartialOrd, Ord, JsonSchema)]
pub struct Uint256(#[schemars(with = "String")] U256);

impl Uint256 {
    pub const MAX: Uint256 = Uint256(U256::MAX);
    pub const MIN: Uint256 = Uint256(U256::zero());

    /// Creates a Uint256(value) from a big endian representation. It's just an alias for
    /// [`Uint256::from_be_bytes`].
    ///
    /// This method is less flexible than `from` but can be called in a const context.
    pub const fn new(value: [u8; 32]) -> Self {}

    /// Creates a Uint256(0)
    #[inline]
    pub const fn zero() -> Self {}

    /// Creates a Uint256(1)
    #[inline]
    pub const fn one() -> Self {}

    pub const fn from_be_bytes(data: [u8; 32]) -> Self {}

    pub const fn from_le_bytes(data: [u8; 32]) -> Self {}

    /// A conversion from `u128` that, unlike the one provided by the `From` trait,
    /// can be used in a `const` context.
    pub const fn from_u128(num: u128) -> Self {}

    /// A conversion from `Uint128` that, unlike the one provided by the `From` trait,
    /// can be used in a `const` context.
    pub const fn from_uint128(num: Uint128) -> Self {}

    /// Returns a copy of the number as big endian bytes.
    pub const fn to_be_bytes(self) -> [u8; 32] {}

    /// Returns a copy of the number as little endian bytes.
    pub const fn to_le_bytes(self) -> [u8; 32] {}

    pub const fn is_zero(&self) -> bool {}

    pub fn pow(self, exp: u32) -> Self {}

    /// Returns `self * numerator / denominator`.
    ///
    /// Due to the nature of the integer division involved, the result is always floored.
    /// E.g. 5 * 99/100 = 4.
    pub fn multiply_ratio<A: Into<Uint256>, B: Into<Uint256>>(
        &self,
        numerator: A,
        denominator: B,
    ) -> Uint256 {}

    /// Returns `self * numerator / denominator`.
    ///
    /// Due to the nature of the integer division involved, the result is always floored.
    /// E.g. 5 * 99/100 = 4.
    pub fn checked_multiply_ratio<A: Into<Uint256>, B: Into<Uint256>>(
        &self,
        numerator: A,
        denominator: B,
    ) -> Result<Uint256, CheckedMultiplyRatioError> {}

    /// Multiplies two u256 values without overflow, producing an
    /// [`Uint512`].
    ///
    /// # Examples
    ///
    /// ```
    /// use cosmwasm_std::Uint256;
    ///
    /// let a = Uint256::MAX;
    /// let result = a.full_mul(2u32);
    /// assert_eq!(
    ///     result.to_string(),
    ///     "231584178474632390847141970017375815706539969331281128078915168015826259279870",
    /// );
    /// ```
    pub fn full_mul(self, rhs: impl Into<Uint256>) -> Uint512 {}

    pub fn checked_add(self, other: Self) -> Result<Self, OverflowError> {}

    pub fn checked_sub(self, other: Self) -> Result<Self, OverflowError> {}

    pub fn checked_mul(self, other: Self) -> Result<Self, OverflowError> {}

    pub fn checked_pow(self, exp: u32) -> Result<Self, OverflowError> {}

    pub fn checked_div(self, other: Self) -> Result<Self, DivideByZeroError> {}

    pub fn checked_div_euclid(self, other: Self) -> Result<Self, DivideByZeroError> {}

    pub fn checked_rem(self, other: Self) -> Result<Self, DivideByZeroError> {}

    pub fn checked_shr(self, other: u32) -> Result<Self, OverflowError> {}

    pub fn checked_shl(self, other: u32) -> Result<Self, OverflowError> {}

    #[inline]
    pub fn wrapping_add(self, other: Self) -> Self {}

    #[inline]
    pub fn wrapping_sub(self, other: Self) -> Self {}

    #[inline]
    pub fn wrapping_mul(self, other: Self) -> Self {}

    #[inline]
    pub fn wrapping_pow(self, other: u32) -> Self {}

    pub fn saturating_add(self, other: Self) -> Self {}

    pub fn saturating_sub(self, other: Self) -> Self {}

    pub fn saturating_mul(self, other: Self) -> Self {}

    pub fn saturating_pow(self, exp: u32) -> Self {}

    pub fn abs_diff(self, other: Self) -> Self {}
}

impl From<Uint128> for Uint256 {
    fn from(val: Uint128) -> Self {}
}

impl From<Uint64> for Uint256 {
    fn from(val: Uint64) -> Self {}
}

impl From<u128> for Uint256 {
    fn from(val: u128) -> Self {}
}

impl From<u64> for Uint256 {
    fn from(val: u64) -> Self {}
}

impl From<u32> for Uint256 {
    fn from(val: u32) -> Self {}
}

impl From<u16> for Uint256 {
    fn from(val: u16) -> Self {}
}

impl From<u8> for Uint256 {
    fn from(val: u8) -> Self {}
}

impl TryFrom<Uint256> for Uint128 {
    type Error = ConversionOverflowError;

    fn try_from(value: Uint256) -> Result<Self, Self::Error> {}
}

impl TryFrom<&str> for Uint256 {
    type Error = StdError;

    fn try_from(val: &str) -> Result<Self, Self::Error> {}
}

impl FromStr for Uint256 {
    type Err = StdError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {}
}

impl From<Uint256> for String {
    fn from(original: Uint256) -> Self {}
}

impl fmt::Display for Uint256 {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {}
}

impl Add<Uint256> for Uint256 {
    type Output = Self;

    fn add(self, rhs: Self) -> Self {}
}

impl<'a> Add<&'a Uint256> for Uint256 {
    type Output = Self;

    fn add(self, rhs: &'a Uint256) -> Self {}
}

impl Sub<Uint256> for Uint256 {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self {}
}
forward_ref_binop!(impl Sub, sub for Uint256, Uint256);

impl SubAssign<Uint256> for Uint256 {
    fn sub_assign(&mut self, rhs: Uint256) {}
}
forward_ref_op_assign!(impl SubAssign, sub_assign for Uint256, Uint256);

impl Div<Uint256> for Uint256 {
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {}
}

impl<'a> Div<&'a Uint256> for Uint256 {
    type Output = Self;

    fn div(self, rhs: &'a Uint256) -> Self::Output {}
}

impl Rem for Uint256 {
    type Output = Self;

    /// # Panics
    ///
    /// This operation will panic if `rhs` is zero.
    #[inline]
    fn rem(self, rhs: Self) -> Self {}
}
forward_ref_binop!(impl Rem, rem for Uint256, Uint256);

impl RemAssign<Uint256> for Uint256 {
    fn rem_assign(&mut self, rhs: Uint256) {}
}
forward_ref_op_assign!(impl RemAssign, rem_assign for Uint256, Uint256);

impl Mul<Uint256> for Uint256 {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {}
}
forward_ref_binop!(impl Mul, mul for Uint256, Uint256);

impl MulAssign<Uint256> for Uint256 {
    fn mul_assign(&mut self, rhs: Self) {}
}
forward_ref_op_assign!(impl MulAssign, mul_assign for Uint256, Uint256);

impl Shr<u32> for Uint256 {
    type Output = Self;

    fn shr(self, rhs: u32) -> Self::Output {}
}

impl<'a> Shr<&'a u32> for Uint256 {
    type Output = Self;

    fn shr(self, rhs: &'a u32) -> Self::Output {}
}

impl Shl<u32> for Uint256 {
    type Output = Self;

    fn shl(self, rhs: u32) -> Self::Output {}
}

impl<'a> Shl<&'a u32> for Uint256 {
    type Output = Self;

    fn shl(self, rhs: &'a u32) -> Self::Output {}
}

impl AddAssign<Uint256> for Uint256 {
    fn add_assign(&mut self, rhs: Uint256) {}
}

impl<'a> AddAssign<&'a Uint256> for Uint256 {
    fn add_assign(&mut self, rhs: &'a Uint256) {}
}

impl DivAssign<Uint256> for Uint256 {
    fn div_assign(&mut self, rhs: Self) {}
}

impl<'a> DivAssign<&'a Uint256> for Uint256 {
    fn div_assign(&mut self, rhs: &'a Uint256) {}
}

impl ShrAssign<u32> for Uint256 {
    fn shr_assign(&mut self, rhs: u32) {}
}

impl<'a> ShrAssign<&'a u32> for Uint256 {
    fn shr_assign(&mut self, rhs: &'a u32) {}
}

impl Serialize for Uint256 {
    /// Serializes as an integer string using base 10
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: ser::Serializer,
    {}
}

impl<'de> Deserialize<'de> for Uint256 {
    /// Deserialized from an integer string using base 10
    fn deserialize<D>(deserializer: D) -> Result<Uint256, D::Error>
    where
        D: Deserializer<'de>,
    {}
}

struct Uint256Visitor;

impl<'de> de::Visitor<'de> for Uint256Visitor {
    type Value = Uint256;

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {}

    fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
    where
        E: de::Error,
    {}
}

impl<A> std::iter::Sum<A> for Uint256
where
    Self: Add<A, Output = Self>,
{
    fn sum<I: Iterator<Item = A>>(iter: I) -> Self {}
}

impl PartialEq<&Uint256> for Uint256 {
    fn eq(&self, rhs: &&Uint256) -> bool {}
}

impl PartialEq<Uint256> for &Uint256 {
    fn eq(&self, rhs: &Uint256) -> bool {}
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{from_slice, to_vec};

    #[test]
    fn uint256_new_works() {}

    #[test]
    fn uint256_zero_works() {}

    #[test]
    fn uin256_one_works() {}

    #[test]
    fn uint256_from_be_bytes() {}

    #[test]
    fn uint256_from_le_bytes() {}

    #[test]
    fn uint256_endianness() {}

    #[test]
    fn uint256_convert_from() {}

    #[test]
    fn uint256_convert_to_uint128() {}

    #[test]
    fn uint256_from_u128() {}

    #[test]
    fn uint256_from_uint128() {}

    #[test]
    fn uint256_implements_display() {}

    #[test]
    fn uint256_display_padding_works() {}

    #[test]
    fn uint256_to_be_bytes_works() {}

    #[test]
    fn uint256_to_le_bytes_works() {}

    #[test]
    fn uint256_is_zero_works() {}

    #[test]
    fn uint256_wrapping_methods() {}

    #[test]
    fn uint256_json() {}

    #[test]
    fn uint256_compare() {}

    #[test]
    #[allow(clippy::op_ref)]
    fn uint256_math() {}

    #[test]
    #[should_panic]
    fn uint256_add_overflow_panics() {}

    #[test]
    #[allow(clippy::op_ref)]
    fn uint256_sub_works() {}

    #[test]
    #[should_panic]
    fn uint256_sub_overflow_panics() {}

    #[test]
    fn uint256_sub_assign_works() {}

    #[test]
    #[allow(clippy::op_ref)]
    fn uint256_mul_works() {}

    #[test]
    fn uint256_mul_assign_works() {}

    #[test]
    fn uint256_pow_works() {}

    #[test]
    #[should_panic]
    fn uint256_pow_overflow_panics() {}

    #[test]
    fn uint256_multiply_ratio_works() {}

    #[test]
    fn uint256_multiply_ratio_does_not_overflow_when_result_fits() {}

    #[test]
    #[should_panic]
    fn uint256_multiply_ratio_panicks_on_overflow() {}

    #[test]
    #[should_panic(expected = "Denominator must not be zero")]
    fn uint256_multiply_ratio_panics_for_zero_denominator() {}

    #[test]
    fn uint256_checked_multiply_ratio_does_not_panic() {}

    #[test]
    fn uint256_shr_works() {}

    #[test]
    #[should_panic]
    fn uint256_shr_overflow_panics() {}

    #[test]
    fn sum_works() {}

    #[test]
    fn uint256_methods() {}

    #[test]
    #[allow(clippy::op_ref)]
    fn uint256_implements_rem() {}

    #[test]
    #[should_panic(expected = "division by zero")]
    fn uint256_rem_panics_for_zero() {}

    #[test]
    #[allow(clippy::op_ref)]
    fn uint256_rem_works() {}

    #[test]
    fn uint256_rem_assign_works() {}

    #[test]
    fn uint256_abs_diff_works() {}

    #[test]
    fn uint256_partial_eq() {}
}
}
mod uint512 {
use forward_ref::{forward_ref_binop, forward_ref_op_assign};
use schemars::JsonSchema;
use serde::{de, ser, Deserialize, Deserializer, Serialize};
use std::fmt;
use std::ops::{
    Add, AddAssign, Div, DivAssign, Mul, MulAssign, Rem, RemAssign, Shr, ShrAssign, Sub, SubAssign,
};
use std::str::FromStr;

use crate::errors::{
    ConversionOverflowError, DivideByZeroError, OverflowError, OverflowOperation, StdError,
};
use crate::{Uint128, Uint256, Uint64};

/// This module is purely a workaround that lets us ignore lints for all the code
/// the `construct_uint!` macro generates.
#[allow(clippy::all)]
mod uints {
    uint::construct_uint! {
        pub struct U512(8);
    }
}

/// Used internally - we don't want to leak this type since we might change
/// the implementation in the future.
use uints::U512;

/// An implementation of u512 that is using strings for JSON encoding/decoding,
/// such that the full u512 range can be used for clients that convert JSON numbers to floats,
/// like JavaScript and jq.
///
/// # Examples
///
/// Use `from` to create instances out of primitive uint types or `new` to provide big
/// endian bytes:
///
/// ```
/// # use cosmwasm_std::Uint512;
/// let a = Uint512::from(258u128);
/// let b = Uint512::new([
///     0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
///     0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
///     0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
///     0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
///     0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
///     0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
///     0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
///     0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 1u8, 2u8,
/// ]);
/// assert_eq!(a, b);
/// ```
#[derive(Copy, Clone, Default, Debug, PartialEq, Eq, PartialOrd, Ord, JsonSchema)]
pub struct Uint512(#[schemars(with = "String")] U512);

impl Uint512 {
    pub const MAX: Uint512 = Uint512(U512::MAX);
    pub const MIN: Uint512 = Uint512(U512::zero());

    /// Creates a Uint512(value) from a big endian representation. It's just an alias for
    /// `from_big_endian`.
    pub const fn new(value: [u8; 64]) -> Self {}

    /// Creates a Uint512(0)
    #[inline]
    pub const fn zero() -> Self {}

    /// Creates a Uint512(1)
    #[inline]
    pub const fn one() -> Self {}

    pub const fn from_be_bytes(data: [u8; 64]) -> Self {}

    pub const fn from_le_bytes(data: [u8; 64]) -> Self {}

    /// A conversion from `Uint256` that, unlike the one provided by the `From` trait,
    /// can be used in a `const` context.
    pub const fn from_uint256(num: Uint256) -> Self {}

    /// Returns a copy of the number as big endian bytes.
    pub const fn to_be_bytes(self) -> [u8; 64] {}

    /// Returns a copy of the number as little endian bytes.
    pub const fn to_le_bytes(self) -> [u8; 64] {}

    pub const fn is_zero(&self) -> bool {}

    pub fn pow(self, exp: u32) -> Self {}

    pub fn checked_add(self, other: Self) -> Result<Self, OverflowError> {}

    pub fn checked_sub(self, other: Self) -> Result<Self, OverflowError> {}

    pub fn checked_mul(self, other: Self) -> Result<Self, OverflowError> {}

    pub fn checked_pow(self, exp: u32) -> Result<Self, OverflowError> {}

    pub fn checked_div(self, other: Self) -> Result<Self, DivideByZeroError> {}

    pub fn checked_div_euclid(self, other: Self) -> Result<Self, DivideByZeroError> {}

    pub fn checked_rem(self, other: Self) -> Result<Self, DivideByZeroError> {}

    pub fn checked_shr(self, other: u32) -> Result<Self, OverflowError> {}

    #[inline]
    pub fn wrapping_add(self, other: Self) -> Self {}

    #[inline]
    pub fn wrapping_sub(self, other: Self) -> Self {}

    #[inline]
    pub fn wrapping_mul(self, other: Self) -> Self {}

    #[inline]
    pub fn wrapping_pow(self, other: u32) -> Self {}

    pub fn saturating_add(self, other: Self) -> Self {}

    pub fn saturating_sub(self, other: Self) -> Self {}

    pub fn saturating_mul(self, other: Self) -> Self {}

    pub fn saturating_pow(self, exp: u32) -> Self {}

    pub fn abs_diff(self, other: Self) -> Self {}
}

impl From<Uint256> for Uint512 {
    fn from(val: Uint256) -> Self {}
}

impl From<Uint128> for Uint512 {
    fn from(val: Uint128) -> Self {}
}

impl From<Uint64> for Uint512 {
    fn from(val: Uint64) -> Self {}
}

impl From<u128> for Uint512 {
    fn from(val: u128) -> Self {}
}

impl From<u64> for Uint512 {
    fn from(val: u64) -> Self {}
}

impl From<u32> for Uint512 {
    fn from(val: u32) -> Self {}
}

impl From<u16> for Uint512 {
    fn from(val: u16) -> Self {}
}

impl From<u8> for Uint512 {
    fn from(val: u8) -> Self {}
}

impl TryFrom<Uint512> for Uint256 {
    type Error = ConversionOverflowError;

    fn try_from(value: Uint512) -> Result<Self, Self::Error> {}
}

impl TryFrom<Uint512> for Uint128 {
    type Error = ConversionOverflowError;

    fn try_from(value: Uint512) -> Result<Self, Self::Error> {}
}

impl TryFrom<&str> for Uint512 {
    type Error = StdError;

    fn try_from(val: &str) -> Result<Self, Self::Error> {}
}

impl FromStr for Uint512 {
    type Err = StdError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {}
}

impl From<Uint512> for String {
    fn from(original: Uint512) -> Self {}
}

impl fmt::Display for Uint512 {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {}
}

impl Add<Uint512> for Uint512 {
    type Output = Self;

    fn add(self, rhs: Self) -> Self {}
}

impl<'a> Add<&'a Uint512> for Uint512 {
    type Output = Self;

    fn add(self, rhs: &'a Uint512) -> Self {}
}

impl Sub<Uint512> for Uint512 {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self {}
}
forward_ref_binop!(impl Sub, sub for Uint512, Uint512);

impl SubAssign<Uint512> for Uint512 {
    fn sub_assign(&mut self, rhs: Uint512) {}
}
forward_ref_op_assign!(impl SubAssign, sub_assign for Uint512, Uint512);

impl Div<Uint512> for Uint512 {
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {}
}

impl<'a> Div<&'a Uint512> for Uint512 {
    type Output = Self;

    fn div(self, rhs: &'a Uint512) -> Self::Output {}
}

impl Rem for Uint512 {
    type Output = Self;

    /// # Panics
    ///
    /// This operation will panic if `rhs` is zero.
    #[inline]
    fn rem(self, rhs: Self) -> Self {}
}
forward_ref_binop!(impl Rem, rem for Uint512, Uint512);

impl RemAssign<Uint512> for Uint512 {
    fn rem_assign(&mut self, rhs: Uint512) {}
}
forward_ref_op_assign!(impl RemAssign, rem_assign for Uint512, Uint512);

impl Mul<Uint512> for Uint512 {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {}
}
forward_ref_binop!(impl Mul, mul for Uint512, Uint512);

impl MulAssign<Uint512> for Uint512 {
    fn mul_assign(&mut self, rhs: Self) {}
}
forward_ref_op_assign!(impl MulAssign, mul_assign for Uint512, Uint512);

impl Shr<u32> for Uint512 {
    type Output = Self;

    fn shr(self, rhs: u32) -> Self::Output {}
}

impl<'a> Shr<&'a u32> for Uint512 {
    type Output = Self;

    fn shr(self, rhs: &'a u32) -> Self::Output {}
}

impl AddAssign<Uint512> for Uint512 {
    fn add_assign(&mut self, rhs: Uint512) {}
}

impl<'a> AddAssign<&'a Uint512> for Uint512 {
    fn add_assign(&mut self, rhs: &'a Uint512) {}
}

impl DivAssign<Uint512> for Uint512 {
    fn div_assign(&mut self, rhs: Self) {}
}

impl<'a> DivAssign<&'a Uint512> for Uint512 {
    fn div_assign(&mut self, rhs: &'a Uint512) {}
}

impl ShrAssign<u32> for Uint512 {
    fn shr_assign(&mut self, rhs: u32) {}
}

impl<'a> ShrAssign<&'a u32> for Uint512 {
    fn shr_assign(&mut self, rhs: &'a u32) {}
}

impl Serialize for Uint512 {
    /// Serializes as an integer string using base 10
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: ser::Serializer,
    {}
}

impl<'de> Deserialize<'de> for Uint512 {
    /// Deserialized from an integer string using base 10
    fn deserialize<D>(deserializer: D) -> Result<Uint512, D::Error>
    where
        D: Deserializer<'de>,
    {}
}

struct Uint512Visitor;

impl<'de> de::Visitor<'de> for Uint512Visitor {
    type Value = Uint512;

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {}

    fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
    where
        E: de::Error,
    {}
}

impl<A> std::iter::Sum<A> for Uint512
where
    Self: Add<A, Output = Self>,
{
    fn sum<I: Iterator<Item = A>>(iter: I) -> Self {}
}

impl PartialEq<&Uint512> for Uint512 {
    fn eq(&self, rhs: &&Uint512) -> bool {}
}

impl PartialEq<Uint512> for &Uint512 {
    fn eq(&self, rhs: &Uint512) -> bool {}
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{from_slice, to_vec};

    #[test]
    fn uint512_new_works() {}

    #[test]
    fn uint512_zero_works() {}

    #[test]
    fn uin512_one_works() {}

    #[test]
    fn uint512_endianness() {}

    #[test]
    fn uint512_convert_from() {}

    #[test]
    fn uint512_convert_to_uint128() {}

    #[test]
    fn uint512_from_uint256() {}

    #[test]
    fn uint512_implements_display() {}

    #[test]
    fn uint512_display_padding_works() {}

    #[test]
    fn uint512_to_be_bytes_works() {}

    #[test]
    fn uint512_to_le_bytes_works() {}

    #[test]
    fn uint512_is_zero_works() {}

    #[test]
    fn uint512_wrapping_methods() {}

    #[test]
    fn uint512_json() {}

    #[test]
    fn uint512_compare() {}

    #[test]
    #[allow(clippy::op_ref)]
    fn uint512_math() {}

    #[test]
    #[should_panic]
    fn uint512_add_overflow_panics() {}

    #[test]
    #[allow(clippy::op_ref)]
    fn uint512_sub_works() {}

    #[test]
    #[should_panic]
    fn uint512_sub_overflow_panics() {}

    #[test]
    fn uint512_sub_assign_works() {}

    #[test]
    #[allow(clippy::op_ref)]
    fn uint512_mul_works() {}

    #[test]
    fn uint512_mul_assign_works() {}

    #[test]
    fn uint512_pow_works() {}

    #[test]
    #[should_panic]
    fn uint512_pow_overflow_panics() {}

    #[test]
    fn uint512_shr_works() {}

    #[test]
    #[should_panic]
    fn uint512_shr_overflow_panics() {}

    #[test]
    fn sum_works() {}

    #[test]
    fn uint512_methods() {}

    #[test]
    #[allow(clippy::op_ref)]
    fn uint512_implements_rem() {}

    #[test]
    #[should_panic(expected = "division by zero")]
    fn uint512_rem_panics_for_zero() {}

    #[test]
    #[allow(clippy::op_ref)]
    fn uint512_rem_works() {}

    #[test]
    fn uint512_rem_assign_works() {}

    #[test]
    fn uint512_abs_diff_works() {}

    #[test]
    fn uint512_partial_eq() {}
}
}
mod uint64 {
use forward_ref::{forward_ref_binop, forward_ref_op_assign};
use schemars::JsonSchema;
use serde::{de, ser, Deserialize, Deserializer, Serialize};
use std::fmt::{self};
use std::ops::{
    Add, AddAssign, Div, DivAssign, Mul, MulAssign, Rem, RemAssign, Shr, ShrAssign, Sub, SubAssign,
};

use crate::errors::{
    CheckedMultiplyRatioError, DivideByZeroError, OverflowError, OverflowOperation, StdError,
};
use crate::Uint128;

/// A thin wrapper around u64 that is using strings for JSON encoding/decoding,
/// such that the full u64 range can be used for clients that convert JSON numbers to floats,
/// like JavaScript and jq.
///
/// # Examples
///
/// Use `from` to create instances of this and `u64` to get the value out:
///
/// ```
/// # use cosmwasm_std::Uint64;
/// let a = Uint64::from(42u64);
/// assert_eq!(a.u64(), 42);
///
/// let b = Uint64::from(70u32);
/// assert_eq!(b.u64(), 70);
/// ```
#[derive(Copy, Clone, Default, Debug, PartialEq, Eq, PartialOrd, Ord, JsonSchema)]
pub struct Uint64(#[schemars(with = "String")] u64);

impl Uint64 {
    pub const MAX: Self = Self(u64::MAX);
    pub const MIN: Self = Self(u64::MIN);

    /// Creates a Uint64(value).
    ///
    /// This method is less flexible than `from` but can be called in a const context.
    pub const fn new(value: u64) -> Self {}

    /// Creates a Uint64(0)
    #[inline]
    pub const fn zero() -> Self {}

    /// Creates a Uint64(1)
    #[inline]
    pub const fn one() -> Self {}

    /// Returns a copy of the internal data
    pub const fn u64(&self) -> u64 {}

    /// Returns a copy of the number as big endian bytes.
    pub const fn to_be_bytes(self) -> [u8; 8] {}

    /// Returns a copy of the number as little endian bytes.
    pub const fn to_le_bytes(self) -> [u8; 8] {}

    pub const fn is_zero(&self) -> bool {}

    pub fn pow(self, exp: u32) -> Self {}

    /// Returns `self * numerator / denominator`.
    ///
    /// Due to the nature of the integer division involved, the result is always floored.
    /// E.g. 5 * 99/100 = 4.
    pub fn multiply_ratio<A: Into<u64>, B: Into<u64>>(
        &self,
        numerator: A,
        denominator: B,
    ) -> Uint64 {}

    /// Returns `self * numerator / denominator`.
    ///
    /// Due to the nature of the integer division involved, the result is always floored.
    /// E.g. 5 * 99/100 = 4.
    pub fn checked_multiply_ratio<A: Into<u64>, B: Into<u64>>(
        &self,
        numerator: A,
        denominator: B,
    ) -> Result<Uint64, CheckedMultiplyRatioError> {}

    /// Multiplies two `Uint64`/`u64` values without overflow, producing an
    /// [`Uint128`].
    ///
    /// # Examples
    ///
    /// ```
    /// use cosmwasm_std::Uint64;
    ///
    /// let a = Uint64::MAX;
    /// let result = a.full_mul(2u32);
    /// assert_eq!(result.to_string(), "36893488147419103230");
    /// ```
    pub fn full_mul(self, rhs: impl Into<u64>) -> Uint128 {}

    pub fn checked_add(self, other: Self) -> Result<Self, OverflowError> {}

    pub fn checked_sub(self, other: Self) -> Result<Self, OverflowError> {}

    pub fn checked_mul(self, other: Self) -> Result<Self, OverflowError> {}

    pub fn checked_pow(self, exp: u32) -> Result<Self, OverflowError> {}

    pub fn checked_div(self, other: Self) -> Result<Self, DivideByZeroError> {}

    pub fn checked_div_euclid(self, other: Self) -> Result<Self, DivideByZeroError> {}

    pub fn checked_rem(self, other: Self) -> Result<Self, DivideByZeroError> {}

    #[inline]
    pub fn wrapping_add(self, other: Self) -> Self {}

    #[inline]
    pub fn wrapping_sub(self, other: Self) -> Self {}

    #[inline]
    pub fn wrapping_mul(self, other: Self) -> Self {}

    #[inline]
    pub fn wrapping_pow(self, other: u32) -> Self {}

    pub fn saturating_add(self, other: Self) -> Self {}

    pub fn saturating_sub(self, other: Self) -> Self {}

    pub fn saturating_mul(self, other: Self) -> Self {}

    pub fn saturating_pow(self, exp: u32) -> Self {}

    pub const fn abs_diff(self, other: Self) -> Self {}
}

// `From<u{128,64,32,16,8}>` is implemented manually instead of
// using `impl<T: Into<u64>> From<T> for Uint64` because
// of the conflict with `TryFrom<&str>` as described here
// https://stackoverflow.com/questions/63136970/how-do-i-work-around-the-upstream-crates-may-add-a-new-impl-of-trait-error

impl From<u64> for Uint64 {
    fn from(val: u64) -> Self {}
}

impl From<u32> for Uint64 {
    fn from(val: u32) -> Self {}
}

impl From<u16> for Uint64 {
    fn from(val: u16) -> Self {}
}

impl From<u8> for Uint64 {
    fn from(val: u8) -> Self {}
}

impl TryFrom<&str> for Uint64 {
    type Error = StdError;

    fn try_from(val: &str) -> Result<Self, Self::Error> {}
}

impl From<Uint64> for String {
    fn from(original: Uint64) -> Self {}
}

impl From<Uint64> for u64 {
    fn from(original: Uint64) -> Self {}
}

impl fmt::Display for Uint64 {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {}
}

impl Add<Uint64> for Uint64 {
    type Output = Self;

    fn add(self, rhs: Self) -> Self {}
}

impl<'a> Add<&'a Uint64> for Uint64 {
    type Output = Self;

    fn add(self, rhs: &'a Uint64) -> Self {}
}

impl Sub<Uint64> for Uint64 {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self {}
}
forward_ref_binop!(impl Sub, sub for Uint64, Uint64);

impl SubAssign<Uint64> for Uint64 {
    fn sub_assign(&mut self, rhs: Uint64) {}
}
forward_ref_op_assign!(impl SubAssign, sub_assign for Uint64, Uint64);

impl Mul<Uint64> for Uint64 {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {}
}
forward_ref_binop!(impl Mul, mul for Uint64, Uint64);

impl MulAssign<Uint64> for Uint64 {
    fn mul_assign(&mut self, rhs: Self) {}
}
forward_ref_op_assign!(impl MulAssign, mul_assign for Uint64, Uint64);

impl Div<Uint64> for Uint64 {
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {}
}

impl<'a> Div<&'a Uint64> for Uint64 {
    type Output = Self;

    fn div(self, rhs: &'a Uint64) -> Self::Output {}
}

impl Rem for Uint64 {
    type Output = Self;

    /// # Panics
    ///
    /// This operation will panic if `rhs` is zero.
    #[inline]
    fn rem(self, rhs: Self) -> Self {}
}
forward_ref_binop!(impl Rem, rem for Uint64, Uint64);

impl RemAssign<Uint64> for Uint64 {
    fn rem_assign(&mut self, rhs: Uint64) {}
}
forward_ref_op_assign!(impl RemAssign, rem_assign for Uint64, Uint64);

impl Shr<u32> for Uint64 {
    type Output = Self;

    fn shr(self, rhs: u32) -> Self::Output {}
}

impl<'a> Shr<&'a u32> for Uint64 {
    type Output = Self;

    fn shr(self, rhs: &'a u32) -> Self::Output {}
}

impl AddAssign<Uint64> for Uint64 {
    fn add_assign(&mut self, rhs: Uint64) {}
}

impl<'a> AddAssign<&'a Uint64> for Uint64 {
    fn add_assign(&mut self, rhs: &'a Uint64) {}
}

impl DivAssign<Uint64> for Uint64 {
    fn div_assign(&mut self, rhs: Self) {}
}

impl<'a> DivAssign<&'a Uint64> for Uint64 {
    fn div_assign(&mut self, rhs: &'a Uint64) {}
}

impl ShrAssign<u32> for Uint64 {
    fn shr_assign(&mut self, rhs: u32) {}
}

impl<'a> ShrAssign<&'a u32> for Uint64 {
    fn shr_assign(&mut self, rhs: &'a u32) {}
}

impl Serialize for Uint64 {
    /// Serializes as an integer string using base 10
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: ser::Serializer,
    {}
}

impl<'de> Deserialize<'de> for Uint64 {
    /// Deserialized from an integer string using base 10
    fn deserialize<D>(deserializer: D) -> Result<Uint64, D::Error>
    where
        D: Deserializer<'de>,
    {}
}

struct Uint64Visitor;

impl<'de> de::Visitor<'de> for Uint64Visitor {
    type Value = Uint64;

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {}

    fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
    where
        E: de::Error,
    {}
}

impl<A> std::iter::Sum<A> for Uint64
where
    Self: Add<A, Output = Self>,
{
    fn sum<I: Iterator<Item = A>>(iter: I) -> Self {}
}

impl PartialEq<&Uint64> for Uint64 {
    fn eq(&self, rhs: &&Uint64) -> bool {}
}

impl PartialEq<Uint64> for &Uint64 {
    fn eq(&self, rhs: &Uint64) -> bool {}
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{from_slice, to_vec};

    #[test]
    fn uint64_zero_works() {}

    #[test]
    fn uint64_one_works() {}

    #[test]
    fn uint64_convert_into() {}

    #[test]
    fn uint64_convert_from() {}

    #[test]
    fn uint64_implements_display() {}

    #[test]
    fn uint64_display_padding_works() {}

    #[test]
    fn uint64_to_be_bytes_works() {}

    #[test]
    fn uint64_to_le_bytes_works() {}

    #[test]
    fn uint64_is_zero_works() {}

    #[test]
    fn uint64_json() {}

    #[test]
    fn uint64_compare() {}

    #[test]
    #[allow(clippy::op_ref)]
    fn uint64_math() {}

    #[test]
    #[allow(clippy::op_ref)]
    fn uint64_sub_works() {}

    #[test]
    #[should_panic]
    fn uint64_sub_overflow_panics() {}

    #[test]
    fn uint64_sub_assign_works() {}

    #[test]
    #[allow(clippy::op_ref)]
    fn uint64_mul_works() {}

    #[test]
    fn uint64_mul_assign_works() {}

    #[test]
    fn uint64_pow_works() {}

    #[test]
    #[should_panic]
    fn uint64_pow_overflow_panics() {}

    #[test]
    #[should_panic]
    fn uint64_math_overflow_panics() {}

    #[test]
    fn uint64_multiply_ratio_works() {}

    #[test]
    fn uint64_multiply_ratio_does_not_overflow_when_result_fits() {}

    #[test]
    #[should_panic]
    fn uint64_multiply_ratio_panicks_on_overflow() {}

    #[test]
    #[should_panic(expected = "Denominator must not be zero")]
    fn uint64_multiply_ratio_panics_for_zero_denominator() {}

    #[test]
    fn uint64_checked_multiply_ratio_does_not_panic() {}

    #[test]
    fn sum_works() {}

    #[test]
    fn uint64_methods() {}

    #[test]
    fn uint64_wrapping_methods() {}

    #[test]
    #[allow(clippy::op_ref)]
    fn uint64_implements_rem() {}

    #[test]
    #[should_panic(expected = "divisor of zero")]
    fn uint64_rem_panics_for_zero() {}

    #[test]
    #[allow(clippy::op_ref)]
    fn uint64_rem_works() {}

    #[test]
    fn uint64_rem_assign_works() {}

    #[test]
    fn uint64_abs_diff_works() {}

    #[test]
    fn uint64_partial_eq() {}
}
}

pub use decimal::{Decimal, DecimalRangeExceeded};
pub use decimal256::{Decimal256, Decimal256RangeExceeded};
pub use fraction::Fraction;
pub use isqrt::Isqrt;
pub use uint128::Uint128;
pub use uint256::Uint256;
pub use uint512::Uint512;
pub use uint64::Uint64;

#[cfg(test)]
mod tests {
    use super::*;
    use std::ops::*;

    /// An trait that ensures other traits are implemented for our number types
    trait AllImpl<'a>:
        Add
        + Add<&'a Self>
        + AddAssign
        + AddAssign<&'a Self>
        + Sub
        + Sub<&'a Self>
        + SubAssign
        + SubAssign<&'a Self>
        + Mul
        + Mul<&'a Self>
        + MulAssign
        + MulAssign<&'a Self>
        + Div
        + Div<&'a Self>
        + DivAssign
        + DivAssign<&'a Self>
        + Rem
        + Rem<&'a Self>
        + RemAssign
        + RemAssign<&'a Self>
        + Sized
        + Copy
    where
        Self: 'a,
    {
    }

    impl AllImpl<'_> for Uint64 {}
    impl AllImpl<'_> for Uint128 {}
    impl AllImpl<'_> for Uint256 {}
    impl AllImpl<'_> for Uint512 {}
    impl AllImpl<'_> for Decimal {}
    impl AllImpl<'_> for Decimal256 {}
}
}
mod panic {
/// Installs a panic handler that aborts the contract execution
/// and sends the panic message and location to the host.
///
/// This overrides any previous panic handler. See <https://doc.rust-lang.org/std/panic/fn.set_hook.html>
/// for details.
#[cfg(all(feature = "abort", target_arch = "wasm32"))]
pub fn install_panic_handler() {}
}
mod query {
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

#[cfg(feature = "stargate")]
use crate::Binary;
use crate::Empty;

mod bank {
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

use crate::Coin;

#[non_exhaustive]
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, JsonSchema)]
#[serde(rename_all = "snake_case")]
pub enum BankQuery {
    /// This calls into the native bank module for querying the total supply of one denomination.
    /// It does the same as the SupplyOf call in Cosmos SDK's RPC API.
    /// Return value is of type SupplyResponse.
    #[cfg(feature = "cosmwasm_1_1")]
    Supply { denom: String },
    /// This calls into the native bank module for one denomination
    /// Return value is BalanceResponse
    Balance { address: String, denom: String },
    /// This calls into the native bank module for all denominations.
    /// Note that this may be much more expensive than Balance and should be avoided if possible.
    /// Return value is AllBalanceResponse.
    AllBalances { address: String },
}

#[cfg(feature = "cosmwasm_1_1")]
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, JsonSchema)]
#[serde(rename_all = "snake_case")]
#[non_exhaustive]
pub struct SupplyResponse {
    /// Always returns a Coin with the requested denom.
    /// This will be of zero amount if the denom does not exist.
    pub amount: Coin,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, JsonSchema)]
#[serde(rename_all = "snake_case")]
pub struct BalanceResponse {
    /// Always returns a Coin with the requested denom.
    /// This may be of 0 amount if no such funds.
    pub amount: Coin,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, JsonSchema)]
#[serde(rename_all = "snake_case")]
pub struct AllBalanceResponse {
    /// Returns all non-zero coins held by this account.
    pub amount: Vec<Coin>,
}
}
mod ibc {
#![cfg(feature = "stargate")]

use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

use crate::ibc::IbcChannel;

/// These are queries to the various IBC modules to see the state of the contract's
/// IBC connection. These will return errors if the contract is not "ibc enabled"
#[non_exhaustive]
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, JsonSchema)]
#[serde(rename_all = "snake_case")]
pub enum IbcQuery {
    /// Gets the Port ID the current contract is bound to.
    ///
    /// Returns a `PortIdResponse`.
    PortId {},
    /// Lists all channels that are bound to a given port.
    /// If `port_id` is omitted, this list all channels bound to the contract's port.
    ///
    /// Returns a `ListChannelsResponse`.
    ListChannels { port_id: Option<String> },
    /// Lists all information for a (portID, channelID) pair.
    /// If port_id is omitted, it will default to the contract's own channel.
    /// (To save a PortId{} call)
    ///
    /// Returns a `ChannelResponse`.
    Channel {
        channel_id: String,
        port_id: Option<String>,
    },
    // TODO: Add more
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, JsonSchema)]
pub struct PortIdResponse {
    pub port_id: String,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, JsonSchema)]
pub struct ListChannelsResponse {
    pub channels: Vec<IbcChannel>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, JsonSchema)]
pub struct ChannelResponse {
    pub channel: Option<IbcChannel>,
}
}
mod staking {
#![cfg(feature = "staking")]

use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

use crate::{Addr, Coin, Decimal};

#[non_exhaustive]
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, JsonSchema)]
#[serde(rename_all = "snake_case")]
pub enum StakingQuery {
    /// Returns the denomination that can be bonded (if there are multiple native tokens on the chain)
    BondedDenom {},
    /// AllDelegations will return all delegations by the delegator
    AllDelegations { delegator: String },
    /// Delegation will return more detailed info on a particular
    /// delegation, defined by delegator/validator pair
    Delegation {
        delegator: String,
        validator: String,
    },
    /// Returns all validators in the currently active validator set.
    ///
    /// The query response type is `AllValidatorsResponse`.
    AllValidators {},
    /// Returns the validator at the given address. Returns None if the validator is
    /// not part of the currently active validator set.
    ///
    /// The query response type is `ValidatorResponse`.
    Validator {
        /// The validator's address (e.g. (e.g. cosmosvaloper1...))
        address: String,
    },
}

/// BondedDenomResponse is data format returned from StakingRequest::BondedDenom query
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, JsonSchema)]
#[serde(rename_all = "snake_case")]
pub struct BondedDenomResponse {
    pub denom: String,
}

/// DelegationsResponse is data format returned from StakingRequest::AllDelegations query
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, JsonSchema)]
#[serde(rename_all = "snake_case")]
pub struct AllDelegationsResponse {
    pub delegations: Vec<Delegation>,
}

/// Delegation is basic (cheap to query) data about a delegation.
///
/// Instances are created in the querier.
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, JsonSchema)]
pub struct Delegation {
    pub delegator: Addr,
    /// A validator address (e.g. cosmosvaloper1...)
    pub validator: String,
    /// How much we have locked in the delegation
    pub amount: Coin,
}

impl From<FullDelegation> for Delegation {
    fn from(full: FullDelegation) -> Self {}
}

/// DelegationResponse is data format returned from StakingRequest::Delegation query
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, JsonSchema)]
#[serde(rename_all = "snake_case")]
pub struct DelegationResponse {
    pub delegation: Option<FullDelegation>,
}

/// FullDelegation is all the info on the delegation, some (like accumulated_reward and can_redelegate)
/// is expensive to query.
///
/// Instances are created in the querier.
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, JsonSchema)]
pub struct FullDelegation {
    pub delegator: Addr,
    /// A validator address (e.g. cosmosvaloper1...)
    pub validator: String,
    /// How much we have locked in the delegation
    pub amount: Coin,
    /// can_redelegate captures how much can be immediately redelegated.
    /// 0 is no redelegation and can_redelegate == amount is redelegate all
    /// but there are many places between the two
    pub can_redelegate: Coin,
    /// How much we can currently withdraw
    pub accumulated_rewards: Vec<Coin>,
}

/// The data format returned from StakingRequest::AllValidators query
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, JsonSchema)]
pub struct AllValidatorsResponse {
    pub validators: Vec<Validator>,
}

/// The data format returned from StakingRequest::Validator query
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, JsonSchema)]
pub struct ValidatorResponse {
    pub validator: Option<Validator>,
}

/// Instances are created in the querier.
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, JsonSchema)]
pub struct Validator {
    /// A validator address (e.g. cosmosvaloper1...)
    pub address: String,
    pub commission: Decimal,
    pub max_commission: Decimal,
    /// TODO: what units are these (in terms of time)?
    pub max_change_rate: Decimal,
}
}
mod wasm {
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

use crate::Binary;

#[non_exhaustive]
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, JsonSchema)]
#[serde(rename_all = "snake_case")]
pub enum WasmQuery {
    /// this queries the public API of another contract at a known address (with known ABI)
    /// Return value is whatever the contract returns (caller should know), wrapped in a
    /// ContractResult that is JSON encoded.
    Smart {
        contract_addr: String,
        /// msg is the json-encoded QueryMsg struct
        msg: Binary,
    },
    /// this queries the raw kv-store of the contract.
    /// returns the raw, unparsed data stored at that key, which may be an empty vector if not present
    Raw {
        contract_addr: String,
        /// Key is the raw key used in the contracts Storage
        key: Binary,
    },
    /// returns a ContractInfoResponse with metadata on the contract from the runtime
    ContractInfo { contract_addr: String },
}

#[non_exhaustive]
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, JsonSchema)]
pub struct ContractInfoResponse {
    pub code_id: u64,
    /// address that instantiated this contract
    pub creator: String,
    /// admin who can run migrations (if any)
    pub admin: Option<String>,
    /// if set, the contract is pinned to the cache, and thus uses less gas when called
    pub pinned: bool,
    /// set if this contract has bound an IBC port
    pub ibc_port: Option<String>,
}

impl ContractInfoResponse {
    /// Convenience constructor for tests / mocks
    #[doc(hidden)]
    pub fn new(code_id: u64, creator: impl Into<String>) -> Self {}
}
}

#[cfg(feature = "cosmwasm_1_1")]
pub use bank::SupplyResponse;
pub use bank::{AllBalanceResponse, BalanceResponse, BankQuery};
#[cfg(feature = "stargate")]
pub use ibc::{ChannelResponse, IbcQuery, ListChannelsResponse, PortIdResponse};
#[cfg(feature = "staking")]
pub use staking::{
    AllDelegationsResponse, AllValidatorsResponse, BondedDenomResponse, Delegation,
    DelegationResponse, FullDelegation, StakingQuery, Validator, ValidatorResponse,
};
pub use wasm::{ContractInfoResponse, WasmQuery};

#[non_exhaustive]
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, JsonSchema)]
#[serde(rename_all = "snake_case")]
pub enum QueryRequest<C> {
    Bank(BankQuery),
    Custom(C),
    #[cfg(feature = "staking")]
    Staking(StakingQuery),
    /// A Stargate query is encoded the same way as abci_query, with path and protobuf encoded request data.
    /// The format is defined in [ADR-21](https://github.com/cosmos/cosmos-sdk/blob/master/docs/architecture/adr-021-protobuf-query-encoding.md).
    /// The response is protobuf encoded data directly without a JSON response wrapper.
    /// The caller is responsible for compiling the proper protobuf definitions for both requests and responses.
    #[cfg(feature = "stargate")]
    Stargate {
        /// this is the fully qualified service path used for routing,
        /// eg. custom/cosmos_sdk.x.bank.v1.Query/QueryBalance
        path: String,
        /// this is the expected protobuf message type (not any), binary encoded
        data: Binary,
    },
    #[cfg(feature = "stargate")]
    Ibc(IbcQuery),
    Wasm(WasmQuery),
}

/// A trait that is required to avoid conflicts with other query types like BankQuery and WasmQuery
/// in generic implementations.
/// You need to implement it in your custom query type.
///
/// # Examples
///
/// ```
/// # use cosmwasm_std::CustomQuery;
/// # use schemars::JsonSchema;
/// # use serde::{Deserialize, Serialize};
/// #[derive(Serialize, Deserialize, Clone, Debug, PartialEq, JsonSchema)]
/// #[serde(rename_all = "snake_case")]
/// pub enum MyCustomQuery {
///     Ping {},
///     Capitalized { text: String },
/// }
///
/// impl CustomQuery for MyCustomQuery {}
/// ```
pub trait CustomQuery: Serialize + Clone {}
// We require `Clone` because `Clone` in `QueryRequest<C>` is only derived for
// `C: Clone` and we want consistent behaviour for all `QueryRequest<C>`

impl CustomQuery for Empty {}

impl<C: CustomQuery> From<BankQuery> for QueryRequest<C> {
    fn from(msg: BankQuery) -> Self {}
}

impl<C: CustomQuery> From<C> for QueryRequest<C> {
    fn from(msg: C) -> Self {}
}

#[cfg(feature = "staking")]
impl<C: CustomQuery> From<StakingQuery> for QueryRequest<C> {
    fn from(msg: StakingQuery) -> Self {}
}

impl<C: CustomQuery> From<WasmQuery> for QueryRequest<C> {
    fn from(msg: WasmQuery) -> Self {}
}

#[cfg(feature = "stargate")]
impl<C: CustomQuery> From<IbcQuery> for QueryRequest<C> {
    fn from(msg: IbcQuery) -> Self {}
}
}
mod results {
//! This module contains the messages that are sent from the contract to the VM as an execution result

mod contract_result {
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use std::fmt;

/// This is the final result type that is created and serialized in a contract for
/// every init/execute/migrate call. The VM then deserializes this type to distinguish
/// between successful and failed executions.
///
/// We use a custom type here instead of Rust's Result because we want to be able to
/// define the serialization, which is a public interface. Every language that compiles
/// to Wasm and runs in the ComsWasm VM needs to create the same JSON representation.
///
/// # Examples
///
/// Success:
///
/// ```
/// # use cosmwasm_std::{to_vec, ContractResult, Response};
/// let response: Response = Response::default();
/// let result: ContractResult<Response> = ContractResult::Ok(response);
/// assert_eq!(to_vec(&result).unwrap(), br#"{"ok":{"messages":[],"attributes":[],"events":[],"data":null}}"#);
/// ```
///
/// Failure:
///
/// ```
/// # use cosmwasm_std::{to_vec, ContractResult, Response};
/// let error_msg = String::from("Something went wrong");
/// let result: ContractResult<Response> = ContractResult::Err(error_msg);
/// assert_eq!(to_vec(&result).unwrap(), br#"{"error":"Something went wrong"}"#);
/// ```
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, JsonSchema)]
#[serde(rename_all = "snake_case")]
pub enum ContractResult<S> {
    Ok(S),
    /// An error type that every custom error created by contract developers can be converted to.
    /// This could potientially have more structure, but String is the easiest.
    #[serde(rename = "error")]
    Err(String),
}

// Implementations here mimic the Result API and should be implemented via a conversion to Result
// to ensure API consistency
impl<S> ContractResult<S> {
    /// Converts a `ContractResult<S>` to a `Result<S, String>` as a convenient way
    /// to access the full Result API.
    pub fn into_result(self) -> Result<S, String> {}

    pub fn unwrap(self) -> S {}

    pub fn is_ok(&self) -> bool {}

    pub fn is_err(&self) -> bool {}
}

impl<S: fmt::Debug> ContractResult<S> {
    pub fn unwrap_err(self) -> String {}
}

impl<S, E: ToString> From<Result<S, E>> for ContractResult<S> {
    fn from(original: Result<S, E>) -> ContractResult<S> {}
}

impl<S> From<ContractResult<S>> for Result<S, String> {
    fn from(original: ContractResult<S>) -> Result<S, String> {}
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{from_slice, to_vec, Response, StdError, StdResult};

    #[test]
    fn contract_result_serialization_works() {}

    #[test]
    fn contract_result_deserialization_works() {}

    #[test]
    fn can_convert_from_core_result() {}

    #[test]
    fn can_convert_to_core_result() {}
}
}
mod cosmos_msg {
use derivative::Derivative;
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use std::fmt;

use crate::binary::Binary;
use crate::coin::Coin;
use crate::errors::StdResult;
#[cfg(feature = "stargate")]
use crate::ibc::IbcMsg;
use crate::serde::to_binary;
#[cfg(all(feature = "stargate", feature = "cosmwasm_1_2"))]
use crate::Decimal;

use super::Empty;

/// Like CustomQuery for better type clarity.
/// Also makes it shorter to use as a trait bound.
pub trait CustomMsg: Serialize + Clone + fmt::Debug + PartialEq + JsonSchema {}

impl CustomMsg for Empty {}

#[non_exhaustive]
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, JsonSchema)]
#[serde(rename_all = "snake_case")]
// See https://github.com/serde-rs/serde/issues/1296 why we cannot add De-Serialize trait bounds to T
pub enum CosmosMsg<T = Empty> {
    Bank(BankMsg),
    // by default we use RawMsg, but a contract can override that
    // to call into more app-specific code (whatever they define)
    Custom(T),
    #[cfg(feature = "staking")]
    Staking(StakingMsg),
    #[cfg(feature = "staking")]
    Distribution(DistributionMsg),
    /// A Stargate message encoded the same way as a protobuf [Any](https://github.com/protocolbuffers/protobuf/blob/master/src/google/protobuf/any.proto).
    /// This is the same structure as messages in `TxBody` from [ADR-020](https://github.com/cosmos/cosmos-sdk/blob/master/docs/architecture/adr-020-protobuf-transaction-encoding.md)
    #[cfg(feature = "stargate")]
    Stargate {
        type_url: String,
        value: Binary,
    },
    #[cfg(feature = "stargate")]
    Ibc(IbcMsg),
    Wasm(WasmMsg),
    #[cfg(feature = "stargate")]
    Gov(GovMsg),
}

/// The message types of the bank module.
///
/// See https://github.com/cosmos/cosmos-sdk/blob/v0.40.0/proto/cosmos/bank/v1beta1/tx.proto
#[non_exhaustive]
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, JsonSchema)]
#[serde(rename_all = "snake_case")]
pub enum BankMsg {
    /// Sends native tokens from the contract to the given address.
    ///
    /// This is translated to a [MsgSend](https://github.com/cosmos/cosmos-sdk/blob/v0.40.0/proto/cosmos/bank/v1beta1/tx.proto#L19-L28).
    /// `from_address` is automatically filled with the current contract's address.
    Send {
        to_address: String,
        amount: Vec<Coin>,
    },
    /// This will burn the given coins from the contract's account.
    /// There is no Cosmos SDK message that performs this, but it can be done by calling the bank keeper.
    /// Important if a contract controls significant token supply that must be retired.
    Burn { amount: Vec<Coin> },
}

/// The message types of the staking module.
///
/// See https://github.com/cosmos/cosmos-sdk/blob/v0.40.0/proto/cosmos/staking/v1beta1/tx.proto
#[cfg(feature = "staking")]
#[non_exhaustive]
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, JsonSchema)]
#[serde(rename_all = "snake_case")]
pub enum StakingMsg {
    /// This is translated to a [MsgDelegate](https://github.com/cosmos/cosmos-sdk/blob/v0.40.0/proto/cosmos/staking/v1beta1/tx.proto#L81-L90).
    /// `delegator_address` is automatically filled with the current contract's address.
    Delegate { validator: String, amount: Coin },
    /// This is translated to a [MsgUndelegate](https://github.com/cosmos/cosmos-sdk/blob/v0.40.0/proto/cosmos/staking/v1beta1/tx.proto#L112-L121).
    /// `delegator_address` is automatically filled with the current contract's address.
    Undelegate { validator: String, amount: Coin },
    /// This is translated to a [MsgBeginRedelegate](https://github.com/cosmos/cosmos-sdk/blob/v0.40.0/proto/cosmos/staking/v1beta1/tx.proto#L95-L105).
    /// `delegator_address` is automatically filled with the current contract's address.
    Redelegate {
        src_validator: String,
        dst_validator: String,
        amount: Coin,
    },
}

/// The message types of the distribution module.
///
/// See https://github.com/cosmos/cosmos-sdk/blob/v0.42.4/proto/cosmos/distribution/v1beta1/tx.proto
#[cfg(feature = "staking")]
#[non_exhaustive]
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, JsonSchema)]
#[serde(rename_all = "snake_case")]
pub enum DistributionMsg {
    /// This is translated to a [MsgSetWithdrawAddress](https://github.com/cosmos/cosmos-sdk/blob/v0.42.4/proto/cosmos/distribution/v1beta1/tx.proto#L29-L37).
    /// `delegator_address` is automatically filled with the current contract's address.
    SetWithdrawAddress {
        /// The `withdraw_address`
        address: String,
    },
    /// This is translated to a [[MsgWithdrawDelegatorReward](https://github.com/cosmos/cosmos-sdk/blob/v0.42.4/proto/cosmos/distribution/v1beta1/tx.proto#L42-L50).
    /// `delegator_address` is automatically filled with the current contract's address.
    WithdrawDelegatorReward {
        /// The `validator_address`
        validator: String,
    },
}

fn binary_to_string(data: &Binary, fmt: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {}

/// The message types of the wasm module.
///
/// See https://github.com/CosmWasm/wasmd/blob/v0.14.0/x/wasm/internal/types/tx.proto
#[non_exhaustive]
#[derive(Serialize, Deserialize, Clone, Derivative, PartialEq, Eq, JsonSchema)]
#[derivative(Debug)]
#[serde(rename_all = "snake_case")]
pub enum WasmMsg {
    /// Dispatches a call to another contract at a known address (with known ABI).
    ///
    /// This is translated to a [MsgExecuteContract](https://github.com/CosmWasm/wasmd/blob/v0.14.0/x/wasm/internal/types/tx.proto#L68-L78).
    /// `sender` is automatically filled with the current contract's address.
    Execute {
        contract_addr: String,
        /// msg is the json-encoded ExecuteMsg struct (as raw Binary)
        #[derivative(Debug(format_with = "binary_to_string"))]
        msg: Binary,
        funds: Vec<Coin>,
    },
    /// Instantiates a new contracts from previously uploaded Wasm code.
    ///
    /// This is translated to a [MsgInstantiateContract](https://github.com/CosmWasm/wasmd/blob/v0.16.0-alpha1/x/wasm/internal/types/tx.proto#L47-L61).
    /// `sender` is automatically filled with the current contract's address.
    Instantiate {
        admin: Option<String>,
        code_id: u64,
        /// msg is the JSON-encoded InstantiateMsg struct (as raw Binary)
        #[derivative(Debug(format_with = "binary_to_string"))]
        msg: Binary,
        funds: Vec<Coin>,
        /// A human-readbale label for the contract
        label: String,
    },
    /// Migrates a given contracts to use new wasm code. Passes a MigrateMsg to allow us to
    /// customize behavior.
    ///
    /// Only the contract admin (as defined in wasmd), if any, is able to make this call.
    ///
    /// This is translated to a [MsgMigrateContract](https://github.com/CosmWasm/wasmd/blob/v0.14.0/x/wasm/internal/types/tx.proto#L86-L96).
    /// `sender` is automatically filled with the current contract's address.
    Migrate {
        contract_addr: String,
        /// the code_id of the new logic to place in the given contract
        new_code_id: u64,
        /// msg is the json-encoded MigrateMsg struct that will be passed to the new code
        #[derivative(Debug(format_with = "binary_to_string"))]
        msg: Binary,
    },
    /// Sets a new admin (for migrate) on the given contract.
    /// Fails if this contract is not currently admin of the target contract.
    UpdateAdmin {
        contract_addr: String,
        admin: String,
    },
    /// Clears the admin on the given contract, so no more migration possible.
    /// Fails if this contract is not currently admin of the target contract.
    ClearAdmin { contract_addr: String },
}

#[cfg(feature = "stargate")]
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, JsonSchema)]
#[serde(rename_all = "snake_case")]
pub enum GovMsg {
    /// This maps directly to [MsgVote](https://github.com/cosmos/cosmos-sdk/blob/v0.42.5/proto/cosmos/gov/v1beta1/tx.proto#L46-L56) in the Cosmos SDK with voter set to the contract address.
    Vote { proposal_id: u64, vote: VoteOption },
    /// This maps directly to [MsgVoteWeighted](https://github.com/cosmos/cosmos-sdk/blob/v0.45.8/proto/cosmos/gov/v1beta1/tx.proto#L66-L78) in the Cosmos SDK with voter set to the contract address.
    #[cfg(feature = "cosmwasm_1_2")]
    VoteWeighted {
        proposal_id: u64,
        vote: WeightedVoteOption,
    },
}

#[cfg(feature = "stargate")]
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, JsonSchema)]
#[serde(rename_all = "snake_case")]
pub enum VoteOption {
    Yes,
    No,
    Abstain,
    NoWithVeto,
}

#[cfg(all(feature = "stargate", feature = "cosmwasm_1_2"))]
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, JsonSchema)]
pub struct WeightedVoteOption {
    option: VoteOption,
    weight: Decimal,
}

/// Shortcut helper as the construction of WasmMsg::Instantiate can be quite verbose in contract code.
///
/// When using this, `admin` is always unset. If you need more flexibility, create the message directly.
pub fn wasm_instantiate(
    code_id: u64,
    msg: &impl Serialize,
    funds: Vec<Coin>,
    label: String,
) -> StdResult<WasmMsg> {}

/// Shortcut helper as the construction of WasmMsg::Instantiate can be quite verbose in contract code
pub fn wasm_execute(
    contract_addr: impl Into<String>,
    msg: &impl Serialize,
    funds: Vec<Coin>,
) -> StdResult<WasmMsg> {}

impl<T> From<BankMsg> for CosmosMsg<T> {
    fn from(msg: BankMsg) -> Self {}
}

#[cfg(feature = "staking")]
impl<T> From<StakingMsg> for CosmosMsg<T> {
    fn from(msg: StakingMsg) -> Self {}
}

#[cfg(feature = "staking")]
impl<T> From<DistributionMsg> for CosmosMsg<T> {
    fn from(msg: DistributionMsg) -> Self {}
}

impl<T> From<WasmMsg> for CosmosMsg<T> {
    fn from(msg: WasmMsg) -> Self {}
}

#[cfg(feature = "stargate")]
impl<T> From<IbcMsg> for CosmosMsg<T> {
    fn from(msg: IbcMsg) -> Self {}
}

#[cfg(feature = "stargate")]
impl<T> From<GovMsg> for CosmosMsg<T> {
    fn from(msg: GovMsg) -> Self {}
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{coin, coins};

    #[test]
    fn from_bank_msg_works() {}

    #[cosmwasm_schema::cw_serde]
    enum ExecuteMsg {
        Mint { coin: Coin },
    }

    #[test]
    fn wasm_msg_debug_decodes_binary_string_when_possible() {}

    #[test]
    fn wasm_msg_debug_dumps_binary_when_not_utf8() {}
}
}
mod empty {
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

/// An empty struct that serves as a placeholder in different places,
/// such as contracts that don't set a custom message.
///
/// It is designed to be expressable in correct JSON and JSON Schema but
/// contains no meaningful data. Previously we used enums without cases,
/// but those cannot represented as valid JSON Schema (https://github.com/CosmWasm/cosmwasm/issues/451)
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, JsonSchema, Default)]
pub struct Empty {}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::serde::{from_slice, to_vec};

    #[test]
    fn empty_can_be_instantiated() {}

    #[test]
    fn empty_can_be_instantiated_serialized_and_deserialized() {}
}
}
mod events {
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

/// A full [*Cosmos SDK* event].
///
/// This version uses string attributes (similar to [*Cosmos SDK* StringEvent]),
/// which then get magically converted to bytes for Tendermint somewhere between
/// the Rust-Go interface, JSON deserialization and the `NewEvent` call in Cosmos SDK.
///
/// [*Cosmos SDK* event]: https://docs.cosmos.network/main/core/events.html
/// [*Cosmos SDK* StringEvent]: https://github.com/cosmos/cosmos-sdk/blob/v0.42.5/proto/cosmos/base/abci/v1beta1/abci.proto#L56-L70
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, JsonSchema)]
#[non_exhaustive]
pub struct Event {
    /// The event type. This is renamed to "ty" because "type" is reserved in Rust. This sucks, we know.
    #[serde(rename = "type")]
    pub ty: String,
    /// The attributes to be included in the event.
    ///
    /// You can learn more about these from [*Cosmos SDK* docs].
    ///
    /// [*Cosmos SDK* docs]: https://docs.cosmos.network/main/core/events.html
    pub attributes: Vec<Attribute>,
}

impl Event {
    /// Create a new event with the given type and an empty list of attributes.
    pub fn new(ty: impl Into<String>) -> Self {}

    /// Add an attribute to the event.
    pub fn add_attribute(mut self, key: impl Into<String>, value: impl Into<String>) -> Self {}

    /// Bulk add attributes to the event.
    ///
    /// Anything that can be turned into an iterator and yields something
    /// that can be converted into an `Attribute` is accepted.
    pub fn add_attributes<A: Into<Attribute>>(
        mut self,
        attrs: impl IntoIterator<Item = A>,
    ) -> Self {}
}

/// An key value pair that is used in the context of event attributes in logs
#[derive(Serialize, Deserialize, Clone, Default, Debug, PartialEq, Eq, JsonSchema)]
pub struct Attribute {
    pub key: String,
    pub value: String,
}

impl Attribute {
    /// Creates a new Attribute. `attr` is just an alias for this.
    pub fn new(key: impl Into<String>, value: impl Into<String>) -> Self {}
}

impl<K: Into<String>, V: Into<String>> From<(K, V)> for Attribute {
    fn from((k, v): (K, V)) -> Self {}
}

impl<K: AsRef<str>, V: AsRef<str>> PartialEq<(K, V)> for Attribute {
    fn eq(&self, (k, v): &(K, V)) -> bool {}
}

impl<K: AsRef<str>, V: AsRef<str>> PartialEq<Attribute> for (K, V) {
    fn eq(&self, attr: &Attribute) -> bool {}
}

impl<K: AsRef<str>, V: AsRef<str>> PartialEq<(K, V)> for &Attribute {
    fn eq(&self, (k, v): &(K, V)) -> bool {}
}

impl<K: AsRef<str>, V: AsRef<str>> PartialEq<&Attribute> for (K, V) {
    fn eq(&self, attr: &&Attribute) -> bool {}
}

impl PartialEq<Attribute> for &Attribute {
    fn eq(&self, rhs: &Attribute) -> bool {}
}

impl PartialEq<&Attribute> for Attribute {
    fn eq(&self, rhs: &&Attribute) -> bool {}
}

/// Creates a new Attribute. `Attribute::new` is an alias for this.
#[inline]
pub fn attr(key: impl Into<String>, value: impl Into<String>) -> Attribute {}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Uint128;

    #[test]
    fn event_construction() {}

    #[test]
    #[should_panic]
    fn attribute_new_reserved_key_panicks() {}

    #[test]
    #[should_panic]
    fn attribute_new_reserved_key_panicks2() {}

    #[test]
    fn attr_works_for_different_types() {}
}
}
mod query {
use crate::binary::Binary;

pub type QueryResponse = Binary;
}
mod response {
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

use crate::Binary;

use super::{Attribute, CosmosMsg, Empty, Event, SubMsg};

/// A response of a contract entry point, such as `instantiate`, `execute` or `migrate`.
///
/// This type can be constructed directly at the end of the call. Alternatively a
/// mutable response instance can be created early in the contract's logic and
/// incrementally be updated.
///
/// ## Examples
///
/// Direct:
///
/// ```
/// # use cosmwasm_std::{Binary, DepsMut, Env, MessageInfo};
/// # type InstantiateMsg = ();
/// #
/// use cosmwasm_std::{attr, Response, StdResult};
///
/// pub fn instantiate(
///     deps: DepsMut,
///     _env: Env,
///     _info: MessageInfo,
///     msg: InstantiateMsg,
/// ) -> StdResult<Response> {}
/// ```
///
/// Mutating:
///
/// ```
/// # use cosmwasm_std::{coins, BankMsg, Binary, DepsMut, Env, MessageInfo, SubMsg};
/// # type InstantiateMsg = ();
/// # type MyError = ();
/// #
/// use cosmwasm_std::Response;
///
/// pub fn instantiate(
///     deps: DepsMut,
///     _env: Env,
///     info: MessageInfo,
///     msg: InstantiateMsg,
/// ) -> Result<Response, MyError> {}
/// ```
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, JsonSchema)]
#[non_exhaustive]
pub struct Response<T = Empty> {
    /// Optional list of messages to pass. These will be executed in order.
    /// If the ReplyOn variant matches the result (Always, Success on Ok, Error on Err),
    /// the runtime will invoke this contract's `reply` entry point
    /// after execution. Otherwise, they act like "fire and forget".
    /// Use `SubMsg::new` to create messages with the older "fire and forget" semantics.
    pub messages: Vec<SubMsg<T>>,
    /// The attributes that will be emitted as part of a "wasm" event.
    ///
    /// More info about events (and their attributes) can be found in [*Cosmos SDK* docs].
    ///
    /// [*Cosmos SDK* docs]: https://docs.cosmos.network/main/core/events.html
    pub attributes: Vec<Attribute>,
    /// Extra, custom events separate from the main `wasm` one. These will have
    /// `wasm-` prepended to the type.
    ///
    /// More info about events can be found in [*Cosmos SDK* docs].
    ///
    /// [*Cosmos SDK* docs]: https://docs.cosmos.network/main/core/events.html
    pub events: Vec<Event>,
    /// The binary payload to include in the response.
    pub data: Option<Binary>,
}

impl<T> Default for Response<T> {
    fn default() -> Self {}
}

impl<T> Response<T> {
    pub fn new() -> Self {}

    /// Add an attribute included in the main `wasm` event.
    ///
    /// For working with optional values or optional attributes, see [`add_attributes`][Self::add_attributes].
    pub fn add_attribute(mut self, key: impl Into<String>, value: impl Into<String>) -> Self {}

    /// This creates a "fire and forget" message, by using `SubMsg::new()` to wrap it,
    /// and adds it to the list of messages to process.
    pub fn add_message(mut self, msg: impl Into<CosmosMsg<T>>) -> Self {}

    /// This takes an explicit SubMsg (creates via eg. `reply_on_error`)
    /// and adds it to the list of messages to process.
    pub fn add_submessage(mut self, msg: SubMsg<T>) -> Self {}

    /// Adds an extra event to the response, separate from the main `wasm` event
    /// that is always created.
    ///
    /// The `wasm-` prefix will be appended by the runtime to the provided type
    /// of event.
    pub fn add_event(mut self, event: Event) -> Self {}

    /// Bulk add attributes included in the main `wasm` event.
    ///
    /// Anything that can be turned into an iterator and yields something
    /// that can be converted into an `Attribute` is accepted.
    ///
    /// ## Examples
    ///
    /// Adding a list of attributes using the pair notation for key and value:
    ///
    /// ```
    /// use cosmwasm_std::Response;
    ///
    /// let attrs = vec![
    ///     ("action", "reaction"),
    ///     ("answer", "42"),
    ///     ("another", "attribute"),
    /// ];
    /// let res: Response = Response::new().add_attributes(attrs.clone());
    /// assert_eq!(res.attributes, attrs);
    /// ```
    ///
    /// Adding an optional value as an optional attribute by turning it into a list of 0 or 1 elements:
    ///
    /// ```
    /// use cosmwasm_std::{Attribute, Response};
    ///
    /// // Some value
    /// let value: Option<String> = Some("sarah".to_string());
    /// let attribute: Option<Attribute> = value.map(|v| Attribute::new("winner", v));
    /// let res: Response = Response::new().add_attributes(attribute);
    /// assert_eq!(res.attributes, [Attribute {
    ///     key: "winner".to_string(),
    ///     value: "sarah".to_string(),
    /// }]);
    ///
    /// // No value
    /// let value: Option<String> = None;
    /// let attribute: Option<Attribute> = value.map(|v| Attribute::new("winner", v));
    /// let res: Response = Response::new().add_attributes(attribute);
    /// assert_eq!(res.attributes.len(), 0);
    /// ```
    pub fn add_attributes<A: Into<Attribute>>(
        mut self,
        attrs: impl IntoIterator<Item = A>,
    ) -> Self {}

    /// Bulk add "fire and forget" messages to the list of messages to process.
    ///
    /// ## Examples
    ///
    /// ```
    /// use cosmwasm_std::{CosmosMsg, Response};
    ///
    /// fn make_response_with_msgs(msgs: Vec<CosmosMsg>) -> Response {}
    /// ```
    pub fn add_messages<M: Into<CosmosMsg<T>>>(self, msgs: impl IntoIterator<Item = M>) -> Self {}

    /// Bulk add explicit SubMsg structs to the list of messages to process.
    ///
    /// ## Examples
    ///
    /// ```
    /// use cosmwasm_std::{SubMsg, Response};
    ///
    /// fn make_response_with_submsgs(msgs: Vec<SubMsg>) -> Response {}
    /// ```
    pub fn add_submessages(mut self, msgs: impl IntoIterator<Item = SubMsg<T>>) -> Self {}

    /// Bulk add custom events to the response. These are separate from the main
    /// `wasm` event.
    ///
    /// The `wasm-` prefix will be appended by the runtime to the provided types
    /// of events.
    pub fn add_events(mut self, events: impl IntoIterator<Item = Event>) -> Self {}

    /// Set the binary data included in the response.
    pub fn set_data(mut self, data: impl Into<Binary>) -> Self {}
}

#[cfg(test)]
mod tests {
    use super::super::BankMsg;
    use super::*;
    use crate::results::submessages::{ReplyOn, UNUSED_MSG_ID};
    use crate::{coins, from_slice, to_vec, ContractResult};

    #[test]
    fn response_add_attributes_works() {}

    #[test]
    fn can_serialize_and_deserialize_init_response() {}

    #[test]
    fn contract_result_is_ok_works() {}

    #[test]
    fn contract_result_is_err_works() {}
}
}
mod submessages {
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

use crate::Binary;

use super::{CosmosMsg, Empty, Event};

/// Use this to define when the contract gets a response callback.
/// If you only need it for errors or success you can select just those in order
/// to save gas.
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, JsonSchema)]
#[serde(rename_all = "snake_case")]
pub enum ReplyOn {
    /// Always perform a callback after SubMsg is processed
    Always,
    /// Only callback if SubMsg returned an error, no callback on success case
    Error,
    /// Only callback if SubMsg was successful, no callback on error case
    Success,
    /// Never make a callback - this is like the original CosmosMsg semantics
    Never,
}

/// A submessage that will guarantee a `reply` call on success or error, depending on
/// the `reply_on` setting. If you do not need to process the result, use regular messages instead.
///
/// Note: On error the submessage execution will revert any partial state changes due to this message,
/// but not revert any state changes in the calling contract. If this is required, it must be done
/// manually in the `reply` entry point.
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, JsonSchema)]
pub struct SubMsg<T = Empty> {
    /// An arbitrary ID chosen by the contract.
    /// This is typically used to match `Reply`s in the `reply` entry point to the submessage.
    pub id: u64,
    pub msg: CosmosMsg<T>,
    /// Gas limit measured in [Cosmos SDK gas](https://github.com/CosmWasm/cosmwasm/blob/main/docs/GAS.md).
    pub gas_limit: Option<u64>,
    pub reply_on: ReplyOn,
}

/// This is used for cases when we use ReplyOn::Never and the id doesn't matter
pub const UNUSED_MSG_ID: u64 = 0;

impl<T> SubMsg<T> {
    /// new creates a "fire and forget" message with the pre-0.14 semantics
    pub fn new(msg: impl Into<CosmosMsg<T>>) -> Self {}

    /// create a `SubMsg` that will provide a `reply` with the given id if the message returns `Ok`
    pub fn reply_on_success(msg: impl Into<CosmosMsg<T>>, id: u64) -> Self {}

    /// create a `SubMsg` that will provide a `reply` with the given id if the message returns `Err`
    pub fn reply_on_error(msg: impl Into<CosmosMsg<T>>, id: u64) -> Self {}

    /// create a `SubMsg` that will always provide a `reply` with the given id
    pub fn reply_always(msg: impl Into<CosmosMsg<T>>, id: u64) -> Self {}

    /// Add a gas limit to the message.
    /// This gas limit measured in [Cosmos SDK gas](https://github.com/CosmWasm/cosmwasm/blob/main/docs/GAS.md).
    ///
    /// ## Examples
    ///
    /// ```
    /// # use cosmwasm_std::{coins, BankMsg, ReplyOn, SubMsg};
    /// # let msg = BankMsg::Send { to_address: String::from("you"), amount: coins(1015, "earth") };
    /// let sub_msg: SubMsg = SubMsg::reply_always(msg, 1234).with_gas_limit(60_000);
    /// assert_eq!(sub_msg.id, 1234);
    /// assert_eq!(sub_msg.gas_limit, Some(60_000));
    /// assert_eq!(sub_msg.reply_on, ReplyOn::Always);
    /// ```
    pub fn with_gas_limit(mut self, limit: u64) -> Self {}

    fn reply_on(msg: CosmosMsg<T>, id: u64, reply_on: ReplyOn) -> Self {}
}

/// The result object returned to `reply`. We always get the ID from the submessage
/// back and then must handle success and error cases ourselves.
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, JsonSchema)]
pub struct Reply {
    /// The ID that the contract set when emitting the `SubMsg`.
    /// Use this to identify which submessage triggered the `reply`.
    pub id: u64,
    pub result: SubMsgResult,
}

/// This is the result type that is returned from a sub message execution.
///
/// We use a custom type here instead of Rust's Result because we want to be able to
/// define the serialization, which is a public interface. Every language that compiles
/// to Wasm and runs in the ComsWasm VM needs to create the same JSON representation.
///
/// Until version 1.0.0-beta5, `ContractResult<SubMsgResponse>` was used instead
/// of this type. Once serialized, the two types are the same. However, in the Rust type
/// system we want different types for clarity and documenation reasons.
///
/// # Examples
///
/// Success:
///
/// ```
/// # use cosmwasm_std::{to_vec, Binary, Event, SubMsgResponse, SubMsgResult};
/// let response = SubMsgResponse {
///     data: Some(Binary::from_base64("MTIzCg==").unwrap()),
///     events: vec![Event::new("wasm").add_attribute("fo", "ba")],
/// };
/// let result: SubMsgResult = SubMsgResult::Ok(response);
/// assert_eq!(to_vec(&result).unwrap(), br#"{"ok":{"events":[{"type":"wasm","attributes":[{"key":"fo","value":"ba"}]}],"data":"MTIzCg=="}}"#);
/// ```
///
/// Failure:
///
/// ```
/// # use cosmwasm_std::{to_vec, SubMsgResult, Response};
/// let error_msg = String::from("Something went wrong");
/// let result = SubMsgResult::Err(error_msg);
/// assert_eq!(to_vec(&result).unwrap(), br#"{"error":"Something went wrong"}"#);
/// ```
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, JsonSchema)]
#[serde(rename_all = "snake_case")]
pub enum SubMsgResult {
    Ok(SubMsgResponse),
    /// An error type that every custom error created by contract developers can be converted to.
    /// This could potientially have more structure, but String is the easiest.
    #[serde(rename = "error")]
    Err(String),
}

// Implementations here mimic the Result API and should be implemented via a conversion to Result
// to ensure API consistency
impl SubMsgResult {
    /// Converts a `SubMsgResult<S>` to a `Result<S, String>` as a convenient way
    /// to access the full Result API.
    pub fn into_result(self) -> Result<SubMsgResponse, String> {}

    pub fn unwrap(self) -> SubMsgResponse {}

    pub fn unwrap_err(self) -> String {}

    pub fn is_ok(&self) -> bool {}

    pub fn is_err(&self) -> bool {}
}

impl<E: ToString> From<Result<SubMsgResponse, E>> for SubMsgResult {
    fn from(original: Result<SubMsgResponse, E>) -> SubMsgResult {}
}

impl From<SubMsgResult> for Result<SubMsgResponse, String> {
    fn from(original: SubMsgResult) -> Result<SubMsgResponse, String> {}
}

/// The information we get back from a successful sub message execution,
/// with full Cosmos SDK events.
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, JsonSchema)]
pub struct SubMsgResponse {
    pub events: Vec<Event>,
    pub data: Option<Binary>,
}

#[deprecated(note = "Renamed to SubMsgResponse")]
pub type SubMsgExecutionResponse = SubMsgResponse;

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{from_slice, to_vec, StdError, StdResult};

    #[test]
    fn sub_msg_result_serialization_works() {}

    #[test]
    fn sub_msg_result_deserialization_works() {}

    #[test]
    fn sub_msg_result_unwrap_works() {}

    #[test]
    #[should_panic]
    fn sub_msg_result_unwrap_panicks_for_err() {}

    #[test]
    fn sub_msg_result_unwrap_err_works() {}

    #[test]
    #[should_panic]
    fn sub_msg_result_unwrap_err_panics_for_ok() {}

    #[test]
    fn sub_msg_result_is_ok_works() {}

    #[test]
    fn sub_msg_result_is_err_works() {}

    #[test]
    fn sub_msg_result_can_convert_from_core_result() {}

    #[test]
    fn sub_msg_result_can_convert_to_core_result() {}
}
}
mod system_result {
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use std::fmt;

use super::super::errors::SystemError;

/// This is the outer result type returned by a querier to the contract.
///
/// We use a custom type here instead of Rust's Result because we want to be able to
/// define the serialization, which is a public interface. Every language that compiles
/// to Wasm and runs in the ComsWasm VM needs to create the same JSON representation.
///
/// # Examples
///
/// Success:
///
/// ```
/// # use cosmwasm_std::{to_vec, Binary, ContractResult, SystemResult};
/// let data = Binary::from(b"hello, world");
/// let result = SystemResult::Ok(ContractResult::Ok(data));
/// assert_eq!(to_vec(&result).unwrap(), br#"{"ok":{"ok":"aGVsbG8sIHdvcmxk"}}"#);
/// ```
///
/// Failure:
///
/// ```
/// # use cosmwasm_std::{to_vec, Binary, ContractResult, SystemResult, SystemError};
/// let error = SystemError::Unknown {};
/// let result: SystemResult<Binary> = SystemResult::Err(error);
/// assert_eq!(to_vec(&result).unwrap(), br#"{"error":{"unknown":{}}}"#);
/// ```
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, JsonSchema)]
#[serde(rename_all = "snake_case")]
pub enum SystemResult<S> {
    Ok(S),
    #[serde(rename = "error")]
    Err(SystemError),
}

// Implementations here mimic the Result API and should be implemented via a conversion to Result
// to ensure API consistency
impl<S> SystemResult<S> {
    /// Converts a `ContractResult<S>` to a `Result<S, SystemError>` as a convenient way
    /// to access the full Result API.
    pub fn into_result(self) -> Result<S, SystemError> {}

    pub fn unwrap(self) -> S {}
}

impl<S: fmt::Debug> SystemResult<S> {
    pub fn unwrap_err(self) -> SystemError {}
}

impl<S> From<Result<S, SystemError>> for SystemResult<S> {
    fn from(original: Result<S, SystemError>) -> SystemResult<S> {}
}

impl<S> From<SystemResult<S>> for Result<S, SystemError> {
    fn from(original: SystemResult<S>) -> Result<S, SystemError> {}
}
}

pub use contract_result::ContractResult;
#[cfg(all(feature = "stargate", feature = "cosmwasm_1_2"))]
pub use cosmos_msg::WeightedVoteOption;
pub use cosmos_msg::{wasm_execute, wasm_instantiate, BankMsg, CosmosMsg, CustomMsg, WasmMsg};
#[cfg(feature = "staking")]
pub use cosmos_msg::{DistributionMsg, StakingMsg};
#[cfg(feature = "stargate")]
pub use cosmos_msg::{GovMsg, VoteOption};
pub use empty::Empty;
pub use events::{attr, Attribute, Event};
pub use query::QueryResponse;
pub use response::Response;
#[allow(deprecated)]
pub use submessages::SubMsgExecutionResponse;
pub use submessages::{Reply, ReplyOn, SubMsg, SubMsgResponse, SubMsgResult};
pub use system_result::SystemResult;
}
mod sections {
use crate::conversion::force_to_u32;

/// A sections decoder for the special case of two elements
#[allow(dead_code)] // used in Wasm and tests only
pub fn decode_sections2(data: Vec<u8>) -> (Vec<u8>, Vec<u8>) {}

/// Encodes multiple sections of data into one vector.
///
/// Each section is suffixed by a section length encoded as big endian uint32.
/// Using suffixes instead of prefixes allows reading sections in reverse order,
/// such that the first element does not need to be re-allocated if the contract's
/// data structure supports truncation (such as a Rust vector).
///
/// The resulting data looks like this:
///
/// ```ignore
/// section1 || section1_len || section2 || section2_len || section3 || section3_len || …
/// ```
#[allow(dead_code)] // used in Wasm and tests only
pub fn encode_sections(sections: &[&[u8]]) -> Vec<u8> {}

/// Splits data into the last section ("tail") and the rest.
/// The tail's length information is cut off, such that it is ready to use.
/// The rest is basically unparsed and contails the lengths of the remaining sections.
///
/// While the tail is copied into a new vector, the rest is only truncated such that
/// no re-allocation is necessary.
///
/// If `data` contains one section only, `data` is moved into the tail entirely
fn split_tail(data: Vec<u8>) -> (Vec<u8>, Vec<u8>) {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn decode_sections2_works() {}

    #[test]
    fn decode_sections2_preserved_first_vector() {}

    #[test]
    fn encode_sections_works_for_empty_sections() {}

    #[test]
    fn encode_sections_works_for_one_element() {}

    #[test]
    fn encode_sections_works_for_multiple_elements() {}
}
}
mod serde {
// This file simply re-exports some methods from serde_json
// The reason is two fold:
// 1. To easily ensure that all calling libraries use the same version (minimize code size)
// 2. To allow us to switch out to eg. serde-json-core more easily
use serde::{de::DeserializeOwned, Serialize};
use std::any::type_name;

use crate::binary::Binary;
use crate::errors::{StdError, StdResult};

pub fn from_slice<T: DeserializeOwned>(value: &[u8]) -> StdResult<T> {}

pub fn from_binary<T: DeserializeOwned>(value: &Binary) -> StdResult<T> {}

pub fn to_vec<T>(data: &T) -> StdResult<Vec<u8>>
where
    T: Serialize + ?Sized,
{}

pub fn to_binary<T>(data: &T) -> StdResult<Binary>
where
    T: Serialize + ?Sized,
{}

#[cfg(test)]
mod tests {
    use super::*;
    use serde::Deserialize;

    #[derive(Serialize, Deserialize, Debug, PartialEq)]
    #[serde(rename_all = "snake_case")]
    enum SomeMsg {
        Refund {},
        ReleaseAll {
            image: String,
            amount: u32,
            time: u64,
            karma: i32,
        },
        Cowsay {
            text: String,
        },
    }

    #[test]
    fn to_vec_works() {}

    #[test]
    fn from_slice_works() {}

    #[test]
    fn from_slice_or_binary() {}

    #[test]
    fn to_vec_works_for_special_chars() {}

    #[test]
    fn from_slice_works_for_special_chars() {}
}
}
mod storage {
use std::collections::BTreeMap;
use std::fmt;
#[cfg(feature = "iterator")]
use std::iter;
#[cfg(feature = "iterator")]
use std::ops::{Bound, RangeBounds};

#[cfg(feature = "iterator")]
use crate::iterator::{Order, Record};
use crate::traits::Storage;

#[derive(Default)]
pub struct MemoryStorage {
    data: BTreeMap<Vec<u8>, Vec<u8>>,
}

impl MemoryStorage {
    pub fn new() -> Self {}
}

impl Storage for MemoryStorage {
    fn get(&self, key: &[u8]) -> Option<Vec<u8>> {}

    fn set(&mut self, key: &[u8], value: &[u8]) {}

    fn remove(&mut self, key: &[u8]) {}

    #[cfg(feature = "iterator")]
    /// range allows iteration over a set of keys, either forwards or backwards
    /// uses standard rust range notation, and eg db.range(b"foo"..b"bar") also works reverse
    fn range<'a>(
        &'a self,
        start: Option<&[u8]>,
        end: Option<&[u8]>,
        order: Order,
    ) -> Box<dyn Iterator<Item = Record> + 'a> {}
}

/// This debug implementation is made for inspecting storages in unit testing.
/// It is made for human readability only and the output can change at any time.
impl fmt::Debug for MemoryStorage {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {}
}

#[cfg(feature = "iterator")]
fn range_bounds(start: Option<&[u8]>, end: Option<&[u8]>) -> impl RangeBounds<Vec<u8>> {}

#[cfg(feature = "iterator")]
/// The BTreeMap specific key-value pair reference type, as returned by BTreeMap<Vec<u8>, Vec<u8>>::range.
/// This is internal as it can change any time if the map implementation is swapped out.
type BTreeMapRecordRef<'a> = (&'a Vec<u8>, &'a Vec<u8>);

#[cfg(feature = "iterator")]
fn clone_item(item_ref: BTreeMapRecordRef) -> Record {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn get_and_set() {}

    #[test]
    #[should_panic(
        expected = "Getting empty values from storage is not well supported at the moment."
    )]
    fn set_panics_for_empty() {}

    #[test]
    fn delete() {}

    #[test]
    #[cfg(feature = "iterator")]
    fn iterator() {}

    #[test]
    fn memory_storage_implements_debug() {}
}
}
mod timestamp {
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use std::fmt;

use crate::math::Uint64;

/// A point in time in nanosecond precision.
///
/// This type can represent times from 1970-01-01T00:00:00Z to 2554-07-21T23:34:33Z.
///
/// ## Examples
///
/// ```
/// # use cosmwasm_std::Timestamp;
/// let ts = Timestamp::from_nanos(1_000_000_202);
/// assert_eq!(ts.nanos(), 1_000_000_202);
/// assert_eq!(ts.seconds(), 1);
/// assert_eq!(ts.subsec_nanos(), 202);
///
/// let ts = ts.plus_seconds(2);
/// assert_eq!(ts.nanos(), 3_000_000_202);
/// assert_eq!(ts.seconds(), 3);
/// assert_eq!(ts.subsec_nanos(), 202);
/// ```
#[derive(
    Serialize, Deserialize, Copy, Clone, Default, Debug, PartialEq, Eq, PartialOrd, Ord, JsonSchema,
)]
pub struct Timestamp(Uint64);

impl Timestamp {
    /// Creates a timestamp from nanoseconds since epoch
    pub const fn from_nanos(nanos_since_epoch: u64) -> Self {}

    /// Creates a timestamp from seconds since epoch
    pub const fn from_seconds(seconds_since_epoch: u64) -> Self {}

    pub const fn plus_seconds(&self, addition: u64) -> Timestamp {}

    pub const fn plus_nanos(&self, addition: u64) -> Timestamp {}

    pub const fn minus_seconds(&self, subtrahend: u64) -> Timestamp {}

    pub const fn minus_nanos(&self, subtrahend: u64) -> Timestamp {}

    /// Returns nanoseconds since epoch
    #[inline]
    pub fn nanos(&self) -> u64 {}

    /// Returns seconds since epoch (truncate nanoseconds)
    #[inline]
    pub fn seconds(&self) -> u64 {}

    /// Returns nanoseconds since the last whole second (the remainder truncated
    /// by `seconds()`)
    #[inline]
    pub fn subsec_nanos(&self) -> u64 {}
}

impl fmt::Display for Timestamp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {}
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn timestamp_from_nanos() {}

    #[test]
    fn timestamp_from_seconds() {}

    #[test]
    fn timestamp_plus_seconds() {}

    #[test]
    fn timestamp_plus_nanos() {}

    #[test]
    fn timestamp_minus_seconds() {}

    #[test]
    #[should_panic(expected = "attempt to subtract with overflow")]
    fn timestamp_minus_seconds_panics_on_overflow() {}

    #[test]
    fn timestamp_minus_nanos() {}

    #[test]
    #[should_panic(expected = "attempt to subtract with overflow")]
    fn timestamp_minus_nanos_panics_on_overflow() {}

    #[test]
    fn timestamp_nanos() {}

    #[test]
    fn timestamp_seconds() {}

    #[test]
    fn timestamp_subsec_nanos() {}

    #[test]
    fn timestamp_implements_display() {}
}
}
mod traits {
use serde::{de::DeserializeOwned, Serialize};
use std::marker::PhantomData;
use std::ops::Deref;

use crate::addresses::{Addr, CanonicalAddr};
use crate::binary::Binary;
use crate::coin::Coin;
use crate::errors::{RecoverPubkeyError, StdError, StdResult, VerificationError};
#[cfg(feature = "iterator")]
use crate::iterator::{Order, Record};
#[cfg(feature = "cosmwasm_1_1")]
use crate::query::SupplyResponse;
use crate::query::{
    AllBalanceResponse, BalanceResponse, BankQuery, CustomQuery, QueryRequest, WasmQuery,
};
#[cfg(feature = "staking")]
use crate::query::{
    AllDelegationsResponse, AllValidatorsResponse, BondedDenomResponse, Delegation,
    DelegationResponse, FullDelegation, StakingQuery, Validator, ValidatorResponse,
};
use crate::results::{ContractResult, Empty, SystemResult};
use crate::serde::{from_binary, to_binary, to_vec};
use crate::ContractInfoResponse;

/// Storage provides read and write access to a persistent storage.
/// If you only want to provide read access, provide `&Storage`
pub trait Storage {
    /// Returns None when key does not exist.
    /// Returns Some(Vec<u8>) when key exists.
    ///
    /// Note: Support for differentiating between a non-existent key and a key with empty value
    /// is not great yet and might not be possible in all backends. But we're trying to get there.
    fn get(&self, key: &[u8]) -> Option<Vec<u8>>;

    #[cfg(feature = "iterator")]
    /// Allows iteration over a set of key/value pairs, either forwards or backwards.
    ///
    /// The bound `start` is inclusive and `end` is exclusive.
    ///
    /// If `start` is lexicographically greater than or equal to `end`, an empty range is described, mo matter of the order.
    fn range<'a>(
        &'a self,
        start: Option<&[u8]>,
        end: Option<&[u8]>,
        order: Order,
    ) -> Box<dyn Iterator<Item = Record> + 'a>;

    fn set(&mut self, key: &[u8], value: &[u8]);

    /// Removes a database entry at `key`.
    ///
    /// The current interface does not allow to differentiate between a key that existed
    /// before and one that didn't exist. See https://github.com/CosmWasm/cosmwasm/issues/290
    fn remove(&mut self, key: &[u8]);
}

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
pub trait Api {
    /// Takes a human readable address and validates if it is valid.
    /// If it the validation succeeds, a `Addr` containing the same data as the input is returned.
    ///
    /// This validation checks two things:
    /// 1. The address is valid in the sense that it can be converted to a canonical representation by the backend.
    /// 2. The address is normalized, i.e. `humanize(canonicalize(input)) == input`.
    ///
    /// Check #2 is typically needed for upper/lower case representations of the same
    /// address that are both valid according to #1. This way we ensure uniqueness
    /// of the human readable address. Clients should perform the normalization before sending
    /// the addresses to the CosmWasm stack. But please note that the definition of normalized
    /// depends on the backend.
    ///
    /// ## Examples
    ///
    /// ```
    /// # use cosmwasm_std::{Api, Addr};
    /// # use cosmwasm_std::testing::MockApi;
    /// # let api = MockApi::default();
    /// let input = "what-users-provide";
    /// let validated: Addr = api.addr_validate(input).unwrap();
    /// assert_eq!(validated, input);
    /// ```
    fn addr_validate(&self, human: &str) -> StdResult<Addr>;

    /// Takes a human readable address and returns a canonical binary representation of it.
    /// This can be used when a compact representation is needed.
    ///
    /// Please note that the length of the resulting address is defined by the chain and
    /// can vary from address to address. On Cosmos chains 20 and 32 bytes are typically used.
    /// But that might change. So your contract should not make assumptions on the size.
    fn addr_canonicalize(&self, human: &str) -> StdResult<CanonicalAddr>;

    /// Takes a canonical address and returns a human readble address.
    /// This is the inverse of [`addr_canonicalize`].
    ///
    /// [`addr_canonicalize`]: Api::addr_canonicalize
    fn addr_humanize(&self, canonical: &CanonicalAddr) -> StdResult<Addr>;

    fn secp256k1_verify(
        &self,
        message_hash: &[u8],
        signature: &[u8],
        public_key: &[u8],
    ) -> Result<bool, VerificationError>;

    fn groth16_verify(
        &self,
        input: &[u8],
        proof: &[u8],
        vk: &[u8],
    ) -> Result<bool, VerificationError>;

    fn poseidon_hash(&self, inputs: &[&[u8]]) -> StdResult<Vec<u8>>;

    fn curve_hash(&self, input: &[u8]) -> StdResult<Vec<u8>>;

    fn secp256k1_recover_pubkey(
        &self,
        message_hash: &[u8],
        signature: &[u8],
        recovery_param: u8,
    ) -> Result<Vec<u8>, RecoverPubkeyError>;

    fn ed25519_verify(
        &self,
        message: &[u8],
        signature: &[u8],
        public_key: &[u8],
    ) -> Result<bool, VerificationError>;

    fn ed25519_batch_verify(
        &self,
        messages: &[&[u8]],
        signatures: &[&[u8]],
        public_keys: &[&[u8]],
    ) -> Result<bool, VerificationError>;

    /// Emits a debugging message that is handled depending on the environment (typically printed to console or ignored).
    /// Those messages are not persisted to chain.
    fn debug(&self, message: &str);
}

/// A short-hand alias for the two-level query result (1. accessing the contract, 2. executing query in the contract)
pub type QuerierResult = SystemResult<ContractResult<Binary>>;

pub trait Querier {
    /// raw_query is all that must be implemented for the Querier.
    /// This allows us to pass through binary queries from one level to another without
    /// knowing the custom format, or we can decode it, with the knowledge of the allowed
    /// types. People using the querier probably want one of the simpler auto-generated
    /// helper methods
    fn raw_query(&self, bin_request: &[u8]) -> QuerierResult;
}

#[derive(Clone)]
pub struct QuerierWrapper<'a, C: CustomQuery = Empty> {
    querier: &'a dyn Querier,
    custom_query_type: PhantomData<C>,
}

// Use custom implementation on order to implement Copy in case `C` is not `Copy`.
// See "There is a small difference between the two: the derive strategy will also
// place a Copy bound on type parameters, which isn’t always desired."
// https://doc.rust-lang.org/std/marker/trait.Copy.html
impl<'a, C: CustomQuery> Copy for QuerierWrapper<'a, C> {}

/// This allows us to use self.raw_query to access the querier.
/// It also allows external callers to access the querier easily.
impl<'a, C: CustomQuery> Deref for QuerierWrapper<'a, C> {
    type Target = dyn Querier + 'a;

    fn deref(&self) -> &Self::Target {}
}

impl<'a, C: CustomQuery> QuerierWrapper<'a, C> {
    pub fn new(querier: &'a dyn Querier) -> Self {}

    /// Makes the query and parses the response.
    ///
    /// Any error (System Error, Error or called contract, or Parse Error) are flattened into
    /// one level. Only use this if you don't need to check the SystemError
    /// eg. If you don't differentiate between contract missing and contract returned error
    pub fn query<U: DeserializeOwned>(&self, request: &QueryRequest<C>) -> StdResult<U> {}

    #[cfg(feature = "cosmwasm_1_1")]
    pub fn query_supply(&self, denom: impl Into<String>) -> StdResult<Coin> {}

    pub fn query_balance(
        &self,
        address: impl Into<String>,
        denom: impl Into<String>,
    ) -> StdResult<Coin> {}

    pub fn query_all_balances(&self, address: impl Into<String>) -> StdResult<Vec<Coin>> {}

    // this queries another wasm contract. You should know a priori the proper types for T and U
    // (response and request) based on the contract API
    pub fn query_wasm_smart<T: DeserializeOwned>(
        &self,
        contract_addr: impl Into<String>,
        msg: &impl Serialize,
    ) -> StdResult<T> {}

    // this queries the raw storage from another wasm contract.
    // you must know the exact layout and are implementation dependent
    // (not tied to an interface like query_wasm_smart)
    // that said, if you are building a few contracts together, this is a much cheaper approach
    //
    // Similar return value to Storage.get(). Returns Some(val) or None if the data is there.
    // It only returns error on some runtime issue, not on any data cases.
    pub fn query_wasm_raw(
        &self,
        contract_addr: impl Into<String>,
        key: impl Into<Binary>,
    ) -> StdResult<Option<Vec<u8>>> {}

    /// Given a contract address, query information about that contract.
    pub fn query_wasm_contract_info(
        &self,
        contract_addr: impl Into<String>,
    ) -> StdResult<ContractInfoResponse> {}

    #[cfg(feature = "staking")]
    pub fn query_all_validators(&self) -> StdResult<Vec<Validator>> {}

    #[cfg(feature = "staking")]
    pub fn query_validator(&self, address: impl Into<String>) -> StdResult<Option<Validator>> {}

    #[cfg(feature = "staking")]
    pub fn query_bonded_denom(&self) -> StdResult<String> {}

    #[cfg(feature = "staking")]
    pub fn query_all_delegations(
        &self,
        delegator: impl Into<String>,
    ) -> StdResult<Vec<Delegation>> {}

    #[cfg(feature = "staking")]
    pub fn query_delegation(
        &self,
        delegator: impl Into<String>,
        validator: impl Into<String>,
    ) -> StdResult<Option<FullDelegation>> {}
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::testing::MockQuerier;
    use crate::{coins, from_slice, Uint128};

    // this is a simple demo helper to prove we can use it
    fn demo_helper(_querier: &dyn Querier) -> u64 {}

    // this just needs to compile to prove we can use it
    #[test]
    fn use_querier_wrapper_as_querier() {}

    #[test]
    fn auto_deref_raw_query() {}

    #[cfg(feature = "cosmwasm_1_1")]
    #[test]
    fn bank_query_helpers_work() {}

    #[test]
    fn contract_info() {}

    #[test]
    fn contract_info_err() {}
}
}
mod types {
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

use crate::addresses::Addr;
use crate::coin::Coin;
use crate::timestamp::Timestamp;

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, JsonSchema)]
pub struct Env {
    pub block: BlockInfo,
    /// Information on the transaction this message was executed in.
    /// The field is unset when the `MsgExecuteContract`/`MsgInstantiateContract`/`MsgMigrateContract`
    /// is not executed as part of a transaction.
    pub transaction: Option<TransactionInfo>,
    pub contract: ContractInfo,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, JsonSchema)]
pub struct TransactionInfo {
    /// The position of this transaction in the block. The first
    /// transaction has index 0.
    ///
    /// This allows you to get a unique transaction indentifier in this chain
    /// using the pair (`env.block.height`, `env.transaction.index`).
    ///
    pub index: u32,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, JsonSchema)]
pub struct BlockInfo {
    /// The height of a block is the number of blocks preceding it in the blockchain.
    pub height: u64,
    /// Absolute time of the block creation in seconds since the UNIX epoch (00:00:00 on 1970-01-01 UTC).
    ///
    /// The source of this is the [BFT Time in Tendermint](https://github.com/tendermint/tendermint/blob/58dc1726/spec/consensus/bft-time.md),
    /// which has the same nanosecond precision as the `Timestamp` type.
    ///
    /// # Examples
    ///
    /// Using chrono:
    ///
    /// ```
    /// # use cosmwasm_std::{Addr, BlockInfo, ContractInfo, Env, MessageInfo, Timestamp, TransactionInfo};
    /// # let env = Env {
    /// #     block: BlockInfo {
    /// #         height: 12_345,
    /// #         time: Timestamp::from_nanos(1_571_797_419_879_305_533),
    /// #         chain_id: "cosmos-testnet-14002".to_string(),
    /// #     },
    /// #     transaction: Some(TransactionInfo { index: 3 }),
    /// #     contract: ContractInfo {
    /// #         address: Addr::unchecked("contract"),
    /// #     },
    /// # };
    /// # extern crate chrono;
    /// use chrono::NaiveDateTime;
    /// let seconds = env.block.time.seconds();
    /// let nsecs = env.block.time.subsec_nanos();
    /// let dt = NaiveDateTime::from_timestamp(seconds as i64, nsecs as u32);
    /// ```
    ///
    /// Creating a simple millisecond-precision timestamp (as used in JavaScript):
    ///
    /// ```
    /// # use cosmwasm_std::{Addr, BlockInfo, ContractInfo, Env, MessageInfo, Timestamp, TransactionInfo};
    /// # let env = Env {
    /// #     block: BlockInfo {
    /// #         height: 12_345,
    /// #         time: Timestamp::from_nanos(1_571_797_419_879_305_533),
    /// #         chain_id: "cosmos-testnet-14002".to_string(),
    /// #     },
    /// #     transaction: Some(TransactionInfo { index: 3 }),
    /// #     contract: ContractInfo {
    /// #         address: Addr::unchecked("contract"),
    /// #     },
    /// # };
    /// let millis = env.block.time.nanos() / 1_000_000;
    /// ```
    pub time: Timestamp,
    pub chain_id: String,
}

/// Additional information from [MsgInstantiateContract] and [MsgExecuteContract], which is passed
/// along with the contract execution message into the `instantiate` and `execute` entry points.
///
/// It contains the essential info for authorization - identity of the call, and payment.
///
/// [MsgInstantiateContract]: https://github.com/CosmWasm/wasmd/blob/v0.15.0/x/wasm/internal/types/tx.proto#L47-L61
/// [MsgExecuteContract]: https://github.com/CosmWasm/wasmd/blob/v0.15.0/x/wasm/internal/types/tx.proto#L68-L78
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct MessageInfo {
    /// The `sender` field from `MsgInstantiateContract` and `MsgExecuteContract`.
    /// You can think of this as the address that initiated the action (i.e. the message). What that
    /// means exactly heavily depends on the application.
    ///
    /// The x/wasm module ensures that the sender address signed the transaction or
    /// is otherwise authorized to send the message.
    ///
    /// Additional signers of the transaction that are either needed for other messages or contain unnecessary
    /// signatures are not propagated into the contract.
    pub sender: Addr,
    /// The funds that are sent to the contract as part of `MsgInstantiateContract`
    /// or `MsgExecuteContract`. The transfer is processed in bank before the contract
    /// is executed such that the new balance is visible during contract execution.
    pub funds: Vec<Coin>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, JsonSchema)]
pub struct ContractInfo {
    pub address: Addr,
}
}

pub use crate::addresses::{instantiate2_address, Addr, CanonicalAddr};
pub use crate::binary::Binary;
pub use crate::coin::{coin, coins, has_coins, Coin};
pub use crate::deps::{Deps, DepsMut, OwnedDeps};
pub use crate::errors::{
    CheckedFromRatioError, CheckedMultiplyRatioError, ConversionOverflowError, DivideByZeroError,
    OverflowError, OverflowOperation, RecoverPubkeyError, StdError, StdResult, SystemError,
    VerificationError,
};
pub use crate::hex_binary::HexBinary;
#[cfg(feature = "stargate")]
pub use crate::ibc::{
    Ibc3ChannelOpenResponse, IbcAcknowledgement, IbcBasicResponse, IbcChannel, IbcChannelCloseMsg,
    IbcChannelConnectMsg, IbcChannelOpenMsg, IbcChannelOpenResponse, IbcEndpoint, IbcMsg, IbcOrder,
    IbcPacket, IbcPacketAckMsg, IbcPacketReceiveMsg, IbcPacketTimeoutMsg, IbcReceiveResponse,
    IbcTimeout, IbcTimeoutBlock,
};
#[cfg(feature = "iterator")]
pub use crate::iterator::{Order, Record};
pub use crate::math::{
    Decimal, Decimal256, Decimal256RangeExceeded, DecimalRangeExceeded, Fraction, Isqrt, Uint128,
    Uint256, Uint512, Uint64,
};
#[cfg(feature = "cosmwasm_1_1")]
pub use crate::query::SupplyResponse;
pub use crate::query::{
    AllBalanceResponse, BalanceResponse, BankQuery, ContractInfoResponse, CustomQuery,
    QueryRequest, WasmQuery,
};
#[cfg(feature = "staking")]
pub use crate::query::{
    AllDelegationsResponse, AllValidatorsResponse, BondedDenomResponse, Delegation,
    DelegationResponse, FullDelegation, StakingQuery, Validator, ValidatorResponse,
};
#[cfg(feature = "stargate")]
pub use crate::query::{ChannelResponse, IbcQuery, ListChannelsResponse, PortIdResponse};
#[allow(deprecated)]
pub use crate::results::SubMsgExecutionResponse;
#[cfg(all(feature = "stargate", feature = "cosmwasm_1_2"))]
pub use crate::results::WeightedVoteOption;
pub use crate::results::{
    attr, wasm_execute, wasm_instantiate, Attribute, BankMsg, ContractResult, CosmosMsg, CustomMsg,
    Empty, Event, QueryResponse, Reply, ReplyOn, Response, SubMsg, SubMsgResponse, SubMsgResult,
    SystemResult, WasmMsg,
};
#[cfg(feature = "staking")]
pub use crate::results::{DistributionMsg, StakingMsg};
#[cfg(feature = "stargate")]
pub use crate::results::{GovMsg, VoteOption};
pub use crate::serde::{from_binary, from_slice, to_binary, to_vec};
pub use crate::storage::MemoryStorage;
pub use crate::timestamp::Timestamp;
pub use crate::traits::{Api, Querier, QuerierResult, QuerierWrapper, Storage};
pub use crate::types::{BlockInfo, ContractInfo, Env, MessageInfo, TransactionInfo};

// Exposed in wasm build only

#[cfg(target_arch = "wasm32")]
mod exports {
//! exports exposes the public wasm API
//!
//! interface_version_8, allocate and deallocate turn into Wasm exports
//! as soon as cosmwasm_std is `use`d in the contract, even privately.
//!
//! `do_execute`, `do_instantiate`, `do_migrate`, `do_query`, `do_reply`
//! and `do_sudo` should be wrapped with a extern "C" entry point including
//! the contract-specific function pointer. This is done via the `#[entry_point]`
//! macro attribute from cosmwasm-derive.
use std::marker::PhantomData;
use std::vec::Vec;

use serde::de::DeserializeOwned;

use crate::deps::OwnedDeps;
#[cfg(feature = "stargate")]
use crate::ibc::{
    IbcBasicResponse, IbcChannelCloseMsg, IbcChannelConnectMsg, IbcChannelOpenMsg,
    IbcChannelOpenResponse, IbcPacketAckMsg, IbcPacketReceiveMsg, IbcPacketTimeoutMsg,
    IbcReceiveResponse,
};
use crate::imports::{ExternalApi, ExternalQuerier, ExternalStorage};
use crate::memory::{alloc, consume_region, release_buffer, Region};
#[cfg(feature = "abort")]
use crate::panic::install_panic_handler;
use crate::query::CustomQuery;
use crate::results::{ContractResult, QueryResponse, Reply, Response};
use crate::serde::{from_slice, to_vec};
use crate::types::Env;
use crate::{CustomMsg, Deps, DepsMut, MessageInfo};

#[cfg(feature = "iterator")]
#[no_mangle]
extern "C" fn requires_iterator() -> () {}

#[cfg(feature = "staking")]
#[no_mangle]
extern "C" fn requires_staking() -> () {}

#[cfg(feature = "stargate")]
#[no_mangle]
extern "C" fn requires_stargate() -> () {}

#[cfg(feature = "cosmwasm_1_1")]
#[no_mangle]
extern "C" fn requires_cosmwasm_1_1() -> () {}

#[cfg(feature = "cosmwasm_1_2")]
#[no_mangle]
extern "C" fn requires_cosmwasm_1_2() -> () {}

/// interface_version_* exports mark which Wasm VM interface level this contract is compiled for.
/// They can be checked by cosmwasm_vm.
/// Update this whenever the Wasm VM interface breaks.
#[no_mangle]
extern "C" fn interface_version_8() -> () {}

/// allocate reserves the given number of bytes in wasm memory and returns a pointer
/// to a Region defining this data. This space is managed by the calling process
/// and should be accompanied by a corresponding deallocate
#[no_mangle]
extern "C" fn allocate(size: usize) -> u32 {}

/// deallocate expects a pointer to a Region created with allocate.
/// It will free both the Region and the memory referenced by the Region.
#[no_mangle]
extern "C" fn deallocate(pointer: u32) {}

// TODO: replace with https://doc.rust-lang.org/std/ops/trait.Try.html once stabilized
macro_rules! r#try_into_contract_result {
    ($expr:expr) => {
        match $expr {
            Ok(val) => val,
            Err(err) => {
                return ContractResult::Err(err.to_string());
            }
        }
    };
    ($expr:expr,) => {
        $crate::try_into_contract_result!($expr)
    };
}

/// This should be wrapped in an external "C" export, containing a contract-specific function as an argument.
///
/// - `Q`: custom query type (see QueryRequest)
/// - `M`: message type for request
/// - `C`: custom response message type (see CosmosMsg)
/// - `E`: error type for responses
pub fn do_instantiate<Q, M, C, E>(
    instantiate_fn: &dyn Fn(DepsMut<Q>, Env, MessageInfo, M) -> Result<Response<C>, E>,
    env_ptr: u32,
    info_ptr: u32,
    msg_ptr: u32,
) -> u32
where
    Q: CustomQuery,
    M: DeserializeOwned,
    C: CustomMsg,
    E: ToString,
{}

/// do_execute should be wrapped in an external "C" export, containing a contract-specific function as arg
///
/// - `Q`: custom query type (see QueryRequest)
/// - `M`: message type for request
/// - `C`: custom response message type (see CosmosMsg)
/// - `E`: error type for responses
pub fn do_execute<Q, M, C, E>(
    execute_fn: &dyn Fn(DepsMut<Q>, Env, MessageInfo, M) -> Result<Response<C>, E>,
    env_ptr: u32,
    info_ptr: u32,
    msg_ptr: u32,
) -> u32
where
    Q: CustomQuery,
    M: DeserializeOwned,
    C: CustomMsg,
    E: ToString,
{}

/// do_migrate should be wrapped in an external "C" export, containing a contract-specific function as arg
///
/// - `Q`: custom query type (see QueryRequest)
/// - `M`: message type for request
/// - `C`: custom response message type (see CosmosMsg)
/// - `E`: error type for responses
pub fn do_migrate<Q, M, C, E>(
    migrate_fn: &dyn Fn(DepsMut<Q>, Env, M) -> Result<Response<C>, E>,
    env_ptr: u32,
    msg_ptr: u32,
) -> u32
where
    Q: CustomQuery,
    M: DeserializeOwned,
    C: CustomMsg,
    E: ToString,
{}

/// do_sudo should be wrapped in an external "C" export, containing a contract-specific function as arg
///
/// - `Q`: custom query type (see QueryRequest)
/// - `M`: message type for request
/// - `C`: custom response message type (see CosmosMsg)
/// - `E`: error type for responses
pub fn do_sudo<Q, M, C, E>(
    sudo_fn: &dyn Fn(DepsMut<Q>, Env, M) -> Result<Response<C>, E>,
    env_ptr: u32,
    msg_ptr: u32,
) -> u32
where
    Q: CustomQuery,
    M: DeserializeOwned,
    C: CustomMsg,
    E: ToString,
{}

/// do_reply should be wrapped in an external "C" export, containing a contract-specific function as arg
/// message body is always `SubcallResult`
///
/// - `Q`: custom query type (see QueryRequest)
/// - `C`: custom response message type (see CosmosMsg)
/// - `E`: error type for responses
pub fn do_reply<Q, C, E>(
    reply_fn: &dyn Fn(DepsMut<Q>, Env, Reply) -> Result<Response<C>, E>,
    env_ptr: u32,
    msg_ptr: u32,
) -> u32
where
    Q: CustomQuery,
    C: CustomMsg,
    E: ToString,
{}

/// do_query should be wrapped in an external "C" export, containing a contract-specific function as arg
///
/// - `Q`: custom query type (see QueryRequest)
/// - `M`: message type for request
/// - `E`: error type for responses
pub fn do_query<Q, M, E>(
    query_fn: &dyn Fn(Deps<Q>, Env, M) -> Result<QueryResponse, E>,
    env_ptr: u32,
    msg_ptr: u32,
) -> u32
where
    Q: CustomQuery,
    M: DeserializeOwned,
    E: ToString,
{}

/// do_ibc_channel_open is designed for use with #[entry_point] to make a "C" extern
///
/// contract_fn does the protocol version negotiation during channel handshake phase
///
/// - `Q`: custom query type (see QueryRequest)
/// - `E`: error type for responses
#[cfg(feature = "stargate")]
pub fn do_ibc_channel_open<Q, E>(
    contract_fn: &dyn Fn(DepsMut<Q>, Env, IbcChannelOpenMsg) -> Result<IbcChannelOpenResponse, E>,
    env_ptr: u32,
    msg_ptr: u32,
) -> u32
where
    Q: CustomQuery,
    E: ToString,
{}

/// do_ibc_channel_connect is designed for use with #[entry_point] to make a "C" extern
///
/// contract_fn is a callback when a IBC channel is established (after both sides agree in open)
///
/// - `Q`: custom query type (see QueryRequest)
/// - `C`: custom response message type (see CosmosMsg)
/// - `E`: error type for responses
#[cfg(feature = "stargate")]
pub fn do_ibc_channel_connect<Q, C, E>(
    contract_fn: &dyn Fn(DepsMut<Q>, Env, IbcChannelConnectMsg) -> Result<IbcBasicResponse<C>, E>,
    env_ptr: u32,
    msg_ptr: u32,
) -> u32
where
    Q: CustomQuery,
    C: CustomMsg,
    E: ToString,
{}

/// do_ibc_channel_close is designed for use with #[entry_point] to make a "C" extern
///
/// contract_fn is a callback when a IBC channel belonging to this contract is closed
///
/// - `Q`: custom query type (see QueryRequest)
/// - `C`: custom response message type (see CosmosMsg)
/// - `E`: error type for responses
#[cfg(feature = "stargate")]
pub fn do_ibc_channel_close<Q, C, E>(
    contract_fn: &dyn Fn(DepsMut<Q>, Env, IbcChannelCloseMsg) -> Result<IbcBasicResponse<C>, E>,
    env_ptr: u32,
    msg_ptr: u32,
) -> u32
where
    Q: CustomQuery,
    C: CustomMsg,
    E: ToString,
{}

/// do_ibc_packet_receive is designed for use with #[entry_point] to make a "C" extern
///
/// contract_fn is called when this chain receives an IBC Packet on a channel belonging
/// to this contract
///
/// - `Q`: custom query type (see QueryRequest)
/// - `C`: custom response message type (see CosmosMsg)
/// - `E`: error type for responses
#[cfg(feature = "stargate")]
pub fn do_ibc_packet_receive<Q, C, E>(
    contract_fn: &dyn Fn(DepsMut<Q>, Env, IbcPacketReceiveMsg) -> Result<IbcReceiveResponse<C>, E>,
    env_ptr: u32,
    msg_ptr: u32,
) -> u32
where
    Q: CustomQuery,
    C: CustomMsg,
    E: ToString,
{}

/// do_ibc_packet_ack is designed for use with #[entry_point] to make a "C" extern
///
/// contract_fn is called when this chain receives an IBC Acknowledgement for a packet
/// that this contract previously sent
///
/// - `Q`: custom query type (see QueryRequest)
/// - `C`: custom response message type (see CosmosMsg)
/// - `E`: error type for responses
#[cfg(feature = "stargate")]
pub fn do_ibc_packet_ack<Q, C, E>(
    contract_fn: &dyn Fn(DepsMut<Q>, Env, IbcPacketAckMsg) -> Result<IbcBasicResponse<C>, E>,
    env_ptr: u32,
    msg_ptr: u32,
) -> u32
where
    Q: CustomQuery,
    C: CustomMsg,
    E: ToString,
{}

/// do_ibc_packet_timeout is designed for use with #[entry_point] to make a "C" extern
///
/// contract_fn is called when a packet that this contract previously sent has provably
/// timedout and will never be relayed to the calling chain. This generally behaves
/// like ick_ack_fn upon an acknowledgement containing an error.
///
/// - `Q`: custom query type (see QueryRequest)
/// - `C`: custom response message type (see CosmosMsg)
/// - `E`: error type for responses
#[cfg(feature = "stargate")]
pub fn do_ibc_packet_timeout<Q, C, E>(
    contract_fn: &dyn Fn(DepsMut<Q>, Env, IbcPacketTimeoutMsg) -> Result<IbcBasicResponse<C>, E>,
    env_ptr: u32,
    msg_ptr: u32,
) -> u32
where
    Q: CustomQuery,
    C: CustomMsg,
    E: ToString,
{}

fn _do_instantiate<Q, M, C, E>(
    instantiate_fn: &dyn Fn(DepsMut<Q>, Env, MessageInfo, M) -> Result<Response<C>, E>,
    env_ptr: *mut Region,
    info_ptr: *mut Region,
    msg_ptr: *mut Region,
) -> ContractResult<Response<C>>
where
    Q: CustomQuery,
    M: DeserializeOwned,
    C: CustomMsg,
    E: ToString,
{}

fn _do_execute<Q, M, C, E>(
    execute_fn: &dyn Fn(DepsMut<Q>, Env, MessageInfo, M) -> Result<Response<C>, E>,
    env_ptr: *mut Region,
    info_ptr: *mut Region,
    msg_ptr: *mut Region,
) -> ContractResult<Response<C>>
where
    Q: CustomQuery,
    M: DeserializeOwned,
    C: CustomMsg,
    E: ToString,
{}

fn _do_migrate<Q, M, C, E>(
    migrate_fn: &dyn Fn(DepsMut<Q>, Env, M) -> Result<Response<C>, E>,
    env_ptr: *mut Region,
    msg_ptr: *mut Region,
) -> ContractResult<Response<C>>
where
    Q: CustomQuery,
    M: DeserializeOwned,
    C: CustomMsg,
    E: ToString,
{}

fn _do_sudo<Q, M, C, E>(
    sudo_fn: &dyn Fn(DepsMut<Q>, Env, M) -> Result<Response<C>, E>,
    env_ptr: *mut Region,
    msg_ptr: *mut Region,
) -> ContractResult<Response<C>>
where
    Q: CustomQuery,
    M: DeserializeOwned,
    C: CustomMsg,
    E: ToString,
{}

fn _do_reply<Q, C, E>(
    reply_fn: &dyn Fn(DepsMut<Q>, Env, Reply) -> Result<Response<C>, E>,
    env_ptr: *mut Region,
    msg_ptr: *mut Region,
) -> ContractResult<Response<C>>
where
    Q: CustomQuery,
    C: CustomMsg,
    E: ToString,
{}

fn _do_query<Q, M, E>(
    query_fn: &dyn Fn(Deps<Q>, Env, M) -> Result<QueryResponse, E>,
    env_ptr: *mut Region,
    msg_ptr: *mut Region,
) -> ContractResult<QueryResponse>
where
    Q: CustomQuery,
    M: DeserializeOwned,
    E: ToString,
{}

#[cfg(feature = "stargate")]
fn _do_ibc_channel_open<Q, E>(
    contract_fn: &dyn Fn(DepsMut<Q>, Env, IbcChannelOpenMsg) -> Result<IbcChannelOpenResponse, E>,
    env_ptr: *mut Region,
    msg_ptr: *mut Region,
) -> ContractResult<IbcChannelOpenResponse>
where
    Q: CustomQuery,
    E: ToString,
{}

#[cfg(feature = "stargate")]
fn _do_ibc_channel_connect<Q, C, E>(
    contract_fn: &dyn Fn(DepsMut<Q>, Env, IbcChannelConnectMsg) -> Result<IbcBasicResponse<C>, E>,
    env_ptr: *mut Region,
    msg_ptr: *mut Region,
) -> ContractResult<IbcBasicResponse<C>>
where
    Q: CustomQuery,
    C: CustomMsg,
    E: ToString,
{}

#[cfg(feature = "stargate")]
fn _do_ibc_channel_close<Q, C, E>(
    contract_fn: &dyn Fn(DepsMut<Q>, Env, IbcChannelCloseMsg) -> Result<IbcBasicResponse<C>, E>,
    env_ptr: *mut Region,
    msg_ptr: *mut Region,
) -> ContractResult<IbcBasicResponse<C>>
where
    Q: CustomQuery,
    C: CustomMsg,
    E: ToString,
{}

#[cfg(feature = "stargate")]
fn _do_ibc_packet_receive<Q, C, E>(
    contract_fn: &dyn Fn(DepsMut<Q>, Env, IbcPacketReceiveMsg) -> Result<IbcReceiveResponse<C>, E>,
    env_ptr: *mut Region,
    msg_ptr: *mut Region,
) -> ContractResult<IbcReceiveResponse<C>>
where
    Q: CustomQuery,
    C: CustomMsg,
    E: ToString,
{}

#[cfg(feature = "stargate")]
fn _do_ibc_packet_ack<Q, C, E>(
    contract_fn: &dyn Fn(DepsMut<Q>, Env, IbcPacketAckMsg) -> Result<IbcBasicResponse<C>, E>,
    env_ptr: *mut Region,
    msg_ptr: *mut Region,
) -> ContractResult<IbcBasicResponse<C>>
where
    Q: CustomQuery,
    C: CustomMsg,
    E: ToString,
{}

#[cfg(feature = "stargate")]
fn _do_ibc_packet_timeout<Q, C, E>(
    contract_fn: &dyn Fn(DepsMut<Q>, Env, IbcPacketTimeoutMsg) -> Result<IbcBasicResponse<C>, E>,
    env_ptr: *mut Region,
    msg_ptr: *mut Region,
) -> ContractResult<IbcBasicResponse<C>>
where
    Q: CustomQuery,
    C: CustomMsg,
    E: ToString,
{}

/// Makes all bridges to external dependencies (i.e. Wasm imports) that are injected by the VM
pub(crate) fn make_dependencies<Q>() -> OwnedDeps<ExternalStorage, ExternalApi, ExternalQuerier, Q>
where
    Q: CustomQuery,
{}
}
#[cfg(target_arch = "wasm32")]
mod imports {
use std::vec::Vec;

use crate::addresses::{Addr, CanonicalAddr};
use crate::errors::{RecoverPubkeyError, StdError, StdResult, SystemError, VerificationError};
use crate::import_helpers::{from_high_half, from_low_half};
use crate::memory::{alloc, build_region, consume_region, Region};
use crate::results::SystemResult;
#[cfg(feature = "iterator")]
use crate::sections::decode_sections2;
use crate::sections::encode_sections;
use crate::serde::from_slice;
use crate::traits::{Api, Querier, QuerierResult, Storage};
#[cfg(feature = "iterator")]
use crate::{
    iterator::{Order, Record},
    memory::get_optional_region_address,
};

/// An upper bound for typical canonical address lengths (e.g. 20 in Cosmos SDK/Ethereum or 32 in Nano/Substrate)
const CANONICAL_ADDRESS_BUFFER_LENGTH: usize = 64;
/// An upper bound for typical human readable address formats (e.g. 42 for Ethereum hex addresses or 90 for bech32)
const HUMAN_ADDRESS_BUFFER_LENGTH: usize = 90;

// This interface will compile into required Wasm imports.
// A complete documentation those functions is available in the VM that provides them:
// https://github.com/CosmWasm/cosmwasm/blob/v1.0.0-beta/packages/vm/src/instance.rs#L89-L206
extern "C" {
    #[cfg(feature = "abort")]
    fn abort(source_ptr: u32);

    fn db_read(key: u32) -> u32;
    fn db_write(key: u32, value: u32);
    fn db_remove(key: u32);

    // scan creates an iterator, which can be read by consecutive next() calls
    #[cfg(feature = "iterator")]
    fn db_scan(start_ptr: u32, end_ptr: u32, order: i32) -> u32;
    #[cfg(feature = "iterator")]
    fn db_next(iterator_id: u32) -> u32;

    fn addr_validate(source_ptr: u32) -> u32;
    fn addr_canonicalize(source_ptr: u32, destination_ptr: u32) -> u32;
    fn addr_humanize(source_ptr: u32, destination_ptr: u32) -> u32;

    /// Verifies message hashes against a signature with a public key, using the
    /// secp256k1 ECDSA parametrization.
    /// Returns 0 on verification success, 1 on verification failure, and values
    /// greater than 1 in case of error.
    fn secp256k1_verify(message_hash_ptr: u32, signature_ptr: u32, public_key_ptr: u32) -> u32;

    /// Verifies groth 16
    fn groth16_verify(input_ptr: u32, proof_ptr: u32, vk_ptr: u32) -> u32;
    /// poseidon hash
    fn poseidon_hash(inputs_ptr: u32, hash_ptr: u32) -> u32;
    /// on curve hash
    fn curve_hash(input_ptr: u32, hash_ptr: u32) -> u32;

    fn secp256k1_recover_pubkey(
        message_hash_ptr: u32,
        signature_ptr: u32,
        recovery_param: u32,
    ) -> u64;

    /// Verifies a message against a signature with a public key, using the
    /// ed25519 EdDSA scheme.
    /// Returns 0 on verification success, 1 on verification failure, and values
    /// greater than 1 in case of error.
    fn ed25519_verify(message_ptr: u32, signature_ptr: u32, public_key_ptr: u32) -> u32;

    /// Verifies a batch of messages against a batch of signatures and public keys, using the
    /// ed25519 EdDSA scheme.
    /// Returns 0 on verification success, 1 on verification failure, and values
    /// greater than 1 in case of error.
    fn ed25519_batch_verify(messages_ptr: u32, signatures_ptr: u32, public_keys_ptr: u32) -> u32;

    /// Writes a debug message (UFT-8 encoded) to the host for debugging purposes.
    /// The host is free to log or process this in any way it considers appropriate.
    /// In production environments it is expected that those messages are discarded.
    fn debug(source_ptr: u32);

    /// Executes a query on the chain (import). Not to be confused with the
    /// query export, which queries the state of the contract.
    fn query_chain(request: u32) -> u32;
}

/// A stateless convenience wrapper around database imports provided by the VM.
/// This cannot be cloned as it would not copy any data. If you need to clone this, it indicates a flaw in your logic.
pub struct ExternalStorage {}

impl ExternalStorage {
    pub fn new() -> ExternalStorage {}
}

impl Storage for ExternalStorage {
    fn get(&self, key: &[u8]) -> Option<Vec<u8>> {}

    fn set(&mut self, key: &[u8], value: &[u8]) {}

    fn remove(&mut self, key: &[u8]) {}

    #[cfg(feature = "iterator")]
    fn range(
        &self,
        start: Option<&[u8]>,
        end: Option<&[u8]>,
        order: Order,
    ) -> Box<dyn Iterator<Item = Record>> {}
}

#[cfg(feature = "iterator")]
/// ExternalIterator makes a call out to next.
/// We use the pointer to differentiate between multiple open iterators.
struct ExternalIterator {
    iterator_id: u32,
}

#[cfg(feature = "iterator")]
impl Iterator for ExternalIterator {
    type Item = Record;

    fn next(&mut self) -> Option<Self::Item> {}
}

/// A stateless convenience wrapper around imports provided by the VM
#[derive(Copy, Clone)]
pub struct ExternalApi {}

impl ExternalApi {
    pub fn new() -> ExternalApi {}
}

impl Api for ExternalApi {
    fn addr_validate(&self, input: &str) -> StdResult<Addr> {}

    fn addr_canonicalize(&self, input: &str) -> StdResult<CanonicalAddr> {}

    fn addr_humanize(&self, canonical: &CanonicalAddr) -> StdResult<Addr> {}

    fn secp256k1_verify(
        &self,
        message_hash: &[u8],
        signature: &[u8],
        public_key: &[u8],
    ) -> Result<bool, VerificationError> {}

    fn groth16_verify(
        &self,
        input: &[u8],
        proof: &[u8],
        vk: &[u8],
    ) -> Result<bool, VerificationError> {}

    fn poseidon_hash(&self, inputs: &[&[u8]]) -> StdResult<Vec<u8>> {}

    fn curve_hash(&self, input: &[u8]) -> StdResult<Vec<u8>> {}

    fn secp256k1_recover_pubkey(
        &self,
        message_hash: &[u8],
        signature: &[u8],
        recover_param: u8,
    ) -> Result<Vec<u8>, RecoverPubkeyError> {}

    fn ed25519_verify(
        &self,
        message: &[u8],
        signature: &[u8],
        public_key: &[u8],
    ) -> Result<bool, VerificationError> {}

    fn ed25519_batch_verify(
        &self,
        messages: &[&[u8]],
        signatures: &[&[u8]],
        public_keys: &[&[u8]],
    ) -> Result<bool, VerificationError> {}

    fn debug(&self, message: &str) {}
}

/// Takes a pointer to a Region and reads the data into a String.
/// This is for trusted string sources only.
unsafe fn consume_string_region_written_by_vm(from: *mut Region) -> String {}

/// A stateless convenience wrapper around imports provided by the VM
pub struct ExternalQuerier {}

impl ExternalQuerier {
    pub fn new() -> ExternalQuerier {}
}

impl Querier for ExternalQuerier {
    fn raw_query(&self, bin_request: &[u8]) -> QuerierResult {}
}

#[cfg(feature = "abort")]
pub fn handle_panic(message: &str) {}
}
#[cfg(target_arch = "wasm32")]
mod memory {
use std::mem;
use std::vec::Vec;

/// Describes some data allocated in Wasm's linear memory.
/// A pointer to an instance of this can be returned over FFI boundaries.
///
/// This struct is crate internal since the cosmwasm-vm defines the same type independently.
#[repr(C)]
pub struct Region {
    /// The beginning of the region expressed as bytes from the beginning of the linear memory
    pub offset: u32,
    /// The number of bytes available in this region
    pub capacity: u32,
    /// The number of bytes used in this region
    pub length: u32,
}

/// Creates a memory region of capacity `size` and length 0. Returns a pointer to the Region.
/// This is the same as the `allocate` export, but designed to be called internally.
pub fn alloc(size: usize) -> *mut Region {}

/// Similar to alloc, but instead of creating a new vector it consumes an existing one and returns
/// a pointer to the Region (preventing the memory from being freed until explicitly called later).
///
/// The resulting Region has capacity = length, i.e. the buffer's capacity is ignored.
pub fn release_buffer(buffer: Vec<u8>) -> *mut Region {}

/// Return the data referenced by the Region and
/// deallocates the Region (and the vector when finished).
/// Warning: only use this when you are sure the caller will never use (or free) the Region later
///
/// # Safety
///
/// The ptr must refer to a valid Region, which was previously returned by alloc,
/// and not yet deallocated. This call will deallocate the Region and return an owner vector
/// to the caller containing the referenced data.
///
/// Naturally, calling this function twice on the same pointer will double deallocate data
/// and lead to a crash. Make sure to call it exactly once (either consuming the input in
/// the wasm code OR deallocating the buffer from the caller).
pub unsafe fn consume_region(ptr: *mut Region) -> Vec<u8> {}

/// Returns a box of a Region, which can be sent over a call to extern
/// note that this DOES NOT take ownership of the data, and we MUST NOT consume_region
/// the resulting data.
/// The Box must be dropped (with scope), but not the data
pub fn build_region(data: &[u8]) -> Box<Region> {}

fn build_region_from_components(offset: u32, capacity: u32, length: u32) -> Box<Region> {}

/// Returns the address of the optional Region as an offset in linear memory,
/// or zero if not present
#[cfg(feature = "iterator")]
pub fn get_optional_region_address(region: &Option<&Box<Region>>) -> u32 {}
}

#[cfg(target_arch = "wasm32")]
pub use crate::exports::{do_execute, do_instantiate, do_migrate, do_query, do_reply, do_sudo};
#[cfg(all(feature = "stargate", target_arch = "wasm32"))]
pub use crate::exports::{
    do_ibc_channel_close, do_ibc_channel_connect, do_ibc_channel_open, do_ibc_packet_ack,
    do_ibc_packet_receive, do_ibc_packet_timeout,
};
#[cfg(target_arch = "wasm32")]
pub use crate::imports::{ExternalApi, ExternalQuerier, ExternalStorage};

// Exposed for testing only
// Both unit tests and integration tests are compiled to native code, so everything in here does not need to compile to Wasm.
#[cfg(not(target_arch = "wasm32"))]
pub mod testing {
#![cfg(not(target_arch = "wasm32"))]

// Exposed for testing only
// Both unit tests and integration tests are compiled to native code, so everything in here does not need to compile to Wasm.

mod assertions {
use crate::{Decimal, Uint128};
use std::str::FromStr as _;

/// Asserts that two expressions are approximately equal to each other.
///
/// The `max_rel_diff` argument defines the maximum relative difference
/// of the `left` and `right` values.
///
/// On panic, this macro will print the values of the arguments and
/// the actual relative difference.
///
/// Like [`assert_eq!`], this macro has a second form, where a custom
/// panic message can be provided.
#[macro_export]
macro_rules! assert_approx_eq {
    ($left:expr, $right:expr, $max_rel_diff:expr $(,)?) => {{
        $crate::testing::assert_approx_eq_impl($left, $right, $max_rel_diff, None);
    }};
    ($left:expr, $right:expr, $max_rel_diff:expr, $($args:tt)+) => {{
        $crate::testing::assert_approx_eq_impl($left, $right, $max_rel_diff, Some(format!($($args)*)));
    }};
}

/// Implementation for the [`cosmwasm_std::assert_approx_eq`] macro. This does not provide any
/// stability guarantees and may change any time.
#[track_caller]
#[doc(hidden)]
pub fn assert_approx_eq_impl<U: Into<Uint128>>(
    left: U,
    right: U,
    max_rel_diff: &str,
    panic_msg: Option<String>,
) {}

#[cfg(test)]
mod tests {
    #[test]
    fn assert_approx() {}

    #[test]
    fn assert_approx_with_vars() {}

    #[test]
    #[should_panic(
        expected = "assertion failed: `(left ≈ right)`\nleft: 8\nright: 10\nrelative difference: 0.2\nmax allowed relative difference: 0.12\n"
    )]
    fn assert_approx_fail() {}

    #[test]
    #[should_panic(
        expected = "assertion failed: `(left ≈ right)`\nleft: 17\nright: 20\nrelative difference: 0.15\nmax allowed relative difference: 0.12\n: some extra info about the error: Foo(8)"
    )]
    fn assert_approx_with_custom_panic_msg() {}
}
}
mod mock {
use cosmwasm_crypto::Poseidon;
use serde::de::DeserializeOwned;
#[cfg(feature = "stargate")]
use serde::Serialize;
use std::collections::HashMap;
use std::marker::PhantomData;

use crate::addresses::{Addr, CanonicalAddr};
use crate::binary::Binary;
use crate::coin::Coin;
use crate::deps::OwnedDeps;
use crate::errors::{RecoverPubkeyError, StdError, StdResult, SystemError, VerificationError};
#[cfg(feature = "stargate")]
use crate::ibc::{
    IbcAcknowledgement, IbcChannel, IbcChannelCloseMsg, IbcChannelConnectMsg, IbcChannelOpenMsg,
    IbcEndpoint, IbcOrder, IbcPacket, IbcPacketAckMsg, IbcPacketReceiveMsg, IbcPacketTimeoutMsg,
    IbcTimeoutBlock,
};
use crate::math::Uint128;
#[cfg(feature = "cosmwasm_1_1")]
use crate::query::SupplyResponse;
use crate::query::{
    AllBalanceResponse, BalanceResponse, BankQuery, CustomQuery, QueryRequest, WasmQuery,
};
#[cfg(feature = "staking")]
use crate::query::{
    AllDelegationsResponse, AllValidatorsResponse, BondedDenomResponse, DelegationResponse,
    FullDelegation, StakingQuery, Validator, ValidatorResponse,
};
use crate::results::{ContractResult, Empty, SystemResult};
use crate::serde::{from_slice, to_binary};
use crate::storage::MemoryStorage;
use crate::timestamp::Timestamp;
use crate::traits::{Api, Querier, QuerierResult};
use crate::types::{BlockInfo, ContractInfo, Env, MessageInfo, TransactionInfo};
use crate::Attribute;

pub const MOCK_CONTRACT_ADDR: &str = "cosmos2contract";

/// Creates all external requirements that can be injected for unit tests.
///
/// See also [`mock_dependencies_with_balance`] and [`mock_dependencies_with_balances`]
/// if you want to start with some initial balances.
pub fn mock_dependencies() -> OwnedDeps<MockStorage, MockApi, MockQuerier, Empty> {}

/// Creates all external requirements that can be injected for unit tests.
///
/// It sets the given balance for the contract itself, nothing else.
pub fn mock_dependencies_with_balance(
    contract_balance: &[Coin],
) -> OwnedDeps<MockStorage, MockApi, MockQuerier, Empty> {}

/// Initializes the querier along with the mock_dependencies.
/// Sets all balances provided (you must explicitly set contract balance if desired).
pub fn mock_dependencies_with_balances(
    balances: &[(&str, &[Coin])],
) -> OwnedDeps<MockStorage, MockApi, MockQuerier> {}

// Use MemoryStorage implementation (which is valid in non-testcode)
// We can later make simplifications here if needed
pub type MockStorage = MemoryStorage;

/// Length of canonical addresses created with this API. Contracts should not make any assumtions
/// what this value is.
/// The value here must be restorable with `SHUFFLES_ENCODE` + `SHUFFLES_DECODE` in-shuffles.
const CANONICAL_LENGTH: usize = 54;

const SHUFFLES_ENCODE: usize = 18;
const SHUFFLES_DECODE: usize = 2;

// MockPrecompiles zero pads all human addresses to make them fit the canonical_length
// it trims off zeros for the reverse operation.
// not really smart, but allows us to see a difference (and consistent length for canonical adddresses)
#[derive(Clone)]
pub struct MockApi {
    /// Length of canonical addresses created with this API. Contracts should not make any assumtions
    /// what this value is.
    canonical_length: usize,
    poseidon: Poseidon,
}

impl Default for MockApi {
    fn default() -> Self {}
}

impl Api for MockApi {
    fn addr_validate(&self, input: &str) -> StdResult<Addr> {}

    fn addr_canonicalize(&self, input: &str) -> StdResult<CanonicalAddr> {}

    fn addr_humanize(&self, canonical: &CanonicalAddr) -> StdResult<Addr> {}

    fn secp256k1_verify(
        &self,
        message_hash: &[u8],
        signature: &[u8],
        public_key: &[u8],
    ) -> Result<bool, VerificationError> {}

    fn poseidon_hash(&self, inputs: &[&[u8]]) -> StdResult<Vec<u8>> {}

    fn curve_hash(&self, input: &[u8]) -> StdResult<Vec<u8>> {}

    fn groth16_verify(
        &self,
        input: &[u8],
        proof: &[u8],
        vk: &[u8],
    ) -> Result<bool, VerificationError> {}

    fn secp256k1_recover_pubkey(
        &self,
        message_hash: &[u8],
        signature: &[u8],
        recovery_param: u8,
    ) -> Result<Vec<u8>, RecoverPubkeyError> {}

    fn ed25519_verify(
        &self,
        message: &[u8],
        signature: &[u8],
        public_key: &[u8],
    ) -> Result<bool, VerificationError> {}

    fn ed25519_batch_verify(
        &self,
        messages: &[&[u8]],
        signatures: &[&[u8]],
        public_keys: &[&[u8]],
    ) -> Result<bool, VerificationError> {}

    fn debug(&self, message: &str) {}
}

/// Returns a default enviroment with height, time, chain_id, and contract address
/// You can submit as is to most contracts, or modify height/time if you want to
/// test for expiration.
///
/// This is intended for use in test code only.
pub fn mock_env() -> Env {}

/// Just set sender and funds for the message.
/// This is intended for use in test code only.
pub fn mock_info(sender: &str, funds: &[Coin]) -> MessageInfo {}

/// Creates an IbcChannel for testing. You set a few key parameters for handshaking,
/// If you want to set more, use this as a default and mutate other fields
#[cfg(feature = "stargate")]
pub fn mock_ibc_channel(my_channel_id: &str, order: IbcOrder, version: &str) -> IbcChannel {}

/// Creates a IbcChannelOpenMsg::OpenInit for testing ibc_channel_open.
#[cfg(feature = "stargate")]
pub fn mock_ibc_channel_open_init(
    my_channel_id: &str,
    order: IbcOrder,
    version: &str,
) -> IbcChannelOpenMsg {}

/// Creates a IbcChannelOpenMsg::OpenTry for testing ibc_channel_open.
#[cfg(feature = "stargate")]
pub fn mock_ibc_channel_open_try(
    my_channel_id: &str,
    order: IbcOrder,
    version: &str,
) -> IbcChannelOpenMsg {}

/// Creates a IbcChannelConnectMsg::ConnectAck for testing ibc_channel_connect.
#[cfg(feature = "stargate")]
pub fn mock_ibc_channel_connect_ack(
    my_channel_id: &str,
    order: IbcOrder,
    version: &str,
) -> IbcChannelConnectMsg {}

/// Creates a IbcChannelConnectMsg::ConnectConfirm for testing ibc_channel_connect.
#[cfg(feature = "stargate")]
pub fn mock_ibc_channel_connect_confirm(
    my_channel_id: &str,
    order: IbcOrder,
    version: &str,
) -> IbcChannelConnectMsg {}

/// Creates a IbcChannelCloseMsg::CloseInit for testing ibc_channel_close.
#[cfg(feature = "stargate")]
pub fn mock_ibc_channel_close_init(
    my_channel_id: &str,
    order: IbcOrder,
    version: &str,
) -> IbcChannelCloseMsg {}

/// Creates a IbcChannelCloseMsg::CloseConfirm for testing ibc_channel_close.
#[cfg(feature = "stargate")]
pub fn mock_ibc_channel_close_confirm(
    my_channel_id: &str,
    order: IbcOrder,
    version: &str,
) -> IbcChannelCloseMsg {}

/// Creates a IbcPacketReceiveMsg for testing ibc_packet_receive. You set a few key parameters that are
/// often parsed. If you want to set more, use this as a default and mutate other fields
#[cfg(feature = "stargate")]
pub fn mock_ibc_packet_recv(
    my_channel_id: &str,
    data: &impl Serialize,
) -> StdResult<IbcPacketReceiveMsg> {}

/// Creates a IbcPacket for testing ibc_packet_{ack,timeout}. You set a few key parameters that are
/// often parsed. If you want to set more, use this as a default and mutate other fields.
/// The difference from mock_ibc_packet_recv is if `my_channel_id` is src or dest.
#[cfg(feature = "stargate")]
fn mock_ibc_packet(my_channel_id: &str, data: &impl Serialize) -> StdResult<IbcPacket> {}

/// Creates a IbcPacketAckMsg for testing ibc_packet_ack. You set a few key parameters that are
/// often parsed. If you want to set more, use this as a default and mutate other fields.
/// The difference from mock_ibc_packet_recv is if `my_channel_id` is src or dest.
#[cfg(feature = "stargate")]
pub fn mock_ibc_packet_ack(
    my_channel_id: &str,
    data: &impl Serialize,
    ack: IbcAcknowledgement,
) -> StdResult<IbcPacketAckMsg> {}

/// Creates a IbcPacketTimeoutMsg for testing ibc_packet_timeout. You set a few key parameters that are
/// often parsed. If you want to set more, use this as a default and mutate other fields.
/// The difference from mock_ibc_packet_recv is if `my_channel_id` is src or dest./
#[cfg(feature = "stargate")]
pub fn mock_ibc_packet_timeout(
    my_channel_id: &str,
    data: &impl Serialize,
) -> StdResult<IbcPacketTimeoutMsg> {}

/// The same type as cosmwasm-std's QuerierResult, but easier to reuse in
/// cosmwasm-vm. It might diverge from QuerierResult at some point.
pub type MockQuerierCustomHandlerResult = SystemResult<ContractResult<Binary>>;

/// MockQuerier holds an immutable table of bank balances
/// and configurable handlers for Wasm queries and custom queries.
pub struct MockQuerier<C: DeserializeOwned = Empty> {
    bank: BankQuerier,
    #[cfg(feature = "staking")]
    staking: StakingQuerier,
    wasm: WasmQuerier,
    /// A handler to handle custom queries. This is set to a dummy handler that
    /// always errors by default. Update it via `with_custom_handler`.
    ///
    /// Use box to avoid the need of another generic type
    custom_handler: Box<dyn for<'a> Fn(&'a C) -> MockQuerierCustomHandlerResult>,
}

impl<C: DeserializeOwned> MockQuerier<C> {
    pub fn new(balances: &[(&str, &[Coin])]) -> Self {}

    // set a new balance for the given address and return the old balance
    pub fn update_balance(
        &mut self,
        addr: impl Into<String>,
        balance: Vec<Coin>,
    ) -> Option<Vec<Coin>> {}

    #[cfg(feature = "staking")]
    pub fn update_staking(
        &mut self,
        denom: &str,
        validators: &[crate::query::Validator],
        delegations: &[crate::query::FullDelegation],
    ) {}

    pub fn update_wasm<WH: 'static>(&mut self, handler: WH)
    where
        WH: Fn(&WasmQuery) -> QuerierResult,
    {}

    pub fn with_custom_handler<CH: 'static>(mut self, handler: CH) -> Self
    where
        CH: Fn(&C) -> MockQuerierCustomHandlerResult,
    {}
}

impl Default for MockQuerier {
    fn default() -> Self {}
}

impl<C: CustomQuery + DeserializeOwned> Querier for MockQuerier<C> {
    fn raw_query(&self, bin_request: &[u8]) -> QuerierResult {}
}

impl<C: CustomQuery + DeserializeOwned> MockQuerier<C> {
    pub fn handle_query(&self, request: &QueryRequest<C>) -> QuerierResult {}
}

struct WasmQuerier {
    /// A handler to handle Wasm queries. This is set to a dummy handler that
    /// always errors by default. Update it via `with_custom_handler`.
    ///
    /// Use box to avoid the need of generic type.
    handler: Box<dyn for<'a> Fn(&'a WasmQuery) -> QuerierResult>,
}

impl WasmQuerier {
    fn new(handler: Box<dyn for<'a> Fn(&'a WasmQuery) -> QuerierResult>) -> Self {}

    fn update_handler<WH: 'static>(&mut self, handler: WH)
    where
        WH: Fn(&WasmQuery) -> QuerierResult,
    {}

    fn query(&self, request: &WasmQuery) -> QuerierResult {}
}

impl Default for WasmQuerier {
    fn default() -> Self {}
}

#[derive(Clone, Default)]
pub struct BankQuerier {
    #[allow(dead_code)]
    /// HashMap<denom, amount>
    supplies: HashMap<String, Uint128>,
    /// HashMap<address, coins>
    balances: HashMap<String, Vec<Coin>>,
}

impl BankQuerier {
    pub fn new(balances: &[(&str, &[Coin])]) -> Self {}

    pub fn update_balance(
        &mut self,
        addr: impl Into<String>,
        balance: Vec<Coin>,
    ) -> Option<Vec<Coin>> {}

    fn calculate_supplies(balances: &HashMap<String, Vec<Coin>>) -> HashMap<String, Uint128> {}

    pub fn query(&self, request: &BankQuery) -> QuerierResult {}
}

#[cfg(feature = "staking")]
#[derive(Clone, Default)]
pub struct StakingQuerier {
    denom: String,
    validators: Vec<Validator>,
    delegations: Vec<FullDelegation>,
}

#[cfg(feature = "staking")]
impl StakingQuerier {
    pub fn new(denom: &str, validators: &[Validator], delegations: &[FullDelegation]) -> Self {}

    pub fn query(&self, request: &StakingQuery) -> QuerierResult {}
}

/// Performs a perfect shuffle (in shuffle)
///
/// https://en.wikipedia.org/wiki/Riffle_shuffle_permutation#Perfect_shuffles
/// https://en.wikipedia.org/wiki/In_shuffle
///
/// The number of shuffles required to restore the original order are listed in
/// https://oeis.org/A002326, e.g.:
///
/// ```ignore
/// 2: 2
/// 4: 4
/// 6: 3
/// 8: 6
/// 10: 10
/// 12: 12
/// 14: 4
/// 16: 8
/// 18: 18
/// 20: 6
/// 22: 11
/// 24: 20
/// 26: 18
/// 28: 28
/// 30: 5
/// 32: 10
/// 34: 12
/// 36: 36
/// 38: 12
/// 40: 20
/// 42: 14
/// 44: 12
/// 46: 23
/// 48: 21
/// 50: 8
/// 52: 52
/// 54: 20
/// 56: 18
/// 58: 58
/// 60: 60
/// 62: 6
/// 64: 12
/// 66: 66
/// 68: 22
/// 70: 35
/// 72: 9
/// 74: 20
/// ```
pub fn riffle_shuffle<T: Clone>(input: &[T]) -> Vec<T> {}

pub fn digit_sum(input: &[u8]) -> usize {}

/// Only for test code. This bypasses assertions in new, allowing us to create _*
/// Attributes to simulate responses from the blockchain
pub fn mock_wasmd_attr(key: impl Into<String>, value: impl Into<String>) -> Attribute {}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{coin, coins, from_binary, to_binary, ContractInfoResponse, Response};
    #[cfg(feature = "staking")]
    use crate::{Decimal, Delegation};
    use hex_literal::hex;
    use serde::Deserialize;

    const SECP256K1_MSG_HASH_HEX: &str =
        "5ae8317d34d1e595e3fa7247db80c0af4320cce1116de187f8f7e2e099c0d8d0";
    const SECP256K1_SIG_HEX: &str = "207082eb2c3dfa0b454e0906051270ba4074ac93760ba9e7110cd9471475111151eb0dbbc9920e72146fb564f99d039802bf6ef2561446eb126ef364d21ee9c4";
    const SECP256K1_PUBKEY_HEX: &str = "04051c1ee2190ecfb174bfe4f90763f2b4ff7517b70a2aec1876ebcfd644c4633fb03f3cfbd94b1f376e34592d9d41ccaf640bb751b00a1fadeb0c01157769eb73";

    const ED25519_MSG_HEX: &str = "72";
    const ED25519_SIG_HEX: &str = "92a009a9f0d4cab8720e820b5f642540a2b27b5416503f8fb3762223ebdb69da085ac1e43e15996e458f3613d0f11d8c387b2eaeb4302aeeb00d291612bb0c00";
    const ED25519_PUBKEY_HEX: &str =
        "3d4017c3e843895a92b70aa74d1b7ebc9c982ccf2ec4968cc0cd55f12af4660c";

    #[test]
    fn mock_info_works() {}

    #[test]
    fn addr_validate_works() {}

    #[test]
    fn addr_canonicalize_works() {}

    #[test]
    fn canonicalize_and_humanize_restores_original() {}

    #[test]
    #[should_panic(expected = "address too short")]
    fn addr_canonicalize_min_input_length() {}

    #[test]
    #[should_panic(expected = "address too long")]
    fn addr_canonicalize_max_input_length() {}

    #[test]
    #[should_panic(expected = "length not correct")]
    fn addr_humanize_input_length() {}

    // Basic "works" test. Exhaustive tests on VM's side (packages/vm/src/imports.rs)
    #[test]
    fn secp256k1_verify_works() {}

    // Basic "fails" test. Exhaustive tests on VM's side (packages/vm/src/imports.rs)
    #[test]
    fn secp256k1_verify_fails() {}

    // Basic "errors" test. Exhaustive tests on VM's side (packages/vm/src/imports.rs)
    #[test]
    fn secp256k1_verify_errs() {}

    #[test]
    fn secp256k1_recover_pubkey_works() {}

    #[test]
    fn secp256k1_recover_pubkey_fails_for_wrong_recovery_param() {}

    #[test]
    fn secp256k1_recover_pubkey_fails_for_wrong_hash() {}

    // Basic "works" test. Exhaustive tests on VM's side (packages/vm/src/imports.rs)
    #[test]
    fn ed25519_verify_works() {}

    // Basic "fails" test. Exhaustive tests on VM's side (packages/vm/src/imports.rs)
    #[test]
    fn ed25519_verify_fails() {}

    // Basic "errors" test. Exhaustive tests on VM's side (packages/vm/src/imports.rs)
    #[test]
    fn ed25519_verify_errs() {}

    // Basic "works" test.
    #[test]
    fn ed25519_batch_verify_works() {}

    // Basic "fails" test.
    #[test]
    fn ed25519_batch_verify_fails() {}

    // Basic "errors" test.
    #[test]
    fn ed25519_batch_verify_errs() {}

    #[cfg(feature = "cosmwasm_1_1")]
    #[test]
    fn bank_querier_supply() {}

    #[test]
    fn bank_querier_all_balances() {}

    #[test]
    fn bank_querier_one_balance() {}

    #[test]
    fn bank_querier_missing_account() {}

    #[cfg(feature = "staking")]
    #[test]
    fn staking_querier_all_validators() {}

    #[cfg(feature = "staking")]
    #[test]
    fn staking_querier_validator() {}

    #[cfg(feature = "staking")]
    // gets delegators from query or panic
    fn get_all_delegators(
        staking: &StakingQuerier,
        delegator: impl Into<String>,
    ) -> Vec<Delegation> {}

    #[cfg(feature = "staking")]
    // gets full delegators from query or panic
    fn get_delegator(
        staking: &StakingQuerier,
        delegator: impl Into<String>,
        validator: impl Into<String>,
    ) -> Option<FullDelegation> {}

    #[cfg(feature = "staking")]
    #[test]
    fn staking_querier_delegations() {}

    #[test]
    fn wasm_querier_works() {}

    #[test]
    fn riffle_shuffle_works() {}

    #[test]
    fn digit_sum_works() {}
}
}

pub use assertions::assert_approx_eq_impl;

#[cfg(feature = "staking")]
pub use mock::StakingQuerier;
pub use mock::{
    digit_sum, mock_dependencies, mock_dependencies_with_balance, mock_dependencies_with_balances,
    mock_env, mock_info, mock_wasmd_attr, riffle_shuffle, BankQuerier, MockApi, MockQuerier,
    MockQuerierCustomHandlerResult, MockStorage, MOCK_CONTRACT_ADDR,
};
#[cfg(feature = "stargate")]
pub use mock::{
    mock_ibc_channel, mock_ibc_channel_close_confirm, mock_ibc_channel_close_init,
    mock_ibc_channel_connect_ack, mock_ibc_channel_connect_confirm, mock_ibc_channel_open_init,
    mock_ibc_channel_open_try, mock_ibc_packet_ack, mock_ibc_packet_recv, mock_ibc_packet_timeout,
};
}

// Re-exports

pub use cosmwasm_derive::entry_point;
