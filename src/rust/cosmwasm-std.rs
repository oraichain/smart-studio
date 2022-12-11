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
    pub fn unchecked(input: impl Into<String>) -> Addr {
        Addr(input.into())
    }

    #[inline]
    pub fn as_str(&self) -> &str {
        self.0.as_str()
    }

    /// Returns the UTF-8 encoded address string as a byte array.
    ///
    /// This is equivalent to `address.as_str().as_bytes()`.
    #[inline]
    pub fn as_bytes(&self) -> &[u8] {
        self.0.as_bytes()
    }

    /// Utility for explicit conversion to `String`.
    #[inline]
    pub fn into_string(self) -> String {
        self.0
    }
}

impl fmt::Display for Addr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", &self.0)
    }
}

impl AsRef<str> for Addr {
    #[inline]
    fn as_ref(&self) -> &str {
        self.as_str()
    }
}

/// Implement `Addr == &str`
impl PartialEq<&str> for Addr {
    fn eq(&self, rhs: &&str) -> bool {
        self.0 == *rhs
    }
}

/// Implement `&str == Addr`
impl PartialEq<Addr> for &str {
    fn eq(&self, rhs: &Addr) -> bool {
        *self == rhs.0
    }
}

/// Implement `Addr == String`
impl PartialEq<String> for Addr {
    fn eq(&self, rhs: &String) -> bool {
        &self.0 == rhs
    }
}

/// Implement `String == Addr`
impl PartialEq<Addr> for String {
    fn eq(&self, rhs: &Addr) -> bool {
        self == &rhs.0
    }
}

// Addr->String is a safe conversion.
// However, the opposite direction is unsafe and must not be implemented.

impl From<Addr> for String {
    fn from(addr: Addr) -> Self {
        addr.0
    }
}

impl From<&Addr> for String {
    fn from(addr: &Addr) -> Self {
        addr.0.clone()
    }
}

impl From<Addr> for Cow<'_, Addr> {
    fn from(addr: Addr) -> Self {
        Cow::Owned(addr)
    }
}

impl<'a> From<&'a Addr> for Cow<'a, Addr> {
    fn from(addr: &'a Addr) -> Self {
        Cow::Borrowed(addr)
    }
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
    fn eq(&self, rhs: &Binary) -> bool {
        &self.0 == rhs
    }
}

/// Implement `Binary == CanonicalAddr`
impl PartialEq<CanonicalAddr> for Binary {
    fn eq(&self, rhs: &CanonicalAddr) -> bool {
        self == &rhs.0
    }
}

/// Implement `CanonicalAddr == HexBinary`
impl PartialEq<HexBinary> for CanonicalAddr {
    fn eq(&self, rhs: &HexBinary) -> bool {
        self.as_slice() == rhs.as_slice()
    }
}

/// Implement `HexBinary == CanonicalAddr`
impl PartialEq<CanonicalAddr> for HexBinary {
    fn eq(&self, rhs: &CanonicalAddr) -> bool {
        self.as_slice() == rhs.0.as_slice()
    }
}

impl From<&[u8]> for CanonicalAddr {
    fn from(source: &[u8]) -> Self {
        Self(source.into())
    }
}

// Array reference
impl<const LENGTH: usize> From<&[u8; LENGTH]> for CanonicalAddr {
    fn from(source: &[u8; LENGTH]) -> Self {
        Self(source.into())
    }
}

// Owned array
impl<const LENGTH: usize> From<[u8; LENGTH]> for CanonicalAddr {
    fn from(source: [u8; LENGTH]) -> Self {
        Self(source.into())
    }
}

// Owned vector -> CanonicalAddr
impl From<Vec<u8>> for CanonicalAddr {
    fn from(source: Vec<u8>) -> Self {
        Self(source.into())
    }
}

// CanonicalAddr -> Owned vector
impl From<CanonicalAddr> for Vec<u8> {
    fn from(source: CanonicalAddr) -> Vec<u8> {
        source.0.into()
    }
}

// Owned Binary -> CanonicalAddr
impl From<Binary> for CanonicalAddr {
    fn from(source: Binary) -> Self {
        Self(source)
    }
}

// CanonicalAddr -> Owned Binary
impl From<CanonicalAddr> for Binary {
    fn from(source: CanonicalAddr) -> Binary {
        source.0
    }
}

// Owned HexBinary -> CanonicalAddr
impl From<HexBinary> for CanonicalAddr {
    fn from(source: HexBinary) -> Self {
        Self(source.into())
    }
}

// CanonicalAddr -> Owned HexBinary
impl From<CanonicalAddr> for HexBinary {
    fn from(source: CanonicalAddr) -> HexBinary {
        source.0.into()
    }
}

/// Just like Vec<u8>, CanonicalAddr is a smart pointer to [u8].
/// This implements `*canonical_address` for us and allows us to
/// do `&*canonical_address`, returning a `&[u8]` from a `&CanonicalAddr`.
/// With [deref coercions](https://doc.rust-lang.org/1.22.1/book/first-edition/deref-coercions.html#deref-coercions),
/// this allows us to use `&canonical_address` whenever a `&[u8]` is required.
impl Deref for CanonicalAddr {
    type Target = [u8];

    fn deref(&self) -> &Self::Target {
        self.as_slice()
    }
}

impl CanonicalAddr {
    pub fn as_slice(&self) -> &[u8] {
        self.0.as_slice()
    }
}

impl fmt::Display for CanonicalAddr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for byte in self.0.as_slice() {
            write!(f, "{:02X}", byte)?;
        }
        Ok(())
    }
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
/// ) -> Result<Response, StdError> {
///     let canonical_creator = deps.api.addr_canonicalize(env.contract.address.as_str())?;
///     let checksum = HexBinary::from_hex("9af782a3a1bcbcd22dbb6a45c751551d9af782a3a1bcbcd22dbb6a45c751551d")?;
///     let salt = b"instance 1231";
///     let canonical_addr = instantiate2_address(&checksum, &canonical_creator, salt, None)
///         .map_err(|_| StdError::generic_err("Could not calculate addr"))?;
///     let addr = deps.api.addr_humanize(&canonical_addr)?;
///
/// #   Ok(Default::default())
/// }
/// ```
pub fn instantiate2_address(
    checksum: &[u8],
    creator: &CanonicalAddr,
    salt: &[u8],
    msg: Option<&[u8]>,
) -> Result<CanonicalAddr, Instantiate2AddressError> {
    if checksum.len() != 32 {
        return Err(Instantiate2AddressError::InvalidChecksumLength);
    }

    if salt.is_empty() || salt.len() > 64 {
        return Err(Instantiate2AddressError::InvalidSaltLength);
    };

    let msg = msg.unwrap_or_default();

    let mut key = Vec::<u8>::new();
    key.extend_from_slice(b"wasm\0");
    key.extend_from_slice(&(checksum.len() as u64).to_be_bytes());
    key.extend_from_slice(checksum);
    key.extend_from_slice(&(creator.len() as u64).to_be_bytes());
    key.extend_from_slice(creator);
    key.extend_from_slice(&(salt.len() as u64).to_be_bytes());
    key.extend_from_slice(salt);
    key.extend_from_slice(&(msg.len() as u64).to_be_bytes());
    key.extend_from_slice(msg);
    let address_data = hash("module", &key);
    Ok(address_data.into())
}

/// The "Basic Address" Hash from
/// https://github.com/cosmos/cosmos-sdk/blob/v0.45.8/docs/architecture/adr-028-public-key-addresses.md
fn hash(ty: &str, key: &[u8]) -> Vec<u8> {
    let inner = Sha256::digest(ty.as_bytes());
    Sha256::new().chain(inner).chain(key).finalize().to_vec()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::HexBinary;
    use hex_literal::hex;
    use std::collections::hash_map::DefaultHasher;
    use std::collections::HashSet;
    use std::hash::{Hash, Hasher};

    #[test]
    fn addr_unchecked_works() {
        let a = Addr::unchecked("123");
        let aa = Addr::unchecked(String::from("123"));
        let b = Addr::unchecked("be");
        assert_eq!(a, aa);
        assert_ne!(a, b);
    }

    #[test]
    fn addr_as_str_works() {
        let addr = Addr::unchecked("literal-string");
        assert_eq!(addr.as_str(), "literal-string");
    }

    #[test]
    fn addr_as_bytes_works() {
        let addr = Addr::unchecked("literal-string");
        assert_eq!(
            addr.as_bytes(),
            [108, 105, 116, 101, 114, 97, 108, 45, 115, 116, 114, 105, 110, 103]
        );
    }

    #[test]
    fn addr_implements_display() {
        let addr = Addr::unchecked("cos934gh9034hg04g0h134");
        let embedded = format!("Address: {}", addr);
        assert_eq!(embedded, "Address: cos934gh9034hg04g0h134");
        assert_eq!(addr.to_string(), "cos934gh9034hg04g0h134");
    }

    #[test]
    fn addr_implements_as_ref_for_str() {
        let addr = Addr::unchecked("literal-string");
        assert_eq!(addr.as_ref(), "literal-string");
    }

    #[test]
    fn addr_implements_partial_eq_with_str() {
        let addr = Addr::unchecked("cos934gh9034hg04g0h134");

        // `Addr == &str`
        assert_eq!(addr, "cos934gh9034hg04g0h134");
        // `&str == Addr`
        assert_eq!("cos934gh9034hg04g0h134", addr);
    }

    #[test]
    fn addr_implements_partial_eq_with_string() {
        let addr = Addr::unchecked("cos934gh9034hg04g0h134");

        // `Addr == String`
        assert_eq!(addr, String::from("cos934gh9034hg04g0h134"));
        // `String == Addr`
        assert_eq!(String::from("cos934gh9034hg04g0h134"), addr);
    }

    #[test]
    fn addr_implements_into_string() {
        // owned Addr
        let addr = Addr::unchecked("cos934gh9034hg04g0h134");
        let string: String = addr.into();
        assert_eq!(string, "cos934gh9034hg04g0h134");

        // &Addr
        let addr = Addr::unchecked("cos934gh9034hg04g0h134");
        let addr_ref = &addr;
        let string: String = addr_ref.into();
        assert_eq!(string, "cos934gh9034hg04g0h134");
    }

    // Test CanonicalAddr as_slice() for each CanonicalAddr::from input type
    #[test]
    fn canonical_addr_from_slice() {
        // slice
        let bytes: &[u8] = &[0u8, 187, 61, 11, 250, 0];
        let canonical_addr_slice = CanonicalAddr::from(bytes);
        assert_eq!(canonical_addr_slice.as_slice(), &[0u8, 187, 61, 11, 250, 0]);

        // Vector
        let bytes: Vec<u8> = vec![0u8, 187, 61, 11, 250, 0];
        let canonical_addr_vec = CanonicalAddr::from(bytes);
        assert_eq!(canonical_addr_vec.as_slice(), &[0u8, 187, 61, 11, 250, 0]);
    }

    #[test]
    fn canonical_addr_implements_partial_eq_with_binary() {
        let addr = CanonicalAddr::from([1, 2, 3]);
        let bin1 = Binary::from([1, 2, 3]);
        let bin2 = Binary::from([42, 43]);

        assert_eq!(addr, bin1);
        assert_eq!(bin1, addr);
        assert_ne!(addr, bin2);
        assert_ne!(bin2, addr);
    }

    #[test]
    fn canonical_addr_implements_partial_eq_with_hex_binary() {
        let addr = CanonicalAddr::from([1, 2, 3]);
        let bin1 = HexBinary::from([1, 2, 3]);
        let bin2 = HexBinary::from([42, 43]);

        assert_eq!(addr, bin1);
        assert_eq!(bin1, addr);
        assert_ne!(addr, bin2);
        assert_ne!(bin2, addr);
    }

    #[test]
    fn canonical_addr_implements_from_array() {
        let array = [1, 2, 3];
        let addr = CanonicalAddr::from(array);
        assert_eq!(addr.as_slice(), [1, 2, 3]);

        let array_ref = b"foo";
        let addr = CanonicalAddr::from(array_ref);
        assert_eq!(addr.as_slice(), [0x66, 0x6f, 0x6f]);
    }

    #[test]
    fn canonical_addr_implements_from_and_to_vector() {
        // Into<CanonicalAddr> for Vec<u8>
        // This test is a bit pointless because we get Into from the From implementation
        let original = vec![0u8, 187, 61, 11, 250, 0];
        let original_ptr = original.as_ptr();
        let addr: CanonicalAddr = original.into();
        assert_eq!(addr.as_slice(), [0u8, 187, 61, 11, 250, 0]);
        assert_eq!((addr.0).0.as_ptr(), original_ptr, "must not be copied");

        // From<Vec<u8>> for CanonicalAddr
        let original = vec![0u8, 187, 61, 11, 250, 0];
        let original_ptr = original.as_ptr();
        let addr = CanonicalAddr::from(original);
        assert_eq!(addr.as_slice(), [0u8, 187, 61, 11, 250, 0]);
        assert_eq!((addr.0).0.as_ptr(), original_ptr, "must not be copied");

        // Into<Vec<u8>> for CanonicalAddr
        // This test is a bit pointless because we get Into from the From implementation
        let original = CanonicalAddr::from(vec![0u8, 187, 61, 11, 250, 0]);
        let original_ptr = (original.0).0.as_ptr();
        let vec: Vec<u8> = original.into();
        assert_eq!(vec.as_slice(), [0u8, 187, 61, 11, 250, 0]);
        assert_eq!(vec.as_ptr(), original_ptr, "must not be copied");

        // From<CanonicalAddr> for Vec<u8>
        let original = CanonicalAddr::from(vec![7u8, 35, 49, 101, 0, 255]);
        let original_ptr = (original.0).0.as_ptr();
        let vec = Vec::<u8>::from(original);
        assert_eq!(vec.as_slice(), [7u8, 35, 49, 101, 0, 255]);
        assert_eq!(vec.as_ptr(), original_ptr, "must not be copied");
    }

    #[test]
    fn canonical_addr_implements_from_and_to_binary() {
        // From<Binary> for CanonicalAddr
        let original = Binary::from([0u8, 187, 61, 11, 250, 0]);
        let original_ptr = original.as_ptr();
        let addr = CanonicalAddr::from(original);
        assert_eq!(addr.as_slice(), [0u8, 187, 61, 11, 250, 0]);
        assert_eq!((addr.0).0.as_ptr(), original_ptr, "must not be copied");

        // From<CanonicalAddr> for Binary
        let original = CanonicalAddr::from(vec![7u8, 35, 49, 101, 0, 255]);
        let original_ptr = (original.0).0.as_ptr();
        let bin = Binary::from(original);
        assert_eq!(bin.as_slice(), [7u8, 35, 49, 101, 0, 255]);
        assert_eq!(bin.as_ptr(), original_ptr, "must not be copied");
    }

    #[test]
    fn canonical_addr_implements_from_and_to_hex_binary() {
        // From<HexBinary> for CanonicalAddr
        let original = HexBinary::from([0u8, 187, 61, 11, 250, 0]);
        let original_ptr = original.as_ptr();
        let addr = CanonicalAddr::from(original);
        assert_eq!(addr.as_slice(), [0u8, 187, 61, 11, 250, 0]);
        assert_eq!((addr.0).0.as_ptr(), original_ptr, "must not be copied");

        // From<CanonicalAddr> for HexBinary
        let original = CanonicalAddr::from(vec![7u8, 35, 49, 101, 0, 255]);
        let original_ptr = (original.0).0.as_ptr();
        let bin = HexBinary::from(original);
        assert_eq!(bin.as_slice(), [7u8, 35, 49, 101, 0, 255]);
        assert_eq!(bin.as_ptr(), original_ptr, "must not be copied");
    }

    #[test]
    fn canonical_addr_len() {
        let bytes: &[u8] = &[0u8, 187, 61, 11, 250, 0];
        let canonical_addr = CanonicalAddr::from(bytes);
        assert_eq!(canonical_addr.len(), bytes.len());
    }

    #[test]
    fn canonical_addr_is_empty() {
        let bytes: &[u8] = &[0u8, 187, 61, 11, 250, 0];
        let canonical_addr = CanonicalAddr::from(bytes);
        assert!(!canonical_addr.is_empty());
        let empty_canonical_addr = CanonicalAddr::from(vec![]);
        assert!(empty_canonical_addr.is_empty());
    }

    #[test]
    fn canonical_addr_implements_display() {
        let bytes: &[u8] = &[
            0x12, // two hex digits
            0x03, // small values must be padded to two digits
            0xab, // ensure we get upper case
            0x00, // always test extreme values
            0xff,
        ];
        let address = CanonicalAddr::from(bytes);
        let embedded = format!("Address: {}", address);
        assert_eq!(embedded, "Address: 1203AB00FF");
        assert_eq!(address.to_string(), "1203AB00FF");
    }

    #[test]
    fn canonical_addr_implements_deref() {
        // Dereference to [u8]
        let bytes: &[u8] = &[0u8, 187, 61, 11, 250, 0];
        let canonical_addr = CanonicalAddr::from(bytes);
        assert_eq!(*canonical_addr, [0u8, 187, 61, 11, 250, 0]);

        // This checks deref coercions from &CanonicalAddr to &[u8] works
        let bytes: &[u8] = &[0u8, 187, 61, 11, 250, 0];
        let canonical_addr = CanonicalAddr::from(bytes);
        assert_eq!(canonical_addr.len(), 6);
        let canonical_addr_slice: &[u8] = &canonical_addr;
        assert_eq!(canonical_addr_slice, &[0u8, 187, 61, 11, 250, 0]);
    }

    #[test]
    fn canonical_addr_implements_hash() {
        let alice1 = CanonicalAddr::from([0, 187, 61, 11, 250, 0]);
        let mut hasher = DefaultHasher::new();
        alice1.hash(&mut hasher);
        let alice1_hash = hasher.finish();

        let alice2 = CanonicalAddr::from([0, 187, 61, 11, 250, 0]);
        let mut hasher = DefaultHasher::new();
        alice2.hash(&mut hasher);
        let alice2_hash = hasher.finish();

        let bob = CanonicalAddr::from([16, 21, 33, 0, 255, 9]);
        let mut hasher = DefaultHasher::new();
        bob.hash(&mut hasher);
        let bob_hash = hasher.finish();

        assert_eq!(alice1_hash, alice2_hash);
        assert_ne!(alice1_hash, bob_hash);
    }

    /// This requires Hash and Eq to be implemented
    #[test]
    fn canonical_addr_can_be_used_in_hash_set() {
        let alice1 = CanonicalAddr::from([0, 187, 61, 11, 250, 0]);
        let alice2 = CanonicalAddr::from([0, 187, 61, 11, 250, 0]);
        let bob = CanonicalAddr::from([16, 21, 33, 0, 255, 9]);

        let mut set = HashSet::new();
        set.insert(alice1.clone());
        set.insert(alice2.clone());
        set.insert(bob.clone());
        assert_eq!(set.len(), 2);

        let set1 = HashSet::<CanonicalAddr>::from_iter(vec![bob.clone(), alice1.clone()]);
        let set2 = HashSet::from_iter(vec![alice1, alice2, bob]);
        assert_eq!(set1, set2);
    }

    // helper to show we can handle Addr and &Addr equally
    fn flexible<'a>(a: impl Into<Cow<'a, Addr>>) -> String {
        a.into().into_owned().to_string()
    }

    #[test]
    fn addr_into_cow() {
        // owned Addr
        let value = "wasmeucn0ur0ncny2308ry";
        let addr = Addr::unchecked(value);

        // pass by ref
        assert_eq!(value, &flexible(&addr));
        // pass by value
        assert_eq!(value, &flexible(addr));
    }

    #[test]
    fn instantiate2_address_works() {
        let checksum1 =
            HexBinary::from_hex("13a1fc994cc6d1c81b746ee0c0ff6f90043875e0bf1d9be6b7d779fc978dc2a5")
                .unwrap();
        let creator1 = CanonicalAddr::from(hex!("9999999999aaaaaaaaaabbbbbbbbbbcccccccccc"));
        let salt1 = hex!("61");
        let salt2 = hex!("aabbccddeeffffeeddbbccddaa66551155aaaabbcc787878789900aabbccddeeffffeeddbbccddaa66551155aaaabbcc787878789900aabbbbcc221100acadae");
        let msg1: Option<&[u8]> = None;
        let msg2: Option<&[u8]> = Some(b"{}");
        let msg3: Option<&[u8]> = Some(b"{\"some\":123,\"structure\":{\"nested\":[\"ok\",true]}}");

        // No msg
        let expected = CanonicalAddr::from(hex!(
            "5e865d3e45ad3e961f77fd77d46543417ced44d924dc3e079b5415ff6775f847"
        ));
        assert_eq!(
            instantiate2_address(&checksum1, &creator1, &salt1, msg1).unwrap(),
            expected
        );

        // With msg
        let expected = CanonicalAddr::from(hex!(
            "0995499608947a5281e2c7ebd71bdb26a1ad981946dad57f6c4d3ee35de77835"
        ));
        assert_eq!(
            instantiate2_address(&checksum1, &creator1, &salt1, msg2).unwrap(),
            expected
        );

        // Long msg
        let expected = CanonicalAddr::from(hex!(
            "83326e554723b15bac664ceabc8a5887e27003abe9fbd992af8c7bcea4745167"
        ));
        assert_eq!(
            instantiate2_address(&checksum1, &creator1, &salt1, msg3).unwrap(),
            expected
        );

        // Long salt
        let expected = CanonicalAddr::from(hex!(
            "9384c6248c0bb171e306fd7da0993ec1e20eba006452a3a9e078883eb3594564"
        ));
        assert_eq!(
            instantiate2_address(&checksum1, &creator1, &salt2, None).unwrap(),
            expected
        );

        // Salt too short or too long
        let empty = Vec::<u8>::new();
        assert!(matches!(
            instantiate2_address(&checksum1, &creator1, &empty, None).unwrap_err(),
            Instantiate2AddressError::InvalidSaltLength
        ));
        let too_long = vec![0x11; 65];
        assert!(matches!(
            instantiate2_address(&checksum1, &creator1, &too_long, None).unwrap_err(),
            Instantiate2AddressError::InvalidSaltLength
        ));

        // invalid checksum length
        let broken_cs = hex!("13a1fc994cc6d1c81b746ee0c0ff6f90043875e0bf1d9be6b7d779fc978dc2");
        assert!(matches!(
            instantiate2_address(&broken_cs, &creator1, &salt1, None).unwrap_err(),
            Instantiate2AddressError::InvalidChecksumLength
        ));
        let broken_cs = hex!("");
        assert!(matches!(
            instantiate2_address(&broken_cs, &creator1, &salt1, None).unwrap_err(),
            Instantiate2AddressError::InvalidChecksumLength
        ));
        let broken_cs = hex!("13a1fc994cc6d1c81b746ee0c0ff6f90043875e0bf1d9be6b7d779fc978dc2aaaa");
        assert!(matches!(
            instantiate2_address(&broken_cs, &creator1, &salt1, None).unwrap_err(),
            Instantiate2AddressError::InvalidChecksumLength
        ));
    }

    #[test]
    fn instantiate2_address_works_for_cosmjs_testvectors() {
        // Test data from https://github.com/cosmos/cosmjs/pull/1253
        const COSMOS_ED25519_TESTS_JSON: &str = "./testdata/instantiate2_addresses.json";

        #[derive(Deserialize, Debug)]
        #[serde(rename_all = "camelCase")]
        #[allow(dead_code)]
        struct In {
            checksum: HexBinary,
            creator: String,
            creator_data: HexBinary,
            salt: HexBinary,
            msg: Option<String>,
        }

        #[derive(Deserialize, Debug)]
        #[serde(rename_all = "camelCase")]
        #[allow(dead_code)]
        struct Intermediate {
            key: HexBinary,
            address_data: HexBinary,
        }

        #[derive(Deserialize, Debug)]
        #[serde(rename_all = "camelCase")]
        #[allow(dead_code)]
        struct Out {
            address: String,
        }

        #[derive(Deserialize, Debug)]
        #[allow(dead_code)]
        struct Row {
            #[serde(rename = "in")]
            input: In,
            intermediate: Intermediate,
            out: Out,
        }

        fn read_tests() -> Vec<Row> {
            use std::fs::File;
            use std::io::BufReader;

            // Open the file in read-only mode with buffer.
            let file = File::open(COSMOS_ED25519_TESTS_JSON).unwrap();
            let reader = BufReader::new(file);

            serde_json::from_reader(reader).unwrap()
        }

        for Row {
            input,
            intermediate,
            out: _,
        } in read_tests()
        {
            let msg = input.msg.map(|msg| msg.into_bytes());
            let addr = instantiate2_address(
                &input.checksum,
                &input.creator_data.into(),
                &input.salt,
                msg.as_deref(),
            )
            .unwrap();
            assert_eq!(addr, intermediate.address_data);
        }
    }

    #[test]
    fn hash_works() {
        // Test case from https://github.com/cosmos/cosmos-sdk/blob/v0.47.0-alpha1/types/address/hash_test.go#L19-L24
        let expected = [
            195, 235, 23, 251, 9, 99, 177, 195, 81, 122, 182, 124, 36, 113, 245, 156, 76, 188, 221,
            83, 181, 192, 227, 82, 100, 177, 161, 133, 240, 160, 5, 25,
        ];
        assert_eq!(hash("1", &[1]), expected);
    }
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
/// # fn body() -> Result<(), ContractError> {
/// # let permissions = Permissions { delegate: true };
/// use cosmwasm_std::ensure;
/// ensure!(permissions.delegate, ContractError::DelegatePerm {});
///
/// // is the same as
///
/// if !permissions.delegate {
///   return Err(ContractError::DelegatePerm {});
/// }
/// # Ok(())
/// # }
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
/// # fn body() -> Result<(), ContractError> {
/// # let info = MessageInfo { sender: Addr::unchecked("foo"), funds: Vec::new() };
/// # let cfg = Config { admin: "foo".to_string() };
/// use cosmwasm_std::ensure_eq;
///
/// ensure_eq!(info.sender, cfg.admin, ContractError::Unauthorized {});
///
/// // is the same as
///
/// if info.sender != cfg.admin {
///   return Err(ContractError::Unauthorized {});
/// }
/// # Ok(())
/// # }
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
/// # fn body() -> Result<(), ContractError> {
/// # let voting_power = 123;
/// use cosmwasm_std::ensure_ne;
///
/// ensure_ne!(voting_power, 0, ContractError::NotAVoter {});
///
/// // is the same as
///
/// if voting_power != 0 {
///   return Err(ContractError::NotAVoter {});
/// }
/// # Ok(())
/// # }
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
    fn ensure_works() {
        fn check(a: usize, b: usize) -> Result<(), StdError> {
            ensure!(a == b, StdError::generic_err("foobar"));
            Ok(())
        }

        let err = check(5, 6).unwrap_err();
        assert!(matches!(err, StdError::GenericErr { .. }));

        check(5, 5).unwrap();
    }

    #[test]
    fn ensure_can_infer_error_type() {
        let check = |a, b| {
            ensure!(a == b, StdError::generic_err("foobar"));
            Ok(())
        };

        let err = check(5, 6).unwrap_err();
        assert!(matches!(err, StdError::GenericErr { .. }));

        check(5, 5).unwrap();
    }

    #[test]
    fn ensure_can_convert_into() {
        #[derive(Debug)]
        struct ContractError;

        impl From<StdError> for ContractError {
            fn from(_original: StdError) -> Self {
                ContractError
            }
        }

        fn check(a: usize, b: usize) -> Result<(), ContractError> {
            ensure!(a == b, StdError::generic_err("foobar"));
            Ok(())
        }

        let err = check(5, 6).unwrap_err();
        assert!(matches!(err, ContractError));

        check(5, 5).unwrap();
    }

    #[test]
    fn ensure_eq_works() {
        let check = |a, b| {
            ensure_eq!(a, b, StdError::generic_err("foobar"));
            Ok(())
        };

        let err = check("123", "456").unwrap_err();
        assert!(matches!(err, StdError::GenericErr { .. }));
        check("123", "123").unwrap();
    }

    #[test]
    fn ensure_eq_gets_precedence_right() {
        // If this was expanded to `true || false == false` we'd get equality.
        // It must be expanded to `(true || false) == false` and we expect inequality.

        #[allow(clippy::nonminimal_bool)]
        fn check() -> Result<(), StdError> {
            ensure_eq!(true || false, false, StdError::generic_err("foobar"));
            Ok(())
        }

        let _err = check().unwrap_err();
    }

    #[test]
    fn ensure_ne_works() {
        let check = |a, b| {
            ensure_ne!(a, b, StdError::generic_err("foobar"));
            Ok(())
        };

        let err = check("123", "123").unwrap_err();
        assert!(matches!(err, StdError::GenericErr { .. }));
        check("123", "456").unwrap();
    }

    #[test]
    fn ensure_ne_gets_precedence_right() {
        // If this was expanded to `true || false == false` we'd get equality.
        // It must be expanded to `(true || false) == false` and we expect inequality.

        #[allow(clippy::nonminimal_bool)]
        fn check() -> Result<(), StdError> {
            ensure_ne!(true || false, false, StdError::generic_err("foobar"));
            Ok(())
        }

        check().unwrap();
    }
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
    pub fn from_base64(encoded: &str) -> StdResult<Self> {
        let binary = base64::decode(encoded).map_err(StdError::invalid_base64)?;
        Ok(Binary(binary))
    }

    /// encode to base64 string (guaranteed to be success as we control the data inside).
    /// this returns normalized form (with trailing = if needed)
    pub fn to_base64(&self) -> String {
        base64::encode(&self.0)
    }

    pub fn as_slice(&self) -> &[u8] {
        self.0.as_slice()
    }

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
    pub fn to_array<const LENGTH: usize>(&self) -> StdResult<[u8; LENGTH]> {
        if self.len() != LENGTH {
            return Err(StdError::invalid_data_size(LENGTH, self.len()));
        }

        let mut out: [u8; LENGTH] = [0; LENGTH];
        out.copy_from_slice(&self.0);
        Ok(out)
    }
}

impl fmt::Display for Binary {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.to_base64())
    }
}

impl fmt::Debug for Binary {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Use an output inspired by tuples (https://doc.rust-lang.org/std/fmt/struct.Formatter.html#method.debug_tuple)
        // but with a custom implementation to avoid the need for an intemediate hex string.
        write!(f, "Binary(")?;
        for byte in self.0.iter() {
            write!(f, "{:02x}", byte)?;
        }
        write!(f, ")")?;
        Ok(())
    }
}

impl From<&[u8]> for Binary {
    fn from(binary: &[u8]) -> Self {
        Self(binary.to_vec())
    }
}

/// Just like Vec<u8>, Binary is a smart pointer to [u8].
/// This implements `*binary` for us and allows us to
/// do `&*binary`, returning a `&[u8]` from a `&Binary`.
/// With [deref coercions](https://doc.rust-lang.org/1.22.1/book/first-edition/deref-coercions.html#deref-coercions),
/// this allows us to use `&binary` whenever a `&[u8]` is required.
impl Deref for Binary {
    type Target = [u8];

    fn deref(&self) -> &Self::Target {
        self.as_slice()
    }
}

// Reference
impl<const LENGTH: usize> From<&[u8; LENGTH]> for Binary {
    fn from(source: &[u8; LENGTH]) -> Self {
        Self(source.to_vec())
    }
}

// Owned
impl<const LENGTH: usize> From<[u8; LENGTH]> for Binary {
    fn from(source: [u8; LENGTH]) -> Self {
        Self(source.into())
    }
}

impl From<Vec<u8>> for Binary {
    fn from(vec: Vec<u8>) -> Self {
        Self(vec)
    }
}

impl From<Binary> for Vec<u8> {
    fn from(original: Binary) -> Vec<u8> {
        original.0
    }
}

/// Implement `encoding::Binary == std::vec::Vec<u8>`
impl PartialEq<Vec<u8>> for Binary {
    fn eq(&self, rhs: &Vec<u8>) -> bool {
        // Use Vec<u8> == Vec<u8>
        self.0 == *rhs
    }
}

/// Implement `std::vec::Vec<u8> == encoding::Binary`
impl PartialEq<Binary> for Vec<u8> {
    fn eq(&self, rhs: &Binary) -> bool {
        // Use Vec<u8> == Vec<u8>
        *self == rhs.0
    }
}

/// Implement `Binary == &[u8]`
impl PartialEq<&[u8]> for Binary {
    fn eq(&self, rhs: &&[u8]) -> bool {
        // Use &[u8] == &[u8]
        self.as_slice() == *rhs
    }
}

/// Implement `&[u8] == Binary`
impl PartialEq<Binary> for &[u8] {
    fn eq(&self, rhs: &Binary) -> bool {
        // Use &[u8] == &[u8]
        *self == rhs.as_slice()
    }
}

/// Implement `Binary == &[u8; LENGTH]`
impl<const LENGTH: usize> PartialEq<&[u8; LENGTH]> for Binary {
    fn eq(&self, rhs: &&[u8; LENGTH]) -> bool {
        self.as_slice() == rhs.as_slice()
    }
}

/// Implement `&[u8; LENGTH] == Binary`
impl<const LENGTH: usize> PartialEq<Binary> for &[u8; LENGTH] {
    fn eq(&self, rhs: &Binary) -> bool {
        self.as_slice() == rhs.as_slice()
    }
}

/// Implement `Binary == [u8; LENGTH]`
impl<const LENGTH: usize> PartialEq<[u8; LENGTH]> for Binary {
    fn eq(&self, rhs: &[u8; LENGTH]) -> bool {
        self.as_slice() == rhs.as_slice()
    }
}

/// Implement `[u8; LENGTH] == Binary`
impl<const LENGTH: usize> PartialEq<Binary> for [u8; LENGTH] {
    fn eq(&self, rhs: &Binary) -> bool {
        self.as_slice() == rhs.as_slice()
    }
}

/// Serializes as a base64 string
impl Serialize for Binary {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: ser::Serializer,
    {
        serializer.serialize_str(&self.to_base64())
    }
}

/// Deserializes as a base64 string
impl<'de> Deserialize<'de> for Binary {
    fn deserialize<D>(deserializer: D) -> Result<Binary, D::Error>
    where
        D: Deserializer<'de>,
    {
        deserializer.deserialize_str(Base64Visitor)
    }
}

struct Base64Visitor;

impl<'de> de::Visitor<'de> for Base64Visitor {
    type Value = Binary;

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str("valid base64 encoded string")
    }

    fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
    where
        E: de::Error,
    {
        match Binary::from_base64(v) {
            Ok(binary) => Ok(binary),
            Err(_) => Err(E::custom(format!("invalid base64: {}", v))),
        }
    }
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
    fn encode_decode() {
        let binary: &[u8] = b"hello";
        let encoded = Binary::from(binary).to_base64();
        assert_eq!(8, encoded.len());
        let decoded = Binary::from_base64(&encoded).unwrap();
        assert_eq!(binary, decoded.as_slice());
    }

    #[test]
    fn encode_decode_non_ascii() {
        let binary = vec![12u8, 187, 0, 17, 250, 1];
        let encoded = Binary(binary.clone()).to_base64();
        assert_eq!(8, encoded.len());
        let decoded = Binary::from_base64(&encoded).unwrap();
        assert_eq!(binary.deref(), decoded.deref());
    }

    #[test]
    fn to_array_works() {
        // simple
        let binary = Binary::from(&[1, 2, 3]);
        let array: [u8; 3] = binary.to_array().unwrap();
        assert_eq!(array, [1, 2, 3]);

        // empty
        let binary = Binary::from(&[]);
        let array: [u8; 0] = binary.to_array().unwrap();
        assert_eq!(array, [] as [u8; 0]);

        // invalid size
        let binary = Binary::from(&[1, 2, 3]);
        let error = binary.to_array::<8>().unwrap_err();
        match error {
            StdError::InvalidDataSize {
                expected, actual, ..
            } => {
                assert_eq!(expected, 8);
                assert_eq!(actual, 3);
            }
            err => panic!("Unexpected error: {:?}", err),
        }

        // long array (32 bytes)
        let binary = Binary::from_base64("t119JOQox4WUQEmO/nyqOZfO+wjJm91YG2sfn4ZglvA=").unwrap();
        let array: [u8; 32] = binary.to_array().unwrap();
        assert_eq!(
            array,
            [
                0xb7, 0x5d, 0x7d, 0x24, 0xe4, 0x28, 0xc7, 0x85, 0x94, 0x40, 0x49, 0x8e, 0xfe, 0x7c,
                0xaa, 0x39, 0x97, 0xce, 0xfb, 0x08, 0xc9, 0x9b, 0xdd, 0x58, 0x1b, 0x6b, 0x1f, 0x9f,
                0x86, 0x60, 0x96, 0xf0,
            ]
        );

        // very long array > 32 bytes (requires Rust 1.47+)
        let binary =
            Binary::from_base64("t119JOQox4WUQEmO/nyqOZfO+wjJm91YG2sfn4ZglvBzyMOwMWq+").unwrap();
        let array: [u8; 39] = binary.to_array().unwrap();
        assert_eq!(
            array,
            [
                0xb7, 0x5d, 0x7d, 0x24, 0xe4, 0x28, 0xc7, 0x85, 0x94, 0x40, 0x49, 0x8e, 0xfe, 0x7c,
                0xaa, 0x39, 0x97, 0xce, 0xfb, 0x08, 0xc9, 0x9b, 0xdd, 0x58, 0x1b, 0x6b, 0x1f, 0x9f,
                0x86, 0x60, 0x96, 0xf0, 0x73, 0xc8, 0xc3, 0xb0, 0x31, 0x6a, 0xbe,
            ]
        );
    }

    #[test]
    fn from_valid_string() {
        let valid_base64 = "cmFuZG9taVo=";
        let binary = Binary::from_base64(valid_base64).unwrap();
        assert_eq!(b"randomiZ", binary.as_slice());
    }

    // this accepts input without a trailing = but outputs normal form
    #[test]
    fn from_shortened_string() {
        let short = "cmFuZG9taVo";
        let long = "cmFuZG9taVo=";
        let binary = Binary::from_base64(short).unwrap();
        assert_eq!(b"randomiZ", binary.as_slice());
        assert_eq!(long, binary.to_base64());
    }

    #[test]
    fn from_invalid_string() {
        let invalid_base64 = "cm%uZG9taVo";
        let res = Binary::from_base64(invalid_base64);
        match res.unwrap_err() {
            StdError::InvalidBase64 { msg, .. } => assert_eq!(msg, "Invalid byte 37, offset 2."),
            _ => panic!("Unexpected error type"),
        }
    }

    #[test]
    fn from_slice_works() {
        let original: &[u8] = &[0u8, 187, 61, 11, 250, 0];
        let binary: Binary = original.into();
        assert_eq!(binary.as_slice(), [0u8, 187, 61, 11, 250, 0]);
    }

    #[test]
    fn from_fixed_length_array_works() {
        let original = &[];
        let binary: Binary = original.into();
        assert_eq!(binary.len(), 0);

        let original = &[0u8];
        let binary: Binary = original.into();
        assert_eq!(binary.as_slice(), [0u8]);

        let original = &[0u8, 187, 61, 11, 250, 0];
        let binary: Binary = original.into();
        assert_eq!(binary.as_slice(), [0u8, 187, 61, 11, 250, 0]);

        let original = &[
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
            1, 1, 1,
        ];
        let binary: Binary = original.into();
        assert_eq!(
            binary.as_slice(),
            [
                1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                1, 1, 1, 1,
            ]
        );
    }

    #[test]
    fn from_owned_fixed_length_array_works() {
        let original = [];
        let binary: Binary = original.into();
        assert_eq!(binary.len(), 0);

        let original = [0u8];
        let binary: Binary = original.into();
        assert_eq!(binary.as_slice(), [0u8]);

        let original = [0u8, 187, 61, 11, 250, 0];
        let binary: Binary = original.into();
        assert_eq!(binary.as_slice(), [0u8, 187, 61, 11, 250, 0]);

        let original = [
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
            1, 1, 1,
        ];
        let binary: Binary = original.into();
        assert_eq!(
            binary.as_slice(),
            [
                1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                1, 1, 1, 1,
            ]
        );
    }

    #[test]
    fn from_literal_works() {
        let a: Binary = b"".into();
        assert_eq!(a.len(), 0);

        let a: Binary = b".".into();
        assert_eq!(a.len(), 1);

        let a: Binary = b"...".into();
        assert_eq!(a.len(), 3);

        let a: Binary = b"...............................".into();
        assert_eq!(a.len(), 31);

        let a: Binary = b"................................".into();
        assert_eq!(a.len(), 32);

        let a: Binary = b".................................".into();
        assert_eq!(a.len(), 33);
    }

    #[test]
    fn from_vec_works() {
        let original = vec![0u8, 187, 61, 11, 250, 0];
        let original_ptr = original.as_ptr();
        let binary: Binary = original.into();
        assert_eq!(binary.as_slice(), [0u8, 187, 61, 11, 250, 0]);
        assert_eq!(binary.0.as_ptr(), original_ptr, "vector must not be copied");
    }

    #[test]
    fn into_vec_works() {
        // Into<Vec<u8>> for Binary
        let original = Binary(vec![0u8, 187, 61, 11, 250, 0]);
        let original_ptr = original.0.as_ptr();
        let vec: Vec<u8> = original.into();
        assert_eq!(vec.as_slice(), [0u8, 187, 61, 11, 250, 0]);
        assert_eq!(vec.as_ptr(), original_ptr, "vector must not be copied");

        // From<Binary> for Vec<u8>
        let original = Binary(vec![7u8, 35, 49, 101, 0, 255]);
        let original_ptr = original.0.as_ptr();
        let vec = Vec::<u8>::from(original);
        assert_eq!(vec.as_slice(), [7u8, 35, 49, 101, 0, 255]);
        assert_eq!(vec.as_ptr(), original_ptr, "vector must not be copied");
    }

    #[test]
    fn serialization_works() {
        let binary = Binary(vec![0u8, 187, 61, 11, 250, 0]);

        let json = to_vec(&binary).unwrap();
        let deserialized: Binary = from_slice(&json).unwrap();

        assert_eq!(binary, deserialized);
    }

    #[test]
    fn deserialize_from_valid_string() {
        let b64_str = "ALs9C/oA";
        // this is the binary behind above string
        let expected = vec![0u8, 187, 61, 11, 250, 0];

        let serialized = to_vec(&b64_str).unwrap();
        let deserialized: Binary = from_slice(&serialized).unwrap();
        assert_eq!(expected, deserialized.as_slice());
    }

    #[test]
    fn deserialize_from_invalid_string() {
        let invalid_str = "**BAD!**";
        let serialized = to_vec(&invalid_str).unwrap();
        let res = from_slice::<Binary>(&serialized);
        assert!(res.is_err());
    }

    #[test]
    fn binary_implements_debug() {
        // Some data
        let binary = Binary(vec![0x07, 0x35, 0xAA, 0xcb, 0x00, 0xff]);
        assert_eq!(format!("{:?}", binary), "Binary(0735aacb00ff)",);

        // Empty
        let binary = Binary(vec![]);
        assert_eq!(format!("{:?}", binary), "Binary()",);
    }

    #[test]
    fn binary_implements_deref() {
        // Dereference to [u8]
        let binary = Binary(vec![7u8, 35, 49, 101, 0, 255]);
        assert_eq!(*binary, [7u8, 35, 49, 101, 0, 255]);

        // This checks deref coercions from &Binary to &[u8] works
        let binary = Binary(vec![7u8, 35, 49, 101, 0, 255]);
        assert_eq!(binary.len(), 6);
        let binary_slice: &[u8] = &binary;
        assert_eq!(binary_slice, &[7u8, 35, 49, 101, 0, 255]);
    }

    #[test]
    fn binary_implements_hash() {
        let a1 = Binary::from([0, 187, 61, 11, 250, 0]);
        let mut hasher = DefaultHasher::new();
        a1.hash(&mut hasher);
        let a1_hash = hasher.finish();

        let a2 = Binary::from([0, 187, 61, 11, 250, 0]);
        let mut hasher = DefaultHasher::new();
        a2.hash(&mut hasher);
        let a2_hash = hasher.finish();

        let b = Binary::from([16, 21, 33, 0, 255, 9]);
        let mut hasher = DefaultHasher::new();
        b.hash(&mut hasher);
        let b_hash = hasher.finish();

        assert_eq!(a1_hash, a2_hash);
        assert_ne!(a1_hash, b_hash);
    }

    /// This requires Hash and Eq to be implemented
    #[test]
    fn binary_can_be_used_in_hash_set() {
        let a1 = Binary::from([0, 187, 61, 11, 250, 0]);
        let a2 = Binary::from([0, 187, 61, 11, 250, 0]);
        let b = Binary::from([16, 21, 33, 0, 255, 9]);

        let mut set = HashSet::new();
        set.insert(a1.clone());
        set.insert(a2.clone());
        set.insert(b.clone());
        assert_eq!(set.len(), 2);

        let set1 = HashSet::<Binary>::from_iter(vec![b.clone(), a1.clone()]);
        let set2 = HashSet::from_iter(vec![a1, a2, b]);
        assert_eq!(set1, set2);
    }

    #[test]
    fn binary_implements_partial_eq_with_vector() {
        let a = Binary(vec![5u8; 3]);
        let b = vec![5u8; 3];
        let c = vec![9u8; 3];
        assert_eq!(a, b);
        assert_eq!(b, a);
        assert_ne!(a, c);
        assert_ne!(c, a);
    }

    #[test]
    fn binary_implements_partial_eq_with_slice_and_array() {
        let a = Binary(vec![0xAA, 0xBB]);

        // Slice: &[u8]
        assert_eq!(a, b"\xAA\xBB" as &[u8]);
        assert_eq!(b"\xAA\xBB" as &[u8], a);
        assert_ne!(a, b"\x11\x22" as &[u8]);
        assert_ne!(b"\x11\x22" as &[u8], a);

        // Array reference: &[u8; 2]
        assert_eq!(a, b"\xAA\xBB");
        assert_eq!(b"\xAA\xBB", a);
        assert_ne!(a, b"\x11\x22");
        assert_ne!(b"\x11\x22", a);

        // Array: [u8; 2]
        assert_eq!(a, [0xAA, 0xBB]);
        assert_eq!([0xAA, 0xBB], a);
        assert_ne!(a, [0x11, 0x22]);
        assert_ne!([0x11, 0x22], a);
    }
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
    pub fn new(amount: u128, denom: impl Into<String>) -> Self {
        Coin {
            amount: Uint128::new(amount),
            denom: denom.into(),
        }
    }
}

impl fmt::Display for Coin {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // We use the formatting without a space between amount and denom,
        // which is common in the Cosmos SDK ecosystem:
        // https://github.com/cosmos/cosmos-sdk/blob/v0.42.4/types/coin.go#L643-L645
        // For communication to end users, Coin needs to transformed anways (e.g. convert integer uatom to decimal ATOM).
        write!(f, "{}{}", self.amount, self.denom)
    }
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
pub fn coins(amount: u128, denom: impl Into<String>) -> Vec<Coin> {
    vec![coin(amount, denom)]
}

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
pub fn coin(amount: u128, denom: impl Into<String>) -> Coin {
    Coin::new(amount, denom)
}

/// has_coins returns true if the list of coins has at least the required amount
pub fn has_coins(coins: &[Coin], required: &Coin) -> bool {
    coins
        .iter()
        .find(|c| c.denom == required.denom)
        .map(|m| m.amount >= required.amount)
        .unwrap_or(false)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn coin_implements_display() {
        let a = Coin {
            amount: Uint128::new(123),
            denom: "ucosm".to_string(),
        };

        let embedded = format!("Amount: {}", a);
        assert_eq!(embedded, "Amount: 123ucosm");
        assert_eq!(a.to_string(), "123ucosm");
    }

    #[test]
    fn coin_works() {
        let a = coin(123, "ucosm");
        assert_eq!(
            a,
            Coin {
                amount: Uint128::new(123),
                denom: "ucosm".to_string()
            }
        );

        let zero = coin(0, "ucosm");
        assert_eq!(
            zero,
            Coin {
                amount: Uint128::new(0),
                denom: "ucosm".to_string()
            }
        );

        let string_denom = coin(42, String::from("ucosm"));
        assert_eq!(
            string_denom,
            Coin {
                amount: Uint128::new(42),
                denom: "ucosm".to_string()
            }
        );
    }

    #[test]
    fn coins_works() {
        let a = coins(123, "ucosm");
        assert_eq!(
            a,
            vec![Coin {
                amount: Uint128::new(123),
                denom: "ucosm".to_string()
            }]
        );

        let zero = coins(0, "ucosm");
        assert_eq!(
            zero,
            vec![Coin {
                amount: Uint128::new(0),
                denom: "ucosm".to_string()
            }]
        );

        let string_denom = coins(42, String::from("ucosm"));
        assert_eq!(
            string_denom,
            vec![Coin {
                amount: Uint128::new(42),
                denom: "ucosm".to_string()
            }]
        );
    }

    #[test]
    fn has_coins_matches() {
        let wallet = vec![coin(12345, "ETH"), coin(555, "BTC")];

        // less than same type
        assert!(has_coins(&wallet, &coin(777, "ETH")));
    }
}
}
mod conversion {
/// Converts an input of type usize to u32.
///
/// On 32 bit platforms such as wasm32 this is just a safe cast.
/// On other platforms the conversion panics for values larger than
/// `u32::MAX`.
#[inline]
pub fn force_to_u32(input: usize) -> u32 {
    #[cfg(target_pointer_width = "32")]
    {
        // usize = u32 on this architecture
        input as u32
    }
    #[cfg(not(target_pointer_width = "32"))]
    {
        input.try_into().expect("Input exceeds u32 range")
    }
}
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
    pub fn as_ref(&'_ self) -> Deps<'_, C> {
        Deps {
            storage: &self.storage,
            api: &self.api,
            querier: QuerierWrapper::new(&self.querier),
        }
    }

    pub fn as_mut(&'_ mut self) -> DepsMut<'_, C> {
        DepsMut {
            storage: &mut self.storage,
            api: &self.api,
            querier: QuerierWrapper::new(&self.querier),
        }
    }
}

impl<'a, C: CustomQuery> DepsMut<'a, C> {
    pub fn as_ref(&'_ self) -> Deps<'_, C> {
        Deps {
            storage: self.storage,
            api: self.api,
            querier: self.querier,
        }
    }

    pub fn branch(&'_ mut self) -> DepsMut<'_, C> {
        DepsMut {
            storage: self.storage,
            api: self.api,
            querier: self.querier,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::testing::{mock_dependencies, MockApi, MockQuerier, MockStorage};
    use serde::{Deserialize, Serialize};

    // ensure we can call these many times, eg. as sub-calls
    fn execute(mut deps: DepsMut) {
        execute2(deps.branch());
        query(deps.as_ref());
        execute2(deps.branch());
    }
    fn execute2(_deps: DepsMut) {}

    fn query(deps: Deps) {
        query2(deps);
        query2(deps);
    }
    fn query2(_deps: Deps) {}

    #[test]
    fn ensure_easy_reuse() {
        let mut deps = mock_dependencies();
        execute(deps.as_mut());
        query(deps.as_ref())
    }

    #[test]
    fn deps_implements_copy() {
        impl CustomQuery for u64 {}
        #[derive(Clone, Serialize, Deserialize)]
        struct MyQuery;
        impl CustomQuery for MyQuery {}

        // With C: Copy
        let owned = OwnedDeps::<_, _, _, u64> {
            storage: MockStorage::default(),
            api: MockApi::default(),
            querier: MockQuerier::<u64>::new(&[]),
            custom_query_type: PhantomData,
        };
        let deps: Deps<u64> = owned.as_ref();
        let _copy1 = deps;
        let _copy2 = deps;

        // Without C: Copy
        let owned = OwnedDeps::<_, _, _, MyQuery> {
            storage: MockStorage::default(),
            api: MockApi::default(),
            querier: MockQuerier::<MyQuery>::new(&[]),
            custom_query_type: PhantomData,
        };
        let deps: Deps<MyQuery> = owned.as_ref();
        let _copy1 = deps;
        let _copy2 = deps;
    }
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
    pub fn unknown_err(error_code: u32) -> Self {
        RecoverPubkeyError::UnknownErr {
            error_code,
            #[cfg(feature = "backtraces")]
            backtrace: Backtrace::capture(),
        }
    }
}

impl PartialEq<RecoverPubkeyError> for RecoverPubkeyError {
    fn eq(&self, rhs: &RecoverPubkeyError) -> bool {
        match self {
            RecoverPubkeyError::InvalidHashFormat => {
                matches!(rhs, RecoverPubkeyError::InvalidHashFormat)
            }
            RecoverPubkeyError::InvalidSignatureFormat => {
                matches!(rhs, RecoverPubkeyError::InvalidSignatureFormat)
            }
            RecoverPubkeyError::InvalidRecoveryParam => {
                matches!(rhs, RecoverPubkeyError::InvalidRecoveryParam)
            }
            RecoverPubkeyError::UnknownErr { error_code, .. } => {
                if let RecoverPubkeyError::UnknownErr {
                    error_code: rhs_error_code,
                    ..
                } = rhs
                {
                    error_code == rhs_error_code
                } else {
                    false
                }
            }
        }
    }
}

#[cfg(not(target_arch = "wasm32"))]
impl From<CryptoError> for RecoverPubkeyError {
    fn from(original: CryptoError) -> Self {
        match original {
            CryptoError::InvalidHashFormat { .. } => RecoverPubkeyError::InvalidHashFormat,
            CryptoError::InvalidPubkeyFormat { .. } => panic!("Conversion not supported"),
            CryptoError::InvalidSignatureFormat { .. } => {
                RecoverPubkeyError::InvalidSignatureFormat
            }
            CryptoError::GenericErr { .. } => RecoverPubkeyError::unknown_err(original.code()),
            CryptoError::InvalidRecoveryParam { .. } => RecoverPubkeyError::InvalidRecoveryParam,
            CryptoError::BatchErr { .. } => panic!("Conversion not supported"),
        }
    }
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
    pub fn verification_err(source: VerificationError) -> Self {
        StdError::VerificationErr {
            source,
            #[cfg(feature = "backtraces")]
            backtrace: Backtrace::capture(),
        }
    }

    pub fn recover_pubkey_err(source: RecoverPubkeyError) -> Self {
        StdError::RecoverPubkeyErr {
            source,
            #[cfg(feature = "backtraces")]
            backtrace: Backtrace::capture(),
        }
    }

    pub fn generic_err(msg: impl Into<String>) -> Self {
        StdError::GenericErr {
            msg: msg.into(),
            #[cfg(feature = "backtraces")]
            backtrace: Backtrace::capture(),
        }
    }

    pub fn invalid_base64(msg: impl ToString) -> Self {
        StdError::InvalidBase64 {
            msg: msg.to_string(),
            #[cfg(feature = "backtraces")]
            backtrace: Backtrace::capture(),
        }
    }

    pub fn invalid_data_size(expected: usize, actual: usize) -> Self {
        StdError::InvalidDataSize {
            // Cast is safe because usize is 32 or 64 bit large in all environments we support
            expected: expected as u64,
            actual: actual as u64,
            #[cfg(feature = "backtraces")]
            backtrace: Backtrace::capture(),
        }
    }

    pub fn invalid_hex(msg: impl ToString) -> Self {
        StdError::InvalidHex {
            msg: msg.to_string(),
            #[cfg(feature = "backtraces")]
            backtrace: Backtrace::capture(),
        }
    }

    pub fn invalid_utf8(msg: impl ToString) -> Self {
        StdError::InvalidUtf8 {
            msg: msg.to_string(),
            #[cfg(feature = "backtraces")]
            backtrace: Backtrace::capture(),
        }
    }

    pub fn not_found(kind: impl Into<String>) -> Self {
        StdError::NotFound {
            kind: kind.into(),
            #[cfg(feature = "backtraces")]
            backtrace: Backtrace::capture(),
        }
    }

    pub fn parse_err(target: impl Into<String>, msg: impl ToString) -> Self {
        StdError::ParseErr {
            target_type: target.into(),
            msg: msg.to_string(),
            #[cfg(feature = "backtraces")]
            backtrace: Backtrace::capture(),
        }
    }

    pub fn serialize_err(source: impl Into<String>, msg: impl ToString) -> Self {
        StdError::SerializeErr {
            source_type: source.into(),
            msg: msg.to_string(),
            #[cfg(feature = "backtraces")]
            backtrace: Backtrace::capture(),
        }
    }

    pub fn overflow(source: OverflowError) -> Self {
        StdError::Overflow {
            source,
            #[cfg(feature = "backtraces")]
            backtrace: Backtrace::capture(),
        }
    }

    pub fn divide_by_zero(source: DivideByZeroError) -> Self {
        StdError::DivideByZero {
            source,
            #[cfg(feature = "backtraces")]
            backtrace: Backtrace::capture(),
        }
    }
}

impl PartialEq<StdError> for StdError {
    fn eq(&self, rhs: &StdError) -> bool {
        match self {
            StdError::VerificationErr {
                source,
                #[cfg(feature = "backtraces")]
                    backtrace: _,
            } => {
                if let StdError::VerificationErr {
                    source: rhs_source,
                    #[cfg(feature = "backtraces")]
                        backtrace: _,
                } = rhs
                {
                    source == rhs_source
                } else {
                    false
                }
            }
            StdError::RecoverPubkeyErr {
                source,
                #[cfg(feature = "backtraces")]
                    backtrace: _,
            } => {
                if let StdError::RecoverPubkeyErr {
                    source: rhs_source,
                    #[cfg(feature = "backtraces")]
                        backtrace: _,
                } = rhs
                {
                    source == rhs_source
                } else {
                    false
                }
            }
            StdError::GenericErr {
                msg,
                #[cfg(feature = "backtraces")]
                    backtrace: _,
            } => {
                if let StdError::GenericErr {
                    msg: rhs_msg,
                    #[cfg(feature = "backtraces")]
                        backtrace: _,
                } = rhs
                {
                    msg == rhs_msg
                } else {
                    false
                }
            }
            StdError::InvalidBase64 {
                msg,
                #[cfg(feature = "backtraces")]
                    backtrace: _,
            } => {
                if let StdError::InvalidBase64 {
                    msg: rhs_msg,
                    #[cfg(feature = "backtraces")]
                        backtrace: _,
                } = rhs
                {
                    msg == rhs_msg
                } else {
                    false
                }
            }
            StdError::InvalidDataSize {
                expected,
                actual,
                #[cfg(feature = "backtraces")]
                    backtrace: _,
            } => {
                if let StdError::InvalidDataSize {
                    expected: rhs_expected,
                    actual: rhs_actual,
                    #[cfg(feature = "backtraces")]
                        backtrace: _,
                } = rhs
                {
                    expected == rhs_expected && actual == rhs_actual
                } else {
                    false
                }
            }
            StdError::InvalidHex {
                msg,
                #[cfg(feature = "backtraces")]
                    backtrace: _,
            } => {
                if let StdError::InvalidHex {
                    msg: rhs_msg,
                    #[cfg(feature = "backtraces")]
                        backtrace: _,
                } = rhs
                {
                    msg == rhs_msg
                } else {
                    false
                }
            }
            StdError::InvalidUtf8 {
                msg,
                #[cfg(feature = "backtraces")]
                    backtrace: _,
            } => {
                if let StdError::InvalidUtf8 {
                    msg: rhs_msg,
                    #[cfg(feature = "backtraces")]
                        backtrace: _,
                } = rhs
                {
                    msg == rhs_msg
                } else {
                    false
                }
            }
            StdError::NotFound {
                kind,
                #[cfg(feature = "backtraces")]
                    backtrace: _,
            } => {
                if let StdError::NotFound {
                    kind: rhs_kind,
                    #[cfg(feature = "backtraces")]
                        backtrace: _,
                } = rhs
                {
                    kind == rhs_kind
                } else {
                    false
                }
            }
            StdError::ParseErr {
                target_type,
                msg,
                #[cfg(feature = "backtraces")]
                    backtrace: _,
            } => {
                if let StdError::ParseErr {
                    target_type: rhs_target_type,
                    msg: rhs_msg,
                    #[cfg(feature = "backtraces")]
                        backtrace: _,
                } = rhs
                {
                    target_type == rhs_target_type && msg == rhs_msg
                } else {
                    false
                }
            }
            StdError::SerializeErr {
                source_type,
                msg,
                #[cfg(feature = "backtraces")]
                    backtrace: _,
            } => {
                if let StdError::SerializeErr {
                    source_type: rhs_source_type,
                    msg: rhs_msg,
                    #[cfg(feature = "backtraces")]
                        backtrace: _,
                } = rhs
                {
                    source_type == rhs_source_type && msg == rhs_msg
                } else {
                    false
                }
            }
            StdError::Overflow {
                source,
                #[cfg(feature = "backtraces")]
                    backtrace: _,
            } => {
                if let StdError::Overflow {
                    source: rhs_source,
                    #[cfg(feature = "backtraces")]
                        backtrace: _,
                } = rhs
                {
                    source == rhs_source
                } else {
                    false
                }
            }
            StdError::DivideByZero {
                source,
                #[cfg(feature = "backtraces")]
                    backtrace: _,
            } => {
                if let StdError::DivideByZero {
                    source: rhs_source,
                    #[cfg(feature = "backtraces")]
                        backtrace: _,
                } = rhs
                {
                    source == rhs_source
                } else {
                    false
                }
            }
            StdError::ConversionOverflow {
                source,
                #[cfg(feature = "backtraces")]
                    backtrace: _,
            } => {
                if let StdError::ConversionOverflow {
                    source: rhs_source,
                    #[cfg(feature = "backtraces")]
                        backtrace: _,
                } = rhs
                {
                    source == rhs_source
                } else {
                    false
                }
            }
        }
    }
}

impl From<std::str::Utf8Error> for StdError {
    fn from(source: std::str::Utf8Error) -> Self {
        Self::invalid_utf8(source)
    }
}

impl From<std::string::FromUtf8Error> for StdError {
    fn from(source: std::string::FromUtf8Error) -> Self {
        Self::invalid_utf8(source)
    }
}

impl From<VerificationError> for StdError {
    fn from(source: VerificationError) -> Self {
        Self::verification_err(source)
    }
}

impl From<RecoverPubkeyError> for StdError {
    fn from(source: RecoverPubkeyError) -> Self {
        Self::recover_pubkey_err(source)
    }
}

impl From<OverflowError> for StdError {
    fn from(source: OverflowError) -> Self {
        Self::overflow(source)
    }
}

impl From<DivideByZeroError> for StdError {
    fn from(source: DivideByZeroError) -> Self {
        Self::divide_by_zero(source)
    }
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
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
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
    ) -> Self {
        Self {
            operation,
            operand1: operand1.to_string(),
            operand2: operand2.to_string(),
        }
    }
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
    ) -> Self {
        Self {
            source_type,
            target_type,
            value: value.into(),
        }
    }
}

#[derive(Error, Debug, PartialEq, Eq)]
#[error("Cannot devide {operand} by zero")]
pub struct DivideByZeroError {
    pub operand: String,
}

impl DivideByZeroError {
    pub fn new(operand: impl ToString) -> Self {
        Self {
            operand: operand.to_string(),
        }
    }
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
    fn generic_err_owned() {
        let guess = 7;
        let error = StdError::generic_err(format!("{} is too low", guess));
        match error {
            StdError::GenericErr { msg, .. } => {
                assert_eq!(msg, String::from("7 is too low"));
            }
            e => panic!("unexpected error, {:?}", e),
        }
    }

    // example of reporting static contract errors
    #[test]
    fn generic_err_ref() {
        let error = StdError::generic_err("not implemented");
        match error {
            StdError::GenericErr { msg, .. } => assert_eq!(msg, "not implemented"),
            e => panic!("unexpected error, {:?}", e),
        }
    }

    #[test]
    fn invalid_base64_works_for_strings() {
        let error = StdError::invalid_base64("my text");
        match error {
            StdError::InvalidBase64 { msg, .. } => {
                assert_eq!(msg, "my text");
            }
            _ => panic!("expect different error"),
        }
    }

    #[test]
    fn invalid_base64_works_for_errors() {
        let original = base64::DecodeError::InvalidLength;
        let error = StdError::invalid_base64(original);
        match error {
            StdError::InvalidBase64 { msg, .. } => {
                assert_eq!(msg, "Encoded text cannot have a 6-bit remainder.");
            }
            _ => panic!("expect different error"),
        }
    }

    #[test]
    fn invalid_data_size_works() {
        let error = StdError::invalid_data_size(31, 14);
        match error {
            StdError::InvalidDataSize {
                expected, actual, ..
            } => {
                assert_eq!(expected, 31);
                assert_eq!(actual, 14);
            }
            _ => panic!("expect different error"),
        }
    }

    #[test]
    fn invalid_hex_works_for_strings() {
        let error = StdError::invalid_hex("my text");
        match error {
            StdError::InvalidHex { msg, .. } => {
                assert_eq!(msg, "my text");
            }
            _ => panic!("expect different error"),
        }
    }

    #[test]
    fn invalid_hex_works_for_errors() {
        let original = hex::FromHexError::OddLength;
        let error = StdError::invalid_hex(original);
        match error {
            StdError::InvalidHex { msg, .. } => {
                assert_eq!(msg, "Odd number of digits");
            }
            _ => panic!("expect different error"),
        }
    }

    #[test]
    fn invalid_utf8_works_for_strings() {
        let error = StdError::invalid_utf8("my text");
        match error {
            StdError::InvalidUtf8 { msg, .. } => {
                assert_eq!(msg, "my text");
            }
            _ => panic!("expect different error"),
        }
    }

    #[test]
    fn invalid_utf8_works_for_errors() {
        let original = String::from_utf8(vec![0x80]).unwrap_err();
        let error = StdError::invalid_utf8(original);
        match error {
            StdError::InvalidUtf8 { msg, .. } => {
                assert_eq!(msg, "invalid utf-8 sequence of 1 bytes from index 0");
            }
            _ => panic!("expect different error"),
        }
    }

    #[test]
    fn not_found_works() {
        let error = StdError::not_found("gold");
        match error {
            StdError::NotFound { kind, .. } => assert_eq!(kind, "gold"),
            _ => panic!("expect different error"),
        }
    }

    #[test]
    fn parse_err_works() {
        let error = StdError::parse_err("Book", "Missing field: title");
        match error {
            StdError::ParseErr {
                target_type, msg, ..
            } => {
                assert_eq!(target_type, "Book");
                assert_eq!(msg, "Missing field: title");
            }
            _ => panic!("expect different error"),
        }
    }

    #[test]
    fn serialize_err_works() {
        let error = StdError::serialize_err("Book", "Content too long");
        match error {
            StdError::SerializeErr {
                source_type, msg, ..
            } => {
                assert_eq!(source_type, "Book");
                assert_eq!(msg, "Content too long");
            }
            _ => panic!("expect different error"),
        }
    }

    #[test]
    fn underflow_works_for_u128() {
        let error =
            StdError::overflow(OverflowError::new(OverflowOperation::Sub, 123u128, 456u128));
        match error {
            StdError::Overflow {
                source:
                    OverflowError {
                        operation,
                        operand1,
                        operand2,
                    },
                ..
            } => {
                assert_eq!(operation, OverflowOperation::Sub);
                assert_eq!(operand1, "123");
                assert_eq!(operand2, "456");
            }
            _ => panic!("expect different error"),
        }
    }

    #[test]
    fn overflow_works_for_i64() {
        let error = StdError::overflow(OverflowError::new(OverflowOperation::Sub, 777i64, 1234i64));
        match error {
            StdError::Overflow {
                source:
                    OverflowError {
                        operation,
                        operand1,
                        operand2,
                    },
                ..
            } => {
                assert_eq!(operation, OverflowOperation::Sub);
                assert_eq!(operand1, "777");
                assert_eq!(operand2, "1234");
            }
            _ => panic!("expect different error"),
        }
    }

    #[test]
    fn divide_by_zero_works() {
        let error = StdError::divide_by_zero(DivideByZeroError::new(123u128));
        match error {
            StdError::DivideByZero {
                source: DivideByZeroError { operand },
                ..
            } => assert_eq!(operand, "123"),
            _ => panic!("expect different error"),
        }
    }

    #[test]
    fn implements_debug() {
        let error: StdError = StdError::from(OverflowError::new(OverflowOperation::Sub, 3, 5));
        let embedded = format!("Debug: {:?}", error);
        #[cfg(not(feature = "backtraces"))]
        let expected = r#"Debug: Overflow { source: OverflowError { operation: Sub, operand1: "3", operand2: "5" } }"#;
        #[cfg(feature = "backtraces")]
        let expected = r#"Debug: Overflow { source: OverflowError { operation: Sub, operand1: "3", operand2: "5" }, backtrace: <disabled> }"#;
        assert_eq!(embedded, expected);
    }

    #[test]
    fn implements_display() {
        let error: StdError = StdError::from(OverflowError::new(OverflowOperation::Sub, 3, 5));
        let embedded = format!("Display: {}", error);
        assert_eq!(embedded, "Display: Overflow: Cannot Sub with 3 and 5");
    }

    #[test]
    fn implements_partial_eq() {
        let u1 = StdError::from(OverflowError::new(OverflowOperation::Sub, 3, 5));
        let u2 = StdError::from(OverflowError::new(OverflowOperation::Sub, 3, 5));
        let u3 = StdError::from(OverflowError::new(OverflowOperation::Sub, 3, 7));
        let s1 = StdError::serialize_err("Book", "Content too long");
        let s2 = StdError::serialize_err("Book", "Content too long");
        let s3 = StdError::serialize_err("Book", "Title too long");
        assert_eq!(u1, u2);
        assert_ne!(u1, u3);
        assert_ne!(u1, s1);
        assert_eq!(s1, s2);
        assert_ne!(s1, s3);
    }

    #[test]
    fn from_std_str_utf8error_works() {
        let error: StdError = str::from_utf8(b"Hello \xF0\x90\x80World")
            .unwrap_err()
            .into();
        match error {
            StdError::InvalidUtf8 { msg, .. } => {
                assert_eq!(msg, "invalid utf-8 sequence of 3 bytes from index 6")
            }
            err => panic!("Unexpected error: {:?}", err),
        }
    }

    #[test]
    fn from_std_string_fromutf8error_works() {
        let error: StdError = String::from_utf8(b"Hello \xF0\x90\x80World".to_vec())
            .unwrap_err()
            .into();
        match error {
            StdError::InvalidUtf8 { msg, .. } => {
                assert_eq!(msg, "invalid utf-8 sequence of 3 bytes from index 6")
            }
            err => panic!("Unexpected error: {:?}", err),
        }
    }
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
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SystemError::InvalidRequest { error, request } => write!(
                f,
                "Cannot parse request: {} in: {}",
                error,
                String::from_utf8_lossy(request)
            ),
            SystemError::InvalidResponse { error, response } => write!(
                f,
                "Cannot parse response: {} in: {}",
                error,
                String::from_utf8_lossy(response)
            ),
            SystemError::NoSuchContract { addr } => write!(f, "No such contract: {}", addr),
            SystemError::Unknown {} => write!(f, "Unknown system error"),
            SystemError::UnsupportedRequest { kind } => {
                write!(f, "Unsupported query type: {}", kind)
            }
        }
    }
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
    pub fn unknown_err(error_code: u32) -> Self {
        VerificationError::UnknownErr {
            error_code,
            #[cfg(feature = "backtraces")]
            backtrace: Backtrace::capture(),
        }
    }
}

impl PartialEq<VerificationError> for VerificationError {
    fn eq(&self, rhs: &VerificationError) -> bool {
        match self {
            VerificationError::BatchErr => matches!(rhs, VerificationError::BatchErr),
            VerificationError::GenericErr => matches!(rhs, VerificationError::GenericErr),
            VerificationError::InvalidHashFormat => {
                matches!(rhs, VerificationError::InvalidHashFormat)
            }
            VerificationError::InvalidPubkeyFormat => {
                matches!(rhs, VerificationError::InvalidPubkeyFormat)
            }
            VerificationError::InvalidSignatureFormat => {
                matches!(rhs, VerificationError::InvalidSignatureFormat)
            }
            VerificationError::InvalidRecoveryParam => {
                matches!(rhs, VerificationError::InvalidRecoveryParam)
            }
            VerificationError::UnknownErr { error_code, .. } => {
                if let VerificationError::UnknownErr {
                    error_code: rhs_error_code,
                    ..
                } = rhs
                {
                    error_code == rhs_error_code
                } else {
                    false
                }
            }
        }
    }
}

#[cfg(not(target_arch = "wasm32"))]
impl From<CryptoError> for VerificationError {
    fn from(original: CryptoError) -> Self {
        match original {
            CryptoError::InvalidHashFormat { .. } => VerificationError::InvalidHashFormat,
            CryptoError::InvalidPubkeyFormat { .. } => VerificationError::InvalidPubkeyFormat,
            CryptoError::InvalidSignatureFormat { .. } => VerificationError::InvalidSignatureFormat,
            CryptoError::GenericErr { .. } => VerificationError::GenericErr,
            CryptoError::InvalidRecoveryParam { .. } => VerificationError::InvalidRecoveryParam,
            CryptoError::BatchErr { .. } => VerificationError::BatchErr,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // constructors
    #[test]
    fn unknown_err_works() {
        let error = VerificationError::unknown_err(123);
        match error {
            VerificationError::UnknownErr { error_code, .. } => assert_eq!(error_code, 123),
            _ => panic!("wrong error type!"),
        }
    }
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
    pub fn from_hex(input: &str) -> StdResult<Self> {
        let vec = hex::decode(input).map_err(StdError::invalid_hex)?;
        Ok(Self(vec))
    }

    pub fn to_hex(&self) -> String {
        hex::encode(&self.0)
    }

    pub fn as_slice(&self) -> &[u8] {
        self.0.as_slice()
    }

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
    pub fn to_array<const LENGTH: usize>(&self) -> StdResult<[u8; LENGTH]> {
        if self.len() != LENGTH {
            return Err(StdError::invalid_data_size(LENGTH, self.len()));
        }

        let mut out: [u8; LENGTH] = [0; LENGTH];
        out.copy_from_slice(&self.0);
        Ok(out)
    }
}

impl fmt::Display for HexBinary {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.to_hex())
    }
}

impl fmt::Debug for HexBinary {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Use an output inspired by tuples (https://doc.rust-lang.org/std/fmt/struct.Formatter.html#method.debug_tuple)
        // but with a custom implementation to avoid the need for an intemediate hex string.
        write!(f, "HexBinary(")?;
        for byte in self.0.iter() {
            write!(f, "{:02x}", byte)?;
        }
        write!(f, ")")?;
        Ok(())
    }
}

impl From<&[u8]> for HexBinary {
    fn from(binary: &[u8]) -> Self {
        Self(binary.to_vec())
    }
}

/// Just like Vec<u8>, HexBinary is a smart pointer to [u8].
/// This implements `*data` for us and allows us to
/// do `&*data`, returning a `&[u8]` from a `&HexBinary`.
/// With [deref coercions](https://doc.rust-lang.org/1.22.1/book/first-edition/deref-coercions.html#deref-coercions),
/// this allows us to use `&data` whenever a `&[u8]` is required.
impl Deref for HexBinary {
    type Target = [u8];

    fn deref(&self) -> &Self::Target {
        self.as_slice()
    }
}

// Reference
impl<const LENGTH: usize> From<&[u8; LENGTH]> for HexBinary {
    fn from(source: &[u8; LENGTH]) -> Self {
        Self(source.to_vec())
    }
}

// Owned
impl<const LENGTH: usize> From<[u8; LENGTH]> for HexBinary {
    fn from(source: [u8; LENGTH]) -> Self {
        Self(source.into())
    }
}

impl From<Vec<u8>> for HexBinary {
    fn from(vec: Vec<u8>) -> Self {
        Self(vec)
    }
}

impl From<HexBinary> for Vec<u8> {
    fn from(original: HexBinary) -> Vec<u8> {
        original.0
    }
}

impl From<Binary> for HexBinary {
    fn from(original: Binary) -> Self {
        Self(original.into())
    }
}

impl From<HexBinary> for Binary {
    fn from(original: HexBinary) -> Binary {
        Binary::from(original.0)
    }
}

/// Implement `HexBinary == std::vec::Vec<u8>`
impl PartialEq<Vec<u8>> for HexBinary {
    fn eq(&self, rhs: &Vec<u8>) -> bool {
        // Use Vec<u8> == Vec<u8>
        self.0 == *rhs
    }
}

/// Implement `std::vec::Vec<u8> == HexBinary`
impl PartialEq<HexBinary> for Vec<u8> {
    fn eq(&self, rhs: &HexBinary) -> bool {
        // Use Vec<u8> == Vec<u8>
        *self == rhs.0
    }
}

/// Implement `HexBinary == &[u8]`
impl PartialEq<&[u8]> for HexBinary {
    fn eq(&self, rhs: &&[u8]) -> bool {
        // Use &[u8] == &[u8]
        self.as_slice() == *rhs
    }
}

/// Implement `&[u8] == HexBinary`
impl PartialEq<HexBinary> for &[u8] {
    fn eq(&self, rhs: &HexBinary) -> bool {
        // Use &[u8] == &[u8]
        *self == rhs.as_slice()
    }
}

/// Implement `HexBinary == [u8; LENGTH]`
impl<const LENGTH: usize> PartialEq<[u8; LENGTH]> for HexBinary {
    fn eq(&self, rhs: &[u8; LENGTH]) -> bool {
        self.as_slice() == rhs.as_slice()
    }
}

/// Implement `[u8; LENGTH] == HexBinary`
impl<const LENGTH: usize> PartialEq<HexBinary> for [u8; LENGTH] {
    fn eq(&self, rhs: &HexBinary) -> bool {
        self.as_slice() == rhs.as_slice()
    }
}

/// Implement `HexBinary == &[u8; LENGTH]`
impl<const LENGTH: usize> PartialEq<&[u8; LENGTH]> for HexBinary {
    fn eq(&self, rhs: &&[u8; LENGTH]) -> bool {
        self.as_slice() == rhs.as_slice()
    }
}

/// Implement `&[u8; LENGTH] == HexBinary`
impl<const LENGTH: usize> PartialEq<HexBinary> for &[u8; LENGTH] {
    fn eq(&self, rhs: &HexBinary) -> bool {
        self.as_slice() == rhs.as_slice()
    }
}

/// Serializes as a hex string
impl Serialize for HexBinary {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: ser::Serializer,
    {
        serializer.serialize_str(&self.to_hex())
    }
}

/// Deserializes as a hex string
impl<'de> Deserialize<'de> for HexBinary {
    fn deserialize<D>(deserializer: D) -> Result<HexBinary, D::Error>
    where
        D: Deserializer<'de>,
    {
        deserializer.deserialize_str(HexVisitor)
    }
}

struct HexVisitor;

impl<'de> de::Visitor<'de> for HexVisitor {
    type Value = HexBinary;

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str("valid hex encoded string")
    }

    fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
    where
        E: de::Error,
    {
        match HexBinary::from_hex(v) {
            Ok(data) => Ok(data),
            Err(_) => Err(E::custom(format!("invalid hex: {}", v))),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::{from_slice, to_vec, StdError};
    use std::collections::hash_map::DefaultHasher;
    use std::collections::HashSet;
    use std::hash::{Hash, Hasher};

    #[test]
    fn from_hex_works() {
        let data = HexBinary::from_hex("").unwrap();
        assert_eq!(data, b"");
        let data = HexBinary::from_hex("61").unwrap();
        assert_eq!(data, b"a");
        let data = HexBinary::from_hex("00").unwrap();
        assert_eq!(data, b"\0");

        let data = HexBinary::from_hex("68656c6c6f").unwrap();
        assert_eq!(data, b"hello");
        let data = HexBinary::from_hex("68656C6C6F").unwrap();
        assert_eq!(data, b"hello");
        let data = HexBinary::from_hex("72616e646f6d695a").unwrap();
        assert_eq!(data.as_slice(), b"randomiZ");

        // odd
        match HexBinary::from_hex("123").unwrap_err() {
            StdError::InvalidHex { msg, .. } => {
                assert_eq!(msg, "Odd number of digits")
            }
            _ => panic!("Unexpected error type"),
        }
        // non-hex
        match HexBinary::from_hex("efgh").unwrap_err() {
            StdError::InvalidHex { msg, .. } => {
                assert_eq!(msg, "Invalid character 'g' at position 2")
            }
            _ => panic!("Unexpected error type"),
        }
        // 0x prefixed
        match HexBinary::from_hex("0xaa").unwrap_err() {
            StdError::InvalidHex { msg, .. } => {
                assert_eq!(msg, "Invalid character 'x' at position 1")
            }
            _ => panic!("Unexpected error type"),
        }
        // spaces
        assert!(matches!(
            HexBinary::from_hex("aa ").unwrap_err(),
            StdError::InvalidHex { .. }
        ));
        assert!(matches!(
            HexBinary::from_hex(" aa").unwrap_err(),
            StdError::InvalidHex { .. }
        ));
        assert!(matches!(
            HexBinary::from_hex("a a").unwrap_err(),
            StdError::InvalidHex { .. }
        ));
        assert!(matches!(
            HexBinary::from_hex(" aa ").unwrap_err(),
            StdError::InvalidHex { .. }
        ));
    }

    #[test]
    fn to_hex_works() {
        let binary: &[u8] = b"";
        let encoded = HexBinary::from(binary).to_hex();
        assert_eq!(encoded, "");

        let binary: &[u8] = b"hello";
        let encoded = HexBinary::from(binary).to_hex();
        assert_eq!(encoded, "68656c6c6f");

        let binary = vec![12u8, 187, 0, 17, 250, 1];
        let encoded = HexBinary(binary).to_hex();
        assert_eq!(encoded, "0cbb0011fa01");
    }

    #[test]
    fn to_array_works() {
        // simple
        let binary = HexBinary::from(&[1, 2, 3]);
        let array: [u8; 3] = binary.to_array().unwrap();
        assert_eq!(array, [1, 2, 3]);

        // empty
        let binary = HexBinary::from(&[]);
        let array: [u8; 0] = binary.to_array().unwrap();
        assert_eq!(array, [] as [u8; 0]);

        // invalid size
        let binary = HexBinary::from(&[1, 2, 3]);
        let error = binary.to_array::<8>().unwrap_err();
        match error {
            StdError::InvalidDataSize {
                expected, actual, ..
            } => {
                assert_eq!(expected, 8);
                assert_eq!(actual, 3);
            }
            err => panic!("Unexpected error: {:?}", err),
        }

        // long array (32 bytes)
        let binary =
            HexBinary::from_hex("b75d7d24e428c7859440498efe7caa3997cefb08c99bdd581b6b1f9f866096f0")
                .unwrap();
        let array: [u8; 32] = binary.to_array().unwrap();
        assert_eq!(
            array,
            [
                0xb7, 0x5d, 0x7d, 0x24, 0xe4, 0x28, 0xc7, 0x85, 0x94, 0x40, 0x49, 0x8e, 0xfe, 0x7c,
                0xaa, 0x39, 0x97, 0xce, 0xfb, 0x08, 0xc9, 0x9b, 0xdd, 0x58, 0x1b, 0x6b, 0x1f, 0x9f,
                0x86, 0x60, 0x96, 0xf0,
            ]
        );

        // very long array > 32 bytes (requires Rust 1.47+)
        let binary = HexBinary::from_hex(
            "b75d7d24e428c7859440498efe7caa3997cefb08c99bdd581b6b1f9f866096f073c8c3b0316abe",
        )
        .unwrap();
        let array: [u8; 39] = binary.to_array().unwrap();
        assert_eq!(
            array,
            [
                0xb7, 0x5d, 0x7d, 0x24, 0xe4, 0x28, 0xc7, 0x85, 0x94, 0x40, 0x49, 0x8e, 0xfe, 0x7c,
                0xaa, 0x39, 0x97, 0xce, 0xfb, 0x08, 0xc9, 0x9b, 0xdd, 0x58, 0x1b, 0x6b, 0x1f, 0x9f,
                0x86, 0x60, 0x96, 0xf0, 0x73, 0xc8, 0xc3, 0xb0, 0x31, 0x6a, 0xbe,
            ]
        );
    }

    #[test]
    fn from_slice_works() {
        let original: &[u8] = &[0u8, 187, 61, 11, 250, 0];
        let binary: HexBinary = original.into();
        assert_eq!(binary.as_slice(), [0u8, 187, 61, 11, 250, 0]);
    }

    #[test]
    fn from_fixed_length_array_works() {
        let original = &[];
        let binary: HexBinary = original.into();
        assert_eq!(binary.len(), 0);

        let original = &[0u8];
        let binary: HexBinary = original.into();
        assert_eq!(binary.as_slice(), [0u8]);

        let original = &[0u8, 187, 61, 11, 250, 0];
        let binary: HexBinary = original.into();
        assert_eq!(binary.as_slice(), [0u8, 187, 61, 11, 250, 0]);

        let original = &[
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
            1, 1, 1,
        ];
        let binary: HexBinary = original.into();
        assert_eq!(
            binary.as_slice(),
            [
                1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                1, 1, 1, 1,
            ]
        );
    }

    #[test]
    fn from_owned_fixed_length_array_works() {
        let original = [];
        let binary: HexBinary = original.into();
        assert_eq!(binary.len(), 0);

        let original = [0u8];
        let binary: HexBinary = original.into();
        assert_eq!(binary.as_slice(), [0u8]);

        let original = [0u8, 187, 61, 11, 250, 0];
        let binary: HexBinary = original.into();
        assert_eq!(binary.as_slice(), [0u8, 187, 61, 11, 250, 0]);

        let original = [
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
            1, 1, 1,
        ];
        let binary: HexBinary = original.into();
        assert_eq!(
            binary.as_slice(),
            [
                1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                1, 1, 1, 1,
            ]
        );
    }

    #[test]
    fn from_literal_works() {
        let a: HexBinary = b"".into();
        assert_eq!(a.len(), 0);

        let a: HexBinary = b".".into();
        assert_eq!(a.len(), 1);

        let a: HexBinary = b"...".into();
        assert_eq!(a.len(), 3);

        let a: HexBinary = b"...............................".into();
        assert_eq!(a.len(), 31);

        let a: HexBinary = b"................................".into();
        assert_eq!(a.len(), 32);

        let a: HexBinary = (b".................................").into();
        assert_eq!(a.len(), 33);
    }

    #[test]
    fn from_vec_works() {
        let original = vec![0u8, 187, 61, 11, 250, 0];
        let original_ptr = original.as_ptr();
        let binary: HexBinary = original.into();
        assert_eq!(binary.as_slice(), [0u8, 187, 61, 11, 250, 0]);
        assert_eq!(binary.0.as_ptr(), original_ptr, "vector must not be copied");
    }

    #[test]
    fn into_vec_works() {
        // Into<Vec<u8>> for HexBinary
        let original = HexBinary(vec![0u8, 187, 61, 11, 250, 0]);
        let original_ptr = original.0.as_ptr();
        let vec: Vec<u8> = original.into();
        assert_eq!(vec.as_slice(), [0u8, 187, 61, 11, 250, 0]);
        assert_eq!(vec.as_ptr(), original_ptr, "vector must not be copied");

        // From<HexBinary> for Vec<u8>
        let original = HexBinary(vec![7u8, 35, 49, 101, 0, 255]);
        let original_ptr = original.0.as_ptr();
        let vec = Vec::<u8>::from(original);
        assert_eq!(vec.as_slice(), [7u8, 35, 49, 101, 0, 255]);
        assert_eq!(vec.as_ptr(), original_ptr, "vector must not be copied");
    }

    #[test]
    fn from_binary_works() {
        let original = Binary::from([0u8, 187, 61, 11, 250, 0]);
        let original_ptr = original.as_ptr();
        let binary: HexBinary = original.into();
        assert_eq!(binary.as_slice(), [0u8, 187, 61, 11, 250, 0]);
        assert_eq!(binary.0.as_ptr(), original_ptr, "vector must not be copied");
    }

    #[test]
    fn into_binary_works() {
        // Into<Binary> for HexBinary
        let original = HexBinary(vec![0u8, 187, 61, 11, 250, 0]);
        let original_ptr = original.0.as_ptr();
        let bin: Binary = original.into();
        assert_eq!(bin.as_slice(), [0u8, 187, 61, 11, 250, 0]);
        assert_eq!(bin.as_ptr(), original_ptr, "vector must not be copied");

        // From<HexBinary> for Binary
        let original = HexBinary(vec![7u8, 35, 49, 101, 0, 255]);
        let original_ptr = original.0.as_ptr();
        let bin = Binary::from(original);
        assert_eq!(bin.as_slice(), [7u8, 35, 49, 101, 0, 255]);
        assert_eq!(bin.as_ptr(), original_ptr, "vector must not be copied");
    }

    #[test]
    fn serialization_works() {
        let binary = HexBinary(vec![0u8, 187, 61, 11, 250, 0]);

        let json = to_vec(&binary).unwrap();
        let deserialized: HexBinary = from_slice(&json).unwrap();

        assert_eq!(binary, deserialized);
    }

    #[test]
    fn deserialize_from_valid_string() {
        let hex = "00bb3d0bfa00";
        // this is the binary behind above string
        let expected = vec![0u8, 187, 61, 11, 250, 0];

        let serialized = to_vec(&hex).unwrap();
        let deserialized: HexBinary = from_slice(&serialized).unwrap();
        assert_eq!(expected, deserialized.as_slice());
    }

    #[test]
    fn deserialize_from_invalid_string() {
        let invalid_str = "**BAD!**";
        let serialized = to_vec(&invalid_str).unwrap();
        let res = from_slice::<HexBinary>(&serialized);
        assert!(res.is_err());
    }

    #[test]
    fn hex_binary_implements_debug() {
        // Some data
        let data = HexBinary(vec![0x07, 0x35, 0xAA, 0xcb, 0x00, 0xff]);
        assert_eq!(format!("{:?}", data), "HexBinary(0735aacb00ff)",);

        // Empty
        let data = HexBinary(vec![]);
        assert_eq!(format!("{:?}", data), "HexBinary()",);
    }

    #[test]
    fn hex_binary_implements_deref() {
        // Dereference to [u8]
        let data = HexBinary(vec![7u8, 35, 49, 101, 0, 255]);
        assert_eq!(*data, [7u8, 35, 49, 101, 0, 255]);

        // This checks deref coercions from &Binary to &[u8] works
        let data = HexBinary(vec![7u8, 35, 49, 101, 0, 255]);
        assert_eq!(data.len(), 6);
        let data_slice: &[u8] = &data;
        assert_eq!(data_slice, &[7u8, 35, 49, 101, 0, 255]);
    }

    #[test]
    fn hex_binary_implements_hash() {
        let a1 = HexBinary::from([0, 187, 61, 11, 250, 0]);
        let mut hasher = DefaultHasher::new();
        a1.hash(&mut hasher);
        let a1_hash = hasher.finish();

        let a2 = HexBinary::from([0, 187, 61, 11, 250, 0]);
        let mut hasher = DefaultHasher::new();
        a2.hash(&mut hasher);
        let a2_hash = hasher.finish();

        let b = HexBinary::from([16, 21, 33, 0, 255, 9]);
        let mut hasher = DefaultHasher::new();
        b.hash(&mut hasher);
        let b_hash = hasher.finish();

        assert_eq!(a1_hash, a2_hash);
        assert_ne!(a1_hash, b_hash);
    }

    /// This requires Hash and Eq to be implemented
    #[test]
    fn hex_binary_can_be_used_in_hash_set() {
        let a1 = HexBinary::from([0, 187, 61, 11, 250, 0]);
        let a2 = HexBinary::from([0, 187, 61, 11, 250, 0]);
        let b = HexBinary::from([16, 21, 33, 0, 255, 9]);

        let mut set = HashSet::new();
        set.insert(a1.clone());
        set.insert(a2.clone());
        set.insert(b.clone());
        assert_eq!(set.len(), 2);

        let set1 = HashSet::<HexBinary>::from_iter(vec![b.clone(), a1.clone()]);
        let set2 = HashSet::from_iter(vec![a1, a2, b]);
        assert_eq!(set1, set2);
    }

    #[test]
    fn hex_binary_implements_partial_eq_with_vector() {
        let a = HexBinary(vec![5u8; 3]);
        let b = vec![5u8; 3];
        let c = vec![9u8; 3];
        assert_eq!(a, b);
        assert_eq!(b, a);
        assert_ne!(a, c);
        assert_ne!(c, a);
    }

    #[test]
    fn hex_binary_implements_partial_eq_with_slice_and_array() {
        let a = HexBinary(vec![0xAA, 0xBB]);

        // Slice: &[u8]
        assert_eq!(a, b"\xAA\xBB" as &[u8]);
        assert_eq!(b"\xAA\xBB" as &[u8], a);
        assert_ne!(a, b"\x11\x22" as &[u8]);
        assert_ne!(b"\x11\x22" as &[u8], a);

        // Array reference: &[u8; 2]
        assert_eq!(a, b"\xAA\xBB");
        assert_eq!(b"\xAA\xBB", a);
        assert_ne!(a, b"\x11\x22");
        assert_ne!(b"\x11\x22", a);

        // Array: [u8; 2]
        assert_eq!(a, [0xAA, 0xBB]);
        assert_eq!([0xAA, 0xBB], a);
        assert_ne!(a, [0x11, 0x22]);
        assert_ne!([0x11, 0x22], a);
    }
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
    pub fn with_block(block: IbcTimeoutBlock) -> Self {
        IbcTimeout {
            block: Some(block),
            timestamp: None,
        }
    }

    pub fn with_timestamp(timestamp: Timestamp) -> Self {
        IbcTimeout {
            block: None,
            timestamp: Some(timestamp),
        }
    }

    pub fn with_both(block: IbcTimeoutBlock, timestamp: Timestamp) -> Self {
        IbcTimeout {
            block: Some(block),
            timestamp: Some(timestamp),
        }
    }

    pub fn block(&self) -> Option<IbcTimeoutBlock> {
        self.block
    }

    pub fn timestamp(&self) -> Option<Timestamp> {
        self.timestamp
    }
}

impl From<Timestamp> for IbcTimeout {
    fn from(timestamp: Timestamp) -> IbcTimeout {
        IbcTimeout::with_timestamp(timestamp)
    }
}

impl From<IbcTimeoutBlock> for IbcTimeout {
    fn from(original: IbcTimeoutBlock) -> IbcTimeout {
        IbcTimeout::with_block(original)
    }
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
    ) -> Self {
        Self {
            endpoint,
            counterparty_endpoint,
            order,
            version: version.into(),
            connection_id: connection_id.into(),
        }
    }
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
    pub fn is_zero(&self) -> bool {
        self.revision == 0 && self.height == 0
    }
}

impl PartialOrd for IbcTimeoutBlock {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for IbcTimeoutBlock {
    fn cmp(&self, other: &Self) -> Ordering {
        match self.revision.cmp(&other.revision) {
            Ordering::Equal => self.height.cmp(&other.height),
            other => other,
        }
    }
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
    ) -> Self {
        Self {
            data: data.into(),
            src,
            dest,
            sequence,
            timeout,
        }
    }
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, JsonSchema)]
#[non_exhaustive]
pub struct IbcAcknowledgement {
    pub data: Binary,
    // we may add more info here in the future (meta-data from the acknowledgement)
    // there have been proposals to extend this type in core ibc for future versions
}

impl IbcAcknowledgement {
    pub fn new(data: impl Into<Binary>) -> Self {
        IbcAcknowledgement { data: data.into() }
    }

    pub fn encode_json(data: &impl Serialize) -> StdResult<Self> {
        Ok(IbcAcknowledgement {
            data: to_binary(data)?,
        })
    }
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
    pub fn new_init(channel: IbcChannel) -> Self {
        Self::OpenInit { channel }
    }

    pub fn new_try(channel: IbcChannel, counterparty_version: impl Into<String>) -> Self {
        Self::OpenTry {
            channel,
            counterparty_version: counterparty_version.into(),
        }
    }

    pub fn channel(&self) -> &IbcChannel {
        match self {
            Self::OpenInit { channel } => channel,
            Self::OpenTry { channel, .. } => channel,
        }
    }

    pub fn counterparty_version(&self) -> Option<&str> {
        match self {
            Self::OpenTry {
                counterparty_version,
                ..
            } => Some(counterparty_version),
            _ => None,
        }
    }
}

impl From<IbcChannelOpenMsg> for IbcChannel {
    fn from(msg: IbcChannelOpenMsg) -> IbcChannel {
        match msg {
            IbcChannelOpenMsg::OpenInit { channel } => channel,
            IbcChannelOpenMsg::OpenTry { channel, .. } => channel,
        }
    }
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
    pub fn new_ack(channel: IbcChannel, counterparty_version: impl Into<String>) -> Self {
        Self::OpenAck {
            channel,
            counterparty_version: counterparty_version.into(),
        }
    }

    pub fn new_confirm(channel: IbcChannel) -> Self {
        Self::OpenConfirm { channel }
    }

    pub fn channel(&self) -> &IbcChannel {
        match self {
            Self::OpenAck { channel, .. } => channel,
            Self::OpenConfirm { channel } => channel,
        }
    }

    pub fn counterparty_version(&self) -> Option<&str> {
        match self {
            Self::OpenAck {
                counterparty_version,
                ..
            } => Some(counterparty_version),
            _ => None,
        }
    }
}

impl From<IbcChannelConnectMsg> for IbcChannel {
    fn from(msg: IbcChannelConnectMsg) -> IbcChannel {
        match msg {
            IbcChannelConnectMsg::OpenAck { channel, .. } => channel,
            IbcChannelConnectMsg::OpenConfirm { channel } => channel,
        }
    }
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
    pub fn new_init(channel: IbcChannel) -> Self {
        Self::CloseInit { channel }
    }

    pub fn new_confirm(channel: IbcChannel) -> Self {
        Self::CloseConfirm { channel }
    }

    pub fn channel(&self) -> &IbcChannel {
        match self {
            Self::CloseInit { channel } => channel,
            Self::CloseConfirm { channel } => channel,
        }
    }
}

impl From<IbcChannelCloseMsg> for IbcChannel {
    fn from(msg: IbcChannelCloseMsg) -> IbcChannel {
        match msg {
            IbcChannelCloseMsg::CloseInit { channel } => channel,
            IbcChannelCloseMsg::CloseConfirm { channel } => channel,
        }
    }
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
    pub fn new(packet: IbcPacket) -> Self {
        Self { packet }
    }

    #[cfg(feature = "ibc3")]
    pub fn new(packet: IbcPacket, relayer: Addr) -> Self {
        Self { packet, relayer }
    }
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
    pub fn new(acknowledgement: IbcAcknowledgement, original_packet: IbcPacket) -> Self {
        Self {
            acknowledgement,
            original_packet,
        }
    }

    #[cfg(feature = "ibc3")]
    pub fn new(
        acknowledgement: IbcAcknowledgement,
        original_packet: IbcPacket,
        relayer: Addr,
    ) -> Self {
        Self {
            acknowledgement,
            original_packet,
            relayer,
        }
    }
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
    pub fn new(packet: IbcPacket) -> Self {
        Self { packet }
    }

    #[cfg(feature = "ibc3")]
    pub fn new(packet: IbcPacket, relayer: Addr) -> Self {
        Self { packet, relayer }
    }
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
    fn default() -> Self {
        IbcBasicResponse {
            messages: vec![],
            attributes: vec![],
            events: vec![],
        }
    }
}

impl<T> IbcBasicResponse<T> {
    pub fn new() -> Self {
        Self::default()
    }

    /// Add an attribute included in the main `wasm` event.
    pub fn add_attribute(mut self, key: impl Into<String>, value: impl Into<String>) -> Self {
        self.attributes.push(Attribute::new(key, value));
        self
    }

    /// This creates a "fire and forget" message, by using `SubMsg::new()` to wrap it,
    /// and adds it to the list of messages to process.
    pub fn add_message(mut self, msg: impl Into<CosmosMsg<T>>) -> Self {
        self.messages.push(SubMsg::new(msg));
        self
    }

    /// This takes an explicit SubMsg (creates via eg. `reply_on_error`)
    /// and adds it to the list of messages to process.
    pub fn add_submessage(mut self, msg: SubMsg<T>) -> Self {
        self.messages.push(msg);
        self
    }

    /// Adds an extra event to the response, separate from the main `wasm` event
    /// that is always created.
    ///
    /// The `wasm-` prefix will be appended by the runtime to the provided type
    /// of event.
    pub fn add_event(mut self, event: Event) -> Self {
        self.events.push(event);
        self
    }

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
    ) -> Self {
        self.attributes.extend(attrs.into_iter().map(A::into));
        self
    }

    /// Bulk add "fire and forget" messages to the list of messages to process.
    ///
    /// ## Examples
    ///
    /// ```
    /// use cosmwasm_std::{CosmosMsg, IbcBasicResponse};
    ///
    /// fn make_response_with_msgs(msgs: Vec<CosmosMsg>) -> IbcBasicResponse {
    ///     IbcBasicResponse::new().add_messages(msgs)
    /// }
    /// ```
    pub fn add_messages<M: Into<CosmosMsg<T>>>(self, msgs: impl IntoIterator<Item = M>) -> Self {
        self.add_submessages(msgs.into_iter().map(SubMsg::new))
    }

    /// Bulk add explicit SubMsg structs to the list of messages to process.
    ///
    /// ## Examples
    ///
    /// ```
    /// use cosmwasm_std::{SubMsg, IbcBasicResponse};
    ///
    /// fn make_response_with_submsgs(msgs: Vec<SubMsg>) -> IbcBasicResponse {
    ///     IbcBasicResponse::new().add_submessages(msgs)
    /// }
    /// ```
    pub fn add_submessages(mut self, msgs: impl IntoIterator<Item = SubMsg<T>>) -> Self {
        self.messages.extend(msgs.into_iter());
        self
    }

    /// Bulk add custom events to the response. These are separate from the main
    /// `wasm` event.
    ///
    /// The `wasm-` prefix will be appended by the runtime to the provided types
    /// of events.
    pub fn add_events(mut self, events: impl IntoIterator<Item = Event>) -> Self {
        self.events.extend(events.into_iter());
        self
    }
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
    fn default() -> Self {
        IbcReceiveResponse {
            acknowledgement: Binary(vec![]),
            messages: vec![],
            attributes: vec![],
            events: vec![],
        }
    }
}

impl<T> IbcReceiveResponse<T> {
    pub fn new() -> Self {
        Self::default()
    }

    /// Set the acknowledgement for this response.
    pub fn set_ack(mut self, ack: impl Into<Binary>) -> Self {
        self.acknowledgement = ack.into();
        self
    }

    /// Add an attribute included in the main `wasm` event.
    pub fn add_attribute(mut self, key: impl Into<String>, value: impl Into<String>) -> Self {
        self.attributes.push(Attribute::new(key, value));
        self
    }

    /// This creates a "fire and forget" message, by using `SubMsg::new()` to wrap it,
    /// and adds it to the list of messages to process.
    pub fn add_message(mut self, msg: impl Into<CosmosMsg<T>>) -> Self {
        self.messages.push(SubMsg::new(msg));
        self
    }

    /// This takes an explicit SubMsg (creates via eg. `reply_on_error`)
    /// and adds it to the list of messages to process.
    pub fn add_submessage(mut self, msg: SubMsg<T>) -> Self {
        self.messages.push(msg);
        self
    }

    /// Adds an extra event to the response, separate from the main `wasm` event
    /// that is always created.
    ///
    /// The `wasm-` prefix will be appended by the runtime to the provided type
    /// of event.
    pub fn add_event(mut self, event: Event) -> Self {
        self.events.push(event);
        self
    }

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
    ) -> Self {
        self.attributes.extend(attrs.into_iter().map(A::into));
        self
    }

    /// Bulk add "fire and forget" messages to the list of messages to process.
    ///
    /// ## Examples
    ///
    /// ```
    /// use cosmwasm_std::{CosmosMsg, IbcReceiveResponse};
    ///
    /// fn make_response_with_msgs(msgs: Vec<CosmosMsg>) -> IbcReceiveResponse {
    ///     IbcReceiveResponse::new().add_messages(msgs)
    /// }
    /// ```
    pub fn add_messages<M: Into<CosmosMsg<T>>>(self, msgs: impl IntoIterator<Item = M>) -> Self {
        self.add_submessages(msgs.into_iter().map(SubMsg::new))
    }

    /// Bulk add explicit SubMsg structs to the list of messages to process.
    ///
    /// ## Examples
    ///
    /// ```
    /// use cosmwasm_std::{SubMsg, IbcReceiveResponse};
    ///
    /// fn make_response_with_submsgs(msgs: Vec<SubMsg>) -> IbcReceiveResponse {
    ///     IbcReceiveResponse::new().add_submessages(msgs)
    /// }
    /// ```
    pub fn add_submessages(mut self, msgs: impl IntoIterator<Item = SubMsg<T>>) -> Self {
        self.messages.extend(msgs.into_iter());
        self
    }

    /// Bulk add custom events to the response. These are separate from the main
    /// `wasm` event.
    ///
    /// The `wasm-` prefix will be appended by the runtime to the provided types
    /// of events.
    pub fn add_events(mut self, events: impl IntoIterator<Item = Event>) -> Self {
        self.events.extend(events.into_iter());
        self
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json_wasm::to_string;

    #[test]
    // added this to check json format for go compat, as I was unsure how some messages are snake encoded
    fn serialize_msg() {
        let msg = IbcMsg::Transfer {
            channel_id: "channel-123".to_string(),
            to_address: "my-special-addr".into(),
            amount: Coin::new(12345678, "uatom"),
            timeout: IbcTimeout::with_timestamp(Timestamp::from_nanos(1234567890)),
        };
        let encoded = to_string(&msg).unwrap();
        let expected = r#"{"transfer":{"channel_id":"channel-123","to_address":"my-special-addr","amount":{"denom":"uatom","amount":"12345678"},"timeout":{"block":null,"timestamp":"1234567890"}}}"#;
        assert_eq!(encoded.as_str(), expected);
    }

    #[test]
    fn ibc_timeout_serialize() {
        let timestamp = IbcTimeout::with_timestamp(Timestamp::from_nanos(684816844));
        let expected = r#"{"block":null,"timestamp":"684816844"}"#;
        assert_eq!(to_string(&timestamp).unwrap(), expected);

        let block = IbcTimeout::with_block(IbcTimeoutBlock {
            revision: 12,
            height: 129,
        });
        let expected = r#"{"block":{"revision":12,"height":129},"timestamp":null}"#;
        assert_eq!(to_string(&block).unwrap(), expected);

        let both = IbcTimeout::with_both(
            IbcTimeoutBlock {
                revision: 12,
                height: 129,
            },
            Timestamp::from_nanos(684816844),
        );
        let expected = r#"{"block":{"revision":12,"height":129},"timestamp":"684816844"}"#;
        assert_eq!(to_string(&both).unwrap(), expected);
    }

    #[test]
    #[allow(clippy::eq_op)]
    fn ibc_timeout_block_ord() {
        let epoch1a = IbcTimeoutBlock {
            revision: 1,
            height: 1000,
        };
        let epoch1b = IbcTimeoutBlock {
            revision: 1,
            height: 3000,
        };
        let epoch2a = IbcTimeoutBlock {
            revision: 2,
            height: 500,
        };
        let epoch2b = IbcTimeoutBlock {
            revision: 2,
            height: 2500,
        };

        // basic checks
        assert!(epoch1a == epoch1a);
        assert!(epoch1a < epoch1b);
        assert!(epoch1b > epoch1a);
        assert!(epoch2a > epoch1a);
        assert!(epoch2b > epoch1a);

        // ensure epoch boundaries are correctly handled
        assert!(epoch1b > epoch1a);
        assert!(epoch2a > epoch1b);
        assert!(epoch2b > epoch2a);
        assert!(epoch2b > epoch1b);
        // and check the inverse compare
        assert!(epoch1a < epoch1b);
        assert!(epoch1b < epoch2a);
        assert!(epoch2a < epoch2b);
        assert!(epoch1b < epoch2b);
    }

    #[test]
    fn ibc_packet_serialize() {
        let packet = IbcPacket {
            data: b"foo".into(),
            src: IbcEndpoint {
                port_id: "their-port".to_string(),
                channel_id: "channel-1234".to_string(),
            },
            dest: IbcEndpoint {
                port_id: "our-port".to_string(),
                channel_id: "chan33".into(),
            },
            sequence: 27,
            timeout: IbcTimeout::with_both(
                IbcTimeoutBlock {
                    revision: 1,
                    height: 12345678,
                },
                Timestamp::from_nanos(4611686018427387904),
            ),
        };
        let expected = r#"{"data":"Zm9v","src":{"port_id":"their-port","channel_id":"channel-1234"},"dest":{"port_id":"our-port","channel_id":"chan33"},"sequence":27,"timeout":{"block":{"revision":1,"height":12345678},"timestamp":"4611686018427387904"}}"#;
        assert_eq!(to_string(&packet).unwrap(), expected);

        let no_timestamp = IbcPacket {
            data: b"foo".into(),
            src: IbcEndpoint {
                port_id: "their-port".to_string(),
                channel_id: "channel-1234".to_string(),
            },
            dest: IbcEndpoint {
                port_id: "our-port".to_string(),
                channel_id: "chan33".into(),
            },
            sequence: 27,
            timeout: IbcTimeout::with_block(IbcTimeoutBlock {
                revision: 1,
                height: 12345678,
            }),
        };
        let expected = r#"{"data":"Zm9v","src":{"port_id":"their-port","channel_id":"channel-1234"},"dest":{"port_id":"our-port","channel_id":"chan33"},"sequence":27,"timeout":{"block":{"revision":1,"height":12345678},"timestamp":null}}"#;
        assert_eq!(to_string(&no_timestamp).unwrap(), expected);
    }
}
}
mod import_helpers {
/// Returns the four most significant bytes
#[allow(dead_code)] // only used in Wasm builds
#[inline]
pub fn from_high_half(data: u64) -> u32 {
    (data >> 32).try_into().unwrap()
}

/// Returns the four least significant bytes
#[allow(dead_code)] // only used in Wasm builds
#[inline]
pub fn from_low_half(data: u64) -> u32 {
    (data & 0xFFFFFFFF).try_into().unwrap()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn from_high_half_works() {
        assert_eq!(from_high_half(0), 0);
        assert_eq!(from_high_half(0x1122334455667788), 0x11223344);
    }

    #[test]
    fn from_low_haf_works() {
        assert_eq!(from_low_half(0), 0);
        assert_eq!(from_low_half(0x1122334455667788), 0x55667788);
    }
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

    fn try_from(value: i32) -> Result<Self, Self::Error> {
        match value {
            1 => Ok(Order::Ascending),
            2 => Ok(Order::Descending),
            _ => Err(StdError::generic_err("Order must be 1 or 2")),
        }
    }
}

impl From<Order> for i32 {
    fn from(original: Order) -> i32 {
        original as _
    }
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
    pub const fn new(value: Uint128) -> Self {
        Self(value)
    }

    /// Creates a Decimal(Uint128(value))
    /// This is equivalent to `Decimal::from_atomics(value, 18)` but usable in a const context.
    pub const fn raw(value: u128) -> Self {
        Self(Uint128::new(value))
    }

    /// Create a 1.0 Decimal
    #[inline]
    pub const fn one() -> Self {
        Self(Self::DECIMAL_FRACTIONAL)
    }

    /// Create a 0.0 Decimal
    #[inline]
    pub const fn zero() -> Self {
        Self(Uint128::zero())
    }

    /// Convert x% into Decimal
    pub fn percent(x: u64) -> Self {
        Self(((x as u128) * 10_000_000_000_000_000).into())
    }

    /// Convert permille (x/1000) into Decimal
    pub fn permille(x: u64) -> Self {
        Self(((x as u128) * 1_000_000_000_000_000).into())
    }

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
    ) -> Result<Self, DecimalRangeExceeded> {
        let atomics = atomics.into();
        const TEN: Uint128 = Uint128::new(10);
        Ok(match decimal_places.cmp(&(Self::DECIMAL_PLACES)) {
            Ordering::Less => {
                let digits = (Self::DECIMAL_PLACES) - decimal_places; // No overflow because decimal_places < DECIMAL_PLACES
                let factor = TEN.checked_pow(digits).unwrap(); // Safe because digits <= 17
                Self(
                    atomics
                        .checked_mul(factor)
                        .map_err(|_| DecimalRangeExceeded)?,
                )
            }
            Ordering::Equal => Self(atomics),
            Ordering::Greater => {
                let digits = decimal_places - (Self::DECIMAL_PLACES); // No overflow because decimal_places > DECIMAL_PLACES
                if let Ok(factor) = TEN.checked_pow(digits) {
                    Self(atomics.checked_div(factor).unwrap()) // Safe because factor cannot be zero
                } else {
                    // In this case `factor` exceeds the Uint128 range.
                    // Any Uint128 `x` divided by `factor` with `factor > Uint128::MAX` is 0.
                    // Try e.g. Python3: `(2**128-1) // 2**128`
                    Self(Uint128::zero())
                }
            }
        })
    }

    /// Returns the ratio (numerator / denominator) as a Decimal
    pub fn from_ratio(numerator: impl Into<Uint128>, denominator: impl Into<Uint128>) -> Self {
        match Decimal::checked_from_ratio(numerator, denominator) {
            Ok(value) => value,
            Err(CheckedFromRatioError::DivideByZero) => {
                panic!("Denominator must not be zero")
            }
            Err(CheckedFromRatioError::Overflow) => panic!("Multiplication overflow"),
        }
    }

    /// Returns the ratio (numerator / denominator) as a Decimal
    pub fn checked_from_ratio(
        numerator: impl Into<Uint128>,
        denominator: impl Into<Uint128>,
    ) -> Result<Self, CheckedFromRatioError> {
        let numerator: Uint128 = numerator.into();
        let denominator: Uint128 = denominator.into();
        match numerator.checked_multiply_ratio(Self::DECIMAL_FRACTIONAL, denominator) {
            Ok(ratio) => {
                // numerator * DECIMAL_FRACTIONAL / denominator
                Ok(Decimal(ratio))
            }
            Err(CheckedMultiplyRatioError::Overflow) => Err(CheckedFromRatioError::Overflow),
            Err(CheckedMultiplyRatioError::DivideByZero) => {
                Err(CheckedFromRatioError::DivideByZero)
            }
        }
    }

    pub const fn is_zero(&self) -> bool {
        self.0.is_zero()
    }

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
    pub const fn atomics(&self) -> Uint128 {
        self.0
    }

    /// The number of decimal places. This is a constant value for now
    /// but this could potentially change as the type evolves.
    ///
    /// See also [`Decimal::atomics()`].
    #[inline]
    pub const fn decimal_places(&self) -> u32 {
        Self::DECIMAL_PLACES
    }

    /// Rounds value down after decimal places.
    pub fn floor(&self) -> Self {
        Self((self.0 / Self::DECIMAL_FRACTIONAL) * Self::DECIMAL_FRACTIONAL)
    }

    /// Rounds value up after decimal places. Panics on overflow.
    pub fn ceil(&self) -> Self {
        match self.checked_ceil() {
            Ok(value) => value,
            Err(_) => panic!("attempt to ceil with overflow"),
        }
    }

    /// Rounds value up after decimal places. Returns OverflowError on overflow.
    pub fn checked_ceil(&self) -> Result<Self, RoundUpOverflowError> {
        let floor = self.floor();
        if floor == self {
            Ok(floor)
        } else {
            floor
                .checked_add(Decimal::one())
                .map_err(|_| RoundUpOverflowError)
        }
    }

    pub fn checked_add(self, other: Self) -> Result<Self, OverflowError> {
        self.0
            .checked_add(other.0)
            .map(Self)
            .map_err(|_| OverflowError::new(OverflowOperation::Add, self, other))
    }

    pub fn checked_sub(self, other: Self) -> Result<Self, OverflowError> {
        self.0
            .checked_sub(other.0)
            .map(Self)
            .map_err(|_| OverflowError::new(OverflowOperation::Sub, self, other))
    }

    /// Multiplies one `Decimal` by another, returning an `OverflowError` if an overflow occurred.
    pub fn checked_mul(self, other: Self) -> Result<Self, OverflowError> {
        let result_as_uint256 = self.numerator().full_mul(other.numerator())
            / Uint256::from_uint128(Self::DECIMAL_FRACTIONAL); // from_uint128 is a const method and should be "free"
        result_as_uint256
            .try_into()
            .map(Self)
            .map_err(|_| OverflowError {
                operation: crate::OverflowOperation::Mul,
                operand1: self.to_string(),
                operand2: other.to_string(),
            })
    }

    /// Raises a value to the power of `exp`, panics if an overflow occurred.
    pub fn pow(self, exp: u32) -> Self {
        match self.checked_pow(exp) {
            Ok(value) => value,
            Err(_) => panic!("Multiplication overflow"),
        }
    }

    /// Raises a value to the power of `exp`, returning an `OverflowError` if an overflow occurred.
    pub fn checked_pow(self, exp: u32) -> Result<Self, OverflowError> {
        // This uses the exponentiation by squaring algorithm:
        // https://en.wikipedia.org/wiki/Exponentiation_by_squaring#Basic_method

        fn inner(mut x: Decimal, mut n: u32) -> Result<Decimal, OverflowError> {
            if n == 0 {
                return Ok(Decimal::one());
            }

            let mut y = Decimal::one();

            while n > 1 {
                if n % 2 == 0 {
                    x = x.checked_mul(x)?;
                    n /= 2;
                } else {
                    y = x.checked_mul(y)?;
                    x = x.checked_mul(x)?;
                    n = (n - 1) / 2;
                }
            }

            Ok(x * y)
        }

        inner(self, exp).map_err(|_| OverflowError {
            operation: crate::OverflowOperation::Pow,
            operand1: self.to_string(),
            operand2: exp.to_string(),
        })
    }

    pub fn checked_div(self, other: Self) -> Result<Self, CheckedFromRatioError> {
        Decimal::checked_from_ratio(self.numerator(), other.numerator())
    }

    pub fn checked_rem(self, other: Self) -> Result<Self, DivideByZeroError> {
        self.0
            .checked_rem(other.0)
            .map(Self)
            .map_err(|_| DivideByZeroError::new(self))
    }

    /// Returns the approximate square root as a Decimal.
    ///
    /// This should not overflow or panic.
    pub fn sqrt(&self) -> Self {
        // Algorithm described in https://hackmd.io/@webmaster128/SJThlukj_
        // We start with the highest precision possible and lower it until
        // there's no overflow.
        //
        // TODO: This could be made more efficient once log10 is in:
        // https://github.com/rust-lang/rust/issues/70887
        // The max precision is something like `9 - log10(self.0) / 2`.
        (0..=Self::DECIMAL_PLACES / 2)
            .rev()
            .find_map(|i| self.sqrt_with_precision(i))
            // The last step (i = 0) is guaranteed to succeed because `isqrt(u128::MAX) * 10^9` does not overflow
            .unwrap()
    }

    /// Lower precision means more aggressive rounding, but less risk of overflow.
    /// Precision *must* be a number between 0 and 9 (inclusive).
    ///
    /// Returns `None` if the internal multiplication overflows.
    fn sqrt_with_precision(&self, precision: u32) -> Option<Self> {
        let inner_mul = 100u128.pow(precision);
        self.0.checked_mul(inner_mul.into()).ok().map(|inner| {
            let outer_mul = 10u128.pow(Self::DECIMAL_PLACES / 2 - precision);
            Decimal(inner.isqrt().checked_mul(Uint128::from(outer_mul)).unwrap())
        })
    }

    pub const fn abs_diff(self, other: Self) -> Self {
        Self(self.0.abs_diff(other.0))
    }

    pub fn saturating_add(self, other: Self) -> Self {
        match self.checked_add(other) {
            Ok(value) => value,
            Err(_) => Self::MAX,
        }
    }

    pub fn saturating_sub(self, other: Self) -> Self {
        match self.checked_sub(other) {
            Ok(value) => value,
            Err(_) => Self::zero(),
        }
    }

    pub fn saturating_mul(self, other: Self) -> Self {
        match self.checked_mul(other) {
            Ok(value) => value,
            Err(_) => Self::MAX,
        }
    }

    pub fn saturating_pow(self, exp: u32) -> Self {
        match self.checked_pow(exp) {
            Ok(value) => value,
            Err(_) => Self::MAX,
        }
    }
}

impl Fraction<Uint128> for Decimal {
    #[inline]
    fn numerator(&self) -> Uint128 {
        self.0
    }

    #[inline]
    fn denominator(&self) -> Uint128 {
        Self::DECIMAL_FRACTIONAL
    }

    /// Returns the multiplicative inverse `1/d` for decimal `d`.
    ///
    /// If `d` is zero, none is returned.
    fn inv(&self) -> Option<Self> {
        if self.is_zero() {
            None
        } else {
            // Let self be p/q with p = self.0 and q = DECIMAL_FRACTIONAL.
            // Now we calculate the inverse a/b = q/p such that b = DECIMAL_FRACTIONAL. Then
            // `a = DECIMAL_FRACTIONAL*DECIMAL_FRACTIONAL / self.0`.
            Some(Decimal(Self::DECIMAL_FRACTIONAL_SQUARED / self.0))
        }
    }
}

impl FromStr for Decimal {
    type Err = StdError;

    /// Converts the decimal string to a Decimal
    /// Possible inputs: "1.23", "1", "000012", "1.123000000"
    /// Disallowed: "", ".23"
    ///
    /// This never performs any kind of rounding.
    /// More than DECIMAL_PLACES fractional digits, even zeros, result in an error.
    fn from_str(input: &str) -> Result<Self, Self::Err> {
        let mut parts_iter = input.split('.');

        let whole_part = parts_iter.next().unwrap(); // split always returns at least one element
        let whole = whole_part
            .parse::<Uint128>()
            .map_err(|_| StdError::generic_err("Error parsing whole"))?;
        let mut atomics = whole
            .checked_mul(Self::DECIMAL_FRACTIONAL)
            .map_err(|_| StdError::generic_err("Value too big"))?;

        if let Some(fractional_part) = parts_iter.next() {
            let fractional = fractional_part
                .parse::<Uint128>()
                .map_err(|_| StdError::generic_err("Error parsing fractional"))?;
            let exp = (Self::DECIMAL_PLACES.checked_sub(fractional_part.len() as u32)).ok_or_else(
                || {
                    StdError::generic_err(format!(
                        "Cannot parse more than {} fractional digits",
                        Self::DECIMAL_PLACES
                    ))
                },
            )?;
            debug_assert!(exp <= Self::DECIMAL_PLACES);
            let fractional_factor = Uint128::from(10u128.pow(exp));
            atomics = atomics
                .checked_add(
                    // The inner multiplication can't overflow because
                    // fractional < 10^DECIMAL_PLACES && fractional_factor <= 10^DECIMAL_PLACES
                    fractional.checked_mul(fractional_factor).unwrap(),
                )
                .map_err(|_| StdError::generic_err("Value too big"))?;
        }

        if parts_iter.next().is_some() {
            return Err(StdError::generic_err("Unexpected number of dots"));
        }

        Ok(Decimal(atomics))
    }
}

impl fmt::Display for Decimal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let whole = (self.0) / Self::DECIMAL_FRACTIONAL;
        let fractional = (self.0).checked_rem(Self::DECIMAL_FRACTIONAL).unwrap();

        if fractional.is_zero() {
            write!(f, "{}", whole)
        } else {
            let fractional_string = format!(
                "{:0>padding$}",
                fractional,
                padding = Self::DECIMAL_PLACES as usize
            );
            f.write_str(&whole.to_string())?;
            f.write_char('.')?;
            f.write_str(fractional_string.trim_end_matches('0'))?;
            Ok(())
        }
    }
}

impl Add for Decimal {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        Decimal(self.0 + other.0)
    }
}
forward_ref_binop!(impl Add, add for Decimal, Decimal);

impl AddAssign for Decimal {
    fn add_assign(&mut self, rhs: Decimal) {
        *self = *self + rhs;
    }
}
forward_ref_op_assign!(impl AddAssign, add_assign for Decimal, Decimal);

impl Sub for Decimal {
    type Output = Self;

    fn sub(self, other: Self) -> Self {
        Decimal(self.0 - other.0)
    }
}
forward_ref_binop!(impl Sub, sub for Decimal, Decimal);

impl SubAssign for Decimal {
    fn sub_assign(&mut self, rhs: Decimal) {
        *self = *self - rhs;
    }
}
forward_ref_op_assign!(impl SubAssign, sub_assign for Decimal, Decimal);

impl Mul for Decimal {
    type Output = Self;

    #[allow(clippy::suspicious_arithmetic_impl)]
    fn mul(self, other: Self) -> Self {
        // Decimals are fractions. We can multiply two decimals a and b
        // via
        //       (a.numerator() * b.numerator()) / (a.denominator() * b.denominator())
        //     = (a.numerator() * b.numerator()) / a.denominator() / b.denominator()

        let result_as_uint256 = self.numerator().full_mul(other.numerator())
            / Uint256::from_uint128(Self::DECIMAL_FRACTIONAL); // from_uint128 is a const method and should be "free"
        match result_as_uint256.try_into() {
            Ok(result) => Self(result),
            Err(_) => panic!("attempt to multiply with overflow"),
        }
    }
}
forward_ref_binop!(impl Mul, mul for Decimal, Decimal);

impl MulAssign for Decimal {
    fn mul_assign(&mut self, rhs: Decimal) {
        *self = *self * rhs;
    }
}
forward_ref_op_assign!(impl MulAssign, mul_assign for Decimal, Decimal);

/// Both d*u and u*d with d: Decimal and u: Uint128 returns an Uint128. There is no
/// specific reason for this decision other than the initial use cases we have. If you
/// need a Decimal result for the same calculation, use Decimal(d*u) or Decimal(u*d).
impl Mul<Decimal> for Uint128 {
    type Output = Self;

    #[allow(clippy::suspicious_arithmetic_impl)]
    fn mul(self, rhs: Decimal) -> Self::Output {
        // 0*a and b*0 is always 0
        if self.is_zero() || rhs.is_zero() {
            return Uint128::zero();
        }
        self.multiply_ratio(rhs.0, Decimal::DECIMAL_FRACTIONAL)
    }
}

impl Mul<Uint128> for Decimal {
    type Output = Uint128;

    fn mul(self, rhs: Uint128) -> Self::Output {
        rhs * self
    }
}

impl Div for Decimal {
    type Output = Self;

    fn div(self, other: Self) -> Self {
        match Decimal::checked_from_ratio(self.numerator(), other.numerator()) {
            Ok(ratio) => ratio,
            Err(CheckedFromRatioError::DivideByZero) => {
                panic!("Division failed - denominator must not be zero")
            }
            Err(CheckedFromRatioError::Overflow) => {
                panic!("Division failed - multiplication overflow")
            }
        }
    }
}
forward_ref_binop!(impl Div, div for Decimal, Decimal);

impl DivAssign for Decimal {
    fn div_assign(&mut self, rhs: Decimal) {
        *self = *self / rhs;
    }
}
forward_ref_op_assign!(impl DivAssign, div_assign for Decimal, Decimal);

impl Div<Uint128> for Decimal {
    type Output = Self;

    fn div(self, rhs: Uint128) -> Self::Output {
        Decimal(self.0 / rhs)
    }
}

impl DivAssign<Uint128> for Decimal {
    fn div_assign(&mut self, rhs: Uint128) {
        self.0 /= rhs;
    }
}

impl Rem for Decimal {
    type Output = Self;

    /// # Panics
    ///
    /// This operation will panic if `rhs` is zero
    #[inline]
    fn rem(self, rhs: Self) -> Self {
        Self(self.0.rem(rhs.0))
    }
}
forward_ref_binop!(impl Rem, rem for Decimal, Decimal);

impl RemAssign<Decimal> for Decimal {
    fn rem_assign(&mut self, rhs: Decimal) {
        *self = *self % rhs;
    }
}
forward_ref_op_assign!(impl RemAssign, rem_assign for Decimal, Decimal);

impl<A> std::iter::Sum<A> for Decimal
where
    Self: Add<A, Output = Self>,
{
    fn sum<I: Iterator<Item = A>>(iter: I) -> Self {
        iter.fold(Self::zero(), Add::add)
    }
}

/// Serializes as a decimal string
impl Serialize for Decimal {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: ser::Serializer,
    {
        serializer.serialize_str(&self.to_string())
    }
}

/// Deserializes as a base64 string
impl<'de> Deserialize<'de> for Decimal {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        deserializer.deserialize_str(DecimalVisitor)
    }
}

struct DecimalVisitor;

impl<'de> de::Visitor<'de> for DecimalVisitor {
    type Value = Decimal;

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str("string-encoded decimal")
    }

    fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
    where
        E: de::Error,
    {
        match Decimal::from_str(v) {
            Ok(d) => Ok(d),
            Err(e) => Err(E::custom(format!("Error parsing decimal '{}': {}", v, e))),
        }
    }
}

impl PartialEq<&Decimal> for Decimal {
    fn eq(&self, rhs: &&Decimal) -> bool {
        self == *rhs
    }
}

impl PartialEq<Decimal> for &Decimal {
    fn eq(&self, rhs: &Decimal) -> bool {
        *self == rhs
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{from_slice, to_vec};

    fn dec(input: &str) -> Decimal {
        Decimal::from_str(input).unwrap()
    }

    #[test]
    fn decimal_new() {
        let expected = Uint128::from(300u128);
        assert_eq!(Decimal::new(expected).0, expected);
    }

    #[test]
    fn decimal_raw() {
        let value = 300u128;
        assert_eq!(Decimal::raw(value).0.u128(), value);
    }

    #[test]
    fn decimal_one() {
        let value = Decimal::one();
        assert_eq!(value.0, Decimal::DECIMAL_FRACTIONAL);
    }

    #[test]
    fn decimal_zero() {
        let value = Decimal::zero();
        assert!(value.0.is_zero());
    }

    #[test]
    fn decimal_percent() {
        let value = Decimal::percent(50);
        assert_eq!(value.0, Decimal::DECIMAL_FRACTIONAL / Uint128::from(2u8));
    }

    #[test]
    fn decimal_permille() {
        let value = Decimal::permille(125);
        assert_eq!(value.0, Decimal::DECIMAL_FRACTIONAL / Uint128::from(8u8));
    }

    #[test]
    fn decimal_from_atomics_works() {
        let one = Decimal::one();
        let two = one + one;

        assert_eq!(Decimal::from_atomics(1u128, 0).unwrap(), one);
        assert_eq!(Decimal::from_atomics(10u128, 1).unwrap(), one);
        assert_eq!(Decimal::from_atomics(100u128, 2).unwrap(), one);
        assert_eq!(Decimal::from_atomics(1000u128, 3).unwrap(), one);
        assert_eq!(
            Decimal::from_atomics(1000000000000000000u128, 18).unwrap(),
            one
        );
        assert_eq!(
            Decimal::from_atomics(10000000000000000000u128, 19).unwrap(),
            one
        );
        assert_eq!(
            Decimal::from_atomics(100000000000000000000u128, 20).unwrap(),
            one
        );

        assert_eq!(Decimal::from_atomics(2u128, 0).unwrap(), two);
        assert_eq!(Decimal::from_atomics(20u128, 1).unwrap(), two);
        assert_eq!(Decimal::from_atomics(200u128, 2).unwrap(), two);
        assert_eq!(Decimal::from_atomics(2000u128, 3).unwrap(), two);
        assert_eq!(
            Decimal::from_atomics(2000000000000000000u128, 18).unwrap(),
            two
        );
        assert_eq!(
            Decimal::from_atomics(20000000000000000000u128, 19).unwrap(),
            two
        );
        assert_eq!(
            Decimal::from_atomics(200000000000000000000u128, 20).unwrap(),
            two
        );

        // Cuts decimal digits (20 provided but only 18 can be stored)
        assert_eq!(
            Decimal::from_atomics(4321u128, 20).unwrap(),
            Decimal::from_str("0.000000000000000043").unwrap()
        );
        assert_eq!(
            Decimal::from_atomics(6789u128, 20).unwrap(),
            Decimal::from_str("0.000000000000000067").unwrap()
        );
        assert_eq!(
            Decimal::from_atomics(u128::MAX, 38).unwrap(),
            Decimal::from_str("3.402823669209384634").unwrap()
        );
        assert_eq!(
            Decimal::from_atomics(u128::MAX, 39).unwrap(),
            Decimal::from_str("0.340282366920938463").unwrap()
        );
        assert_eq!(
            Decimal::from_atomics(u128::MAX, 45).unwrap(),
            Decimal::from_str("0.000000340282366920").unwrap()
        );
        assert_eq!(
            Decimal::from_atomics(u128::MAX, 51).unwrap(),
            Decimal::from_str("0.000000000000340282").unwrap()
        );
        assert_eq!(
            Decimal::from_atomics(u128::MAX, 56).unwrap(),
            Decimal::from_str("0.000000000000000003").unwrap()
        );
        assert_eq!(
            Decimal::from_atomics(u128::MAX, 57).unwrap(),
            Decimal::from_str("0.000000000000000000").unwrap()
        );
        assert_eq!(
            Decimal::from_atomics(u128::MAX, u32::MAX).unwrap(),
            Decimal::from_str("0.000000000000000000").unwrap()
        );

        // Can be used with max value
        let max = Decimal::MAX;
        assert_eq!(
            Decimal::from_atomics(max.atomics(), max.decimal_places()).unwrap(),
            max
        );

        // Overflow is only possible with digits < 18
        let result = Decimal::from_atomics(u128::MAX, 17);
        assert_eq!(result.unwrap_err(), DecimalRangeExceeded);
    }

    #[test]
    fn decimal_from_ratio_works() {
        // 1.0
        assert_eq!(Decimal::from_ratio(1u128, 1u128), Decimal::one());
        assert_eq!(Decimal::from_ratio(53u128, 53u128), Decimal::one());
        assert_eq!(Decimal::from_ratio(125u128, 125u128), Decimal::one());

        // 1.5
        assert_eq!(Decimal::from_ratio(3u128, 2u128), Decimal::percent(150));
        assert_eq!(Decimal::from_ratio(150u128, 100u128), Decimal::percent(150));
        assert_eq!(Decimal::from_ratio(333u128, 222u128), Decimal::percent(150));

        // 0.125
        assert_eq!(Decimal::from_ratio(1u64, 8u64), Decimal::permille(125));
        assert_eq!(Decimal::from_ratio(125u64, 1000u64), Decimal::permille(125));

        // 1/3 (result floored)
        assert_eq!(
            Decimal::from_ratio(1u64, 3u64),
            Decimal(Uint128::from(333_333_333_333_333_333u128))
        );

        // 2/3 (result floored)
        assert_eq!(
            Decimal::from_ratio(2u64, 3u64),
            Decimal(Uint128::from(666_666_666_666_666_666u128))
        );

        // large inputs
        assert_eq!(Decimal::from_ratio(0u128, u128::MAX), Decimal::zero());
        assert_eq!(Decimal::from_ratio(u128::MAX, u128::MAX), Decimal::one());
        // 340282366920938463463 is the largest integer <= Decimal::MAX
        assert_eq!(
            Decimal::from_ratio(340282366920938463463u128, 1u128),
            Decimal::from_str("340282366920938463463").unwrap()
        );
    }

    #[test]
    #[should_panic(expected = "Denominator must not be zero")]
    fn decimal_from_ratio_panics_for_zero_denominator() {
        Decimal::from_ratio(1u128, 0u128);
    }

    #[test]
    #[should_panic(expected = "Multiplication overflow")]
    fn decimal_from_ratio_panics_for_mul_overflow() {
        Decimal::from_ratio(u128::MAX, 1u128);
    }

    #[test]
    fn decimal_checked_from_ratio_does_not_panic() {
        assert_eq!(
            Decimal::checked_from_ratio(1u128, 0u128),
            Err(CheckedFromRatioError::DivideByZero)
        );

        assert_eq!(
            Decimal::checked_from_ratio(u128::MAX, 1u128),
            Err(CheckedFromRatioError::Overflow)
        );
    }

    #[test]
    fn decimal_implements_fraction() {
        let fraction = Decimal::from_str("1234.567").unwrap();
        assert_eq!(
            fraction.numerator(),
            Uint128::from(1_234_567_000_000_000_000_000u128)
        );
        assert_eq!(
            fraction.denominator(),
            Uint128::from(1_000_000_000_000_000_000u128)
        );
    }

    #[test]
    fn decimal_from_str_works() {
        // Integers
        assert_eq!(Decimal::from_str("0").unwrap(), Decimal::percent(0));
        assert_eq!(Decimal::from_str("1").unwrap(), Decimal::percent(100));
        assert_eq!(Decimal::from_str("5").unwrap(), Decimal::percent(500));
        assert_eq!(Decimal::from_str("42").unwrap(), Decimal::percent(4200));
        assert_eq!(Decimal::from_str("000").unwrap(), Decimal::percent(0));
        assert_eq!(Decimal::from_str("001").unwrap(), Decimal::percent(100));
        assert_eq!(Decimal::from_str("005").unwrap(), Decimal::percent(500));
        assert_eq!(Decimal::from_str("0042").unwrap(), Decimal::percent(4200));

        // Decimals
        assert_eq!(Decimal::from_str("1.0").unwrap(), Decimal::percent(100));
        assert_eq!(Decimal::from_str("1.5").unwrap(), Decimal::percent(150));
        assert_eq!(Decimal::from_str("0.5").unwrap(), Decimal::percent(50));
        assert_eq!(Decimal::from_str("0.123").unwrap(), Decimal::permille(123));

        assert_eq!(Decimal::from_str("40.00").unwrap(), Decimal::percent(4000));
        assert_eq!(Decimal::from_str("04.00").unwrap(), Decimal::percent(400));
        assert_eq!(Decimal::from_str("00.40").unwrap(), Decimal::percent(40));
        assert_eq!(Decimal::from_str("00.04").unwrap(), Decimal::percent(4));

        // Can handle DECIMAL_PLACES fractional digits
        assert_eq!(
            Decimal::from_str("7.123456789012345678").unwrap(),
            Decimal(Uint128::from(7123456789012345678u128))
        );
        assert_eq!(
            Decimal::from_str("7.999999999999999999").unwrap(),
            Decimal(Uint128::from(7999999999999999999u128))
        );

        // Works for documented max value
        assert_eq!(
            Decimal::from_str("340282366920938463463.374607431768211455").unwrap(),
            Decimal::MAX
        );
    }

    #[test]
    fn decimal_from_str_errors_for_broken_whole_part() {
        match Decimal::from_str("").unwrap_err() {
            StdError::GenericErr { msg, .. } => assert_eq!(msg, "Error parsing whole"),
            e => panic!("Unexpected error: {:?}", e),
        }

        match Decimal::from_str(" ").unwrap_err() {
            StdError::GenericErr { msg, .. } => assert_eq!(msg, "Error parsing whole"),
            e => panic!("Unexpected error: {:?}", e),
        }

        match Decimal::from_str("-1").unwrap_err() {
            StdError::GenericErr { msg, .. } => assert_eq!(msg, "Error parsing whole"),
            e => panic!("Unexpected error: {:?}", e),
        }
    }

    #[test]
    fn decimal_from_str_errors_for_broken_fractinal_part() {
        match Decimal::from_str("1.").unwrap_err() {
            StdError::GenericErr { msg, .. } => assert_eq!(msg, "Error parsing fractional"),
            e => panic!("Unexpected error: {:?}", e),
        }

        match Decimal::from_str("1. ").unwrap_err() {
            StdError::GenericErr { msg, .. } => assert_eq!(msg, "Error parsing fractional"),
            e => panic!("Unexpected error: {:?}", e),
        }

        match Decimal::from_str("1.e").unwrap_err() {
            StdError::GenericErr { msg, .. } => assert_eq!(msg, "Error parsing fractional"),
            e => panic!("Unexpected error: {:?}", e),
        }

        match Decimal::from_str("1.2e3").unwrap_err() {
            StdError::GenericErr { msg, .. } => assert_eq!(msg, "Error parsing fractional"),
            e => panic!("Unexpected error: {:?}", e),
        }
    }

    #[test]
    fn decimal_from_str_errors_for_more_than_18_fractional_digits() {
        match Decimal::from_str("7.1234567890123456789").unwrap_err() {
            StdError::GenericErr { msg, .. } => {
                assert_eq!(msg, "Cannot parse more than 18 fractional digits",)
            }
            e => panic!("Unexpected error: {:?}", e),
        }

        // No special rules for trailing zeros. This could be changed but adds gas cost for the happy path.
        match Decimal::from_str("7.1230000000000000000").unwrap_err() {
            StdError::GenericErr { msg, .. } => {
                assert_eq!(msg, "Cannot parse more than 18 fractional digits")
            }
            e => panic!("Unexpected error: {:?}", e),
        }
    }

    #[test]
    fn decimal_from_str_errors_for_invalid_number_of_dots() {
        match Decimal::from_str("1.2.3").unwrap_err() {
            StdError::GenericErr { msg, .. } => assert_eq!(msg, "Unexpected number of dots"),
            e => panic!("Unexpected error: {:?}", e),
        }

        match Decimal::from_str("1.2.3.4").unwrap_err() {
            StdError::GenericErr { msg, .. } => assert_eq!(msg, "Unexpected number of dots"),
            e => panic!("Unexpected error: {:?}", e),
        }
    }

    #[test]
    fn decimal_from_str_errors_for_more_than_max_value() {
        // Integer
        match Decimal::from_str("340282366920938463464").unwrap_err() {
            StdError::GenericErr { msg, .. } => assert_eq!(msg, "Value too big"),
            e => panic!("Unexpected error: {:?}", e),
        }

        // Decimal
        match Decimal::from_str("340282366920938463464.0").unwrap_err() {
            StdError::GenericErr { msg, .. } => assert_eq!(msg, "Value too big"),
            e => panic!("Unexpected error: {:?}", e),
        }
        match Decimal::from_str("340282366920938463463.374607431768211456").unwrap_err() {
            StdError::GenericErr { msg, .. } => assert_eq!(msg, "Value too big"),
            e => panic!("Unexpected error: {:?}", e),
        }
    }

    #[test]
    fn decimal_atomics_works() {
        let zero = Decimal::zero();
        let one = Decimal::one();
        let half = Decimal::percent(50);
        let two = Decimal::percent(200);
        let max = Decimal::MAX;

        assert_eq!(zero.atomics(), Uint128::new(0));
        assert_eq!(one.atomics(), Uint128::new(1000000000000000000));
        assert_eq!(half.atomics(), Uint128::new(500000000000000000));
        assert_eq!(two.atomics(), Uint128::new(2000000000000000000));
        assert_eq!(max.atomics(), Uint128::MAX);
    }

    #[test]
    fn decimal_decimal_places_works() {
        let zero = Decimal::zero();
        let one = Decimal::one();
        let half = Decimal::percent(50);
        let two = Decimal::percent(200);
        let max = Decimal::MAX;

        assert_eq!(zero.decimal_places(), 18);
        assert_eq!(one.decimal_places(), 18);
        assert_eq!(half.decimal_places(), 18);
        assert_eq!(two.decimal_places(), 18);
        assert_eq!(max.decimal_places(), 18);
    }

    #[test]
    fn decimal_is_zero_works() {
        assert!(Decimal::zero().is_zero());
        assert!(Decimal::percent(0).is_zero());
        assert!(Decimal::permille(0).is_zero());

        assert!(!Decimal::one().is_zero());
        assert!(!Decimal::percent(123).is_zero());
        assert!(!Decimal::permille(1234).is_zero());
    }

    #[test]
    fn decimal_inv_works() {
        // d = 0
        assert_eq!(Decimal::zero().inv(), None);

        // d == 1
        assert_eq!(Decimal::one().inv(), Some(Decimal::one()));

        // d > 1 exact
        assert_eq!(
            Decimal::from_str("2").unwrap().inv(),
            Some(Decimal::from_str("0.5").unwrap())
        );
        assert_eq!(
            Decimal::from_str("20").unwrap().inv(),
            Some(Decimal::from_str("0.05").unwrap())
        );
        assert_eq!(
            Decimal::from_str("200").unwrap().inv(),
            Some(Decimal::from_str("0.005").unwrap())
        );
        assert_eq!(
            Decimal::from_str("2000").unwrap().inv(),
            Some(Decimal::from_str("0.0005").unwrap())
        );

        // d > 1 rounded
        assert_eq!(
            Decimal::from_str("3").unwrap().inv(),
            Some(Decimal::from_str("0.333333333333333333").unwrap())
        );
        assert_eq!(
            Decimal::from_str("6").unwrap().inv(),
            Some(Decimal::from_str("0.166666666666666666").unwrap())
        );

        // d < 1 exact
        assert_eq!(
            Decimal::from_str("0.5").unwrap().inv(),
            Some(Decimal::from_str("2").unwrap())
        );
        assert_eq!(
            Decimal::from_str("0.05").unwrap().inv(),
            Some(Decimal::from_str("20").unwrap())
        );
        assert_eq!(
            Decimal::from_str("0.005").unwrap().inv(),
            Some(Decimal::from_str("200").unwrap())
        );
        assert_eq!(
            Decimal::from_str("0.0005").unwrap().inv(),
            Some(Decimal::from_str("2000").unwrap())
        );
    }

    #[test]
    #[allow(clippy::op_ref)]
    fn decimal_add_works() {
        let value = Decimal::one() + Decimal::percent(50); // 1.5
        assert_eq!(
            value.0,
            Decimal::DECIMAL_FRACTIONAL * Uint128::from(3u8) / Uint128::from(2u8)
        );

        assert_eq!(
            Decimal::percent(5) + Decimal::percent(4),
            Decimal::percent(9)
        );
        assert_eq!(Decimal::percent(5) + Decimal::zero(), Decimal::percent(5));
        assert_eq!(Decimal::zero() + Decimal::zero(), Decimal::zero());

        // works for refs
        let a = Decimal::percent(15);
        let b = Decimal::percent(25);
        let expected = Decimal::percent(40);
        assert_eq!(a + b, expected);
        assert_eq!(&a + b, expected);
        assert_eq!(a + &b, expected);
        assert_eq!(&a + &b, expected);
    }

    #[test]
    #[should_panic(expected = "attempt to add with overflow")]
    fn decimal_add_overflow_panics() {
        let _value = Decimal::MAX + Decimal::percent(50);
    }

    #[test]
    fn decimal_add_assign_works() {
        let mut a = Decimal::percent(30);
        a += Decimal::percent(20);
        assert_eq!(a, Decimal::percent(50));

        // works for refs
        let mut a = Decimal::percent(15);
        let b = Decimal::percent(3);
        let expected = Decimal::percent(18);
        a += &b;
        assert_eq!(a, expected);
    }

    #[test]
    #[allow(clippy::op_ref)]
    fn decimal_sub_works() {
        let value = Decimal::one() - Decimal::percent(50); // 0.5
        assert_eq!(value.0, Decimal::DECIMAL_FRACTIONAL / Uint128::from(2u8));

        assert_eq!(
            Decimal::percent(9) - Decimal::percent(4),
            Decimal::percent(5)
        );
        assert_eq!(Decimal::percent(16) - Decimal::zero(), Decimal::percent(16));
        assert_eq!(Decimal::percent(16) - Decimal::percent(16), Decimal::zero());
        assert_eq!(Decimal::zero() - Decimal::zero(), Decimal::zero());

        // works for refs
        let a = Decimal::percent(13);
        let b = Decimal::percent(6);
        let expected = Decimal::percent(7);
        assert_eq!(a - b, expected);
        assert_eq!(&a - b, expected);
        assert_eq!(a - &b, expected);
        assert_eq!(&a - &b, expected);
    }

    #[test]
    #[should_panic(expected = "attempt to subtract with overflow")]
    fn decimal_sub_overflow_panics() {
        let _value = Decimal::zero() - Decimal::percent(50);
    }

    #[test]
    fn decimal_sub_assign_works() {
        let mut a = Decimal::percent(20);
        a -= Decimal::percent(2);
        assert_eq!(a, Decimal::percent(18));

        // works for refs
        let mut a = Decimal::percent(33);
        let b = Decimal::percent(13);
        let expected = Decimal::percent(20);
        a -= &b;
        assert_eq!(a, expected);
    }

    #[test]
    #[allow(clippy::op_ref)]
    fn decimal_implements_mul() {
        let one = Decimal::one();
        let two = one + one;
        let half = Decimal::percent(50);

        // 1*x and x*1
        assert_eq!(one * Decimal::percent(0), Decimal::percent(0));
        assert_eq!(one * Decimal::percent(1), Decimal::percent(1));
        assert_eq!(one * Decimal::percent(10), Decimal::percent(10));
        assert_eq!(one * Decimal::percent(100), Decimal::percent(100));
        assert_eq!(one * Decimal::percent(1000), Decimal::percent(1000));
        assert_eq!(one * Decimal::MAX, Decimal::MAX);
        assert_eq!(Decimal::percent(0) * one, Decimal::percent(0));
        assert_eq!(Decimal::percent(1) * one, Decimal::percent(1));
        assert_eq!(Decimal::percent(10) * one, Decimal::percent(10));
        assert_eq!(Decimal::percent(100) * one, Decimal::percent(100));
        assert_eq!(Decimal::percent(1000) * one, Decimal::percent(1000));
        assert_eq!(Decimal::MAX * one, Decimal::MAX);

        // double
        assert_eq!(two * Decimal::percent(0), Decimal::percent(0));
        assert_eq!(two * Decimal::percent(1), Decimal::percent(2));
        assert_eq!(two * Decimal::percent(10), Decimal::percent(20));
        assert_eq!(two * Decimal::percent(100), Decimal::percent(200));
        assert_eq!(two * Decimal::percent(1000), Decimal::percent(2000));
        assert_eq!(Decimal::percent(0) * two, Decimal::percent(0));
        assert_eq!(Decimal::percent(1) * two, Decimal::percent(2));
        assert_eq!(Decimal::percent(10) * two, Decimal::percent(20));
        assert_eq!(Decimal::percent(100) * two, Decimal::percent(200));
        assert_eq!(Decimal::percent(1000) * two, Decimal::percent(2000));

        // half
        assert_eq!(half * Decimal::percent(0), Decimal::percent(0));
        assert_eq!(half * Decimal::percent(1), Decimal::permille(5));
        assert_eq!(half * Decimal::percent(10), Decimal::percent(5));
        assert_eq!(half * Decimal::percent(100), Decimal::percent(50));
        assert_eq!(half * Decimal::percent(1000), Decimal::percent(500));
        assert_eq!(Decimal::percent(0) * half, Decimal::percent(0));
        assert_eq!(Decimal::percent(1) * half, Decimal::permille(5));
        assert_eq!(Decimal::percent(10) * half, Decimal::percent(5));
        assert_eq!(Decimal::percent(100) * half, Decimal::percent(50));
        assert_eq!(Decimal::percent(1000) * half, Decimal::percent(500));

        // Move left
        let a = dec("123.127726548762582");
        assert_eq!(a * dec("1"), dec("123.127726548762582"));
        assert_eq!(a * dec("10"), dec("1231.27726548762582"));
        assert_eq!(a * dec("100"), dec("12312.7726548762582"));
        assert_eq!(a * dec("1000"), dec("123127.726548762582"));
        assert_eq!(a * dec("1000000"), dec("123127726.548762582"));
        assert_eq!(a * dec("1000000000"), dec("123127726548.762582"));
        assert_eq!(a * dec("1000000000000"), dec("123127726548762.582"));
        assert_eq!(a * dec("1000000000000000"), dec("123127726548762582"));
        assert_eq!(a * dec("1000000000000000000"), dec("123127726548762582000"));
        assert_eq!(dec("1") * a, dec("123.127726548762582"));
        assert_eq!(dec("10") * a, dec("1231.27726548762582"));
        assert_eq!(dec("100") * a, dec("12312.7726548762582"));
        assert_eq!(dec("1000") * a, dec("123127.726548762582"));
        assert_eq!(dec("1000000") * a, dec("123127726.548762582"));
        assert_eq!(dec("1000000000") * a, dec("123127726548.762582"));
        assert_eq!(dec("1000000000000") * a, dec("123127726548762.582"));
        assert_eq!(dec("1000000000000000") * a, dec("123127726548762582"));
        assert_eq!(dec("1000000000000000000") * a, dec("123127726548762582000"));

        // Move right
        let max = Decimal::MAX;
        assert_eq!(
            max * dec("1.0"),
            dec("340282366920938463463.374607431768211455")
        );
        assert_eq!(
            max * dec("0.1"),
            dec("34028236692093846346.337460743176821145")
        );
        assert_eq!(
            max * dec("0.01"),
            dec("3402823669209384634.633746074317682114")
        );
        assert_eq!(
            max * dec("0.001"),
            dec("340282366920938463.463374607431768211")
        );
        assert_eq!(
            max * dec("0.000001"),
            dec("340282366920938.463463374607431768")
        );
        assert_eq!(
            max * dec("0.000000001"),
            dec("340282366920.938463463374607431")
        );
        assert_eq!(
            max * dec("0.000000000001"),
            dec("340282366.920938463463374607")
        );
        assert_eq!(
            max * dec("0.000000000000001"),
            dec("340282.366920938463463374")
        );
        assert_eq!(
            max * dec("0.000000000000000001"),
            dec("340.282366920938463463")
        );

        // works for refs
        let a = Decimal::percent(20);
        let b = Decimal::percent(30);
        let expected = Decimal::percent(6);
        assert_eq!(a * b, expected);
        assert_eq!(&a * b, expected);
        assert_eq!(a * &b, expected);
        assert_eq!(&a * &b, expected);
    }

    #[test]
    fn decimal_mul_assign_works() {
        let mut a = Decimal::percent(15);
        a *= Decimal::percent(60);
        assert_eq!(a, Decimal::percent(9));

        // works for refs
        let mut a = Decimal::percent(50);
        let b = Decimal::percent(20);
        a *= &b;
        assert_eq!(a, Decimal::percent(10));
    }

    #[test]
    #[should_panic(expected = "attempt to multiply with overflow")]
    fn decimal_mul_overflow_panics() {
        let _value = Decimal::MAX * Decimal::percent(101);
    }

    #[test]
    fn decimal_checked_mul() {
        let test_data = [
            (Decimal::zero(), Decimal::zero()),
            (Decimal::zero(), Decimal::one()),
            (Decimal::one(), Decimal::zero()),
            (Decimal::percent(10), Decimal::zero()),
            (Decimal::percent(10), Decimal::percent(5)),
            (Decimal::MAX, Decimal::one()),
            (Decimal::MAX / Uint128::new(2), Decimal::percent(200)),
            (Decimal::permille(6), Decimal::permille(13)),
        ];

        // The regular std::ops::Mul is our source of truth for these tests.
        for (x, y) in test_data.into_iter() {
            assert_eq!(x * y, x.checked_mul(y).unwrap());
        }
    }

    #[test]
    fn decimal_checked_mul_overflow() {
        assert_eq!(
            Decimal::MAX.checked_mul(Decimal::percent(200)),
            Err(OverflowError {
                operation: crate::OverflowOperation::Mul,
                operand1: Decimal::MAX.to_string(),
                operand2: Decimal::percent(200).to_string(),
            })
        );
    }

    #[test]
    // in this test the Decimal is on the right
    fn uint128_decimal_multiply() {
        // a*b
        let left = Uint128::new(300);
        let right = Decimal::one() + Decimal::percent(50); // 1.5
        assert_eq!(left * right, Uint128::new(450));

        // a*0
        let left = Uint128::new(300);
        let right = Decimal::zero();
        assert_eq!(left * right, Uint128::new(0));

        // 0*a
        let left = Uint128::new(0);
        let right = Decimal::one() + Decimal::percent(50); // 1.5
        assert_eq!(left * right, Uint128::new(0));
    }

    #[test]
    // in this test the Decimal is on the left
    fn decimal_uint128_multiply() {
        // a*b
        let left = Decimal::one() + Decimal::percent(50); // 1.5
        let right = Uint128::new(300);
        assert_eq!(left * right, Uint128::new(450));

        // 0*a
        let left = Decimal::zero();
        let right = Uint128::new(300);
        assert_eq!(left * right, Uint128::new(0));

        // a*0
        let left = Decimal::one() + Decimal::percent(50); // 1.5
        let right = Uint128::new(0);
        assert_eq!(left * right, Uint128::new(0));
    }

    #[test]
    #[allow(clippy::op_ref)]
    fn decimal_implements_div() {
        let one = Decimal::one();
        let two = one + one;
        let half = Decimal::percent(50);

        // 1/x and x/1
        assert_eq!(one / Decimal::percent(1), Decimal::percent(10_000));
        assert_eq!(one / Decimal::percent(10), Decimal::percent(1_000));
        assert_eq!(one / Decimal::percent(100), Decimal::percent(100));
        assert_eq!(one / Decimal::percent(1000), Decimal::percent(10));
        assert_eq!(Decimal::percent(0) / one, Decimal::percent(0));
        assert_eq!(Decimal::percent(1) / one, Decimal::percent(1));
        assert_eq!(Decimal::percent(10) / one, Decimal::percent(10));
        assert_eq!(Decimal::percent(100) / one, Decimal::percent(100));
        assert_eq!(Decimal::percent(1000) / one, Decimal::percent(1000));

        // double
        assert_eq!(two / Decimal::percent(1), Decimal::percent(20_000));
        assert_eq!(two / Decimal::percent(10), Decimal::percent(2_000));
        assert_eq!(two / Decimal::percent(100), Decimal::percent(200));
        assert_eq!(two / Decimal::percent(1000), Decimal::percent(20));
        assert_eq!(Decimal::percent(0) / two, Decimal::percent(0));
        assert_eq!(Decimal::percent(1) / two, dec("0.005"));
        assert_eq!(Decimal::percent(10) / two, Decimal::percent(5));
        assert_eq!(Decimal::percent(100) / two, Decimal::percent(50));
        assert_eq!(Decimal::percent(1000) / two, Decimal::percent(500));

        // half
        assert_eq!(half / Decimal::percent(1), Decimal::percent(5_000));
        assert_eq!(half / Decimal::percent(10), Decimal::percent(500));
        assert_eq!(half / Decimal::percent(100), Decimal::percent(50));
        assert_eq!(half / Decimal::percent(1000), Decimal::percent(5));
        assert_eq!(Decimal::percent(0) / half, Decimal::percent(0));
        assert_eq!(Decimal::percent(1) / half, Decimal::percent(2));
        assert_eq!(Decimal::percent(10) / half, Decimal::percent(20));
        assert_eq!(Decimal::percent(100) / half, Decimal::percent(200));
        assert_eq!(Decimal::percent(1000) / half, Decimal::percent(2000));

        // Move right
        let a = dec("123127726548762582");
        assert_eq!(a / dec("1"), dec("123127726548762582"));
        assert_eq!(a / dec("10"), dec("12312772654876258.2"));
        assert_eq!(a / dec("100"), dec("1231277265487625.82"));
        assert_eq!(a / dec("1000"), dec("123127726548762.582"));
        assert_eq!(a / dec("1000000"), dec("123127726548.762582"));
        assert_eq!(a / dec("1000000000"), dec("123127726.548762582"));
        assert_eq!(a / dec("1000000000000"), dec("123127.726548762582"));
        assert_eq!(a / dec("1000000000000000"), dec("123.127726548762582"));
        assert_eq!(a / dec("1000000000000000000"), dec("0.123127726548762582"));
        assert_eq!(dec("1") / a, dec("0.000000000000000008"));
        assert_eq!(dec("10") / a, dec("0.000000000000000081"));
        assert_eq!(dec("100") / a, dec("0.000000000000000812"));
        assert_eq!(dec("1000") / a, dec("0.000000000000008121"));
        assert_eq!(dec("1000000") / a, dec("0.000000000008121647"));
        assert_eq!(dec("1000000000") / a, dec("0.000000008121647560"));
        assert_eq!(dec("1000000000000") / a, dec("0.000008121647560868"));
        assert_eq!(dec("1000000000000000") / a, dec("0.008121647560868164"));
        assert_eq!(dec("1000000000000000000") / a, dec("8.121647560868164773"));

        // Move left
        let a = dec("0.123127726548762582");
        assert_eq!(a / dec("1.0"), dec("0.123127726548762582"));
        assert_eq!(a / dec("0.1"), dec("1.23127726548762582"));
        assert_eq!(a / dec("0.01"), dec("12.3127726548762582"));
        assert_eq!(a / dec("0.001"), dec("123.127726548762582"));
        assert_eq!(a / dec("0.000001"), dec("123127.726548762582"));
        assert_eq!(a / dec("0.000000001"), dec("123127726.548762582"));
        assert_eq!(a / dec("0.000000000001"), dec("123127726548.762582"));
        assert_eq!(a / dec("0.000000000000001"), dec("123127726548762.582"));
        assert_eq!(a / dec("0.000000000000000001"), dec("123127726548762582"));

        assert_eq!(
            Decimal::percent(15) / Decimal::percent(60),
            Decimal::percent(25)
        );

        // works for refs
        let a = Decimal::percent(100);
        let b = Decimal::percent(20);
        let expected = Decimal::percent(500);
        assert_eq!(a / b, expected);
        assert_eq!(&a / b, expected);
        assert_eq!(a / &b, expected);
        assert_eq!(&a / &b, expected);
    }

    #[test]
    fn decimal_div_assign_works() {
        let mut a = Decimal::percent(15);
        a /= Decimal::percent(20);
        assert_eq!(a, Decimal::percent(75));

        // works for refs
        let mut a = Decimal::percent(50);
        let b = Decimal::percent(20);
        a /= &b;
        assert_eq!(a, Decimal::percent(250));
    }

    #[test]
    #[should_panic(expected = "Division failed - multiplication overflow")]
    fn decimal_div_overflow_panics() {
        let _value = Decimal::MAX / Decimal::percent(10);
    }

    #[test]
    #[should_panic(expected = "Division failed - denominator must not be zero")]
    fn decimal_div_by_zero_panics() {
        let _value = Decimal::one() / Decimal::zero();
    }

    #[test]
    fn decimal_uint128_division() {
        // a/b
        let left = Decimal::percent(150); // 1.5
        let right = Uint128::new(3);
        assert_eq!(left / right, Decimal::percent(50));

        // 0/a
        let left = Decimal::zero();
        let right = Uint128::new(300);
        assert_eq!(left / right, Decimal::zero());
    }

    #[test]
    #[should_panic(expected = "attempt to divide by zero")]
    fn decimal_uint128_divide_by_zero() {
        let left = Decimal::percent(150); // 1.5
        let right = Uint128::new(0);
        let _result = left / right;
    }

    #[test]
    fn decimal_uint128_div_assign() {
        // a/b
        let mut dec = Decimal::percent(150); // 1.5
        dec /= Uint128::new(3);
        assert_eq!(dec, Decimal::percent(50));

        // 0/a
        let mut dec = Decimal::zero();
        dec /= Uint128::new(300);
        assert_eq!(dec, Decimal::zero());
    }

    #[test]
    #[should_panic(expected = "attempt to divide by zero")]
    fn decimal_uint128_div_assign_by_zero() {
        // a/0
        let mut dec = Decimal::percent(50);
        dec /= Uint128::new(0);
    }

    #[test]
    fn decimal_uint128_sqrt() {
        assert_eq!(Decimal::percent(900).sqrt(), Decimal::percent(300));

        assert!(Decimal::percent(316) < Decimal::percent(1000).sqrt());
        assert!(Decimal::percent(1000).sqrt() < Decimal::percent(317));
    }

    /// sqrt(2) is an irrational number, i.e. all 18 decimal places should be used.
    #[test]
    fn decimal_uint128_sqrt_is_precise() {
        assert_eq!(
            Decimal::from_str("2").unwrap().sqrt(),
            Decimal::from_str("1.414213562373095048").unwrap() // https://www.wolframalpha.com/input/?i=sqrt%282%29
        );
    }

    #[test]
    fn decimal_uint128_sqrt_does_not_overflow() {
        assert_eq!(
            Decimal::from_str("400").unwrap().sqrt(),
            Decimal::from_str("20").unwrap()
        );
    }

    #[test]
    fn decimal_uint128_sqrt_intermediate_precision_used() {
        assert_eq!(
            Decimal::from_str("400001").unwrap().sqrt(),
            // The last two digits (27) are truncated below due to the algorithm
            // we use. Larger numbers will cause less precision.
            // https://www.wolframalpha.com/input/?i=sqrt%28400001%29
            Decimal::from_str("632.456322602596803200").unwrap()
        );
    }

    #[test]
    fn decimal_checked_pow() {
        for exp in 0..10 {
            assert_eq!(Decimal::one().checked_pow(exp).unwrap(), Decimal::one());
        }

        // This case is mathematically undefined but we ensure consistency with Rust stdandard types
        // https://play.rust-lang.org/?version=stable&mode=debug&edition=2021&gist=20df6716048e77087acd40194b233494
        assert_eq!(Decimal::zero().checked_pow(0).unwrap(), Decimal::one());

        for exp in 1..10 {
            assert_eq!(Decimal::zero().checked_pow(exp).unwrap(), Decimal::zero());
        }

        for num in &[
            Decimal::percent(50),
            Decimal::percent(99),
            Decimal::percent(200),
        ] {
            assert_eq!(num.checked_pow(0).unwrap(), Decimal::one())
        }

        assert_eq!(
            Decimal::percent(20).checked_pow(2).unwrap(),
            Decimal::percent(4)
        );

        assert_eq!(
            Decimal::percent(20).checked_pow(3).unwrap(),
            Decimal::permille(8)
        );

        assert_eq!(
            Decimal::percent(200).checked_pow(4).unwrap(),
            Decimal::percent(1600)
        );

        assert_eq!(
            Decimal::percent(200).checked_pow(4).unwrap(),
            Decimal::percent(1600)
        );

        assert_eq!(
            Decimal::percent(700).checked_pow(5).unwrap(),
            Decimal::percent(1680700)
        );

        assert_eq!(
            Decimal::percent(700).checked_pow(8).unwrap(),
            Decimal::percent(576480100)
        );

        assert_eq!(
            Decimal::percent(700).checked_pow(10).unwrap(),
            Decimal::percent(28247524900)
        );

        assert_eq!(
            Decimal::percent(120).checked_pow(123).unwrap(),
            Decimal(5486473221892422150877397607u128.into())
        );

        assert_eq!(
            Decimal::percent(10).checked_pow(2).unwrap(),
            Decimal(10000000000000000u128.into())
        );

        assert_eq!(
            Decimal::percent(10).checked_pow(18).unwrap(),
            Decimal(1u128.into())
        );
    }

    #[test]
    fn decimal_checked_pow_overflow() {
        assert_eq!(
            Decimal::MAX.checked_pow(2),
            Err(OverflowError {
                operation: crate::OverflowOperation::Pow,
                operand1: Decimal::MAX.to_string(),
                operand2: "2".to_string(),
            })
        );
    }

    #[test]
    fn decimal_to_string() {
        // Integers
        assert_eq!(Decimal::zero().to_string(), "0");
        assert_eq!(Decimal::one().to_string(), "1");
        assert_eq!(Decimal::percent(500).to_string(), "5");

        // Decimals
        assert_eq!(Decimal::percent(125).to_string(), "1.25");
        assert_eq!(Decimal::percent(42638).to_string(), "426.38");
        assert_eq!(Decimal::percent(3).to_string(), "0.03");
        assert_eq!(Decimal::permille(987).to_string(), "0.987");

        assert_eq!(
            Decimal(Uint128::from(1u128)).to_string(),
            "0.000000000000000001"
        );
        assert_eq!(
            Decimal(Uint128::from(10u128)).to_string(),
            "0.00000000000000001"
        );
        assert_eq!(
            Decimal(Uint128::from(100u128)).to_string(),
            "0.0000000000000001"
        );
        assert_eq!(
            Decimal(Uint128::from(1000u128)).to_string(),
            "0.000000000000001"
        );
        assert_eq!(
            Decimal(Uint128::from(10000u128)).to_string(),
            "0.00000000000001"
        );
        assert_eq!(
            Decimal(Uint128::from(100000u128)).to_string(),
            "0.0000000000001"
        );
        assert_eq!(
            Decimal(Uint128::from(1000000u128)).to_string(),
            "0.000000000001"
        );
        assert_eq!(
            Decimal(Uint128::from(10000000u128)).to_string(),
            "0.00000000001"
        );
        assert_eq!(
            Decimal(Uint128::from(100000000u128)).to_string(),
            "0.0000000001"
        );
        assert_eq!(
            Decimal(Uint128::from(1000000000u128)).to_string(),
            "0.000000001"
        );
        assert_eq!(
            Decimal(Uint128::from(10000000000u128)).to_string(),
            "0.00000001"
        );
        assert_eq!(
            Decimal(Uint128::from(100000000000u128)).to_string(),
            "0.0000001"
        );
        assert_eq!(
            Decimal(Uint128::from(10000000000000u128)).to_string(),
            "0.00001"
        );
        assert_eq!(
            Decimal(Uint128::from(100000000000000u128)).to_string(),
            "0.0001"
        );
        assert_eq!(
            Decimal(Uint128::from(1000000000000000u128)).to_string(),
            "0.001"
        );
        assert_eq!(
            Decimal(Uint128::from(10000000000000000u128)).to_string(),
            "0.01"
        );
        assert_eq!(
            Decimal(Uint128::from(100000000000000000u128)).to_string(),
            "0.1"
        );
    }

    #[test]
    fn decimal_iter_sum() {
        let items = vec![
            Decimal::zero(),
            Decimal(Uint128::from(2u128)),
            Decimal(Uint128::from(2u128)),
        ];
        assert_eq!(items.iter().sum::<Decimal>(), Decimal(Uint128::from(4u128)));
        assert_eq!(
            items.into_iter().sum::<Decimal>(),
            Decimal(Uint128::from(4u128))
        );

        let empty: Vec<Decimal> = vec![];
        assert_eq!(Decimal::zero(), empty.iter().sum::<Decimal>());
    }

    #[test]
    fn decimal_serialize() {
        assert_eq!(to_vec(&Decimal::zero()).unwrap(), br#""0""#);
        assert_eq!(to_vec(&Decimal::one()).unwrap(), br#""1""#);
        assert_eq!(to_vec(&Decimal::percent(8)).unwrap(), br#""0.08""#);
        assert_eq!(to_vec(&Decimal::percent(87)).unwrap(), br#""0.87""#);
        assert_eq!(to_vec(&Decimal::percent(876)).unwrap(), br#""8.76""#);
        assert_eq!(to_vec(&Decimal::percent(8765)).unwrap(), br#""87.65""#);
    }

    #[test]
    fn decimal_deserialize() {
        assert_eq!(from_slice::<Decimal>(br#""0""#).unwrap(), Decimal::zero());
        assert_eq!(from_slice::<Decimal>(br#""1""#).unwrap(), Decimal::one());
        assert_eq!(from_slice::<Decimal>(br#""000""#).unwrap(), Decimal::zero());
        assert_eq!(from_slice::<Decimal>(br#""001""#).unwrap(), Decimal::one());

        assert_eq!(
            from_slice::<Decimal>(br#""0.08""#).unwrap(),
            Decimal::percent(8)
        );
        assert_eq!(
            from_slice::<Decimal>(br#""0.87""#).unwrap(),
            Decimal::percent(87)
        );
        assert_eq!(
            from_slice::<Decimal>(br#""8.76""#).unwrap(),
            Decimal::percent(876)
        );
        assert_eq!(
            from_slice::<Decimal>(br#""87.65""#).unwrap(),
            Decimal::percent(8765)
        );
    }

    #[test]
    fn decimal_abs_diff_works() {
        let a = Decimal::percent(285);
        let b = Decimal::percent(200);
        let expected = Decimal::percent(85);
        assert_eq!(a.abs_diff(b), expected);
        assert_eq!(b.abs_diff(a), expected);
    }

    #[test]
    #[allow(clippy::op_ref)]
    fn decimal_rem_works() {
        // 4.02 % 1.11 = 0.69
        assert_eq!(
            Decimal::percent(402) % Decimal::percent(111),
            Decimal::percent(69)
        );

        // 15.25 % 4 = 3.25
        assert_eq!(
            Decimal::percent(1525) % Decimal::percent(400),
            Decimal::percent(325)
        );

        let a = Decimal::percent(318);
        let b = Decimal::percent(317);
        let expected = Decimal::percent(1);
        assert_eq!(a % b, expected);
        assert_eq!(a % &b, expected);
        assert_eq!(&a % b, expected);
        assert_eq!(&a % &b, expected);
    }

    #[test]
    fn decimal_rem_assign_works() {
        let mut a = Decimal::percent(17673);
        a %= Decimal::percent(2362);
        assert_eq!(a, Decimal::percent(1139)); // 176.73 % 23.62 = 11.39

        let mut a = Decimal::percent(4262);
        let b = Decimal::percent(1270);
        a %= &b;
        assert_eq!(a, Decimal::percent(452)); // 42.62 % 12.7 = 4.52
    }

    #[test]
    #[should_panic(expected = "divisor of zero")]
    fn decimal_rem_panics_for_zero() {
        let _ = Decimal::percent(777) % Decimal::zero();
    }

    #[test]
    fn decimal_checked_methods() {
        // checked add
        assert_eq!(
            Decimal::percent(402)
                .checked_add(Decimal::percent(111))
                .unwrap(),
            Decimal::percent(513)
        );
        assert!(matches!(
            Decimal::MAX.checked_add(Decimal::percent(1)),
            Err(OverflowError { .. })
        ));

        // checked sub
        assert_eq!(
            Decimal::percent(1111)
                .checked_sub(Decimal::percent(111))
                .unwrap(),
            Decimal::percent(1000)
        );
        assert!(matches!(
            Decimal::zero().checked_sub(Decimal::percent(1)),
            Err(OverflowError { .. })
        ));

        // checked div
        assert_eq!(
            Decimal::percent(30)
                .checked_div(Decimal::percent(200))
                .unwrap(),
            Decimal::percent(15)
        );
        assert_eq!(
            Decimal::percent(88)
                .checked_div(Decimal::percent(20))
                .unwrap(),
            Decimal::percent(440)
        );
        assert!(matches!(
            Decimal::MAX.checked_div(Decimal::zero()),
            Err(CheckedFromRatioError::DivideByZero {})
        ));
        assert!(matches!(
            Decimal::MAX.checked_div(Decimal::percent(1)),
            Err(CheckedFromRatioError::Overflow {})
        ));

        // checked rem
        assert_eq!(
            Decimal::percent(402)
                .checked_rem(Decimal::percent(111))
                .unwrap(),
            Decimal::percent(69)
        );
        assert_eq!(
            Decimal::percent(1525)
                .checked_rem(Decimal::percent(400))
                .unwrap(),
            Decimal::percent(325)
        );
        assert!(matches!(
            Decimal::MAX.checked_rem(Decimal::zero()),
            Err(DivideByZeroError { .. })
        ));
    }

    #[test]
    fn decimal_pow_works() {
        assert_eq!(Decimal::percent(200).pow(2), Decimal::percent(400));
        assert_eq!(Decimal::percent(200).pow(10), Decimal::percent(102400));
    }

    #[test]
    #[should_panic]
    fn decimal_pow_overflow_panics() {
        Decimal::MAX.pow(2u32);
    }

    #[test]
    fn decimal_saturating_works() {
        assert_eq!(
            Decimal::percent(200).saturating_add(Decimal::percent(200)),
            Decimal::percent(400)
        );
        assert_eq!(
            Decimal::MAX.saturating_add(Decimal::percent(200)),
            Decimal::MAX
        );
        assert_eq!(
            Decimal::percent(200).saturating_sub(Decimal::percent(100)),
            Decimal::percent(100)
        );
        assert_eq!(
            Decimal::zero().saturating_sub(Decimal::percent(200)),
            Decimal::zero()
        );
        assert_eq!(
            Decimal::percent(200).saturating_mul(Decimal::percent(50)),
            Decimal::percent(100)
        );
        assert_eq!(
            Decimal::MAX.saturating_mul(Decimal::percent(200)),
            Decimal::MAX
        );
        assert_eq!(
            Decimal::percent(400).saturating_pow(2u32),
            Decimal::percent(1600)
        );
        assert_eq!(Decimal::MAX.saturating_pow(2u32), Decimal::MAX);
    }

    #[test]
    fn decimal_rounding() {
        assert_eq!(Decimal::one().floor(), Decimal::one());
        assert_eq!(Decimal::percent(150).floor(), Decimal::one());
        assert_eq!(Decimal::percent(199).floor(), Decimal::one());
        assert_eq!(Decimal::percent(200).floor(), Decimal::percent(200));
        assert_eq!(Decimal::percent(99).floor(), Decimal::zero());

        assert_eq!(Decimal::one().ceil(), Decimal::one());
        assert_eq!(Decimal::percent(150).ceil(), Decimal::percent(200));
        assert_eq!(Decimal::percent(199).ceil(), Decimal::percent(200));
        assert_eq!(Decimal::percent(99).ceil(), Decimal::one());
        assert_eq!(Decimal(Uint128::from(1u128)).ceil(), Decimal::one());
    }

    #[test]
    #[should_panic(expected = "attempt to ceil with overflow")]
    fn decimal_ceil_panics() {
        let _ = Decimal::MAX.ceil();
    }

    #[test]
    fn decimal_checked_ceil() {
        assert_eq!(
            Decimal::percent(199).checked_ceil(),
            Ok(Decimal::percent(200))
        );
        assert!(matches!(
            Decimal::MAX.checked_ceil(),
            Err(RoundUpOverflowError { .. })
        ));
    }

    #[test]
    fn decimal_partial_eq() {
        let test_cases = [
            ("1", "1", true),
            ("0.5", "0.5", true),
            ("0.5", "0.51", false),
            ("0", "0.00000", true),
        ]
        .into_iter()
        .map(|(lhs, rhs, expected)| (dec(lhs), dec(rhs), expected));

        #[allow(clippy::op_ref)]
        for (lhs, rhs, expected) in test_cases {
            assert_eq!(lhs == rhs, expected);
            assert_eq!(&lhs == rhs, expected);
            assert_eq!(lhs == &rhs, expected);
            assert_eq!(&lhs == &rhs, expected);
        }
    }
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
    pub const fn new(value: Uint256) -> Self {
        Self(value)
    }

    /// Creates a Decimal256 from u128
    /// This is equivalent to `Decimal256::from_atomics(value, 18)` but usable in a const context.
    pub const fn raw(value: u128) -> Self {
        Self(Uint256::from_u128(value))
    }

    /// Create a 1.0 Decimal256
    #[inline]
    pub const fn one() -> Self {
        Self(Self::DECIMAL_FRACTIONAL)
    }

    /// Create a 0.0 Decimal256
    #[inline]
    pub const fn zero() -> Self {
        Self(Uint256::zero())
    }

    /// Convert x% into Decimal256
    pub fn percent(x: u64) -> Self {
        Self(Uint256::from(x) * Uint256::from(10_000_000_000_000_000u128))
    }

    /// Convert permille (x/1000) into Decimal256
    pub fn permille(x: u64) -> Self {
        Self(Uint256::from(x) * Uint256::from(1_000_000_000_000_000u128))
    }

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
    ) -> Result<Self, Decimal256RangeExceeded> {
        let atomics = atomics.into();
        let ten = Uint256::from(10u64); // TODO: make const
        Ok(match decimal_places.cmp(&(Self::DECIMAL_PLACES)) {
            Ordering::Less => {
                let digits = (Self::DECIMAL_PLACES) - decimal_places; // No overflow because decimal_places < DECIMAL_PLACES
                let factor = ten.checked_pow(digits).unwrap(); // Safe because digits <= 17
                Self(
                    atomics
                        .checked_mul(factor)
                        .map_err(|_| Decimal256RangeExceeded)?,
                )
            }
            Ordering::Equal => Self(atomics),
            Ordering::Greater => {
                let digits = decimal_places - (Self::DECIMAL_PLACES); // No overflow because decimal_places > DECIMAL_PLACES
                if let Ok(factor) = ten.checked_pow(digits) {
                    Self(atomics.checked_div(factor).unwrap()) // Safe because factor cannot be zero
                } else {
                    // In this case `factor` exceeds the Uint256 range.
                    // Any Uint256 `x` divided by `factor` with `factor > Uint256::MAX` is 0.
                    // Try e.g. Python3: `(2**256-1) // 2**256`
                    Self(Uint256::zero())
                }
            }
        })
    }

    /// Returns the ratio (numerator / denominator) as a Decimal256
    pub fn from_ratio(numerator: impl Into<Uint256>, denominator: impl Into<Uint256>) -> Self {
        match Decimal256::checked_from_ratio(numerator, denominator) {
            Ok(value) => value,
            Err(CheckedFromRatioError::DivideByZero) => {
                panic!("Denominator must not be zero")
            }
            Err(CheckedFromRatioError::Overflow) => panic!("Multiplication overflow"),
        }
    }

    /// Returns the ratio (numerator / denominator) as a Decimal256
    pub fn checked_from_ratio(
        numerator: impl Into<Uint256>,
        denominator: impl Into<Uint256>,
    ) -> Result<Self, CheckedFromRatioError> {
        let numerator: Uint256 = numerator.into();
        let denominator: Uint256 = denominator.into();
        match numerator.checked_multiply_ratio(Self::DECIMAL_FRACTIONAL, denominator) {
            Ok(ratio) => {
                // numerator * DECIMAL_FRACTIONAL / denominator
                Ok(Self(ratio))
            }
            Err(CheckedMultiplyRatioError::Overflow) => Err(CheckedFromRatioError::Overflow),
            Err(CheckedMultiplyRatioError::DivideByZero) => {
                Err(CheckedFromRatioError::DivideByZero)
            }
        }
    }

    pub const fn is_zero(&self) -> bool {
        self.0.is_zero()
    }

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
    pub const fn atomics(&self) -> Uint256 {
        self.0
    }

    /// The number of decimal places. This is a constant value for now
    /// but this could potentially change as the type evolves.
    ///
    /// See also [`Decimal256::atomics()`].
    #[inline]
    pub const fn decimal_places(&self) -> u32 {
        Self::DECIMAL_PLACES
    }

    /// Rounds value down after decimal places.
    pub fn floor(&self) -> Self {
        Self((self.0 / Self::DECIMAL_FRACTIONAL) * Self::DECIMAL_FRACTIONAL)
    }

    /// Rounds value up after decimal places. Panics on overflow.
    pub fn ceil(&self) -> Self {
        match self.checked_ceil() {
            Ok(value) => value,
            Err(_) => panic!("attempt to ceil with overflow"),
        }
    }

    /// Rounds value up after decimal places. Returns OverflowError on overflow.
    pub fn checked_ceil(&self) -> Result<Self, RoundUpOverflowError> {
        let floor = self.floor();
        if floor == self {
            Ok(floor)
        } else {
            floor
                .checked_add(Decimal256::one())
                .map_err(|_| RoundUpOverflowError)
        }
    }

    pub fn checked_add(self, other: Self) -> Result<Self, OverflowError> {
        self.0
            .checked_add(other.0)
            .map(Self)
            .map_err(|_| OverflowError::new(OverflowOperation::Add, self, other))
    }

    pub fn checked_sub(self, other: Self) -> Result<Self, OverflowError> {
        self.0
            .checked_sub(other.0)
            .map(Self)
            .map_err(|_| OverflowError::new(OverflowOperation::Sub, self, other))
    }

    /// Multiplies one `Decimal256` by another, returning an `OverflowError` if an overflow occurred.
    pub fn checked_mul(self, other: Self) -> Result<Self, OverflowError> {
        let result_as_uint512 = self.numerator().full_mul(other.numerator())
            / Uint512::from_uint256(Self::DECIMAL_FRACTIONAL); // from_uint128 is a const method and should be "free"
        result_as_uint512
            .try_into()
            .map(Self)
            .map_err(|_| OverflowError {
                operation: crate::OverflowOperation::Mul,
                operand1: self.to_string(),
                operand2: other.to_string(),
            })
    }

    /// Raises a value to the power of `exp`, panics if an overflow occurred.
    pub fn pow(self, exp: u32) -> Self {
        match self.checked_pow(exp) {
            Ok(value) => value,
            Err(_) => panic!("Multiplication overflow"),
        }
    }

    /// Raises a value to the power of `exp`, returning an `OverflowError` if an overflow occurred.
    pub fn checked_pow(self, exp: u32) -> Result<Self, OverflowError> {
        // This uses the exponentiation by squaring algorithm:
        // https://en.wikipedia.org/wiki/Exponentiation_by_squaring#Basic_method

        fn inner(mut x: Decimal256, mut n: u32) -> Result<Decimal256, OverflowError> {
            if n == 0 {
                return Ok(Decimal256::one());
            }

            let mut y = Decimal256::one();

            while n > 1 {
                if n % 2 == 0 {
                    x = x.checked_mul(x)?;
                    n /= 2;
                } else {
                    y = x.checked_mul(y)?;
                    x = x.checked_mul(x)?;
                    n = (n - 1) / 2;
                }
            }

            Ok(x * y)
        }

        inner(self, exp).map_err(|_| OverflowError {
            operation: crate::OverflowOperation::Pow,
            operand1: self.to_string(),
            operand2: exp.to_string(),
        })
    }

    pub fn checked_div(self, other: Self) -> Result<Self, CheckedFromRatioError> {
        Decimal256::checked_from_ratio(self.numerator(), other.numerator())
    }

    pub fn checked_rem(self, other: Self) -> Result<Self, DivideByZeroError> {
        self.0
            .checked_rem(other.0)
            .map(Self)
            .map_err(|_| DivideByZeroError::new(self))
    }

    /// Returns the approximate square root as a Decimal256.
    ///
    /// This should not overflow or panic.
    pub fn sqrt(&self) -> Self {
        // Algorithm described in https://hackmd.io/@webmaster128/SJThlukj_
        // We start with the highest precision possible and lower it until
        // there's no overflow.
        //
        // TODO: This could be made more efficient once log10 is in:
        // https://github.com/rust-lang/rust/issues/70887
        // The max precision is something like `18 - log10(self.0) / 2`.
        (0..=Self::DECIMAL_PLACES / 2)
            .rev()
            .find_map(|i| self.sqrt_with_precision(i))
            // The last step (i = 0) is guaranteed to succeed because `isqrt(Uint256::MAX) * 10^9` does not overflow
            .unwrap()
    }

    /// Lower precision means more aggressive rounding, but less risk of overflow.
    /// Precision *must* be a number between 0 and 9 (inclusive).
    ///
    /// Returns `None` if the internal multiplication overflows.
    fn sqrt_with_precision(&self, precision: u32) -> Option<Self> {
        let inner_mul = Uint256::from(100u128).pow(precision);
        self.0.checked_mul(inner_mul).ok().map(|inner| {
            let outer_mul = Uint256::from(10u128).pow(Self::DECIMAL_PLACES / 2 - precision);
            Self(inner.isqrt().checked_mul(outer_mul).unwrap())
        })
    }

    pub fn abs_diff(self, other: Self) -> Self {
        if self < other {
            other - self
        } else {
            self - other
        }
    }

    pub fn saturating_add(self, other: Self) -> Self {
        match self.checked_add(other) {
            Ok(value) => value,
            Err(_) => Self::MAX,
        }
    }

    pub fn saturating_sub(self, other: Self) -> Self {
        match self.checked_sub(other) {
            Ok(value) => value,
            Err(_) => Self::zero(),
        }
    }

    pub fn saturating_mul(self, other: Self) -> Self {
        match self.checked_mul(other) {
            Ok(value) => value,
            Err(_) => Self::MAX,
        }
    }

    pub fn saturating_pow(self, exp: u32) -> Self {
        match self.checked_pow(exp) {
            Ok(value) => value,
            Err(_) => Self::MAX,
        }
    }
}

impl Fraction<Uint256> for Decimal256 {
    #[inline]
    fn numerator(&self) -> Uint256 {
        self.0
    }

    #[inline]
    fn denominator(&self) -> Uint256 {
        Self::DECIMAL_FRACTIONAL
    }

    /// Returns the multiplicative inverse `1/d` for decimal `d`.
    ///
    /// If `d` is zero, none is returned.
    fn inv(&self) -> Option<Self> {
        if self.is_zero() {
            None
        } else {
            // Let self be p/q with p = self.0 and q = DECIMAL_FRACTIONAL.
            // Now we calculate the inverse a/b = q/p such that b = DECIMAL_FRACTIONAL. Then
            // `a = DECIMAL_FRACTIONAL*DECIMAL_FRACTIONAL / self.0`.
            Some(Self(Self::DECIMAL_FRACTIONAL_SQUARED / self.0))
        }
    }
}

impl From<Decimal> for Decimal256 {
    fn from(input: Decimal) -> Self {
        // Unwrap is safe because Decimal256 and Decimal have the same decimal places.
        // Every Decimal value can be stored in Decimal256.
        Decimal256::from_atomics(input.atomics(), input.decimal_places()).unwrap()
    }
}

impl FromStr for Decimal256 {
    type Err = StdError;

    /// Converts the decimal string to a Decimal256
    /// Possible inputs: "1.23", "1", "000012", "1.123000000"
    /// Disallowed: "", ".23"
    ///
    /// This never performs any kind of rounding.
    /// More than DECIMAL_PLACES fractional digits, even zeros, result in an error.
    fn from_str(input: &str) -> Result<Self, Self::Err> {
        let mut parts_iter = input.split('.');

        let whole_part = parts_iter.next().unwrap(); // split always returns at least one element
        let whole = whole_part
            .parse::<Uint256>()
            .map_err(|_| StdError::generic_err("Error parsing whole"))?;
        let mut atomics = whole
            .checked_mul(Self::DECIMAL_FRACTIONAL)
            .map_err(|_| StdError::generic_err("Value too big"))?;

        if let Some(fractional_part) = parts_iter.next() {
            let fractional = fractional_part
                .parse::<Uint256>()
                .map_err(|_| StdError::generic_err("Error parsing fractional"))?;
            let exp = (Self::DECIMAL_PLACES.checked_sub(fractional_part.len() as u32)).ok_or_else(
                || {
                    StdError::generic_err(format!(
                        "Cannot parse more than {} fractional digits",
                        Self::DECIMAL_PLACES
                    ))
                },
            )?;
            debug_assert!(exp <= Self::DECIMAL_PLACES);
            let fractional_factor = Uint256::from(10u128).pow(exp);
            atomics = atomics
                .checked_add(
                    // The inner multiplication can't overflow because
                    // fractional < 10^DECIMAL_PLACES && fractional_factor <= 10^DECIMAL_PLACES
                    fractional.checked_mul(fractional_factor).unwrap(),
                )
                .map_err(|_| StdError::generic_err("Value too big"))?;
        }

        if parts_iter.next().is_some() {
            return Err(StdError::generic_err("Unexpected number of dots"));
        }

        Ok(Self(atomics))
    }
}

impl fmt::Display for Decimal256 {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let whole = (self.0) / Self::DECIMAL_FRACTIONAL;
        let fractional = (self.0).checked_rem(Self::DECIMAL_FRACTIONAL).unwrap();

        if fractional.is_zero() {
            write!(f, "{}", whole)
        } else {
            let fractional_string = format!(
                "{:0>padding$}",
                fractional,
                padding = Self::DECIMAL_PLACES as usize
            );
            f.write_str(&whole.to_string())?;
            f.write_char('.')?;
            f.write_str(fractional_string.trim_end_matches('0'))?;
            Ok(())
        }
    }
}

impl Add for Decimal256 {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        Self(self.0 + other.0)
    }
}
forward_ref_binop!(impl Add, add for Decimal256, Decimal256);

impl AddAssign for Decimal256 {
    fn add_assign(&mut self, rhs: Decimal256) {
        *self = *self + rhs;
    }
}
forward_ref_op_assign!(impl AddAssign, add_assign for Decimal256, Decimal256);

impl Sub for Decimal256 {
    type Output = Self;

    fn sub(self, other: Self) -> Self {
        Self(self.0 - other.0)
    }
}
forward_ref_binop!(impl Sub, sub for Decimal256, Decimal256);

impl SubAssign for Decimal256 {
    fn sub_assign(&mut self, rhs: Decimal256) {
        *self = *self - rhs;
    }
}
forward_ref_op_assign!(impl SubAssign, sub_assign for Decimal256, Decimal256);

impl Mul for Decimal256 {
    type Output = Self;

    #[allow(clippy::suspicious_arithmetic_impl)]
    fn mul(self, other: Self) -> Self {
        // Decimals are fractions. We can multiply two decimals a and b
        // via
        //       (a.numerator() * b.numerator()) / (a.denominator() * b.denominator())
        //     = (a.numerator() * b.numerator()) / a.denominator() / b.denominator()

        let result_as_uint512 = self.numerator().full_mul(other.numerator())
            / Uint512::from_uint256(Self::DECIMAL_FRACTIONAL); // from_uint256 is a const method and should be "free"
        match result_as_uint512.try_into() {
            Ok(result) => Self(result),
            Err(_) => panic!("attempt to multiply with overflow"),
        }
    }
}
forward_ref_binop!(impl Mul, mul for Decimal256, Decimal256);

impl MulAssign for Decimal256 {
    fn mul_assign(&mut self, rhs: Self) {
        *self = *self * rhs;
    }
}
forward_ref_op_assign!(impl MulAssign, mul_assign for Decimal256, Decimal256);

/// Both d*u and u*d with d: Decimal256 and u: Uint256 returns an Uint256. There is no
/// specific reason for this decision other than the initial use cases we have. If you
/// need a Decimal256 result for the same calculation, use Decimal256(d*u) or Decimal256(u*d).
impl Mul<Decimal256> for Uint256 {
    type Output = Self;

    #[allow(clippy::suspicious_arithmetic_impl)]
    fn mul(self, rhs: Decimal256) -> Self::Output {
        // 0*a and b*0 is always 0
        if self.is_zero() || rhs.is_zero() {
            return Uint256::zero();
        }
        self.multiply_ratio(rhs.0, Decimal256::DECIMAL_FRACTIONAL)
    }
}

impl Mul<Uint256> for Decimal256 {
    type Output = Uint256;

    fn mul(self, rhs: Uint256) -> Self::Output {
        rhs * self
    }
}

impl Div for Decimal256 {
    type Output = Self;

    fn div(self, other: Self) -> Self {
        match Decimal256::checked_from_ratio(self.numerator(), other.numerator()) {
            Ok(ratio) => ratio,
            Err(CheckedFromRatioError::DivideByZero) => {
                panic!("Division failed - denominator must not be zero")
            }
            Err(CheckedFromRatioError::Overflow) => {
                panic!("Division failed - multiplication overflow")
            }
        }
    }
}
forward_ref_binop!(impl Div, div for Decimal256, Decimal256);

impl DivAssign for Decimal256 {
    fn div_assign(&mut self, rhs: Decimal256) {
        *self = *self / rhs;
    }
}
forward_ref_op_assign!(impl DivAssign, div_assign for Decimal256, Decimal256);

impl Div<Uint256> for Decimal256 {
    type Output = Self;

    fn div(self, rhs: Uint256) -> Self::Output {
        Self(self.0 / rhs)
    }
}

impl DivAssign<Uint256> for Decimal256 {
    fn div_assign(&mut self, rhs: Uint256) {
        self.0 /= rhs;
    }
}

impl Rem for Decimal256 {
    type Output = Self;

    /// # Panics
    ///
    /// This operation will panic if `rhs` is zero
    #[inline]
    fn rem(self, rhs: Self) -> Self {
        Self(self.0.rem(rhs.0))
    }
}
forward_ref_binop!(impl Rem, rem for Decimal256, Decimal256);

impl RemAssign<Decimal256> for Decimal256 {
    fn rem_assign(&mut self, rhs: Decimal256) {
        *self = *self % rhs;
    }
}
forward_ref_op_assign!(impl RemAssign, rem_assign for Decimal256, Decimal256);

impl<A> std::iter::Sum<A> for Decimal256
where
    Self: Add<A, Output = Self>,
{
    fn sum<I: Iterator<Item = A>>(iter: I) -> Self {
        iter.fold(Self::zero(), Add::add)
    }
}

/// Serializes as a decimal string
impl Serialize for Decimal256 {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: ser::Serializer,
    {
        serializer.serialize_str(&self.to_string())
    }
}

/// Deserializes as a base64 string
impl<'de> Deserialize<'de> for Decimal256 {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        deserializer.deserialize_str(Decimal256Visitor)
    }
}

struct Decimal256Visitor;

impl<'de> de::Visitor<'de> for Decimal256Visitor {
    type Value = Decimal256;

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str("string-encoded decimal")
    }

    fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
    where
        E: de::Error,
    {
        match Self::Value::from_str(v) {
            Ok(d) => Ok(d),
            Err(e) => Err(E::custom(format!("Error parsing decimal '{}': {}", v, e))),
        }
    }
}

impl PartialEq<&Decimal256> for Decimal256 {
    fn eq(&self, rhs: &&Decimal256) -> bool {
        self == *rhs
    }
}

impl PartialEq<Decimal256> for &Decimal256 {
    fn eq(&self, rhs: &Decimal256) -> bool {
        *self == rhs
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::errors::StdError;
    use crate::{from_slice, to_vec};

    fn dec(input: &str) -> Decimal256 {
        Decimal256::from_str(input).unwrap()
    }

    #[test]
    fn decimal256_new() {
        let expected = Uint256::from(300u128);
        assert_eq!(Decimal256::new(expected).0, expected);
    }

    #[test]
    fn decimal256_raw() {
        let value = 300u128;
        let expected = Uint256::from(value);
        assert_eq!(Decimal256::raw(value).0, expected);
    }

    #[test]
    fn decimal256_one() {
        let value = Decimal256::one();
        assert_eq!(value.0, Decimal256::DECIMAL_FRACTIONAL);
    }

    #[test]
    fn decimal256_zero() {
        let value = Decimal256::zero();
        assert!(value.0.is_zero());
    }

    #[test]
    fn decimal256_percent() {
        let value = Decimal256::percent(50);
        assert_eq!(value.0, Decimal256::DECIMAL_FRACTIONAL / Uint256::from(2u8));
    }

    #[test]
    fn decimal256_permille() {
        let value = Decimal256::permille(125);
        assert_eq!(value.0, Decimal256::DECIMAL_FRACTIONAL / Uint256::from(8u8));
    }

    #[test]
    fn decimal256_from_atomics_works() {
        let one = Decimal256::one();
        let two = one + one;

        assert_eq!(Decimal256::from_atomics(1u128, 0).unwrap(), one);
        assert_eq!(Decimal256::from_atomics(10u128, 1).unwrap(), one);
        assert_eq!(Decimal256::from_atomics(100u128, 2).unwrap(), one);
        assert_eq!(Decimal256::from_atomics(1000u128, 3).unwrap(), one);
        assert_eq!(
            Decimal256::from_atomics(1000000000000000000u128, 18).unwrap(),
            one
        );
        assert_eq!(
            Decimal256::from_atomics(10000000000000000000u128, 19).unwrap(),
            one
        );
        assert_eq!(
            Decimal256::from_atomics(100000000000000000000u128, 20).unwrap(),
            one
        );

        assert_eq!(Decimal256::from_atomics(2u128, 0).unwrap(), two);
        assert_eq!(Decimal256::from_atomics(20u128, 1).unwrap(), two);
        assert_eq!(Decimal256::from_atomics(200u128, 2).unwrap(), two);
        assert_eq!(Decimal256::from_atomics(2000u128, 3).unwrap(), two);
        assert_eq!(
            Decimal256::from_atomics(2000000000000000000u128, 18).unwrap(),
            two
        );
        assert_eq!(
            Decimal256::from_atomics(20000000000000000000u128, 19).unwrap(),
            two
        );
        assert_eq!(
            Decimal256::from_atomics(200000000000000000000u128, 20).unwrap(),
            two
        );

        // Cuts decimal digits (20 provided but only 18 can be stored)
        assert_eq!(
            Decimal256::from_atomics(4321u128, 20).unwrap(),
            Decimal256::from_str("0.000000000000000043").unwrap()
        );
        assert_eq!(
            Decimal256::from_atomics(6789u128, 20).unwrap(),
            Decimal256::from_str("0.000000000000000067").unwrap()
        );
        assert_eq!(
            Decimal256::from_atomics(u128::MAX, 38).unwrap(),
            Decimal256::from_str("3.402823669209384634").unwrap()
        );
        assert_eq!(
            Decimal256::from_atomics(u128::MAX, 39).unwrap(),
            Decimal256::from_str("0.340282366920938463").unwrap()
        );
        assert_eq!(
            Decimal256::from_atomics(u128::MAX, 45).unwrap(),
            Decimal256::from_str("0.000000340282366920").unwrap()
        );
        assert_eq!(
            Decimal256::from_atomics(u128::MAX, 51).unwrap(),
            Decimal256::from_str("0.000000000000340282").unwrap()
        );
        assert_eq!(
            Decimal256::from_atomics(u128::MAX, 56).unwrap(),
            Decimal256::from_str("0.000000000000000003").unwrap()
        );
        assert_eq!(
            Decimal256::from_atomics(u128::MAX, 57).unwrap(),
            Decimal256::from_str("0.000000000000000000").unwrap()
        );
        assert_eq!(
            Decimal256::from_atomics(u128::MAX, u32::MAX).unwrap(),
            Decimal256::from_str("0.000000000000000000").unwrap()
        );

        // Can be used with max value
        let max = Decimal256::MAX;
        assert_eq!(
            Decimal256::from_atomics(max.atomics(), max.decimal_places()).unwrap(),
            max
        );

        // Overflow is only possible with digits < 18
        let result = Decimal256::from_atomics(Uint256::MAX, 17);
        assert_eq!(result.unwrap_err(), Decimal256RangeExceeded);
    }

    #[test]
    fn decimal256_from_ratio_works() {
        // 1.0
        assert_eq!(Decimal256::from_ratio(1u128, 1u128), Decimal256::one());
        assert_eq!(Decimal256::from_ratio(53u128, 53u128), Decimal256::one());
        assert_eq!(Decimal256::from_ratio(125u128, 125u128), Decimal256::one());

        // 1.5
        assert_eq!(
            Decimal256::from_ratio(3u128, 2u128),
            Decimal256::percent(150)
        );
        assert_eq!(
            Decimal256::from_ratio(150u128, 100u128),
            Decimal256::percent(150)
        );
        assert_eq!(
            Decimal256::from_ratio(333u128, 222u128),
            Decimal256::percent(150)
        );

        // 0.125
        assert_eq!(
            Decimal256::from_ratio(1u64, 8u64),
            Decimal256::permille(125)
        );
        assert_eq!(
            Decimal256::from_ratio(125u64, 1000u64),
            Decimal256::permille(125)
        );

        // 1/3 (result floored)
        assert_eq!(
            Decimal256::from_ratio(1u64, 3u64),
            Decimal256(Uint256::from_str("333333333333333333").unwrap())
        );

        // 2/3 (result floored)
        assert_eq!(
            Decimal256::from_ratio(2u64, 3u64),
            Decimal256(Uint256::from_str("666666666666666666").unwrap())
        );

        // large inputs
        assert_eq!(Decimal256::from_ratio(0u128, u128::MAX), Decimal256::zero());
        assert_eq!(
            Decimal256::from_ratio(u128::MAX, u128::MAX),
            Decimal256::one()
        );
        // 340282366920938463463 is the largest integer <= Decimal256::MAX
        assert_eq!(
            Decimal256::from_ratio(340282366920938463463u128, 1u128),
            Decimal256::from_str("340282366920938463463").unwrap()
        );
    }

    #[test]
    #[should_panic(expected = "Denominator must not be zero")]
    fn decimal256_from_ratio_panics_for_zero_denominator() {
        Decimal256::from_ratio(1u128, 0u128);
    }

    #[test]
    #[should_panic(expected = "Multiplication overflow")]
    fn decimal256_from_ratio_panics_for_mul_overflow() {
        Decimal256::from_ratio(Uint256::MAX, 1u128);
    }

    #[test]
    fn decimal256_checked_from_ratio_does_not_panic() {
        assert_eq!(
            Decimal256::checked_from_ratio(1u128, 0u128),
            Err(CheckedFromRatioError::DivideByZero)
        );

        assert_eq!(
            Decimal256::checked_from_ratio(Uint256::MAX, 1u128),
            Err(CheckedFromRatioError::Overflow)
        );
    }

    #[test]
    fn decimal256_implements_fraction() {
        let fraction = Decimal256::from_str("1234.567").unwrap();
        assert_eq!(
            fraction.numerator(),
            Uint256::from_str("1234567000000000000000").unwrap()
        );
        assert_eq!(
            fraction.denominator(),
            Uint256::from_str("1000000000000000000").unwrap()
        );
    }

    #[test]
    fn decimal256_implements_from_decimal() {
        let a = Decimal::from_str("123.456").unwrap();
        let b = Decimal256::from(a);
        assert_eq!(b.to_string(), "123.456");

        let a = Decimal::from_str("0").unwrap();
        let b = Decimal256::from(a);
        assert_eq!(b.to_string(), "0");

        let a = Decimal::MAX;
        let b = Decimal256::from(a);
        assert_eq!(b.to_string(), "340282366920938463463.374607431768211455");
    }

    #[test]
    fn decimal256_from_str_works() {
        // Integers
        assert_eq!(Decimal256::from_str("0").unwrap(), Decimal256::percent(0));
        assert_eq!(Decimal256::from_str("1").unwrap(), Decimal256::percent(100));
        assert_eq!(Decimal256::from_str("5").unwrap(), Decimal256::percent(500));
        assert_eq!(
            Decimal256::from_str("42").unwrap(),
            Decimal256::percent(4200)
        );
        assert_eq!(Decimal256::from_str("000").unwrap(), Decimal256::percent(0));
        assert_eq!(
            Decimal256::from_str("001").unwrap(),
            Decimal256::percent(100)
        );
        assert_eq!(
            Decimal256::from_str("005").unwrap(),
            Decimal256::percent(500)
        );
        assert_eq!(
            Decimal256::from_str("0042").unwrap(),
            Decimal256::percent(4200)
        );

        // Decimals
        assert_eq!(
            Decimal256::from_str("1.0").unwrap(),
            Decimal256::percent(100)
        );
        assert_eq!(
            Decimal256::from_str("1.5").unwrap(),
            Decimal256::percent(150)
        );
        assert_eq!(
            Decimal256::from_str("0.5").unwrap(),
            Decimal256::percent(50)
        );
        assert_eq!(
            Decimal256::from_str("0.123").unwrap(),
            Decimal256::permille(123)
        );

        assert_eq!(
            Decimal256::from_str("40.00").unwrap(),
            Decimal256::percent(4000)
        );
        assert_eq!(
            Decimal256::from_str("04.00").unwrap(),
            Decimal256::percent(400)
        );
        assert_eq!(
            Decimal256::from_str("00.40").unwrap(),
            Decimal256::percent(40)
        );
        assert_eq!(
            Decimal256::from_str("00.04").unwrap(),
            Decimal256::percent(4)
        );

        // Can handle 18 fractional digits
        assert_eq!(
            Decimal256::from_str("7.123456789012345678").unwrap(),
            Decimal256(Uint256::from(7123456789012345678u128))
        );
        assert_eq!(
            Decimal256::from_str("7.999999999999999999").unwrap(),
            Decimal256(Uint256::from(7999999999999999999u128))
        );

        // Works for documented max value
        assert_eq!(
            Decimal256::from_str(
                "115792089237316195423570985008687907853269984665640564039457.584007913129639935"
            )
            .unwrap(),
            Decimal256::MAX
        );
    }

    #[test]
    fn decimal256_from_str_errors_for_broken_whole_part() {
        match Decimal256::from_str("").unwrap_err() {
            StdError::GenericErr { msg, .. } => assert_eq!(msg, "Error parsing whole"),
            e => panic!("Unexpected error: {:?}", e),
        }

        match Decimal256::from_str(" ").unwrap_err() {
            StdError::GenericErr { msg, .. } => assert_eq!(msg, "Error parsing whole"),
            e => panic!("Unexpected error: {:?}", e),
        }

        match Decimal256::from_str("-1").unwrap_err() {
            StdError::GenericErr { msg, .. } => assert_eq!(msg, "Error parsing whole"),
            e => panic!("Unexpected error: {:?}", e),
        }
    }

    #[test]
    fn decimal256_from_str_errors_for_broken_fractinal_part() {
        match Decimal256::from_str("1.").unwrap_err() {
            StdError::GenericErr { msg, .. } => assert_eq!(msg, "Error parsing fractional"),
            e => panic!("Unexpected error: {:?}", e),
        }

        match Decimal256::from_str("1. ").unwrap_err() {
            StdError::GenericErr { msg, .. } => assert_eq!(msg, "Error parsing fractional"),
            e => panic!("Unexpected error: {:?}", e),
        }

        match Decimal256::from_str("1.e").unwrap_err() {
            StdError::GenericErr { msg, .. } => assert_eq!(msg, "Error parsing fractional"),
            e => panic!("Unexpected error: {:?}", e),
        }

        match Decimal256::from_str("1.2e3").unwrap_err() {
            StdError::GenericErr { msg, .. } => assert_eq!(msg, "Error parsing fractional"),
            e => panic!("Unexpected error: {:?}", e),
        }
    }

    #[test]
    fn decimal256_from_str_errors_for_more_than_36_fractional_digits() {
        match Decimal256::from_str("7.1234567890123456789").unwrap_err() {
            StdError::GenericErr { msg, .. } => {
                assert_eq!(msg, "Cannot parse more than 18 fractional digits")
            }
            e => panic!("Unexpected error: {:?}", e),
        }

        // No special rules for trailing zeros. This could be changed but adds gas cost for the happy path.
        match Decimal256::from_str("7.1230000000000000000").unwrap_err() {
            StdError::GenericErr { msg, .. } => {
                assert_eq!(msg, "Cannot parse more than 18 fractional digits")
            }
            e => panic!("Unexpected error: {:?}", e),
        }
    }

    #[test]
    fn decimal256_from_str_errors_for_invalid_number_of_dots() {
        match Decimal256::from_str("1.2.3").unwrap_err() {
            StdError::GenericErr { msg, .. } => assert_eq!(msg, "Unexpected number of dots"),
            e => panic!("Unexpected error: {:?}", e),
        }

        match Decimal256::from_str("1.2.3.4").unwrap_err() {
            StdError::GenericErr { msg, .. } => assert_eq!(msg, "Unexpected number of dots"),
            e => panic!("Unexpected error: {:?}", e),
        }
    }

    #[test]
    fn decimal256_from_str_errors_for_more_than_max_value() {
        // Integer
        match Decimal256::from_str("115792089237316195423570985008687907853269984665640564039458")
            .unwrap_err()
        {
            StdError::GenericErr { msg, .. } => assert_eq!(msg, "Value too big"),
            e => panic!("Unexpected error: {:?}", e),
        }

        // Decimal
        match Decimal256::from_str("115792089237316195423570985008687907853269984665640564039458.0")
            .unwrap_err()
        {
            StdError::GenericErr { msg, .. } => assert_eq!(msg, "Value too big"),
            e => panic!("Unexpected error: {:?}", e),
        }
        match Decimal256::from_str(
            "115792089237316195423570985008687907853269984665640564039457.584007913129639936",
        )
        .unwrap_err()
        {
            StdError::GenericErr { msg, .. } => assert_eq!(msg, "Value too big"),
            e => panic!("Unexpected error: {:?}", e),
        }
    }

    #[test]
    fn decimal256_atomics_works() {
        let zero = Decimal256::zero();
        let one = Decimal256::one();
        let half = Decimal256::percent(50);
        let two = Decimal256::percent(200);
        let max = Decimal256::MAX;

        assert_eq!(zero.atomics(), Uint256::from(0u128));
        assert_eq!(one.atomics(), Uint256::from(1000000000000000000u128));
        assert_eq!(half.atomics(), Uint256::from(500000000000000000u128));
        assert_eq!(two.atomics(), Uint256::from(2000000000000000000u128));
        assert_eq!(max.atomics(), Uint256::MAX);
    }

    #[test]
    fn decimal256_decimal_places_works() {
        let zero = Decimal256::zero();
        let one = Decimal256::one();
        let half = Decimal256::percent(50);
        let two = Decimal256::percent(200);
        let max = Decimal256::MAX;

        assert_eq!(zero.decimal_places(), 18);
        assert_eq!(one.decimal_places(), 18);
        assert_eq!(half.decimal_places(), 18);
        assert_eq!(two.decimal_places(), 18);
        assert_eq!(max.decimal_places(), 18);
    }

    #[test]
    fn decimal256_is_zero_works() {
        assert!(Decimal256::zero().is_zero());
        assert!(Decimal256::percent(0).is_zero());
        assert!(Decimal256::permille(0).is_zero());

        assert!(!Decimal256::one().is_zero());
        assert!(!Decimal256::percent(123).is_zero());
        assert!(!Decimal256::permille(1234).is_zero());
    }

    #[test]
    fn decimal256_inv_works() {
        // d = 0
        assert_eq!(Decimal256::zero().inv(), None);

        // d == 1
        assert_eq!(Decimal256::one().inv(), Some(Decimal256::one()));

        // d > 1 exact
        assert_eq!(
            Decimal256::from_str("2").unwrap().inv(),
            Some(Decimal256::from_str("0.5").unwrap())
        );
        assert_eq!(
            Decimal256::from_str("20").unwrap().inv(),
            Some(Decimal256::from_str("0.05").unwrap())
        );
        assert_eq!(
            Decimal256::from_str("200").unwrap().inv(),
            Some(Decimal256::from_str("0.005").unwrap())
        );
        assert_eq!(
            Decimal256::from_str("2000").unwrap().inv(),
            Some(Decimal256::from_str("0.0005").unwrap())
        );

        // d > 1 rounded
        assert_eq!(
            Decimal256::from_str("3").unwrap().inv(),
            Some(Decimal256::from_str("0.333333333333333333").unwrap())
        );
        assert_eq!(
            Decimal256::from_str("6").unwrap().inv(),
            Some(Decimal256::from_str("0.166666666666666666").unwrap())
        );

        // d < 1 exact
        assert_eq!(
            Decimal256::from_str("0.5").unwrap().inv(),
            Some(Decimal256::from_str("2").unwrap())
        );
        assert_eq!(
            Decimal256::from_str("0.05").unwrap().inv(),
            Some(Decimal256::from_str("20").unwrap())
        );
        assert_eq!(
            Decimal256::from_str("0.005").unwrap().inv(),
            Some(Decimal256::from_str("200").unwrap())
        );
        assert_eq!(
            Decimal256::from_str("0.0005").unwrap().inv(),
            Some(Decimal256::from_str("2000").unwrap())
        );
    }

    #[test]
    #[allow(clippy::op_ref)]
    fn decimal256_add_works() {
        let value = Decimal256::one() + Decimal256::percent(50); // 1.5
        assert_eq!(
            value.0,
            Decimal256::DECIMAL_FRACTIONAL * Uint256::from(3u8) / Uint256::from(2u8)
        );

        assert_eq!(
            Decimal256::percent(5) + Decimal256::percent(4),
            Decimal256::percent(9)
        );
        assert_eq!(
            Decimal256::percent(5) + Decimal256::zero(),
            Decimal256::percent(5)
        );
        assert_eq!(Decimal256::zero() + Decimal256::zero(), Decimal256::zero());

        // works for refs
        let a = Decimal256::percent(15);
        let b = Decimal256::percent(25);
        let expected = Decimal256::percent(40);
        assert_eq!(a + b, expected);
        assert_eq!(&a + b, expected);
        assert_eq!(a + &b, expected);
        assert_eq!(&a + &b, expected);
    }

    #[test]
    #[should_panic(expected = "attempt to add with overflow")]
    fn decimal256_add_overflow_panics() {
        let _value = Decimal256::MAX + Decimal256::percent(50);
    }

    #[test]
    fn decimal256_add_assign_works() {
        let mut a = Decimal256::percent(30);
        a += Decimal256::percent(20);
        assert_eq!(a, Decimal256::percent(50));

        // works for refs
        let mut a = Decimal256::percent(15);
        let b = Decimal256::percent(3);
        let expected = Decimal256::percent(18);
        a += &b;
        assert_eq!(a, expected);
    }

    #[test]
    #[allow(clippy::op_ref)]
    fn decimal256_sub_works() {
        let value = Decimal256::one() - Decimal256::percent(50); // 0.5
        assert_eq!(value.0, Decimal256::DECIMAL_FRACTIONAL / Uint256::from(2u8));

        assert_eq!(
            Decimal256::percent(9) - Decimal256::percent(4),
            Decimal256::percent(5)
        );
        assert_eq!(
            Decimal256::percent(16) - Decimal256::zero(),
            Decimal256::percent(16)
        );
        assert_eq!(
            Decimal256::percent(16) - Decimal256::percent(16),
            Decimal256::zero()
        );
        assert_eq!(Decimal256::zero() - Decimal256::zero(), Decimal256::zero());

        // works for refs
        let a = Decimal256::percent(13);
        let b = Decimal256::percent(6);
        let expected = Decimal256::percent(7);
        assert_eq!(a - b, expected);
        assert_eq!(&a - b, expected);
        assert_eq!(a - &b, expected);
        assert_eq!(&a - &b, expected);
    }

    #[test]
    #[should_panic(expected = "attempt to subtract with overflow")]
    fn decimal256_sub_overflow_panics() {
        let _value = Decimal256::zero() - Decimal256::percent(50);
    }

    #[test]
    fn decimal256_sub_assign_works() {
        let mut a = Decimal256::percent(20);
        a -= Decimal256::percent(2);
        assert_eq!(a, Decimal256::percent(18));

        // works for refs
        let mut a = Decimal256::percent(33);
        let b = Decimal256::percent(13);
        let expected = Decimal256::percent(20);
        a -= &b;
        assert_eq!(a, expected);
    }

    #[test]
    #[allow(clippy::op_ref)]
    fn decimal256_implements_mul() {
        let one = Decimal256::one();
        let two = one + one;
        let half = Decimal256::percent(50);

        // 1*x and x*1
        assert_eq!(one * Decimal256::percent(0), Decimal256::percent(0));
        assert_eq!(one * Decimal256::percent(1), Decimal256::percent(1));
        assert_eq!(one * Decimal256::percent(10), Decimal256::percent(10));
        assert_eq!(one * Decimal256::percent(100), Decimal256::percent(100));
        assert_eq!(one * Decimal256::percent(1000), Decimal256::percent(1000));
        assert_eq!(one * Decimal256::MAX, Decimal256::MAX);
        assert_eq!(Decimal256::percent(0) * one, Decimal256::percent(0));
        assert_eq!(Decimal256::percent(1) * one, Decimal256::percent(1));
        assert_eq!(Decimal256::percent(10) * one, Decimal256::percent(10));
        assert_eq!(Decimal256::percent(100) * one, Decimal256::percent(100));
        assert_eq!(Decimal256::percent(1000) * one, Decimal256::percent(1000));
        assert_eq!(Decimal256::MAX * one, Decimal256::MAX);

        // double
        assert_eq!(two * Decimal256::percent(0), Decimal256::percent(0));
        assert_eq!(two * Decimal256::percent(1), Decimal256::percent(2));
        assert_eq!(two * Decimal256::percent(10), Decimal256::percent(20));
        assert_eq!(two * Decimal256::percent(100), Decimal256::percent(200));
        assert_eq!(two * Decimal256::percent(1000), Decimal256::percent(2000));
        assert_eq!(Decimal256::percent(0) * two, Decimal256::percent(0));
        assert_eq!(Decimal256::percent(1) * two, Decimal256::percent(2));
        assert_eq!(Decimal256::percent(10) * two, Decimal256::percent(20));
        assert_eq!(Decimal256::percent(100) * two, Decimal256::percent(200));
        assert_eq!(Decimal256::percent(1000) * two, Decimal256::percent(2000));

        // half
        assert_eq!(half * Decimal256::percent(0), Decimal256::percent(0));
        assert_eq!(half * Decimal256::percent(1), Decimal256::permille(5));
        assert_eq!(half * Decimal256::percent(10), Decimal256::percent(5));
        assert_eq!(half * Decimal256::percent(100), Decimal256::percent(50));
        assert_eq!(half * Decimal256::percent(1000), Decimal256::percent(500));
        assert_eq!(Decimal256::percent(0) * half, Decimal256::percent(0));
        assert_eq!(Decimal256::percent(1) * half, Decimal256::permille(5));
        assert_eq!(Decimal256::percent(10) * half, Decimal256::percent(5));
        assert_eq!(Decimal256::percent(100) * half, Decimal256::percent(50));
        assert_eq!(Decimal256::percent(1000) * half, Decimal256::percent(500));

        // Move left
        let a = dec("123.127726548762582");
        assert_eq!(a * dec("1"), dec("123.127726548762582"));
        assert_eq!(a * dec("10"), dec("1231.27726548762582"));
        assert_eq!(a * dec("100"), dec("12312.7726548762582"));
        assert_eq!(a * dec("1000"), dec("123127.726548762582"));
        assert_eq!(a * dec("1000000"), dec("123127726.548762582"));
        assert_eq!(a * dec("1000000000"), dec("123127726548.762582"));
        assert_eq!(a * dec("1000000000000"), dec("123127726548762.582"));
        assert_eq!(a * dec("1000000000000000"), dec("123127726548762582"));
        assert_eq!(a * dec("1000000000000000000"), dec("123127726548762582000"));
        assert_eq!(dec("1") * a, dec("123.127726548762582"));
        assert_eq!(dec("10") * a, dec("1231.27726548762582"));
        assert_eq!(dec("100") * a, dec("12312.7726548762582"));
        assert_eq!(dec("1000") * a, dec("123127.726548762582"));
        assert_eq!(dec("1000000") * a, dec("123127726.548762582"));
        assert_eq!(dec("1000000000") * a, dec("123127726548.762582"));
        assert_eq!(dec("1000000000000") * a, dec("123127726548762.582"));
        assert_eq!(dec("1000000000000000") * a, dec("123127726548762582"));
        assert_eq!(dec("1000000000000000000") * a, dec("123127726548762582000"));

        // Move right
        let max = Decimal256::MAX;
        assert_eq!(
            max * dec("1.0"),
            dec("115792089237316195423570985008687907853269984665640564039457.584007913129639935")
        );
        assert_eq!(
            max * dec("0.1"),
            dec("11579208923731619542357098500868790785326998466564056403945.758400791312963993")
        );
        assert_eq!(
            max * dec("0.01"),
            dec("1157920892373161954235709850086879078532699846656405640394.575840079131296399")
        );
        assert_eq!(
            max * dec("0.001"),
            dec("115792089237316195423570985008687907853269984665640564039.457584007913129639")
        );
        assert_eq!(
            max * dec("0.000001"),
            dec("115792089237316195423570985008687907853269984665640564.039457584007913129")
        );
        assert_eq!(
            max * dec("0.000000001"),
            dec("115792089237316195423570985008687907853269984665640.564039457584007913")
        );
        assert_eq!(
            max * dec("0.000000000001"),
            dec("115792089237316195423570985008687907853269984665.640564039457584007")
        );
        assert_eq!(
            max * dec("0.000000000000001"),
            dec("115792089237316195423570985008687907853269984.665640564039457584")
        );
        assert_eq!(
            max * dec("0.000000000000000001"),
            dec("115792089237316195423570985008687907853269.984665640564039457")
        );

        // works for refs
        let a = Decimal256::percent(20);
        let b = Decimal256::percent(30);
        let expected = Decimal256::percent(6);
        assert_eq!(a * b, expected);
        assert_eq!(&a * b, expected);
        assert_eq!(a * &b, expected);
        assert_eq!(&a * &b, expected);
    }

    #[test]
    fn decimal256_mul_assign_works() {
        let mut a = Decimal256::percent(15);
        a *= Decimal256::percent(60);
        assert_eq!(a, Decimal256::percent(9));

        // works for refs
        let mut a = Decimal256::percent(50);
        let b = Decimal256::percent(20);
        a *= &b;
        assert_eq!(a, Decimal256::percent(10));
    }

    #[test]
    #[should_panic(expected = "attempt to multiply with overflow")]
    fn decimal256_mul_overflow_panics() {
        let _value = Decimal256::MAX * Decimal256::percent(101);
    }

    #[test]
    fn decimal256_checked_mul() {
        let test_data = [
            (Decimal256::zero(), Decimal256::zero()),
            (Decimal256::zero(), Decimal256::one()),
            (Decimal256::one(), Decimal256::zero()),
            (Decimal256::percent(10), Decimal256::zero()),
            (Decimal256::percent(10), Decimal256::percent(5)),
            (Decimal256::MAX, Decimal256::one()),
            (
                Decimal256::MAX / Uint256::from_uint128(2u128.into()),
                Decimal256::percent(200),
            ),
            (Decimal256::permille(6), Decimal256::permille(13)),
        ];

        // The regular std::ops::Mul is our source of truth for these tests.
        for (x, y) in test_data.into_iter() {
            assert_eq!(x * y, x.checked_mul(y).unwrap());
        }
    }

    #[test]
    fn decimal256_checked_mul_overflow() {
        assert_eq!(
            Decimal256::MAX.checked_mul(Decimal256::percent(200)),
            Err(OverflowError {
                operation: crate::OverflowOperation::Mul,
                operand1: Decimal256::MAX.to_string(),
                operand2: Decimal256::percent(200).to_string(),
            })
        );
    }

    #[test]
    // in this test the Decimal256 is on the right
    fn uint128_decimal_multiply() {
        // a*b
        let left = Uint256::from(300u128);
        let right = Decimal256::one() + Decimal256::percent(50); // 1.5
        assert_eq!(left * right, Uint256::from(450u32));

        // a*0
        let left = Uint256::from(300u128);
        let right = Decimal256::zero();
        assert_eq!(left * right, Uint256::from(0u128));

        // 0*a
        let left = Uint256::from(0u128);
        let right = Decimal256::one() + Decimal256::percent(50); // 1.5
        assert_eq!(left * right, Uint256::from(0u128));
    }

    #[test]
    // in this test the Decimal256 is on the left
    fn decimal256_uint128_multiply() {
        // a*b
        let left = Decimal256::one() + Decimal256::percent(50); // 1.5
        let right = Uint256::from(300u128);
        assert_eq!(left * right, Uint256::from(450u128));

        // 0*a
        let left = Decimal256::zero();
        let right = Uint256::from(300u128);
        assert_eq!(left * right, Uint256::from(0u128));

        // a*0
        let left = Decimal256::one() + Decimal256::percent(50); // 1.5
        let right = Uint256::from(0u128);
        assert_eq!(left * right, Uint256::from(0u128));
    }

    #[test]
    #[allow(clippy::op_ref)]
    fn decimal256_implements_div() {
        let one = Decimal256::one();
        let two = one + one;
        let half = Decimal256::percent(50);

        // 1/x and x/1
        assert_eq!(one / Decimal256::percent(1), Decimal256::percent(10_000));
        assert_eq!(one / Decimal256::percent(10), Decimal256::percent(1_000));
        assert_eq!(one / Decimal256::percent(100), Decimal256::percent(100));
        assert_eq!(one / Decimal256::percent(1000), Decimal256::percent(10));
        assert_eq!(Decimal256::percent(0) / one, Decimal256::percent(0));
        assert_eq!(Decimal256::percent(1) / one, Decimal256::percent(1));
        assert_eq!(Decimal256::percent(10) / one, Decimal256::percent(10));
        assert_eq!(Decimal256::percent(100) / one, Decimal256::percent(100));
        assert_eq!(Decimal256::percent(1000) / one, Decimal256::percent(1000));

        // double
        assert_eq!(two / Decimal256::percent(1), Decimal256::percent(20_000));
        assert_eq!(two / Decimal256::percent(10), Decimal256::percent(2_000));
        assert_eq!(two / Decimal256::percent(100), Decimal256::percent(200));
        assert_eq!(two / Decimal256::percent(1000), Decimal256::percent(20));
        assert_eq!(Decimal256::percent(0) / two, Decimal256::percent(0));
        assert_eq!(Decimal256::percent(1) / two, dec("0.005"));
        assert_eq!(Decimal256::percent(10) / two, Decimal256::percent(5));
        assert_eq!(Decimal256::percent(100) / two, Decimal256::percent(50));
        assert_eq!(Decimal256::percent(1000) / two, Decimal256::percent(500));

        // half
        assert_eq!(half / Decimal256::percent(1), Decimal256::percent(5_000));
        assert_eq!(half / Decimal256::percent(10), Decimal256::percent(500));
        assert_eq!(half / Decimal256::percent(100), Decimal256::percent(50));
        assert_eq!(half / Decimal256::percent(1000), Decimal256::percent(5));
        assert_eq!(Decimal256::percent(0) / half, Decimal256::percent(0));
        assert_eq!(Decimal256::percent(1) / half, Decimal256::percent(2));
        assert_eq!(Decimal256::percent(10) / half, Decimal256::percent(20));
        assert_eq!(Decimal256::percent(100) / half, Decimal256::percent(200));
        assert_eq!(Decimal256::percent(1000) / half, Decimal256::percent(2000));

        // Move right
        let a = dec("123127726548762582");
        assert_eq!(a / dec("1"), dec("123127726548762582"));
        assert_eq!(a / dec("10"), dec("12312772654876258.2"));
        assert_eq!(a / dec("100"), dec("1231277265487625.82"));
        assert_eq!(a / dec("1000"), dec("123127726548762.582"));
        assert_eq!(a / dec("1000000"), dec("123127726548.762582"));
        assert_eq!(a / dec("1000000000"), dec("123127726.548762582"));
        assert_eq!(a / dec("1000000000000"), dec("123127.726548762582"));
        assert_eq!(a / dec("1000000000000000"), dec("123.127726548762582"));
        assert_eq!(a / dec("1000000000000000000"), dec("0.123127726548762582"));
        assert_eq!(dec("1") / a, dec("0.000000000000000008"));
        assert_eq!(dec("10") / a, dec("0.000000000000000081"));
        assert_eq!(dec("100") / a, dec("0.000000000000000812"));
        assert_eq!(dec("1000") / a, dec("0.000000000000008121"));
        assert_eq!(dec("1000000") / a, dec("0.000000000008121647"));
        assert_eq!(dec("1000000000") / a, dec("0.000000008121647560"));
        assert_eq!(dec("1000000000000") / a, dec("0.000008121647560868"));
        assert_eq!(dec("1000000000000000") / a, dec("0.008121647560868164"));
        assert_eq!(dec("1000000000000000000") / a, dec("8.121647560868164773"));

        // Move left
        let a = dec("0.123127726548762582");
        assert_eq!(a / dec("1.0"), dec("0.123127726548762582"));
        assert_eq!(a / dec("0.1"), dec("1.23127726548762582"));
        assert_eq!(a / dec("0.01"), dec("12.3127726548762582"));
        assert_eq!(a / dec("0.001"), dec("123.127726548762582"));
        assert_eq!(a / dec("0.000001"), dec("123127.726548762582"));
        assert_eq!(a / dec("0.000000001"), dec("123127726.548762582"));
        assert_eq!(a / dec("0.000000000001"), dec("123127726548.762582"));
        assert_eq!(a / dec("0.000000000000001"), dec("123127726548762.582"));
        assert_eq!(a / dec("0.000000000000000001"), dec("123127726548762582"));

        assert_eq!(
            Decimal256::percent(15) / Decimal256::percent(60),
            Decimal256::percent(25)
        );

        // works for refs
        let a = Decimal256::percent(100);
        let b = Decimal256::percent(20);
        let expected = Decimal256::percent(500);
        assert_eq!(a / b, expected);
        assert_eq!(&a / b, expected);
        assert_eq!(a / &b, expected);
        assert_eq!(&a / &b, expected);
    }

    #[test]
    fn decimal256_div_assign_works() {
        let mut a = Decimal256::percent(15);
        a /= Decimal256::percent(20);
        assert_eq!(a, Decimal256::percent(75));

        // works for refs
        let mut a = Decimal256::percent(50);
        let b = Decimal256::percent(20);
        a /= &b;
        assert_eq!(a, Decimal256::percent(250));
    }

    #[test]
    #[should_panic(expected = "Division failed - multiplication overflow")]
    fn decimal256_div_overflow_panics() {
        let _value = Decimal256::MAX / Decimal256::percent(10);
    }

    #[test]
    #[should_panic(expected = "Division failed - denominator must not be zero")]
    fn decimal256_div_by_zero_panics() {
        let _value = Decimal256::one() / Decimal256::zero();
    }

    #[test]
    fn decimal256_uint128_division() {
        // a/b
        let left = Decimal256::percent(150); // 1.5
        let right = Uint256::from(3u128);
        assert_eq!(left / right, Decimal256::percent(50));

        // 0/a
        let left = Decimal256::zero();
        let right = Uint256::from(300u128);
        assert_eq!(left / right, Decimal256::zero());
    }

    #[test]
    #[should_panic(expected = "attempt to divide by zero")]
    fn decimal256_uint128_divide_by_zero() {
        let left = Decimal256::percent(150); // 1.5
        let right = Uint256::from(0u128);
        let _result = left / right;
    }

    #[test]
    fn decimal256_uint128_div_assign() {
        // a/b
        let mut dec = Decimal256::percent(150); // 1.5
        dec /= Uint256::from(3u128);
        assert_eq!(dec, Decimal256::percent(50));

        // 0/a
        let mut dec = Decimal256::zero();
        dec /= Uint256::from(300u128);
        assert_eq!(dec, Decimal256::zero());
    }

    #[test]
    #[should_panic(expected = "attempt to divide by zero")]
    fn decimal256_uint128_div_assign_by_zero() {
        // a/0
        let mut dec = Decimal256::percent(50);
        dec /= Uint256::from(0u128);
    }

    #[test]
    fn decimal256_uint128_sqrt() {
        assert_eq!(Decimal256::percent(900).sqrt(), Decimal256::percent(300));

        assert!(Decimal256::percent(316) < Decimal256::percent(1000).sqrt());
        assert!(Decimal256::percent(1000).sqrt() < Decimal256::percent(317));
    }

    /// sqrt(2) is an irrational number, i.e. all 36 decimal places should be used.
    #[test]
    fn decimal256_uint128_sqrt_is_precise() {
        assert_eq!(
            Decimal256::from_str("2").unwrap().sqrt(),
            Decimal256::from_str("1.414213562373095048").unwrap() // https://www.wolframalpha.com/input/?i=sqrt%282%29
        );
    }

    #[test]
    fn decimal256_uint128_sqrt_does_not_overflow() {
        assert_eq!(
            Decimal256::from_str("40000000000000000000000000000000000000000000000000000000000")
                .unwrap()
                .sqrt(),
            Decimal256::from_str("200000000000000000000000000000").unwrap()
        );
    }

    #[test]
    fn decimal256_uint128_sqrt_intermediate_precision_used() {
        assert_eq!(
            Decimal256::from_str("40000000000000000000000000000000000000000000000001")
                .unwrap()
                .sqrt(),
            // The last few digits (39110) are truncated below due to the algorithm
            // we use. Larger numbers will cause less precision.
            // https://www.wolframalpha.com/input/?i=sqrt%2840000000000000000000000000000000000000000000000001%29
            Decimal256::from_str("6324555320336758663997787.088865437067400000").unwrap()
        );
    }

    #[test]
    fn decimal256_checked_pow() {
        for exp in 0..10 {
            assert_eq!(
                Decimal256::one().checked_pow(exp).unwrap(),
                Decimal256::one()
            );
        }

        // This case is mathematically undefined but we ensure consistency with Rust stdandard types
        // https://play.rust-lang.org/?version=stable&mode=debug&edition=2021&gist=20df6716048e77087acd40194b233494
        assert_eq!(
            Decimal256::zero().checked_pow(0).unwrap(),
            Decimal256::one()
        );

        for exp in 1..10 {
            assert_eq!(
                Decimal256::zero().checked_pow(exp).unwrap(),
                Decimal256::zero()
            );
        }

        for num in &[
            Decimal256::percent(50),
            Decimal256::percent(99),
            Decimal256::percent(200),
        ] {
            assert_eq!(num.checked_pow(0).unwrap(), Decimal256::one())
        }

        assert_eq!(
            Decimal256::percent(20).checked_pow(2).unwrap(),
            Decimal256::percent(4)
        );

        assert_eq!(
            Decimal256::percent(20).checked_pow(3).unwrap(),
            Decimal256::permille(8)
        );

        assert_eq!(
            Decimal256::percent(200).checked_pow(4).unwrap(),
            Decimal256::percent(1600)
        );

        assert_eq!(
            Decimal256::percent(200).checked_pow(4).unwrap(),
            Decimal256::percent(1600)
        );

        assert_eq!(
            Decimal256::percent(700).checked_pow(5).unwrap(),
            Decimal256::percent(1680700)
        );

        assert_eq!(
            Decimal256::percent(700).checked_pow(8).unwrap(),
            Decimal256::percent(576480100)
        );

        assert_eq!(
            Decimal256::percent(700).checked_pow(10).unwrap(),
            Decimal256::percent(28247524900)
        );

        assert_eq!(
            Decimal256::percent(120).checked_pow(123).unwrap(),
            Decimal256(5486473221892422150877397607u128.into())
        );

        assert_eq!(
            Decimal256::percent(10).checked_pow(2).unwrap(),
            Decimal256(10000000000000000u128.into())
        );

        assert_eq!(
            Decimal256::percent(10).checked_pow(18).unwrap(),
            Decimal256(1u128.into())
        );
    }

    #[test]
    fn decimal256_checked_pow_overflow() {
        assert_eq!(
            Decimal256::MAX.checked_pow(2),
            Err(OverflowError {
                operation: crate::OverflowOperation::Pow,
                operand1: Decimal256::MAX.to_string(),
                operand2: "2".to_string(),
            })
        );
    }

    #[test]
    fn decimal256_to_string() {
        // Integers
        assert_eq!(Decimal256::zero().to_string(), "0");
        assert_eq!(Decimal256::one().to_string(), "1");
        assert_eq!(Decimal256::percent(500).to_string(), "5");

        // Decimals
        assert_eq!(Decimal256::percent(125).to_string(), "1.25");
        assert_eq!(Decimal256::percent(42638).to_string(), "426.38");
        assert_eq!(Decimal256::percent(3).to_string(), "0.03");
        assert_eq!(Decimal256::permille(987).to_string(), "0.987");

        assert_eq!(
            Decimal256(Uint256::from(1u128)).to_string(),
            "0.000000000000000001"
        );
        assert_eq!(
            Decimal256(Uint256::from(10u128)).to_string(),
            "0.00000000000000001"
        );
        assert_eq!(
            Decimal256(Uint256::from(100u128)).to_string(),
            "0.0000000000000001"
        );
        assert_eq!(
            Decimal256(Uint256::from(1000u128)).to_string(),
            "0.000000000000001"
        );
        assert_eq!(
            Decimal256(Uint256::from(10000u128)).to_string(),
            "0.00000000000001"
        );
        assert_eq!(
            Decimal256(Uint256::from(100000u128)).to_string(),
            "0.0000000000001"
        );
        assert_eq!(
            Decimal256(Uint256::from(1000000u128)).to_string(),
            "0.000000000001"
        );
        assert_eq!(
            Decimal256(Uint256::from(10000000u128)).to_string(),
            "0.00000000001"
        );
        assert_eq!(
            Decimal256(Uint256::from(100000000u128)).to_string(),
            "0.0000000001"
        );
        assert_eq!(
            Decimal256(Uint256::from(1000000000u128)).to_string(),
            "0.000000001"
        );
        assert_eq!(
            Decimal256(Uint256::from(10000000000u128)).to_string(),
            "0.00000001"
        );
        assert_eq!(
            Decimal256(Uint256::from(100000000000u128)).to_string(),
            "0.0000001"
        );
        assert_eq!(
            Decimal256(Uint256::from(10000000000000u128)).to_string(),
            "0.00001"
        );
        assert_eq!(
            Decimal256(Uint256::from(100000000000000u128)).to_string(),
            "0.0001"
        );
        assert_eq!(
            Decimal256(Uint256::from(1000000000000000u128)).to_string(),
            "0.001"
        );
        assert_eq!(
            Decimal256(Uint256::from(10000000000000000u128)).to_string(),
            "0.01"
        );
        assert_eq!(
            Decimal256(Uint256::from(100000000000000000u128)).to_string(),
            "0.1"
        );
    }

    #[test]
    fn decimal256_iter_sum() {
        let items = vec![
            Decimal256::zero(),
            Decimal256::from_str("2").unwrap(),
            Decimal256::from_str("2").unwrap(),
        ];
        assert_eq!(
            items.iter().sum::<Decimal256>(),
            Decimal256::from_str("4").unwrap()
        );
        assert_eq!(
            items.into_iter().sum::<Decimal256>(),
            Decimal256::from_str("4").unwrap()
        );

        let empty: Vec<Decimal256> = vec![];
        assert_eq!(Decimal256::zero(), empty.iter().sum::<Decimal256>());
    }

    #[test]
    fn decimal256_serialize() {
        assert_eq!(to_vec(&Decimal256::zero()).unwrap(), br#""0""#);
        assert_eq!(to_vec(&Decimal256::one()).unwrap(), br#""1""#);
        assert_eq!(to_vec(&Decimal256::percent(8)).unwrap(), br#""0.08""#);
        assert_eq!(to_vec(&Decimal256::percent(87)).unwrap(), br#""0.87""#);
        assert_eq!(to_vec(&Decimal256::percent(876)).unwrap(), br#""8.76""#);
        assert_eq!(to_vec(&Decimal256::percent(8765)).unwrap(), br#""87.65""#);
    }

    #[test]
    fn decimal256_deserialize() {
        assert_eq!(
            from_slice::<Decimal256>(br#""0""#).unwrap(),
            Decimal256::zero()
        );
        assert_eq!(
            from_slice::<Decimal256>(br#""1""#).unwrap(),
            Decimal256::one()
        );
        assert_eq!(
            from_slice::<Decimal256>(br#""000""#).unwrap(),
            Decimal256::zero()
        );
        assert_eq!(
            from_slice::<Decimal256>(br#""001""#).unwrap(),
            Decimal256::one()
        );

        assert_eq!(
            from_slice::<Decimal256>(br#""0.08""#).unwrap(),
            Decimal256::percent(8)
        );
        assert_eq!(
            from_slice::<Decimal256>(br#""0.87""#).unwrap(),
            Decimal256::percent(87)
        );
        assert_eq!(
            from_slice::<Decimal256>(br#""8.76""#).unwrap(),
            Decimal256::percent(876)
        );
        assert_eq!(
            from_slice::<Decimal256>(br#""87.65""#).unwrap(),
            Decimal256::percent(8765)
        );
    }

    #[test]
    fn decimal256_abs_diff_works() {
        let a = Decimal256::percent(285);
        let b = Decimal256::percent(200);
        let expected = Decimal256::percent(85);
        assert_eq!(a.abs_diff(b), expected);
        assert_eq!(b.abs_diff(a), expected);
    }

    #[test]
    #[allow(clippy::op_ref)]
    fn decimal256_rem_works() {
        // 4.02 % 1.11 = 0.69
        assert_eq!(
            Decimal256::percent(402) % Decimal256::percent(111),
            Decimal256::percent(69)
        );

        // 15.25 % 4 = 3.25
        assert_eq!(
            Decimal256::percent(1525) % Decimal256::percent(400),
            Decimal256::percent(325)
        );

        let a = Decimal256::percent(318);
        let b = Decimal256::percent(317);
        let expected = Decimal256::percent(1);
        assert_eq!(a % b, expected);
        assert_eq!(a % &b, expected);
        assert_eq!(&a % b, expected);
        assert_eq!(&a % &b, expected);
    }

    #[test]
    fn decimal_rem_assign_works() {
        let mut a = Decimal256::percent(17673);
        a %= Decimal256::percent(2362);
        assert_eq!(a, Decimal256::percent(1139)); // 176.73 % 23.62 = 11.39

        let mut a = Decimal256::percent(4262);
        let b = Decimal256::percent(1270);
        a %= &b;
        assert_eq!(a, Decimal256::percent(452)); // 42.62 % 12.7 = 4.52
    }

    #[test]
    #[should_panic(expected = "division by zero")]
    fn decimal256_rem_panics_for_zero() {
        let _ = Decimal256::percent(777) % Decimal256::zero();
    }

    #[test]
    fn decimal256_checked_methods() {
        // checked add
        assert_eq!(
            Decimal256::percent(402)
                .checked_add(Decimal256::percent(111))
                .unwrap(),
            Decimal256::percent(513)
        );
        assert!(matches!(
            Decimal256::MAX.checked_add(Decimal256::percent(1)),
            Err(OverflowError { .. })
        ));

        // checked sub
        assert_eq!(
            Decimal256::percent(1111)
                .checked_sub(Decimal256::percent(111))
                .unwrap(),
            Decimal256::percent(1000)
        );
        assert!(matches!(
            Decimal256::zero().checked_sub(Decimal256::percent(1)),
            Err(OverflowError { .. })
        ));

        // checked div
        assert_eq!(
            Decimal256::percent(30)
                .checked_div(Decimal256::percent(200))
                .unwrap(),
            Decimal256::percent(15)
        );
        assert_eq!(
            Decimal256::percent(88)
                .checked_div(Decimal256::percent(20))
                .unwrap(),
            Decimal256::percent(440)
        );
        assert!(matches!(
            Decimal256::MAX.checked_div(Decimal256::zero()),
            Err(CheckedFromRatioError::DivideByZero { .. })
        ));
        assert!(matches!(
            Decimal256::MAX.checked_div(Decimal256::percent(1)),
            Err(CheckedFromRatioError::Overflow { .. })
        ));

        // checked rem
        assert_eq!(
            Decimal256::percent(402)
                .checked_rem(Decimal256::percent(111))
                .unwrap(),
            Decimal256::percent(69)
        );
        assert_eq!(
            Decimal256::percent(1525)
                .checked_rem(Decimal256::percent(400))
                .unwrap(),
            Decimal256::percent(325)
        );
        assert!(matches!(
            Decimal256::MAX.checked_rem(Decimal256::zero()),
            Err(DivideByZeroError { .. })
        ));
    }

    #[test]
    fn decimal256_pow_works() {
        assert_eq!(Decimal256::percent(200).pow(2), Decimal256::percent(400));
        assert_eq!(
            Decimal256::percent(200).pow(10),
            Decimal256::percent(102400)
        );
    }

    #[test]
    #[should_panic]
    fn decimal256_pow_overflow_panics() {
        Decimal256::MAX.pow(2u32);
    }

    #[test]
    fn decimal256_saturating_works() {
        assert_eq!(
            Decimal256::percent(200).saturating_add(Decimal256::percent(200)),
            Decimal256::percent(400)
        );
        assert_eq!(
            Decimal256::MAX.saturating_add(Decimal256::percent(200)),
            Decimal256::MAX
        );
        assert_eq!(
            Decimal256::percent(200).saturating_sub(Decimal256::percent(100)),
            Decimal256::percent(100)
        );
        assert_eq!(
            Decimal256::zero().saturating_sub(Decimal256::percent(200)),
            Decimal256::zero()
        );
        assert_eq!(
            Decimal256::percent(200).saturating_mul(Decimal256::percent(50)),
            Decimal256::percent(100)
        );
        assert_eq!(
            Decimal256::MAX.saturating_mul(Decimal256::percent(200)),
            Decimal256::MAX
        );
        assert_eq!(
            Decimal256::percent(400).saturating_pow(2u32),
            Decimal256::percent(1600)
        );
        assert_eq!(Decimal256::MAX.saturating_pow(2u32), Decimal256::MAX);
    }

    #[test]
    fn decimal256_rounding() {
        assert_eq!(Decimal256::one().floor(), Decimal256::one());
        assert_eq!(Decimal256::percent(150).floor(), Decimal256::one());
        assert_eq!(Decimal256::percent(199).floor(), Decimal256::one());
        assert_eq!(Decimal256::percent(200).floor(), Decimal256::percent(200));
        assert_eq!(Decimal256::percent(99).floor(), Decimal256::zero());

        assert_eq!(Decimal256::one().ceil(), Decimal256::one());
        assert_eq!(Decimal256::percent(150).ceil(), Decimal256::percent(200));
        assert_eq!(Decimal256::percent(199).ceil(), Decimal256::percent(200));
        assert_eq!(Decimal256::percent(99).ceil(), Decimal256::one());
        assert_eq!(Decimal256(Uint256::from(1u128)).ceil(), Decimal256::one());
    }

    #[test]
    #[should_panic(expected = "attempt to ceil with overflow")]
    fn decimal256_ceil_panics() {
        let _ = Decimal256::MAX.ceil();
    }

    #[test]
    fn decimal256_checked_ceil() {
        assert_eq!(
            Decimal256::percent(199).checked_ceil(),
            Ok(Decimal256::percent(200))
        );
        assert_eq!(Decimal256::MAX.checked_ceil(), Err(RoundUpOverflowError));
    }

    #[test]
    fn decimal256_partial_eq() {
        let test_cases = [
            ("1", "1", true),
            ("0.5", "0.5", true),
            ("0.5", "0.51", false),
            ("0", "0.00000", true),
        ]
        .into_iter()
        .map(|(lhs, rhs, expected)| (dec(lhs), dec(rhs), expected));

        #[allow(clippy::op_ref)]
        for (lhs, rhs, expected) in test_cases {
            assert_eq!(lhs == rhs, expected);
            assert_eq!(&lhs == rhs, expected);
            assert_eq!(lhs == &rhs, expected);
            assert_eq!(&lhs == &rhs, expected);
        }
    }
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
    fn isqrt(self) -> Self {
        let mut x0 = self >> 1;

        if x0 > 0.into() {
            let mut x1 = (x0 + self / x0) >> 1;

            while x1 < x0 {
                x0 = x1;
                x1 = (x0 + self / x0) >> 1;
            }

            return x0;
        }
        self
    }
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
    fn isqrt_primitives() {
        // Let's check correctness.
        assert_eq!(0u8.isqrt(), 0);
        assert_eq!(1u8.isqrt(), 1);
        assert_eq!(24u8.isqrt(), 4);
        assert_eq!(25u8.isqrt(), 5);
        assert_eq!(26u8.isqrt(), 5);
        assert_eq!(36u8.isqrt(), 6);

        // Let's also check different types.
        assert_eq!(26u8.isqrt(), 5);
        assert_eq!(26u16.isqrt(), 5);
        assert_eq!(26u32.isqrt(), 5);
        assert_eq!(26u64.isqrt(), 5);
        assert_eq!(26u128.isqrt(), 5);
    }

    #[test]
    fn isqrt_uint64() {
        assert_eq!(Uint64::new(24).isqrt(), Uint64::new(4));
    }

    #[test]
    fn isqrt_uint128() {
        assert_eq!(Uint128::new(24).isqrt(), Uint128::new(4));
    }

    #[test]
    fn isqrt_uint256() {
        assert_eq!(Uint256::from(24u32).isqrt(), Uint256::from(4u32));
        assert_eq!(
            (Uint256::from(u128::MAX) * Uint256::from(u128::MAX)).isqrt(),
            Uint256::try_from("340282366920938463463374607431768211455").unwrap()
        );
    }

    #[test]
    fn isqrt_uint512() {
        assert_eq!(Uint512::from(24u32).isqrt(), Uint512::from(4u32));
        assert_eq!(
            (Uint512::from(Uint256::MAX) * Uint512::from(Uint256::MAX)).isqrt(),
            Uint512::try_from(
                "115792089237316195423570985008687907853269984665640564039457584007913129639935"
            )
            .unwrap()
        );
    }
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
    pub const fn new(value: u128) -> Self {
        Uint128(value)
    }

    /// Creates a Uint128(0)
    #[inline]
    pub const fn zero() -> Self {
        Uint128(0)
    }

    /// Creates a Uint128(1)
    #[inline]
    pub const fn one() -> Self {
        Self(1)
    }

    /// Returns a copy of the internal data
    pub const fn u128(&self) -> u128 {
        self.0
    }

    /// Returns a copy of the number as big endian bytes.
    pub const fn to_be_bytes(self) -> [u8; 16] {
        self.0.to_be_bytes()
    }

    /// Returns a copy of the number as little endian bytes.
    pub const fn to_le_bytes(self) -> [u8; 16] {
        self.0.to_le_bytes()
    }

    pub const fn is_zero(&self) -> bool {
        self.0 == 0
    }

    pub fn pow(self, exp: u32) -> Self {
        self.0.pow(exp).into()
    }

    /// Returns `self * numerator / denominator`.
    ///
    /// Due to the nature of the integer division involved, the result is always floored.
    /// E.g. 5 * 99/100 = 4.
    pub fn multiply_ratio<A: Into<u128>, B: Into<u128>>(
        &self,
        numerator: A,
        denominator: B,
    ) -> Uint128 {
        match self.checked_multiply_ratio(numerator, denominator) {
            Ok(value) => value,
            Err(CheckedMultiplyRatioError::DivideByZero) => {
                panic!("Denominator must not be zero")
            }
            Err(CheckedMultiplyRatioError::Overflow) => panic!("Multiplication overflow"),
        }
    }

    /// Returns `self * numerator / denominator`.
    ///
    /// Due to the nature of the integer division involved, the result is always floored.
    /// E.g. 5 * 99/100 = 4.
    pub fn checked_multiply_ratio<A: Into<u128>, B: Into<u128>>(
        &self,
        numerator: A,
        denominator: B,
    ) -> Result<Uint128, CheckedMultiplyRatioError> {
        let numerator: u128 = numerator.into();
        let denominator: u128 = denominator.into();
        if denominator == 0 {
            return Err(CheckedMultiplyRatioError::DivideByZero);
        }
        match (self.full_mul(numerator) / Uint256::from(denominator)).try_into() {
            Ok(ratio) => Ok(ratio),
            Err(_) => Err(CheckedMultiplyRatioError::Overflow),
        }
    }

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
    pub fn full_mul(self, rhs: impl Into<u128>) -> Uint256 {
        Uint256::from(self.u128())
            .checked_mul(Uint256::from(rhs.into()))
            .unwrap()
    }

    pub fn checked_add(self, other: Self) -> Result<Self, OverflowError> {
        self.0
            .checked_add(other.0)
            .map(Self)
            .ok_or_else(|| OverflowError::new(OverflowOperation::Add, self, other))
    }

    pub fn checked_sub(self, other: Self) -> Result<Self, OverflowError> {
        self.0
            .checked_sub(other.0)
            .map(Self)
            .ok_or_else(|| OverflowError::new(OverflowOperation::Sub, self, other))
    }

    pub fn checked_mul(self, other: Self) -> Result<Self, OverflowError> {
        self.0
            .checked_mul(other.0)
            .map(Self)
            .ok_or_else(|| OverflowError::new(OverflowOperation::Mul, self, other))
    }

    pub fn checked_pow(self, exp: u32) -> Result<Self, OverflowError> {
        self.0
            .checked_pow(exp)
            .map(Self)
            .ok_or_else(|| OverflowError::new(OverflowOperation::Pow, self, exp))
    }

    pub fn checked_div(self, other: Self) -> Result<Self, DivideByZeroError> {
        self.0
            .checked_div(other.0)
            .map(Self)
            .ok_or_else(|| DivideByZeroError::new(self))
    }

    pub fn checked_div_euclid(self, other: Self) -> Result<Self, DivideByZeroError> {
        self.0
            .checked_div_euclid(other.0)
            .map(Self)
            .ok_or_else(|| DivideByZeroError::new(self))
    }

    pub fn checked_rem(self, other: Self) -> Result<Self, DivideByZeroError> {
        self.0
            .checked_rem(other.0)
            .map(Self)
            .ok_or_else(|| DivideByZeroError::new(self))
    }

    #[inline]
    pub fn wrapping_add(self, other: Self) -> Self {
        Self(self.0.wrapping_add(other.0))
    }

    #[inline]
    pub fn wrapping_sub(self, other: Self) -> Self {
        Self(self.0.wrapping_sub(other.0))
    }

    #[inline]
    pub fn wrapping_mul(self, other: Self) -> Self {
        Self(self.0.wrapping_mul(other.0))
    }

    #[inline]
    pub fn wrapping_pow(self, other: u32) -> Self {
        Self(self.0.wrapping_pow(other))
    }

    pub fn saturating_add(self, other: Self) -> Self {
        Self(self.0.saturating_add(other.0))
    }

    pub fn saturating_sub(self, other: Self) -> Self {
        Self(self.0.saturating_sub(other.0))
    }

    pub fn saturating_mul(self, other: Self) -> Self {
        Self(self.0.saturating_mul(other.0))
    }

    pub fn saturating_pow(self, exp: u32) -> Self {
        Self(self.0.saturating_pow(exp))
    }

    pub const fn abs_diff(self, other: Self) -> Self {
        Self(if self.0 < other.0 {
            other.0 - self.0
        } else {
            self.0 - other.0
        })
    }
}

// `From<u{128,64,32,16,8}>` is implemented manually instead of
// using `impl<T: Into<u128>> From<T> for Uint128` because
// of the conflict with `TryFrom<&str>` as described here
// https://stackoverflow.com/questions/63136970/how-do-i-work-around-the-upstream-crates-may-add-a-new-impl-of-trait-error

impl From<Uint64> for Uint128 {
    fn from(val: Uint64) -> Self {
        val.u64().into()
    }
}

impl From<u128> for Uint128 {
    fn from(val: u128) -> Self {
        Uint128(val)
    }
}

impl From<u64> for Uint128 {
    fn from(val: u64) -> Self {
        Uint128(val.into())
    }
}

impl From<u32> for Uint128 {
    fn from(val: u32) -> Self {
        Uint128(val.into())
    }
}

impl From<u16> for Uint128 {
    fn from(val: u16) -> Self {
        Uint128(val.into())
    }
}

impl From<u8> for Uint128 {
    fn from(val: u8) -> Self {
        Uint128(val.into())
    }
}

impl TryFrom<Uint128> for Uint64 {
    type Error = ConversionOverflowError;

    fn try_from(value: Uint128) -> Result<Self, Self::Error> {
        Ok(Uint64::new(value.0.try_into().map_err(|_| {
            ConversionOverflowError::new("Uint128", "Uint64", value.to_string())
        })?))
    }
}

impl TryFrom<&str> for Uint128 {
    type Error = StdError;

    fn try_from(val: &str) -> Result<Self, Self::Error> {
        Self::from_str(val)
    }
}

impl FromStr for Uint128 {
    type Err = StdError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.parse::<u128>() {
            Ok(u) => Ok(Uint128(u)),
            Err(e) => Err(StdError::generic_err(format!("Parsing u128: {}", e))),
        }
    }
}

impl From<Uint128> for String {
    fn from(original: Uint128) -> Self {
        original.to_string()
    }
}

impl From<Uint128> for u128 {
    fn from(original: Uint128) -> Self {
        original.0
    }
}

impl fmt::Display for Uint128 {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl Add<Uint128> for Uint128 {
    type Output = Self;

    fn add(self, rhs: Self) -> Self {
        Uint128(
            self.u128()
                .checked_add(rhs.u128())
                .expect("attempt to add with overflow"),
        )
    }
}

impl<'a> Add<&'a Uint128> for Uint128 {
    type Output = Self;

    fn add(self, rhs: &'a Uint128) -> Self {
        self + *rhs
    }
}

impl Sub<Uint128> for Uint128 {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self {
        Uint128(
            self.u128()
                .checked_sub(rhs.u128())
                .expect("attempt to subtract with overflow"),
        )
    }
}
forward_ref_binop!(impl Sub, sub for Uint128, Uint128);

impl SubAssign<Uint128> for Uint128 {
    fn sub_assign(&mut self, rhs: Uint128) {
        *self = *self - rhs;
    }
}
forward_ref_op_assign!(impl SubAssign, sub_assign for Uint128, Uint128);

impl Mul<Uint128> for Uint128 {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        Self(
            self.u128()
                .checked_mul(rhs.u128())
                .expect("attempt to multiply with overflow"),
        )
    }
}
forward_ref_binop!(impl Mul, mul for Uint128, Uint128);

impl MulAssign<Uint128> for Uint128 {
    fn mul_assign(&mut self, rhs: Self) {
        *self = *self * rhs;
    }
}
forward_ref_op_assign!(impl MulAssign, mul_assign for Uint128, Uint128);

impl Div<Uint128> for Uint128 {
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        Self(
            self.u128()
                .checked_div(rhs.u128())
                .expect("attempt to divide by zero"),
        )
    }
}

impl<'a> Div<&'a Uint128> for Uint128 {
    type Output = Self;

    fn div(self, rhs: &'a Uint128) -> Self::Output {
        self / *rhs
    }
}

impl Shr<u32> for Uint128 {
    type Output = Self;

    fn shr(self, rhs: u32) -> Self::Output {
        Self(
            self.u128()
                .checked_shr(rhs)
                .expect("attempt to shift right with overflow"),
        )
    }
}

impl<'a> Shr<&'a u32> for Uint128 {
    type Output = Self;

    fn shr(self, rhs: &'a u32) -> Self::Output {
        self >> *rhs
    }
}

impl AddAssign<Uint128> for Uint128 {
    fn add_assign(&mut self, rhs: Uint128) {
        *self = *self + rhs;
    }
}

impl<'a> AddAssign<&'a Uint128> for Uint128 {
    fn add_assign(&mut self, rhs: &'a Uint128) {
        *self = *self + rhs;
    }
}

impl DivAssign<Uint128> for Uint128 {
    fn div_assign(&mut self, rhs: Self) {
        *self = *self / rhs;
    }
}

impl<'a> DivAssign<&'a Uint128> for Uint128 {
    fn div_assign(&mut self, rhs: &'a Uint128) {
        *self = *self / rhs;
    }
}

impl Rem for Uint128 {
    type Output = Self;

    /// # Panics
    ///
    /// This operation will panic if `rhs` is zero.
    #[inline]
    fn rem(self, rhs: Self) -> Self {
        Self(self.0.rem(rhs.0))
    }
}
forward_ref_binop!(impl Rem, rem for Uint128, Uint128);

impl RemAssign<Uint128> for Uint128 {
    fn rem_assign(&mut self, rhs: Uint128) {
        *self = *self % rhs;
    }
}
forward_ref_op_assign!(impl RemAssign, rem_assign for Uint128, Uint128);

impl ShrAssign<u32> for Uint128 {
    fn shr_assign(&mut self, rhs: u32) {
        *self = *self >> rhs;
    }
}

impl<'a> ShrAssign<&'a u32> for Uint128 {
    fn shr_assign(&mut self, rhs: &'a u32) {
        *self = *self >> rhs;
    }
}

impl Serialize for Uint128 {
    /// Serializes as an integer string using base 10
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: ser::Serializer,
    {
        serializer.serialize_str(&self.to_string())
    }
}

impl<'de> Deserialize<'de> for Uint128 {
    /// Deserialized from an integer string using base 10
    fn deserialize<D>(deserializer: D) -> Result<Uint128, D::Error>
    where
        D: Deserializer<'de>,
    {
        deserializer.deserialize_str(Uint128Visitor)
    }
}

struct Uint128Visitor;

impl<'de> de::Visitor<'de> for Uint128Visitor {
    type Value = Uint128;

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str("string-encoded integer")
    }

    fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
    where
        E: de::Error,
    {
        match v.parse::<u128>() {
            Ok(u) => Ok(Uint128(u)),
            Err(e) => Err(E::custom(format!("invalid Uint128 '{}' - {}", v, e))),
        }
    }
}

impl<A> std::iter::Sum<A> for Uint128
where
    Self: Add<A, Output = Self>,
{
    fn sum<I: Iterator<Item = A>>(iter: I) -> Self {
        iter.fold(Self::zero(), Add::add)
    }
}

impl PartialEq<&Uint128> for Uint128 {
    fn eq(&self, rhs: &&Uint128) -> bool {
        self == *rhs
    }
}

impl PartialEq<Uint128> for &Uint128 {
    fn eq(&self, rhs: &Uint128) -> bool {
        *self == rhs
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{from_slice, to_vec};

    #[test]
    fn uint128_zero_works() {
        let zero = Uint128::zero();
        assert_eq!(
            zero.to_be_bytes(),
            [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
        );
    }

    #[test]
    fn uint128_one_works() {
        let one = Uint128::one();
        assert_eq!(
            one.to_be_bytes(),
            [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1]
        );
    }

    #[test]
    fn uint128_convert_into() {
        let original = Uint128(12345);
        let a = u128::from(original);
        assert_eq!(a, 12345);

        let original = Uint128(12345);
        let a = String::from(original);
        assert_eq!(a, "12345");
    }

    #[test]
    fn uint128_convert_from() {
        let a = Uint128::from(5u128);
        assert_eq!(a.0, 5);

        let a = Uint128::from(5u64);
        assert_eq!(a.0, 5);

        let a = Uint128::from(5u32);
        assert_eq!(a.0, 5);

        let a = Uint128::from(5u16);
        assert_eq!(a.0, 5);

        let a = Uint128::from(5u8);
        assert_eq!(a.0, 5);

        let result = Uint128::try_from("34567");
        assert_eq!(result.unwrap().0, 34567);

        let result = Uint128::try_from("1.23");
        assert!(result.is_err());
    }

    #[test]
    fn uint128_implements_display() {
        let a = Uint128(12345);
        assert_eq!(format!("Embedded: {}", a), "Embedded: 12345");
        assert_eq!(a.to_string(), "12345");

        let a = Uint128(0);
        assert_eq!(format!("Embedded: {}", a), "Embedded: 0");
        assert_eq!(a.to_string(), "0");
    }

    #[test]
    fn uint128_display_padding_works() {
        let a = Uint128::from(123u64);
        assert_eq!(format!("Embedded: {:05}", a), "Embedded: 00123");
    }

    #[test]
    fn uint128_to_be_bytes_works() {
        assert_eq!(
            Uint128::zero().to_be_bytes(),
            [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
        );
        assert_eq!(
            Uint128::MAX.to_be_bytes(),
            [
                0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
                0xff, 0xff
            ]
        );
        assert_eq!(
            Uint128::new(1).to_be_bytes(),
            [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1]
        );
        // Python: `[b for b in (240282366920938463463374607431768124608).to_bytes(16, "big")]`
        assert_eq!(
            Uint128::new(240282366920938463463374607431768124608).to_be_bytes(),
            [180, 196, 179, 87, 165, 121, 59, 133, 246, 117, 221, 191, 255, 254, 172, 192]
        );
    }

    #[test]
    fn uint128_to_le_bytes_works() {
        assert_eq!(
            Uint128::zero().to_le_bytes(),
            [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
        );
        assert_eq!(
            Uint128::MAX.to_le_bytes(),
            [
                0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
                0xff, 0xff
            ]
        );
        assert_eq!(
            Uint128::new(1).to_le_bytes(),
            [1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
        );
        // Python: `[b for b in (240282366920938463463374607431768124608).to_bytes(16, "little")]`
        assert_eq!(
            Uint128::new(240282366920938463463374607431768124608).to_le_bytes(),
            [192, 172, 254, 255, 191, 221, 117, 246, 133, 59, 121, 165, 87, 179, 196, 180]
        );
    }

    #[test]
    fn uint128_is_zero_works() {
        assert!(Uint128::zero().is_zero());
        assert!(Uint128(0).is_zero());

        assert!(!Uint128(1).is_zero());
        assert!(!Uint128(123).is_zero());
    }

    #[test]
    fn uint128_json() {
        let orig = Uint128(1234567890987654321);
        let serialized = to_vec(&orig).unwrap();
        assert_eq!(serialized.as_slice(), b"\"1234567890987654321\"");
        let parsed: Uint128 = from_slice(&serialized).unwrap();
        assert_eq!(parsed, orig);
    }

    #[test]
    fn uint128_compare() {
        let a = Uint128(12345);
        let b = Uint128(23456);

        assert!(a < b);
        assert!(b > a);
        assert_eq!(a, Uint128(12345));
    }

    #[test]
    #[allow(clippy::op_ref)]
    fn uint128_math() {
        let a = Uint128(12345);
        let b = Uint128(23456);

        // test + with owned and reference right hand side
        assert_eq!(a + b, Uint128(35801));
        assert_eq!(a + &b, Uint128(35801));

        // test - with owned and reference right hand side
        assert_eq!(b - a, Uint128(11111));
        assert_eq!(b - &a, Uint128(11111));

        // test += with owned and reference right hand side
        let mut c = Uint128(300000);
        c += b;
        assert_eq!(c, Uint128(323456));
        let mut d = Uint128(300000);
        d += &b;
        assert_eq!(d, Uint128(323456));

        // test -= with owned and reference right hand side
        let mut c = Uint128(300000);
        c -= b;
        assert_eq!(c, Uint128(276544));
        let mut d = Uint128(300000);
        d -= &b;
        assert_eq!(d, Uint128(276544));

        // error result on underflow (- would produce negative result)
        let underflow_result = a.checked_sub(b);
        let OverflowError {
            operand1, operand2, ..
        } = underflow_result.unwrap_err();
        assert_eq!((operand1, operand2), (a.to_string(), b.to_string()));
    }

    #[test]
    #[should_panic]
    fn uint128_add_overflow_panics() {
        // almost_max is 2^128 - 10
        let almost_max = Uint128(340282366920938463463374607431768211446);
        let _ = almost_max + Uint128(12);
    }

    #[test]
    #[allow(clippy::op_ref)]
    fn uint128_sub_works() {
        assert_eq!(Uint128(2) - Uint128(1), Uint128(1));
        assert_eq!(Uint128(2) - Uint128(0), Uint128(2));
        assert_eq!(Uint128(2) - Uint128(2), Uint128(0));

        // works for refs
        let a = Uint128::new(10);
        let b = Uint128::new(3);
        let expected = Uint128::new(7);
        assert_eq!(a - b, expected);
        assert_eq!(a - &b, expected);
        assert_eq!(&a - b, expected);
        assert_eq!(&a - &b, expected);
    }

    #[test]
    #[should_panic]
    fn uint128_sub_overflow_panics() {
        let _ = Uint128(1) - Uint128(2);
    }

    #[test]
    fn uint128_sub_assign_works() {
        let mut a = Uint128(14);
        a -= Uint128(2);
        assert_eq!(a, Uint128(12));

        // works for refs
        let mut a = Uint128::new(10);
        let b = Uint128::new(3);
        let expected = Uint128::new(7);
        a -= &b;
        assert_eq!(a, expected);
    }

    #[test]
    #[allow(clippy::op_ref)]
    fn uint128_mul_works() {
        assert_eq!(
            Uint128::from(2u32) * Uint128::from(3u32),
            Uint128::from(6u32)
        );
        assert_eq!(Uint128::from(2u32) * Uint128::zero(), Uint128::zero());

        // works for refs
        let a = Uint128::from(11u32);
        let b = Uint128::from(3u32);
        let expected = Uint128::from(33u32);
        assert_eq!(a * b, expected);
        assert_eq!(a * &b, expected);
        assert_eq!(&a * b, expected);
        assert_eq!(&a * &b, expected);
    }

    #[test]
    fn uint128_mul_assign_works() {
        let mut a = Uint128::from(14u32);
        a *= Uint128::from(2u32);
        assert_eq!(a, Uint128::from(28u32));

        // works for refs
        let mut a = Uint128::from(10u32);
        let b = Uint128::from(3u32);
        a *= &b;
        assert_eq!(a, Uint128::from(30u32));
    }

    #[test]
    fn uint128_pow_works() {
        assert_eq!(Uint128::from(2u32).pow(2), Uint128::from(4u32));
        assert_eq!(Uint128::from(2u32).pow(10), Uint128::from(1024u32));
    }

    #[test]
    #[should_panic]
    fn uint128_pow_overflow_panics() {
        Uint128::MAX.pow(2u32);
    }

    #[test]
    fn uint128_multiply_ratio_works() {
        let base = Uint128(500);

        // factor 1/1
        assert_eq!(base.multiply_ratio(1u128, 1u128), base);
        assert_eq!(base.multiply_ratio(3u128, 3u128), base);
        assert_eq!(base.multiply_ratio(654321u128, 654321u128), base);
        assert_eq!(base.multiply_ratio(u128::MAX, u128::MAX), base);

        // factor 3/2
        assert_eq!(base.multiply_ratio(3u128, 2u128), Uint128(750));
        assert_eq!(base.multiply_ratio(333333u128, 222222u128), Uint128(750));

        // factor 2/3 (integer devision always floors the result)
        assert_eq!(base.multiply_ratio(2u128, 3u128), Uint128(333));
        assert_eq!(base.multiply_ratio(222222u128, 333333u128), Uint128(333));

        // factor 5/6 (integer devision always floors the result)
        assert_eq!(base.multiply_ratio(5u128, 6u128), Uint128(416));
        assert_eq!(base.multiply_ratio(100u128, 120u128), Uint128(416));
    }

    #[test]
    fn uint128_multiply_ratio_does_not_overflow_when_result_fits() {
        // Almost max value for Uint128.
        let base = Uint128(u128::MAX - 9);

        assert_eq!(base.multiply_ratio(2u128, 2u128), base);
    }

    #[test]
    #[should_panic]
    fn uint128_multiply_ratio_panicks_on_overflow() {
        // Almost max value for Uint128.
        let base = Uint128(u128::MAX - 9);

        assert_eq!(base.multiply_ratio(2u128, 1u128), base);
    }

    #[test]
    #[should_panic(expected = "Denominator must not be zero")]
    fn uint128_multiply_ratio_panics_for_zero_denominator() {
        Uint128(500).multiply_ratio(1u128, 0u128);
    }

    #[test]
    fn uint128_checked_multiply_ratio_does_not_panic() {
        assert_eq!(
            Uint128(500u128).checked_multiply_ratio(1u128, 0u128),
            Err(CheckedMultiplyRatioError::DivideByZero),
        );
        assert_eq!(
            Uint128(500u128).checked_multiply_ratio(u128::MAX, 1u128),
            Err(CheckedMultiplyRatioError::Overflow),
        );
    }

    #[test]
    fn sum_works() {
        let nums = vec![Uint128(17), Uint128(123), Uint128(540), Uint128(82)];
        let expected = Uint128(762);

        let sum_as_ref: Uint128 = nums.iter().sum();
        assert_eq!(expected, sum_as_ref);

        let sum_as_owned: Uint128 = nums.into_iter().sum();
        assert_eq!(expected, sum_as_owned);
    }

    #[test]
    fn uint128_methods() {
        // checked_*
        assert!(matches!(
            Uint128::MAX.checked_add(Uint128(1)),
            Err(OverflowError { .. })
        ));
        assert!(matches!(Uint128(1).checked_add(Uint128(1)), Ok(Uint128(2))));
        assert!(matches!(
            Uint128(0).checked_sub(Uint128(1)),
            Err(OverflowError { .. })
        ));
        assert!(matches!(Uint128(2).checked_sub(Uint128(1)), Ok(Uint128(1))));
        assert!(matches!(
            Uint128::MAX.checked_mul(Uint128(2)),
            Err(OverflowError { .. })
        ));
        assert!(matches!(Uint128(2).checked_mul(Uint128(2)), Ok(Uint128(4))));
        assert!(matches!(
            Uint128::MAX.checked_pow(2u32),
            Err(OverflowError { .. })
        ));
        assert!(matches!(Uint128(2).checked_pow(3), Ok(Uint128(8))));
        assert!(matches!(
            Uint128::MAX.checked_div(Uint128(0)),
            Err(DivideByZeroError { .. })
        ));
        assert!(matches!(Uint128(6).checked_div(Uint128(2)), Ok(Uint128(3))));
        assert!(matches!(
            Uint128::MAX.checked_div_euclid(Uint128(0)),
            Err(DivideByZeroError { .. })
        ));
        assert!(matches!(
            Uint128(6).checked_div_euclid(Uint128(2)),
            Ok(Uint128(3)),
        ));
        assert!(matches!(
            Uint128::MAX.checked_rem(Uint128(0)),
            Err(DivideByZeroError { .. })
        ));

        // saturating_*
        assert_eq!(Uint128::MAX.saturating_add(Uint128(1)), Uint128::MAX);
        assert_eq!(Uint128(0).saturating_sub(Uint128(1)), Uint128(0));
        assert_eq!(Uint128::MAX.saturating_mul(Uint128(2)), Uint128::MAX);
        assert_eq!(Uint128::MAX.saturating_pow(2), Uint128::MAX);
    }

    #[test]
    fn uint128_wrapping_methods() {
        // wrapping_add
        assert_eq!(Uint128(2).wrapping_add(Uint128(2)), Uint128(4)); // non-wrapping
        assert_eq!(Uint128::MAX.wrapping_add(Uint128(1)), Uint128(0)); // wrapping

        // wrapping_sub
        assert_eq!(Uint128(7).wrapping_sub(Uint128(5)), Uint128(2)); // non-wrapping
        assert_eq!(Uint128(0).wrapping_sub(Uint128(1)), Uint128::MAX); // wrapping

        // wrapping_mul
        assert_eq!(Uint128(3).wrapping_mul(Uint128(2)), Uint128(6)); // non-wrapping
        assert_eq!(
            Uint128::MAX.wrapping_mul(Uint128(2)),
            Uint128::MAX - Uint128::one()
        ); // wrapping

        // wrapping_pow
        assert_eq!(Uint128(2).wrapping_pow(3), Uint128(8)); // non-wrapping
        assert_eq!(Uint128::MAX.wrapping_pow(2), Uint128(1)); // wrapping
    }

    #[test]
    #[allow(clippy::op_ref)]
    fn uint128_implements_rem() {
        let a = Uint128::new(10);
        assert_eq!(a % Uint128::new(10), Uint128::zero());
        assert_eq!(a % Uint128::new(2), Uint128::zero());
        assert_eq!(a % Uint128::new(1), Uint128::zero());
        assert_eq!(a % Uint128::new(3), Uint128::new(1));
        assert_eq!(a % Uint128::new(4), Uint128::new(2));

        // works for refs
        let a = Uint128::new(10);
        let b = Uint128::new(3);
        let expected = Uint128::new(1);
        assert_eq!(a % b, expected);
        assert_eq!(a % &b, expected);
        assert_eq!(&a % b, expected);
        assert_eq!(&a % &b, expected);
    }

    #[test]
    #[should_panic(expected = "divisor of zero")]
    fn uint128_rem_panics_for_zero() {
        let _ = Uint128::new(10) % Uint128::zero();
    }

    #[test]
    #[allow(clippy::op_ref)]
    fn uint128_rem_works() {
        assert_eq!(
            Uint128::from(12u32) % Uint128::from(10u32),
            Uint128::from(2u32)
        );
        assert_eq!(Uint128::from(50u32) % Uint128::from(5u32), Uint128::zero());

        // works for refs
        let a = Uint128::from(42u32);
        let b = Uint128::from(5u32);
        let expected = Uint128::from(2u32);
        assert_eq!(a % b, expected);
        assert_eq!(a % &b, expected);
        assert_eq!(&a % b, expected);
        assert_eq!(&a % &b, expected);
    }

    #[test]
    fn uint128_rem_assign_works() {
        let mut a = Uint128::from(30u32);
        a %= Uint128::from(4u32);
        assert_eq!(a, Uint128::from(2u32));

        // works for refs
        let mut a = Uint128::from(25u32);
        let b = Uint128::from(6u32);
        a %= &b;
        assert_eq!(a, Uint128::from(1u32));
    }

    #[test]
    fn uint128_abs_diff_works() {
        let a = Uint128::from(42u32);
        let b = Uint128::from(5u32);
        let expected = Uint128::from(37u32);
        assert_eq!(a.abs_diff(b), expected);
        assert_eq!(b.abs_diff(a), expected);
    }

    #[test]
    fn uint128_partial_eq() {
        let test_cases = [(1, 1, true), (42, 42, true), (42, 24, false), (0, 0, true)]
            .into_iter()
            .map(|(lhs, rhs, expected)| (Uint128::new(lhs), Uint128::new(rhs), expected));

        #[allow(clippy::op_ref)]
        for (lhs, rhs, expected) in test_cases {
            assert_eq!(lhs == rhs, expected);
            assert_eq!(&lhs == rhs, expected);
            assert_eq!(lhs == &rhs, expected);
            assert_eq!(&lhs == &rhs, expected);
        }
    }
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
    pub const fn new(value: [u8; 32]) -> Self {
        Self::from_be_bytes(value)
    }

    /// Creates a Uint256(0)
    #[inline]
    pub const fn zero() -> Self {
        Uint256(U256::zero())
    }

    /// Creates a Uint256(1)
    #[inline]
    pub const fn one() -> Self {
        Self::from_be_bytes([
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 1,
        ])
    }

    pub const fn from_be_bytes(data: [u8; 32]) -> Self {
        let words: [u64; 4] = [
            u64::from_le_bytes([
                data[31], data[30], data[29], data[28], data[27], data[26], data[25], data[24],
            ]),
            u64::from_le_bytes([
                data[23], data[22], data[21], data[20], data[19], data[18], data[17], data[16],
            ]),
            u64::from_le_bytes([
                data[15], data[14], data[13], data[12], data[11], data[10], data[9], data[8],
            ]),
            u64::from_le_bytes([
                data[7], data[6], data[5], data[4], data[3], data[2], data[1], data[0],
            ]),
        ];
        Self(U256(words))
    }

    pub const fn from_le_bytes(data: [u8; 32]) -> Self {
        let words: [u64; 4] = [
            u64::from_le_bytes([
                data[0], data[1], data[2], data[3], data[4], data[5], data[6], data[7],
            ]),
            u64::from_le_bytes([
                data[8], data[9], data[10], data[11], data[12], data[13], data[14], data[15],
            ]),
            u64::from_le_bytes([
                data[16], data[17], data[18], data[19], data[20], data[21], data[22], data[23],
            ]),
            u64::from_le_bytes([
                data[24], data[25], data[26], data[27], data[28], data[29], data[30], data[31],
            ]),
        ];
        Uint256(U256(words))
    }

    /// A conversion from `u128` that, unlike the one provided by the `From` trait,
    /// can be used in a `const` context.
    pub const fn from_u128(num: u128) -> Self {
        let bytes = num.to_le_bytes();

        Self::from_le_bytes([
            bytes[0], bytes[1], bytes[2], bytes[3], bytes[4], bytes[5], bytes[6], bytes[7],
            bytes[8], bytes[9], bytes[10], bytes[11], bytes[12], bytes[13], bytes[14], bytes[15],
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        ])
    }

    /// A conversion from `Uint128` that, unlike the one provided by the `From` trait,
    /// can be used in a `const` context.
    pub const fn from_uint128(num: Uint128) -> Self {
        Self::from_u128(num.u128())
    }

    /// Returns a copy of the number as big endian bytes.
    pub const fn to_be_bytes(self) -> [u8; 32] {
        let words = [
            (self.0).0[3].to_be_bytes(),
            (self.0).0[2].to_be_bytes(),
            (self.0).0[1].to_be_bytes(),
            (self.0).0[0].to_be_bytes(),
        ];
        unsafe { std::mem::transmute::<[[u8; 8]; 4], [u8; 32]>(words) }
    }

    /// Returns a copy of the number as little endian bytes.
    pub const fn to_le_bytes(self) -> [u8; 32] {
        let words = [
            (self.0).0[0].to_le_bytes(),
            (self.0).0[1].to_le_bytes(),
            (self.0).0[2].to_le_bytes(),
            (self.0).0[3].to_le_bytes(),
        ];
        unsafe { std::mem::transmute::<[[u8; 8]; 4], [u8; 32]>(words) }
    }

    pub const fn is_zero(&self) -> bool {
        let words = (self.0).0;
        words[0] == 0 && words[1] == 0 && words[2] == 0 && words[3] == 0
    }

    pub fn pow(self, exp: u32) -> Self {
        let res = self.0.pow(exp.into());
        Self(res)
    }

    /// Returns `self * numerator / denominator`.
    ///
    /// Due to the nature of the integer division involved, the result is always floored.
    /// E.g. 5 * 99/100 = 4.
    pub fn multiply_ratio<A: Into<Uint256>, B: Into<Uint256>>(
        &self,
        numerator: A,
        denominator: B,
    ) -> Uint256 {
        match self.checked_multiply_ratio(numerator, denominator) {
            Ok(value) => value,
            Err(CheckedMultiplyRatioError::DivideByZero) => {
                panic!("Denominator must not be zero")
            }
            Err(CheckedMultiplyRatioError::Overflow) => panic!("Multiplication overflow"),
        }
    }

    /// Returns `self * numerator / denominator`.
    ///
    /// Due to the nature of the integer division involved, the result is always floored.
    /// E.g. 5 * 99/100 = 4.
    pub fn checked_multiply_ratio<A: Into<Uint256>, B: Into<Uint256>>(
        &self,
        numerator: A,
        denominator: B,
    ) -> Result<Uint256, CheckedMultiplyRatioError> {
        let numerator: Uint256 = numerator.into();
        let denominator: Uint256 = denominator.into();
        if denominator.is_zero() {
            return Err(CheckedMultiplyRatioError::DivideByZero);
        }
        match (self.full_mul(numerator) / Uint512::from(denominator)).try_into() {
            Ok(ratio) => Ok(ratio),
            Err(_) => Err(CheckedMultiplyRatioError::Overflow),
        }
    }

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
    pub fn full_mul(self, rhs: impl Into<Uint256>) -> Uint512 {
        Uint512::from(self)
            .checked_mul(Uint512::from(rhs.into()))
            .unwrap()
    }

    pub fn checked_add(self, other: Self) -> Result<Self, OverflowError> {
        self.0
            .checked_add(other.0)
            .map(Self)
            .ok_or_else(|| OverflowError::new(OverflowOperation::Add, self, other))
    }

    pub fn checked_sub(self, other: Self) -> Result<Self, OverflowError> {
        self.0
            .checked_sub(other.0)
            .map(Self)
            .ok_or_else(|| OverflowError::new(OverflowOperation::Sub, self, other))
    }

    pub fn checked_mul(self, other: Self) -> Result<Self, OverflowError> {
        self.0
            .checked_mul(other.0)
            .map(Self)
            .ok_or_else(|| OverflowError::new(OverflowOperation::Mul, self, other))
    }

    pub fn checked_pow(self, exp: u32) -> Result<Self, OverflowError> {
        self.0
            .checked_pow(exp.into())
            .map(Self)
            .ok_or_else(|| OverflowError::new(OverflowOperation::Pow, self, exp))
    }

    pub fn checked_div(self, other: Self) -> Result<Self, DivideByZeroError> {
        self.0
            .checked_div(other.0)
            .map(Self)
            .ok_or_else(|| DivideByZeroError::new(self))
    }

    pub fn checked_div_euclid(self, other: Self) -> Result<Self, DivideByZeroError> {
        self.checked_div(other)
    }

    pub fn checked_rem(self, other: Self) -> Result<Self, DivideByZeroError> {
        self.0
            .checked_rem(other.0)
            .map(Self)
            .ok_or_else(|| DivideByZeroError::new(self))
    }

    pub fn checked_shr(self, other: u32) -> Result<Self, OverflowError> {
        if other >= 256 {
            return Err(OverflowError::new(OverflowOperation::Shr, self, other));
        }

        Ok(Self(self.0.shr(other)))
    }

    pub fn checked_shl(self, other: u32) -> Result<Self, OverflowError> {
        if other >= 256 {
            return Err(OverflowError::new(OverflowOperation::Shl, self, other));
        }

        Ok(Self(self.0.shl(other)))
    }

    #[inline]
    pub fn wrapping_add(self, other: Self) -> Self {
        let (value, _did_overflow) = self.0.overflowing_add(other.0);
        Self(value)
    }

    #[inline]
    pub fn wrapping_sub(self, other: Self) -> Self {
        let (value, _did_overflow) = self.0.overflowing_sub(other.0);
        Self(value)
    }

    #[inline]
    pub fn wrapping_mul(self, other: Self) -> Self {
        let (value, _did_overflow) = self.0.overflowing_mul(other.0);
        Self(value)
    }

    #[inline]
    pub fn wrapping_pow(self, other: u32) -> Self {
        let (value, _did_overflow) = self.0.overflowing_pow(other.into());
        Self(value)
    }

    pub fn saturating_add(self, other: Self) -> Self {
        Self(self.0.saturating_add(other.0))
    }

    pub fn saturating_sub(self, other: Self) -> Self {
        Self(self.0.saturating_sub(other.0))
    }

    pub fn saturating_mul(self, other: Self) -> Self {
        Self(self.0.saturating_mul(other.0))
    }

    pub fn saturating_pow(self, exp: u32) -> Self {
        match self.checked_pow(exp) {
            Ok(value) => value,
            Err(_) => Self::MAX,
        }
    }

    pub fn abs_diff(self, other: Self) -> Self {
        if self < other {
            other - self
        } else {
            self - other
        }
    }
}

impl From<Uint128> for Uint256 {
    fn from(val: Uint128) -> Self {
        val.u128().into()
    }
}

impl From<Uint64> for Uint256 {
    fn from(val: Uint64) -> Self {
        val.u64().into()
    }
}

impl From<u128> for Uint256 {
    fn from(val: u128) -> Self {
        Uint256(val.into())
    }
}

impl From<u64> for Uint256 {
    fn from(val: u64) -> Self {
        Uint256(val.into())
    }
}

impl From<u32> for Uint256 {
    fn from(val: u32) -> Self {
        Uint256(val.into())
    }
}

impl From<u16> for Uint256 {
    fn from(val: u16) -> Self {
        Uint256(val.into())
    }
}

impl From<u8> for Uint256 {
    fn from(val: u8) -> Self {
        Uint256(val.into())
    }
}

impl TryFrom<Uint256> for Uint128 {
    type Error = ConversionOverflowError;

    fn try_from(value: Uint256) -> Result<Self, Self::Error> {
        Ok(Uint128::new(value.0.try_into().map_err(|_| {
            ConversionOverflowError::new("Uint256", "Uint128", value.to_string())
        })?))
    }
}

impl TryFrom<&str> for Uint256 {
    type Error = StdError;

    fn try_from(val: &str) -> Result<Self, Self::Error> {
        Self::from_str(val)
    }
}

impl FromStr for Uint256 {
    type Err = StdError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.is_empty() {
            return Err(StdError::generic_err("Parsing u256: received empty string"));
        }

        match U256::from_dec_str(s) {
            Ok(u) => Ok(Uint256(u)),
            Err(e) => Err(StdError::generic_err(format!("Parsing u256: {}", e))),
        }
    }
}

impl From<Uint256> for String {
    fn from(original: Uint256) -> Self {
        original.to_string()
    }
}

impl fmt::Display for Uint256 {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // The inner type doesn't work as expected with padding, so we
        // work around that.
        let unpadded = self.0.to_string();

        f.pad_integral(true, "", &unpadded)
    }
}

impl Add<Uint256> for Uint256 {
    type Output = Self;

    fn add(self, rhs: Self) -> Self {
        Self(
            self.0
                .checked_add(rhs.0)
                .expect("attempt to add with overflow"),
        )
    }
}

impl<'a> Add<&'a Uint256> for Uint256 {
    type Output = Self;

    fn add(self, rhs: &'a Uint256) -> Self {
        self + *rhs
    }
}

impl Sub<Uint256> for Uint256 {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self {
        Self(
            self.0
                .checked_sub(rhs.0)
                .expect("attempt to subtract with overflow"),
        )
    }
}
forward_ref_binop!(impl Sub, sub for Uint256, Uint256);

impl SubAssign<Uint256> for Uint256 {
    fn sub_assign(&mut self, rhs: Uint256) {
        *self = *self - rhs;
    }
}
forward_ref_op_assign!(impl SubAssign, sub_assign for Uint256, Uint256);

impl Div<Uint256> for Uint256 {
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        Self(
            self.0
                .checked_div(rhs.0)
                .expect("attempt to divide by zero"),
        )
    }
}

impl<'a> Div<&'a Uint256> for Uint256 {
    type Output = Self;

    fn div(self, rhs: &'a Uint256) -> Self::Output {
        self / *rhs
    }
}

impl Rem for Uint256 {
    type Output = Self;

    /// # Panics
    ///
    /// This operation will panic if `rhs` is zero.
    #[inline]
    fn rem(self, rhs: Self) -> Self {
        Self(self.0.rem(rhs.0))
    }
}
forward_ref_binop!(impl Rem, rem for Uint256, Uint256);

impl RemAssign<Uint256> for Uint256 {
    fn rem_assign(&mut self, rhs: Uint256) {
        *self = *self % rhs;
    }
}
forward_ref_op_assign!(impl RemAssign, rem_assign for Uint256, Uint256);

impl Mul<Uint256> for Uint256 {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        Self(
            self.0
                .checked_mul(rhs.0)
                .expect("attempt to multiply with overflow"),
        )
    }
}
forward_ref_binop!(impl Mul, mul for Uint256, Uint256);

impl MulAssign<Uint256> for Uint256 {
    fn mul_assign(&mut self, rhs: Self) {
        *self = *self * rhs;
    }
}
forward_ref_op_assign!(impl MulAssign, mul_assign for Uint256, Uint256);

impl Shr<u32> for Uint256 {
    type Output = Self;

    fn shr(self, rhs: u32) -> Self::Output {
        self.checked_shr(rhs).unwrap_or_else(|_| {
            panic!(
                "right shift error: {} is larger or equal than the number of bits in Uint256",
                rhs,
            )
        })
    }
}

impl<'a> Shr<&'a u32> for Uint256 {
    type Output = Self;

    fn shr(self, rhs: &'a u32) -> Self::Output {
        self.shr(*rhs)
    }
}

impl Shl<u32> for Uint256 {
    type Output = Self;

    fn shl(self, rhs: u32) -> Self::Output {
        self.checked_shl(rhs).unwrap_or_else(|_| {
            panic!(
                "left shift error: {} is larger or equal than the number of bits in Uint256",
                rhs,
            )
        })
    }
}

impl<'a> Shl<&'a u32> for Uint256 {
    type Output = Self;

    fn shl(self, rhs: &'a u32) -> Self::Output {
        self.shl(*rhs)
    }
}

impl AddAssign<Uint256> for Uint256 {
    fn add_assign(&mut self, rhs: Uint256) {
        *self = *self + rhs;
    }
}

impl<'a> AddAssign<&'a Uint256> for Uint256 {
    fn add_assign(&mut self, rhs: &'a Uint256) {
        *self = *self + rhs;
    }
}

impl DivAssign<Uint256> for Uint256 {
    fn div_assign(&mut self, rhs: Self) {
        *self = *self / rhs;
    }
}

impl<'a> DivAssign<&'a Uint256> for Uint256 {
    fn div_assign(&mut self, rhs: &'a Uint256) {
        *self = *self / rhs;
    }
}

impl ShrAssign<u32> for Uint256 {
    fn shr_assign(&mut self, rhs: u32) {
        *self = Shr::<u32>::shr(*self, rhs);
    }
}

impl<'a> ShrAssign<&'a u32> for Uint256 {
    fn shr_assign(&mut self, rhs: &'a u32) {
        *self = Shr::<u32>::shr(*self, *rhs);
    }
}

impl Serialize for Uint256 {
    /// Serializes as an integer string using base 10
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: ser::Serializer,
    {
        serializer.serialize_str(&self.to_string())
    }
}

impl<'de> Deserialize<'de> for Uint256 {
    /// Deserialized from an integer string using base 10
    fn deserialize<D>(deserializer: D) -> Result<Uint256, D::Error>
    where
        D: Deserializer<'de>,
    {
        deserializer.deserialize_str(Uint256Visitor)
    }
}

struct Uint256Visitor;

impl<'de> de::Visitor<'de> for Uint256Visitor {
    type Value = Uint256;

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str("string-encoded integer")
    }

    fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
    where
        E: de::Error,
    {
        Uint256::try_from(v).map_err(|e| E::custom(format!("invalid Uint256 '{}' - {}", v, e)))
    }
}

impl<A> std::iter::Sum<A> for Uint256
where
    Self: Add<A, Output = Self>,
{
    fn sum<I: Iterator<Item = A>>(iter: I) -> Self {
        iter.fold(Self::zero(), Add::add)
    }
}

impl PartialEq<&Uint256> for Uint256 {
    fn eq(&self, rhs: &&Uint256) -> bool {
        self == *rhs
    }
}

impl PartialEq<Uint256> for &Uint256 {
    fn eq(&self, rhs: &Uint256) -> bool {
        *self == rhs
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{from_slice, to_vec};

    #[test]
    fn uint256_new_works() {
        let num = Uint256::new([1; 32]);
        let a: [u8; 32] = num.to_be_bytes();
        assert_eq!(a, [1; 32]);

        let be_bytes = [
            0u8, 222u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
            0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 1u8, 2u8, 3u8,
        ];
        let num = Uint256::new(be_bytes);
        let resulting_bytes: [u8; 32] = num.to_be_bytes();
        assert_eq!(be_bytes, resulting_bytes);
    }

    #[test]
    fn uint256_zero_works() {
        let zero = Uint256::zero();
        assert_eq!(
            zero.to_be_bytes(),
            [
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0
            ]
        );
    }

    #[test]
    fn uin256_one_works() {
        let one = Uint256::one();
        assert_eq!(
            one.to_be_bytes(),
            [
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 1,
            ]
        );
    }

    #[test]
    fn uint256_from_be_bytes() {
        let a = Uint256::from_be_bytes([
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0,
        ]);
        assert_eq!(a, Uint256::from(0u128));
        let a = Uint256::from_be_bytes([
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 42,
        ]);
        assert_eq!(a, Uint256::from(42u128));
        let a = Uint256::from_be_bytes([
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 1,
        ]);
        assert_eq!(a, Uint256::from(1u128));
        let a = Uint256::from_be_bytes([
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 1, 0,
        ]);
        assert_eq!(a, Uint256::from(256u128));
        let a = Uint256::from_be_bytes([
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            1, 0, 0,
        ]);
        assert_eq!(a, Uint256::from(65536u128));
        let a = Uint256::from_be_bytes([
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,
            0, 0, 0,
        ]);
        assert_eq!(a, Uint256::from(16777216u128));
        let a = Uint256::from_be_bytes([
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0,
            0, 0, 0,
        ]);
        assert_eq!(a, Uint256::from(4294967296u128));
        let a = Uint256::from_be_bytes([
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0,
            0, 0, 0,
        ]);
        assert_eq!(a, Uint256::from(1099511627776u128));
        let a = Uint256::from_be_bytes([
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0,
            0, 0, 0,
        ]);
        assert_eq!(a, Uint256::from(281474976710656u128));
        let a = Uint256::from_be_bytes([
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0,
            0, 0, 0,
        ]);
        assert_eq!(a, Uint256::from(72057594037927936u128));
        let a = Uint256::from_be_bytes([
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0,
            0, 0, 0,
        ]);
        assert_eq!(a, Uint256::from(18446744073709551616u128));
        let a = Uint256::from_be_bytes([
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0,
            0, 0, 0,
        ]);
        assert_eq!(a, Uint256::from(4722366482869645213696u128));
        let a = Uint256::from_be_bytes([
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0,
        ]);
        assert_eq!(a, Uint256::from(1208925819614629174706176u128));
        let a = Uint256::from_be_bytes([
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0,
        ]);
        assert_eq!(a, Uint256::from(1329227995784915872903807060280344576u128));

        // Values > u128::MAX
        let a = Uint256::from_be_bytes([
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0,
        ]);
        assert_eq!(a, Uint256::from(1u128) << (8 * 16));
        let a = Uint256::from_be_bytes([
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0,
        ]);
        assert_eq!(a, Uint256::from(1u128) << (8 * 17));
        let a = Uint256::from_be_bytes([
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0,
        ]);
        assert_eq!(a, Uint256::from(1u128) << (8 * 18));
        let a = Uint256::from_be_bytes([
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0,
        ]);
        assert_eq!(a, Uint256::from(1u128) << (8 * 19));
        let a = Uint256::from_be_bytes([
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0,
        ]);
        assert_eq!(a, Uint256::from(1u128) << (8 * 20));
        let a = Uint256::from_be_bytes([
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0,
        ]);
        assert_eq!(a, Uint256::from(1u128) << (8 * 21));
        let a = Uint256::from_be_bytes([
            0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0,
        ]);
        assert_eq!(a, Uint256::from(1u128) << (8 * 22));
        let a = Uint256::from_be_bytes([
            0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0,
        ]);
        assert_eq!(a, Uint256::from(1u128) << (8 * 23));
        let a = Uint256::from_be_bytes([
            0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0,
        ]);
        assert_eq!(a, Uint256::from(1u128) << (8 * 24));
        let a = Uint256::from_be_bytes([
            0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0,
        ]);
        assert_eq!(a, Uint256::from(1u128) << (8 * 25));
        let a = Uint256::from_be_bytes([
            0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0,
        ]);
        assert_eq!(a, Uint256::from(1u128) << (8 * 26));
        let a = Uint256::from_be_bytes([
            0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0,
        ]);
        assert_eq!(a, Uint256::from(1u128) << (8 * 27));
        let a = Uint256::from_be_bytes([
            0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0,
        ]);
        assert_eq!(a, Uint256::from(1u128) << (8 * 28));
        let a = Uint256::from_be_bytes([
            0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0,
        ]);
        assert_eq!(a, Uint256::from(1u128) << (8 * 29));
        let a = Uint256::from_be_bytes([
            0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0,
        ]);
        assert_eq!(a, Uint256::from(1u128) << (8 * 30));
        let a = Uint256::from_be_bytes([
            1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0,
        ]);
        assert_eq!(a, Uint256::from(1u128) << (8 * 31));
    }

    #[test]
    fn uint256_from_le_bytes() {
        let a = Uint256::from_le_bytes([
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0,
        ]);
        assert_eq!(a, Uint256::from(0u128));
        let a = Uint256::from_le_bytes([
            42, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0,
        ]);
        assert_eq!(a, Uint256::from(42u128));
        let a = Uint256::from_le_bytes([
            1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0,
        ]);
        assert_eq!(a, Uint256::from(1u128));
        let a = Uint256::from_le_bytes([
            0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0,
        ]);
        assert_eq!(a, Uint256::from(256u128));
        let a = Uint256::from_le_bytes([
            0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0,
        ]);
        assert_eq!(a, Uint256::from(65536u128));
        let a = Uint256::from_le_bytes([
            0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0,
        ]);
        assert_eq!(a, Uint256::from(16777216u128));
        let a = Uint256::from_le_bytes([
            0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0,
        ]);
        assert_eq!(a, Uint256::from(4294967296u128));
        let a = Uint256::from_le_bytes([
            0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0,
        ]);
        assert_eq!(a, Uint256::from(72057594037927936u128));
        let a = Uint256::from_le_bytes([
            0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0,
        ]);
        assert_eq!(a, Uint256::from(18446744073709551616u128));
        let a = Uint256::from_le_bytes([
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0,
        ]);
        assert_eq!(a, Uint256::from(1329227995784915872903807060280344576u128));

        // Values > u128::MAX
        let a = Uint256::from_le_bytes([
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0,
        ]);
        assert_eq!(a, Uint256::from(1u128) << (8 * 16));
        let a = Uint256::from_le_bytes([
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0,
        ]);
        assert_eq!(a, Uint256::from(1u128) << (8 * 17));
        let a = Uint256::from_le_bytes([
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0,
        ]);
        assert_eq!(a, Uint256::from(1u128) << (8 * 18));
        let a = Uint256::from_le_bytes([
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0,
        ]);
        assert_eq!(a, Uint256::from(1u128) << (8 * 19));
        let a = Uint256::from_le_bytes([
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0,
        ]);
        assert_eq!(a, Uint256::from(1u128) << (8 * 20));
        let a = Uint256::from_le_bytes([
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0,
        ]);
        assert_eq!(a, Uint256::from(1u128) << (8 * 21));
        let a = Uint256::from_le_bytes([
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0,
            0, 0, 0,
        ]);
        assert_eq!(a, Uint256::from(1u128) << (8 * 22));
        let a = Uint256::from_le_bytes([
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0,
            0, 0, 0,
        ]);
        assert_eq!(a, Uint256::from(1u128) << (8 * 23));
        let a = Uint256::from_le_bytes([
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0,
            0, 0, 0,
        ]);
        assert_eq!(a, Uint256::from(1u128) << (8 * 24));
        let a = Uint256::from_le_bytes([
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0,
            0, 0, 0,
        ]);
        assert_eq!(a, Uint256::from(1u128) << (8 * 25));
        let a = Uint256::from_le_bytes([
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0,
            0, 0, 0,
        ]);
        assert_eq!(a, Uint256::from(1u128) << (8 * 26));
        let a = Uint256::from_le_bytes([
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0,
            0, 0, 0,
        ]);
        assert_eq!(a, Uint256::from(1u128) << (8 * 27));
        let a = Uint256::from_le_bytes([
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,
            0, 0, 0,
        ]);
        assert_eq!(a, Uint256::from(1u128) << (8 * 28));
        let a = Uint256::from_le_bytes([
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            1, 0, 0,
        ]);
        assert_eq!(a, Uint256::from(1u128) << (8 * 29));
        let a = Uint256::from_le_bytes([
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 1, 0,
        ]);
        assert_eq!(a, Uint256::from(1u128) << (8 * 30));
        let a = Uint256::from_le_bytes([
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 1,
        ]);
        assert_eq!(a, Uint256::from(1u128) << (8 * 31));
    }

    #[test]
    fn uint256_endianness() {
        let be_bytes = [
            0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
            0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 1u8, 2u8, 3u8,
        ];
        let le_bytes = [
            3u8, 2u8, 1u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
            0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
        ];

        // These should all be the same.
        let num1 = Uint256::new(be_bytes);
        let num2 = Uint256::from_be_bytes(be_bytes);
        let num3 = Uint256::from_le_bytes(le_bytes);
        assert_eq!(num1, Uint256::from(65536u32 + 512 + 3));
        assert_eq!(num1, num2);
        assert_eq!(num1, num3);
    }

    #[test]
    fn uint256_convert_from() {
        let a = Uint256::from(5u128);
        assert_eq!(a.0, U256::from(5));

        let a = Uint256::from(5u64);
        assert_eq!(a.0, U256::from(5));

        let a = Uint256::from(5u32);
        assert_eq!(a.0, U256::from(5));

        let a = Uint256::from(5u16);
        assert_eq!(a.0, U256::from(5));

        let a = Uint256::from(5u8);
        assert_eq!(a.0, U256::from(5));

        let result = Uint256::try_from("34567");
        assert_eq!(result.unwrap().0, U256::from_dec_str("34567").unwrap());

        let result = Uint256::try_from("1.23");
        assert!(result.is_err());
    }

    #[test]
    fn uint256_convert_to_uint128() {
        let source = Uint256::from(42u128);
        let target = Uint128::try_from(source);
        assert_eq!(target, Ok(Uint128::new(42u128)));

        let source = Uint256::MAX;
        let target = Uint128::try_from(source);
        assert_eq!(
            target,
            Err(ConversionOverflowError::new(
                "Uint256",
                "Uint128",
                Uint256::MAX.to_string()
            ))
        );
    }

    #[test]
    fn uint256_from_u128() {
        assert_eq!(
            Uint256::from_u128(123u128),
            Uint256::from_str("123").unwrap()
        );

        assert_eq!(
            Uint256::from_u128(9785746283745u128),
            Uint256::from_str("9785746283745").unwrap()
        );
    }

    #[test]
    fn uint256_from_uint128() {
        assert_eq!(
            Uint256::from_uint128(Uint128::new(123)),
            Uint256::from_str("123").unwrap()
        );

        assert_eq!(
            Uint256::from_uint128(Uint128::new(9785746283745)),
            Uint256::from_str("9785746283745").unwrap()
        );
    }

    #[test]
    fn uint256_implements_display() {
        let a = Uint256::from(12345u32);
        assert_eq!(format!("Embedded: {}", a), "Embedded: 12345");
        assert_eq!(a.to_string(), "12345");

        let a = Uint256::zero();
        assert_eq!(format!("Embedded: {}", a), "Embedded: 0");
        assert_eq!(a.to_string(), "0");
    }

    #[test]
    fn uint256_display_padding_works() {
        let a = Uint256::from(123u64);
        assert_eq!(format!("Embedded: {:05}", a), "Embedded: 00123");
    }

    #[test]
    fn uint256_to_be_bytes_works() {
        assert_eq!(
            Uint256::zero().to_be_bytes(),
            [
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0,
            ]
        );
        assert_eq!(
            Uint256::MAX.to_be_bytes(),
            [
                0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
                0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
                0xff, 0xff, 0xff, 0xff,
            ]
        );
        assert_eq!(
            Uint256::from(1u128).to_be_bytes(),
            [
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 1
            ]
        );
        // Python: `[b for b in (240282366920938463463374607431768124608).to_bytes(32, "big")]`
        assert_eq!(
            Uint256::from(240282366920938463463374607431768124608u128).to_be_bytes(),
            [
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 180, 196, 179, 87, 165, 121, 59,
                133, 246, 117, 221, 191, 255, 254, 172, 192
            ]
        );
        assert_eq!(
            Uint256::from_be_bytes([
                233, 2, 240, 200, 115, 150, 240, 218, 88, 106, 45, 208, 134, 238, 119, 85, 22, 14,
                88, 166, 195, 154, 73, 64, 10, 44, 252, 96, 230, 187, 38, 29
            ])
            .to_be_bytes(),
            [
                233, 2, 240, 200, 115, 150, 240, 218, 88, 106, 45, 208, 134, 238, 119, 85, 22, 14,
                88, 166, 195, 154, 73, 64, 10, 44, 252, 96, 230, 187, 38, 29
            ]
        );
    }

    #[test]
    fn uint256_to_le_bytes_works() {
        assert_eq!(
            Uint256::zero().to_le_bytes(),
            [
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0
            ]
        );
        assert_eq!(
            Uint256::MAX.to_le_bytes(),
            [
                0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
                0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
                0xff, 0xff, 0xff, 0xff
            ]
        );
        assert_eq!(
            Uint256::from(1u128).to_le_bytes(),
            [
                1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0
            ]
        );
        // Python: `[b for b in (240282366920938463463374607431768124608).to_bytes(32, "little")]`
        assert_eq!(
            Uint256::from(240282366920938463463374607431768124608u128).to_le_bytes(),
            [
                192, 172, 254, 255, 191, 221, 117, 246, 133, 59, 121, 165, 87, 179, 196, 180, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
            ]
        );
        assert_eq!(
            Uint256::from_be_bytes([
                233, 2, 240, 200, 115, 150, 240, 218, 88, 106, 45, 208, 134, 238, 119, 85, 22, 14,
                88, 166, 195, 154, 73, 64, 10, 44, 252, 96, 230, 187, 38, 29
            ])
            .to_le_bytes(),
            [
                29, 38, 187, 230, 96, 252, 44, 10, 64, 73, 154, 195, 166, 88, 14, 22, 85, 119, 238,
                134, 208, 45, 106, 88, 218, 240, 150, 115, 200, 240, 2, 233
            ]
        );
    }

    #[test]
    fn uint256_is_zero_works() {
        assert!(Uint256::zero().is_zero());
        assert!(Uint256(U256::from(0)).is_zero());

        assert!(!Uint256::from(1u32).is_zero());
        assert!(!Uint256::from(123u32).is_zero());
    }

    #[test]
    fn uint256_wrapping_methods() {
        // wrapping_add
        assert_eq!(
            Uint256::from(2u32).wrapping_add(Uint256::from(2u32)),
            Uint256::from(4u32)
        ); // non-wrapping
        assert_eq!(
            Uint256::MAX.wrapping_add(Uint256::from(1u32)),
            Uint256::from(0u32)
        ); // wrapping

        // wrapping_sub
        assert_eq!(
            Uint256::from(7u32).wrapping_sub(Uint256::from(5u32)),
            Uint256::from(2u32)
        ); // non-wrapping
        assert_eq!(
            Uint256::from(0u32).wrapping_sub(Uint256::from(1u32)),
            Uint256::MAX
        ); // wrapping

        // wrapping_mul
        assert_eq!(
            Uint256::from(3u32).wrapping_mul(Uint256::from(2u32)),
            Uint256::from(6u32)
        ); // non-wrapping
        assert_eq!(
            Uint256::MAX.wrapping_mul(Uint256::from(2u32)),
            Uint256::MAX - Uint256::one()
        ); // wrapping

        // wrapping_pow
        assert_eq!(Uint256::from(2u32).wrapping_pow(3), Uint256::from(8u32)); // non-wrapping
        assert_eq!(Uint256::MAX.wrapping_pow(2), Uint256::from(1u32)); // wrapping
    }

    #[test]
    fn uint256_json() {
        let orig = Uint256::from(1234567890987654321u128);
        let serialized = to_vec(&orig).unwrap();
        assert_eq!(serialized.as_slice(), b"\"1234567890987654321\"");
        let parsed: Uint256 = from_slice(&serialized).unwrap();
        assert_eq!(parsed, orig);
    }

    #[test]
    fn uint256_compare() {
        let a = Uint256::from(12345u32);
        let b = Uint256::from(23456u32);

        assert!(a < b);
        assert!(b > a);
        assert_eq!(a, Uint256::from(12345u32));
    }

    #[test]
    #[allow(clippy::op_ref)]
    fn uint256_math() {
        let a = Uint256::from(12345u32);
        let b = Uint256::from(23456u32);

        // test + with owned and reference right hand side
        assert_eq!(a + b, Uint256::from(35801u32));
        assert_eq!(a + &b, Uint256::from(35801u32));

        // test - with owned and reference right hand side
        assert_eq!(b - a, Uint256::from(11111u32));
        assert_eq!(b - &a, Uint256::from(11111u32));

        // test += with owned and reference right hand side
        let mut c = Uint256::from(300000u32);
        c += b;
        assert_eq!(c, Uint256::from(323456u32));
        let mut d = Uint256::from(300000u32);
        d += &b;
        assert_eq!(d, Uint256::from(323456u32));

        // test -= with owned and reference right hand side
        let mut c = Uint256::from(300000u32);
        c -= b;
        assert_eq!(c, Uint256::from(276544u32));
        let mut d = Uint256::from(300000u32);
        d -= &b;
        assert_eq!(d, Uint256::from(276544u32));

        // error result on underflow (- would produce negative result)
        let underflow_result = a.checked_sub(b);
        let OverflowError {
            operand1, operand2, ..
        } = underflow_result.unwrap_err();
        assert_eq!((operand1, operand2), (a.to_string(), b.to_string()));
    }

    #[test]
    #[should_panic]
    fn uint256_add_overflow_panics() {
        let max = Uint256::new([255u8; 32]);
        let _ = max + Uint256::from(12u32);
    }

    #[test]
    #[allow(clippy::op_ref)]
    fn uint256_sub_works() {
        assert_eq!(
            Uint256::from(2u32) - Uint256::from(1u32),
            Uint256::from(1u32)
        );
        assert_eq!(
            Uint256::from(2u32) - Uint256::from(0u32),
            Uint256::from(2u32)
        );
        assert_eq!(
            Uint256::from(2u32) - Uint256::from(2u32),
            Uint256::from(0u32)
        );

        // works for refs
        let a = Uint256::from(10u32);
        let b = Uint256::from(3u32);
        let expected = Uint256::from(7u32);
        assert_eq!(a - b, expected);
        assert_eq!(a - &b, expected);
        assert_eq!(&a - b, expected);
        assert_eq!(&a - &b, expected);
    }

    #[test]
    #[should_panic]
    fn uint256_sub_overflow_panics() {
        let _ = Uint256::from(1u32) - Uint256::from(2u32);
    }

    #[test]
    fn uint256_sub_assign_works() {
        let mut a = Uint256::from(14u32);
        a -= Uint256::from(2u32);
        assert_eq!(a, Uint256::from(12u32));

        // works for refs
        let mut a = Uint256::from(10u32);
        let b = Uint256::from(3u32);
        let expected = Uint256::from(7u32);
        a -= &b;
        assert_eq!(a, expected);
    }

    #[test]
    #[allow(clippy::op_ref)]
    fn uint256_mul_works() {
        assert_eq!(
            Uint256::from(2u32) * Uint256::from(3u32),
            Uint256::from(6u32)
        );
        assert_eq!(Uint256::from(2u32) * Uint256::zero(), Uint256::zero());

        // works for refs
        let a = Uint256::from(11u32);
        let b = Uint256::from(3u32);
        let expected = Uint256::from(33u32);
        assert_eq!(a * b, expected);
        assert_eq!(a * &b, expected);
        assert_eq!(&a * b, expected);
        assert_eq!(&a * &b, expected);
    }

    #[test]
    fn uint256_mul_assign_works() {
        let mut a = Uint256::from(14u32);
        a *= Uint256::from(2u32);
        assert_eq!(a, Uint256::from(28u32));

        // works for refs
        let mut a = Uint256::from(10u32);
        let b = Uint256::from(3u32);
        a *= &b;
        assert_eq!(a, Uint256::from(30u32));
    }

    #[test]
    fn uint256_pow_works() {
        assert_eq!(Uint256::from(2u32).pow(2), Uint256::from(4u32));
        assert_eq!(Uint256::from(2u32).pow(10), Uint256::from(1024u32));
    }

    #[test]
    #[should_panic]
    fn uint256_pow_overflow_panics() {
        Uint256::MAX.pow(2u32);
    }

    #[test]
    fn uint256_multiply_ratio_works() {
        let base = Uint256::from(500u32);

        // factor 1/1
        assert_eq!(base.multiply_ratio(1u128, 1u128), base);
        assert_eq!(base.multiply_ratio(3u128, 3u128), base);
        assert_eq!(base.multiply_ratio(654321u128, 654321u128), base);
        assert_eq!(base.multiply_ratio(Uint256::MAX, Uint256::MAX), base);

        // factor 3/2
        assert_eq!(base.multiply_ratio(3u128, 2u128), Uint256::from(750u32));
        assert_eq!(
            base.multiply_ratio(333333u128, 222222u128),
            Uint256::from(750u32)
        );

        // factor 2/3 (integer devision always floors the result)
        assert_eq!(base.multiply_ratio(2u128, 3u128), Uint256::from(333u32));
        assert_eq!(
            base.multiply_ratio(222222u128, 333333u128),
            Uint256::from(333u32)
        );

        // factor 5/6 (integer devision always floors the result)
        assert_eq!(base.multiply_ratio(5u128, 6u128), Uint256::from(416u32));
        assert_eq!(base.multiply_ratio(100u128, 120u128), Uint256::from(416u32));
    }

    #[test]
    fn uint256_multiply_ratio_does_not_overflow_when_result_fits() {
        // Almost max value for Uint256.
        let base = Uint256::MAX - Uint256::from(9u8);

        assert_eq!(base.multiply_ratio(2u128, 2u128), base);
    }

    #[test]
    #[should_panic]
    fn uint256_multiply_ratio_panicks_on_overflow() {
        // Almost max value for Uint256.
        let base = Uint256::MAX - Uint256::from(9u8);

        assert_eq!(base.multiply_ratio(2u128, 1u128), base);
    }

    #[test]
    #[should_panic(expected = "Denominator must not be zero")]
    fn uint256_multiply_ratio_panics_for_zero_denominator() {
        Uint256::from(500u32).multiply_ratio(1u128, 0u128);
    }

    #[test]
    fn uint256_checked_multiply_ratio_does_not_panic() {
        assert_eq!(
            Uint256::from(500u32).checked_multiply_ratio(1u128, 0u128),
            Err(CheckedMultiplyRatioError::DivideByZero),
        );
        assert_eq!(
            Uint256::from(500u32).checked_multiply_ratio(Uint256::MAX, 1u128),
            Err(CheckedMultiplyRatioError::Overflow),
        );
    }

    #[test]
    fn uint256_shr_works() {
        let original = Uint256::new([
            0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
            0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 2u8, 0u8, 4u8, 2u8,
        ]);

        let shifted = Uint256::new([
            0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
            0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 128u8, 1u8, 0u8,
        ]);

        assert_eq!(original >> 2u32, shifted);
    }

    #[test]
    #[should_panic]
    fn uint256_shr_overflow_panics() {
        let _ = Uint256::from(1u32) >> 256u32;
    }

    #[test]
    fn sum_works() {
        let nums = vec![
            Uint256::from(17u32),
            Uint256::from(123u32),
            Uint256::from(540u32),
            Uint256::from(82u32),
        ];
        let expected = Uint256::from(762u32);

        let sum_as_ref: Uint256 = nums.iter().sum();
        assert_eq!(expected, sum_as_ref);

        let sum_as_owned: Uint256 = nums.into_iter().sum();
        assert_eq!(expected, sum_as_owned);
    }

    #[test]
    fn uint256_methods() {
        // checked_*
        assert!(matches!(
            Uint256::MAX.checked_add(Uint256::from(1u32)),
            Err(OverflowError { .. })
        ));
        assert_eq!(
            Uint256::from(1u32).checked_add(Uint256::from(1u32)),
            Ok(Uint256::from(2u32)),
        );
        assert!(matches!(
            Uint256::from(0u32).checked_sub(Uint256::from(1u32)),
            Err(OverflowError { .. })
        ));
        assert_eq!(
            Uint256::from(2u32).checked_sub(Uint256::from(1u32)),
            Ok(Uint256::from(1u32)),
        );
        assert!(matches!(
            Uint256::MAX.checked_mul(Uint256::from(2u32)),
            Err(OverflowError { .. })
        ));
        assert_eq!(
            Uint256::from(2u32).checked_mul(Uint256::from(2u32)),
            Ok(Uint256::from(4u32)),
        );
        assert!(matches!(
            Uint256::MAX.checked_pow(2u32),
            Err(OverflowError { .. })
        ));
        assert_eq!(
            Uint256::from(2u32).checked_pow(3u32),
            Ok(Uint256::from(8u32)),
        );
        assert!(matches!(
            Uint256::MAX.checked_div(Uint256::from(0u32)),
            Err(DivideByZeroError { .. })
        ));
        assert_eq!(
            Uint256::from(6u32).checked_div(Uint256::from(2u32)),
            Ok(Uint256::from(3u32)),
        );
        assert!(matches!(
            Uint256::MAX.checked_div_euclid(Uint256::from(0u32)),
            Err(DivideByZeroError { .. })
        ));
        assert_eq!(
            Uint256::from(6u32).checked_div_euclid(Uint256::from(2u32)),
            Ok(Uint256::from(3u32)),
        );
        assert_eq!(
            Uint256::from(7u32).checked_div_euclid(Uint256::from(2u32)),
            Ok(Uint256::from(3u32)),
        );
        assert!(matches!(
            Uint256::MAX.checked_rem(Uint256::from(0u32)),
            Err(DivideByZeroError { .. })
        ));

        // saturating_*
        assert_eq!(
            Uint256::MAX.saturating_add(Uint256::from(1u32)),
            Uint256::MAX
        );
        assert_eq!(
            Uint256::from(0u32).saturating_sub(Uint256::from(1u32)),
            Uint256::from(0u32)
        );
        assert_eq!(
            Uint256::MAX.saturating_mul(Uint256::from(2u32)),
            Uint256::MAX
        );
        assert_eq!(
            Uint256::from(4u32).saturating_pow(2u32),
            Uint256::from(16u32)
        );
        assert_eq!(Uint256::MAX.saturating_pow(2u32), Uint256::MAX);
    }

    #[test]
    #[allow(clippy::op_ref)]
    fn uint256_implements_rem() {
        let a = Uint256::from(10u32);
        assert_eq!(a % Uint256::from(10u32), Uint256::zero());
        assert_eq!(a % Uint256::from(2u32), Uint256::zero());
        assert_eq!(a % Uint256::from(1u32), Uint256::zero());
        assert_eq!(a % Uint256::from(3u32), Uint256::from(1u32));
        assert_eq!(a % Uint256::from(4u32), Uint256::from(2u32));

        // works for refs
        let a = Uint256::from(10u32);
        let b = Uint256::from(3u32);
        let expected = Uint256::from(1u32);
        assert_eq!(a % b, expected);
        assert_eq!(a % &b, expected);
        assert_eq!(&a % b, expected);
        assert_eq!(&a % &b, expected);
    }

    #[test]
    #[should_panic(expected = "division by zero")]
    fn uint256_rem_panics_for_zero() {
        let _ = Uint256::from(10u32) % Uint256::zero();
    }

    #[test]
    #[allow(clippy::op_ref)]
    fn uint256_rem_works() {
        assert_eq!(
            Uint256::from(12u32) % Uint256::from(10u32),
            Uint256::from(2u32)
        );
        assert_eq!(Uint256::from(50u32) % Uint256::from(5u32), Uint256::zero());

        // works for refs
        let a = Uint256::from(42u32);
        let b = Uint256::from(5u32);
        let expected = Uint256::from(2u32);
        assert_eq!(a % b, expected);
        assert_eq!(a % &b, expected);
        assert_eq!(&a % b, expected);
        assert_eq!(&a % &b, expected);
    }

    #[test]
    fn uint256_rem_assign_works() {
        let mut a = Uint256::from(30u32);
        a %= Uint256::from(4u32);
        assert_eq!(a, Uint256::from(2u32));

        // works for refs
        let mut a = Uint256::from(25u32);
        let b = Uint256::from(6u32);
        a %= &b;
        assert_eq!(a, Uint256::from(1u32));
    }

    #[test]
    fn uint256_abs_diff_works() {
        let a = Uint256::from(42u32);
        let b = Uint256::from(5u32);
        let expected = Uint256::from(37u32);
        assert_eq!(a.abs_diff(b), expected);
        assert_eq!(b.abs_diff(a), expected);
    }

    #[test]
    fn uint256_partial_eq() {
        let test_cases = [(1, 1, true), (42, 42, true), (42, 24, false), (0, 0, true)]
            .into_iter()
            .map(|(lhs, rhs, expected): (u64, u64, bool)| {
                (Uint256::from(lhs), Uint256::from(rhs), expected)
            });

        #[allow(clippy::op_ref)]
        for (lhs, rhs, expected) in test_cases {
            assert_eq!(lhs == rhs, expected);
            assert_eq!(&lhs == rhs, expected);
            assert_eq!(lhs == &rhs, expected);
            assert_eq!(&lhs == &rhs, expected);
        }
    }
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
    pub const fn new(value: [u8; 64]) -> Self {
        Self::from_be_bytes(value)
    }

    /// Creates a Uint512(0)
    #[inline]
    pub const fn zero() -> Self {
        Uint512(U512::zero())
    }

    /// Creates a Uint512(1)
    #[inline]
    pub const fn one() -> Self {
        Self::from_be_bytes([
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 1,
        ])
    }

    pub const fn from_be_bytes(data: [u8; 64]) -> Self {
        let words: [u64; 8] = [
            u64::from_le_bytes([
                data[63], data[62], data[61], data[60], data[59], data[58], data[57], data[56],
            ]),
            u64::from_le_bytes([
                data[55], data[54], data[53], data[52], data[51], data[50], data[49], data[48],
            ]),
            u64::from_le_bytes([
                data[47], data[46], data[45], data[44], data[43], data[42], data[41], data[40],
            ]),
            u64::from_le_bytes([
                data[39], data[38], data[37], data[36], data[35], data[34], data[33], data[32],
            ]),
            u64::from_le_bytes([
                data[31], data[30], data[29], data[28], data[27], data[26], data[25], data[24],
            ]),
            u64::from_le_bytes([
                data[23], data[22], data[21], data[20], data[19], data[18], data[17], data[16],
            ]),
            u64::from_le_bytes([
                data[15], data[14], data[13], data[12], data[11], data[10], data[9], data[8],
            ]),
            u64::from_le_bytes([
                data[7], data[6], data[5], data[4], data[3], data[2], data[1], data[0],
            ]),
        ];
        Self(U512(words))
    }

    pub const fn from_le_bytes(data: [u8; 64]) -> Self {
        let words: [u64; 8] = [
            u64::from_le_bytes([
                data[0], data[1], data[2], data[3], data[4], data[5], data[6], data[7],
            ]),
            u64::from_le_bytes([
                data[8], data[9], data[10], data[11], data[12], data[13], data[14], data[15],
            ]),
            u64::from_le_bytes([
                data[16], data[17], data[18], data[19], data[20], data[21], data[22], data[23],
            ]),
            u64::from_le_bytes([
                data[24], data[25], data[26], data[27], data[28], data[29], data[30], data[31],
            ]),
            u64::from_le_bytes([
                data[32], data[33], data[34], data[35], data[36], data[37], data[38], data[39],
            ]),
            u64::from_le_bytes([
                data[40], data[41], data[42], data[43], data[44], data[45], data[46], data[47],
            ]),
            u64::from_le_bytes([
                data[48], data[49], data[50], data[51], data[52], data[53], data[54], data[55],
            ]),
            u64::from_le_bytes([
                data[56], data[57], data[58], data[59], data[60], data[61], data[62], data[63],
            ]),
        ];
        Self(U512(words))
    }

    /// A conversion from `Uint256` that, unlike the one provided by the `From` trait,
    /// can be used in a `const` context.
    pub const fn from_uint256(num: Uint256) -> Self {
        let bytes = num.to_le_bytes();
        Self::from_le_bytes([
            bytes[0], bytes[1], bytes[2], bytes[3], bytes[4], bytes[5], bytes[6], bytes[7],
            bytes[8], bytes[9], bytes[10], bytes[11], bytes[12], bytes[13], bytes[14], bytes[15],
            bytes[16], bytes[17], bytes[18], bytes[19], bytes[20], bytes[21], bytes[22], bytes[23],
            bytes[24], bytes[25], bytes[26], bytes[27], bytes[28], bytes[29], bytes[30], bytes[31],
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0,
        ])
    }

    /// Returns a copy of the number as big endian bytes.
    pub const fn to_be_bytes(self) -> [u8; 64] {
        let words = [
            (self.0).0[7].to_be_bytes(),
            (self.0).0[6].to_be_bytes(),
            (self.0).0[5].to_be_bytes(),
            (self.0).0[4].to_be_bytes(),
            (self.0).0[3].to_be_bytes(),
            (self.0).0[2].to_be_bytes(),
            (self.0).0[1].to_be_bytes(),
            (self.0).0[0].to_be_bytes(),
        ];
        unsafe { std::mem::transmute::<[[u8; 8]; 8], [u8; 64]>(words) }
    }

    /// Returns a copy of the number as little endian bytes.
    pub const fn to_le_bytes(self) -> [u8; 64] {
        let words = [
            (self.0).0[0].to_le_bytes(),
            (self.0).0[1].to_le_bytes(),
            (self.0).0[2].to_le_bytes(),
            (self.0).0[3].to_le_bytes(),
            (self.0).0[4].to_le_bytes(),
            (self.0).0[5].to_le_bytes(),
            (self.0).0[6].to_le_bytes(),
            (self.0).0[7].to_le_bytes(),
        ];
        unsafe { std::mem::transmute::<[[u8; 8]; 8], [u8; 64]>(words) }
    }

    pub const fn is_zero(&self) -> bool {
        let words = (self.0).0;
        words[0] == 0
            && words[1] == 0
            && words[2] == 0
            && words[3] == 0
            && words[4] == 0
            && words[5] == 0
            && words[6] == 0
            && words[7] == 0
    }

    pub fn pow(self, exp: u32) -> Self {
        let res = self.0.pow(exp.into());
        Self(res)
    }

    pub fn checked_add(self, other: Self) -> Result<Self, OverflowError> {
        self.0
            .checked_add(other.0)
            .map(Self)
            .ok_or_else(|| OverflowError::new(OverflowOperation::Add, self, other))
    }

    pub fn checked_sub(self, other: Self) -> Result<Self, OverflowError> {
        self.0
            .checked_sub(other.0)
            .map(Self)
            .ok_or_else(|| OverflowError::new(OverflowOperation::Sub, self, other))
    }

    pub fn checked_mul(self, other: Self) -> Result<Self, OverflowError> {
        self.0
            .checked_mul(other.0)
            .map(Self)
            .ok_or_else(|| OverflowError::new(OverflowOperation::Mul, self, other))
    }

    pub fn checked_pow(self, exp: u32) -> Result<Self, OverflowError> {
        self.0
            .checked_pow(exp.into())
            .map(Self)
            .ok_or_else(|| OverflowError::new(OverflowOperation::Pow, self, exp))
    }

    pub fn checked_div(self, other: Self) -> Result<Self, DivideByZeroError> {
        self.0
            .checked_div(other.0)
            .map(Self)
            .ok_or_else(|| DivideByZeroError::new(self))
    }

    pub fn checked_div_euclid(self, other: Self) -> Result<Self, DivideByZeroError> {
        self.checked_div(other)
    }

    pub fn checked_rem(self, other: Self) -> Result<Self, DivideByZeroError> {
        self.0
            .checked_rem(other.0)
            .map(Self)
            .ok_or_else(|| DivideByZeroError::new(self))
    }

    pub fn checked_shr(self, other: u32) -> Result<Self, OverflowError> {
        if other >= 512 {
            return Err(OverflowError::new(OverflowOperation::Shr, self, other));
        }

        Ok(Self(self.0.shr(other)))
    }

    #[inline]
    pub fn wrapping_add(self, other: Self) -> Self {
        let (value, _did_overflow) = self.0.overflowing_add(other.0);
        Self(value)
    }

    #[inline]
    pub fn wrapping_sub(self, other: Self) -> Self {
        let (value, _did_overflow) = self.0.overflowing_sub(other.0);
        Self(value)
    }

    #[inline]
    pub fn wrapping_mul(self, other: Self) -> Self {
        let (value, _did_overflow) = self.0.overflowing_mul(other.0);
        Self(value)
    }

    #[inline]
    pub fn wrapping_pow(self, other: u32) -> Self {
        let (value, _did_overflow) = self.0.overflowing_pow(other.into());
        Self(value)
    }

    pub fn saturating_add(self, other: Self) -> Self {
        Self(self.0.saturating_add(other.0))
    }

    pub fn saturating_sub(self, other: Self) -> Self {
        Self(self.0.saturating_sub(other.0))
    }

    pub fn saturating_mul(self, other: Self) -> Self {
        Self(self.0.saturating_mul(other.0))
    }

    pub fn saturating_pow(self, exp: u32) -> Self {
        match self.checked_pow(exp) {
            Ok(value) => value,
            Err(_) => Self::MAX,
        }
    }

    pub fn abs_diff(self, other: Self) -> Self {
        if self < other {
            other - self
        } else {
            self - other
        }
    }
}

impl From<Uint256> for Uint512 {
    fn from(val: Uint256) -> Self {
        let bytes = [[0u8; 32], val.to_be_bytes()].concat();

        Self::from_be_bytes(bytes.try_into().unwrap())
    }
}

impl From<Uint128> for Uint512 {
    fn from(val: Uint128) -> Self {
        val.u128().into()
    }
}

impl From<Uint64> for Uint512 {
    fn from(val: Uint64) -> Self {
        val.u64().into()
    }
}

impl From<u128> for Uint512 {
    fn from(val: u128) -> Self {
        Uint512(val.into())
    }
}

impl From<u64> for Uint512 {
    fn from(val: u64) -> Self {
        Uint512(val.into())
    }
}

impl From<u32> for Uint512 {
    fn from(val: u32) -> Self {
        Uint512(val.into())
    }
}

impl From<u16> for Uint512 {
    fn from(val: u16) -> Self {
        Uint512(val.into())
    }
}

impl From<u8> for Uint512 {
    fn from(val: u8) -> Self {
        Uint512(val.into())
    }
}

impl TryFrom<Uint512> for Uint256 {
    type Error = ConversionOverflowError;

    fn try_from(value: Uint512) -> Result<Self, Self::Error> {
        let bytes = value.to_be_bytes();
        let (first_bytes, last_bytes) = bytes.split_at(32);

        if first_bytes != [0u8; 32] {
            return Err(ConversionOverflowError::new(
                "Uint512",
                "Uint256",
                value.to_string(),
            ));
        }

        Ok(Self::from_be_bytes(last_bytes.try_into().unwrap()))
    }
}

impl TryFrom<Uint512> for Uint128 {
    type Error = ConversionOverflowError;

    fn try_from(value: Uint512) -> Result<Self, Self::Error> {
        Ok(Uint128::new(value.0.try_into().map_err(|_| {
            ConversionOverflowError::new("Uint512", "Uint128", value.to_string())
        })?))
    }
}

impl TryFrom<&str> for Uint512 {
    type Error = StdError;

    fn try_from(val: &str) -> Result<Self, Self::Error> {
        Self::from_str(val)
    }
}

impl FromStr for Uint512 {
    type Err = StdError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match U512::from_dec_str(s) {
            Ok(u) => Ok(Self(u)),
            Err(e) => Err(StdError::generic_err(format!("Parsing u512: {}", e))),
        }
    }
}

impl From<Uint512> for String {
    fn from(original: Uint512) -> Self {
        original.to_string()
    }
}

impl fmt::Display for Uint512 {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // The inner type doesn't work as expected with padding, so we
        // work around that.
        let unpadded = self.0.to_string();

        f.pad_integral(true, "", &unpadded)
    }
}

impl Add<Uint512> for Uint512 {
    type Output = Self;

    fn add(self, rhs: Self) -> Self {
        Uint512(self.0.checked_add(rhs.0).unwrap())
    }
}

impl<'a> Add<&'a Uint512> for Uint512 {
    type Output = Self;

    fn add(self, rhs: &'a Uint512) -> Self {
        Uint512(self.0.checked_add(rhs.0).unwrap())
    }
}

impl Sub<Uint512> for Uint512 {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self {
        Uint512(self.0.checked_sub(rhs.0).unwrap())
    }
}
forward_ref_binop!(impl Sub, sub for Uint512, Uint512);

impl SubAssign<Uint512> for Uint512 {
    fn sub_assign(&mut self, rhs: Uint512) {
        self.0 = self.0.checked_sub(rhs.0).unwrap();
    }
}
forward_ref_op_assign!(impl SubAssign, sub_assign for Uint512, Uint512);

impl Div<Uint512> for Uint512 {
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        Self(self.0.checked_div(rhs.0).unwrap())
    }
}

impl<'a> Div<&'a Uint512> for Uint512 {
    type Output = Self;

    fn div(self, rhs: &'a Uint512) -> Self::Output {
        Self(self.0.checked_div(rhs.0).unwrap())
    }
}

impl Rem for Uint512 {
    type Output = Self;

    /// # Panics
    ///
    /// This operation will panic if `rhs` is zero.
    #[inline]
    fn rem(self, rhs: Self) -> Self {
        Self(self.0.rem(rhs.0))
    }
}
forward_ref_binop!(impl Rem, rem for Uint512, Uint512);

impl RemAssign<Uint512> for Uint512 {
    fn rem_assign(&mut self, rhs: Uint512) {
        *self = *self % rhs;
    }
}
forward_ref_op_assign!(impl RemAssign, rem_assign for Uint512, Uint512);

impl Mul<Uint512> for Uint512 {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        Self(self.0.checked_mul(rhs.0).unwrap())
    }
}
forward_ref_binop!(impl Mul, mul for Uint512, Uint512);

impl MulAssign<Uint512> for Uint512 {
    fn mul_assign(&mut self, rhs: Self) {
        self.0 = self.0.checked_mul(rhs.0).unwrap();
    }
}
forward_ref_op_assign!(impl MulAssign, mul_assign for Uint512, Uint512);

impl Shr<u32> for Uint512 {
    type Output = Self;

    fn shr(self, rhs: u32) -> Self::Output {
        self.checked_shr(rhs).unwrap_or_else(|_| {
            panic!(
                "right shift error: {} is larger or equal than the number of bits in Uint512",
                rhs,
            )
        })
    }
}

impl<'a> Shr<&'a u32> for Uint512 {
    type Output = Self;

    fn shr(self, rhs: &'a u32) -> Self::Output {
        Shr::<u32>::shr(self, *rhs)
    }
}

impl AddAssign<Uint512> for Uint512 {
    fn add_assign(&mut self, rhs: Uint512) {
        self.0 = self.0.checked_add(rhs.0).unwrap();
    }
}

impl<'a> AddAssign<&'a Uint512> for Uint512 {
    fn add_assign(&mut self, rhs: &'a Uint512) {
        self.0 = self.0.checked_add(rhs.0).unwrap();
    }
}

impl DivAssign<Uint512> for Uint512 {
    fn div_assign(&mut self, rhs: Self) {
        self.0 = self.0.checked_div(rhs.0).unwrap();
    }
}

impl<'a> DivAssign<&'a Uint512> for Uint512 {
    fn div_assign(&mut self, rhs: &'a Uint512) {
        self.0 = self.0.checked_div(rhs.0).unwrap();
    }
}

impl ShrAssign<u32> for Uint512 {
    fn shr_assign(&mut self, rhs: u32) {
        *self = Shr::<u32>::shr(*self, rhs);
    }
}

impl<'a> ShrAssign<&'a u32> for Uint512 {
    fn shr_assign(&mut self, rhs: &'a u32) {
        *self = Shr::<u32>::shr(*self, *rhs);
    }
}

impl Serialize for Uint512 {
    /// Serializes as an integer string using base 10
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: ser::Serializer,
    {
        serializer.serialize_str(&self.to_string())
    }
}

impl<'de> Deserialize<'de> for Uint512 {
    /// Deserialized from an integer string using base 10
    fn deserialize<D>(deserializer: D) -> Result<Uint512, D::Error>
    where
        D: Deserializer<'de>,
    {
        deserializer.deserialize_str(Uint512Visitor)
    }
}

struct Uint512Visitor;

impl<'de> de::Visitor<'de> for Uint512Visitor {
    type Value = Uint512;

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str("string-encoded integer")
    }

    fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
    where
        E: de::Error,
    {
        Uint512::try_from(v).map_err(|e| E::custom(format!("invalid Uint512 '{}' - {}", v, e)))
    }
}

impl<A> std::iter::Sum<A> for Uint512
where
    Self: Add<A, Output = Self>,
{
    fn sum<I: Iterator<Item = A>>(iter: I) -> Self {
        iter.fold(Self::zero(), Add::add)
    }
}

impl PartialEq<&Uint512> for Uint512 {
    fn eq(&self, rhs: &&Uint512) -> bool {
        self == *rhs
    }
}

impl PartialEq<Uint512> for &Uint512 {
    fn eq(&self, rhs: &Uint512) -> bool {
        *self == rhs
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{from_slice, to_vec};

    #[test]
    fn uint512_new_works() {
        let num = Uint512::new([1; 64]);
        let a: [u8; 64] = num.to_be_bytes();
        assert_eq!(a, [1; 64]);

        let be_bytes = [
            0u8, 222u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
            0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
            0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
            0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 1u8, 2u8, 3u8,
        ];
        let num = Uint512::new(be_bytes);
        let resulting_bytes: [u8; 64] = num.to_be_bytes();
        assert_eq!(be_bytes, resulting_bytes);
    }

    #[test]
    fn uint512_zero_works() {
        let zero = Uint512::zero();
        assert_eq!(
            zero.to_be_bytes(),
            [
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0
            ]
        );
    }

    #[test]
    fn uin512_one_works() {
        let one = Uint512::one();
        assert_eq!(
            one.to_be_bytes(),
            [
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 1
            ]
        );
    }

    #[test]
    fn uint512_endianness() {
        let be_bytes = [
            0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
            0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
            0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
            0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 1u8, 2u8, 3u8,
        ];
        let le_bytes = [
            3u8, 2u8, 1u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
            0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
            0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
            0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
        ];

        // These should all be the same.
        let num1 = Uint512::new(be_bytes);
        let num2 = Uint512::from_be_bytes(be_bytes);
        let num3 = Uint512::from_le_bytes(le_bytes);
        assert_eq!(num1, Uint512::from(65536u32 + 512 + 3));
        assert_eq!(num1, num2);
        assert_eq!(num1, num3);
    }

    #[test]
    fn uint512_convert_from() {
        let a = Uint512::from(5u128);
        assert_eq!(a.0, U512::from(5));

        let a = Uint512::from(5u64);
        assert_eq!(a.0, U512::from(5));

        let a = Uint512::from(5u32);
        assert_eq!(a.0, U512::from(5));

        let a = Uint512::from(5u16);
        assert_eq!(a.0, U512::from(5));

        let a = Uint512::from(5u8);
        assert_eq!(a.0, U512::from(5));

        let result = Uint512::try_from("34567");
        assert_eq!(result.unwrap().0, U512::from_dec_str("34567").unwrap());

        let result = Uint512::try_from("1.23");
        assert!(result.is_err());
    }

    #[test]
    fn uint512_convert_to_uint128() {
        let source = Uint512::from(42u128);
        let target = Uint128::try_from(source);
        assert_eq!(target, Ok(Uint128::new(42u128)));

        let source = Uint512::MAX;
        let target = Uint128::try_from(source);
        assert_eq!(
            target,
            Err(ConversionOverflowError::new(
                "Uint512",
                "Uint128",
                Uint512::MAX.to_string()
            ))
        );
    }

    #[test]
    fn uint512_from_uint256() {
        assert_eq!(
            Uint512::from_uint256(Uint256::from_str("123").unwrap()),
            Uint512::from_str("123").unwrap()
        );

        assert_eq!(
            Uint512::from_uint256(Uint256::from_str("9785746283745").unwrap()),
            Uint512::from_str("9785746283745").unwrap()
        );

        assert_eq!(
            Uint512::from_uint256(
                Uint256::from_str(
                    "97857462837575757832978493758398593853985452378423874623874628736482736487236"
                )
                .unwrap()
            ),
            Uint512::from_str(
                "97857462837575757832978493758398593853985452378423874623874628736482736487236"
            )
            .unwrap()
        );
    }

    #[test]
    fn uint512_implements_display() {
        let a = Uint512::from(12345u32);
        assert_eq!(format!("Embedded: {}", a), "Embedded: 12345");
        assert_eq!(a.to_string(), "12345");

        let a = Uint512::zero();
        assert_eq!(format!("Embedded: {}", a), "Embedded: 0");
        assert_eq!(a.to_string(), "0");
    }

    #[test]
    fn uint512_display_padding_works() {
        let a = Uint512::from(123u64);
        assert_eq!(format!("Embedded: {:05}", a), "Embedded: 00123");
    }

    #[test]
    fn uint512_to_be_bytes_works() {
        assert_eq!(
            Uint512::zero().to_be_bytes(),
            [
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0,
            ]
        );
        assert_eq!(
            Uint512::MAX.to_be_bytes(),
            [
                0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
                0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
                0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
                0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
                0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
            ]
        );
        assert_eq!(
            Uint512::from(1u128).to_be_bytes(),
            [
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 1
            ]
        );
        // Python: `[b for b in (240282366920938463463374607431768124608).to_bytes(64, "big")]`
        assert_eq!(
            Uint512::from(240282366920938463463374607431768124608u128).to_be_bytes(),
            [
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 180, 196, 179, 87, 165,
                121, 59, 133, 246, 117, 221, 191, 255, 254, 172, 192
            ]
        );
        assert_eq!(
            Uint512::from_be_bytes([
                17, 4, 23, 32, 87, 67, 123, 200, 58, 91, 0, 38, 33, 21, 67, 78, 87, 76, 65, 54,
                211, 201, 192, 7, 42, 233, 2, 240, 200, 115, 150, 240, 218, 88, 106, 45, 208, 134,
                238, 119, 85, 22, 14, 88, 166, 195, 154, 73, 64, 10, 44, 59, 13, 22, 47, 12, 99, 8,
                252, 96, 230, 187, 38, 29
            ])
            .to_be_bytes(),
            [
                17, 4, 23, 32, 87, 67, 123, 200, 58, 91, 0, 38, 33, 21, 67, 78, 87, 76, 65, 54,
                211, 201, 192, 7, 42, 233, 2, 240, 200, 115, 150, 240, 218, 88, 106, 45, 208, 134,
                238, 119, 85, 22, 14, 88, 166, 195, 154, 73, 64, 10, 44, 59, 13, 22, 47, 12, 99, 8,
                252, 96, 230, 187, 38, 29
            ]
        );
    }

    #[test]
    fn uint512_to_le_bytes_works() {
        assert_eq!(
            Uint512::zero().to_le_bytes(),
            [
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0
            ]
        );
        assert_eq!(
            Uint512::MAX.to_le_bytes(),
            [
                0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
                0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
                0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
                0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
                0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff
            ]
        );
        assert_eq!(
            Uint512::from(1u128).to_le_bytes(),
            [
                1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0
            ]
        );
        // Python: `[b for b in (240282366920938463463374607431768124608).to_bytes(64, "little")]`
        assert_eq!(
            Uint512::from(240282366920938463463374607431768124608u128).to_le_bytes(),
            [
                192, 172, 254, 255, 191, 221, 117, 246, 133, 59, 121, 165, 87, 179, 196, 180, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
            ]
        );
        assert_eq!(
            Uint512::from_be_bytes([
                17, 4, 23, 32, 87, 67, 123, 200, 58, 91, 0, 38, 33, 21, 67, 78, 87, 76, 65, 54,
                211, 201, 192, 7, 42, 233, 2, 240, 200, 115, 150, 240, 218, 88, 106, 45, 208, 134,
                238, 119, 85, 22, 14, 88, 166, 195, 154, 73, 64, 10, 44, 59, 13, 22, 47, 12, 99, 8,
                252, 96, 230, 187, 38, 29
            ])
            .to_le_bytes(),
            [
                29, 38, 187, 230, 96, 252, 8, 99, 12, 47, 22, 13, 59, 44, 10, 64, 73, 154, 195,
                166, 88, 14, 22, 85, 119, 238, 134, 208, 45, 106, 88, 218, 240, 150, 115, 200, 240,
                2, 233, 42, 7, 192, 201, 211, 54, 65, 76, 87, 78, 67, 21, 33, 38, 0, 91, 58, 200,
                123, 67, 87, 32, 23, 4, 17
            ]
        );
    }

    #[test]
    fn uint512_is_zero_works() {
        assert!(Uint512::zero().is_zero());
        assert!(Uint512(U512::from(0)).is_zero());

        assert!(!Uint512::from(1u32).is_zero());
        assert!(!Uint512::from(123u32).is_zero());
    }

    #[test]
    fn uint512_wrapping_methods() {
        // wrapping_add
        assert_eq!(
            Uint512::from(2u32).wrapping_add(Uint512::from(2u32)),
            Uint512::from(4u32)
        ); // non-wrapping
        assert_eq!(
            Uint512::MAX.wrapping_add(Uint512::from(1u32)),
            Uint512::from(0u32)
        ); // wrapping

        // wrapping_sub
        assert_eq!(
            Uint512::from(7u32).wrapping_sub(Uint512::from(5u32)),
            Uint512::from(2u32)
        ); // non-wrapping
        assert_eq!(
            Uint512::from(0u32).wrapping_sub(Uint512::from(1u32)),
            Uint512::MAX
        ); // wrapping

        // wrapping_mul
        assert_eq!(
            Uint512::from(3u32).wrapping_mul(Uint512::from(2u32)),
            Uint512::from(6u32)
        ); // non-wrapping
        assert_eq!(
            Uint512::MAX.wrapping_mul(Uint512::from(2u32)),
            Uint512::MAX - Uint512::one()
        ); // wrapping

        // wrapping_pow
        assert_eq!(Uint512::from(2u32).wrapping_pow(3), Uint512::from(8u32)); // non-wrapping
        assert_eq!(Uint512::MAX.wrapping_pow(2), Uint512::from(1u32)); // wrapping
    }

    #[test]
    fn uint512_json() {
        let orig = Uint512::from(1234567890987654321u128);
        let serialized = to_vec(&orig).unwrap();
        assert_eq!(serialized.as_slice(), b"\"1234567890987654321\"");
        let parsed: Uint512 = from_slice(&serialized).unwrap();
        assert_eq!(parsed, orig);
    }

    #[test]
    fn uint512_compare() {
        let a = Uint512::from(12345u32);
        let b = Uint512::from(23456u32);

        assert!(a < b);
        assert!(b > a);
        assert_eq!(a, Uint512::from(12345u32));
    }

    #[test]
    #[allow(clippy::op_ref)]
    fn uint512_math() {
        let a = Uint512::from(12345u32);
        let b = Uint512::from(23456u32);

        // test + with owned and reference right hand side
        assert_eq!(a + b, Uint512::from(35801u32));
        assert_eq!(a + &b, Uint512::from(35801u32));

        // test - with owned and reference right hand side
        assert_eq!(b - a, Uint512::from(11111u32));
        assert_eq!(b - &a, Uint512::from(11111u32));

        // test += with owned and reference right hand side
        let mut c = Uint512::from(300000u32);
        c += b;
        assert_eq!(c, Uint512::from(323456u32));
        let mut d = Uint512::from(300000u32);
        d += &b;
        assert_eq!(d, Uint512::from(323456u32));

        // test -= with owned and reference right hand side
        let mut c = Uint512::from(300000u32);
        c -= b;
        assert_eq!(c, Uint512::from(276544u32));
        let mut d = Uint512::from(300000u32);
        d -= &b;
        assert_eq!(d, Uint512::from(276544u32));

        // error result on underflow (- would produce negative result)
        let underflow_result = a.checked_sub(b);
        let OverflowError {
            operand1, operand2, ..
        } = underflow_result.unwrap_err();
        assert_eq!((operand1, operand2), (a.to_string(), b.to_string()));
    }

    #[test]
    #[should_panic]
    fn uint512_add_overflow_panics() {
        let max = Uint512::new([255u8; 64]);
        let _ = max + Uint512::from(12u32);
    }

    #[test]
    #[allow(clippy::op_ref)]
    fn uint512_sub_works() {
        assert_eq!(
            Uint512::from(2u32) - Uint512::from(1u32),
            Uint512::from(1u32)
        );
        assert_eq!(
            Uint512::from(2u32) - Uint512::from(0u32),
            Uint512::from(2u32)
        );
        assert_eq!(
            Uint512::from(2u32) - Uint512::from(2u32),
            Uint512::from(0u32)
        );

        // works for refs
        let a = Uint512::from(10u32);
        let b = Uint512::from(3u32);
        let expected = Uint512::from(7u32);
        assert_eq!(a - b, expected);
        assert_eq!(a - &b, expected);
        assert_eq!(&a - b, expected);
        assert_eq!(&a - &b, expected);
    }

    #[test]
    #[should_panic]
    fn uint512_sub_overflow_panics() {
        let _ = Uint512::from(1u32) - Uint512::from(2u32);
    }

    #[test]
    fn uint512_sub_assign_works() {
        let mut a = Uint512::from(14u32);
        a -= Uint512::from(2u32);
        assert_eq!(a, Uint512::from(12u32));

        // works for refs
        let mut a = Uint512::from(10u32);
        let b = Uint512::from(3u32);
        let expected = Uint512::from(7u32);
        a -= &b;
        assert_eq!(a, expected);
    }

    #[test]
    #[allow(clippy::op_ref)]
    fn uint512_mul_works() {
        assert_eq!(
            Uint512::from(2u32) * Uint512::from(3u32),
            Uint512::from(6u32)
        );
        assert_eq!(Uint512::from(2u32) * Uint512::zero(), Uint512::zero());

        // works for refs
        let a = Uint512::from(11u32);
        let b = Uint512::from(3u32);
        let expected = Uint512::from(33u32);
        assert_eq!(a * b, expected);
        assert_eq!(a * &b, expected);
        assert_eq!(&a * b, expected);
        assert_eq!(&a * &b, expected);
    }

    #[test]
    fn uint512_mul_assign_works() {
        let mut a = Uint512::from(14u32);
        a *= Uint512::from(2u32);
        assert_eq!(a, Uint512::from(28u32));

        // works for refs
        let mut a = Uint512::from(10u32);
        let b = Uint512::from(3u32);
        a *= &b;
        assert_eq!(a, Uint512::from(30u32));
    }

    #[test]
    fn uint512_pow_works() {
        assert_eq!(Uint512::from(2u32).pow(2), Uint512::from(4u32));
        assert_eq!(Uint512::from(2u32).pow(10), Uint512::from(1024u32));
    }

    #[test]
    #[should_panic]
    fn uint512_pow_overflow_panics() {
        Uint512::MAX.pow(2u32);
    }

    #[test]
    fn uint512_shr_works() {
        let original = Uint512::new([
            0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
            0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
            0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
            0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 2u8, 0u8, 4u8, 2u8,
        ]);

        let shifted = Uint512::new([
            0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
            0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
            0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
            0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 128u8, 1u8, 0u8,
        ]);

        assert_eq!(original >> 2u32, shifted);
    }

    #[test]
    #[should_panic]
    fn uint512_shr_overflow_panics() {
        let _ = Uint512::from(1u32) >> 512u32;
    }

    #[test]
    fn sum_works() {
        let nums = vec![
            Uint512::from(17u32),
            Uint512::from(123u32),
            Uint512::from(540u32),
            Uint512::from(82u32),
        ];
        let expected = Uint512::from(762u32);

        let sum_as_ref: Uint512 = nums.iter().sum();
        assert_eq!(expected, sum_as_ref);

        let sum_as_owned: Uint512 = nums.into_iter().sum();
        assert_eq!(expected, sum_as_owned);
    }

    #[test]
    fn uint512_methods() {
        // checked_*
        assert!(matches!(
            Uint512::MAX.checked_add(Uint512::from(1u32)),
            Err(OverflowError { .. })
        ));
        assert_eq!(
            Uint512::from(1u32).checked_add(Uint512::from(1u32)),
            Ok(Uint512::from(2u32)),
        );
        assert!(matches!(
            Uint512::from(0u32).checked_sub(Uint512::from(1u32)),
            Err(OverflowError { .. })
        ));
        assert_eq!(
            Uint512::from(2u32).checked_sub(Uint512::from(1u32)),
            Ok(Uint512::from(1u32)),
        );
        assert!(matches!(
            Uint512::MAX.checked_mul(Uint512::from(2u32)),
            Err(OverflowError { .. })
        ));
        assert_eq!(
            Uint512::from(2u32).checked_mul(Uint512::from(2u32)),
            Ok(Uint512::from(4u32)),
        );
        assert!(matches!(
            Uint512::MAX.checked_pow(2u32),
            Err(OverflowError { .. })
        ));
        assert_eq!(
            Uint512::from(2u32).checked_pow(3u32),
            Ok(Uint512::from(8u32)),
        );
        assert!(matches!(
            Uint512::MAX.checked_div(Uint512::from(0u32)),
            Err(DivideByZeroError { .. })
        ));
        assert_eq!(
            Uint512::from(6u32).checked_div(Uint512::from(2u32)),
            Ok(Uint512::from(3u32)),
        );
        assert!(matches!(
            Uint512::MAX.checked_div_euclid(Uint512::from(0u32)),
            Err(DivideByZeroError { .. })
        ));
        assert_eq!(
            Uint512::from(6u32).checked_div_euclid(Uint512::from(2u32)),
            Ok(Uint512::from(3u32)),
        );
        assert_eq!(
            Uint512::from(7u32).checked_div_euclid(Uint512::from(2u32)),
            Ok(Uint512::from(3u32)),
        );
        assert!(matches!(
            Uint512::MAX.checked_rem(Uint512::from(0u32)),
            Err(DivideByZeroError { .. })
        ));

        // saturating_*
        assert_eq!(
            Uint512::MAX.saturating_add(Uint512::from(1u32)),
            Uint512::MAX
        );
        assert_eq!(
            Uint512::from(0u32).saturating_sub(Uint512::from(1u32)),
            Uint512::from(0u32)
        );
        assert_eq!(
            Uint512::MAX.saturating_mul(Uint512::from(2u32)),
            Uint512::MAX
        );
        assert_eq!(
            Uint512::from(4u32).saturating_pow(2u32),
            Uint512::from(16u32)
        );
        assert_eq!(Uint512::MAX.saturating_pow(2u32), Uint512::MAX);
    }

    #[test]
    #[allow(clippy::op_ref)]
    fn uint512_implements_rem() {
        let a = Uint512::from(10u32);
        assert_eq!(a % Uint512::from(10u32), Uint512::zero());
        assert_eq!(a % Uint512::from(2u32), Uint512::zero());
        assert_eq!(a % Uint512::from(1u32), Uint512::zero());
        assert_eq!(a % Uint512::from(3u32), Uint512::from(1u32));
        assert_eq!(a % Uint512::from(4u32), Uint512::from(2u32));

        // works for refs
        let a = Uint512::from(10u32);
        let b = Uint512::from(3u32);
        let expected = Uint512::from(1u32);
        assert_eq!(a % b, expected);
        assert_eq!(a % &b, expected);
        assert_eq!(&a % b, expected);
        assert_eq!(&a % &b, expected);
    }

    #[test]
    #[should_panic(expected = "division by zero")]
    fn uint512_rem_panics_for_zero() {
        let _ = Uint512::from(10u32) % Uint512::zero();
    }

    #[test]
    #[allow(clippy::op_ref)]
    fn uint512_rem_works() {
        assert_eq!(
            Uint512::from(12u32) % Uint512::from(10u32),
            Uint512::from(2u32)
        );
        assert_eq!(Uint512::from(50u32) % Uint512::from(5u32), Uint512::zero());

        // works for refs
        let a = Uint512::from(42u32);
        let b = Uint512::from(5u32);
        let expected = Uint512::from(2u32);
        assert_eq!(a % b, expected);
        assert_eq!(a % &b, expected);
        assert_eq!(&a % b, expected);
        assert_eq!(&a % &b, expected);
    }

    #[test]
    fn uint512_rem_assign_works() {
        let mut a = Uint512::from(30u32);
        a %= Uint512::from(4u32);
        assert_eq!(a, Uint512::from(2u32));

        // works for refs
        let mut a = Uint512::from(25u32);
        let b = Uint512::from(6u32);
        a %= &b;
        assert_eq!(a, Uint512::from(1u32));
    }

    #[test]
    fn uint512_abs_diff_works() {
        let a = Uint512::from(42u32);
        let b = Uint512::from(5u32);
        let expected = Uint512::from(37u32);
        assert_eq!(a.abs_diff(b), expected);
        assert_eq!(b.abs_diff(a), expected);
    }

    #[test]
    fn uint512_partial_eq() {
        let test_cases = [(1, 1, true), (42, 42, true), (42, 24, false), (0, 0, true)]
            .into_iter()
            .map(|(lhs, rhs, expected): (u64, u64, bool)| {
                (Uint512::from(lhs), Uint512::from(rhs), expected)
            });

        #[allow(clippy::op_ref)]
        for (lhs, rhs, expected) in test_cases {
            assert_eq!(lhs == rhs, expected);
            assert_eq!(&lhs == rhs, expected);
            assert_eq!(lhs == &rhs, expected);
            assert_eq!(&lhs == &rhs, expected);
        }
    }
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
    pub const fn new(value: u64) -> Self {
        Uint64(value)
    }

    /// Creates a Uint64(0)
    #[inline]
    pub const fn zero() -> Self {
        Uint64(0)
    }

    /// Creates a Uint64(1)
    #[inline]
    pub const fn one() -> Self {
        Self(1)
    }

    /// Returns a copy of the internal data
    pub const fn u64(&self) -> u64 {
        self.0
    }

    /// Returns a copy of the number as big endian bytes.
    pub const fn to_be_bytes(self) -> [u8; 8] {
        self.0.to_be_bytes()
    }

    /// Returns a copy of the number as little endian bytes.
    pub const fn to_le_bytes(self) -> [u8; 8] {
        self.0.to_le_bytes()
    }

    pub const fn is_zero(&self) -> bool {
        self.0 == 0
    }

    pub fn pow(self, exp: u32) -> Self {
        self.0.pow(exp).into()
    }

    /// Returns `self * numerator / denominator`.
    ///
    /// Due to the nature of the integer division involved, the result is always floored.
    /// E.g. 5 * 99/100 = 4.
    pub fn multiply_ratio<A: Into<u64>, B: Into<u64>>(
        &self,
        numerator: A,
        denominator: B,
    ) -> Uint64 {
        match self.checked_multiply_ratio(numerator, denominator) {
            Ok(value) => value,
            Err(CheckedMultiplyRatioError::DivideByZero) => {
                panic!("Denominator must not be zero")
            }
            Err(CheckedMultiplyRatioError::Overflow) => panic!("Multiplication overflow"),
        }
    }

    /// Returns `self * numerator / denominator`.
    ///
    /// Due to the nature of the integer division involved, the result is always floored.
    /// E.g. 5 * 99/100 = 4.
    pub fn checked_multiply_ratio<A: Into<u64>, B: Into<u64>>(
        &self,
        numerator: A,
        denominator: B,
    ) -> Result<Uint64, CheckedMultiplyRatioError> {
        let numerator = numerator.into();
        let denominator = denominator.into();
        if denominator == 0 {
            return Err(CheckedMultiplyRatioError::DivideByZero);
        }
        match (self.full_mul(numerator) / Uint128::from(denominator)).try_into() {
            Ok(ratio) => Ok(ratio),
            Err(_) => Err(CheckedMultiplyRatioError::Overflow),
        }
    }

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
    pub fn full_mul(self, rhs: impl Into<u64>) -> Uint128 {
        Uint128::from(self.u64())
            .checked_mul(Uint128::from(rhs.into()))
            .unwrap()
    }

    pub fn checked_add(self, other: Self) -> Result<Self, OverflowError> {
        self.0
            .checked_add(other.0)
            .map(Self)
            .ok_or_else(|| OverflowError::new(OverflowOperation::Add, self, other))
    }

    pub fn checked_sub(self, other: Self) -> Result<Self, OverflowError> {
        self.0
            .checked_sub(other.0)
            .map(Self)
            .ok_or_else(|| OverflowError::new(OverflowOperation::Sub, self, other))
    }

    pub fn checked_mul(self, other: Self) -> Result<Self, OverflowError> {
        self.0
            .checked_mul(other.0)
            .map(Self)
            .ok_or_else(|| OverflowError::new(OverflowOperation::Mul, self, other))
    }

    pub fn checked_pow(self, exp: u32) -> Result<Self, OverflowError> {
        self.0
            .checked_pow(exp)
            .map(Self)
            .ok_or_else(|| OverflowError::new(OverflowOperation::Pow, self, exp))
    }

    pub fn checked_div(self, other: Self) -> Result<Self, DivideByZeroError> {
        self.0
            .checked_div(other.0)
            .map(Self)
            .ok_or_else(|| DivideByZeroError::new(self))
    }

    pub fn checked_div_euclid(self, other: Self) -> Result<Self, DivideByZeroError> {
        self.0
            .checked_div_euclid(other.0)
            .map(Self)
            .ok_or_else(|| DivideByZeroError::new(self))
    }

    pub fn checked_rem(self, other: Self) -> Result<Self, DivideByZeroError> {
        self.0
            .checked_rem(other.0)
            .map(Self)
            .ok_or_else(|| DivideByZeroError::new(self))
    }

    #[inline]
    pub fn wrapping_add(self, other: Self) -> Self {
        Self(self.0.wrapping_add(other.0))
    }

    #[inline]
    pub fn wrapping_sub(self, other: Self) -> Self {
        Self(self.0.wrapping_sub(other.0))
    }

    #[inline]
    pub fn wrapping_mul(self, other: Self) -> Self {
        Self(self.0.wrapping_mul(other.0))
    }

    #[inline]
    pub fn wrapping_pow(self, other: u32) -> Self {
        Self(self.0.wrapping_pow(other))
    }

    pub fn saturating_add(self, other: Self) -> Self {
        Self(self.0.saturating_add(other.0))
    }

    pub fn saturating_sub(self, other: Self) -> Self {
        Self(self.0.saturating_sub(other.0))
    }

    pub fn saturating_mul(self, other: Self) -> Self {
        Self(self.0.saturating_mul(other.0))
    }

    pub fn saturating_pow(self, exp: u32) -> Self {
        Self(self.0.saturating_pow(exp))
    }

    pub const fn abs_diff(self, other: Self) -> Self {
        Self(if self.0 < other.0 {
            other.0 - self.0
        } else {
            self.0 - other.0
        })
    }
}

// `From<u{128,64,32,16,8}>` is implemented manually instead of
// using `impl<T: Into<u64>> From<T> for Uint64` because
// of the conflict with `TryFrom<&str>` as described here
// https://stackoverflow.com/questions/63136970/how-do-i-work-around-the-upstream-crates-may-add-a-new-impl-of-trait-error

impl From<u64> for Uint64 {
    fn from(val: u64) -> Self {
        Uint64(val)
    }
}

impl From<u32> for Uint64 {
    fn from(val: u32) -> Self {
        Uint64(val.into())
    }
}

impl From<u16> for Uint64 {
    fn from(val: u16) -> Self {
        Uint64(val.into())
    }
}

impl From<u8> for Uint64 {
    fn from(val: u8) -> Self {
        Uint64(val.into())
    }
}

impl TryFrom<&str> for Uint64 {
    type Error = StdError;

    fn try_from(val: &str) -> Result<Self, Self::Error> {
        match val.parse::<u64>() {
            Ok(u) => Ok(Uint64(u)),
            Err(e) => Err(StdError::generic_err(format!("Parsing u64: {}", e))),
        }
    }
}

impl From<Uint64> for String {
    fn from(original: Uint64) -> Self {
        original.to_string()
    }
}

impl From<Uint64> for u64 {
    fn from(original: Uint64) -> Self {
        original.0
    }
}

impl fmt::Display for Uint64 {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl Add<Uint64> for Uint64 {
    type Output = Self;

    fn add(self, rhs: Self) -> Self {
        Uint64(self.u64().checked_add(rhs.u64()).unwrap())
    }
}

impl<'a> Add<&'a Uint64> for Uint64 {
    type Output = Self;

    fn add(self, rhs: &'a Uint64) -> Self {
        Uint64(self.u64().checked_add(rhs.u64()).unwrap())
    }
}

impl Sub<Uint64> for Uint64 {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self {
        Uint64(
            self.u64()
                .checked_sub(rhs.u64())
                .expect("attempt to subtract with overflow"),
        )
    }
}
forward_ref_binop!(impl Sub, sub for Uint64, Uint64);

impl SubAssign<Uint64> for Uint64 {
    fn sub_assign(&mut self, rhs: Uint64) {
        *self = *self - rhs;
    }
}
forward_ref_op_assign!(impl SubAssign, sub_assign for Uint64, Uint64);

impl Mul<Uint64> for Uint64 {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        Self(
            self.u64()
                .checked_mul(rhs.u64())
                .expect("attempt to multiply with overflow"),
        )
    }
}
forward_ref_binop!(impl Mul, mul for Uint64, Uint64);

impl MulAssign<Uint64> for Uint64 {
    fn mul_assign(&mut self, rhs: Self) {
        *self = *self * rhs;
    }
}
forward_ref_op_assign!(impl MulAssign, mul_assign for Uint64, Uint64);

impl Div<Uint64> for Uint64 {
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        Self(self.u64().checked_div(rhs.u64()).unwrap())
    }
}

impl<'a> Div<&'a Uint64> for Uint64 {
    type Output = Self;

    fn div(self, rhs: &'a Uint64) -> Self::Output {
        Self(self.u64().checked_div(rhs.u64()).unwrap())
    }
}

impl Rem for Uint64 {
    type Output = Self;

    /// # Panics
    ///
    /// This operation will panic if `rhs` is zero.
    #[inline]
    fn rem(self, rhs: Self) -> Self {
        Self(self.0.rem(rhs.0))
    }
}
forward_ref_binop!(impl Rem, rem for Uint64, Uint64);

impl RemAssign<Uint64> for Uint64 {
    fn rem_assign(&mut self, rhs: Uint64) {
        *self = *self % rhs;
    }
}
forward_ref_op_assign!(impl RemAssign, rem_assign for Uint64, Uint64);

impl Shr<u32> for Uint64 {
    type Output = Self;

    fn shr(self, rhs: u32) -> Self::Output {
        Self(self.u64().checked_shr(rhs).unwrap())
    }
}

impl<'a> Shr<&'a u32> for Uint64 {
    type Output = Self;

    fn shr(self, rhs: &'a u32) -> Self::Output {
        Self(self.u64().checked_shr(*rhs).unwrap())
    }
}

impl AddAssign<Uint64> for Uint64 {
    fn add_assign(&mut self, rhs: Uint64) {
        self.0 = self.0.checked_add(rhs.u64()).unwrap();
    }
}

impl<'a> AddAssign<&'a Uint64> for Uint64 {
    fn add_assign(&mut self, rhs: &'a Uint64) {
        self.0 = self.0.checked_add(rhs.u64()).unwrap();
    }
}

impl DivAssign<Uint64> for Uint64 {
    fn div_assign(&mut self, rhs: Self) {
        self.0 = self.0.checked_div(rhs.u64()).unwrap();
    }
}

impl<'a> DivAssign<&'a Uint64> for Uint64 {
    fn div_assign(&mut self, rhs: &'a Uint64) {
        self.0 = self.0.checked_div(rhs.u64()).unwrap();
    }
}

impl ShrAssign<u32> for Uint64 {
    fn shr_assign(&mut self, rhs: u32) {
        self.0 = self.0.checked_shr(rhs).unwrap();
    }
}

impl<'a> ShrAssign<&'a u32> for Uint64 {
    fn shr_assign(&mut self, rhs: &'a u32) {
        self.0 = self.0.checked_shr(*rhs).unwrap();
    }
}

impl Serialize for Uint64 {
    /// Serializes as an integer string using base 10
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: ser::Serializer,
    {
        serializer.serialize_str(&self.to_string())
    }
}

impl<'de> Deserialize<'de> for Uint64 {
    /// Deserialized from an integer string using base 10
    fn deserialize<D>(deserializer: D) -> Result<Uint64, D::Error>
    where
        D: Deserializer<'de>,
    {
        deserializer.deserialize_str(Uint64Visitor)
    }
}

struct Uint64Visitor;

impl<'de> de::Visitor<'de> for Uint64Visitor {
    type Value = Uint64;

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str("string-encoded integer")
    }

    fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
    where
        E: de::Error,
    {
        match v.parse::<u64>() {
            Ok(u) => Ok(Uint64(u)),
            Err(e) => Err(E::custom(format!("invalid Uint64 '{}' - {}", v, e))),
        }
    }
}

impl<A> std::iter::Sum<A> for Uint64
where
    Self: Add<A, Output = Self>,
{
    fn sum<I: Iterator<Item = A>>(iter: I) -> Self {
        iter.fold(Self::zero(), Add::add)
    }
}

impl PartialEq<&Uint64> for Uint64 {
    fn eq(&self, rhs: &&Uint64) -> bool {
        self == *rhs
    }
}

impl PartialEq<Uint64> for &Uint64 {
    fn eq(&self, rhs: &Uint64) -> bool {
        *self == rhs
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{from_slice, to_vec};

    #[test]
    fn uint64_zero_works() {
        let zero = Uint64::zero();
        assert_eq!(zero.to_be_bytes(), [0, 0, 0, 0, 0, 0, 0, 0]);
    }

    #[test]
    fn uint64_one_works() {
        let one = Uint64::one();
        assert_eq!(one.to_be_bytes(), [0, 0, 0, 0, 0, 0, 0, 1]);
    }

    #[test]
    fn uint64_convert_into() {
        let original = Uint64(12345);
        let a = u64::from(original);
        assert_eq!(a, 12345);

        let original = Uint64(12345);
        let a = String::from(original);
        assert_eq!(a, "12345");
    }

    #[test]
    fn uint64_convert_from() {
        let a = Uint64::from(5u64);
        assert_eq!(a.0, 5);

        let a = Uint64::from(5u32);
        assert_eq!(a.0, 5);

        let a = Uint64::from(5u16);
        assert_eq!(a.0, 5);

        let a = Uint64::from(5u8);
        assert_eq!(a.0, 5);

        let result = Uint64::try_from("34567");
        assert_eq!(result.unwrap().0, 34567);

        let result = Uint64::try_from("1.23");
        assert!(result.is_err());
    }

    #[test]
    fn uint64_implements_display() {
        let a = Uint64(12345);
        assert_eq!(format!("Embedded: {}", a), "Embedded: 12345");
        assert_eq!(a.to_string(), "12345");

        let a = Uint64(0);
        assert_eq!(format!("Embedded: {}", a), "Embedded: 0");
        assert_eq!(a.to_string(), "0");
    }

    #[test]
    fn uint64_display_padding_works() {
        let a = Uint64::from(123u64);
        assert_eq!(format!("Embedded: {:05}", a), "Embedded: 00123");
    }

    #[test]
    fn uint64_to_be_bytes_works() {
        assert_eq!(Uint64::zero().to_be_bytes(), [0, 0, 0, 0, 0, 0, 0, 0]);
        assert_eq!(
            Uint64::MAX.to_be_bytes(),
            [0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff]
        );
        assert_eq!(Uint64::new(1).to_be_bytes(), [0, 0, 0, 0, 0, 0, 0, 1]);
        // Python: `[b for b in (63374607431768124608).to_bytes(8, "big")]`
        assert_eq!(
            Uint64::new(874607431768124608).to_be_bytes(),
            [12, 35, 58, 211, 72, 116, 172, 192]
        );
    }

    #[test]
    fn uint64_to_le_bytes_works() {
        assert_eq!(Uint64::zero().to_le_bytes(), [0, 0, 0, 0, 0, 0, 0, 0]);
        assert_eq!(
            Uint64::MAX.to_le_bytes(),
            [0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff]
        );
        assert_eq!(Uint64::new(1).to_le_bytes(), [1, 0, 0, 0, 0, 0, 0, 0]);
        // Python: `[b for b in (240282366920938463463374607431768124608).to_bytes(16, "little")]`
        assert_eq!(
            Uint64::new(874607431768124608).to_le_bytes(),
            [192, 172, 116, 72, 211, 58, 35, 12]
        );
    }

    #[test]
    fn uint64_is_zero_works() {
        assert!(Uint64::zero().is_zero());
        assert!(Uint64(0).is_zero());

        assert!(!Uint64(1).is_zero());
        assert!(!Uint64(123).is_zero());
    }

    #[test]
    fn uint64_json() {
        let orig = Uint64(1234567890987654321);
        let serialized = to_vec(&orig).unwrap();
        assert_eq!(serialized.as_slice(), b"\"1234567890987654321\"");
        let parsed: Uint64 = from_slice(&serialized).unwrap();
        assert_eq!(parsed, orig);
    }

    #[test]
    fn uint64_compare() {
        let a = Uint64(12345);
        let b = Uint64(23456);

        assert!(a < b);
        assert!(b > a);
        assert_eq!(a, Uint64(12345));
    }

    #[test]
    #[allow(clippy::op_ref)]
    fn uint64_math() {
        let a = Uint64(12345);
        let b = Uint64(23456);

        // test + with owned and reference right hand side
        assert_eq!(a + b, Uint64(35801));
        assert_eq!(a + &b, Uint64(35801));

        // test - with owned and reference right hand side
        assert_eq!((b.checked_sub(a)).unwrap(), Uint64(11111));

        // test += with owned and reference right hand side
        let mut c = Uint64(300000);
        c += b;
        assert_eq!(c, Uint64(323456));
        let mut d = Uint64(300000);
        d += &b;
        assert_eq!(d, Uint64(323456));

        // error result on underflow (- would produce negative result)
        let underflow_result = a.checked_sub(b);
        let OverflowError {
            operand1, operand2, ..
        } = underflow_result.unwrap_err();
        assert_eq!((operand1, operand2), (a.to_string(), b.to_string()));
    }

    #[test]
    #[allow(clippy::op_ref)]
    fn uint64_sub_works() {
        assert_eq!(Uint64(2) - Uint64(1), Uint64(1));
        assert_eq!(Uint64(2) - Uint64(0), Uint64(2));
        assert_eq!(Uint64(2) - Uint64(2), Uint64(0));

        // works for refs
        let a = Uint64::new(10);
        let b = Uint64::new(3);
        let expected = Uint64::new(7);
        assert_eq!(a - b, expected);
        assert_eq!(a - &b, expected);
        assert_eq!(&a - b, expected);
        assert_eq!(&a - &b, expected);
    }

    #[test]
    #[should_panic]
    fn uint64_sub_overflow_panics() {
        let _ = Uint64(1) - Uint64(2);
    }

    #[test]
    fn uint64_sub_assign_works() {
        let mut a = Uint64(14);
        a -= Uint64(2);
        assert_eq!(a, Uint64(12));

        // works for refs
        let mut a = Uint64::new(10);
        let b = Uint64::new(3);
        let expected = Uint64::new(7);
        a -= &b;
        assert_eq!(a, expected);
    }

    #[test]
    #[allow(clippy::op_ref)]
    fn uint64_mul_works() {
        assert_eq!(Uint64::from(2u32) * Uint64::from(3u32), Uint64::from(6u32));
        assert_eq!(Uint64::from(2u32) * Uint64::zero(), Uint64::zero());

        // works for refs
        let a = Uint64::from(11u32);
        let b = Uint64::from(3u32);
        let expected = Uint64::from(33u32);
        assert_eq!(a * b, expected);
        assert_eq!(a * &b, expected);
        assert_eq!(&a * b, expected);
        assert_eq!(&a * &b, expected);
    }

    #[test]
    fn uint64_mul_assign_works() {
        let mut a = Uint64::from(14u32);
        a *= Uint64::from(2u32);
        assert_eq!(a, Uint64::from(28u32));

        // works for refs
        let mut a = Uint64::from(10u32);
        let b = Uint64::from(3u32);
        a *= &b;
        assert_eq!(a, Uint64::from(30u32));
    }

    #[test]
    fn uint64_pow_works() {
        assert_eq!(Uint64::from(2u32).pow(2), Uint64::from(4u32));
        assert_eq!(Uint64::from(2u32).pow(10), Uint64::from(1024u32));
    }

    #[test]
    #[should_panic]
    fn uint64_pow_overflow_panics() {
        Uint64::MAX.pow(2u32);
    }

    #[test]
    #[should_panic]
    fn uint64_math_overflow_panics() {
        // almost_max is 2^64 - 10
        let almost_max = Uint64(18446744073709551606);
        let _ = almost_max + Uint64(12);
    }

    #[test]
    fn uint64_multiply_ratio_works() {
        let base = Uint64(500);

        // factor 1/1
        assert_eq!(base.multiply_ratio(1u64, 1u64), base);
        assert_eq!(base.multiply_ratio(3u64, 3u64), base);
        assert_eq!(base.multiply_ratio(654321u64, 654321u64), base);
        assert_eq!(base.multiply_ratio(u64::MAX, u64::MAX), base);

        // factor 3/2
        assert_eq!(base.multiply_ratio(3u64, 2u64), Uint64(750));
        assert_eq!(base.multiply_ratio(333333u64, 222222u64), Uint64(750));

        // factor 2/3 (integer devision always floors the result)
        assert_eq!(base.multiply_ratio(2u64, 3u64), Uint64(333));
        assert_eq!(base.multiply_ratio(222222u64, 333333u64), Uint64(333));

        // factor 5/6 (integer devision always floors the result)
        assert_eq!(base.multiply_ratio(5u64, 6u64), Uint64(416));
        assert_eq!(base.multiply_ratio(100u64, 120u64), Uint64(416));
    }

    #[test]
    fn uint64_multiply_ratio_does_not_overflow_when_result_fits() {
        // Almost max value for Uint64.
        let base = Uint64(u64::MAX - 9);

        assert_eq!(base.multiply_ratio(2u64, 2u64), base);
    }

    #[test]
    #[should_panic]
    fn uint64_multiply_ratio_panicks_on_overflow() {
        // Almost max value for Uint64.
        let base = Uint64(u64::MAX - 9);

        assert_eq!(base.multiply_ratio(2u64, 1u64), base);
    }

    #[test]
    #[should_panic(expected = "Denominator must not be zero")]
    fn uint64_multiply_ratio_panics_for_zero_denominator() {
        Uint64(500).multiply_ratio(1u64, 0u64);
    }

    #[test]
    fn uint64_checked_multiply_ratio_does_not_panic() {
        assert_eq!(
            Uint64(500u64).checked_multiply_ratio(1u64, 0u64),
            Err(CheckedMultiplyRatioError::DivideByZero),
        );
        assert_eq!(
            Uint64(500u64).checked_multiply_ratio(u64::MAX, 1u64),
            Err(CheckedMultiplyRatioError::Overflow),
        );
    }

    #[test]
    fn sum_works() {
        let nums = vec![Uint64(17), Uint64(123), Uint64(540), Uint64(82)];
        let expected = Uint64(762);

        let sum_as_ref: Uint64 = nums.iter().sum();
        assert_eq!(expected, sum_as_ref);

        let sum_as_owned: Uint64 = nums.into_iter().sum();
        assert_eq!(expected, sum_as_owned);
    }

    #[test]
    fn uint64_methods() {
        // checked_*
        assert!(matches!(
            Uint64::MAX.checked_add(Uint64(1)),
            Err(OverflowError { .. })
        ));
        assert!(matches!(Uint64(1).checked_add(Uint64(1)), Ok(Uint64(2))));
        assert!(matches!(
            Uint64(0).checked_sub(Uint64(1)),
            Err(OverflowError { .. })
        ));
        assert!(matches!(Uint64(2).checked_sub(Uint64(1)), Ok(Uint64(1))));
        assert!(matches!(
            Uint64::MAX.checked_mul(Uint64(2)),
            Err(OverflowError { .. })
        ));
        assert!(matches!(Uint64(2).checked_mul(Uint64(2)), Ok(Uint64(4))));
        assert!(matches!(
            Uint64::MAX.checked_pow(2u32),
            Err(OverflowError { .. })
        ));
        assert!(matches!(Uint64(2).checked_pow(3), Ok(Uint64(8))));
        assert!(matches!(
            Uint64::MAX.checked_div(Uint64(0)),
            Err(DivideByZeroError { .. })
        ));
        assert!(matches!(Uint64(6).checked_div(Uint64(2)), Ok(Uint64(3))));
        assert!(matches!(
            Uint64::MAX.checked_div_euclid(Uint64(0)),
            Err(DivideByZeroError { .. })
        ));
        assert!(matches!(
            Uint64(6).checked_div_euclid(Uint64(2)),
            Ok(Uint64(3)),
        ));
        assert!(matches!(
            Uint64::MAX.checked_rem(Uint64(0)),
            Err(DivideByZeroError { .. })
        ));
        assert!(matches!(Uint64(7).checked_rem(Uint64(2)), Ok(Uint64(1))));

        // saturating_*
        assert_eq!(Uint64::MAX.saturating_add(Uint64(1)), Uint64::MAX);
        assert_eq!(Uint64(0).saturating_sub(Uint64(1)), Uint64(0));
        assert_eq!(Uint64::MAX.saturating_mul(Uint64(2)), Uint64::MAX);
        assert_eq!(Uint64::MAX.saturating_pow(2), Uint64::MAX);
    }

    #[test]
    fn uint64_wrapping_methods() {
        // wrapping_add
        assert_eq!(Uint64(2).wrapping_add(Uint64(2)), Uint64(4)); // non-wrapping
        assert_eq!(Uint64::MAX.wrapping_add(Uint64(1)), Uint64(0)); // wrapping

        // wrapping_sub
        assert_eq!(Uint64(7).wrapping_sub(Uint64(5)), Uint64(2)); // non-wrapping
        assert_eq!(Uint64(0).wrapping_sub(Uint64(1)), Uint64::MAX); // wrapping

        // wrapping_mul
        assert_eq!(Uint64(3).wrapping_mul(Uint64(2)), Uint64(6)); // non-wrapping
        assert_eq!(
            Uint64::MAX.wrapping_mul(Uint64(2)),
            Uint64::MAX - Uint64::one()
        ); // wrapping

        // wrapping_pow
        assert_eq!(Uint64(2).wrapping_pow(3), Uint64(8)); // non-wrapping
        assert_eq!(Uint64::MAX.wrapping_pow(2), Uint64(1)); // wrapping
    }

    #[test]
    #[allow(clippy::op_ref)]
    fn uint64_implements_rem() {
        let a = Uint64::new(10);
        assert_eq!(a % Uint64::new(10), Uint64::zero());
        assert_eq!(a % Uint64::new(2), Uint64::zero());
        assert_eq!(a % Uint64::new(1), Uint64::zero());
        assert_eq!(a % Uint64::new(3), Uint64::new(1));
        assert_eq!(a % Uint64::new(4), Uint64::new(2));

        // works for refs
        let a = Uint64::new(10);
        let b = Uint64::new(3);
        let expected = Uint64::new(1);
        assert_eq!(a % b, expected);
        assert_eq!(a % &b, expected);
        assert_eq!(&a % b, expected);
        assert_eq!(&a % &b, expected);
    }

    #[test]
    #[should_panic(expected = "divisor of zero")]
    fn uint64_rem_panics_for_zero() {
        let _ = Uint64::new(10) % Uint64::zero();
    }

    #[test]
    #[allow(clippy::op_ref)]
    fn uint64_rem_works() {
        assert_eq!(
            Uint64::from(12u32) % Uint64::from(10u32),
            Uint64::from(2u32)
        );
        assert_eq!(Uint64::from(50u32) % Uint64::from(5u32), Uint64::zero());

        // works for refs
        let a = Uint64::from(42u32);
        let b = Uint64::from(5u32);
        let expected = Uint64::from(2u32);
        assert_eq!(a % b, expected);
        assert_eq!(a % &b, expected);
        assert_eq!(&a % b, expected);
        assert_eq!(&a % &b, expected);
    }

    #[test]
    fn uint64_rem_assign_works() {
        let mut a = Uint64::from(30u32);
        a %= Uint64::from(4u32);
        assert_eq!(a, Uint64::from(2u32));

        // works for refs
        let mut a = Uint64::from(25u32);
        let b = Uint64::from(6u32);
        a %= &b;
        assert_eq!(a, Uint64::from(1u32));
    }

    #[test]
    fn uint64_abs_diff_works() {
        let a = Uint64::from(42u32);
        let b = Uint64::from(5u32);
        let expected = Uint64::from(37u32);
        assert_eq!(a.abs_diff(b), expected);
        assert_eq!(b.abs_diff(a), expected);
    }

    #[test]
    fn uint64_partial_eq() {
        let test_cases = [(1, 1, true), (42, 42, true), (42, 24, false), (0, 0, true)]
            .into_iter()
            .map(|(lhs, rhs, expected)| (Uint64::new(lhs), Uint64::new(rhs), expected));

        #[allow(clippy::op_ref)]
        for (lhs, rhs, expected) in test_cases {
            assert_eq!(lhs == rhs, expected);
            assert_eq!(&lhs == rhs, expected);
            assert_eq!(lhs == &rhs, expected);
            assert_eq!(&lhs == &rhs, expected);
        }
    }
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
pub fn install_panic_handler() {
    use super::imports::handle_panic;
    std::panic::set_hook(Box::new(|info| {
        // E.g. "panicked at 'oh no (a = 3)', src/contract.rs:51:5"
        let full_message = info.to_string();
        handle_panic(&full_message);
    }));
}
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
    fn from(full: FullDelegation) -> Self {
        Delegation {
            delegator: full.delegator,
            validator: full.validator,
            amount: full.amount,
        }
    }
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
    pub fn new(code_id: u64, creator: impl Into<String>) -> Self {
        Self {
            code_id,
            creator: creator.into(),
            admin: None,
            pinned: false,
            ibc_port: None,
        }
    }
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
    fn from(msg: BankQuery) -> Self {
        QueryRequest::Bank(msg)
    }
}

impl<C: CustomQuery> From<C> for QueryRequest<C> {
    fn from(msg: C) -> Self {
        QueryRequest::Custom(msg)
    }
}

#[cfg(feature = "staking")]
impl<C: CustomQuery> From<StakingQuery> for QueryRequest<C> {
    fn from(msg: StakingQuery) -> Self {
        QueryRequest::Staking(msg)
    }
}

impl<C: CustomQuery> From<WasmQuery> for QueryRequest<C> {
    fn from(msg: WasmQuery) -> Self {
        QueryRequest::Wasm(msg)
    }
}

#[cfg(feature = "stargate")]
impl<C: CustomQuery> From<IbcQuery> for QueryRequest<C> {
    fn from(msg: IbcQuery) -> Self {
        QueryRequest::Ibc(msg)
    }
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
    pub fn into_result(self) -> Result<S, String> {
        Result::<S, String>::from(self)
    }

    pub fn unwrap(self) -> S {
        self.into_result().unwrap()
    }

    pub fn is_ok(&self) -> bool {
        matches!(self, ContractResult::Ok(_))
    }

    pub fn is_err(&self) -> bool {
        matches!(self, ContractResult::Err(_))
    }
}

impl<S: fmt::Debug> ContractResult<S> {
    pub fn unwrap_err(self) -> String {
        self.into_result().unwrap_err()
    }
}

impl<S, E: ToString> From<Result<S, E>> for ContractResult<S> {
    fn from(original: Result<S, E>) -> ContractResult<S> {
        match original {
            Ok(value) => ContractResult::Ok(value),
            Err(err) => ContractResult::Err(err.to_string()),
        }
    }
}

impl<S> From<ContractResult<S>> for Result<S, String> {
    fn from(original: ContractResult<S>) -> Result<S, String> {
        match original {
            ContractResult::Ok(value) => Ok(value),
            ContractResult::Err(err) => Err(err),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{from_slice, to_vec, Response, StdError, StdResult};

    #[test]
    fn contract_result_serialization_works() {
        let result = ContractResult::Ok(12);
        assert_eq!(&to_vec(&result).unwrap(), b"{\"ok\":12}");

        let result = ContractResult::Ok("foo");
        assert_eq!(&to_vec(&result).unwrap(), b"{\"ok\":\"foo\"}");

        let result: ContractResult<Response> = ContractResult::Ok(Response::default());
        assert_eq!(
            to_vec(&result).unwrap(),
            br#"{"ok":{"messages":[],"attributes":[],"events":[],"data":null}}"#
        );

        let result: ContractResult<Response> = ContractResult::Err("broken".to_string());
        assert_eq!(&to_vec(&result).unwrap(), b"{\"error\":\"broken\"}");
    }

    #[test]
    fn contract_result_deserialization_works() {
        let result: ContractResult<u64> = from_slice(br#"{"ok":12}"#).unwrap();
        assert_eq!(result, ContractResult::Ok(12));

        let result: ContractResult<String> = from_slice(br#"{"ok":"foo"}"#).unwrap();
        assert_eq!(result, ContractResult::Ok("foo".to_string()));

        let result: ContractResult<Response> =
            from_slice(br#"{"ok":{"messages":[],"attributes":[],"events":[],"data":null}}"#)
                .unwrap();
        assert_eq!(result, ContractResult::Ok(Response::default()));

        let result: ContractResult<Response> = from_slice(br#"{"error":"broken"}"#).unwrap();
        assert_eq!(result, ContractResult::Err("broken".to_string()));

        // ignores whitespace
        let result: ContractResult<u64> = from_slice(b" {\n\t  \"ok\": 5898\n}  ").unwrap();
        assert_eq!(result, ContractResult::Ok(5898));

        // fails for additional attributes
        let parse: StdResult<ContractResult<u64>> = from_slice(br#"{"unrelated":321,"ok":4554}"#);
        match parse.unwrap_err() {
            StdError::ParseErr { .. } => {}
            err => panic!("Unexpected error: {:?}", err),
        }
        let parse: StdResult<ContractResult<u64>> = from_slice(br#"{"ok":4554,"unrelated":321}"#);
        match parse.unwrap_err() {
            StdError::ParseErr { .. } => {}
            err => panic!("Unexpected error: {:?}", err),
        }
        let parse: StdResult<ContractResult<u64>> =
            from_slice(br#"{"ok":4554,"error":"What's up now?"}"#);
        match parse.unwrap_err() {
            StdError::ParseErr { .. } => {}
            err => panic!("Unexpected error: {:?}", err),
        }
    }

    #[test]
    fn can_convert_from_core_result() {
        let original: Result<Response, StdError> = Ok(Response::default());
        let converted: ContractResult<Response> = original.into();
        assert_eq!(converted, ContractResult::Ok(Response::default()));

        let original: Result<Response, StdError> = Err(StdError::generic_err("broken"));
        let converted: ContractResult<Response> = original.into();
        assert_eq!(
            converted,
            ContractResult::Err("Generic error: broken".to_string())
        );
    }

    #[test]
    fn can_convert_to_core_result() {
        let original = ContractResult::Ok(Response::default());
        let converted: Result<Response, String> = original.into();
        assert_eq!(converted, Ok(Response::default()));

        let original = ContractResult::Err("went wrong".to_string());
        let converted: Result<Response, String> = original.into();
        assert_eq!(converted, Err("went wrong".to_string()));
    }
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

fn binary_to_string(data: &Binary, fmt: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
    match std::str::from_utf8(data.as_slice()) {
        Ok(s) => fmt.write_str(s),
        Err(_) => write!(fmt, "{:?}", data),
    }
}

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
) -> StdResult<WasmMsg> {
    let payload = to_binary(msg)?;
    Ok(WasmMsg::Instantiate {
        admin: None,
        code_id,
        msg: payload,
        funds,
        label,
    })
}

/// Shortcut helper as the construction of WasmMsg::Instantiate can be quite verbose in contract code
pub fn wasm_execute(
    contract_addr: impl Into<String>,
    msg: &impl Serialize,
    funds: Vec<Coin>,
) -> StdResult<WasmMsg> {
    let payload = to_binary(msg)?;
    Ok(WasmMsg::Execute {
        contract_addr: contract_addr.into(),
        msg: payload,
        funds,
    })
}

impl<T> From<BankMsg> for CosmosMsg<T> {
    fn from(msg: BankMsg) -> Self {
        CosmosMsg::Bank(msg)
    }
}

#[cfg(feature = "staking")]
impl<T> From<StakingMsg> for CosmosMsg<T> {
    fn from(msg: StakingMsg) -> Self {
        CosmosMsg::Staking(msg)
    }
}

#[cfg(feature = "staking")]
impl<T> From<DistributionMsg> for CosmosMsg<T> {
    fn from(msg: DistributionMsg) -> Self {
        CosmosMsg::Distribution(msg)
    }
}

impl<T> From<WasmMsg> for CosmosMsg<T> {
    fn from(msg: WasmMsg) -> Self {
        CosmosMsg::Wasm(msg)
    }
}

#[cfg(feature = "stargate")]
impl<T> From<IbcMsg> for CosmosMsg<T> {
    fn from(msg: IbcMsg) -> Self {
        CosmosMsg::Ibc(msg)
    }
}

#[cfg(feature = "stargate")]
impl<T> From<GovMsg> for CosmosMsg<T> {
    fn from(msg: GovMsg) -> Self {
        CosmosMsg::Gov(msg)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{coin, coins};

    #[test]
    fn from_bank_msg_works() {
        let to_address = String::from("you");
        let amount = coins(1015, "earth");
        let bank = BankMsg::Send { to_address, amount };
        let msg: CosmosMsg = bank.clone().into();
        match msg {
            CosmosMsg::Bank(msg) => assert_eq!(bank, msg),
            _ => panic!("must encode in Bank variant"),
        }
    }

    #[cosmwasm_schema::cw_serde]
    enum ExecuteMsg {
        Mint { coin: Coin },
    }

    #[test]
    fn wasm_msg_debug_decodes_binary_string_when_possible() {
        let msg = WasmMsg::Execute {
            contract_addr: "joe".to_string(),
            msg: to_binary(&ExecuteMsg::Mint {
                coin: coin(10, "BTC"),
            })
            .unwrap(),
            funds: vec![],
        };

        assert_eq!(
            format!("{:?}", msg),
            "Execute { contract_addr: \"joe\", msg: {\"mint\":{\"coin\":{\"denom\":\"BTC\",\"amount\":\"10\"}}}, funds: [] }"
        );
    }

    #[test]
    fn wasm_msg_debug_dumps_binary_when_not_utf8() {
        let msg = WasmMsg::Execute {
            contract_addr: "joe".to_string(),
            msg: Binary::from([0, 159, 146, 150]),
            funds: vec![],
        };

        assert_eq!(
            format!("{:?}", msg),
            "Execute { contract_addr: \"joe\", msg: Binary(009f9296), funds: [] }"
        );
    }
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
    fn empty_can_be_instantiated() {
        let instance = Empty::default();
        assert_eq!(instance, Empty {});
    }

    #[test]
    fn empty_can_be_instantiated_serialized_and_deserialized() {
        let instance = Empty {};
        let serialized = to_vec(&instance).unwrap();
        assert_eq!(serialized, b"{}");

        let deserialized: Empty = from_slice(b"{}").unwrap();
        assert_eq!(deserialized, instance);

        let deserialized: Empty = from_slice(b"{\"stray\":\"data\"}").unwrap();
        assert_eq!(deserialized, instance);
    }
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
    pub fn new(ty: impl Into<String>) -> Self {
        Event {
            ty: ty.into(),
            attributes: Vec::with_capacity(10),
        }
    }

    /// Add an attribute to the event.
    pub fn add_attribute(mut self, key: impl Into<String>, value: impl Into<String>) -> Self {
        self.attributes.push(Attribute {
            key: key.into(),
            value: value.into(),
        });
        self
    }

    /// Bulk add attributes to the event.
    ///
    /// Anything that can be turned into an iterator and yields something
    /// that can be converted into an `Attribute` is accepted.
    pub fn add_attributes<A: Into<Attribute>>(
        mut self,
        attrs: impl IntoIterator<Item = A>,
    ) -> Self {
        self.attributes.extend(attrs.into_iter().map(A::into));
        self
    }
}

/// An key value pair that is used in the context of event attributes in logs
#[derive(Serialize, Deserialize, Clone, Default, Debug, PartialEq, Eq, JsonSchema)]
pub struct Attribute {
    pub key: String,
    pub value: String,
}

impl Attribute {
    /// Creates a new Attribute. `attr` is just an alias for this.
    pub fn new(key: impl Into<String>, value: impl Into<String>) -> Self {
        let key = key.into();

        #[cfg(debug_assertions)]
        if key.starts_with('_') {
            panic!(
                "attribute key `{}` is invalid - keys starting with an underscore are reserved",
                key
            );
        }

        Self {
            key,
            value: value.into(),
        }
    }
}

impl<K: Into<String>, V: Into<String>> From<(K, V)> for Attribute {
    fn from((k, v): (K, V)) -> Self {
        Attribute::new(k, v)
    }
}

impl<K: AsRef<str>, V: AsRef<str>> PartialEq<(K, V)> for Attribute {
    fn eq(&self, (k, v): &(K, V)) -> bool {
        (self.key.as_str(), self.value.as_str()) == (k.as_ref(), v.as_ref())
    }
}

impl<K: AsRef<str>, V: AsRef<str>> PartialEq<Attribute> for (K, V) {
    fn eq(&self, attr: &Attribute) -> bool {
        attr == self
    }
}

impl<K: AsRef<str>, V: AsRef<str>> PartialEq<(K, V)> for &Attribute {
    fn eq(&self, (k, v): &(K, V)) -> bool {
        (self.key.as_str(), self.value.as_str()) == (k.as_ref(), v.as_ref())
    }
}

impl<K: AsRef<str>, V: AsRef<str>> PartialEq<&Attribute> for (K, V) {
    fn eq(&self, attr: &&Attribute) -> bool {
        attr == self
    }
}

impl PartialEq<Attribute> for &Attribute {
    fn eq(&self, rhs: &Attribute) -> bool {
        *self == rhs
    }
}

impl PartialEq<&Attribute> for Attribute {
    fn eq(&self, rhs: &&Attribute) -> bool {
        self == *rhs
    }
}

/// Creates a new Attribute. `Attribute::new` is an alias for this.
#[inline]
pub fn attr(key: impl Into<String>, value: impl Into<String>) -> Attribute {
    Attribute::new(key, value)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Uint128;

    #[test]
    fn event_construction() {
        let event_direct = Event {
            ty: "test".to_string(),
            attributes: vec![attr("foo", "bar"), attr("bar", "baz")],
        };
        let event_builder = Event::new("test").add_attributes(vec![("foo", "bar"), ("bar", "baz")]);

        assert_eq!(event_direct, event_builder);
    }

    #[test]
    #[should_panic]
    fn attribute_new_reserved_key_panicks() {
        Attribute::new("_invalid", "value");
    }

    #[test]
    #[should_panic]
    fn attribute_new_reserved_key_panicks2() {
        Attribute::new("_", "value");
    }

    #[test]
    fn attr_works_for_different_types() {
        let expected = ("foo", "42");

        assert_eq!(attr("foo", "42"), expected);
        assert_eq!(attr("foo", "42"), expected);
        assert_eq!(attr("foo", "42"), expected);
        assert_eq!(attr("foo", Uint128::new(42)), expected);
    }
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
/// ) -> StdResult<Response> {
///     // ...
///
///     Ok(Response::new().add_attribute("action", "instantiate"))
/// }
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
/// ) -> Result<Response, MyError> {
///     let mut response = Response::new()
///         .add_attribute("Let the", "hacking begin")
///         .add_message(BankMsg::Send {
///             to_address: String::from("recipient"),
///             amount: coins(128, "uint"),
///         })
///         .add_attribute("foo", "bar")
///         .set_data(b"the result data");
///     Ok(response)
/// }
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
    fn default() -> Self {
        Response {
            messages: vec![],
            attributes: vec![],
            events: vec![],
            data: None,
        }
    }
}

impl<T> Response<T> {
    pub fn new() -> Self {
        Self::default()
    }

    /// Add an attribute included in the main `wasm` event.
    ///
    /// For working with optional values or optional attributes, see [`add_attributes`][Self::add_attributes].
    pub fn add_attribute(mut self, key: impl Into<String>, value: impl Into<String>) -> Self {
        self.attributes.push(Attribute::new(key, value));
        self
    }

    /// This creates a "fire and forget" message, by using `SubMsg::new()` to wrap it,
    /// and adds it to the list of messages to process.
    pub fn add_message(mut self, msg: impl Into<CosmosMsg<T>>) -> Self {
        self.messages.push(SubMsg::new(msg));
        self
    }

    /// This takes an explicit SubMsg (creates via eg. `reply_on_error`)
    /// and adds it to the list of messages to process.
    pub fn add_submessage(mut self, msg: SubMsg<T>) -> Self {
        self.messages.push(msg);
        self
    }

    /// Adds an extra event to the response, separate from the main `wasm` event
    /// that is always created.
    ///
    /// The `wasm-` prefix will be appended by the runtime to the provided type
    /// of event.
    pub fn add_event(mut self, event: Event) -> Self {
        self.events.push(event);
        self
    }

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
    ) -> Self {
        self.attributes.extend(attrs.into_iter().map(A::into));
        self
    }

    /// Bulk add "fire and forget" messages to the list of messages to process.
    ///
    /// ## Examples
    ///
    /// ```
    /// use cosmwasm_std::{CosmosMsg, Response};
    ///
    /// fn make_response_with_msgs(msgs: Vec<CosmosMsg>) -> Response {
    ///     Response::new().add_messages(msgs)
    /// }
    /// ```
    pub fn add_messages<M: Into<CosmosMsg<T>>>(self, msgs: impl IntoIterator<Item = M>) -> Self {
        self.add_submessages(msgs.into_iter().map(SubMsg::new))
    }

    /// Bulk add explicit SubMsg structs to the list of messages to process.
    ///
    /// ## Examples
    ///
    /// ```
    /// use cosmwasm_std::{SubMsg, Response};
    ///
    /// fn make_response_with_submsgs(msgs: Vec<SubMsg>) -> Response {
    ///     Response::new().add_submessages(msgs)
    /// }
    /// ```
    pub fn add_submessages(mut self, msgs: impl IntoIterator<Item = SubMsg<T>>) -> Self {
        self.messages.extend(msgs.into_iter());
        self
    }

    /// Bulk add custom events to the response. These are separate from the main
    /// `wasm` event.
    ///
    /// The `wasm-` prefix will be appended by the runtime to the provided types
    /// of events.
    pub fn add_events(mut self, events: impl IntoIterator<Item = Event>) -> Self {
        self.events.extend(events.into_iter());
        self
    }

    /// Set the binary data included in the response.
    pub fn set_data(mut self, data: impl Into<Binary>) -> Self {
        self.data = Some(data.into());
        self
    }
}

#[cfg(test)]
mod tests {
    use super::super::BankMsg;
    use super::*;
    use crate::results::submessages::{ReplyOn, UNUSED_MSG_ID};
    use crate::{coins, from_slice, to_vec, ContractResult};

    #[test]
    fn response_add_attributes_works() {
        let res = Response::<Empty>::new().add_attributes(std::iter::empty::<Attribute>());
        assert_eq!(res.attributes.len(), 0);

        let res = Response::<Empty>::new().add_attributes([Attribute::new("test", "ing")]);
        assert_eq!(res.attributes.len(), 1);
        assert_eq!(
            res.attributes[0],
            Attribute {
                key: "test".to_string(),
                value: "ing".to_string(),
            }
        );

        let attrs = vec![
            ("action", "reaction"),
            ("answer", "42"),
            ("another", "attribute"),
        ];
        let res: Response = Response::new().add_attributes(attrs.clone());
        assert_eq!(res.attributes, attrs);

        let optional = Option::<Attribute>::None;
        let res: Response = Response::new().add_attributes(optional.into_iter());
        assert_eq!(res.attributes.len(), 0);

        let optional = Option::<Attribute>::Some(Attribute::new("test", "ing"));
        let res: Response = Response::new().add_attributes(optional.into_iter());
        assert_eq!(res.attributes.len(), 1);
        assert_eq!(
            res.attributes[0],
            Attribute {
                key: "test".to_string(),
                value: "ing".to_string(),
            }
        );
    }

    #[test]
    fn can_serialize_and_deserialize_init_response() {
        let original = Response {
            messages: vec![
                SubMsg {
                    id: 12,
                    msg: BankMsg::Send {
                        to_address: String::from("checker"),
                        amount: coins(888, "moon"),
                    }
                    .into(),
                    gas_limit: Some(12345u64),
                    reply_on: ReplyOn::Always,
                },
                SubMsg {
                    id: UNUSED_MSG_ID,
                    msg: BankMsg::Send {
                        to_address: String::from("you"),
                        amount: coins(1015, "earth"),
                    }
                    .into(),
                    gas_limit: None,
                    reply_on: ReplyOn::Never,
                },
            ],
            attributes: vec![Attribute {
                key: "action".to_string(),
                value: "release".to_string(),
            }],
            events: vec![],
            data: Some(Binary::from([0xAA, 0xBB])),
        };
        let serialized = to_vec(&original).expect("encode contract result");
        let deserialized: Response = from_slice(&serialized).expect("decode contract result");
        assert_eq!(deserialized, original);
    }

    #[test]
    fn contract_result_is_ok_works() {
        let success = ContractResult::<()>::Ok(());
        let failure = ContractResult::<()>::Err("broken".to_string());
        assert!(success.is_ok());
        assert!(!failure.is_ok());
    }

    #[test]
    fn contract_result_is_err_works() {
        let success = ContractResult::<()>::Ok(());
        let failure = ContractResult::<()>::Err("broken".to_string());
        assert!(failure.is_err());
        assert!(!success.is_err());
    }
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
    pub fn new(msg: impl Into<CosmosMsg<T>>) -> Self {
        SubMsg {
            id: UNUSED_MSG_ID,
            msg: msg.into(),
            reply_on: ReplyOn::Never,
            gas_limit: None,
        }
    }

    /// create a `SubMsg` that will provide a `reply` with the given id if the message returns `Ok`
    pub fn reply_on_success(msg: impl Into<CosmosMsg<T>>, id: u64) -> Self {
        Self::reply_on(msg.into(), id, ReplyOn::Success)
    }

    /// create a `SubMsg` that will provide a `reply` with the given id if the message returns `Err`
    pub fn reply_on_error(msg: impl Into<CosmosMsg<T>>, id: u64) -> Self {
        Self::reply_on(msg.into(), id, ReplyOn::Error)
    }

    /// create a `SubMsg` that will always provide a `reply` with the given id
    pub fn reply_always(msg: impl Into<CosmosMsg<T>>, id: u64) -> Self {
        Self::reply_on(msg.into(), id, ReplyOn::Always)
    }

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
    pub fn with_gas_limit(mut self, limit: u64) -> Self {
        self.gas_limit = Some(limit);
        self
    }

    fn reply_on(msg: CosmosMsg<T>, id: u64, reply_on: ReplyOn) -> Self {
        SubMsg {
            id,
            msg,
            reply_on,
            gas_limit: None,
        }
    }
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
    pub fn into_result(self) -> Result<SubMsgResponse, String> {
        Result::<SubMsgResponse, String>::from(self)
    }

    pub fn unwrap(self) -> SubMsgResponse {
        self.into_result().unwrap()
    }

    pub fn unwrap_err(self) -> String {
        self.into_result().unwrap_err()
    }

    pub fn is_ok(&self) -> bool {
        matches!(self, SubMsgResult::Ok(_))
    }

    pub fn is_err(&self) -> bool {
        matches!(self, SubMsgResult::Err(_))
    }
}

impl<E: ToString> From<Result<SubMsgResponse, E>> for SubMsgResult {
    fn from(original: Result<SubMsgResponse, E>) -> SubMsgResult {
        match original {
            Ok(value) => SubMsgResult::Ok(value),
            Err(err) => SubMsgResult::Err(err.to_string()),
        }
    }
}

impl From<SubMsgResult> for Result<SubMsgResponse, String> {
    fn from(original: SubMsgResult) -> Result<SubMsgResponse, String> {
        match original {
            SubMsgResult::Ok(value) => Ok(value),
            SubMsgResult::Err(err) => Err(err),
        }
    }
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
    fn sub_msg_result_serialization_works() {
        let result = SubMsgResult::Ok(SubMsgResponse {
            data: None,
            events: vec![],
        });
        assert_eq!(
            &to_vec(&result).unwrap(),
            br#"{"ok":{"events":[],"data":null}}"#
        );

        let result = SubMsgResult::Ok(SubMsgResponse {
            data: Some(Binary::from_base64("MTIzCg==").unwrap()),
            events: vec![Event::new("wasm").add_attribute("fo", "ba")],
        });
        assert_eq!(
            &to_vec(&result).unwrap(),
            br#"{"ok":{"events":[{"type":"wasm","attributes":[{"key":"fo","value":"ba"}]}],"data":"MTIzCg=="}}"#
        );

        let result: SubMsgResult = SubMsgResult::Err("broken".to_string());
        assert_eq!(&to_vec(&result).unwrap(), b"{\"error\":\"broken\"}");
    }

    #[test]
    fn sub_msg_result_deserialization_works() {
        let result: SubMsgResult = from_slice(br#"{"ok":{"events":[],"data":null}}"#).unwrap();
        assert_eq!(
            result,
            SubMsgResult::Ok(SubMsgResponse {
                events: vec![],
                data: None,
            })
        );

        let result: SubMsgResult = from_slice(
            br#"{"ok":{"events":[{"type":"wasm","attributes":[{"key":"fo","value":"ba"}]}],"data":"MTIzCg=="}}"#).unwrap();
        assert_eq!(
            result,
            SubMsgResult::Ok(SubMsgResponse {
                data: Some(Binary::from_base64("MTIzCg==").unwrap()),
                events: vec![Event::new("wasm").add_attribute("fo", "ba")],
            })
        );

        let result: SubMsgResult = from_slice(br#"{"error":"broken"}"#).unwrap();
        assert_eq!(result, SubMsgResult::Err("broken".to_string()));

        // fails for additional attributes
        let parse: StdResult<SubMsgResult> = from_slice(br#"{"unrelated":321,"error":"broken"}"#);
        match parse.unwrap_err() {
            StdError::ParseErr { .. } => {}
            err => panic!("Unexpected error: {:?}", err),
        }
        let parse: StdResult<SubMsgResult> = from_slice(br#"{"error":"broken","unrelated":321}"#);
        match parse.unwrap_err() {
            StdError::ParseErr { .. } => {}
            err => panic!("Unexpected error: {:?}", err),
        }
    }

    #[test]
    fn sub_msg_result_unwrap_works() {
        let response = SubMsgResponse {
            data: Some(Binary::from_base64("MTIzCg==").unwrap()),
            events: vec![Event::new("wasm").add_attribute("fo", "ba")],
        };
        let success = SubMsgResult::Ok(response.clone());
        assert_eq!(success.unwrap(), response);
    }

    #[test]
    #[should_panic]
    fn sub_msg_result_unwrap_panicks_for_err() {
        let failure = SubMsgResult::Err("broken".to_string());
        let _ = failure.unwrap();
    }

    #[test]
    fn sub_msg_result_unwrap_err_works() {
        let failure = SubMsgResult::Err("broken".to_string());
        assert_eq!(failure.unwrap_err(), "broken");
    }

    #[test]
    #[should_panic]
    fn sub_msg_result_unwrap_err_panics_for_ok() {
        let response = SubMsgResponse {
            data: Some(Binary::from_base64("MTIzCg==").unwrap()),
            events: vec![Event::new("wasm").add_attribute("fo", "ba")],
        };
        let success = SubMsgResult::Ok(response);
        let _ = success.unwrap_err();
    }

    #[test]
    fn sub_msg_result_is_ok_works() {
        let success = SubMsgResult::Ok(SubMsgResponse {
            data: Some(Binary::from_base64("MTIzCg==").unwrap()),
            events: vec![Event::new("wasm").add_attribute("fo", "ba")],
        });
        let failure = SubMsgResult::Err("broken".to_string());
        assert!(success.is_ok());
        assert!(!failure.is_ok());
    }

    #[test]
    fn sub_msg_result_is_err_works() {
        let success = SubMsgResult::Ok(SubMsgResponse {
            data: Some(Binary::from_base64("MTIzCg==").unwrap()),
            events: vec![Event::new("wasm").add_attribute("fo", "ba")],
        });
        let failure = SubMsgResult::Err("broken".to_string());
        assert!(failure.is_err());
        assert!(!success.is_err());
    }

    #[test]
    fn sub_msg_result_can_convert_from_core_result() {
        let original: Result<SubMsgResponse, StdError> = Ok(SubMsgResponse {
            data: Some(Binary::from_base64("MTIzCg==").unwrap()),
            events: vec![],
        });
        let converted: SubMsgResult = original.into();
        assert_eq!(
            converted,
            SubMsgResult::Ok(SubMsgResponse {
                data: Some(Binary::from_base64("MTIzCg==").unwrap()),
                events: vec![],
            })
        );

        let original: Result<SubMsgResponse, StdError> = Err(StdError::generic_err("broken"));
        let converted: SubMsgResult = original.into();
        assert_eq!(
            converted,
            SubMsgResult::Err("Generic error: broken".to_string())
        );
    }

    #[test]
    fn sub_msg_result_can_convert_to_core_result() {
        let original = SubMsgResult::Ok(SubMsgResponse {
            data: Some(Binary::from_base64("MTIzCg==").unwrap()),
            events: vec![],
        });
        let converted: Result<SubMsgResponse, String> = original.into();
        assert_eq!(
            converted,
            Ok(SubMsgResponse {
                data: Some(Binary::from_base64("MTIzCg==").unwrap()),
                events: vec![],
            })
        );

        let original = SubMsgResult::Err("went wrong".to_string());
        let converted: Result<SubMsgResponse, String> = original.into();
        assert_eq!(converted, Err("went wrong".to_string()));
    }
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
    pub fn into_result(self) -> Result<S, SystemError> {
        Result::<S, SystemError>::from(self)
    }

    pub fn unwrap(self) -> S {
        self.into_result().unwrap()
    }
}

impl<S: fmt::Debug> SystemResult<S> {
    pub fn unwrap_err(self) -> SystemError {
        self.into_result().unwrap_err()
    }
}

impl<S> From<Result<S, SystemError>> for SystemResult<S> {
    fn from(original: Result<S, SystemError>) -> SystemResult<S> {
        match original {
            Ok(value) => SystemResult::Ok(value),
            Err(err) => SystemResult::Err(err),
        }
    }
}

impl<S> From<SystemResult<S>> for Result<S, SystemError> {
    fn from(original: SystemResult<S>) -> Result<S, SystemError> {
        match original {
            SystemResult::Ok(value) => Ok(value),
            SystemResult::Err(err) => Err(err),
        }
    }
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
pub fn decode_sections2(data: Vec<u8>) -> (Vec<u8>, Vec<u8>) {
    let (rest, second) = split_tail(data);
    let (_, first) = split_tail(rest);
    (first, second)
}

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
pub fn encode_sections(sections: &[&[u8]]) -> Vec<u8> {
    let mut out_len: usize = sections.iter().map(|section| section.len()).sum();
    out_len += 4 * sections.len();
    let mut out_data = Vec::with_capacity(out_len);
    for &section in sections {
        let section_len = force_to_u32(section.len()).to_be_bytes();
        out_data.extend(section);
        out_data.extend_from_slice(&section_len);
    }
    debug_assert_eq!(out_data.len(), out_len);
    debug_assert_eq!(out_data.capacity(), out_len);
    out_data
}

/// Splits data into the last section ("tail") and the rest.
/// The tail's length information is cut off, such that it is ready to use.
/// The rest is basically unparsed and contails the lengths of the remaining sections.
///
/// While the tail is copied into a new vector, the rest is only truncated such that
/// no re-allocation is necessary.
///
/// If `data` contains one section only, `data` is moved into the tail entirely
fn split_tail(data: Vec<u8>) -> (Vec<u8>, Vec<u8>) {
    let tail_len: usize = if data.len() >= 4 {
        u32::from_be_bytes([
            data[data.len() - 4],
            data[data.len() - 3],
            data[data.len() - 2],
            data[data.len() - 1],
        ]) as usize
    } else {
        panic!("Cannot read section length");
    };
    let rest_len_end = data.len() - 4 - tail_len;

    let (rest, mut tail) = if rest_len_end == 0 {
        // i.e. all data is the tail
        (Vec::new(), data)
    } else {
        let mut rest = data;
        let tail = rest.split_off(rest_len_end);
        (rest, tail)
    };
    tail.truncate(tail_len); // cut off length
    (rest, tail)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn decode_sections2_works() {
        let data = b"\xAA\0\0\0\x01\xBB\xCC\0\0\0\x02".to_vec();
        assert_eq!(decode_sections2(data), (vec![0xAA], vec![0xBB, 0xCC]));

        let data = b"\xDE\xEF\x62\0\0\0\x03\0\0\0\0".to_vec();
        assert_eq!(decode_sections2(data), (vec![0xDE, 0xEF, 0x62], vec![]));

        let data = b"\0\0\0\0\xDE\xEF\x62\0\0\0\x03".to_vec();
        assert_eq!(decode_sections2(data), (vec![], vec![0xDE, 0xEF, 0x62]));

        let data = b"\0\0\0\0\0\0\0\0".to_vec();
        assert_eq!(decode_sections2(data), (vec![], vec![]));

        let data = b"\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\0\0\0\x13\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\0\0\x01\x15".to_vec();
        assert_eq!(decode_sections2(data), (vec![0xFF; 19], vec![0x9D; 277]));
    }

    #[test]
    fn decode_sections2_preserved_first_vector() {
        let original = b"\xAA\0\0\0\x01\xBB\xCC\0\0\0\x02".to_vec();
        let original_capacity = original.capacity();
        let original_ptr = original.as_ptr();
        let (first, second) = decode_sections2(original);

        // This is not copied
        assert_eq!(first.capacity(), original_capacity);
        assert_eq!(first.as_ptr(), original_ptr);

        // This is a copy
        assert_ne!(second.capacity(), original_capacity);
        assert_ne!(second.as_ptr(), original_ptr);
    }

    #[test]
    fn encode_sections_works_for_empty_sections() {
        let enc = encode_sections(&[]);
        assert_eq!(enc, b"" as &[u8]);
        let enc = encode_sections(&[&[]]);
        assert_eq!(enc, b"\0\0\0\0" as &[u8]);
        let enc = encode_sections(&[&[], &[]]);
        assert_eq!(enc, b"\0\0\0\0\0\0\0\0" as &[u8]);
        let enc = encode_sections(&[&[], &[], &[]]);
        assert_eq!(enc, b"\0\0\0\0\0\0\0\0\0\0\0\0" as &[u8]);
    }

    #[test]
    fn encode_sections_works_for_one_element() {
        let enc = encode_sections(&[]);
        assert_eq!(enc, b"" as &[u8]);
        let enc = encode_sections(&[&[0xAA]]);
        assert_eq!(enc, b"\xAA\0\0\0\x01" as &[u8]);
        let enc = encode_sections(&[&[0xAA, 0xBB]]);
        assert_eq!(enc, b"\xAA\xBB\0\0\0\x02" as &[u8]);
        let enc = encode_sections(&[&[0x9D; 277]]);
        assert_eq!(enc, b"\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\x9D\0\0\x01\x15" as &[u8]);
    }

    #[test]
    fn encode_sections_works_for_multiple_elements() {
        let enc = encode_sections(&[&[0xAA]]);
        assert_eq!(enc, b"\xAA\0\0\0\x01" as &[u8]);
        let enc = encode_sections(&[&[0xAA], &[0xDE, 0xDE]]);
        assert_eq!(enc, b"\xAA\0\0\0\x01\xDE\xDE\0\0\0\x02" as &[u8]);
        let enc = encode_sections(&[&[0xAA], &[0xDE, 0xDE], &[]]);
        assert_eq!(enc, b"\xAA\0\0\0\x01\xDE\xDE\0\0\0\x02\0\0\0\0" as &[u8]);
        let enc = encode_sections(&[&[0xAA], &[0xDE, 0xDE], &[], &[0xFF; 19]]);
        assert_eq!(enc, b"\xAA\0\0\0\x01\xDE\xDE\0\0\0\x02\0\0\0\0\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\0\0\0\x13" as &[u8]);
    }
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

pub fn from_slice<T: DeserializeOwned>(value: &[u8]) -> StdResult<T> {
    serde_json_wasm::from_slice(value).map_err(|e| StdError::parse_err(type_name::<T>(), e))
}

pub fn from_binary<T: DeserializeOwned>(value: &Binary) -> StdResult<T> {
    from_slice(value.as_slice())
}

pub fn to_vec<T>(data: &T) -> StdResult<Vec<u8>>
where
    T: Serialize + ?Sized,
{
    serde_json_wasm::to_vec(data).map_err(|e| StdError::serialize_err(type_name::<T>(), e))
}

pub fn to_binary<T>(data: &T) -> StdResult<Binary>
where
    T: Serialize + ?Sized,
{
    to_vec(data).map(Binary)
}

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
    fn to_vec_works() {
        let msg = SomeMsg::Refund {};
        let serialized = to_vec(&msg).unwrap();
        assert_eq!(serialized, br#"{"refund":{}}"#);

        let msg = SomeMsg::ReleaseAll {
            image: "foo".to_string(),
            amount: 42,
            time: 9007199254740999, // Number.MAX_SAFE_INTEGER + 7
            karma: -17,
        };
        let serialized = String::from_utf8(to_vec(&msg).unwrap()).unwrap();
        assert_eq!(
            serialized,
            r#"{"release_all":{"image":"foo","amount":42,"time":9007199254740999,"karma":-17}}"#
        );
    }

    #[test]
    fn from_slice_works() {
        let deserialized: SomeMsg = from_slice(br#"{"refund":{}}"#).unwrap();
        assert_eq!(deserialized, SomeMsg::Refund {});

        let deserialized: SomeMsg = from_slice(
            br#"{"release_all":{"image":"foo","amount":42,"time":18446744073709551615,"karma":-17}}"#,
        )
        .unwrap();
        assert_eq!(
            deserialized,
            SomeMsg::ReleaseAll {
                image: "foo".to_string(),
                amount: 42,
                time: 18446744073709551615,
                karma: -17
            }
        );
    }

    #[test]
    fn from_slice_or_binary() {
        let msg = SomeMsg::Refund {};
        let serialized: Binary = to_binary(&msg).unwrap();

        let parse_binary: SomeMsg = from_binary(&serialized).unwrap();
        assert_eq!(parse_binary, msg);

        let parse_slice: SomeMsg = from_slice(&serialized).unwrap();
        assert_eq!(parse_slice, msg);
    }

    #[test]
    fn to_vec_works_for_special_chars() {
        let msg = SomeMsg::Cowsay {
            text: "foo\"bar\\\"bla".to_string(),
        };
        let serialized = String::from_utf8(to_vec(&msg).unwrap()).unwrap();
        assert_eq!(serialized, r#"{"cowsay":{"text":"foo\"bar\\\"bla"}}"#);
    }

    #[test]
    fn from_slice_works_for_special_chars() {
        let deserialized: SomeMsg =
            from_slice(br#"{"cowsay":{"text":"foo\"bar\\\"bla"}}"#).unwrap();
        assert_eq!(
            deserialized,
            SomeMsg::Cowsay {
                text: "foo\"bar\\\"bla".to_string(),
            }
        );
    }
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
    pub fn new() -> Self {
        MemoryStorage::default()
    }
}

impl Storage for MemoryStorage {
    fn get(&self, key: &[u8]) -> Option<Vec<u8>> {
        self.data.get(key).cloned()
    }

    fn set(&mut self, key: &[u8], value: &[u8]) {
        if value.is_empty() {
            panic!("TL;DR: Value must not be empty in Storage::set but in most cases you can use Storage::remove instead. Long story: Getting empty values from storage is not well supported at the moment. Some of our internal interfaces cannot differentiate between a non-existent key and an empty value. Right now, you cannot rely on the behaviour of empty values. To protect you from trouble later on, we stop here. Sorry for the inconvenience! We highly welcome you to contribute to CosmWasm, making this more solid one way or the other.");
        }

        self.data.insert(key.to_vec(), value.to_vec());
    }

    fn remove(&mut self, key: &[u8]) {
        self.data.remove(key);
    }

    #[cfg(feature = "iterator")]
    /// range allows iteration over a set of keys, either forwards or backwards
    /// uses standard rust range notation, and eg db.range(b"foo"..b"bar") also works reverse
    fn range<'a>(
        &'a self,
        start: Option<&[u8]>,
        end: Option<&[u8]>,
        order: Order,
    ) -> Box<dyn Iterator<Item = Record> + 'a> {
        let bounds = range_bounds(start, end);

        // BTreeMap.range panics if range is start > end.
        // However, this cases represent just empty range and we treat it as such.
        match (bounds.start_bound(), bounds.end_bound()) {
            (Bound::Included(start), Bound::Excluded(end)) if start > end => {
                return Box::new(iter::empty());
            }
            _ => {}
        }

        let iter = self.data.range(bounds);
        match order {
            Order::Ascending => Box::new(iter.map(clone_item)),
            Order::Descending => Box::new(iter.rev().map(clone_item)),
        }
    }
}

/// This debug implementation is made for inspecting storages in unit testing.
/// It is made for human readability only and the output can change at any time.
impl fmt::Debug for MemoryStorage {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "MemoryStorage ({} entries)", self.data.len())?;
        f.write_str(" {\n")?;
        for (key, value) in &self.data {
            f.write_str("  0x")?;
            for byte in key {
                write!(f, "{:02x}", byte)?;
            }
            f.write_str(": 0x")?;
            for byte in value {
                write!(f, "{:02x}", byte)?;
            }
            f.write_str("\n")?;
        }
        f.write_str("}")?;
        Ok(())
    }
}

#[cfg(feature = "iterator")]
fn range_bounds(start: Option<&[u8]>, end: Option<&[u8]>) -> impl RangeBounds<Vec<u8>> {
    (
        start.map_or(Bound::Unbounded, |x| Bound::Included(x.to_vec())),
        end.map_or(Bound::Unbounded, |x| Bound::Excluded(x.to_vec())),
    )
}

#[cfg(feature = "iterator")]
/// The BTreeMap specific key-value pair reference type, as returned by BTreeMap<Vec<u8>, Vec<u8>>::range.
/// This is internal as it can change any time if the map implementation is swapped out.
type BTreeMapRecordRef<'a> = (&'a Vec<u8>, &'a Vec<u8>);

#[cfg(feature = "iterator")]
fn clone_item(item_ref: BTreeMapRecordRef) -> Record {
    let (key, value) = item_ref;
    (key.clone(), value.clone())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn get_and_set() {
        let mut store = MemoryStorage::new();
        assert_eq!(store.get(b"foo"), None);
        store.set(b"foo", b"bar");
        assert_eq!(store.get(b"foo"), Some(b"bar".to_vec()));
        assert_eq!(store.get(b"food"), None);
    }

    #[test]
    #[should_panic(
        expected = "Getting empty values from storage is not well supported at the moment."
    )]
    fn set_panics_for_empty() {
        let mut store = MemoryStorage::new();
        store.set(b"foo", b"");
    }

    #[test]
    fn delete() {
        let mut store = MemoryStorage::new();
        store.set(b"foo", b"bar");
        store.set(b"food", b"bank");
        store.remove(b"foo");

        assert_eq!(store.get(b"foo"), None);
        assert_eq!(store.get(b"food"), Some(b"bank".to_vec()));
    }

    #[test]
    #[cfg(feature = "iterator")]
    fn iterator() {
        let mut store = MemoryStorage::new();
        store.set(b"foo", b"bar");

        // ensure we had previously set "foo" = "bar"
        assert_eq!(store.get(b"foo"), Some(b"bar".to_vec()));
        assert_eq!(store.range(None, None, Order::Ascending).count(), 1);

        // setup - add some data, and delete part of it as well
        store.set(b"ant", b"hill");
        store.set(b"ze", b"bra");

        // noise that should be ignored
        store.set(b"bye", b"bye");
        store.remove(b"bye");

        // unbounded
        {
            let iter = store.range(None, None, Order::Ascending);
            let elements: Vec<Record> = iter.collect();
            assert_eq!(
                elements,
                vec![
                    (b"ant".to_vec(), b"hill".to_vec()),
                    (b"foo".to_vec(), b"bar".to_vec()),
                    (b"ze".to_vec(), b"bra".to_vec()),
                ]
            );
        }

        // unbounded (descending)
        {
            let iter = store.range(None, None, Order::Descending);
            let elements: Vec<Record> = iter.collect();
            assert_eq!(
                elements,
                vec![
                    (b"ze".to_vec(), b"bra".to_vec()),
                    (b"foo".to_vec(), b"bar".to_vec()),
                    (b"ant".to_vec(), b"hill".to_vec()),
                ]
            );
        }

        // bounded
        {
            let iter = store.range(Some(b"f"), Some(b"n"), Order::Ascending);
            let elements: Vec<Record> = iter.collect();
            assert_eq!(elements, vec![(b"foo".to_vec(), b"bar".to_vec())]);
        }

        // bounded (descending)
        {
            let iter = store.range(Some(b"air"), Some(b"loop"), Order::Descending);
            let elements: Vec<Record> = iter.collect();
            assert_eq!(
                elements,
                vec![
                    (b"foo".to_vec(), b"bar".to_vec()),
                    (b"ant".to_vec(), b"hill".to_vec()),
                ]
            );
        }

        // bounded empty [a, a)
        {
            let iter = store.range(Some(b"foo"), Some(b"foo"), Order::Ascending);
            let elements: Vec<Record> = iter.collect();
            assert_eq!(elements, vec![]);
        }

        // bounded empty [a, a) (descending)
        {
            let iter = store.range(Some(b"foo"), Some(b"foo"), Order::Descending);
            let elements: Vec<Record> = iter.collect();
            assert_eq!(elements, vec![]);
        }

        // bounded empty [a, b) with b < a
        {
            let iter = store.range(Some(b"z"), Some(b"a"), Order::Ascending);
            let elements: Vec<Record> = iter.collect();
            assert_eq!(elements, vec![]);
        }

        // bounded empty [a, b) with b < a (descending)
        {
            let iter = store.range(Some(b"z"), Some(b"a"), Order::Descending);
            let elements: Vec<Record> = iter.collect();
            assert_eq!(elements, vec![]);
        }

        // right unbounded
        {
            let iter = store.range(Some(b"f"), None, Order::Ascending);
            let elements: Vec<Record> = iter.collect();
            assert_eq!(
                elements,
                vec![
                    (b"foo".to_vec(), b"bar".to_vec()),
                    (b"ze".to_vec(), b"bra".to_vec()),
                ]
            );
        }

        // right unbounded (descending)
        {
            let iter = store.range(Some(b"f"), None, Order::Descending);
            let elements: Vec<Record> = iter.collect();
            assert_eq!(
                elements,
                vec![
                    (b"ze".to_vec(), b"bra".to_vec()),
                    (b"foo".to_vec(), b"bar".to_vec()),
                ]
            );
        }

        // left unbounded
        {
            let iter = store.range(None, Some(b"f"), Order::Ascending);
            let elements: Vec<Record> = iter.collect();
            assert_eq!(elements, vec![(b"ant".to_vec(), b"hill".to_vec()),]);
        }

        // left unbounded (descending)
        {
            let iter = store.range(None, Some(b"no"), Order::Descending);
            let elements: Vec<Record> = iter.collect();
            assert_eq!(
                elements,
                vec![
                    (b"foo".to_vec(), b"bar".to_vec()),
                    (b"ant".to_vec(), b"hill".to_vec()),
                ]
            );
        }
    }

    #[test]
    fn memory_storage_implements_debug() {
        let store = MemoryStorage::new();
        assert_eq!(
            format!("{:?}", store),
            "MemoryStorage (0 entries) {\n\
            }"
        );

        // With one element
        let mut store = MemoryStorage::new();
        store.set(&[0x00, 0xAB, 0xDD], &[0xFF, 0xD5]);
        assert_eq!(
            format!("{:?}", store),
            "MemoryStorage (1 entries) {\n\
            \x20\x200x00abdd: 0xffd5\n\
            }"
        );

        // Sorted by key
        let mut store = MemoryStorage::new();
        store.set(&[0x00, 0xAB, 0xDD], &[0xFF, 0xD5]);
        store.set(&[0x00, 0xAB, 0xEE], &[0xFF, 0xD5]);
        store.set(&[0x00, 0xAB, 0xCC], &[0xFF, 0xD5]);
        assert_eq!(
            format!("{:?}", store),
            "MemoryStorage (3 entries) {\n\
            \x20\x200x00abcc: 0xffd5\n\
            \x20\x200x00abdd: 0xffd5\n\
            \x20\x200x00abee: 0xffd5\n\
            }"
        );

        // Different lengths
        let mut store = MemoryStorage::new();
        store.set(&[0xAA], &[0x11]);
        store.set(&[0xAA, 0xBB], &[0x11, 0x22]);
        store.set(&[0xAA, 0xBB, 0xCC], &[0x11, 0x22, 0x33]);
        store.set(&[0xAA, 0xBB, 0xCC, 0xDD], &[0x11, 0x22, 0x33, 0x44]);
        assert_eq!(
            format!("{:?}", store),
            "MemoryStorage (4 entries) {\n\
            \x20\x200xaa: 0x11\n\
            \x20\x200xaabb: 0x1122\n\
            \x20\x200xaabbcc: 0x112233\n\
            \x20\x200xaabbccdd: 0x11223344\n\
            }"
        );
    }
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
    pub const fn from_nanos(nanos_since_epoch: u64) -> Self {
        Timestamp(Uint64::new(nanos_since_epoch))
    }

    /// Creates a timestamp from seconds since epoch
    pub const fn from_seconds(seconds_since_epoch: u64) -> Self {
        Timestamp(Uint64::new(seconds_since_epoch * 1_000_000_000))
    }

    pub const fn plus_seconds(&self, addition: u64) -> Timestamp {
        self.plus_nanos(addition * 1_000_000_000)
    }

    pub const fn plus_nanos(&self, addition: u64) -> Timestamp {
        let nanos = Uint64::new(self.0.u64() + addition);
        Timestamp(nanos)
    }

    pub const fn minus_seconds(&self, subtrahend: u64) -> Timestamp {
        self.minus_nanos(subtrahend * 1_000_000_000)
    }

    pub const fn minus_nanos(&self, subtrahend: u64) -> Timestamp {
        let nanos = Uint64::new(self.0.u64() - subtrahend);
        Timestamp(nanos)
    }

    /// Returns nanoseconds since epoch
    #[inline]
    pub fn nanos(&self) -> u64 {
        self.0.u64()
    }

    /// Returns seconds since epoch (truncate nanoseconds)
    #[inline]
    pub fn seconds(&self) -> u64 {
        self.0.u64() / 1_000_000_000
    }

    /// Returns nanoseconds since the last whole second (the remainder truncated
    /// by `seconds()`)
    #[inline]
    pub fn subsec_nanos(&self) -> u64 {
        self.0.u64() % 1_000_000_000
    }
}

impl fmt::Display for Timestamp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let whole = self.seconds();
        let fractional = self.subsec_nanos();
        write!(f, "{}.{:09}", whole, fractional)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn timestamp_from_nanos() {
        let t = Timestamp::from_nanos(123);
        assert_eq!(t.0.u64(), 123);
        let t = Timestamp::from_nanos(0);
        assert_eq!(t.0.u64(), 0);
    }

    #[test]
    fn timestamp_from_seconds() {
        let t = Timestamp::from_seconds(123);
        assert_eq!(t.0.u64(), 123_000_000_000);
        let t = Timestamp::from_seconds(0);
        assert_eq!(t.0.u64(), 0);
    }

    #[test]
    fn timestamp_plus_seconds() {
        let sum = Timestamp::from_nanos(123).plus_seconds(42);
        assert_eq!(sum.0.u64(), 42_000_000_123);
        let sum = Timestamp::from_nanos(123).plus_seconds(0);
        assert_eq!(sum.0.u64(), 123);
    }

    #[test]
    fn timestamp_plus_nanos() {
        let sum = Timestamp::from_nanos(123).plus_nanos(3);
        assert_eq!(sum.0.u64(), 126);
        let sum = Timestamp::from_nanos(123).plus_nanos(0);
        assert_eq!(sum.0.u64(), 123);
    }

    #[test]
    fn timestamp_minus_seconds() {
        let earlier = Timestamp::from_seconds(123).minus_seconds(0);
        assert_eq!(earlier.0.u64(), 123_000_000_000);
        let earlier = Timestamp::from_seconds(123).minus_seconds(3);
        assert_eq!(earlier.0.u64(), 120_000_000_000);
        let earlier = Timestamp::from_seconds(123).minus_seconds(123);
        assert_eq!(earlier.0.u64(), 0);
    }

    #[test]
    #[should_panic(expected = "attempt to subtract with overflow")]
    fn timestamp_minus_seconds_panics_on_overflow() {
        let _earlier = Timestamp::from_seconds(100).minus_seconds(101);
    }

    #[test]
    fn timestamp_minus_nanos() {
        let earlier = Timestamp::from_seconds(123).minus_nanos(0);
        assert_eq!(earlier.0.u64(), 123_000_000_000);
        let earlier = Timestamp::from_seconds(123).minus_nanos(3);
        assert_eq!(earlier.0.u64(), 122_999_999_997);
        let earlier = Timestamp::from_seconds(123).minus_nanos(123_000_000_000);
        assert_eq!(earlier.0.u64(), 0);
    }

    #[test]
    #[should_panic(expected = "attempt to subtract with overflow")]
    fn timestamp_minus_nanos_panics_on_overflow() {
        let _earlier = Timestamp::from_nanos(100).minus_nanos(101);
    }

    #[test]
    fn timestamp_nanos() {
        let sum = Timestamp::from_nanos(123);
        assert_eq!(sum.nanos(), 123);
        let sum = Timestamp::from_nanos(0);
        assert_eq!(sum.nanos(), 0);
        let sum = Timestamp::from_nanos(987654321000);
        assert_eq!(sum.nanos(), 987654321000);
    }

    #[test]
    fn timestamp_seconds() {
        let sum = Timestamp::from_nanos(987654321000);
        assert_eq!(sum.seconds(), 987);
        let sum = Timestamp::from_seconds(1234567).plus_nanos(8765436);
        assert_eq!(sum.seconds(), 1234567);
    }

    #[test]
    fn timestamp_subsec_nanos() {
        let sum = Timestamp::from_nanos(987654321000);
        assert_eq!(sum.subsec_nanos(), 654321000);
        let sum = Timestamp::from_seconds(1234567).plus_nanos(8765436);
        assert_eq!(sum.subsec_nanos(), 8765436);
    }

    #[test]
    fn timestamp_implements_display() {
        let embedded = format!("Time: {}", Timestamp::from_nanos(0));
        assert_eq!(embedded, "Time: 0.000000000");
        let embedded = format!("Time: {}", Timestamp::from_nanos(1));
        assert_eq!(embedded, "Time: 0.000000001");
        let embedded = format!("Time: {}", Timestamp::from_nanos(10));
        assert_eq!(embedded, "Time: 0.000000010");
        let embedded = format!("Time: {}", Timestamp::from_nanos(100));
        assert_eq!(embedded, "Time: 0.000000100");
        let embedded = format!("Time: {}", Timestamp::from_nanos(1000));
        assert_eq!(embedded, "Time: 0.000001000");
        let embedded = format!("Time: {}", Timestamp::from_nanos(10000));
        assert_eq!(embedded, "Time: 0.000010000");
        let embedded = format!("Time: {}", Timestamp::from_nanos(100000));
        assert_eq!(embedded, "Time: 0.000100000");
        let embedded = format!("Time: {}", Timestamp::from_nanos(1000000));
        assert_eq!(embedded, "Time: 0.001000000");
        let embedded = format!("Time: {}", Timestamp::from_nanos(1000000));
        assert_eq!(embedded, "Time: 0.001000000");
        let embedded = format!("Time: {}", Timestamp::from_nanos(10000000));
        assert_eq!(embedded, "Time: 0.010000000");
        let embedded = format!("Time: {}", Timestamp::from_nanos(100000000));
        assert_eq!(embedded, "Time: 0.100000000");
        let embedded = format!("Time: {}", Timestamp::from_nanos(1000000000));
        assert_eq!(embedded, "Time: 1.000000000");
        let embedded = format!("Time: {}", Timestamp::from_nanos(10000000000));
        assert_eq!(embedded, "Time: 10.000000000");
        let embedded = format!("Time: {}", Timestamp::from_nanos(100000000000));
        assert_eq!(embedded, "Time: 100.000000000");
    }
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

    fn deref(&self) -> &Self::Target {
        self.querier
    }
}

impl<'a, C: CustomQuery> QuerierWrapper<'a, C> {
    pub fn new(querier: &'a dyn Querier) -> Self {
        QuerierWrapper {
            querier,
            custom_query_type: PhantomData,
        }
    }

    /// Makes the query and parses the response.
    ///
    /// Any error (System Error, Error or called contract, or Parse Error) are flattened into
    /// one level. Only use this if you don't need to check the SystemError
    /// eg. If you don't differentiate between contract missing and contract returned error
    pub fn query<U: DeserializeOwned>(&self, request: &QueryRequest<C>) -> StdResult<U> {
        let raw = to_vec(request).map_err(|serialize_err| {
            StdError::generic_err(format!("Serializing QueryRequest: {}", serialize_err))
        })?;
        match self.raw_query(&raw) {
            SystemResult::Err(system_err) => Err(StdError::generic_err(format!(
                "Querier system error: {}",
                system_err
            ))),
            SystemResult::Ok(ContractResult::Err(contract_err)) => Err(StdError::generic_err(
                format!("Querier contract error: {}", contract_err),
            )),
            SystemResult::Ok(ContractResult::Ok(value)) => from_binary(&value),
        }
    }

    #[cfg(feature = "cosmwasm_1_1")]
    pub fn query_supply(&self, denom: impl Into<String>) -> StdResult<Coin> {
        let request = BankQuery::Supply {
            denom: denom.into(),
        }
        .into();
        let res: SupplyResponse = self.query(&request)?;
        Ok(res.amount)
    }

    pub fn query_balance(
        &self,
        address: impl Into<String>,
        denom: impl Into<String>,
    ) -> StdResult<Coin> {
        let request = BankQuery::Balance {
            address: address.into(),
            denom: denom.into(),
        }
        .into();
        let res: BalanceResponse = self.query(&request)?;
        Ok(res.amount)
    }

    pub fn query_all_balances(&self, address: impl Into<String>) -> StdResult<Vec<Coin>> {
        let request = BankQuery::AllBalances {
            address: address.into(),
        }
        .into();
        let res: AllBalanceResponse = self.query(&request)?;
        Ok(res.amount)
    }

    // this queries another wasm contract. You should know a priori the proper types for T and U
    // (response and request) based on the contract API
    pub fn query_wasm_smart<T: DeserializeOwned>(
        &self,
        contract_addr: impl Into<String>,
        msg: &impl Serialize,
    ) -> StdResult<T> {
        let request = WasmQuery::Smart {
            contract_addr: contract_addr.into(),
            msg: to_binary(msg)?,
        }
        .into();
        self.query(&request)
    }

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
    ) -> StdResult<Option<Vec<u8>>> {
        let request: QueryRequest<Empty> = WasmQuery::Raw {
            contract_addr: contract_addr.into(),
            key: key.into(),
        }
        .into();
        // we cannot use query, as it will try to parse the binary data, when we just want to return it,
        // so a bit of code copy here...
        let raw = to_vec(&request).map_err(|serialize_err| {
            StdError::generic_err(format!("Serializing QueryRequest: {}", serialize_err))
        })?;
        match self.raw_query(&raw) {
            SystemResult::Err(system_err) => Err(StdError::generic_err(format!(
                "Querier system error: {}",
                system_err
            ))),
            SystemResult::Ok(ContractResult::Err(contract_err)) => Err(StdError::generic_err(
                format!("Querier contract error: {}", contract_err),
            )),
            SystemResult::Ok(ContractResult::Ok(value)) => {
                if value.is_empty() {
                    Ok(None)
                } else {
                    Ok(Some(value.into()))
                }
            }
        }
    }

    /// Given a contract address, query information about that contract.
    pub fn query_wasm_contract_info(
        &self,
        contract_addr: impl Into<String>,
    ) -> StdResult<ContractInfoResponse> {
        let request = WasmQuery::ContractInfo {
            contract_addr: contract_addr.into(),
        }
        .into();
        self.query(&request)
    }

    #[cfg(feature = "staking")]
    pub fn query_all_validators(&self) -> StdResult<Vec<Validator>> {
        let request = StakingQuery::AllValidators {}.into();
        let res: AllValidatorsResponse = self.query(&request)?;
        Ok(res.validators)
    }

    #[cfg(feature = "staking")]
    pub fn query_validator(&self, address: impl Into<String>) -> StdResult<Option<Validator>> {
        let request = StakingQuery::Validator {
            address: address.into(),
        }
        .into();
        let res: ValidatorResponse = self.query(&request)?;
        Ok(res.validator)
    }

    #[cfg(feature = "staking")]
    pub fn query_bonded_denom(&self) -> StdResult<String> {
        let request = StakingQuery::BondedDenom {}.into();
        let res: BondedDenomResponse = self.query(&request)?;
        Ok(res.denom)
    }

    #[cfg(feature = "staking")]
    pub fn query_all_delegations(
        &self,
        delegator: impl Into<String>,
    ) -> StdResult<Vec<Delegation>> {
        let request = StakingQuery::AllDelegations {
            delegator: delegator.into(),
        }
        .into();
        let res: AllDelegationsResponse = self.query(&request)?;
        Ok(res.delegations)
    }

    #[cfg(feature = "staking")]
    pub fn query_delegation(
        &self,
        delegator: impl Into<String>,
        validator: impl Into<String>,
    ) -> StdResult<Option<FullDelegation>> {
        let request = StakingQuery::Delegation {
            delegator: delegator.into(),
            validator: validator.into(),
        }
        .into();
        let res: DelegationResponse = self.query(&request)?;
        Ok(res.delegation)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::testing::MockQuerier;
    use crate::{coins, from_slice, Uint128};

    // this is a simple demo helper to prove we can use it
    fn demo_helper(_querier: &dyn Querier) -> u64 {
        2
    }

    // this just needs to compile to prove we can use it
    #[test]
    fn use_querier_wrapper_as_querier() {
        let querier: MockQuerier<Empty> = MockQuerier::new(&[]);
        let wrapper = QuerierWrapper::<Empty>::new(&querier);

        // call with deref shortcut
        let res = demo_helper(&*wrapper);
        assert_eq!(2, res);

        // call with explicit deref
        let res = demo_helper(wrapper.deref());
        assert_eq!(2, res);
    }

    #[test]
    fn auto_deref_raw_query() {
        let acct = String::from("foobar");
        let querier: MockQuerier<Empty> = MockQuerier::new(&[(&acct, &coins(5, "BTC"))]);
        let wrapper = QuerierWrapper::<Empty>::new(&querier);
        let query = QueryRequest::<Empty>::Bank(BankQuery::Balance {
            address: acct,
            denom: "BTC".to_string(),
        });

        let raw = wrapper
            .raw_query(&to_vec(&query).unwrap())
            .unwrap()
            .unwrap();
        let balance: BalanceResponse = from_slice(&raw).unwrap();
        assert_eq!(balance.amount.amount, Uint128::new(5));
    }

    #[cfg(feature = "cosmwasm_1_1")]
    #[test]
    fn bank_query_helpers_work() {
        use crate::coin;

        let querier: MockQuerier<Empty> = MockQuerier::new(&[
            ("foo", &[coin(123, "ELF"), coin(777, "FLY")]),
            ("bar", &[coin(321, "ELF")]),
        ]);
        let wrapper = QuerierWrapper::<Empty>::new(&querier);

        let supply = wrapper.query_supply("ELF").unwrap();
        assert_eq!(supply, coin(444, "ELF"));

        let balance = wrapper.query_balance("foo", "ELF").unwrap();
        assert_eq!(balance, coin(123, "ELF"));

        let all_balances = wrapper.query_all_balances("foo").unwrap();
        assert_eq!(all_balances, vec![coin(123, "ELF"), coin(777, "FLY")]);
    }

    #[test]
    fn contract_info() {
        const ACCT: &str = "foobar";
        fn mock_resp() -> ContractInfoResponse {
            ContractInfoResponse {
                code_id: 0,
                creator: "creator".to_string(),
                admin: None,
                pinned: false,
                ibc_port: None,
            }
        }

        let mut querier: MockQuerier<Empty> = MockQuerier::new(&[(ACCT, &coins(5, "BTC"))]);
        querier.update_wasm(|q| -> QuerierResult {
            if q == &(WasmQuery::ContractInfo {
                contract_addr: ACCT.to_string(),
            }) {
                SystemResult::Ok(ContractResult::Ok(to_binary(&mock_resp()).unwrap()))
            } else {
                SystemResult::Err(crate::SystemError::NoSuchContract {
                    addr: ACCT.to_string(),
                })
            }
        });
        let wrapper = QuerierWrapper::<Empty>::new(&querier);

        let contract_info = wrapper.query_wasm_contract_info(ACCT).unwrap();
        assert_eq!(contract_info, mock_resp());
    }

    #[test]
    fn contract_info_err() {
        const ACCT: &str = "foobar";
        fn mock_resp() -> ContractInfoResponse {
            ContractInfoResponse {
                code_id: 0,
                creator: "creator".to_string(),
                admin: None,
                pinned: false,
                ibc_port: None,
            }
        }

        let mut querier: MockQuerier<Empty> = MockQuerier::new(&[(ACCT, &coins(5, "BTC"))]);
        querier.update_wasm(|q| -> QuerierResult {
            if q == &(WasmQuery::ContractInfo {
                contract_addr: ACCT.to_string(),
            }) {
                SystemResult::Ok(ContractResult::Ok(to_binary(&mock_resp()).unwrap()))
            } else {
                SystemResult::Err(crate::SystemError::NoSuchContract {
                    addr: ACCT.to_string(),
                })
            }
        });
        let wrapper = QuerierWrapper::<Empty>::new(&querier);

        let err = wrapper.query_wasm_contract_info("unknown").unwrap_err();
        assert!(matches!(
            err,
            StdError::GenericErr {
                msg,
                ..
            } if msg == "Querier system error: No such contract: foobar"
        ));
    }
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
extern "C" fn allocate(size: usize) -> u32 {
    alloc(size) as u32
}

/// deallocate expects a pointer to a Region created with allocate.
/// It will free both the Region and the memory referenced by the Region.
#[no_mangle]
extern "C" fn deallocate(pointer: u32) {
    // auto-drop Region on function end
    let _ = unsafe { consume_region(pointer as *mut Region) };
}

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
{
    #[cfg(feature = "abort")]
    install_panic_handler();
    let res = _do_instantiate(
        instantiate_fn,
        env_ptr as *mut Region,
        info_ptr as *mut Region,
        msg_ptr as *mut Region,
    );
    let v = to_vec(&res).unwrap();
    release_buffer(v) as u32
}

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
{
    #[cfg(feature = "abort")]
    install_panic_handler();
    let res = _do_execute(
        execute_fn,
        env_ptr as *mut Region,
        info_ptr as *mut Region,
        msg_ptr as *mut Region,
    );
    let v = to_vec(&res).unwrap();
    release_buffer(v) as u32
}

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
{
    #[cfg(feature = "abort")]
    install_panic_handler();
    let res = _do_migrate(migrate_fn, env_ptr as *mut Region, msg_ptr as *mut Region);
    let v = to_vec(&res).unwrap();
    release_buffer(v) as u32
}

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
{
    #[cfg(feature = "abort")]
    install_panic_handler();
    let res = _do_sudo(sudo_fn, env_ptr as *mut Region, msg_ptr as *mut Region);
    let v = to_vec(&res).unwrap();
    release_buffer(v) as u32
}

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
{
    #[cfg(feature = "abort")]
    install_panic_handler();
    let res = _do_reply(reply_fn, env_ptr as *mut Region, msg_ptr as *mut Region);
    let v = to_vec(&res).unwrap();
    release_buffer(v) as u32
}

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
{
    #[cfg(feature = "abort")]
    install_panic_handler();
    let res = _do_query(query_fn, env_ptr as *mut Region, msg_ptr as *mut Region);
    let v = to_vec(&res).unwrap();
    release_buffer(v) as u32
}

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
{
    #[cfg(feature = "abort")]
    install_panic_handler();
    let res = _do_ibc_channel_open(contract_fn, env_ptr as *mut Region, msg_ptr as *mut Region);
    let v = to_vec(&res).unwrap();
    release_buffer(v) as u32
}

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
{
    #[cfg(feature = "abort")]
    install_panic_handler();
    let res = _do_ibc_channel_connect(contract_fn, env_ptr as *mut Region, msg_ptr as *mut Region);
    let v = to_vec(&res).unwrap();
    release_buffer(v) as u32
}

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
{
    #[cfg(feature = "abort")]
    install_panic_handler();
    let res = _do_ibc_channel_close(contract_fn, env_ptr as *mut Region, msg_ptr as *mut Region);
    let v = to_vec(&res).unwrap();
    release_buffer(v) as u32
}

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
{
    #[cfg(feature = "abort")]
    install_panic_handler();
    let res = _do_ibc_packet_receive(contract_fn, env_ptr as *mut Region, msg_ptr as *mut Region);
    let v = to_vec(&res).unwrap();
    release_buffer(v) as u32
}

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
{
    #[cfg(feature = "abort")]
    install_panic_handler();
    let res = _do_ibc_packet_ack(contract_fn, env_ptr as *mut Region, msg_ptr as *mut Region);
    let v = to_vec(&res).unwrap();
    release_buffer(v) as u32
}

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
{
    #[cfg(feature = "abort")]
    install_panic_handler();
    let res = _do_ibc_packet_timeout(contract_fn, env_ptr as *mut Region, msg_ptr as *mut Region);
    let v = to_vec(&res).unwrap();
    release_buffer(v) as u32
}

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
{
    let env: Vec<u8> = unsafe { consume_region(env_ptr) };
    let info: Vec<u8> = unsafe { consume_region(info_ptr) };
    let msg: Vec<u8> = unsafe { consume_region(msg_ptr) };

    let env: Env = try_into_contract_result!(from_slice(&env));
    let info: MessageInfo = try_into_contract_result!(from_slice(&info));
    let msg: M = try_into_contract_result!(from_slice(&msg));

    let mut deps = make_dependencies();
    instantiate_fn(deps.as_mut(), env, info, msg).into()
}

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
{
    let env: Vec<u8> = unsafe { consume_region(env_ptr) };
    let info: Vec<u8> = unsafe { consume_region(info_ptr) };
    let msg: Vec<u8> = unsafe { consume_region(msg_ptr) };

    let env: Env = try_into_contract_result!(from_slice(&env));
    let info: MessageInfo = try_into_contract_result!(from_slice(&info));
    let msg: M = try_into_contract_result!(from_slice(&msg));

    let mut deps = make_dependencies();
    execute_fn(deps.as_mut(), env, info, msg).into()
}

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
{
    let env: Vec<u8> = unsafe { consume_region(env_ptr) };
    let msg: Vec<u8> = unsafe { consume_region(msg_ptr) };

    let env: Env = try_into_contract_result!(from_slice(&env));
    let msg: M = try_into_contract_result!(from_slice(&msg));

    let mut deps = make_dependencies();
    migrate_fn(deps.as_mut(), env, msg).into()
}

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
{
    let env: Vec<u8> = unsafe { consume_region(env_ptr) };
    let msg: Vec<u8> = unsafe { consume_region(msg_ptr) };

    let env: Env = try_into_contract_result!(from_slice(&env));
    let msg: M = try_into_contract_result!(from_slice(&msg));

    let mut deps = make_dependencies();
    sudo_fn(deps.as_mut(), env, msg).into()
}

fn _do_reply<Q, C, E>(
    reply_fn: &dyn Fn(DepsMut<Q>, Env, Reply) -> Result<Response<C>, E>,
    env_ptr: *mut Region,
    msg_ptr: *mut Region,
) -> ContractResult<Response<C>>
where
    Q: CustomQuery,
    C: CustomMsg,
    E: ToString,
{
    let env: Vec<u8> = unsafe { consume_region(env_ptr) };
    let msg: Vec<u8> = unsafe { consume_region(msg_ptr) };

    let env: Env = try_into_contract_result!(from_slice(&env));
    let msg: Reply = try_into_contract_result!(from_slice(&msg));

    let mut deps = make_dependencies();
    reply_fn(deps.as_mut(), env, msg).into()
}

fn _do_query<Q, M, E>(
    query_fn: &dyn Fn(Deps<Q>, Env, M) -> Result<QueryResponse, E>,
    env_ptr: *mut Region,
    msg_ptr: *mut Region,
) -> ContractResult<QueryResponse>
where
    Q: CustomQuery,
    M: DeserializeOwned,
    E: ToString,
{
    let env: Vec<u8> = unsafe { consume_region(env_ptr) };
    let msg: Vec<u8> = unsafe { consume_region(msg_ptr) };

    let env: Env = try_into_contract_result!(from_slice(&env));
    let msg: M = try_into_contract_result!(from_slice(&msg));

    let deps = make_dependencies();
    query_fn(deps.as_ref(), env, msg).into()
}

#[cfg(feature = "stargate")]
fn _do_ibc_channel_open<Q, E>(
    contract_fn: &dyn Fn(DepsMut<Q>, Env, IbcChannelOpenMsg) -> Result<IbcChannelOpenResponse, E>,
    env_ptr: *mut Region,
    msg_ptr: *mut Region,
) -> ContractResult<IbcChannelOpenResponse>
where
    Q: CustomQuery,
    E: ToString,
{
    let env: Vec<u8> = unsafe { consume_region(env_ptr) };
    let msg: Vec<u8> = unsafe { consume_region(msg_ptr) };

    let env: Env = try_into_contract_result!(from_slice(&env));
    let msg: IbcChannelOpenMsg = try_into_contract_result!(from_slice(&msg));

    let mut deps = make_dependencies();
    contract_fn(deps.as_mut(), env, msg).into()
}

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
{
    let env: Vec<u8> = unsafe { consume_region(env_ptr) };
    let msg: Vec<u8> = unsafe { consume_region(msg_ptr) };

    let env: Env = try_into_contract_result!(from_slice(&env));
    let msg: IbcChannelConnectMsg = try_into_contract_result!(from_slice(&msg));

    let mut deps = make_dependencies();
    contract_fn(deps.as_mut(), env, msg).into()
}

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
{
    let env: Vec<u8> = unsafe { consume_region(env_ptr) };
    let msg: Vec<u8> = unsafe { consume_region(msg_ptr) };

    let env: Env = try_into_contract_result!(from_slice(&env));
    let msg: IbcChannelCloseMsg = try_into_contract_result!(from_slice(&msg));

    let mut deps = make_dependencies();
    contract_fn(deps.as_mut(), env, msg).into()
}

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
{
    let env: Vec<u8> = unsafe { consume_region(env_ptr) };
    let msg: Vec<u8> = unsafe { consume_region(msg_ptr) };

    let env: Env = try_into_contract_result!(from_slice(&env));
    let msg: IbcPacketReceiveMsg = try_into_contract_result!(from_slice(&msg));

    let mut deps = make_dependencies();
    contract_fn(deps.as_mut(), env, msg).into()
}

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
{
    let env: Vec<u8> = unsafe { consume_region(env_ptr) };
    let msg: Vec<u8> = unsafe { consume_region(msg_ptr) };

    let env: Env = try_into_contract_result!(from_slice(&env));
    let msg: IbcPacketAckMsg = try_into_contract_result!(from_slice(&msg));

    let mut deps = make_dependencies();
    contract_fn(deps.as_mut(), env, msg).into()
}

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
{
    let env: Vec<u8> = unsafe { consume_region(env_ptr) };
    let msg: Vec<u8> = unsafe { consume_region(msg_ptr) };

    let env: Env = try_into_contract_result!(from_slice(&env));
    let msg: IbcPacketTimeoutMsg = try_into_contract_result!(from_slice(&msg));

    let mut deps = make_dependencies();
    contract_fn(deps.as_mut(), env, msg).into()
}

/// Makes all bridges to external dependencies (i.e. Wasm imports) that are injected by the VM
pub(crate) fn make_dependencies<Q>() -> OwnedDeps<ExternalStorage, ExternalApi, ExternalQuerier, Q>
where
    Q: CustomQuery,
{
    OwnedDeps {
        storage: ExternalStorage::new(),
        api: ExternalApi::new(),
        querier: ExternalQuerier::new(),
        custom_query_type: PhantomData,
    }
}
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
    pub fn new() -> ExternalStorage {
        ExternalStorage {}
    }
}

impl Storage for ExternalStorage {
    fn get(&self, key: &[u8]) -> Option<Vec<u8>> {
        let key = build_region(key);
        let key_ptr = &*key as *const Region as u32;

        let read = unsafe { db_read(key_ptr) };
        if read == 0 {
            // key does not exist in external storage
            return None;
        }

        let value_ptr = read as *mut Region;
        let data = unsafe { consume_region(value_ptr) };
        Some(data)
    }

    fn set(&mut self, key: &[u8], value: &[u8]) {
        if value.is_empty() {
            panic!("TL;DR: Value must not be empty in Storage::set but in most cases you can use Storage::remove instead. Long story: Getting empty values from storage is not well supported at the moment. Some of our internal interfaces cannot differentiate between a non-existent key and an empty value. Right now, you cannot rely on the behaviour of empty values. To protect you from trouble later on, we stop here. Sorry for the inconvenience! We highly welcome you to contribute to CosmWasm, making this more solid one way or the other.");
        }

        // keep the boxes in scope, so we free it at the end (don't cast to pointers same line as build_region)
        let key = build_region(key);
        let key_ptr = &*key as *const Region as u32;
        let mut value = build_region(value);
        let value_ptr = &mut *value as *mut Region as u32;
        unsafe { db_write(key_ptr, value_ptr) };
    }

    fn remove(&mut self, key: &[u8]) {
        // keep the boxes in scope, so we free it at the end (don't cast to pointers same line as build_region)
        let key = build_region(key);
        let key_ptr = &*key as *const Region as u32;
        unsafe { db_remove(key_ptr) };
    }

    #[cfg(feature = "iterator")]
    fn range(
        &self,
        start: Option<&[u8]>,
        end: Option<&[u8]>,
        order: Order,
    ) -> Box<dyn Iterator<Item = Record>> {
        // There is lots of gotchas on turning options into regions for FFI, thus this design
        // See: https://github.com/CosmWasm/cosmwasm/pull/509
        let start_region = start.map(build_region);
        let end_region = end.map(build_region);
        let start_region_addr = get_optional_region_address(&start_region.as_ref());
        let end_region_addr = get_optional_region_address(&end_region.as_ref());
        let iterator_id = unsafe { db_scan(start_region_addr, end_region_addr, order as i32) };
        let iter = ExternalIterator { iterator_id };
        Box::new(iter)
    }
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

    fn next(&mut self) -> Option<Self::Item> {
        let next_result = unsafe { db_next(self.iterator_id) };
        let kv_region_ptr = next_result as *mut Region;
        let kv = unsafe { consume_region(kv_region_ptr) };
        let (key, value) = decode_sections2(kv);
        if key.len() == 0 {
            None
        } else {
            Some((key, value))
        }
    }
}

/// A stateless convenience wrapper around imports provided by the VM
#[derive(Copy, Clone)]
pub struct ExternalApi {}

impl ExternalApi {
    pub fn new() -> ExternalApi {
        ExternalApi {}
    }
}

impl Api for ExternalApi {
    fn addr_validate(&self, input: &str) -> StdResult<Addr> {
        let input_bytes = input.as_bytes();
        if input_bytes.len() > 256 {
            // See MAX_LENGTH_HUMAN_ADDRESS in the VM.
            // In this case, the VM will refuse to read the input from the contract.
            // Stop here to allow handling the error in the contract.
            return Err(StdError::generic_err("input too long for addr_validate"));
        }
        let source = build_region(input_bytes);
        let source_ptr = &*source as *const Region as u32;

        let result = unsafe { addr_validate(source_ptr) };
        if result != 0 {
            let error = unsafe { consume_string_region_written_by_vm(result as *mut Region) };
            return Err(StdError::generic_err(format!(
                "addr_validate errored: {}",
                error
            )));
        }

        Ok(Addr::unchecked(input))
    }

    fn addr_canonicalize(&self, input: &str) -> StdResult<CanonicalAddr> {
        let input_bytes = input.as_bytes();
        if input_bytes.len() > 256 {
            // See MAX_LENGTH_HUMAN_ADDRESS in the VM.
            // In this case, the VM will refuse to read the input from the contract.
            // Stop here to allow handling the error in the contract.
            return Err(StdError::generic_err(
                "input too long for addr_canonicalize",
            ));
        }
        let send = build_region(input_bytes);
        let send_ptr = &*send as *const Region as u32;
        let canon = alloc(CANONICAL_ADDRESS_BUFFER_LENGTH);

        let result = unsafe { addr_canonicalize(send_ptr, canon as u32) };
        if result != 0 {
            let error = unsafe { consume_string_region_written_by_vm(result as *mut Region) };
            return Err(StdError::generic_err(format!(
                "addr_canonicalize errored: {}",
                error
            )));
        }

        let out = unsafe { consume_region(canon) };
        Ok(CanonicalAddr::from(out))
    }

    fn addr_humanize(&self, canonical: &CanonicalAddr) -> StdResult<Addr> {
        let send = build_region(&canonical);
        let send_ptr = &*send as *const Region as u32;
        let human = alloc(HUMAN_ADDRESS_BUFFER_LENGTH);

        let result = unsafe { addr_humanize(send_ptr, human as u32) };
        if result != 0 {
            let error = unsafe { consume_string_region_written_by_vm(result as *mut Region) };
            return Err(StdError::generic_err(format!(
                "addr_humanize errored: {}",
                error
            )));
        }

        let address = unsafe { consume_string_region_written_by_vm(human) };
        Ok(Addr::unchecked(address))
    }

    fn secp256k1_verify(
        &self,
        message_hash: &[u8],
        signature: &[u8],
        public_key: &[u8],
    ) -> Result<bool, VerificationError> {
        let hash_send = build_region(message_hash);
        let hash_send_ptr = &*hash_send as *const Region as u32;
        let sig_send = build_region(signature);
        let sig_send_ptr = &*sig_send as *const Region as u32;
        let pubkey_send = build_region(public_key);
        let pubkey_send_ptr = &*pubkey_send as *const Region as u32;

        let result = unsafe { secp256k1_verify(hash_send_ptr, sig_send_ptr, pubkey_send_ptr) };
        match result {
            0 => Ok(true),
            1 => Ok(false),
            2 => panic!("MessageTooLong must not happen. This is a bug in the VM."),
            3 => Err(VerificationError::InvalidHashFormat),
            4 => Err(VerificationError::InvalidSignatureFormat),
            5 => Err(VerificationError::InvalidPubkeyFormat),
            10 => Err(VerificationError::GenericErr),
            error_code => Err(VerificationError::unknown_err(error_code)),
        }
    }

    fn groth16_verify(
        &self,
        input: &[u8],
        proof: &[u8],
        vk: &[u8],
    ) -> Result<bool, VerificationError> {
        let input_send = build_region(input);
        let input_send_ptr = &*input_send as *const Region as u32;
        let proof_send = build_region(proof);
        let proof_send_ptr = &*proof_send as *const Region as u32;
        let vk_send = build_region(vk);
        let vk_send_ptr = &*vk_send as *const Region as u32;

        let result = unsafe { groth16_verify(input_send_ptr, proof_send_ptr, vk_send_ptr) };
        match result {
            0 => Ok(true),
            1 => Ok(false),
            2 => panic!("MessageTooLong must not happen. This is a bug in the VM."),
            10 => Err(VerificationError::GenericErr),
            error_code => Err(VerificationError::unknown_err(error_code)), // 3, 4 => verification and input hash error
        }
    }

    fn poseidon_hash(&self, inputs: &[&[u8]]) -> StdResult<Vec<u8>> {
        let msgs_encoded = encode_sections(inputs);
        let msgs_send = build_region(&msgs_encoded);
        let msgs_send_ptr = &*msgs_send as *const Region as u32;

        let hash = alloc(32); // hash

        let result = unsafe { poseidon_hash(msgs_send_ptr, hash as u32) };
        if result != 0 {
            let error = unsafe { consume_string_region_written_by_vm(result as *mut Region) };
            return Err(StdError::generic_err(format!(
                "poseidon_hash errored: {}",
                error
            )));
        }

        let out = unsafe { consume_region(hash) };
        Ok(out)
    }

    fn curve_hash(&self, input: &[u8]) -> StdResult<Vec<u8>> {
        let input_send = build_region(input);
        let input_send_ptr = &*input_send as *const Region as u32;

        let hash = alloc(32); // hash

        let result = unsafe { curve_hash(input_send_ptr, hash as u32) };
        if result != 0 {
            let error = unsafe { consume_string_region_written_by_vm(result as *mut Region) };
            return Err(StdError::generic_err(format!(
                "curve_hash errored: {}",
                error
            )));
        }

        let out = unsafe { consume_region(hash) };
        Ok(out)
    }

    fn secp256k1_recover_pubkey(
        &self,
        message_hash: &[u8],
        signature: &[u8],
        recover_param: u8,
    ) -> Result<Vec<u8>, RecoverPubkeyError> {
        let hash_send = build_region(message_hash);
        let hash_send_ptr = &*hash_send as *const Region as u32;
        let sig_send = build_region(signature);
        let sig_send_ptr = &*sig_send as *const Region as u32;

        let result =
            unsafe { secp256k1_recover_pubkey(hash_send_ptr, sig_send_ptr, recover_param.into()) };
        let error_code = from_high_half(result);
        let pubkey_ptr = from_low_half(result);
        match error_code {
            0 => {
                let pubkey = unsafe { consume_region(pubkey_ptr as *mut Region) };
                Ok(pubkey)
            }
            2 => panic!("MessageTooLong must not happen. This is a bug in the VM."),
            3 => Err(RecoverPubkeyError::InvalidHashFormat),
            4 => Err(RecoverPubkeyError::InvalidSignatureFormat),
            6 => Err(RecoverPubkeyError::InvalidRecoveryParam),
            error_code => Err(RecoverPubkeyError::unknown_err(error_code)),
        }
    }

    fn ed25519_verify(
        &self,
        message: &[u8],
        signature: &[u8],
        public_key: &[u8],
    ) -> Result<bool, VerificationError> {
        let msg_send = build_region(message);
        let msg_send_ptr = &*msg_send as *const Region as u32;
        let sig_send = build_region(signature);
        let sig_send_ptr = &*sig_send as *const Region as u32;
        let pubkey_send = build_region(public_key);
        let pubkey_send_ptr = &*pubkey_send as *const Region as u32;

        let result = unsafe { ed25519_verify(msg_send_ptr, sig_send_ptr, pubkey_send_ptr) };
        match result {
            0 => Ok(true),
            1 => Ok(false),
            2 => panic!("Error code 2 unused since CosmWasm 0.15. This is a bug in the VM."),
            3 => panic!("InvalidHashFormat must not happen. This is a bug in the VM."),
            4 => Err(VerificationError::InvalidSignatureFormat),
            5 => Err(VerificationError::InvalidPubkeyFormat),
            10 => Err(VerificationError::GenericErr),
            error_code => Err(VerificationError::unknown_err(error_code)),
        }
    }

    fn ed25519_batch_verify(
        &self,
        messages: &[&[u8]],
        signatures: &[&[u8]],
        public_keys: &[&[u8]],
    ) -> Result<bool, VerificationError> {
        let msgs_encoded = encode_sections(messages);
        let msgs_send = build_region(&msgs_encoded);
        let msgs_send_ptr = &*msgs_send as *const Region as u32;

        let sigs_encoded = encode_sections(signatures);
        let sig_sends = build_region(&sigs_encoded);
        let sigs_send_ptr = &*sig_sends as *const Region as u32;

        let pubkeys_encoded = encode_sections(public_keys);
        let pubkeys_send = build_region(&pubkeys_encoded);
        let pubkeys_send_ptr = &*pubkeys_send as *const Region as u32;

        let result =
            unsafe { ed25519_batch_verify(msgs_send_ptr, sigs_send_ptr, pubkeys_send_ptr) };
        match result {
            0 => Ok(true),
            1 => Ok(false),
            2 => panic!("Error code 2 unused since CosmWasm 0.15. This is a bug in the VM."),
            3 => panic!("InvalidHashFormat must not happen. This is a bug in the VM."),
            4 => Err(VerificationError::InvalidSignatureFormat),
            5 => Err(VerificationError::InvalidPubkeyFormat),
            10 => Err(VerificationError::GenericErr),
            error_code => Err(VerificationError::unknown_err(error_code)),
        }
    }

    fn debug(&self, message: &str) {
        // keep the boxes in scope, so we free it at the end (don't cast to pointers same line as build_region)
        let region = build_region(message.as_bytes());
        let region_ptr = region.as_ref() as *const Region as u32;
        unsafe { debug(region_ptr) };
    }
}

/// Takes a pointer to a Region and reads the data into a String.
/// This is for trusted string sources only.
unsafe fn consume_string_region_written_by_vm(from: *mut Region) -> String {
    let data = consume_region(from);
    // We trust the VM/chain to return correct UTF-8, so let's save some gas
    String::from_utf8_unchecked(data)
}

/// A stateless convenience wrapper around imports provided by the VM
pub struct ExternalQuerier {}

impl ExternalQuerier {
    pub fn new() -> ExternalQuerier {
        ExternalQuerier {}
    }
}

impl Querier for ExternalQuerier {
    fn raw_query(&self, bin_request: &[u8]) -> QuerierResult {
        let req = build_region(bin_request);
        let request_ptr = &*req as *const Region as u32;

        let response_ptr = unsafe { query_chain(request_ptr) };
        let response = unsafe { consume_region(response_ptr as *mut Region) };

        from_slice(&response).unwrap_or_else(|parsing_err| {
            SystemResult::Err(SystemError::InvalidResponse {
                error: parsing_err.to_string(),
                response: response.into(),
            })
        })
    }
}

#[cfg(feature = "abort")]
pub fn handle_panic(message: &str) {
    // keep the boxes in scope, so we free it at the end (don't cast to pointers same line as build_region)
    let region = build_region(message.as_bytes());
    let region_ptr = region.as_ref() as *const Region as u32;
    unsafe { abort(region_ptr) };
}
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
pub fn alloc(size: usize) -> *mut Region {
    let data: Vec<u8> = Vec::with_capacity(size);
    let data_ptr = data.as_ptr() as usize;

    let region = build_region_from_components(
        u32::try_from(data_ptr).expect("pointer doesn't fit in u32"),
        u32::try_from(data.capacity()).expect("capacity doesn't fit in u32"),
        0,
    );
    mem::forget(data);
    Box::into_raw(region)
}

/// Similar to alloc, but instead of creating a new vector it consumes an existing one and returns
/// a pointer to the Region (preventing the memory from being freed until explicitly called later).
///
/// The resulting Region has capacity = length, i.e. the buffer's capacity is ignored.
pub fn release_buffer(buffer: Vec<u8>) -> *mut Region {
    let region = build_region(&buffer);
    mem::forget(buffer);
    Box::into_raw(region)
}

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
pub unsafe fn consume_region(ptr: *mut Region) -> Vec<u8> {
    assert!(!ptr.is_null(), "Region pointer is null");
    let region = Box::from_raw(ptr);

    let region_start = region.offset as *mut u8;
    // This case is explicitely disallowed by Vec
    // "The pointer will never be null, so this type is null-pointer-optimized."
    assert!(!region_start.is_null(), "Region starts at null pointer");

    Vec::from_raw_parts(
        region_start,
        region.length as usize,
        region.capacity as usize,
    )
}

/// Returns a box of a Region, which can be sent over a call to extern
/// note that this DOES NOT take ownership of the data, and we MUST NOT consume_region
/// the resulting data.
/// The Box must be dropped (with scope), but not the data
pub fn build_region(data: &[u8]) -> Box<Region> {
    let data_ptr = data.as_ptr() as usize;
    build_region_from_components(
        u32::try_from(data_ptr).expect("pointer doesn't fit in u32"),
        u32::try_from(data.len()).expect("length doesn't fit in u32"),
        u32::try_from(data.len()).expect("length doesn't fit in u32"),
    )
}

fn build_region_from_components(offset: u32, capacity: u32, length: u32) -> Box<Region> {
    Box::new(Region {
        offset,
        capacity,
        length,
    })
}

/// Returns the address of the optional Region as an offset in linear memory,
/// or zero if not present
#[cfg(feature = "iterator")]
pub fn get_optional_region_address(region: &Option<&Box<Region>>) -> u32 {
    /// Returns the address of the Region as an offset in linear memory
    fn get_region_address(region: &Box<Region>) -> u32 {
        region.as_ref() as *const Region as u32
    }

    region.map(get_region_address).unwrap_or(0)
}
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
) {
    let left = left.into();
    let right = right.into();
    let max_rel_diff = Decimal::from_str(max_rel_diff).unwrap();

    let largest = std::cmp::max(left, right);
    let rel_diff = Decimal::from_ratio(left.abs_diff(right), largest);

    if rel_diff > max_rel_diff {
        match panic_msg {
            Some(panic_msg) => panic!(
                "assertion failed: `(left ≈ right)`\nleft: {}\nright: {}\nrelative difference: {}\nmax allowed relative difference: {}\n: {}",
                left, right, rel_diff, max_rel_diff, panic_msg
            ),
            None => panic!(
                "assertion failed: `(left ≈ right)`\nleft: {}\nright: {}\nrelative difference: {}\nmax allowed relative difference: {}\n",
                left, right, rel_diff, max_rel_diff
            ),
        }
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn assert_approx() {
        assert_approx_eq!(9_u32, 10_u32, "0.12");
        assert_approx_eq!(9_u64, 10_u64, "0.12");
        assert_approx_eq!(
            9_000_000_000_000_000_000_000_000_000_000_000_000_u128,
            10_000_000_000_000_000_000_000_000_000_000_000_000_u128,
            "0.10"
        );
    }

    #[test]
    fn assert_approx_with_vars() {
        let a = 66_u32;
        let b = 67_u32;
        assert_approx_eq!(a, b, "0.02");

        let a = 66_u64;
        let b = 67_u64;
        assert_approx_eq!(a, b, "0.02");

        let a = 66_u128;
        let b = 67_u128;
        assert_approx_eq!(a, b, "0.02");
    }

    #[test]
    #[should_panic(
        expected = "assertion failed: `(left ≈ right)`\nleft: 8\nright: 10\nrelative difference: 0.2\nmax allowed relative difference: 0.12\n"
    )]
    fn assert_approx_fail() {
        assert_approx_eq!(8_u32, 10_u32, "0.12");
    }

    #[test]
    #[should_panic(
        expected = "assertion failed: `(left ≈ right)`\nleft: 17\nright: 20\nrelative difference: 0.15\nmax allowed relative difference: 0.12\n: some extra info about the error: Foo(8)"
    )]
    fn assert_approx_with_custom_panic_msg() {
        let adjective = "extra";
        #[derive(Debug)]
        struct Foo(u32);
        assert_approx_eq!(
            17_u32,
            20_u32,
            "0.12",
            "some {adjective} {} about the error: {:?}",
            "info",
            Foo(8),
        );
    }
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
pub fn mock_dependencies() -> OwnedDeps<MockStorage, MockApi, MockQuerier, Empty> {
    OwnedDeps {
        storage: MockStorage::default(),
        api: MockApi::default(),
        querier: MockQuerier::default(),
        custom_query_type: PhantomData,
    }
}

/// Creates all external requirements that can be injected for unit tests.
///
/// It sets the given balance for the contract itself, nothing else.
pub fn mock_dependencies_with_balance(
    contract_balance: &[Coin],
) -> OwnedDeps<MockStorage, MockApi, MockQuerier, Empty> {
    mock_dependencies_with_balances(&[(MOCK_CONTRACT_ADDR, contract_balance)])
}

/// Initializes the querier along with the mock_dependencies.
/// Sets all balances provided (you must explicitly set contract balance if desired).
pub fn mock_dependencies_with_balances(
    balances: &[(&str, &[Coin])],
) -> OwnedDeps<MockStorage, MockApi, MockQuerier> {
    OwnedDeps {
        storage: MockStorage::default(),
        api: MockApi::default(),
        querier: MockQuerier::new(balances),
        custom_query_type: PhantomData,
    }
}

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
    fn default() -> Self {
        MockApi {
            canonical_length: CANONICAL_LENGTH,
            poseidon: Poseidon::new(),
        }
    }
}

impl Api for MockApi {
    fn addr_validate(&self, input: &str) -> StdResult<Addr> {
        let canonical = self.addr_canonicalize(input)?;
        let normalized = self.addr_humanize(&canonical)?;
        if input != normalized {
            return Err(StdError::generic_err(
                "Invalid input: address not normalized",
            ));
        }

        Ok(Addr::unchecked(input))
    }

    fn addr_canonicalize(&self, input: &str) -> StdResult<CanonicalAddr> {
        // Dummy input validation. This is more sophisticated for formats like bech32, where format and checksum are validated.
        if input.len() < 3 {
            return Err(StdError::generic_err(
                "Invalid input: human address too short",
            ));
        }
        if input.len() > self.canonical_length {
            return Err(StdError::generic_err(
                "Invalid input: human address too long",
            ));
        }

        // mimicks formats like hex or bech32 where different casings are valid for one address
        let normalized = input.to_lowercase();

        let mut out = Vec::from(normalized);

        // pad to canonical length with NULL bytes
        out.resize(self.canonical_length, 0x00);
        // content-dependent rotate followed by shuffle to destroy
        // the most obvious structure (https://github.com/CosmWasm/cosmwasm/issues/552)
        let rotate_by = digit_sum(&out) % self.canonical_length;
        out.rotate_left(rotate_by);
        for _ in 0..SHUFFLES_ENCODE {
            out = riffle_shuffle(&out);
        }
        Ok(out.into())
    }

    fn addr_humanize(&self, canonical: &CanonicalAddr) -> StdResult<Addr> {
        if canonical.len() != self.canonical_length {
            return Err(StdError::generic_err(
                "Invalid input: canonical address length not correct",
            ));
        }

        let mut tmp: Vec<u8> = canonical.clone().into();
        // Shuffle two more times which restored the original value (24 elements are back to original after 20 rounds)
        for _ in 0..SHUFFLES_DECODE {
            tmp = riffle_shuffle(&tmp);
        }
        // Rotate back
        let rotate_by = digit_sum(&tmp) % self.canonical_length;
        tmp.rotate_right(rotate_by);
        // Remove NULL bytes (i.e. the padding)
        let trimmed = tmp.into_iter().filter(|&x| x != 0x00).collect();
        // decode UTF-8 bytes into string
        let human = String::from_utf8(trimmed)?;
        Ok(Addr::unchecked(human))
    }

    fn secp256k1_verify(
        &self,
        message_hash: &[u8],
        signature: &[u8],
        public_key: &[u8],
    ) -> Result<bool, VerificationError> {
        Ok(cosmwasm_crypto::secp256k1_verify(
            message_hash,
            signature,
            public_key,
        )?)
    }

    fn poseidon_hash(&self, inputs: &[&[u8]]) -> StdResult<Vec<u8>> {
        match self.poseidon.hash(inputs) {
            Ok(hash) => Ok(hash.to_vec()),
            Err(_) => return Err(StdError::generic_err("poseidon hash error")),
        }
    }

    fn curve_hash(&self, input: &[u8]) -> StdResult<Vec<u8>> {
        Ok(cosmwasm_crypto::curve_hash(input))
    }

    fn groth16_verify(
        &self,
        input: &[u8],
        proof: &[u8],
        vk: &[u8],
    ) -> Result<bool, VerificationError> {
        cosmwasm_crypto::groth16_verify(input, proof, vk)
            .map_err(|_| VerificationError::GenericErr {})
    }

    fn secp256k1_recover_pubkey(
        &self,
        message_hash: &[u8],
        signature: &[u8],
        recovery_param: u8,
    ) -> Result<Vec<u8>, RecoverPubkeyError> {
        let pubkey =
            cosmwasm_crypto::secp256k1_recover_pubkey(message_hash, signature, recovery_param)?;
        Ok(pubkey.to_vec())
    }

    fn ed25519_verify(
        &self,
        message: &[u8],
        signature: &[u8],
        public_key: &[u8],
    ) -> Result<bool, VerificationError> {
        Ok(cosmwasm_crypto::ed25519_verify(
            message, signature, public_key,
        )?)
    }

    fn ed25519_batch_verify(
        &self,
        messages: &[&[u8]],
        signatures: &[&[u8]],
        public_keys: &[&[u8]],
    ) -> Result<bool, VerificationError> {
        Ok(cosmwasm_crypto::ed25519_batch_verify(
            messages,
            signatures,
            public_keys,
        )?)
    }

    fn debug(&self, message: &str) {
        println!("{}", message);
    }
}

/// Returns a default enviroment with height, time, chain_id, and contract address
/// You can submit as is to most contracts, or modify height/time if you want to
/// test for expiration.
///
/// This is intended for use in test code only.
pub fn mock_env() -> Env {
    Env {
        block: BlockInfo {
            height: 12_345,
            time: Timestamp::from_nanos(1_571_797_419_879_305_533),
            chain_id: "cosmos-testnet-14002".to_string(),
        },
        transaction: Some(TransactionInfo { index: 3 }),
        contract: ContractInfo {
            address: Addr::unchecked(MOCK_CONTRACT_ADDR),
        },
    }
}

/// Just set sender and funds for the message.
/// This is intended for use in test code only.
pub fn mock_info(sender: &str, funds: &[Coin]) -> MessageInfo {
    MessageInfo {
        sender: Addr::unchecked(sender),
        funds: funds.to_vec(),
    }
}

/// Creates an IbcChannel for testing. You set a few key parameters for handshaking,
/// If you want to set more, use this as a default and mutate other fields
#[cfg(feature = "stargate")]
pub fn mock_ibc_channel(my_channel_id: &str, order: IbcOrder, version: &str) -> IbcChannel {
    IbcChannel {
        endpoint: IbcEndpoint {
            port_id: "my_port".to_string(),
            channel_id: my_channel_id.to_string(),
        },
        counterparty_endpoint: IbcEndpoint {
            port_id: "their_port".to_string(),
            channel_id: "channel-7".to_string(),
        },
        order,
        version: version.to_string(),
        connection_id: "connection-2".to_string(),
    }
}

/// Creates a IbcChannelOpenMsg::OpenInit for testing ibc_channel_open.
#[cfg(feature = "stargate")]
pub fn mock_ibc_channel_open_init(
    my_channel_id: &str,
    order: IbcOrder,
    version: &str,
) -> IbcChannelOpenMsg {
    IbcChannelOpenMsg::new_init(mock_ibc_channel(my_channel_id, order, version))
}

/// Creates a IbcChannelOpenMsg::OpenTry for testing ibc_channel_open.
#[cfg(feature = "stargate")]
pub fn mock_ibc_channel_open_try(
    my_channel_id: &str,
    order: IbcOrder,
    version: &str,
) -> IbcChannelOpenMsg {
    IbcChannelOpenMsg::new_try(mock_ibc_channel(my_channel_id, order, version), version)
}

/// Creates a IbcChannelConnectMsg::ConnectAck for testing ibc_channel_connect.
#[cfg(feature = "stargate")]
pub fn mock_ibc_channel_connect_ack(
    my_channel_id: &str,
    order: IbcOrder,
    version: &str,
) -> IbcChannelConnectMsg {
    IbcChannelConnectMsg::new_ack(mock_ibc_channel(my_channel_id, order, version), version)
}

/// Creates a IbcChannelConnectMsg::ConnectConfirm for testing ibc_channel_connect.
#[cfg(feature = "stargate")]
pub fn mock_ibc_channel_connect_confirm(
    my_channel_id: &str,
    order: IbcOrder,
    version: &str,
) -> IbcChannelConnectMsg {
    IbcChannelConnectMsg::new_confirm(mock_ibc_channel(my_channel_id, order, version))
}

/// Creates a IbcChannelCloseMsg::CloseInit for testing ibc_channel_close.
#[cfg(feature = "stargate")]
pub fn mock_ibc_channel_close_init(
    my_channel_id: &str,
    order: IbcOrder,
    version: &str,
) -> IbcChannelCloseMsg {
    IbcChannelCloseMsg::new_init(mock_ibc_channel(my_channel_id, order, version))
}

/// Creates a IbcChannelCloseMsg::CloseConfirm for testing ibc_channel_close.
#[cfg(feature = "stargate")]
pub fn mock_ibc_channel_close_confirm(
    my_channel_id: &str,
    order: IbcOrder,
    version: &str,
) -> IbcChannelCloseMsg {
    IbcChannelCloseMsg::new_confirm(mock_ibc_channel(my_channel_id, order, version))
}

/// Creates a IbcPacketReceiveMsg for testing ibc_packet_receive. You set a few key parameters that are
/// often parsed. If you want to set more, use this as a default and mutate other fields
#[cfg(feature = "stargate")]
pub fn mock_ibc_packet_recv(
    my_channel_id: &str,
    data: &impl Serialize,
) -> StdResult<IbcPacketReceiveMsg> {
    Ok(IbcPacketReceiveMsg::new(
        IbcPacket {
            data: to_binary(data)?,
            src: IbcEndpoint {
                port_id: "their-port".to_string(),
                channel_id: "channel-1234".to_string(),
            },
            dest: IbcEndpoint {
                port_id: "our-port".to_string(),
                channel_id: my_channel_id.into(),
            },
            sequence: 27,
            timeout: IbcTimeoutBlock {
                revision: 1,
                height: 12345678,
            }
            .into(),
        },
        #[cfg(feature = "ibc3")]
        Addr::unchecked("relayer"),
    ))
}

/// Creates a IbcPacket for testing ibc_packet_{ack,timeout}. You set a few key parameters that are
/// often parsed. If you want to set more, use this as a default and mutate other fields.
/// The difference from mock_ibc_packet_recv is if `my_channel_id` is src or dest.
#[cfg(feature = "stargate")]
fn mock_ibc_packet(my_channel_id: &str, data: &impl Serialize) -> StdResult<IbcPacket> {
    Ok(IbcPacket {
        data: to_binary(data)?,
        src: IbcEndpoint {
            port_id: "their-port".to_string(),
            channel_id: my_channel_id.into(),
        },
        dest: IbcEndpoint {
            port_id: "our-port".to_string(),
            channel_id: "channel-1234".to_string(),
        },
        sequence: 29,
        timeout: IbcTimeoutBlock {
            revision: 1,
            height: 432332552,
        }
        .into(),
    })
}

/// Creates a IbcPacketAckMsg for testing ibc_packet_ack. You set a few key parameters that are
/// often parsed. If you want to set more, use this as a default and mutate other fields.
/// The difference from mock_ibc_packet_recv is if `my_channel_id` is src or dest.
#[cfg(feature = "stargate")]
pub fn mock_ibc_packet_ack(
    my_channel_id: &str,
    data: &impl Serialize,
    ack: IbcAcknowledgement,
) -> StdResult<IbcPacketAckMsg> {
    let packet = mock_ibc_packet(my_channel_id, data)?;

    Ok(IbcPacketAckMsg::new(
        ack,
        packet,
        #[cfg(feature = "ibc3")]
        Addr::unchecked("relayer"),
    ))
}

/// Creates a IbcPacketTimeoutMsg for testing ibc_packet_timeout. You set a few key parameters that are
/// often parsed. If you want to set more, use this as a default and mutate other fields.
/// The difference from mock_ibc_packet_recv is if `my_channel_id` is src or dest./
#[cfg(feature = "stargate")]
pub fn mock_ibc_packet_timeout(
    my_channel_id: &str,
    data: &impl Serialize,
) -> StdResult<IbcPacketTimeoutMsg> {
    let packet = mock_ibc_packet(my_channel_id, data)?;
    Ok(IbcPacketTimeoutMsg::new(
        packet,
        #[cfg(feature = "ibc3")]
        Addr::unchecked("relayer"),
    ))
}

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
    pub fn new(balances: &[(&str, &[Coin])]) -> Self {
        MockQuerier {
            bank: BankQuerier::new(balances),
            #[cfg(feature = "staking")]
            staking: StakingQuerier::default(),
            wasm: WasmQuerier::default(),
            // strange argument notation suggested as a workaround here: https://github.com/rust-lang/rust/issues/41078#issuecomment-294296365
            custom_handler: Box::from(|_: &_| -> MockQuerierCustomHandlerResult {
                SystemResult::Err(SystemError::UnsupportedRequest {
                    kind: "custom".to_string(),
                })
            }),
        }
    }

    // set a new balance for the given address and return the old balance
    pub fn update_balance(
        &mut self,
        addr: impl Into<String>,
        balance: Vec<Coin>,
    ) -> Option<Vec<Coin>> {
        self.bank.update_balance(addr, balance)
    }

    #[cfg(feature = "staking")]
    pub fn update_staking(
        &mut self,
        denom: &str,
        validators: &[crate::query::Validator],
        delegations: &[crate::query::FullDelegation],
    ) {
        self.staking = StakingQuerier::new(denom, validators, delegations);
    }

    pub fn update_wasm<WH: 'static>(&mut self, handler: WH)
    where
        WH: Fn(&WasmQuery) -> QuerierResult,
    {
        self.wasm.update_handler(handler)
    }

    pub fn with_custom_handler<CH: 'static>(mut self, handler: CH) -> Self
    where
        CH: Fn(&C) -> MockQuerierCustomHandlerResult,
    {
        self.custom_handler = Box::from(handler);
        self
    }
}

impl Default for MockQuerier {
    fn default() -> Self {
        MockQuerier::new(&[])
    }
}

impl<C: CustomQuery + DeserializeOwned> Querier for MockQuerier<C> {
    fn raw_query(&self, bin_request: &[u8]) -> QuerierResult {
        let request: QueryRequest<C> = match from_slice(bin_request) {
            Ok(v) => v,
            Err(e) => {
                return SystemResult::Err(SystemError::InvalidRequest {
                    error: format!("Parsing query request: {}", e),
                    request: bin_request.into(),
                })
            }
        };
        self.handle_query(&request)
    }
}

impl<C: CustomQuery + DeserializeOwned> MockQuerier<C> {
    pub fn handle_query(&self, request: &QueryRequest<C>) -> QuerierResult {
        match &request {
            QueryRequest::Bank(bank_query) => self.bank.query(bank_query),
            QueryRequest::Custom(custom_query) => (*self.custom_handler)(custom_query),
            #[cfg(feature = "staking")]
            QueryRequest::Staking(staking_query) => self.staking.query(staking_query),
            QueryRequest::Wasm(msg) => self.wasm.query(msg),
            #[cfg(feature = "stargate")]
            QueryRequest::Stargate { .. } => SystemResult::Err(SystemError::UnsupportedRequest {
                kind: "Stargate".to_string(),
            }),
            #[cfg(feature = "stargate")]
            QueryRequest::Ibc(_) => SystemResult::Err(SystemError::UnsupportedRequest {
                kind: "Ibc".to_string(),
            }),
        }
    }
}

struct WasmQuerier {
    /// A handler to handle Wasm queries. This is set to a dummy handler that
    /// always errors by default. Update it via `with_custom_handler`.
    ///
    /// Use box to avoid the need of generic type.
    handler: Box<dyn for<'a> Fn(&'a WasmQuery) -> QuerierResult>,
}

impl WasmQuerier {
    fn new(handler: Box<dyn for<'a> Fn(&'a WasmQuery) -> QuerierResult>) -> Self {
        Self { handler }
    }

    fn update_handler<WH: 'static>(&mut self, handler: WH)
    where
        WH: Fn(&WasmQuery) -> QuerierResult,
    {
        self.handler = Box::from(handler)
    }

    fn query(&self, request: &WasmQuery) -> QuerierResult {
        (*self.handler)(request)
    }
}

impl Default for WasmQuerier {
    fn default() -> Self {
        let handler = Box::from(|request: &WasmQuery| -> QuerierResult {
            let addr = match request {
                WasmQuery::Smart { contract_addr, .. } => contract_addr,
                WasmQuery::Raw { contract_addr, .. } => contract_addr,
                WasmQuery::ContractInfo { contract_addr, .. } => contract_addr,
            }
            .clone();
            SystemResult::Err(SystemError::NoSuchContract { addr })
        });
        Self::new(handler)
    }
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
    pub fn new(balances: &[(&str, &[Coin])]) -> Self {
        let balances: HashMap<_, _> = balances
            .iter()
            .map(|(s, c)| (s.to_string(), c.to_vec()))
            .collect();

        BankQuerier {
            supplies: Self::calculate_supplies(&balances),
            balances,
        }
    }

    pub fn update_balance(
        &mut self,
        addr: impl Into<String>,
        balance: Vec<Coin>,
    ) -> Option<Vec<Coin>> {
        let result = self.balances.insert(addr.into(), balance);
        self.supplies = Self::calculate_supplies(&self.balances);

        result
    }

    fn calculate_supplies(balances: &HashMap<String, Vec<Coin>>) -> HashMap<String, Uint128> {
        let mut supplies = HashMap::new();

        let all_coins = balances.iter().flat_map(|(_, coins)| coins);

        for coin in all_coins {
            *supplies
                .entry(coin.denom.clone())
                .or_insert_with(Uint128::zero) += coin.amount;
        }

        supplies
    }

    pub fn query(&self, request: &BankQuery) -> QuerierResult {
        let contract_result: ContractResult<Binary> = match request {
            #[cfg(feature = "cosmwasm_1_1")]
            BankQuery::Supply { denom } => {
                let amount = self
                    .supplies
                    .get(denom)
                    .cloned()
                    .unwrap_or_else(Uint128::zero);
                let bank_res = SupplyResponse {
                    amount: Coin {
                        amount,
                        denom: denom.to_string(),
                    },
                };
                to_binary(&bank_res).into()
            }
            BankQuery::Balance { address, denom } => {
                // proper error on not found, serialize result on found
                let amount = self
                    .balances
                    .get(address)
                    .and_then(|v| v.iter().find(|c| &c.denom == denom).map(|c| c.amount))
                    .unwrap_or_default();
                let bank_res = BalanceResponse {
                    amount: Coin {
                        amount,
                        denom: denom.to_string(),
                    },
                };
                to_binary(&bank_res).into()
            }
            BankQuery::AllBalances { address } => {
                // proper error on not found, serialize result on found
                let bank_res = AllBalanceResponse {
                    amount: self.balances.get(address).cloned().unwrap_or_default(),
                };
                to_binary(&bank_res).into()
            }
        };
        // system result is always ok in the mock implementation
        SystemResult::Ok(contract_result)
    }
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
    pub fn new(denom: &str, validators: &[Validator], delegations: &[FullDelegation]) -> Self {
        StakingQuerier {
            denom: denom.to_string(),
            validators: validators.to_vec(),
            delegations: delegations.to_vec(),
        }
    }

    pub fn query(&self, request: &StakingQuery) -> QuerierResult {
        let contract_result: ContractResult<Binary> = match request {
            StakingQuery::BondedDenom {} => {
                let res = BondedDenomResponse {
                    denom: self.denom.clone(),
                };
                to_binary(&res).into()
            }
            StakingQuery::AllValidators {} => {
                let res = AllValidatorsResponse {
                    validators: self.validators.clone(),
                };
                to_binary(&res).into()
            }
            StakingQuery::Validator { address } => {
                let validator: Option<Validator> = self
                    .validators
                    .iter()
                    .find(|validator| validator.address == *address)
                    .cloned();
                let res = ValidatorResponse { validator };
                to_binary(&res).into()
            }
            StakingQuery::AllDelegations { delegator } => {
                let delegations: Vec<_> = self
                    .delegations
                    .iter()
                    .filter(|d| d.delegator.as_str() == delegator)
                    .cloned()
                    .map(|d| d.into())
                    .collect();
                let res = AllDelegationsResponse { delegations };
                to_binary(&res).into()
            }
            StakingQuery::Delegation {
                delegator,
                validator,
            } => {
                let delegation = self
                    .delegations
                    .iter()
                    .find(|d| d.delegator.as_str() == delegator && d.validator == *validator);
                let res = DelegationResponse {
                    delegation: delegation.cloned(),
                };
                to_binary(&res).into()
            }
        };
        // system result is always ok in the mock implementation
        SystemResult::Ok(contract_result)
    }
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
pub fn riffle_shuffle<T: Clone>(input: &[T]) -> Vec<T> {
    assert!(
        input.len() % 2 == 0,
        "Method only defined for even number of elements"
    );
    let mid = input.len() / 2;
    let (left, right) = input.split_at(mid);
    let mut out = Vec::<T>::with_capacity(input.len());
    for i in 0..mid {
        out.push(right[i].clone());
        out.push(left[i].clone());
    }
    out
}

pub fn digit_sum(input: &[u8]) -> usize {
    input.iter().fold(0, |sum, val| sum + (*val as usize))
}

/// Only for test code. This bypasses assertions in new, allowing us to create _*
/// Attributes to simulate responses from the blockchain
pub fn mock_wasmd_attr(key: impl Into<String>, value: impl Into<String>) -> Attribute {
    Attribute {
        key: key.into(),
        value: value.into(),
    }
}

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
    fn mock_info_works() {
        let info = mock_info("my name", &coins(100, "atom"));
        assert_eq!(
            info,
            MessageInfo {
                sender: Addr::unchecked("my name"),
                funds: vec![Coin {
                    amount: 100u128.into(),
                    denom: "atom".into(),
                }]
            }
        );
    }

    #[test]
    fn addr_validate_works() {
        let api = MockApi::default();

        // valid
        let addr = api.addr_validate("foobar123").unwrap();
        assert_eq!(addr, "foobar123");

        // invalid: too short
        api.addr_validate("").unwrap_err();
        // invalid: not normalized
        api.addr_validate("Foobar123").unwrap_err();
        api.addr_validate("FOOBAR123").unwrap_err();
    }

    #[test]
    fn addr_canonicalize_works() {
        let api = MockApi::default();

        api.addr_canonicalize("foobar123").unwrap();

        // is case insensitive
        let data1 = api.addr_canonicalize("foo123").unwrap();
        let data2 = api.addr_canonicalize("FOO123").unwrap();
        assert_eq!(data1, data2);
    }

    #[test]
    fn canonicalize_and_humanize_restores_original() {
        let api = MockApi::default();

        // simple
        let original = String::from("shorty");
        let canonical = api.addr_canonicalize(&original).unwrap();
        let recovered = api.addr_humanize(&canonical).unwrap();
        assert_eq!(recovered, original);

        // normalizes input
        let original = String::from("CosmWasmChef");
        let canonical = api.addr_canonicalize(&original).unwrap();
        let recovered = api.addr_humanize(&canonical).unwrap();
        assert_eq!(recovered, "cosmwasmchef");
    }

    #[test]
    #[should_panic(expected = "address too short")]
    fn addr_canonicalize_min_input_length() {
        let api = MockApi::default();
        let human = String::from("1");
        let _ = api.addr_canonicalize(&human).unwrap();
    }

    #[test]
    #[should_panic(expected = "address too long")]
    fn addr_canonicalize_max_input_length() {
        let api = MockApi::default();
        let human =
            String::from("some-extremely-long-address-not-supported-by-this-api-longer-than-54");
        let _ = api.addr_canonicalize(&human).unwrap();
    }

    #[test]
    #[should_panic(expected = "length not correct")]
    fn addr_humanize_input_length() {
        let api = MockApi::default();
        let input = CanonicalAddr::from(vec![61; 11]);
        api.addr_humanize(&input).unwrap();
    }

    // Basic "works" test. Exhaustive tests on VM's side (packages/vm/src/imports.rs)
    #[test]
    fn secp256k1_verify_works() {
        let api = MockApi::default();

        let hash = hex::decode(SECP256K1_MSG_HASH_HEX).unwrap();
        let signature = hex::decode(SECP256K1_SIG_HEX).unwrap();
        let public_key = hex::decode(SECP256K1_PUBKEY_HEX).unwrap();

        assert!(api
            .secp256k1_verify(&hash, &signature, &public_key)
            .unwrap());
    }

    // Basic "fails" test. Exhaustive tests on VM's side (packages/vm/src/imports.rs)
    #[test]
    fn secp256k1_verify_fails() {
        let api = MockApi::default();

        let mut hash = hex::decode(SECP256K1_MSG_HASH_HEX).unwrap();
        // alter hash
        hash[0] ^= 0x01;
        let signature = hex::decode(SECP256K1_SIG_HEX).unwrap();
        let public_key = hex::decode(SECP256K1_PUBKEY_HEX).unwrap();

        assert!(!api
            .secp256k1_verify(&hash, &signature, &public_key)
            .unwrap());
    }

    // Basic "errors" test. Exhaustive tests on VM's side (packages/vm/src/imports.rs)
    #[test]
    fn secp256k1_verify_errs() {
        let api = MockApi::default();

        let hash = hex::decode(SECP256K1_MSG_HASH_HEX).unwrap();
        let signature = hex::decode(SECP256K1_SIG_HEX).unwrap();
        let public_key = vec![];

        let res = api.secp256k1_verify(&hash, &signature, &public_key);
        assert_eq!(res.unwrap_err(), VerificationError::InvalidPubkeyFormat);
    }

    #[test]
    fn secp256k1_recover_pubkey_works() {
        let api = MockApi::default();

        // https://gist.github.com/webmaster128/130b628d83621a33579751846699ed15
        let hash = hex!("5ae8317d34d1e595e3fa7247db80c0af4320cce1116de187f8f7e2e099c0d8d0");
        let signature = hex!("45c0b7f8c09a9e1f1cea0c25785594427b6bf8f9f878a8af0b1abbb48e16d0920d8becd0c220f67c51217eecfd7184ef0732481c843857e6bc7fc095c4f6b788");
        let recovery_param = 1;
        let expected = hex!("044a071e8a6e10aada2b8cf39fa3b5fb3400b04e99ea8ae64ceea1a977dbeaf5d5f8c8fbd10b71ab14cd561f7df8eb6da50f8a8d81ba564342244d26d1d4211595");

        let pubkey = api
            .secp256k1_recover_pubkey(&hash, &signature, recovery_param)
            .unwrap();
        assert_eq!(pubkey, expected);
    }

    #[test]
    fn secp256k1_recover_pubkey_fails_for_wrong_recovery_param() {
        let api = MockApi::default();

        // https://gist.github.com/webmaster128/130b628d83621a33579751846699ed15
        let hash = hex!("5ae8317d34d1e595e3fa7247db80c0af4320cce1116de187f8f7e2e099c0d8d0");
        let signature = hex!("45c0b7f8c09a9e1f1cea0c25785594427b6bf8f9f878a8af0b1abbb48e16d0920d8becd0c220f67c51217eecfd7184ef0732481c843857e6bc7fc095c4f6b788");
        let _recovery_param = 1;
        let expected = hex!("044a071e8a6e10aada2b8cf39fa3b5fb3400b04e99ea8ae64ceea1a977dbeaf5d5f8c8fbd10b71ab14cd561f7df8eb6da50f8a8d81ba564342244d26d1d4211595");

        // Wrong recovery param leads to different pubkey
        let pubkey = api.secp256k1_recover_pubkey(&hash, &signature, 0).unwrap();
        assert_eq!(pubkey.len(), 65);
        assert_ne!(pubkey, expected);

        // Invalid recovery param leads to error
        let result = api.secp256k1_recover_pubkey(&hash, &signature, 42);
        match result.unwrap_err() {
            RecoverPubkeyError::InvalidRecoveryParam => {}
            err => panic!("Unexpected error: {:?}", err),
        }
    }

    #[test]
    fn secp256k1_recover_pubkey_fails_for_wrong_hash() {
        let api = MockApi::default();

        // https://gist.github.com/webmaster128/130b628d83621a33579751846699ed15
        let hash = hex!("5ae8317d34d1e595e3fa7247db80c0af4320cce1116de187f8f7e2e099c0d8d0");
        let signature = hex!("45c0b7f8c09a9e1f1cea0c25785594427b6bf8f9f878a8af0b1abbb48e16d0920d8becd0c220f67c51217eecfd7184ef0732481c843857e6bc7fc095c4f6b788");
        let recovery_param = 1;
        let expected = hex!("044a071e8a6e10aada2b8cf39fa3b5fb3400b04e99ea8ae64ceea1a977dbeaf5d5f8c8fbd10b71ab14cd561f7df8eb6da50f8a8d81ba564342244d26d1d4211595");

        // Wrong hash
        let mut corrupted_hash = hash;
        corrupted_hash[0] ^= 0x01;
        let pubkey = api
            .secp256k1_recover_pubkey(&corrupted_hash, &signature, recovery_param)
            .unwrap();
        assert_eq!(pubkey.len(), 65);
        assert_ne!(pubkey, expected);

        // Malformed hash
        let mut malformed_hash = hash.to_vec();
        malformed_hash.push(0x8a);
        let result = api.secp256k1_recover_pubkey(&malformed_hash, &signature, recovery_param);
        match result.unwrap_err() {
            RecoverPubkeyError::InvalidHashFormat => {}
            err => panic!("Unexpected error: {:?}", err),
        }
    }

    // Basic "works" test. Exhaustive tests on VM's side (packages/vm/src/imports.rs)
    #[test]
    fn ed25519_verify_works() {
        let api = MockApi::default();

        let msg = hex::decode(ED25519_MSG_HEX).unwrap();
        let signature = hex::decode(ED25519_SIG_HEX).unwrap();
        let public_key = hex::decode(ED25519_PUBKEY_HEX).unwrap();

        assert!(api.ed25519_verify(&msg, &signature, &public_key).unwrap());
    }

    // Basic "fails" test. Exhaustive tests on VM's side (packages/vm/src/imports.rs)
    #[test]
    fn ed25519_verify_fails() {
        let api = MockApi::default();

        let mut msg = hex::decode(ED25519_MSG_HEX).unwrap();
        // alter msg
        msg[0] ^= 0x01;
        let signature = hex::decode(ED25519_SIG_HEX).unwrap();
        let public_key = hex::decode(ED25519_PUBKEY_HEX).unwrap();

        assert!(!api.ed25519_verify(&msg, &signature, &public_key).unwrap());
    }

    // Basic "errors" test. Exhaustive tests on VM's side (packages/vm/src/imports.rs)
    #[test]
    fn ed25519_verify_errs() {
        let api = MockApi::default();

        let msg = hex::decode(ED25519_MSG_HEX).unwrap();
        let signature = hex::decode(ED25519_SIG_HEX).unwrap();
        let public_key = vec![];

        let res = api.ed25519_verify(&msg, &signature, &public_key);
        assert_eq!(res.unwrap_err(), VerificationError::InvalidPubkeyFormat);
    }

    // Basic "works" test.
    #[test]
    fn ed25519_batch_verify_works() {
        let api = MockApi::default();

        let msg = hex::decode(ED25519_MSG_HEX).unwrap();
        let signature = hex::decode(ED25519_SIG_HEX).unwrap();
        let public_key = hex::decode(ED25519_PUBKEY_HEX).unwrap();

        let msgs: Vec<&[u8]> = vec![&msg];
        let signatures: Vec<&[u8]> = vec![&signature];
        let public_keys: Vec<&[u8]> = vec![&public_key];

        assert!(api
            .ed25519_batch_verify(&msgs, &signatures, &public_keys)
            .unwrap());
    }

    // Basic "fails" test.
    #[test]
    fn ed25519_batch_verify_fails() {
        let api = MockApi::default();

        let mut msg = hex::decode(ED25519_MSG_HEX).unwrap();
        // alter msg
        msg[0] ^= 0x01;
        let signature = hex::decode(ED25519_SIG_HEX).unwrap();
        let public_key = hex::decode(ED25519_PUBKEY_HEX).unwrap();

        let msgs: Vec<&[u8]> = vec![&msg];
        let signatures: Vec<&[u8]> = vec![&signature];
        let public_keys: Vec<&[u8]> = vec![&public_key];

        assert!(!api
            .ed25519_batch_verify(&msgs, &signatures, &public_keys)
            .unwrap());
    }

    // Basic "errors" test.
    #[test]
    fn ed25519_batch_verify_errs() {
        let api = MockApi::default();

        let msg = hex::decode(ED25519_MSG_HEX).unwrap();
        let signature = hex::decode(ED25519_SIG_HEX).unwrap();
        let public_key: Vec<u8> = vec![0u8; 0];

        let msgs: Vec<&[u8]> = vec![msg.as_slice()];
        let signatures: Vec<&[u8]> = vec![signature.as_slice()];
        let public_keys: Vec<&[u8]> = vec![&public_key];

        let res = api.ed25519_batch_verify(&msgs, &signatures, &public_keys);
        assert_eq!(res.unwrap_err(), VerificationError::InvalidPubkeyFormat);
    }

    #[cfg(feature = "cosmwasm_1_1")]
    #[test]
    fn bank_querier_supply() {
        let addr1 = String::from("foo");
        let balance1 = vec![coin(123, "ELF"), coin(777, "FLY")];

        let addr2 = String::from("bar");
        let balance2 = coins(321, "ELF");

        let bank = BankQuerier::new(&[(&addr1, &balance1), (&addr2, &balance2)]);

        let elf = bank
            .query(&BankQuery::Supply {
                denom: "ELF".to_string(),
            })
            .unwrap()
            .unwrap();
        let res: SupplyResponse = from_binary(&elf).unwrap();
        assert_eq!(res.amount, coin(444, "ELF"));

        let fly = bank
            .query(&BankQuery::Supply {
                denom: "FLY".to_string(),
            })
            .unwrap()
            .unwrap();
        let res: SupplyResponse = from_binary(&fly).unwrap();
        assert_eq!(res.amount, coin(777, "FLY"));

        // if a denom does not exist, should return zero amount, instead of throwing an error
        let atom = bank
            .query(&BankQuery::Supply {
                denom: "ATOM".to_string(),
            })
            .unwrap()
            .unwrap();
        let res: SupplyResponse = from_binary(&atom).unwrap();
        assert_eq!(res.amount, coin(0, "ATOM"));
    }

    #[test]
    fn bank_querier_all_balances() {
        let addr = String::from("foobar");
        let balance = vec![coin(123, "ELF"), coin(777, "FLY")];
        let bank = BankQuerier::new(&[(&addr, &balance)]);

        let all = bank
            .query(&BankQuery::AllBalances { address: addr })
            .unwrap()
            .unwrap();
        let res: AllBalanceResponse = from_binary(&all).unwrap();
        assert_eq!(&res.amount, &balance);
    }

    #[test]
    fn bank_querier_one_balance() {
        let addr = String::from("foobar");
        let balance = vec![coin(123, "ELF"), coin(777, "FLY")];
        let bank = BankQuerier::new(&[(&addr, &balance)]);

        // one match
        let fly = bank
            .query(&BankQuery::Balance {
                address: addr.clone(),
                denom: "FLY".to_string(),
            })
            .unwrap()
            .unwrap();
        let res: BalanceResponse = from_binary(&fly).unwrap();
        assert_eq!(res.amount, coin(777, "FLY"));

        // missing denom
        let miss = bank
            .query(&BankQuery::Balance {
                address: addr,
                denom: "MISS".to_string(),
            })
            .unwrap()
            .unwrap();
        let res: BalanceResponse = from_binary(&miss).unwrap();
        assert_eq!(res.amount, coin(0, "MISS"));
    }

    #[test]
    fn bank_querier_missing_account() {
        let addr = String::from("foobar");
        let balance = vec![coin(123, "ELF"), coin(777, "FLY")];
        let bank = BankQuerier::new(&[(&addr, &balance)]);

        // all balances on empty account is empty vec
        let all = bank
            .query(&BankQuery::AllBalances {
                address: String::from("elsewhere"),
            })
            .unwrap()
            .unwrap();
        let res: AllBalanceResponse = from_binary(&all).unwrap();
        assert_eq!(res.amount, vec![]);

        // any denom on balances on empty account is empty coin
        let miss = bank
            .query(&BankQuery::Balance {
                address: String::from("elsewhere"),
                denom: "ELF".to_string(),
            })
            .unwrap()
            .unwrap();
        let res: BalanceResponse = from_binary(&miss).unwrap();
        assert_eq!(res.amount, coin(0, "ELF"));
    }

    #[cfg(feature = "staking")]
    #[test]
    fn staking_querier_all_validators() {
        let val1 = Validator {
            address: String::from("validator-one"),
            commission: Decimal::percent(1),
            max_commission: Decimal::percent(3),
            max_change_rate: Decimal::percent(1),
        };
        let val2 = Validator {
            address: String::from("validator-two"),
            commission: Decimal::permille(15),
            max_commission: Decimal::permille(40),
            max_change_rate: Decimal::permille(5),
        };

        let staking = StakingQuerier::new("ustake", &[val1.clone(), val2.clone()], &[]);

        // one match
        let raw = staking
            .query(&StakingQuery::AllValidators {})
            .unwrap()
            .unwrap();
        let vals: AllValidatorsResponse = from_binary(&raw).unwrap();
        assert_eq!(vals.validators, vec![val1, val2]);
    }

    #[cfg(feature = "staking")]
    #[test]
    fn staking_querier_validator() {
        let address1 = String::from("validator-one");
        let address2 = String::from("validator-two");
        let address_non_existent = String::from("wannabe-validator");

        let val1 = Validator {
            address: address1.clone(),
            commission: Decimal::percent(1),
            max_commission: Decimal::percent(3),
            max_change_rate: Decimal::percent(1),
        };
        let val2 = Validator {
            address: address2.clone(),
            commission: Decimal::permille(15),
            max_commission: Decimal::permille(40),
            max_change_rate: Decimal::permille(5),
        };

        let staking = StakingQuerier::new("ustake", &[val1.clone(), val2.clone()], &[]);

        // query 1
        let raw = staking
            .query(&StakingQuery::Validator { address: address1 })
            .unwrap()
            .unwrap();
        let res: ValidatorResponse = from_binary(&raw).unwrap();
        assert_eq!(res.validator, Some(val1));

        // query 2
        let raw = staking
            .query(&StakingQuery::Validator { address: address2 })
            .unwrap()
            .unwrap();
        let res: ValidatorResponse = from_binary(&raw).unwrap();
        assert_eq!(res.validator, Some(val2));

        // query non-existent
        let raw = staking
            .query(&StakingQuery::Validator {
                address: address_non_existent,
            })
            .unwrap()
            .unwrap();
        let res: ValidatorResponse = from_binary(&raw).unwrap();
        assert_eq!(res.validator, None);
    }

    #[cfg(feature = "staking")]
    // gets delegators from query or panic
    fn get_all_delegators(
        staking: &StakingQuerier,
        delegator: impl Into<String>,
    ) -> Vec<Delegation> {
        let raw = staking
            .query(&StakingQuery::AllDelegations {
                delegator: delegator.into(),
            })
            .unwrap()
            .unwrap();
        let dels: AllDelegationsResponse = from_binary(&raw).unwrap();
        dels.delegations
    }

    #[cfg(feature = "staking")]
    // gets full delegators from query or panic
    fn get_delegator(
        staking: &StakingQuerier,
        delegator: impl Into<String>,
        validator: impl Into<String>,
    ) -> Option<FullDelegation> {
        let raw = staking
            .query(&StakingQuery::Delegation {
                delegator: delegator.into(),
                validator: validator.into(),
            })
            .unwrap()
            .unwrap();
        let dels: DelegationResponse = from_binary(&raw).unwrap();
        dels.delegation
    }

    #[cfg(feature = "staking")]
    #[test]
    fn staking_querier_delegations() {
        let val1 = String::from("validator-one");
        let val2 = String::from("validator-two");

        let user_a = Addr::unchecked("investor");
        let user_b = Addr::unchecked("speculator");
        let user_c = Addr::unchecked("hodler");

        // we need multiple validators per delegator, so the queries provide different results
        let del1a = FullDelegation {
            delegator: user_a.clone(),
            validator: val1.clone(),
            amount: coin(100, "ustake"),
            can_redelegate: coin(100, "ustake"),
            accumulated_rewards: coins(5, "ustake"),
        };
        let del2a = FullDelegation {
            delegator: user_a.clone(),
            validator: val2.clone(),
            amount: coin(500, "ustake"),
            can_redelegate: coin(500, "ustake"),
            accumulated_rewards: coins(20, "ustake"),
        };

        // note we cannot have multiple delegations on one validator, they are collapsed into one
        let del1b = FullDelegation {
            delegator: user_b.clone(),
            validator: val1.clone(),
            amount: coin(500, "ustake"),
            can_redelegate: coin(0, "ustake"),
            accumulated_rewards: coins(0, "ustake"),
        };

        // and another one on val2
        let del2c = FullDelegation {
            delegator: user_c.clone(),
            validator: val2.clone(),
            amount: coin(8888, "ustake"),
            can_redelegate: coin(4567, "ustake"),
            accumulated_rewards: coins(900, "ustake"),
        };

        let staking = StakingQuerier::new(
            "ustake",
            &[],
            &[del1a.clone(), del1b.clone(), del2a.clone(), del2c.clone()],
        );

        // get all for user a
        let dels = get_all_delegators(&staking, user_a.clone());
        assert_eq!(dels, vec![del1a.clone().into(), del2a.clone().into()]);

        // get all for user b
        let dels = get_all_delegators(&staking, user_b.clone());
        assert_eq!(dels, vec![del1b.clone().into()]);

        // get all for user c
        let dels = get_all_delegators(&staking, user_c.clone());
        assert_eq!(dels, vec![del2c.clone().into()]);

        // for user with no delegations...
        let dels = get_all_delegators(&staking, String::from("no one"));
        assert_eq!(dels, vec![]);

        // filter a by validator (1 and 1)
        let dels = get_delegator(&staking, user_a.clone(), val1.clone());
        assert_eq!(dels, Some(del1a));
        let dels = get_delegator(&staking, user_a, val2.clone());
        assert_eq!(dels, Some(del2a));

        // filter b by validator (2 and 0)
        let dels = get_delegator(&staking, user_b.clone(), val1.clone());
        assert_eq!(dels, Some(del1b));
        let dels = get_delegator(&staking, user_b, val2.clone());
        assert_eq!(dels, None);

        // filter c by validator (0 and 1)
        let dels = get_delegator(&staking, user_c.clone(), val1);
        assert_eq!(dels, None);
        let dels = get_delegator(&staking, user_c, val2);
        assert_eq!(dels, Some(del2c));
    }

    #[test]
    fn wasm_querier_works() {
        let mut querier = WasmQuerier::default();

        let any_addr = "foo".to_string();

        // Query WasmQuery::Raw
        let system_err = querier
            .query(&WasmQuery::Raw {
                contract_addr: any_addr.clone(),
                key: b"the key".into(),
            })
            .unwrap_err();
        match system_err {
            SystemError::NoSuchContract { addr } => assert_eq!(addr, any_addr),
            err => panic!("Unexpected error: {:?}", err),
        }

        // Query WasmQuery::Smart
        let system_err = querier
            .query(&WasmQuery::Smart {
                contract_addr: any_addr.clone(),
                msg: b"{}".into(),
            })
            .unwrap_err();
        match system_err {
            SystemError::NoSuchContract { addr } => assert_eq!(addr, any_addr),
            err => panic!("Unexpected error: {:?}", err),
        }

        // Query WasmQuery::ContractInfo
        let system_err = querier
            .query(&WasmQuery::ContractInfo {
                contract_addr: any_addr.clone(),
            })
            .unwrap_err();
        match system_err {
            SystemError::NoSuchContract { addr } => assert_eq!(addr, any_addr),
            err => panic!("Unexpected error: {:?}", err),
        }

        querier.update_handler(|request| {
            let constract1 = Addr::unchecked("contract1");
            let mut storage1 = HashMap::<Binary, Binary>::default();
            storage1.insert(b"the key".into(), b"the value".into());

            match request {
                WasmQuery::Raw { contract_addr, key } => {
                    if *contract_addr == constract1 {
                        if let Some(value) = storage1.get(key) {
                            SystemResult::Ok(ContractResult::Ok(value.clone()))
                        } else {
                            SystemResult::Ok(ContractResult::Ok(Binary::default()))
                        }
                    } else {
                        SystemResult::Err(SystemError::NoSuchContract {
                            addr: contract_addr.clone(),
                        })
                    }
                }
                WasmQuery::Smart { contract_addr, msg } => {
                    if *contract_addr == constract1 {
                        #[derive(Deserialize)]
                        struct MyMsg {}
                        let _msg: MyMsg = match from_binary(msg) {
                            Ok(msg) => msg,
                            Err(err) => {
                                return SystemResult::Ok(ContractResult::Err(err.to_string()))
                            }
                        };
                        let response: Response = Response::new().set_data(b"good");
                        SystemResult::Ok(ContractResult::Ok(to_binary(&response).unwrap()))
                    } else {
                        SystemResult::Err(SystemError::NoSuchContract {
                            addr: contract_addr.clone(),
                        })
                    }
                }
                WasmQuery::ContractInfo { contract_addr } => {
                    if *contract_addr == constract1 {
                        let response = ContractInfoResponse {
                            code_id: 4,
                            creator: "lalala".into(),
                            admin: None,
                            pinned: false,
                            ibc_port: None,
                        };
                        SystemResult::Ok(ContractResult::Ok(to_binary(&response).unwrap()))
                    } else {
                        SystemResult::Err(SystemError::NoSuchContract {
                            addr: contract_addr.clone(),
                        })
                    }
                }
            }
        });

        // WasmQuery::Raw
        let result = querier.query(&WasmQuery::Raw {
            contract_addr: "contract1".into(),
            key: b"the key".into(),
        });
        match result {
            SystemResult::Ok(ContractResult::Ok(value)) => assert_eq!(value, b"the value" as &[u8]),
            res => panic!("Unexpected result: {:?}", res),
        }
        let result = querier.query(&WasmQuery::Raw {
            contract_addr: "contract1".into(),
            key: b"other key".into(),
        });
        match result {
            SystemResult::Ok(ContractResult::Ok(value)) => assert_eq!(value, b"" as &[u8]),
            res => panic!("Unexpected result: {:?}", res),
        }

        // WasmQuery::Smart
        let result = querier.query(&WasmQuery::Smart {
            contract_addr: "contract1".into(),
            msg: b"{}".into(),
        });
        match result {
            SystemResult::Ok(ContractResult::Ok(value)) => assert_eq!(
                value,
                br#"{"messages":[],"attributes":[],"events":[],"data":"Z29vZA=="}"# as &[u8]
            ),
            res => panic!("Unexpected result: {:?}", res),
        }
        let result = querier.query(&WasmQuery::Smart {
            contract_addr: "contract1".into(),
            msg: b"a broken request".into(),
        });
        match result {
            SystemResult::Ok(ContractResult::Err(err)) => {
                assert_eq!(err, "Error parsing into type cosmwasm_std::testing::mock::tests::wasm_querier_works::{{closure}}::MyMsg: Invalid type")
            }
            res => panic!("Unexpected result: {:?}", res),
        }

        // WasmQuery::ContractInfo
        let result = querier.query(&WasmQuery::ContractInfo {
            contract_addr: "contract1".into(),
        });
        match result {
            SystemResult::Ok(ContractResult::Ok(value)) => assert_eq!(
                value,
                br#"{"code_id":4,"creator":"lalala","admin":null,"pinned":false,"ibc_port":null}"#
                    as &[u8]
            ),
            res => panic!("Unexpected result: {:?}", res),
        }
    }

    #[test]
    fn riffle_shuffle_works() {
        // Example from https://en.wikipedia.org/wiki/In_shuffle
        let start = [0xA, 0x2, 0x3, 0x4, 0x5, 0x6];
        let round1 = riffle_shuffle(&start);
        assert_eq!(round1, [0x4, 0xA, 0x5, 0x2, 0x6, 0x3]);
        let round2 = riffle_shuffle(&round1);
        assert_eq!(round2, [0x2, 0x4, 0x6, 0xA, 0x3, 0x5]);
        let round3 = riffle_shuffle(&round2);
        assert_eq!(round3, start);

        // For 14 elements, the original order is restored after 4 executions
        // See https://en.wikipedia.org/wiki/In_shuffle#Mathematics and https://oeis.org/A002326
        let original = [12, 33, 76, 576, 0, 44, 1, 14, 78, 99, 871212, -7, 2, -1];
        let mut result = Vec::from(original);
        for _ in 0..4 {
            result = riffle_shuffle(&result);
        }
        assert_eq!(result, original);

        // For 24 elements, the original order is restored after 20 executions
        let original = [
            7, 4, 2, 4656, 23, 45, 23, 1, 12, 76, 576, 0, 12, 1, 14, 78, 99, 12, 1212, 444, 31,
            111, 424, 34,
        ];
        let mut result = Vec::from(original);
        for _ in 0..20 {
            result = riffle_shuffle(&result);
        }
        assert_eq!(result, original);
    }

    #[test]
    fn digit_sum_works() {
        assert_eq!(digit_sum(&[]), 0);
        assert_eq!(digit_sum(&[0]), 0);
        assert_eq!(digit_sum(&[0, 0]), 0);
        assert_eq!(digit_sum(&[0, 0, 0]), 0);

        assert_eq!(digit_sum(&[1, 0, 0]), 1);
        assert_eq!(digit_sum(&[0, 1, 0]), 1);
        assert_eq!(digit_sum(&[0, 0, 1]), 1);

        assert_eq!(digit_sum(&[1, 2, 3]), 6);

        assert_eq!(digit_sum(&[255, 1]), 256);
    }
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
