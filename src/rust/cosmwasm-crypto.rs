//! The crypto crate is intended to be used in internal crates / utils.
//! Please don't use any of these types directly, as
//! they might change frequently, or be removed in the future.
//! This crate does not adhere to semantic versioning.
#![cfg_attr(feature = "backtraces", feature(backtrace))]

mod ed25519 {
use ed25519_zebra::{batch, Signature, VerificationKey};
use rand_core::OsRng;

use crate::errors::{CryptoError, CryptoResult};

/// Length of a serialized public key
pub const EDDSA_PUBKEY_LEN: usize = 32;

/// EdDSA ed25519 implementation.
///
/// This function verifies messages against a signature, with the public key of the signer,
/// using the ed25519 elliptic curve digital signature parametrization / algorithm.
///
/// The maximum currently supported message length is 4096 bytes.
/// The signature and public key are in [Tendermint](https://docs.tendermint.com/v0.32/spec/blockchain/encoding.html#public-key-cryptography)
/// format:
/// - signature: raw ED25519 signature (64 bytes).
/// - public key: raw ED25519 public key (32 bytes).
pub fn ed25519_verify(message: &[u8], signature: &[u8], public_key: &[u8]) -> CryptoResult<bool> {}

/// Performs batch Ed25519 signature verification.
///
/// Batch verification asks whether all signatures in some set are valid, rather than asking whether
/// each of them is valid. This allows sharing computations among all signature verifications,
/// performing less work overall, at the cost of higher latency (the entire batch must complete),
/// complexity of caller code (which must assemble a batch of signatures across work-items),
/// and loss of the ability to easily pinpoint failing signatures.
///
/// This batch verification implementation is adaptive, in the sense that it detects multiple
/// signatures created with the same verification key, and automatically coalesces terms
/// in the final verification equation.
///
/// In the limiting case where all signatures in the batch are made with the same verification key,
/// coalesced batch verification runs twice as fast as ordinary batch verification.
///
/// Three Variants are suppported in the input for convenience:
///  - Equal number of messages, signatures, and public keys: Standard, generic functionality.
///  - One message, and an equal number of signatures and public keys: Multiple digital signature
/// (multisig) verification of a single message.
///  - One public key, and an equal number of messages and signatures: Verification of multiple
/// messages, all signed with the same private key.
///
/// Any other variants of input vectors result in an error.
///
/// Notes:
///  - The "one-message, with zero signatures and zero public keys" case, is considered the empty
/// case.
///  - The "one-public key, with zero messages and zero signatures" case, is considered the empty
/// case.
///  - The empty case (no messages, no signatures and no public keys) returns true.
pub fn ed25519_batch_verify(
    messages: &[&[u8]],
    signatures: &[&[u8]],
    public_keys: &[&[u8]],
) -> CryptoResult<bool> {}

/// Error raised when signature is not 64 bytes long
struct InvalidEd25519SignatureFormat;

impl From<InvalidEd25519SignatureFormat> for CryptoError {
    fn from(_original: InvalidEd25519SignatureFormat) -> Self {}
}

fn read_signature(data: &[u8]) -> Result<[u8; 64], InvalidEd25519SignatureFormat> {}

/// Error raised when pubkey is not 32 bytes long
struct InvalidEd25519PubkeyFormat;

impl From<InvalidEd25519PubkeyFormat> for CryptoError {
    fn from(_original: InvalidEd25519PubkeyFormat) -> Self {}
}

fn read_pubkey(data: &[u8]) -> Result<[u8; 32], InvalidEd25519PubkeyFormat> {}

#[cfg(test)]
mod tests {
    use super::*;
    use ed25519_zebra::SigningKey;
    use serde::Deserialize;

    // For generic signature verification
    const MSG: &str = "Hello World!";

    // Cosmos ed25519 signature verification
    // TEST 1 from https://tools.ietf.org/html/rfc8032#section-7.1
    const COSMOS_ED25519_MSG: &str = "";
    const COSMOS_ED25519_PRIVATE_KEY_HEX: &str =
        "9d61b19deffd5a60ba844af492ec2cc44449c5697b326919703bac031cae7f60";
    const COSMOS_ED25519_PUBLIC_KEY_HEX: &str =
        "d75a980182b10ab7d54bfed3c964073a0ee172f3daa62325af021a68f707511a";
    const COSMOS_ED25519_SIGNATURE_HEX: &str = "e5564300c360ac729086e2cc806e828a84877f1eb8e5d974d873e065224901555fb8821590a33bacc61e39701cf9b46bd25bf5f0595bbe24655141438e7a100b";

    // Test data from https://tools.ietf.org/html/rfc8032#section-7.1
    const COSMOS_ED25519_TESTS_JSON: &str = "./testdata/ed25519_tests.json";

    #[derive(Deserialize, Debug)]
    struct Encoded {
        #[serde(rename = "privkey")]
        #[allow(dead_code)]
        private_key: String,
        #[serde(rename = "pubkey")]
        public_key: String,
        message: String,
        signature: String,
    }

    fn read_cosmos_sigs() -> Vec<Encoded> {}

    #[test]
    fn test_ed25519_verify() {}

    #[test]
    fn test_cosmos_ed25519_verify() {}

    #[test]
    fn test_cosmos_extra_ed25519_verify() {}

    #[test]
    fn test_cosmos_ed25519_batch_verify() {}

    // structural tests
    #[test]
    fn test_cosmos_ed25519_batch_verify_empty_works() {}

    #[test]
    fn test_cosmos_ed25519_batch_verify_wrong_number_of_items_errors() {}

    #[test]
    fn test_cosmos_ed25519_batch_verify_one_msg_different_number_of_sigs_pubkeys_errors() {}

    #[test]
    fn test_cosmos_ed25519_batch_verify_one_pubkey_different_number_of_msgs_sigs_errors() {}

    #[test]
    fn test_cosmos_ed25519_batch_verify_one_msg_zero_sigs_pubkeys_works() {}

    #[test]
    fn test_cosmos_ed25519_batch_verify_one_pubkey_zero_msgs_sigs_works() {}
}
}
mod errors {
#[cfg(feature = "backtraces")]
use std::backtrace::Backtrace;
use std::fmt::Debug;
use thiserror::Error;

pub type CryptoResult<T> = core::result::Result<T, CryptoError>;

#[derive(Error, Debug)]
pub enum CryptoError {
    #[error("Batch verify error: {msg}")]
    BatchErr {
        msg: String,
        #[cfg(feature = "backtraces")]
        backtrace: Backtrace,
    },
    #[error("Crypto error: {msg}")]
    GenericErr {
        msg: String,
        #[cfg(feature = "backtraces")]
        backtrace: Backtrace,
    },
    #[error("Invalid hash format")]
    InvalidHashFormat {
        #[cfg(feature = "backtraces")]
        backtrace: Backtrace,
    },
    #[error("Invalid public key format")]
    InvalidPubkeyFormat {
        #[cfg(feature = "backtraces")]
        backtrace: Backtrace,
    },
    #[error("Invalid signature format")]
    InvalidSignatureFormat {
        #[cfg(feature = "backtraces")]
        backtrace: Backtrace,
    },
    #[error("Invalid recovery parameter. Supported values: 0 and 1.")]
    InvalidRecoveryParam {
        #[cfg(feature = "backtraces")]
        backtrace: Backtrace,
    },
}

impl CryptoError {
    pub fn batch_err(msg: impl Into<String>) -> Self {}

    pub fn generic_err(msg: impl Into<String>) -> Self {}

    pub fn invalid_hash_format() -> Self {}

    pub fn invalid_pubkey_format() -> Self {}

    pub fn invalid_signature_format() -> Self {}

    pub fn invalid_recovery_param() -> Self {}

    /// Numeric error code that can easily be passed over the
    /// contract VM boundary.
    pub fn code(&self) -> u32 {}
}

#[cfg(test)]
mod tests {
    use super::*;

    // constructors
    #[test]
    fn batch_err_works() {}

    #[test]
    fn generic_err_works() {}

    #[test]
    fn invalid_hash_format_works() {}

    #[test]
    fn invalid_signature_format_works() {}

    #[test]
    fn invalid_pubkey_format_works() {}
}
}
mod identity_digest {
//! Dummy 256-bits Digest impl.
//! This digest stores/accepts a value of the proper length.
//! To be used for / with already hashed values, just to comply with the Digest contract.
//!
//! Adapted from `sha2` [sha256.rs](https://github.com/RustCrypto/hashes/blob/master/sha2/src/sha256.rs)
use digest::consts::U32;
use digest::generic_array::GenericArray;
use digest::{FixedOutput, HashMarker, Output, OutputSizeUser, Reset, Update};

/// The 256-bits identity container
#[derive(Clone, Default)]
pub struct Identity256 {
    array: GenericArray<u8, U32>,
}

impl Update for Identity256 {
    fn update(&mut self, hash: &[u8]) {}
}

impl OutputSizeUser for Identity256 {
    type OutputSize = U32;
}

impl FixedOutput for Identity256 {
    fn finalize_into(self, out: &mut Output<Self>) {}
}

impl HashMarker for Identity256 {}

impl Reset for Identity256 {
    fn reset(&mut self) {}
}
}
mod secp256k1 {
use digest::{Digest, Update}; // trait
use k256::{
    ecdsa::recoverable,
    ecdsa::signature::{DigestVerifier, Signature as _}, // traits
    ecdsa::{Signature, VerifyingKey},                   // type aliases
    elliptic_curve::sec1::ToEncodedPoint,
};

use crate::errors::{CryptoError, CryptoResult};
use crate::identity_digest::Identity256;

/// Max length of a message hash for secp256k1 verification in bytes.
/// This is typically a 32 byte output of e.g. SHA-256 or Keccak256. In theory shorter values
/// are possible but currently not supported by the implementation. Let us know when you need them.
pub const MESSAGE_HASH_MAX_LEN: usize = 32;

/// ECDSA (secp256k1) parameters
/// Length of a serialized signature
pub const ECDSA_SIGNATURE_LEN: usize = 64;

/// Length of a serialized compressed public key
const ECDSA_COMPRESSED_PUBKEY_LEN: usize = 33;
/// Length of a serialized uncompressed public key
const ECDSA_UNCOMPRESSED_PUBKEY_LEN: usize = 65;
/// Max length of a serialized public key
pub const ECDSA_PUBKEY_MAX_LEN: usize = ECDSA_UNCOMPRESSED_PUBKEY_LEN;

/// ECDSA secp256k1 implementation.
///
/// This function verifies message hashes (typically, hashed unsing SHA-256) against a signature,
/// with the public key of the signer, using the secp256k1 elliptic curve digital signature
/// parametrization / algorithm.
///
/// The signature and public key are in "Cosmos" format:
/// - signature:  Serialized "compact" signature (64 bytes).
/// - public key: [Serialized according to SEC 2](https://www.oreilly.com/library/view/programming-bitcoin/9781492031482/ch04.html)
/// (33 or 65 bytes).
pub fn secp256k1_verify(
    message_hash: &[u8],
    signature: &[u8],
    public_key: &[u8],
) -> CryptoResult<bool> {}

/// Recovers a public key from a message hash and a signature.
///
/// This is required when working with Ethereum where public keys
/// are not stored on chain directly.
///
/// `recovery_param` must be 0 or 1. The values 2 and 3 are unsupported by this implementation,
/// which is the same restriction as Ethereum has (https://github.com/ethereum/go-ethereum/blob/v1.9.25/internal/ethapi/api.go#L466-L469).
/// All other values are invalid.
///
/// Returns the recovered pubkey in compressed form, which can be used
/// in secp256k1_verify directly.
pub fn secp256k1_recover_pubkey(
    message_hash: &[u8],
    signature: &[u8],
    recovery_param: u8,
) -> Result<Vec<u8>, CryptoError> {}

/// Error raised when hash is not 32 bytes long
struct InvalidSecp256k1HashFormat;

impl From<InvalidSecp256k1HashFormat> for CryptoError {
    fn from(_original: InvalidSecp256k1HashFormat) -> Self {}
}

fn read_hash(data: &[u8]) -> Result<[u8; 32], InvalidSecp256k1HashFormat> {}

/// Error raised when signature is not 64 bytes long (32 bytes r, 32 bytes s)
struct InvalidSecp256k1SignatureFormat;

impl From<InvalidSecp256k1SignatureFormat> for CryptoError {
    fn from(_original: InvalidSecp256k1SignatureFormat) -> Self {}
}

fn read_signature(data: &[u8]) -> Result<[u8; 64], InvalidSecp256k1SignatureFormat> {}

/// Error raised when public key is not in one of the two supported formats:
/// 1. Uncompressed: 65 bytes starting with 0x04
/// 2. Compressed: 33 bytes starting with 0x02 or 0x03
struct InvalidSecp256k1PubkeyFormat;

impl From<InvalidSecp256k1PubkeyFormat> for CryptoError {
    fn from(_original: InvalidSecp256k1PubkeyFormat) -> Self {}
}

fn check_pubkey(data: &[u8]) -> Result<(), InvalidSecp256k1PubkeyFormat> {}

#[cfg(test)]
mod tests {
    use super::*;

    use hex_literal::hex;
    use k256::{
        ecdsa::signature::DigestSigner, // trait
        ecdsa::SigningKey,              // type alias
        elliptic_curve::rand_core::OsRng,
        elliptic_curve::sec1::ToEncodedPoint,
    };
    use sha2::Sha256;

    // For generic signature verification
    const MSG: &str = "Hello World!";

    // Cosmos secp256k1 signature verification
    // tendermint/PubKeySecp256k1 pubkey
    const COSMOS_SECP256K1_PUBKEY_BASE64: &str = "A08EGB7ro1ORuFhjOnZcSgwYlpe0DSFjVNUIkNNQxwKQ";

    const COSMOS_SECP256K1_MSG_HEX1: &str = "0a93010a90010a1c2f636f736d6f732e62616e6b2e763162657461312e4d736753656e6412700a2d636f736d6f7331706b707472653766646b6c366766727a6c65736a6a766878686c63337234676d6d6b38727336122d636f736d6f7331717970717870713971637273737a673270767871367273307a716733797963356c7a763778751a100a0575636f736d12073132333435363712650a4e0a460a1f2f636f736d6f732e63727970746f2e736563703235366b312e5075624b657912230a21034f04181eeba35391b858633a765c4a0c189697b40d216354d50890d350c7029012040a02080112130a0d0a0575636f736d12043230303010c09a0c1a0c73696d642d74657374696e672001";
    const COSMOS_SECP256K1_MSG_HEX2: &str = "0a93010a90010a1c2f636f736d6f732e62616e6b2e763162657461312e4d736753656e6412700a2d636f736d6f7331706b707472653766646b6c366766727a6c65736a6a766878686c63337234676d6d6b38727336122d636f736d6f7331717970717870713971637273737a673270767871367273307a716733797963356c7a763778751a100a0575636f736d12073132333435363712670a500a460a1f2f636f736d6f732e63727970746f2e736563703235366b312e5075624b657912230a21034f04181eeba35391b858633a765c4a0c189697b40d216354d50890d350c7029012040a020801180112130a0d0a0575636f736d12043230303010c09a0c1a0c73696d642d74657374696e672001";
    const COSMOS_SECP256K1_MSG_HEX3: &str = "0a93010a90010a1c2f636f736d6f732e62616e6b2e763162657461312e4d736753656e6412700a2d636f736d6f7331706b707472653766646b6c366766727a6c65736a6a766878686c63337234676d6d6b38727336122d636f736d6f7331717970717870713971637273737a673270767871367273307a716733797963356c7a763778751a100a0575636f736d12073132333435363712670a500a460a1f2f636f736d6f732e63727970746f2e736563703235366b312e5075624b657912230a21034f04181eeba35391b858633a765c4a0c189697b40d216354d50890d350c7029012040a020801180212130a0d0a0575636f736d12043230303010c09a0c1a0c73696d642d74657374696e672001";

    const COSMOS_SECP256K1_SIGNATURE_HEX1: &str = "c9dd20e07464d3a688ff4b710b1fbc027e495e797cfa0b4804da2ed117959227772de059808f765aa29b8f92edf30f4c2c5a438e30d3fe6897daa7141e3ce6f9";
    const COSMOS_SECP256K1_SIGNATURE_HEX2: &str = "525adc7e61565a509c60497b798c549fbf217bb5cd31b24cc9b419d098cc95330c99ecc4bc72448f85c365a4e3f91299a3d40412fb3751bab82f1940a83a0a4c";
    const COSMOS_SECP256K1_SIGNATURE_HEX3: &str = "f3f2ca73806f2abbf6e0fe85f9b8af66f0e9f7f79051fdb8abe5bb8633b17da132e82d577b9d5f7a6dae57a144efc9ccc6eef15167b44b3b22a57240109762af";

    // Test data originally from https://github.com/cosmos/cosmjs/blob/v0.24.0-alpha.22/packages/crypto/src/secp256k1.spec.ts#L195-L394
    const COSMOS_SECP256K1_TESTS_JSON: &str = "./testdata/secp256k1_tests.json";

    #[test]
    fn test_secp256k1_verify() {}

    #[test]
    fn test_cosmos_secp256k1_verify() {}

    #[test]
    fn test_cosmos_extra_secp256k1_verify() {}

    #[test]
    fn secp256k1_recover_pubkey_works() {}

    #[test]
    fn secp256k1_recover_pubkey_fails_for_invalid_recovery_param() {}
}
}
mod zk {
mod errors {
#[cfg(feature = "backtraces")]
use std::backtrace::Backtrace;
use std::fmt::Debug;
use thiserror::Error;

pub type ZKResult<T> = core::result::Result<T, ZKError>;

#[derive(Error, Debug)]
pub enum ZKError {
    #[error("ZK verification error")]
    VerifierError {
        #[cfg(feature = "backtraces")]
        backtrace: Backtrace,
    },

    #[error("Invalid hash input")]
    InvalidHashInput {},

    #[error("ZK error: {msg}")]
    GenericErr {
        msg: String,
        #[cfg(feature = "backtraces")]
        backtrace: Backtrace,
    },
}

impl ZKError {
    pub fn generic_err(msg: impl Into<String>) -> Self {}

    /// Numeric error code that can easily be passed over the
    /// contract VM boundary.
    pub fn code(&self) -> u32 {}
}
}

#[allow(clippy::all)]
mod poseidon {
use ark_bn254::Fr as Bn254Fr;
use ark_ff::{BigInteger, PrimeField};
use ark_std::vec::Vec;
use arkworks_native_gadgets::poseidon::{FieldHasher, Poseidon as ArkworksPoseidon};
use arkworks_native_gadgets::to_field_elements;
use arkworks_setups::common::setup_params;
use arkworks_setups::Curve;

use crate::ZKError;

pub type PoseidonHasher = ArkworksPoseidon<Bn254Fr>;

#[derive(Debug, Clone)]
pub struct Poseidon {
    poseidon_width_3_bytes: PoseidonHasher,
    poseidon_width_4_bytes: PoseidonHasher,
    poseidon_width_5_bytes: PoseidonHasher,
}

impl Poseidon {
    pub fn new() -> Self {}

    pub fn hash(&self, inputs: &[&[u8]]) -> Result<Vec<u8>, ZKError> {}
}

impl Default for Poseidon {
    fn default() -> Self {}
}

#[test]
fn test_hash() {}
}

#[allow(clippy::all)]
mod verifier {
use super::{ZKError, ZKResult};

pub const GROTH16_VERIFIER_KEY_LEN: usize = 360;
pub const GROTH16_PROOF_LEN: usize = 128;

use ark_bn254::Bn254;
#[allow(clippy::all)]
use ark_crypto_primitives::{Error, SNARK};
use ark_ec::PairingEngine;
use ark_groth16::{Groth16, Proof, VerifyingKey};
use ark_serialize::CanonicalDeserialize;
use ark_std::marker::PhantomData;
use arkworks_native_gadgets::to_field_elements;

pub struct ArkworksVerifierGroth16<E: PairingEngine>(PhantomData<E>);

impl<E: PairingEngine> ArkworksVerifierGroth16<E> {
    pub fn verify(
        public_inp_bytes: &[u8],
        proof_bytes: &[u8],
        vk_bytes: &[u8],
    ) -> Result<bool, Error> {}
}

pub type ArkworksVerifierBn254 = ArkworksVerifierGroth16<Bn254>;

pub fn groth16_verify(
    public_inp_bytes: &[u8],
    proof_bytes: &[u8],
    vk_bytes: &[u8],
) -> ZKResult<bool> {}
}

#[allow(clippy::all)]
mod keccak {
use ark_bn254::Fr as Bn254Fr;
use ark_ff::{BigInteger, PrimeField};
use ark_std::vec::Vec;
use arkworks_setups::common::keccak_256;

pub fn curve_hash(input: &[u8]) -> Vec<u8> {}
}

pub use errors::{ZKError, ZKResult};
pub use keccak::curve_hash;
pub use poseidon::Poseidon;
pub use verifier::{
    groth16_verify, ArkworksVerifierBn254, GROTH16_PROOF_LEN, GROTH16_VERIFIER_KEY_LEN,
};
}

#[doc(hidden)]
pub use crate::ed25519::EDDSA_PUBKEY_LEN;
#[doc(hidden)]
pub use crate::ed25519::{ed25519_batch_verify, ed25519_verify};
#[doc(hidden)]
pub use crate::errors::{CryptoError, CryptoResult};
#[doc(hidden)]
pub use crate::secp256k1::{secp256k1_recover_pubkey, secp256k1_verify};
#[doc(hidden)]
pub use crate::secp256k1::{ECDSA_PUBKEY_MAX_LEN, ECDSA_SIGNATURE_LEN, MESSAGE_HASH_MAX_LEN};
#[doc(hidden)]
pub use crate::zk::*;
