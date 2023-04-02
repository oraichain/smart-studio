//! The crypto crate is intended to be used in internal crates / utils.
//! Please don't use any of these types directly, as
//! they might change frequently, or be removed in the future.
//! This crate does not adhere to semantic versioning.
#![cfg_attr(feature = "backtraces", feature(error_generic_member_access))]
#![cfg_attr(feature = "backtraces", feature(provide_any))]

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
pub fn ed25519_verify(message: &[u8], signature: &[u8], public_key: &[u8]) -> CryptoResult<bool> {
}

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
) -> CryptoResult<bool> {
}

/// Error raised when signature is not 64 bytes long
struct InvalidEd25519SignatureFormat;

impl From<InvalidEd25519SignatureFormat> for CryptoError {
    fn from(_original: InvalidEd25519SignatureFormat) -> Self {
}
}

fn read_signature(data: &[u8]) -> Result<[u8; 64], InvalidEd25519SignatureFormat> {
}

/// Error raised when pubkey is not 32 bytes long
struct InvalidEd25519PubkeyFormat;

impl From<InvalidEd25519PubkeyFormat> for CryptoError {
    fn from(_original: InvalidEd25519PubkeyFormat) -> Self {
}
}

fn read_pubkey(data: &[u8]) -> Result<[u8; 32], InvalidEd25519PubkeyFormat> {
}

#[cfg(test)]
mod tests {
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
    pub fn batch_err(msg: impl Into<String>) -> Self {
}

    pub fn generic_err(msg: impl Into<String>) -> Self {
}

    pub fn invalid_hash_format() -> Self {
}

    pub fn invalid_pubkey_format() -> Self {
}

    pub fn invalid_signature_format() -> Self {
}

    pub fn invalid_recovery_param() -> Self {
}

    /// Numeric error code that can easily be passed over the
    /// contract VM boundary.
    pub fn code(&self) -> u32 {
}
}

#[cfg(test)]
mod tests {
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
    fn update(&mut self, hash: &[u8]) {
}
}

impl OutputSizeUser for Identity256 {
    type OutputSize = U32;
}

impl FixedOutput for Identity256 {
    fn finalize_into(self, out: &mut Output<Self>) {
}
}

impl HashMarker for Identity256 {}

impl Reset for Identity256 {
    fn reset(&mut self) {
}
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
///
/// This implementation accepts both high-S and low-S signatures. Some applications
/// including Ethereum transactions consider high-S signatures invalid in order to
/// avoid malleability. If that's the case for your protocol, the signature needs
/// to be tested for low-S in addition to this verification.
pub fn secp256k1_verify(
    message_hash: &[u8],
    signature: &[u8],
    public_key: &[u8],
) -> CryptoResult<bool> {
}

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
///
/// This implementation accepts both high-S and low-S signatures. This is the
/// same behavior as Ethereum's `ecrecover`. The reason is that high-S signatures
/// may be perfectly valid if the application protocol does not disallow them.
/// Or as [EIP-2] put it "The ECDSA recover precompiled contract remains unchanged
/// and will keep accepting high s-values; this is useful e.g. if a contract
/// recovers old Bitcoin signatures.".
///
/// See also OpenZeppelin's [ECDSA.recover implementation](https://github.com/OpenZeppelin/openzeppelin-contracts/blob/v4.8.1/contracts/utils/cryptography/ECDSA.sol#L138-L149)
/// which adds further restrictions to avoid potential signature malleability.
/// Please note that restricting signatures to low-S does not make signatures unique
/// in the sense that for each (pubkey, message) there is only one signature. The
/// signer can generate an arbitrary amount of valid signatures.
/// <https://medium.com/@simonwarta/signature-determinism-for-blockchain-developers-dbd84865a93e>
///
/// [EIP-2]: https://eips.ethereum.org/EIPS/eip-2
pub fn secp256k1_recover_pubkey(
    message_hash: &[u8],
    signature: &[u8],
    recovery_param: u8,
) -> Result<Vec<u8>, CryptoError> {
}

/// Error raised when hash is not 32 bytes long
struct InvalidSecp256k1HashFormat;

impl From<InvalidSecp256k1HashFormat> for CryptoError {
    fn from(_original: InvalidSecp256k1HashFormat) -> Self {
}
}

fn read_hash(data: &[u8]) -> Result<[u8; 32], InvalidSecp256k1HashFormat> {
}

/// Error raised when signature is not 64 bytes long (32 bytes r, 32 bytes s)
struct InvalidSecp256k1SignatureFormat;

impl From<InvalidSecp256k1SignatureFormat> for CryptoError {
    fn from(_original: InvalidSecp256k1SignatureFormat) -> Self {
}
}

fn read_signature(data: &[u8]) -> Result<[u8; 64], InvalidSecp256k1SignatureFormat> {
}

/// Error raised when public key is not in one of the two supported formats:
/// 1. Uncompressed: 65 bytes starting with 0x04
/// 2. Compressed: 33 bytes starting with 0x02 or 0x03
struct InvalidSecp256k1PubkeyFormat;

impl From<InvalidSecp256k1PubkeyFormat> for CryptoError {
    fn from(_original: InvalidSecp256k1PubkeyFormat) -> Self {
}
}

fn check_pubkey(data: &[u8]) -> Result<(), InvalidSecp256k1PubkeyFormat> {
}

#[cfg(test)]
mod tests {
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

    #[error("Curve is unimplemented")]
    Unimplemented {},

    #[error("ZK error: {msg}")]
    GenericErr {
        msg: String,
        #[cfg(feature = "backtraces")]
        backtrace: Backtrace,
    },
}

impl ZKError {
    pub fn generic_err(msg: impl Into<String>) -> Self {
}

    /// Numeric error code that can easily be passed over the
    /// contract VM boundary.
    pub fn code(&self) -> u32 {
}
}
}

#[allow(clippy::all)]
mod poseidon {
use crate::{Bls381Fr, Bn254Fr};
use ark_ff::{BigInteger, PrimeField};
use ark_std::vec::Vec;
use arkworks_native_gadgets::poseidon::sbox::PoseidonSbox;
use arkworks_native_gadgets::poseidon::{
    FieldHasher, Poseidon as ArkworksPoseidon, PoseidonParameters,
};
use arkworks_native_gadgets::to_field_elements;
use arkworks_utils::poseidon_params::setup_poseidon_params;
use arkworks_utils::{bytes_matrix_to_f, bytes_vec_to_f, Curve};

use crate::ZKError;

pub type PoseidonHasherBn254 = ArkworksPoseidon<Bn254Fr>;
pub type PoseidonHasherBls381 = ArkworksPoseidon<Bls381Fr>;

#[derive(Debug, Clone)]
pub struct Poseidon {
    hasher_bn254: PoseidonHasherBn254,
    hasher_bls381: PoseidonHasherBls381,
}

fn inner_hash<F: PrimeField>(
    hasher: &ArkworksPoseidon<F>,
    inputs: &[&[u8]],
) -> Result<Vec<u8>, ZKError> {
}

pub fn setup_params<F: PrimeField>(curve: Curve, exp: i8, width: u8) -> PoseidonParameters<F> {
}

impl Poseidon {
    pub fn new() -> Self {
}

    pub fn hash(
        &self,
        left_input: &[u8],
        right_input: &[u8],
        curve: u8,
    ) -> Result<Vec<u8>, ZKError> {
}
}

impl Default for Poseidon {
    fn default() -> Self {
}
}

#[test]
fn test_hash() {
}
}

#[allow(clippy::all)]
mod verifier {
use crate::{Bls381, Bn254, ZKError, ZKResult};
pub const GROTH16_VERIFIER_KEY_LEN: usize = 360;
pub const GROTH16_PROOF_LEN: usize = 128;

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
    ) -> Result<bool, Error> {
}
}

pub type ArkworksVerifierBn254 = ArkworksVerifierGroth16<Bn254>;
pub type ArkworksVerifierBls381 = ArkworksVerifierGroth16<Bls381>;

pub fn groth16_verify(
    public_inp_bytes: &[u8],
    proof_bytes: &[u8],
    vk_bytes: &[u8],
    curve: u8,
) -> ZKResult<bool> {
}
}

#[allow(clippy::all)]
mod hash {
use crate::{Bls381Fr, Bn254Fr};
use ark_ff::{BigInteger, PrimeField};
use ark_std::vec::Vec;
use sha2::{Digest, Sha256};
use sha3::Keccak256;

pub fn keccak_256(sign_bytes: &[u8]) -> Vec<u8> {
}

pub fn sha256(sign_bytes: &[u8]) -> Vec<u8> {
}

pub fn curve_hash(input: &[u8], curve: u8) -> Vec<u8> {
}
}

pub use ark_bls12_381::{Bls12_381 as Bls381, Fr as Bls381Fr};
pub use ark_bn254::{Bn254, Fr as Bn254Fr};
pub use errors::{ZKError, ZKResult};
pub use hash::{curve_hash, keccak_256, sha256};
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
