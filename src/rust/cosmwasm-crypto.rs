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
pub fn ed25519_verify(message: &[u8], signature: &[u8], public_key: &[u8]) -> CryptoResult<bool> {
    // Validation
    let signature = read_signature(signature)?;
    let pubkey = read_pubkey(public_key)?;

    // Verification
    match VerificationKey::try_from(pubkey)
        .and_then(|vk| vk.verify(&Signature::from(signature), message))
    {
        Ok(()) => Ok(true),
        Err(_) => Ok(false),
    }
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
    // Structural checks
    let messages_len = messages.len();
    let signatures_len = signatures.len();
    let public_keys_len = public_keys.len();

    let mut messages = messages.to_vec();
    let mut public_keys = public_keys.to_vec();
    if messages_len == signatures_len && messages_len == public_keys_len { // We're good to go
    } else if messages_len == 1 && signatures_len == public_keys_len {
        // Replicate message, for multisig
        messages = messages.repeat(signatures_len);
    } else if public_keys_len == 1 && messages_len == signatures_len {
        // Replicate pubkey
        public_keys = public_keys.repeat(messages_len);
    } else {
        return Err(CryptoError::batch_err(
            "Mismatched / erroneous number of messages / signatures / public keys",
        ));
    }
    debug_assert_eq!(messages.len(), signatures_len);
    debug_assert_eq!(messages.len(), public_keys.len());

    let mut batch = batch::Verifier::new();

    for ((&message, &signature), &public_key) in messages
        .iter()
        .zip(signatures.iter())
        .zip(public_keys.iter())
    {
        // Validation
        let signature = read_signature(signature)?;
        let pubkey = read_pubkey(public_key)?;

        // Enqueing
        batch.queue((pubkey.into(), signature.into(), message));
    }

    // Batch verification
    match batch.verify(OsRng) {
        Ok(()) => Ok(true),
        Err(_) => Ok(false),
    }
}

/// Error raised when signature is not 64 bytes long
struct InvalidEd25519SignatureFormat;

impl From<InvalidEd25519SignatureFormat> for CryptoError {
    fn from(_original: InvalidEd25519SignatureFormat) -> Self {
        CryptoError::invalid_signature_format()
    }
}

fn read_signature(data: &[u8]) -> Result<[u8; 64], InvalidEd25519SignatureFormat> {
    data.try_into().map_err(|_| InvalidEd25519SignatureFormat)
}

/// Error raised when pubkey is not 32 bytes long
struct InvalidEd25519PubkeyFormat;

impl From<InvalidEd25519PubkeyFormat> for CryptoError {
    fn from(_original: InvalidEd25519PubkeyFormat) -> Self {
        CryptoError::invalid_pubkey_format()
    }
}

fn read_pubkey(data: &[u8]) -> Result<[u8; 32], InvalidEd25519PubkeyFormat> {
    data.try_into().map_err(|_| InvalidEd25519PubkeyFormat)
}

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

    fn read_cosmos_sigs() -> Vec<Encoded> {
        use std::fs::File;
        use std::io::BufReader;

        // Open the file in read-only mode with buffer.
        let file = File::open(COSMOS_ED25519_TESTS_JSON).unwrap();
        let reader = BufReader::new(file);

        serde_json::from_reader(reader).unwrap()
    }

    #[test]
    fn test_ed25519_verify() {
        let message = MSG.as_bytes();
        // Signing
        let secret_key = SigningKey::new(OsRng);
        let signature = secret_key.sign(message);

        let public_key = VerificationKey::from(&secret_key);

        // Serialization. Types can be converted to raw byte arrays with From/Into
        let signature_bytes: [u8; 64] = signature.into();
        let public_key_bytes: [u8; 32] = public_key.into();

        // Verification
        assert!(ed25519_verify(message, &signature_bytes, &public_key_bytes).unwrap());

        // Wrong message fails
        let bad_message = [message, b"\0"].concat();
        assert!(!ed25519_verify(&bad_message, &signature_bytes, &public_key_bytes).unwrap());

        // Other pubkey fails
        let other_secret_key = SigningKey::new(OsRng);
        let other_public_key = VerificationKey::from(&other_secret_key);
        let other_public_key_bytes: [u8; 32] = other_public_key.into();
        assert!(!ed25519_verify(message, &signature_bytes, &other_public_key_bytes).unwrap());
    }

    #[test]
    fn test_cosmos_ed25519_verify() {
        let secret_key = SigningKey::try_from(
            hex::decode(COSMOS_ED25519_PRIVATE_KEY_HEX)
                .unwrap()
                .as_slice(),
        )
        .unwrap();
        let public_key = VerificationKey::try_from(
            hex::decode(COSMOS_ED25519_PUBLIC_KEY_HEX)
                .unwrap()
                .as_slice(),
        )
        .unwrap();
        let signature = secret_key.sign(COSMOS_ED25519_MSG.as_bytes());

        let signature_bytes: [u8; 64] = signature.into();
        let public_key_bytes: [u8; 32] = public_key.into();

        assert_eq!(
            signature_bytes,
            hex::decode(COSMOS_ED25519_SIGNATURE_HEX)
                .unwrap()
                .as_slice()
        );

        assert!(ed25519_verify(
            COSMOS_ED25519_MSG.as_bytes(),
            &signature_bytes,
            &public_key_bytes
        )
        .unwrap());
    }

    #[test]
    fn test_cosmos_extra_ed25519_verify() {
        let codes = read_cosmos_sigs();

        for (i, encoded) in (1..).zip(codes) {
            let message = hex::decode(&encoded.message).unwrap();

            let signature = hex::decode(&encoded.signature).unwrap();

            let public_key = hex::decode(&encoded.public_key).unwrap();

            // ed25519_verify() works
            assert!(
                ed25519_verify(&message, &signature, &public_key).unwrap(),
                "verify() failed (test case {})",
                i
            );
        }
    }

    #[test]
    fn test_cosmos_ed25519_batch_verify() {
        let codes = read_cosmos_sigs();

        let mut messages: Vec<Vec<u8>> = vec![];
        let mut signatures: Vec<Vec<u8>> = vec![];
        let mut public_keys: Vec<Vec<u8>> = vec![];

        for encoded in codes {
            let message = hex::decode(&encoded.message).unwrap();
            messages.push(message);

            let signature = hex::decode(&encoded.signature).unwrap();
            signatures.push(signature);

            let public_key = hex::decode(&encoded.public_key).unwrap();
            public_keys.push(public_key);
        }

        let messages: Vec<&[u8]> = messages.iter().map(|m| m.as_slice()).collect();
        let signatures: Vec<&[u8]> = signatures.iter().map(|m| m.as_slice()).collect();
        let public_keys: Vec<&[u8]> = public_keys.iter().map(|m| m.as_slice()).collect();

        // ed25519_batch_verify() works
        assert!(ed25519_batch_verify(&messages, &signatures, &public_keys).unwrap());
    }

    // structural tests
    #[test]
    fn test_cosmos_ed25519_batch_verify_empty_works() {
        let messages: Vec<&[u8]> = vec![];
        let signatures: Vec<&[u8]> = vec![];
        let public_keys: Vec<&[u8]> = vec![];

        // ed25519_batch_verify() works for empty msgs / sigs / pubkeys
        assert!(ed25519_batch_verify(&messages, &signatures, &public_keys).unwrap());
    }

    #[test]
    fn test_cosmos_ed25519_batch_verify_wrong_number_of_items_errors() {
        let codes = read_cosmos_sigs();

        let mut messages: Vec<Vec<u8>> = vec![];
        let mut signatures: Vec<Vec<u8>> = vec![];
        let mut public_keys: Vec<Vec<u8>> = vec![];

        for encoded in codes {
            let message = hex::decode(&encoded.message).unwrap();
            messages.push(message);

            let signature = hex::decode(&encoded.signature).unwrap();
            signatures.push(signature);

            let public_key = hex::decode(&encoded.public_key).unwrap();
            public_keys.push(public_key);
        }

        let mut messages: Vec<&[u8]> = messages.iter().map(|m| m.as_slice()).collect();
        let mut signatures: Vec<&[u8]> = signatures.iter().map(|m| m.as_slice()).collect();
        let mut public_keys: Vec<&[u8]> = public_keys.iter().map(|m| m.as_slice()).collect();

        // Check the whole set passes
        assert!(ed25519_batch_verify(&messages, &signatures, &public_keys).unwrap());

        // Remove one message
        let msg = messages.pop().unwrap();

        let res = ed25519_batch_verify(&messages, &signatures, &public_keys);
        match res.unwrap_err() {
            CryptoError::BatchErr { msg, .. } => assert_eq!(
                msg,
                "Mismatched / erroneous number of messages / signatures / public keys"
            ),
            _ => panic!("Wrong error message"),
        }

        // Restore messages
        messages.push(msg);

        // Remove one signature
        let sig = signatures.pop().unwrap();

        let res = ed25519_batch_verify(&messages, &signatures, &public_keys);
        match res.unwrap_err() {
            CryptoError::BatchErr { msg, .. } => assert_eq!(
                msg,
                "Mismatched / erroneous number of messages / signatures / public keys"
            ),
            _ => panic!("Wrong error message"),
        }

        // Restore signatures
        signatures.push(sig);

        // Remove one public key
        let pubkey = public_keys.pop().unwrap();

        let res = ed25519_batch_verify(&messages, &signatures, &public_keys);
        match res.unwrap_err() {
            CryptoError::BatchErr { msg, .. } => assert_eq!(
                msg,
                "Mismatched / erroneous number of messages / signatures / public keys"
            ),
            _ => panic!("Wrong error message"),
        }

        // Restore public keys
        public_keys.push(pubkey);

        // Add one message
        messages.push(messages[0]);

        let res = ed25519_batch_verify(&messages, &signatures, &public_keys);
        match res.unwrap_err() {
            CryptoError::BatchErr { msg, .. } => assert_eq!(
                msg,
                "Mismatched / erroneous number of messages / signatures / public keys"
            ),
            _ => panic!("Wrong error message"),
        }

        // Restore messages
        messages.pop();

        // Add one signature
        signatures.push(signatures[0]);
        let res = ed25519_batch_verify(&messages, &signatures, &public_keys);
        match res.unwrap_err() {
            CryptoError::BatchErr { msg, .. } => assert_eq!(
                msg,
                "Mismatched / erroneous number of messages / signatures / public keys"
            ),
            _ => panic!("Wrong error message"),
        }

        // Restore signatures
        signatures.pop();

        // Add one public keys
        public_keys.push(public_keys[0]);
        let res = ed25519_batch_verify(&messages, &signatures, &public_keys);
        match res.unwrap_err() {
            CryptoError::BatchErr { msg, .. } => assert_eq!(
                msg,
                "Mismatched / erroneous number of messages / signatures / public keys"
            ),
            _ => panic!("Wrong error message"),
        }
    }

    #[test]
    fn test_cosmos_ed25519_batch_verify_one_msg_different_number_of_sigs_pubkeys_errors() {
        let codes = read_cosmos_sigs();

        let mut messages: Vec<Vec<u8>> = vec![];
        let mut signatures: Vec<Vec<u8>> = vec![];
        let mut public_keys: Vec<Vec<u8>> = vec![];

        for encoded in codes {
            let message = hex::decode(&encoded.message).unwrap();
            messages.push(message);

            let signature = hex::decode(&encoded.signature).unwrap();
            signatures.push(signature);

            let public_key = hex::decode(&encoded.public_key).unwrap();
            public_keys.push(public_key);
        }

        let mut messages: Vec<&[u8]> = messages.iter().map(|m| m.as_slice()).collect();
        let mut signatures: Vec<&[u8]> = signatures.iter().map(|m| m.as_slice()).collect();
        let mut public_keys: Vec<&[u8]> = public_keys.iter().map(|m| m.as_slice()).collect();

        // Check the whole set passes
        assert!(ed25519_batch_verify(&messages, &signatures, &public_keys).unwrap());

        // Just one message
        messages.truncate(1);

        // Check (in passing) this fails verification
        assert!(!ed25519_batch_verify(&messages, &signatures, &public_keys).unwrap());

        // Remove one sig
        let sig = signatures.pop().unwrap();

        let res = ed25519_batch_verify(&messages, &signatures, &public_keys);
        match res.unwrap_err() {
            CryptoError::BatchErr { msg, .. } => assert_eq!(
                msg,
                "Mismatched / erroneous number of messages / signatures / public keys"
            ),
            _ => panic!("Wrong error message"),
        }

        // Restore signatures
        signatures.push(sig);

        // Remove one public key
        let pubkey = public_keys.pop().unwrap();

        let res = ed25519_batch_verify(&messages, &signatures, &public_keys);
        match res.unwrap_err() {
            CryptoError::BatchErr { msg, .. } => assert_eq!(
                msg,
                "Mismatched / erroneous number of messages / signatures / public keys"
            ),
            _ => panic!("Wrong error message"),
        }

        // Restore public keys
        public_keys.push(pubkey);
    }

    #[test]
    fn test_cosmos_ed25519_batch_verify_one_pubkey_different_number_of_msgs_sigs_errors() {
        let codes = read_cosmos_sigs();

        let mut messages: Vec<Vec<u8>> = vec![];
        let mut signatures: Vec<Vec<u8>> = vec![];
        let mut public_keys: Vec<Vec<u8>> = vec![];

        for encoded in codes {
            let message = hex::decode(&encoded.message).unwrap();
            messages.push(message);

            let signature = hex::decode(&encoded.signature).unwrap();
            signatures.push(signature);

            let public_key = hex::decode(&encoded.public_key).unwrap();
            public_keys.push(public_key);
        }

        let mut messages: Vec<&[u8]> = messages.iter().map(|m| m.as_slice()).collect();
        let mut signatures: Vec<&[u8]> = signatures.iter().map(|m| m.as_slice()).collect();
        let mut public_keys: Vec<&[u8]> = public_keys.iter().map(|m| m.as_slice()).collect();

        // Check the whole set passes
        assert!(ed25519_batch_verify(&messages, &signatures, &public_keys).unwrap());

        // Just one public key
        public_keys.truncate(1);

        // Check (in passing) this fails verification
        assert!(!ed25519_batch_verify(&messages, &signatures, &public_keys).unwrap());

        // Remove one sig
        let sig = signatures.pop().unwrap();

        let res = ed25519_batch_verify(&messages, &signatures, &public_keys);
        match res.unwrap_err() {
            CryptoError::BatchErr { msg, .. } => assert_eq!(
                msg,
                "Mismatched / erroneous number of messages / signatures / public keys"
            ),
            _ => panic!("Wrong error message"),
        }

        // Restore signatures
        signatures.push(sig);

        // Remove one msg
        let msg = messages.pop().unwrap();

        let res = ed25519_batch_verify(&messages, &signatures, &public_keys);
        match res.unwrap_err() {
            CryptoError::BatchErr { msg, .. } => assert_eq!(
                msg,
                "Mismatched / erroneous number of messages / signatures / public keys"
            ),
            _ => panic!("Wrong error message"),
        }

        // Restore messages
        messages.push(msg);
    }

    #[test]
    fn test_cosmos_ed25519_batch_verify_one_msg_zero_sigs_pubkeys_works() {
        let codes = read_cosmos_sigs();

        let mut messages: Vec<Vec<u8>> = vec![];
        // Zero sigs / pubkeys
        let signatures: Vec<&[u8]> = vec![];
        let public_keys: Vec<&[u8]> = vec![];

        // Just one message
        for encoded in codes[..1].iter() {
            let message = hex::decode(&encoded.message).unwrap();
            messages.push(message);
        }
        let messages: Vec<&[u8]> = messages.iter().map(|m| m.as_slice()).collect();

        // ed25519_batch_verify() works for empty sigs / pubkeys
        assert!(ed25519_batch_verify(&messages, &signatures, &public_keys).unwrap());
    }

    #[test]
    fn test_cosmos_ed25519_batch_verify_one_pubkey_zero_msgs_sigs_works() {
        let codes = read_cosmos_sigs();

        // Zero msgs / sigs
        let messages: Vec<&[u8]> = vec![];
        let signatures: Vec<&[u8]> = vec![];
        let mut public_keys: Vec<Vec<u8>> = vec![];

        // Just one public key
        for encoded in codes[..1].iter() {
            let public_key = hex::decode(&encoded.public_key).unwrap();
            public_keys.push(public_key);
        }
        let public_keys: Vec<&[u8]> = public_keys.iter().map(|m| m.as_slice()).collect();

        // ed25519_batch_verify() works for empty msgs / sigs
        assert!(ed25519_batch_verify(&messages, &signatures, &public_keys).unwrap());
    }
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
        CryptoError::BatchErr {
            msg: msg.into(),
            #[cfg(feature = "backtraces")]
            backtrace: Backtrace::capture(),
        }
    }

    pub fn generic_err(msg: impl Into<String>) -> Self {
        CryptoError::GenericErr {
            msg: msg.into(),
            #[cfg(feature = "backtraces")]
            backtrace: Backtrace::capture(),
        }
    }

    pub fn invalid_hash_format() -> Self {
        CryptoError::InvalidHashFormat {
            #[cfg(feature = "backtraces")]
            backtrace: Backtrace::capture(),
        }
    }

    pub fn invalid_pubkey_format() -> Self {
        CryptoError::InvalidPubkeyFormat {
            #[cfg(feature = "backtraces")]
            backtrace: Backtrace::capture(),
        }
    }

    pub fn invalid_signature_format() -> Self {
        CryptoError::InvalidSignatureFormat {
            #[cfg(feature = "backtraces")]
            backtrace: Backtrace::capture(),
        }
    }

    pub fn invalid_recovery_param() -> Self {
        CryptoError::InvalidRecoveryParam {
            #[cfg(feature = "backtraces")]
            backtrace: Backtrace::capture(),
        }
    }

    /// Numeric error code that can easily be passed over the
    /// contract VM boundary.
    pub fn code(&self) -> u32 {
        match self {
            CryptoError::InvalidHashFormat { .. } => 3,
            CryptoError::InvalidSignatureFormat { .. } => 4,
            CryptoError::InvalidPubkeyFormat { .. } => 5,
            CryptoError::InvalidRecoveryParam { .. } => 6,
            CryptoError::BatchErr { .. } => 7,
            CryptoError::GenericErr { .. } => 10,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // constructors
    #[test]
    fn batch_err_works() {
        let error = CryptoError::batch_err("something went wrong in a batch way");
        match error {
            CryptoError::BatchErr { msg, .. } => {
                assert_eq!(msg, "something went wrong in a batch way")
            }
            _ => panic!("wrong error type!"),
        }
    }

    #[test]
    fn generic_err_works() {
        let error = CryptoError::generic_err("something went wrong in a general way");
        match error {
            CryptoError::GenericErr { msg, .. } => {
                assert_eq!(msg, "something went wrong in a general way")
            }
            _ => panic!("wrong error type!"),
        }
    }

    #[test]
    fn invalid_hash_format_works() {
        let error = CryptoError::invalid_hash_format();
        match error {
            CryptoError::InvalidHashFormat { .. } => {}
            _ => panic!("wrong error type!"),
        }
    }

    #[test]
    fn invalid_signature_format_works() {
        let error = CryptoError::invalid_signature_format();
        match error {
            CryptoError::InvalidSignatureFormat { .. } => {}
            _ => panic!("wrong error type!"),
        }
    }

    #[test]
    fn invalid_pubkey_format_works() {
        let error = CryptoError::invalid_pubkey_format();
        match error {
            CryptoError::InvalidPubkeyFormat { .. } => {}
            _ => panic!("wrong error type!"),
        }
    }
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
        assert_eq!(hash.as_ref().len(), 32);
        self.array = *GenericArray::from_slice(hash);
    }
}

impl OutputSizeUser for Identity256 {
    type OutputSize = U32;
}

impl FixedOutput for Identity256 {
    fn finalize_into(self, out: &mut Output<Self>) {
        *out = self.array;
    }
}

impl HashMarker for Identity256 {}

impl Reset for Identity256 {
    fn reset(&mut self) {
        *self = Self::default();
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
pub fn secp256k1_verify(
    message_hash: &[u8],
    signature: &[u8],
    public_key: &[u8],
) -> CryptoResult<bool> {
    let message_hash = read_hash(message_hash)?;
    let signature = read_signature(signature)?;
    check_pubkey(public_key)?;

    // Already hashed, just build Digest container
    let message_digest = Identity256::new().chain(message_hash);

    let mut signature =
        Signature::from_bytes(&signature).map_err(|e| CryptoError::generic_err(e.to_string()))?;
    // Non low-S signatures require normalization
    if let Some(normalized) = signature.normalize_s() {
        signature = normalized;
    }

    let public_key = VerifyingKey::from_sec1_bytes(public_key)
        .map_err(|e| CryptoError::generic_err(e.to_string()))?;

    match public_key.verify_digest(message_digest, &signature) {
        Ok(()) => Ok(true),
        Err(_) => Ok(false),
    }
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
pub fn secp256k1_recover_pubkey(
    message_hash: &[u8],
    signature: &[u8],
    recovery_param: u8,
) -> Result<Vec<u8>, CryptoError> {
    let message_hash = read_hash(message_hash)?;
    let signature = read_signature(signature)?;

    let id =
        recoverable::Id::new(recovery_param).map_err(|_| CryptoError::invalid_recovery_param())?;

    // Compose extended signature
    let signature =
        Signature::from_bytes(&signature).map_err(|e| CryptoError::generic_err(e.to_string()))?;
    let extended_signature = recoverable::Signature::new(&signature, id)
        .map_err(|e| CryptoError::generic_err(e.to_string()))?;

    // Recover
    let message_digest = Identity256::new().chain(message_hash);
    let pubkey = extended_signature
        .recover_verifying_key_from_digest(message_digest)
        .map_err(|e| CryptoError::generic_err(e.to_string()))?;
    let encoded: Vec<u8> = pubkey.to_encoded_point(false).as_bytes().into();
    Ok(encoded)
}

/// Error raised when hash is not 32 bytes long
struct InvalidSecp256k1HashFormat;

impl From<InvalidSecp256k1HashFormat> for CryptoError {
    fn from(_original: InvalidSecp256k1HashFormat) -> Self {
        CryptoError::invalid_hash_format()
    }
}

fn read_hash(data: &[u8]) -> Result<[u8; 32], InvalidSecp256k1HashFormat> {
    data.try_into().map_err(|_| InvalidSecp256k1HashFormat)
}

/// Error raised when signature is not 64 bytes long (32 bytes r, 32 bytes s)
struct InvalidSecp256k1SignatureFormat;

impl From<InvalidSecp256k1SignatureFormat> for CryptoError {
    fn from(_original: InvalidSecp256k1SignatureFormat) -> Self {
        CryptoError::invalid_signature_format()
    }
}

fn read_signature(data: &[u8]) -> Result<[u8; 64], InvalidSecp256k1SignatureFormat> {
    data.try_into().map_err(|_| InvalidSecp256k1SignatureFormat)
}

/// Error raised when public key is not in one of the two supported formats:
/// 1. Uncompressed: 65 bytes starting with 0x04
/// 2. Compressed: 33 bytes starting with 0x02 or 0x03
struct InvalidSecp256k1PubkeyFormat;

impl From<InvalidSecp256k1PubkeyFormat> for CryptoError {
    fn from(_original: InvalidSecp256k1PubkeyFormat) -> Self {
        CryptoError::invalid_pubkey_format()
    }
}

fn check_pubkey(data: &[u8]) -> Result<(), InvalidSecp256k1PubkeyFormat> {
    let ok = match data.first() {
        Some(0x02) | Some(0x03) => data.len() == ECDSA_COMPRESSED_PUBKEY_LEN,
        Some(0x04) => data.len() == ECDSA_UNCOMPRESSED_PUBKEY_LEN,
        _ => false,
    };
    if ok {
        Ok(())
    } else {
        Err(InvalidSecp256k1PubkeyFormat)
    }
}

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
    fn test_secp256k1_verify() {
        // Explicit / external hashing
        let message_digest = Sha256::new().chain(MSG);
        let message_hash = message_digest.clone().finalize();

        // Signing
        let secret_key = SigningKey::random(&mut OsRng); // Serialize with `::to_bytes()`

        // Note: the signature type must be annotated or otherwise inferrable as
        // `Signer` has many impls of the `Signer` trait (for both regular and
        // recoverable signature types).
        let signature: Signature = secret_key.sign_digest(message_digest);

        let public_key = VerifyingKey::from(&secret_key); // Serialize with `::to_encoded_point()`

        // Verification (uncompressed public key)
        assert!(secp256k1_verify(
            &message_hash,
            signature.as_bytes(),
            public_key.to_encoded_point(false).as_bytes()
        )
        .unwrap());

        // Verification (compressed public key)
        assert!(secp256k1_verify(
            &message_hash,
            signature.as_bytes(),
            public_key.to_encoded_point(true).as_bytes()
        )
        .unwrap());

        // Wrong message fails
        let bad_message_hash = Sha256::new().chain(MSG).chain("\0").finalize();
        assert!(!secp256k1_verify(
            &bad_message_hash,
            signature.as_bytes(),
            public_key.to_encoded_point(false).as_bytes()
        )
        .unwrap());

        // Other pubkey fails
        let other_secret_key = SigningKey::random(&mut OsRng);
        let other_public_key = VerifyingKey::from(&other_secret_key);
        assert!(!secp256k1_verify(
            &message_hash,
            signature.as_bytes(),
            other_public_key.to_encoded_point(false).as_bytes()
        )
        .unwrap());
    }

    #[test]
    fn test_cosmos_secp256k1_verify() {
        let public_key = base64::decode(COSMOS_SECP256K1_PUBKEY_BASE64).unwrap();

        for ((i, msg), sig) in (1..)
            .zip(&[
                COSMOS_SECP256K1_MSG_HEX1,
                COSMOS_SECP256K1_MSG_HEX2,
                COSMOS_SECP256K1_MSG_HEX3,
            ])
            .zip(&[
                COSMOS_SECP256K1_SIGNATURE_HEX1,
                COSMOS_SECP256K1_SIGNATURE_HEX2,
                COSMOS_SECP256K1_SIGNATURE_HEX3,
            ])
        {
            let message = hex::decode(msg).unwrap();
            let signature = hex::decode(sig).unwrap();

            // Explicit hash
            let message_hash = Sha256::digest(&message);

            // secp256k1_verify works
            assert!(
                secp256k1_verify(&message_hash, &signature, &public_key).unwrap(),
                "secp256k1_verify() failed (test case {})",
                i
            );
        }
    }

    #[test]
    fn test_cosmos_extra_secp256k1_verify() {
        use std::fs::File;
        use std::io::BufReader;

        use serde::Deserialize;

        #[derive(Deserialize, Debug)]
        struct Encoded {
            message: String,
            message_hash: String,
            signature: String,
            #[serde(rename = "pubkey")]
            public_key: String,
        }

        // Open the file in read-only mode with buffer.
        let file = File::open(COSMOS_SECP256K1_TESTS_JSON).unwrap();
        let reader = BufReader::new(file);

        let codes: Vec<Encoded> = serde_json::from_reader(reader).unwrap();

        for (i, encoded) in (1..).zip(codes) {
            let message = hex::decode(&encoded.message).unwrap();

            let hash = hex::decode(&encoded.message_hash).unwrap();
            let message_hash = Sha256::digest(&message);
            assert_eq!(hash.as_slice(), message_hash.as_slice());

            let signature = hex::decode(&encoded.signature).unwrap();

            let public_key = hex::decode(&encoded.public_key).unwrap();

            // secp256k1_verify() works
            assert!(
                secp256k1_verify(&message_hash, &signature, &public_key).unwrap(),
                "verify() failed (test case {})",
                i
            );
        }
    }

    #[test]
    fn secp256k1_recover_pubkey_works() {
        // Test data from https://github.com/ethereumjs/ethereumjs-util/blob/v6.1.0/test/index.js#L496
        {
            let private_key =
                hex!("3c9229289a6125f7fdf1885a77bb12c37a8d3b4962d936f7e3084dece32a3ca1");
            let expected = SigningKey::from_bytes(&private_key)
                .unwrap()
                .verifying_key()
                .to_encoded_point(false)
                .as_bytes()
                .to_vec();
            let r_s = hex!("99e71a99cb2270b8cac5254f9e99b6210c6c10224a1579cf389ef88b20a1abe9129ff05af364204442bdb53ab6f18a99ab48acc9326fa689f228040429e3ca66");
            let recovery_param: u8 = 0;
            let message_hash =
                hex!("82ff40c0a986c6a5cfad4ddf4c3aa6996f1a7837f9c398e17e5de5cbd5a12b28");
            let pubkey = secp256k1_recover_pubkey(&message_hash, &r_s, recovery_param).unwrap();
            assert_eq!(pubkey, expected);
        }

        // Test data from https://github.com/randombit/botan/blob/2.9.0/src/tests/data/pubkey/ecdsa_key_recovery.vec
        {
            let expected_x = "F3F8BB913AA68589A2C8C607A877AB05252ADBD963E1BE846DDEB8456942AEDC";
            let expected_y = "A2ED51F08CA3EF3DAC0A7504613D54CD539FC1B3CBC92453CD704B6A2D012B2C";
            let expected = hex::decode(format!("04{}{}", expected_x, expected_y)).unwrap();
            let r_s = hex!("E30F2E6A0F705F4FB5F8501BA79C7C0D3FAC847F1AD70B873E9797B17B89B39081F1A4457589F30D76AB9F89E748A68C8A94C30FE0BAC8FB5C0B54EA70BF6D2F");
            let recovery_param: u8 = 0;
            let message_hash =
                hex!("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF");
            let pubkey = secp256k1_recover_pubkey(&message_hash, &r_s, recovery_param).unwrap();
            assert_eq!(pubkey, expected);
        }

        // Test data calculated via Secp256k1.createSignature from @cosmjs/crypto
        {
            let expected = hex!("044a071e8a6e10aada2b8cf39fa3b5fb3400b04e99ea8ae64ceea1a977dbeaf5d5f8c8fbd10b71ab14cd561f7df8eb6da50f8a8d81ba564342244d26d1d4211595");
            let r_s = hex!("45c0b7f8c09a9e1f1cea0c25785594427b6bf8f9f878a8af0b1abbb48e16d0920d8becd0c220f67c51217eecfd7184ef0732481c843857e6bc7fc095c4f6b788");
            let recovery_param: u8 = 1;
            let message_hash =
                hex!("5ae8317d34d1e595e3fa7247db80c0af4320cce1116de187f8f7e2e099c0d8d0");
            let pubkey = secp256k1_recover_pubkey(&message_hash, &r_s, recovery_param).unwrap();
            assert_eq!(pubkey, expected);
        }
    }

    #[test]
    fn secp256k1_recover_pubkey_fails_for_invalid_recovery_param() {
        let r_s = hex!("45c0b7f8c09a9e1f1cea0c25785594427b6bf8f9f878a8af0b1abbb48e16d0920d8becd0c220f67c51217eecfd7184ef0732481c843857e6bc7fc095c4f6b788");
        let message_hash = hex!("5ae8317d34d1e595e3fa7247db80c0af4320cce1116de187f8f7e2e099c0d8d0");

        // 2 and 3 are explicitly unsupported
        let recovery_param: u8 = 2;
        match secp256k1_recover_pubkey(&message_hash, &r_s, recovery_param).unwrap_err() {
            CryptoError::InvalidRecoveryParam { .. } => {}
            err => panic!("Unexpected error: {}", err),
        }
        let recovery_param: u8 = 3;
        match secp256k1_recover_pubkey(&message_hash, &r_s, recovery_param).unwrap_err() {
            CryptoError::InvalidRecoveryParam { .. } => {}
            err => panic!("Unexpected error: {}", err),
        }

        // Other values are garbage
        let recovery_param: u8 = 4;
        match secp256k1_recover_pubkey(&message_hash, &r_s, recovery_param).unwrap_err() {
            CryptoError::InvalidRecoveryParam { .. } => {}
            err => panic!("Unexpected error: {}", err),
        }
        let recovery_param: u8 = 255;
        match secp256k1_recover_pubkey(&message_hash, &r_s, recovery_param).unwrap_err() {
            CryptoError::InvalidRecoveryParam { .. } => {}
            err => panic!("Unexpected error: {}", err),
        }
    }
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
    pub fn generic_err(msg: impl Into<String>) -> Self {
        ZKError::GenericErr {
            msg: msg.into(),
            #[cfg(feature = "backtraces")]
            backtrace: Backtrace::capture(),
        }
    }

    /// Numeric error code that can easily be passed over the
    /// contract VM boundary.
    pub fn code(&self) -> u32 {
        match self {
            ZKError::VerifierError { .. } => 3,
            ZKError::InvalidHashInput { .. } => 4,
            ZKError::GenericErr { .. } => 10,
        }
    }
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
    pub fn new() -> Self {
        Self {
            poseidon_width_3_bytes: PoseidonHasher::new(setup_params(Curve::Bn254, 5, 3)),
            poseidon_width_4_bytes: PoseidonHasher::new(setup_params(Curve::Bn254, 5, 4)),
            poseidon_width_5_bytes: PoseidonHasher::new(setup_params(Curve::Bn254, 5, 5)),
        }
    }

    pub fn hash(&self, inputs: &[&[u8]]) -> Result<Vec<u8>, ZKError> {
        let num_inputs = inputs.len();
        let mut packed_inputs = Vec::new();

        for &inp in inputs {
            packed_inputs.extend_from_slice(inp);
        }

        let input_f = to_field_elements(&packed_inputs)
            .map_err(|err| ZKError::generic_err(err.to_string()))?;

        let hash_result = match num_inputs {
            2 => self.poseidon_width_3_bytes.hash(&input_f),
            3 => self.poseidon_width_4_bytes.hash(&input_f),
            4 => self.poseidon_width_5_bytes.hash(&input_f),
            _ => return Err(ZKError::InvalidHashInput {}),
        };

        hash_result
            .map(|h| h.into_repr().to_bytes_le())
            .map_err(|err| ZKError::generic_err(err.to_string()))
    }
}

impl Default for Poseidon {
    fn default() -> Self {
        Self::new()
    }
}

#[test]
fn test_hash() {
    let p = Poseidon::new();
    let commitment_hash =
        hex::decode("84d6bdcfd953993012f08970d9c9b472d96114b4edc69481968cafc07877381c").unwrap();
    let ret = p.hash(&[&commitment_hash, &commitment_hash]);
    assert!(ret.is_ok())
}
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
    ) -> Result<bool, Error> {
        let public_input_field_elts = to_field_elements::<E::Fr>(public_inp_bytes)?;
        let vk = VerifyingKey::<E>::deserialize(vk_bytes)?;
        let proof = Proof::<E>::deserialize(proof_bytes)?;

        let res = Groth16::<E>::verify(&vk, &public_input_field_elts, &proof)?;
        Ok(res)
    }
}

pub type ArkworksVerifierBn254 = ArkworksVerifierGroth16<Bn254>;

pub fn groth16_verify(
    public_inp_bytes: &[u8],
    proof_bytes: &[u8],
    vk_bytes: &[u8],
) -> ZKResult<bool> {
    ArkworksVerifierBn254::verify(public_inp_bytes, proof_bytes, &vk_bytes)
        .map_err(|_| ZKError::VerifierError {})
}
}

#[allow(clippy::all)]
mod keccak {
use ark_bn254::Fr as Bn254Fr;
use ark_ff::{BigInteger, PrimeField};
use ark_std::vec::Vec;
use arkworks_setups::common::keccak_256;

pub fn curve_hash(input: &[u8]) -> Vec<u8> {
    // better secure
    let res = keccak_256(input);
    Bn254Fr::from_le_bytes_mod_order(&res)
        .into_repr()
        .to_bytes_le()
}
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
