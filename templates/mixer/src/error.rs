use cosmwasm_std::StdError;
use thiserror::Error;

#[derive(Error, Debug, PartialEq)]
pub enum ContractError {
    #[error("{0}")]
    Std(#[from] StdError),

    #[error("Unauthorized")]
    Unauthorized {},

    #[error("Unnecessary_funds")]
    UnnecessaryFunds {},

    #[error("Insufficient_funds")]
    InsufficientFunds {},

    /* -------   mixer related error  ------- */
    /// Returned if the mixer is not initialized
    #[error("NotInitialized")]
    NotInitialized,
    /// Returned if the mixer is already initialized
    #[error("AlreadyInitialized")]
    AlreadyInitialized,
    /// Returned if the merkle tree is full.
    #[error("FullMerkleTree")]
    MerkleTreeIsFull,
    /// Hash error
    #[error("HashError")]
    HashError,
    /// Verify error
    #[error("VerifyError")]
    VerifyError,
    // Failing to decode a hex string
    #[error("DecodeError")]
    DecodeError,

    // Returned if a mapping item is not found
    #[error("Mapping item not found")]
    ItemNotFound,

    #[error("Invalid merkle roots")]
    InvaidMerkleRoots,

    #[error("Unknown root")]
    UnknownRoot,

    #[error("Invalid withdraw proof")]
    InvalidWithdrawProof,

    #[error("Invalid arbitrary data passed")]
    InvalidArbitraryData,

    #[error("Invalid nullifier that is already used")]
    AlreadyRevealedNullfier,
}
