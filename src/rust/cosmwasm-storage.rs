mod bucket {
use serde::{de::DeserializeOwned, ser::Serialize};
use std::marker::PhantomData;

use cosmwasm_std::{to_vec, StdError, StdResult, Storage};
#[cfg(feature = "iterator")]
use cosmwasm_std::{Order, Record};

use crate::length_prefixed::{to_length_prefixed, to_length_prefixed_nested};
#[cfg(feature = "iterator")]
use crate::namespace_helpers::range_with_prefix;
use crate::namespace_helpers::{get_with_prefix, remove_with_prefix, set_with_prefix};
#[cfg(feature = "iterator")]
use crate::type_helpers::deserialize_kv;
use crate::type_helpers::{may_deserialize, must_deserialize};

/// An alias of Bucket::new for less verbose usage
pub fn bucket<'a, T>(storage: &'a mut dyn Storage, namespace: &[u8]) -> Bucket<'a, T>
where
    T: Serialize + DeserializeOwned,
{
}

/// An alias of ReadonlyBucket::new for less verbose usage
pub fn bucket_read<'a, T>(storage: &'a dyn Storage, namespace: &[u8]) -> ReadonlyBucket<'a, T>
where
    T: Serialize + DeserializeOwned,
{
}

pub struct Bucket<'a, T>
where
    T: Serialize + DeserializeOwned,
{
    storage: &'a mut dyn Storage,
    prefix: Vec<u8>,
    // see https://doc.rust-lang.org/std/marker/struct.PhantomData.html#unused-type-parameters for why this is needed
    data: PhantomData<T>,
}

impl<'a, T> Bucket<'a, T>
where
    T: Serialize + DeserializeOwned,
{
    pub fn new(storage: &'a mut dyn Storage, namespace: &[u8]) -> Self {
}

    pub fn multilevel(storage: &'a mut dyn Storage, namespaces: &[&[u8]]) -> Self {
}

    /// save will serialize the model and store, returns an error on serialization issues
    pub fn save(&mut self, key: &[u8], data: &T) -> StdResult<()> {
}

    pub fn remove(&mut self, key: &[u8]) {
}

    /// load will return an error if no data is set at the given key, or on parse error
    pub fn load(&self, key: &[u8]) -> StdResult<T> {
}

    /// may_load will parse the data stored at the key if present, returns Ok(None) if no data there.
    /// returns an error on issues parsing
    pub fn may_load(&self, key: &[u8]) -> StdResult<Option<T>> {
}

    #[cfg(feature = "iterator")]
    pub fn range<'b>(
        &'b self,
        start: Option<&[u8]>,
        end: Option<&[u8]>,
        order: Order,
    ) -> Box<dyn Iterator<Item = StdResult<Record<T>>> + 'b> {
}

    /// Loads the data, perform the specified action, and store the result
    /// in the database. This is shorthand for some common sequences, which may be useful.
    ///
    /// If the data exists, `action(Some(value))` is called. Otherwise `action(None)` is called.
    pub fn update<A, E>(&mut self, key: &[u8], action: A) -> Result<T, E>
    where
        A: FnOnce(Option<T>) -> Result<T, E>,
        E: From<StdError>,
    {
}
}

pub struct ReadonlyBucket<'a, T>
where
    T: Serialize + DeserializeOwned,
{
    storage: &'a dyn Storage,
    prefix: Vec<u8>,
    // see https://doc.rust-lang.org/std/marker/struct.PhantomData.html#unused-type-parameters for why this is needed
    data: PhantomData<T>,
}

impl<'a, T> ReadonlyBucket<'a, T>
where
    T: Serialize + DeserializeOwned,
{
    pub fn new(storage: &'a dyn Storage, namespace: &[u8]) -> Self {
}

    pub fn multilevel(storage: &'a dyn Storage, namespaces: &[&[u8]]) -> Self {
}

    /// load will return an error if no data is set at the given key, or on parse error
    pub fn load(&self, key: &[u8]) -> StdResult<T> {
}

    /// may_load will parse the data stored at the key if present, returns Ok(None) if no data there.
    /// returns an error on issues parsing
    pub fn may_load(&self, key: &[u8]) -> StdResult<Option<T>> {
}

    #[cfg(feature = "iterator")]
    pub fn range<'b>(
        &'b self,
        start: Option<&[u8]>,
        end: Option<&[u8]>,
        order: Order,
    ) -> Box<dyn Iterator<Item = StdResult<Record<T>>> + 'b> {
}
}

#[cfg(test)]
mod tests {
}
}
mod length_prefixed {
//! This module is an implemention of a namespacing scheme described
//! in https://github.com/webmaster128/key-namespacing#length-prefixed-keys
//!
//! Everything in this file is only responsible for building such keys
//! and is in no way specific to any kind of storage.

/// Calculates the raw key prefix for a given namespace as documented
/// in https://github.com/webmaster128/key-namespacing#length-prefixed-keys
pub fn to_length_prefixed(namespace: &[u8]) -> Vec<u8> {
}

/// Calculates the raw key prefix for a given nested namespace
/// as documented in https://github.com/webmaster128/key-namespacing#nesting
pub fn to_length_prefixed_nested(namespaces: &[&[u8]]) -> Vec<u8> {
}

/// Encodes the length of a given namespace as a 2 byte big endian encoded integer
fn encode_length(namespace: &[u8]) -> [u8; 2] {
}

#[cfg(test)]
mod tests {
}
}
mod namespace_helpers {
use cosmwasm_std::Storage;
#[cfg(feature = "iterator")]
use cosmwasm_std::{Order, Record};

pub(crate) fn get_with_prefix(
    storage: &dyn Storage,
    namespace: &[u8],
    key: &[u8],
) -> Option<Vec<u8>> {
}

pub(crate) fn set_with_prefix(
    storage: &mut dyn Storage,
    namespace: &[u8],
    key: &[u8],
    value: &[u8],
) {
}

pub(crate) fn remove_with_prefix(storage: &mut dyn Storage, namespace: &[u8], key: &[u8]) {
}

#[inline]
fn concat(namespace: &[u8], key: &[u8]) -> Vec<u8> {
}

#[cfg(feature = "iterator")]
pub(crate) fn range_with_prefix<'a>(
    storage: &'a dyn Storage,
    namespace: &[u8],
    start: Option<&[u8]>,
    end: Option<&[u8]>,
    order: Order,
) -> Box<dyn Iterator<Item = Record> + 'a> {
}

#[cfg(feature = "iterator")]
#[inline]
fn trim(namespace: &[u8], key: &[u8]) -> Vec<u8> {
}

/// Returns a new vec of same length and last byte incremented by one
/// If last bytes are 255, we handle overflow up the chain.
/// If all bytes are 255, this returns wrong data - but that is never possible as a namespace
#[cfg(feature = "iterator")]
fn namespace_upper_bound(input: &[u8]) -> Vec<u8> {
}

#[cfg(test)]
mod tests {
}
}
mod prefixed_storage {
use cosmwasm_std::Storage;
#[cfg(feature = "iterator")]
use cosmwasm_std::{Order, Record};

use crate::length_prefixed::{to_length_prefixed, to_length_prefixed_nested};
#[cfg(feature = "iterator")]
use crate::namespace_helpers::range_with_prefix;
use crate::namespace_helpers::{get_with_prefix, remove_with_prefix, set_with_prefix};

/// An alias of PrefixedStorage::new for less verbose usage
pub fn prefixed<'a>(storage: &'a mut dyn Storage, namespace: &[u8]) -> PrefixedStorage<'a> {
}

/// An alias of ReadonlyPrefixedStorage::new for less verbose usage
pub fn prefixed_read<'a>(
    storage: &'a dyn Storage,
    namespace: &[u8],
) -> ReadonlyPrefixedStorage<'a> {
}

pub struct PrefixedStorage<'a> {
    storage: &'a mut dyn Storage,
    prefix: Vec<u8>,
}

impl<'a> PrefixedStorage<'a> {
    pub fn new(storage: &'a mut dyn Storage, namespace: &[u8]) -> Self {
}

    // Nested namespaces as documented in
    // https://github.com/webmaster128/key-namespacing#nesting
    pub fn multilevel(storage: &'a mut dyn Storage, namespaces: &[&[u8]]) -> Self {
}
}

impl<'a> Storage for PrefixedStorage<'a> {
    fn get(&self, key: &[u8]) -> Option<Vec<u8>> {
}

    fn set(&mut self, key: &[u8], value: &[u8]) {
}

    fn remove(&mut self, key: &[u8]) {
}

    #[cfg(feature = "iterator")]
    /// range allows iteration over a set of keys, either forwards or backwards
    /// uses standard rust range notation, and eg db.range(b"foo"..b"bar") also works reverse
    fn range<'b>(
        &'b self,
        start: Option<&[u8]>,
        end: Option<&[u8]>,
        order: Order,
    ) -> Box<dyn Iterator<Item = Record> + 'b> {
}
}

pub struct ReadonlyPrefixedStorage<'a> {
    storage: &'a dyn Storage,
    prefix: Vec<u8>,
}

impl<'a> ReadonlyPrefixedStorage<'a> {
    pub fn new(storage: &'a dyn Storage, namespace: &[u8]) -> Self {
}

    // Nested namespaces as documented in
    // https://github.com/webmaster128/key-namespacing#nesting
    pub fn multilevel(storage: &'a dyn Storage, namespaces: &[&[u8]]) -> Self {
}
}

impl<'a> Storage for ReadonlyPrefixedStorage<'a> {
    fn get(&self, key: &[u8]) -> Option<Vec<u8>> {
}

    fn set(&mut self, _key: &[u8], _value: &[u8]) {
}

    fn remove(&mut self, _key: &[u8]) {
}

    #[cfg(feature = "iterator")]
    /// range allows iteration over a set of keys, either forwards or backwards
    fn range<'b>(
        &'b self,
        start: Option<&[u8]>,
        end: Option<&[u8]>,
        order: Order,
    ) -> Box<dyn Iterator<Item = Record> + 'b> {
}
}

#[cfg(test)]
mod tests {
}
}
mod sequence {
use cosmwasm_std::{StdResult, Storage};

use crate::Singleton;

/// Sequence creates a custom Singleton to hold an empty sequence
pub fn sequence<'a>(storage: &'a mut dyn Storage, key: &[u8]) -> Singleton<'a, u64> {
}

/// currval returns the last value returned by nextval. If the sequence has never been used,
/// then it will return 0.
pub fn currval(seq: &Singleton<u64>) -> StdResult<u64> {
}

/// nextval increments the counter by 1 and returns the new value.
/// On the first time it is called (no sequence info in db) it will return 1.
pub fn nextval(seq: &mut Singleton<u64>) -> StdResult<u64> {
}

#[cfg(test)]
mod tests {
}
}
mod singleton {
use serde::{de::DeserializeOwned, ser::Serialize};
use std::marker::PhantomData;

use cosmwasm_std::{to_vec, StdError, StdResult, Storage};

use crate::length_prefixed::to_length_prefixed;
use crate::type_helpers::{may_deserialize, must_deserialize};

/// An alias of Singleton::new for less verbose usage
pub fn singleton<'a, T>(storage: &'a mut dyn Storage, key: &[u8]) -> Singleton<'a, T>
where
    T: Serialize + DeserializeOwned,
{
}

/// An alias of ReadonlySingleton::new for less verbose usage
pub fn singleton_read<'a, T>(storage: &'a dyn Storage, key: &[u8]) -> ReadonlySingleton<'a, T>
where
    T: Serialize + DeserializeOwned,
{
}

/// Singleton effectively combines PrefixedStorage with TypedStorage to
/// work on a single storage key. It performs the to_length_prefixed transformation
/// on the given name to ensure no collisions, and then provides the standard
/// TypedStorage accessors, without requiring a key (which is defined in the constructor)
pub struct Singleton<'a, T>
where
    T: Serialize + DeserializeOwned,
{
    storage: &'a mut dyn Storage,
    key: Vec<u8>,
    // see https://doc.rust-lang.org/std/marker/struct.PhantomData.html#unused-type-parameters for why this is needed
    data: PhantomData<T>,
}

impl<'a, T> Singleton<'a, T>
where
    T: Serialize + DeserializeOwned,
{
    pub fn new(storage: &'a mut dyn Storage, key: &[u8]) -> Self {
}

    /// save will serialize the model and store, returns an error on serialization issues
    pub fn save(&mut self, data: &T) -> StdResult<()> {
}

    pub fn remove(&mut self) {
}

    /// load will return an error if no data is set at the given key, or on parse error
    pub fn load(&self) -> StdResult<T> {
}

    /// may_load will parse the data stored at the key if present, returns Ok(None) if no data there.
    /// returns an error on issues parsing
    pub fn may_load(&self) -> StdResult<Option<T>> {
}

    /// update will load the data, perform the specified action, and store the result
    /// in the database. This is shorthand for some common sequences, which may be useful
    ///
    /// This is the least stable of the APIs, and definitely needs some usage
    pub fn update<A, E>(&mut self, action: A) -> Result<T, E>
    where
        A: FnOnce(T) -> Result<T, E>,
        E: From<StdError>,
    {
}
}

/// ReadonlySingleton only requires a Storage and exposes only the
/// methods of Singleton that don't modify state.
pub struct ReadonlySingleton<'a, T>
where
    T: Serialize + DeserializeOwned,
{
    storage: &'a dyn Storage,
    key: Vec<u8>,
    // see https://doc.rust-lang.org/std/marker/struct.PhantomData.html#unused-type-parameters for why this is needed
    data: PhantomData<T>,
}

impl<'a, T> ReadonlySingleton<'a, T>
where
    T: Serialize + DeserializeOwned,
{
    pub fn new(storage: &'a dyn Storage, key: &[u8]) -> Self {
}

    /// load will return an error if no data is set at the given key, or on parse error
    pub fn load(&self) -> StdResult<T> {
}

    /// may_load will parse the data stored at the key if present, returns Ok(None) if no data there.
    /// returns an error on issues parsing
    pub fn may_load(&self) -> StdResult<Option<T>> {
}
}

#[cfg(test)]
mod tests {
}
}
mod type_helpers {
use serde::de::DeserializeOwned;
use std::any::type_name;

#[cfg(feature = "iterator")]
use cosmwasm_std::Record;
use cosmwasm_std::{from_slice, StdError, StdResult};

/// may_deserialize parses json bytes from storage (Option), returning Ok(None) if no data present
///
/// value is an odd type, but this is meant to be easy to use with output from storage.get (Option<Vec<u8>>)
/// and value.map(|s| s.as_slice()) seems trickier than &value
pub(crate) fn may_deserialize<T: DeserializeOwned>(
    value: &Option<Vec<u8>>,
) -> StdResult<Option<T>> {
}

/// must_deserialize parses json bytes from storage (Option), returning NotFound error if no data present
pub(crate) fn must_deserialize<T: DeserializeOwned>(value: &Option<Vec<u8>>) -> StdResult<T> {
}

#[cfg(feature = "iterator")]
pub(crate) fn deserialize_kv<T: DeserializeOwned>(kv: Record<Vec<u8>>) -> StdResult<Record<T>> {
}

#[cfg(test)]
mod tests {
}
}

pub use bucket::{bucket, bucket_read, Bucket, ReadonlyBucket};
pub use length_prefixed::{to_length_prefixed, to_length_prefixed_nested};
pub use prefixed_storage::{prefixed, prefixed_read, PrefixedStorage, ReadonlyPrefixedStorage};
pub use sequence::{currval, nextval, sequence};
pub use singleton::{singleton, singleton_read, ReadonlySingleton, Singleton};
