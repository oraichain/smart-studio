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
    Bucket::new(storage, namespace)
}

/// An alias of ReadonlyBucket::new for less verbose usage
pub fn bucket_read<'a, T>(storage: &'a dyn Storage, namespace: &[u8]) -> ReadonlyBucket<'a, T>
where
    T: Serialize + DeserializeOwned,
{
    ReadonlyBucket::new(storage, namespace)
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
        Bucket {
            storage,
            prefix: to_length_prefixed(namespace),
            data: PhantomData,
        }
    }

    pub fn multilevel(storage: &'a mut dyn Storage, namespaces: &[&[u8]]) -> Self {
        Bucket {
            storage,
            prefix: to_length_prefixed_nested(namespaces),
            data: PhantomData,
        }
    }

    /// save will serialize the model and store, returns an error on serialization issues
    pub fn save(&mut self, key: &[u8], data: &T) -> StdResult<()> {
        set_with_prefix(self.storage, &self.prefix, key, &to_vec(data)?);
        Ok(())
    }

    pub fn remove(&mut self, key: &[u8]) {
        remove_with_prefix(self.storage, &self.prefix, key)
    }

    /// load will return an error if no data is set at the given key, or on parse error
    pub fn load(&self, key: &[u8]) -> StdResult<T> {
        let value = get_with_prefix(self.storage, &self.prefix, key);
        must_deserialize(&value)
    }

    /// may_load will parse the data stored at the key if present, returns Ok(None) if no data there.
    /// returns an error on issues parsing
    pub fn may_load(&self, key: &[u8]) -> StdResult<Option<T>> {
        let value = get_with_prefix(self.storage, &self.prefix, key);
        may_deserialize(&value)
    }

    #[cfg(feature = "iterator")]
    pub fn range<'b>(
        &'b self,
        start: Option<&[u8]>,
        end: Option<&[u8]>,
        order: Order,
    ) -> Box<dyn Iterator<Item = StdResult<Record<T>>> + 'b> {
        let mapped = range_with_prefix(self.storage, &self.prefix, start, end, order)
            .map(deserialize_kv::<T>);
        Box::new(mapped)
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
        let input = self.may_load(key)?;
        let output = action(input)?;
        self.save(key, &output)?;
        Ok(output)
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
        ReadonlyBucket {
            storage,
            prefix: to_length_prefixed(namespace),
            data: PhantomData,
        }
    }

    pub fn multilevel(storage: &'a dyn Storage, namespaces: &[&[u8]]) -> Self {
        ReadonlyBucket {
            storage,
            prefix: to_length_prefixed_nested(namespaces),
            data: PhantomData,
        }
    }

    /// load will return an error if no data is set at the given key, or on parse error
    pub fn load(&self, key: &[u8]) -> StdResult<T> {
        let value = get_with_prefix(self.storage, &self.prefix, key);
        must_deserialize(&value)
    }

    /// may_load will parse the data stored at the key if present, returns Ok(None) if no data there.
    /// returns an error on issues parsing
    pub fn may_load(&self, key: &[u8]) -> StdResult<Option<T>> {
        let value = get_with_prefix(self.storage, &self.prefix, key);
        may_deserialize(&value)
    }

    #[cfg(feature = "iterator")]
    pub fn range<'b>(
        &'b self,
        start: Option<&[u8]>,
        end: Option<&[u8]>,
        order: Order,
    ) -> Box<dyn Iterator<Item = StdResult<Record<T>>> + 'b> {
        let mapped = range_with_prefix(self.storage, &self.prefix, start, end, order)
            .map(deserialize_kv::<T>);
        Box::new(mapped)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use cosmwasm_std::testing::MockStorage;
    use cosmwasm_std::StdError;
    use serde::{Deserialize, Serialize};

    #[derive(Serialize, Deserialize, PartialEq, Debug, Clone)]
    struct Data {
        pub name: String,
        pub age: i32,
    }

    #[test]
    fn store_and_load() {
        let mut store = MockStorage::new();
        let mut bucket = bucket::<Data>(&mut store, b"data");

        // save data
        let data = Data {
            name: "Maria".to_string(),
            age: 42,
        };
        bucket.save(b"maria", &data).unwrap();

        // load it properly
        let loaded = bucket.load(b"maria").unwrap();
        assert_eq!(data, loaded);
    }

    #[test]
    fn remove_works() {
        let mut store = MockStorage::new();
        let mut bucket = bucket::<Data>(&mut store, b"data");

        // save data
        let data = Data {
            name: "Maria".to_string(),
            age: 42,
        };
        bucket.save(b"maria", &data).unwrap();
        assert_eq!(data, bucket.load(b"maria").unwrap());

        // deleting random key does nothing
        bucket.remove(b"foobar");
        assert_eq!(data, bucket.load(b"maria").unwrap());

        // deleting maria removes the data
        bucket.remove(b"maria");
        assert_eq!(None, bucket.may_load(b"maria").unwrap());
    }

    #[test]
    fn readonly_works() {
        let mut store = MockStorage::new();
        let mut bucket = bucket::<Data>(&mut store, b"data");

        // save data
        let data = Data {
            name: "Maria".to_string(),
            age: 42,
        };
        bucket.save(b"maria", &data).unwrap();

        let reader = bucket_read::<Data>(&store, b"data");

        // check empty data handling
        assert!(reader.load(b"john").is_err());
        assert_eq!(reader.may_load(b"john").unwrap(), None);

        // load it properly
        let loaded = reader.load(b"maria").unwrap();
        assert_eq!(data, loaded);
    }

    #[test]
    fn buckets_isolated() {
        let mut store = MockStorage::new();
        let mut bucket1 = bucket::<Data>(&mut store, b"data");

        // save data
        let data = Data {
            name: "Maria".to_string(),
            age: 42,
        };
        bucket1.save(b"maria", &data).unwrap();

        let mut bucket2 = bucket::<Data>(&mut store, b"dat");

        // save data (dat, amaria) vs (data, maria)
        let data2 = Data {
            name: "Amen".to_string(),
            age: 67,
        };
        bucket2.save(b"amaria", &data2).unwrap();

        // load one
        let reader = bucket_read::<Data>(&store, b"data");
        let loaded = reader.load(b"maria").unwrap();
        assert_eq!(data, loaded);
        // no cross load
        assert_eq!(None, reader.may_load(b"amaria").unwrap());

        // load the other
        let reader2 = bucket_read::<Data>(&store, b"dat");
        let loaded2 = reader2.load(b"amaria").unwrap();
        assert_eq!(data2, loaded2);
        // no cross load
        assert_eq!(None, reader2.may_load(b"maria").unwrap());
    }

    #[test]
    fn update_success() {
        let mut store = MockStorage::new();
        let mut bucket = bucket::<Data>(&mut store, b"data");

        // initial data
        let init = Data {
            name: "Maria".to_string(),
            age: 42,
        };
        bucket.save(b"maria", &init).unwrap();

        // it's my birthday
        let birthday = |mayd: Option<Data>| -> StdResult<Data> {
            let mut d = mayd.ok_or_else(|| StdError::not_found("Data"))?;
            d.age += 1;
            Ok(d)
        };
        let output = bucket.update(b"maria", birthday).unwrap();
        let expected = Data {
            name: "Maria".to_string(),
            age: 43,
        };
        assert_eq!(output, expected);

        // load it properly
        let loaded = bucket.load(b"maria").unwrap();
        assert_eq!(loaded, expected);
    }

    #[test]
    fn update_can_change_variable_from_outer_scope() {
        let mut store = MockStorage::new();
        let mut bucket = bucket::<Data>(&mut store, b"data");
        let init = Data {
            name: "Maria".to_string(),
            age: 42,
        };
        bucket.save(b"maria", &init).unwrap();

        // show we can capture data from the closure
        let mut old_age = 0i32;
        bucket
            .update(b"maria", |mayd: Option<Data>| -> StdResult<_> {
                let mut d = mayd.ok_or_else(|| StdError::not_found("Data"))?;
                old_age = d.age;
                d.age += 1;
                Ok(d)
            })
            .unwrap();
        assert_eq!(old_age, 42);
    }

    #[test]
    fn update_fails_on_error() {
        let mut store = MockStorage::new();
        let mut bucket = bucket::<Data>(&mut store, b"data");

        // initial data
        let init = Data {
            name: "Maria".to_string(),
            age: 42,
        };
        bucket.save(b"maria", &init).unwrap();

        // it's my birthday
        let output = bucket.update(b"maria", |_d| {
            Err(StdError::generic_err("cuz i feel like it"))
        });
        assert!(output.is_err());

        // load it properly
        let loaded = bucket.load(b"maria").unwrap();
        assert_eq!(loaded, init);
    }

    #[test]
    fn update_supports_custom_error_types() {
        #[derive(Debug)]
        enum MyError {
            Std,
            NotFound,
        }

        impl From<StdError> for MyError {
            fn from(_original: StdError) -> MyError {
                MyError::Std
            }
        }

        let mut store = MockStorage::new();
        let mut bucket = bucket::<Data>(&mut store, b"data");

        // initial data
        let init = Data {
            name: "Maria".to_string(),
            age: 42,
        };
        bucket.save(b"maria", &init).unwrap();

        // it's my birthday
        let res = bucket.update(b"bob", |data| {
            if let Some(mut data) = data {
                if data.age < 0 {
                    // Uses Into to convert StdError to MyError
                    return Err(StdError::generic_err("Current age is negative").into());
                }
                if data.age > 10 {
                    to_vec(&data)?; // Uses From to convert StdError to MyError
                }
                data.age += 1;
                Ok(data)
            } else {
                Err(MyError::NotFound)
            }
        });
        match res.unwrap_err() {
            MyError::NotFound { .. } => {}
            err => panic!("Unexpected error: {:?}", err),
        }
    }

    #[test]
    fn update_handles_on_no_data() {
        let mut store = MockStorage::new();
        let mut bucket = bucket::<Data>(&mut store, b"data");

        let init_value = Data {
            name: "Maria".to_string(),
            age: 42,
        };

        // it's my birthday
        let output = bucket
            .update(b"maria", |d| match d {
                Some(_) => Err(StdError::generic_err("Ensure this was empty")),
                None => Ok(init_value.clone()),
            })
            .unwrap();
        assert_eq!(output, init_value);

        // nothing stored
        let loaded = bucket.load(b"maria").unwrap();
        assert_eq!(loaded, init_value);
    }

    #[test]
    #[cfg(feature = "iterator")]
    fn range_over_data() {
        let mut store = MockStorage::new();
        let mut bucket = bucket::<Data>(&mut store, b"data");

        let jose = Data {
            name: "Jose".to_string(),
            age: 42,
        };
        let maria = Data {
            name: "Maria".to_string(),
            age: 27,
        };

        bucket.save(b"maria", &maria).unwrap();
        bucket.save(b"jose", &jose).unwrap();

        let res_data: StdResult<Vec<Record<Data>>> =
            bucket.range(None, None, Order::Ascending).collect();
        let data = res_data.unwrap();
        assert_eq!(data.len(), 2);
        assert_eq!(data[0], (b"jose".to_vec(), jose.clone()));
        assert_eq!(data[1], (b"maria".to_vec(), maria.clone()));

        // also works for readonly
        let read_bucket = bucket_read::<Data>(&store, b"data");
        let res_data: StdResult<Vec<Record<Data>>> =
            read_bucket.range(None, None, Order::Ascending).collect();
        let data = res_data.unwrap();
        assert_eq!(data.len(), 2);
        assert_eq!(data[0], (b"jose".to_vec(), jose));
        assert_eq!(data[1], (b"maria".to_vec(), maria));
    }
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
    let mut out = Vec::with_capacity(namespace.len() + 2);
    out.extend_from_slice(&encode_length(namespace));
    out.extend_from_slice(namespace);
    out
}

/// Calculates the raw key prefix for a given nested namespace
/// as documented in https://github.com/webmaster128/key-namespacing#nesting
pub fn to_length_prefixed_nested(namespaces: &[&[u8]]) -> Vec<u8> {
    let mut size = 0;
    for &namespace in namespaces {
        size += namespace.len() + 2;
    }

    let mut out = Vec::with_capacity(size);
    for &namespace in namespaces {
        out.extend_from_slice(&encode_length(namespace));
        out.extend_from_slice(namespace);
    }
    out
}

/// Encodes the length of a given namespace as a 2 byte big endian encoded integer
fn encode_length(namespace: &[u8]) -> [u8; 2] {
    if namespace.len() > 0xFFFF {
        panic!("only supports namespaces up to length 0xFFFF")
    }
    let length_bytes = (namespace.len() as u32).to_be_bytes();
    [length_bytes[2], length_bytes[3]]
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn to_length_prefixed_works() {
        assert_eq!(to_length_prefixed(b""), b"\x00\x00");
        assert_eq!(to_length_prefixed(b"a"), b"\x00\x01a");
        assert_eq!(to_length_prefixed(b"ab"), b"\x00\x02ab");
        assert_eq!(to_length_prefixed(b"abc"), b"\x00\x03abc");
    }

    #[test]
    fn to_length_prefixed_works_for_long_prefix() {
        let long_namespace1 = vec![0; 256];
        let prefix1 = to_length_prefixed(&long_namespace1);
        assert_eq!(prefix1.len(), 256 + 2);
        assert_eq!(&prefix1[0..2], b"\x01\x00");

        let long_namespace2 = vec![0; 30000];
        let prefix2 = to_length_prefixed(&long_namespace2);
        assert_eq!(prefix2.len(), 30000 + 2);
        assert_eq!(&prefix2[0..2], b"\x75\x30");

        let long_namespace3 = vec![0; 0xFFFF];
        let prefix3 = to_length_prefixed(&long_namespace3);
        assert_eq!(prefix3.len(), 0xFFFF + 2);
        assert_eq!(&prefix3[0..2], b"\xFF\xFF");
    }

    #[test]
    #[should_panic(expected = "only supports namespaces up to length 0xFFFF")]
    fn to_length_prefixed_panics_for_too_long_prefix() {
        let limit = 0xFFFF;
        let long_namespace = vec![0; limit + 1];
        to_length_prefixed(&long_namespace);
    }

    #[test]
    fn to_length_prefixed_calculates_capacity_correctly() {
        // Those tests cannot guarantee the required capacity was calculated correctly before
        // the vector allocation but increase the likelyhood of a proper implementation.

        let key = to_length_prefixed(b"");
        assert_eq!(key.capacity(), key.len());

        let key = to_length_prefixed(b"h");
        assert_eq!(key.capacity(), key.len());

        let key = to_length_prefixed(b"hij");
        assert_eq!(key.capacity(), key.len());
    }

    #[test]
    fn to_length_prefixed_nested_works() {
        assert_eq!(to_length_prefixed_nested(&[]), b"");
        assert_eq!(to_length_prefixed_nested(&[b""]), b"\x00\x00");
        assert_eq!(to_length_prefixed_nested(&[b"", b""]), b"\x00\x00\x00\x00");

        assert_eq!(to_length_prefixed_nested(&[b"a"]), b"\x00\x01a");
        assert_eq!(
            to_length_prefixed_nested(&[b"a", b"ab"]),
            b"\x00\x01a\x00\x02ab"
        );
        assert_eq!(
            to_length_prefixed_nested(&[b"a", b"ab", b"abc"]),
            b"\x00\x01a\x00\x02ab\x00\x03abc"
        );
    }

    #[test]
    fn to_length_prefixed_nested_allows_many_long_namespaces() {
        // The 0xFFFF limit is for each namespace, not for the combination of them

        let long_namespace1 = vec![0xaa; 0xFFFD];
        let long_namespace2 = vec![0xbb; 0xFFFE];
        let long_namespace3 = vec![0xcc; 0xFFFF];

        let prefix =
            to_length_prefixed_nested(&[&long_namespace1, &long_namespace2, &long_namespace3]);
        assert_eq!(&prefix[0..2], b"\xFF\xFD");
        assert_eq!(&prefix[2..(2 + 0xFFFD)], long_namespace1.as_slice());
        assert_eq!(&prefix[(2 + 0xFFFD)..(2 + 0xFFFD + 2)], b"\xFF\xFe");
        assert_eq!(
            &prefix[(2 + 0xFFFD + 2)..(2 + 0xFFFD + 2 + 0xFFFE)],
            long_namespace2.as_slice()
        );
        assert_eq!(
            &prefix[(2 + 0xFFFD + 2 + 0xFFFE)..(2 + 0xFFFD + 2 + 0xFFFE + 2)],
            b"\xFF\xFf"
        );
        assert_eq!(
            &prefix[(2 + 0xFFFD + 2 + 0xFFFE + 2)..(2 + 0xFFFD + 2 + 0xFFFE + 2 + 0xFFFF)],
            long_namespace3.as_slice()
        );
    }

    #[test]
    fn to_length_prefixed_nested_calculates_capacity_correctly() {
        // Those tests cannot guarantee the required capacity was calculated correctly before
        // the vector allocation but increase the likelyhood of a proper implementation.

        let key = to_length_prefixed_nested(&[]);
        assert_eq!(key.capacity(), key.len());

        let key = to_length_prefixed_nested(&[b""]);
        assert_eq!(key.capacity(), key.len());

        let key = to_length_prefixed_nested(&[b"a"]);
        assert_eq!(key.capacity(), key.len());

        let key = to_length_prefixed_nested(&[b"a", b"bc"]);
        assert_eq!(key.capacity(), key.len());

        let key = to_length_prefixed_nested(&[b"a", b"bc", b"def"]);
        assert_eq!(key.capacity(), key.len());
    }

    #[test]
    fn encode_length_works() {
        assert_eq!(encode_length(b""), *b"\x00\x00");
        assert_eq!(encode_length(b"a"), *b"\x00\x01");
        assert_eq!(encode_length(b"aa"), *b"\x00\x02");
        assert_eq!(encode_length(b"aaa"), *b"\x00\x03");
        assert_eq!(encode_length(&vec![1; 255]), *b"\x00\xff");
        assert_eq!(encode_length(&vec![1; 256]), *b"\x01\x00");
        assert_eq!(encode_length(&vec![1; 12345]), *b"\x30\x39");
        assert_eq!(encode_length(&vec![1; 65535]), *b"\xff\xff");
    }

    #[test]
    #[should_panic(expected = "only supports namespaces up to length 0xFFFF")]
    fn encode_length_panics_for_large_values() {
        encode_length(&vec![1; 65536]);
    }
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
    storage.get(&concat(namespace, key))
}

pub(crate) fn set_with_prefix(
    storage: &mut dyn Storage,
    namespace: &[u8],
    key: &[u8],
    value: &[u8],
) {
    storage.set(&concat(namespace, key), value);
}

pub(crate) fn remove_with_prefix(storage: &mut dyn Storage, namespace: &[u8], key: &[u8]) {
    storage.remove(&concat(namespace, key));
}

#[inline]
fn concat(namespace: &[u8], key: &[u8]) -> Vec<u8> {
    let mut k = namespace.to_vec();
    k.extend_from_slice(key);
    k
}

#[cfg(feature = "iterator")]
pub(crate) fn range_with_prefix<'a>(
    storage: &'a dyn Storage,
    namespace: &[u8],
    start: Option<&[u8]>,
    end: Option<&[u8]>,
    order: Order,
) -> Box<dyn Iterator<Item = Record> + 'a> {
    // prepare start, end with prefix
    let start = match start {
        Some(s) => concat(namespace, s),
        None => namespace.to_vec(),
    };
    let end = match end {
        Some(e) => concat(namespace, e),
        // end is updating last byte by one
        None => namespace_upper_bound(namespace),
    };

    // get iterator from storage
    let base_iterator = storage.range(Some(&start), Some(&end), order);

    // make a copy for the closure to handle lifetimes safely
    let prefix = namespace.to_vec();
    let mapped = base_iterator.map(move |(k, v)| (trim(&prefix, &k), v));
    Box::new(mapped)
}

#[cfg(feature = "iterator")]
#[inline]
fn trim(namespace: &[u8], key: &[u8]) -> Vec<u8> {
    key[namespace.len()..].to_vec()
}

/// Returns a new vec of same length and last byte incremented by one
/// If last bytes are 255, we handle overflow up the chain.
/// If all bytes are 255, this returns wrong data - but that is never possible as a namespace
#[cfg(feature = "iterator")]
fn namespace_upper_bound(input: &[u8]) -> Vec<u8> {
    let mut copy = input.to_vec();
    // zero out all trailing 255, increment first that is not such
    for i in (0..input.len()).rev() {
        if copy[i] == 255 {
            copy[i] = 0;
        } else {
            copy[i] += 1;
            break;
        }
    }
    copy
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::length_prefixed::to_length_prefixed;
    use cosmwasm_std::testing::MockStorage;

    #[test]
    fn prefix_get_set() {
        let mut storage = MockStorage::new();
        let prefix = to_length_prefixed(b"foo");

        set_with_prefix(&mut storage, &prefix, b"bar", b"gotcha");
        let rfoo = get_with_prefix(&storage, &prefix, b"bar");
        assert_eq!(rfoo, Some(b"gotcha".to_vec()));

        // no collisions with other prefixes
        let other_prefix = to_length_prefixed(b"fo");
        let collision = get_with_prefix(&storage, &other_prefix, b"obar");
        assert_eq!(collision, None);
    }

    #[test]
    #[cfg(feature = "iterator")]
    fn range_works() {
        let mut storage = MockStorage::new();
        let prefix = to_length_prefixed(b"foo");
        let other_prefix = to_length_prefixed(b"food");

        // set some values in this range
        set_with_prefix(&mut storage, &prefix, b"bar", b"none");
        set_with_prefix(&mut storage, &prefix, b"snowy", b"day");

        // set some values outside this range
        set_with_prefix(&mut storage, &other_prefix, b"moon", b"buggy");

        // ensure we get proper result from prefixed_range iterator
        let mut iter = range_with_prefix(&storage, &prefix, None, None, Order::Descending);
        let first = iter.next().unwrap();
        assert_eq!(first, (b"snowy".to_vec(), b"day".to_vec()));
        let second = iter.next().unwrap();
        assert_eq!(second, (b"bar".to_vec(), b"none".to_vec()));
        assert!(iter.next().is_none());

        // ensure we get raw result from base range
        let iter = storage.range(None, None, Order::Ascending);
        assert_eq!(3, iter.count());

        // foo comes first
        let mut iter = storage.range(None, None, Order::Ascending);
        let first = iter.next().unwrap();
        let expected_key = concat(&prefix, b"bar");
        assert_eq!(first, (expected_key, b"none".to_vec()));
    }

    #[test]
    #[cfg(feature = "iterator")]
    fn range_with_prefix_wrapover() {
        let mut storage = MockStorage::new();
        // if we don't properly wrap over there will be issues here (note 255+1 is used to calculate end)
        let prefix = to_length_prefixed(b"f\xff\xff");
        let other_prefix = to_length_prefixed(b"f\xff\x44");

        // set some values in this range
        set_with_prefix(&mut storage, &prefix, b"bar", b"none");
        set_with_prefix(&mut storage, &prefix, b"snowy", b"day");

        // set some values outside this range
        set_with_prefix(&mut storage, &other_prefix, b"moon", b"buggy");

        // ensure we get proper result from prefixed_range iterator
        let iter = range_with_prefix(&storage, &prefix, None, None, Order::Descending);
        let elements: Vec<Record> = iter.collect();
        assert_eq!(
            elements,
            vec![
                (b"snowy".to_vec(), b"day".to_vec()),
                (b"bar".to_vec(), b"none".to_vec()),
            ]
        );
    }

    #[test]
    #[cfg(feature = "iterator")]
    fn range_with_start_end_set() {
        let mut storage = MockStorage::new();
        // if we don't properly wrap over there will be issues here (note 255+1 is used to calculate end)
        let prefix = to_length_prefixed(b"f\xff\xff");
        let other_prefix = to_length_prefixed(b"f\xff\x44");

        // set some values in this range
        set_with_prefix(&mut storage, &prefix, b"bar", b"none");
        set_with_prefix(&mut storage, &prefix, b"snowy", b"day");

        // set some values outside this range
        set_with_prefix(&mut storage, &other_prefix, b"moon", b"buggy");

        // make sure start and end are applied properly
        let res: Vec<Record> =
            range_with_prefix(&storage, &prefix, Some(b"b"), Some(b"c"), Order::Ascending)
                .collect();
        assert_eq!(res.len(), 1);
        assert_eq!(res[0], (b"bar".to_vec(), b"none".to_vec()));

        // make sure start and end are applied properly
        let res_count = range_with_prefix(
            &storage,
            &prefix,
            Some(b"bas"),
            Some(b"sno"),
            Order::Ascending,
        )
        .count();
        assert_eq!(res_count, 0);

        let res: Vec<Record> =
            range_with_prefix(&storage, &prefix, Some(b"ant"), None, Order::Ascending).collect();
        assert_eq!(res.len(), 2);
        assert_eq!(res[0], (b"bar".to_vec(), b"none".to_vec()));
        assert_eq!(res[1], (b"snowy".to_vec(), b"day".to_vec()));
    }

    #[test]
    #[cfg(feature = "iterator")]
    fn namespace_upper_bound_works() {
        assert_eq!(namespace_upper_bound(b"bob"), b"boc".to_vec());
        assert_eq!(namespace_upper_bound(b"fo\xfe"), b"fo\xff".to_vec());
        assert_eq!(namespace_upper_bound(b"fo\xff"), b"fp\x00".to_vec());
        // multiple \xff roll over
        assert_eq!(
            namespace_upper_bound(b"fo\xff\xff\xff"),
            b"fp\x00\x00\x00".to_vec()
        );
        // \xff not at the end are ignored
        assert_eq!(namespace_upper_bound(b"\xffabc"), b"\xffabd".to_vec());
    }
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
    PrefixedStorage::new(storage, namespace)
}

/// An alias of ReadonlyPrefixedStorage::new for less verbose usage
pub fn prefixed_read<'a>(
    storage: &'a dyn Storage,
    namespace: &[u8],
) -> ReadonlyPrefixedStorage<'a> {
    ReadonlyPrefixedStorage::new(storage, namespace)
}

pub struct PrefixedStorage<'a> {
    storage: &'a mut dyn Storage,
    prefix: Vec<u8>,
}

impl<'a> PrefixedStorage<'a> {
    pub fn new(storage: &'a mut dyn Storage, namespace: &[u8]) -> Self {
        PrefixedStorage {
            storage,
            prefix: to_length_prefixed(namespace),
        }
    }

    // Nested namespaces as documented in
    // https://github.com/webmaster128/key-namespacing#nesting
    pub fn multilevel(storage: &'a mut dyn Storage, namespaces: &[&[u8]]) -> Self {
        PrefixedStorage {
            storage,
            prefix: to_length_prefixed_nested(namespaces),
        }
    }
}

impl<'a> Storage for PrefixedStorage<'a> {
    fn get(&self, key: &[u8]) -> Option<Vec<u8>> {
        get_with_prefix(self.storage, &self.prefix, key)
    }

    fn set(&mut self, key: &[u8], value: &[u8]) {
        set_with_prefix(self.storage, &self.prefix, key, value);
    }

    fn remove(&mut self, key: &[u8]) {
        remove_with_prefix(self.storage, &self.prefix, key);
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
        range_with_prefix(self.storage, &self.prefix, start, end, order)
    }
}

pub struct ReadonlyPrefixedStorage<'a> {
    storage: &'a dyn Storage,
    prefix: Vec<u8>,
}

impl<'a> ReadonlyPrefixedStorage<'a> {
    pub fn new(storage: &'a dyn Storage, namespace: &[u8]) -> Self {
        ReadonlyPrefixedStorage {
            storage,
            prefix: to_length_prefixed(namespace),
        }
    }

    // Nested namespaces as documented in
    // https://github.com/webmaster128/key-namespacing#nesting
    pub fn multilevel(storage: &'a dyn Storage, namespaces: &[&[u8]]) -> Self {
        ReadonlyPrefixedStorage {
            storage,
            prefix: to_length_prefixed_nested(namespaces),
        }
    }
}

impl<'a> Storage for ReadonlyPrefixedStorage<'a> {
    fn get(&self, key: &[u8]) -> Option<Vec<u8>> {
        get_with_prefix(self.storage, &self.prefix, key)
    }

    fn set(&mut self, _key: &[u8], _value: &[u8]) {
        unimplemented!();
    }

    fn remove(&mut self, _key: &[u8]) {
        unimplemented!();
    }

    #[cfg(feature = "iterator")]
    /// range allows iteration over a set of keys, either forwards or backwards
    fn range<'b>(
        &'b self,
        start: Option<&[u8]>,
        end: Option<&[u8]>,
        order: Order,
    ) -> Box<dyn Iterator<Item = Record> + 'b> {
        range_with_prefix(self.storage, &self.prefix, start, end, order)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use cosmwasm_std::testing::MockStorage;

    #[test]
    fn prefixed_storage_set_and_get() {
        let mut storage = MockStorage::new();

        // set
        let mut s1 = PrefixedStorage::new(&mut storage, b"foo");
        s1.set(b"bar", b"gotcha");
        assert_eq!(storage.get(b"\x00\x03foobar").unwrap(), b"gotcha".to_vec());

        // get
        let s2 = PrefixedStorage::new(&mut storage, b"foo");
        assert_eq!(s2.get(b"bar"), Some(b"gotcha".to_vec()));
        assert_eq!(s2.get(b"elsewhere"), None);
    }

    #[test]
    fn prefixed_storage_multilevel_set_and_get() {
        let mut storage = MockStorage::new();

        // set
        let mut bar = PrefixedStorage::multilevel(&mut storage, &[b"foo", b"bar"]);
        bar.set(b"baz", b"winner");
        assert_eq!(
            storage.get(b"\x00\x03foo\x00\x03barbaz").unwrap(),
            b"winner".to_vec()
        );

        // get
        let bar = PrefixedStorage::multilevel(&mut storage, &[b"foo", b"bar"]);
        assert_eq!(bar.get(b"baz"), Some(b"winner".to_vec()));
        assert_eq!(bar.get(b"elsewhere"), None);
    }

    #[test]
    fn readonly_prefixed_storage_get() {
        let mut storage = MockStorage::new();
        storage.set(b"\x00\x03foobar", b"gotcha");

        // try readonly correctly
        let s1 = ReadonlyPrefixedStorage::new(&storage, b"foo");
        assert_eq!(s1.get(b"bar"), Some(b"gotcha".to_vec()));
        assert_eq!(s1.get(b"elsewhere"), None);

        // no collisions with other prefixes
        let s2 = ReadonlyPrefixedStorage::new(&storage, b"fo");
        assert_eq!(s2.get(b"obar"), None);
    }

    #[test]
    fn readonly_prefixed_storage_multilevel_get() {
        let mut storage = MockStorage::new();
        storage.set(b"\x00\x03foo\x00\x03barbaz", b"winner");

        let bar = ReadonlyPrefixedStorage::multilevel(&storage, &[b"foo", b"bar"]);
        assert_eq!(bar.get(b"baz"), Some(b"winner".to_vec()));
        assert_eq!(bar.get(b"elsewhere"), None);
    }
}
}
mod sequence {
use cosmwasm_std::{StdResult, Storage};

use crate::Singleton;

/// Sequence creates a custom Singleton to hold an empty sequence
pub fn sequence<'a>(storage: &'a mut dyn Storage, key: &[u8]) -> Singleton<'a, u64> {
    Singleton::new(storage, key)
}

/// currval returns the last value returned by nextval. If the sequence has never been used,
/// then it will return 0.
pub fn currval(seq: &Singleton<u64>) -> StdResult<u64> {
    Ok(seq.may_load()?.unwrap_or_default())
}

/// nextval increments the counter by 1 and returns the new value.
/// On the first time it is called (no sequence info in db) it will return 1.
pub fn nextval(seq: &mut Singleton<u64>) -> StdResult<u64> {
    let val = currval(seq)? + 1;
    seq.save(&val)?;
    Ok(val)
}

#[cfg(test)]
mod tests {
    use super::*;
    use cosmwasm_std::testing::MockStorage;

    #[test]
    fn walk_through_sequence() {
        let mut store = MockStorage::new();
        let mut seq = sequence(&mut store, b"seq");

        assert_eq!(currval(&seq).unwrap(), 0);
        assert_eq!(nextval(&mut seq).unwrap(), 1);
        assert_eq!(nextval(&mut seq).unwrap(), 2);
        assert_eq!(nextval(&mut seq).unwrap(), 3);
        assert_eq!(currval(&seq).unwrap(), 3);
        assert_eq!(currval(&seq).unwrap(), 3);
    }

    #[test]
    fn sequences_independent() {
        let mut store = MockStorage::new();

        let mut seq = sequence(&mut store, b"seq");
        assert_eq!(nextval(&mut seq).unwrap(), 1);
        assert_eq!(nextval(&mut seq).unwrap(), 2);
        assert_eq!(nextval(&mut seq).unwrap(), 3);

        let mut seq2 = sequence(&mut store, b"seq2");
        assert_eq!(nextval(&mut seq2).unwrap(), 1);
        assert_eq!(nextval(&mut seq2).unwrap(), 2);

        let mut seq3 = sequence(&mut store, b"seq");
        assert_eq!(nextval(&mut seq3).unwrap(), 4);
    }

    #[test]
    fn set_sequence() {
        let mut store = MockStorage::new();
        let mut seq = sequence(&mut store, b"seq");

        assert_eq!(nextval(&mut seq).unwrap(), 1);
        assert_eq!(nextval(&mut seq).unwrap(), 2);

        seq.save(&20).unwrap();

        assert_eq!(currval(&seq).unwrap(), 20);
        assert_eq!(nextval(&mut seq).unwrap(), 21);
    }
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
    Singleton::new(storage, key)
}

/// An alias of ReadonlySingleton::new for less verbose usage
pub fn singleton_read<'a, T>(storage: &'a dyn Storage, key: &[u8]) -> ReadonlySingleton<'a, T>
where
    T: Serialize + DeserializeOwned,
{
    ReadonlySingleton::new(storage, key)
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
        Singleton {
            storage,
            key: to_length_prefixed(key),
            data: PhantomData,
        }
    }

    /// save will serialize the model and store, returns an error on serialization issues
    pub fn save(&mut self, data: &T) -> StdResult<()> {
        self.storage.set(&self.key, &to_vec(data)?);
        Ok(())
    }

    pub fn remove(&mut self) {
        self.storage.remove(&self.key)
    }

    /// load will return an error if no data is set at the given key, or on parse error
    pub fn load(&self) -> StdResult<T> {
        let value = self.storage.get(&self.key);
        must_deserialize(&value)
    }

    /// may_load will parse the data stored at the key if present, returns Ok(None) if no data there.
    /// returns an error on issues parsing
    pub fn may_load(&self) -> StdResult<Option<T>> {
        let value = self.storage.get(&self.key);
        may_deserialize(&value)
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
        let input = self.load()?;
        let output = action(input)?;
        self.save(&output)?;
        Ok(output)
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
        ReadonlySingleton {
            storage,
            key: to_length_prefixed(key),
            data: PhantomData,
        }
    }

    /// load will return an error if no data is set at the given key, or on parse error
    pub fn load(&self) -> StdResult<T> {
        let value = self.storage.get(&self.key);
        must_deserialize(&value)
    }

    /// may_load will parse the data stored at the key if present, returns Ok(None) if no data there.
    /// returns an error on issues parsing
    pub fn may_load(&self) -> StdResult<Option<T>> {
        let value = self.storage.get(&self.key);
        may_deserialize(&value)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use cosmwasm_std::testing::MockStorage;
    use serde::{Deserialize, Serialize};

    use cosmwasm_std::{OverflowError, OverflowOperation, StdError};

    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    struct Config {
        pub owner: String,
        pub max_tokens: i32,
    }

    #[test]
    fn save_and_load() {
        let mut store = MockStorage::new();
        let mut single = Singleton::<Config>::new(&mut store, b"config");

        assert!(single.load().is_err());
        assert_eq!(single.may_load().unwrap(), None);

        let cfg = Config {
            owner: "admin".to_string(),
            max_tokens: 1234,
        };
        single.save(&cfg).unwrap();

        assert_eq!(cfg, single.load().unwrap());
    }

    #[test]
    fn remove_works() {
        let mut store = MockStorage::new();
        let mut single = Singleton::<Config>::new(&mut store, b"config");

        // store data
        let cfg = Config {
            owner: "admin".to_string(),
            max_tokens: 1234,
        };
        single.save(&cfg).unwrap();
        assert_eq!(cfg, single.load().unwrap());

        // remove it and loads None
        single.remove();
        assert_eq!(None, single.may_load().unwrap());

        // safe to remove 2 times
        single.remove();
        assert_eq!(None, single.may_load().unwrap());
    }

    #[test]
    fn isolated_reads() {
        let mut store = MockStorage::new();
        let mut writer = singleton::<Config>(&mut store, b"config");

        let cfg = Config {
            owner: "admin".to_string(),
            max_tokens: 1234,
        };
        writer.save(&cfg).unwrap();

        let reader = singleton_read::<Config>(&store, b"config");
        assert_eq!(cfg, reader.load().unwrap());

        let other_reader = singleton_read::<Config>(&store, b"config2");
        assert_eq!(other_reader.may_load().unwrap(), None);
    }

    #[test]
    fn update_success() {
        let mut store = MockStorage::new();
        let mut writer = singleton::<Config>(&mut store, b"config");

        let cfg = Config {
            owner: "admin".to_string(),
            max_tokens: 1234,
        };
        writer.save(&cfg).unwrap();

        let output = writer.update(|mut c| -> StdResult<_> {
            c.max_tokens *= 2;
            Ok(c)
        });
        let expected = Config {
            owner: "admin".to_string(),
            max_tokens: 2468,
        };
        assert_eq!(output.unwrap(), expected);
        assert_eq!(writer.load().unwrap(), expected);
    }

    #[test]
    fn update_can_change_variable_from_outer_scope() {
        let mut store = MockStorage::new();
        let mut writer = singleton::<Config>(&mut store, b"config");
        let cfg = Config {
            owner: "admin".to_string(),
            max_tokens: 1234,
        };
        writer.save(&cfg).unwrap();

        let mut old_max_tokens = 0i32;
        writer
            .update(|mut c| -> StdResult<_> {
                old_max_tokens = c.max_tokens;
                c.max_tokens *= 2;
                Ok(c)
            })
            .unwrap();
        assert_eq!(old_max_tokens, 1234);
    }

    #[test]
    fn update_does_not_change_data_on_error() {
        let mut store = MockStorage::new();
        let mut writer = singleton::<Config>(&mut store, b"config");

        let cfg = Config {
            owner: "admin".to_string(),
            max_tokens: 1234,
        };
        writer.save(&cfg).unwrap();

        let output = writer.update(|_c| {
            Err(StdError::from(OverflowError::new(
                OverflowOperation::Sub,
                4,
                7,
            )))
        });
        match output.unwrap_err() {
            StdError::Overflow { .. } => {}
            err => panic!("Unexpected error: {:?}", err),
        }
        assert_eq!(writer.load().unwrap(), cfg);
    }

    #[test]
    fn update_supports_custom_errors() {
        #[derive(Debug)]
        enum MyError {
            Std(StdError),
            Foo,
        }

        impl From<StdError> for MyError {
            fn from(original: StdError) -> MyError {
                MyError::Std(original)
            }
        }

        let mut store = MockStorage::new();
        let mut writer = singleton::<Config>(&mut store, b"config");

        let cfg = Config {
            owner: "admin".to_string(),
            max_tokens: 1234,
        };
        writer.save(&cfg).unwrap();

        let res = writer.update(|mut c| {
            if c.max_tokens > 5000 {
                return Err(MyError::Foo);
            }
            if c.max_tokens > 20 {
                return Err(StdError::generic_err("broken stuff").into()); // Uses Into to convert StdError to MyError
            }
            if c.max_tokens > 10 {
                to_vec(&c)?; // Uses From to convert StdError to MyError
            }
            c.max_tokens += 20;
            Ok(c)
        });
        match res.unwrap_err() {
            MyError::Std(StdError::GenericErr { .. }) => {}
            err => panic!("Unexpected error: {:?}", err),
        }
        assert_eq!(writer.load().unwrap(), cfg);
    }
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
    match value {
        Some(data) => Ok(Some(from_slice(data)?)),
        None => Ok(None),
    }
}

/// must_deserialize parses json bytes from storage (Option), returning NotFound error if no data present
pub(crate) fn must_deserialize<T: DeserializeOwned>(value: &Option<Vec<u8>>) -> StdResult<T> {
    match value {
        Some(data) => from_slice(data),
        None => Err(StdError::not_found(type_name::<T>())),
    }
}

#[cfg(feature = "iterator")]
pub(crate) fn deserialize_kv<T: DeserializeOwned>(kv: Record<Vec<u8>>) -> StdResult<Record<T>> {
    let (k, v) = kv;
    let t = from_slice::<T>(&v)?;
    Ok((k, t))
}

#[cfg(test)]
mod tests {
    use super::*;
    use cosmwasm_std::{to_vec, StdError};
    use serde::{Deserialize, Serialize};

    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    struct Person {
        pub name: String,
        pub age: i32,
    }

    #[test]
    fn may_deserialize_handles_some() {
        let person = Person {
            name: "Maria".to_string(),
            age: 42,
        };
        let value = to_vec(&person).unwrap();

        let may_parse: Option<Person> = may_deserialize(&Some(value)).unwrap();
        assert_eq!(may_parse, Some(person));
    }

    #[test]
    fn may_deserialize_handles_none() {
        let may_parse = may_deserialize::<Person>(&None).unwrap();
        assert_eq!(may_parse, None);
    }

    #[test]
    fn must_deserialize_handles_some() {
        let person = Person {
            name: "Maria".to_string(),
            age: 42,
        };
        let value = to_vec(&person).unwrap();
        let loaded = Some(value);

        let parsed: Person = must_deserialize(&loaded).unwrap();
        assert_eq!(parsed, person);
    }

    #[test]
    fn must_deserialize_handles_none() {
        let parsed = must_deserialize::<Person>(&None);
        match parsed.unwrap_err() {
            StdError::NotFound { kind, .. } => {
                assert_eq!(kind, "cosmwasm_storage::type_helpers::tests::Person")
            }
            e => panic!("Unexpected error {}", e),
        }
    }
}
}

pub use bucket::{bucket, bucket_read, Bucket, ReadonlyBucket};
pub use length_prefixed::{to_length_prefixed, to_length_prefixed_nested};
pub use prefixed_storage::{prefixed, prefixed_read, PrefixedStorage, ReadonlyPrefixedStorage};
pub use sequence::{currval, nextval, sequence};
pub use singleton::{singleton, singleton_read, ReadonlySingleton, Singleton};
