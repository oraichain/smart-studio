//! # The Rust core allocation and collections library
//!
//! This library provides smart pointers and collections for managing
//! heap-allocated values.
//!
//! This library, like libcore, normally doesn’t need to be used directly
//! since its contents are re-exported in the [`std` crate](../std/index.html).
//! Crates that use the `#![no_std]` attribute however will typically
//! not depend on `std`, so they’d use this crate instead.
//!
//! ## Boxed values
//!
//! The [`Box`] type is a smart pointer type. There can only be one owner of a
//! [`Box`], and the owner can decide to mutate the contents, which live on the
//! heap.
//!
//! This type can be sent among threads efficiently as the size of a `Box` value
//! is the same as that of a pointer. Tree-like data structures are often built
//! with boxes because each node often has only one owner, the parent.
//!
//! ## Reference counted pointers
//!
//! The [`Rc`] type is a non-threadsafe reference-counted pointer type intended
//! for sharing memory within a thread. An [`Rc`] pointer wraps a type, `T`, and
//! only allows access to `&T`, a shared reference.
//!
//! This type is useful when inherited mutability (such as using [`Box`]) is too
//! constraining for an application, and is often paired with the [`Cell`] or
//! [`RefCell`] types in order to allow mutation.
//!
//! ## Atomically reference counted pointers
//!
//! The [`Arc`] type is the threadsafe equivalent of the [`Rc`] type. It
//! provides all the same functionality of [`Rc`], except it requires that the
//! contained type `T` is shareable. Additionally, [`Arc<T>`][`Arc`] is itself
//! sendable while [`Rc<T>`][`Rc`] is not.
//!
//! This type allows for shared access to the contained data, and is often
//! paired with synchronization primitives such as mutexes to allow mutation of
//! shared resources.
//!
//! ## Collections
//!
//! Implementations of the most common general purpose data structures are
//! defined in this library. They are re-exported through the
//! [standard collections library](../std/collections/index.html).
//!
//! ## Heap interfaces
//!
//! The [`alloc`](alloc/index.html) module defines the low-level interface to the
//! default global allocator. It is not compatible with the libc allocator API.
//!
//! [`Arc`]: sync
//! [`Box`]: boxed
//! [`Cell`]: core::cell
//! [`Rc`]: rc
//! [`RefCell`]: core::cell

// To run liballoc tests without x.py without ending up with two copies of liballoc, Miri needs to be
// able to "empty" this crate. See <https://github.com/rust-lang/miri-test-libstd/issues/4>.
// rustc itself never sets the feature, so this line has no affect there.
#![cfg(any(not(feature = "miri-test-libstd"), test, doctest))]
#![allow(unused_attributes)]
#![stable(feature = "alloc", since = "1.36.0")]
#![doc(
    html_playground_url = "https://play.rust-lang.org/",
    issue_tracker_base_url = "https://github.com/rust-lang/rust/issues/",
    test(no_crate_inject, attr(allow(unused_variables), deny(warnings)))
)]
#![no_std]
#![needs_allocator]
#![warn(deprecated_in_future)]
#![warn(missing_docs)]
#![warn(missing_debug_implementations)]
#![allow(explicit_outlives_requirements)]
#![deny(unsafe_op_in_unsafe_fn)]
#![feature(rustc_allow_const_fn_unstable)]
#![cfg_attr(not(test), feature(generator_trait))]
#![cfg_attr(test, feature(test))]
#![cfg_attr(test, feature(new_uninit))]
#![feature(allocator_api)]
#![feature(array_chunks)]
#![feature(array_methods)]
#![feature(array_windows)]
#![feature(allow_internal_unstable)]
#![feature(arbitrary_self_types)]
#![feature(async_stream)]
#![cfg_attr(bootstrap, feature(bindings_after_at))]
#![feature(box_patterns)]
#![feature(box_syntax)]
#![feature(cfg_sanitize)]
#![feature(cfg_target_has_atomic)]
#![feature(coerce_unsized)]
#![cfg_attr(not(no_global_oom_handling), feature(const_btree_new))]
#![feature(const_fn_trait_bound)]
#![feature(cow_is_borrowed)]
#![feature(const_cow_is_borrowed)]
#![feature(const_trait_impl)]
#![feature(destructuring_assignment)]
#![feature(dispatch_from_dyn)]
#![feature(core_intrinsics)]
#![feature(dropck_eyepatch)]
#![feature(exact_size_is_empty)]
#![feature(exclusive_range_pattern)]
#![feature(extend_one)]
#![feature(fmt_internals)]
#![feature(fn_traits)]
#![feature(fundamental)]
#![feature(inplace_iteration)]
// Technically, this is a bug in rustdoc: rustdoc sees the documentation on `#[lang = slice_alloc]`
// blocks is for `&[T]`, which also has documentation using this feature in `core`, and gets mad
// that the feature-gate isn't enabled. Ideally, it wouldn't check for the feature gate for docs
// from other crates, but since this can only appear for lang items, it doesn't seem worth fixing.
#![feature(intra_doc_pointers)]
#![feature(iter_zip)]
#![feature(lang_items)]
#![feature(layout_for_ptr)]
#![feature(negative_impls)]
#![feature(never_type)]
#![feature(nll)]
#![feature(nonnull_slice_from_raw_parts)]
#![feature(auto_traits)]
#![feature(option_result_unwrap_unchecked)]
#![feature(pattern)]
#![feature(ptr_internals)]
#![feature(rustc_attrs)]
#![feature(receiver_trait)]
#![feature(min_specialization)]
#![feature(set_ptr_value)]
#![feature(slice_ptr_get)]
#![feature(slice_ptr_len)]
#![feature(slice_range)]
#![feature(staged_api)]
#![feature(str_internals)]
#![feature(trusted_len)]
#![feature(unboxed_closures)]
#![feature(unicode_internals)]
#![feature(unsize)]
#![feature(unsized_fn_params)]
#![feature(allocator_internals)]
#![feature(slice_partition_dedup)]
#![feature(maybe_uninit_extra, maybe_uninit_slice, maybe_uninit_uninit_array)]
#![feature(alloc_layout_extra)]
#![feature(trusted_random_access)]
#![feature(try_trait_v2)]
#![cfg_attr(bootstrap, feature(min_type_alias_impl_trait))]
#![cfg_attr(not(bootstrap), feature(type_alias_impl_trait))]
#![feature(associated_type_bounds)]
#![feature(slice_group_by)]
#![feature(decl_macro)]
// Allow testing this library

#[cfg(test)]
#[macro_use]
extern crate std;
#[cfg(test)]
extern crate test;

// Module with internal macros used by other modules (needs to be included before other modules).
#[macro_use]
mod macros {
/// Creates a [`Vec`] containing the arguments.
///
/// `vec!` allows `Vec`s to be defined with the same syntax as array expressions.
/// There are two forms of this macro:
///
/// - Create a [`Vec`] containing a given list of elements:
///
/// ```
/// let v = vec![1, 2, 3];
/// assert_eq!(v[0], 1);
/// assert_eq!(v[1], 2);
/// assert_eq!(v[2], 3);
/// ```
///
/// - Create a [`Vec`] from a given element and size:
///
/// ```
/// let v = vec![1; 3];
/// assert_eq!(v, [1, 1, 1]);
/// ```
///
/// Note that unlike array expressions this syntax supports all elements
/// which implement [`Clone`] and the number of elements doesn't have to be
/// a constant.
///
/// This will use `clone` to duplicate an expression, so one should be careful
/// using this with types having a nonstandard `Clone` implementation. For
/// example, `vec![Rc::new(1); 5]` will create a vector of five references
/// to the same boxed integer value, not five references pointing to independently
/// boxed integers.
///
/// Also, note that `vec![expr; 0]` is allowed, and produces an empty vector.
/// This will still evaluate `expr`, however, and immediately drop the resulting value, so
/// be mindful of side effects.
///
/// [`Vec`]: crate::vec::Vec
#[cfg(not(test))]
#[macro_export]
#[stable(feature = "rust1", since = "1.0.0")]
#[allow_internal_unstable(box_syntax, liballoc_internals)]
macro_rules! vec {
    () => (
        $crate::__rust_force_expr!($crate::vec::Vec::new())
    );
    ($elem:expr; $n:expr) => (
        $crate::__rust_force_expr!($crate::vec::from_elem($elem, $n))
    );
    ($($x:expr),+ $(,)?) => (
        $crate::__rust_force_expr!(<[_]>::into_vec(box [$($x),+]))
    );
}

// HACK(japaric): with cfg(test) the inherent `[T]::into_vec` method, which is
// required for this macro definition, is not available. Instead use the
// `slice::into_vec`  function which is only available with cfg(test)
// NB see the slice::hack module in slice.rs for more information
#[cfg(test)]
macro_rules! vec {
}

/// Creates a `String` using interpolation of runtime expressions.
///
/// The first argument `format!` receives is a format string. This must be a string
/// literal. The power of the formatting string is in the `{}`s contained.
///
/// Additional parameters passed to `format!` replace the `{}`s within the
/// formatting string in the order given unless named or positional parameters
/// are used; see [`std::fmt`] for more information.
///
/// A common use for `format!` is concatenation and interpolation of strings.
/// The same convention is used with [`print!`] and [`write!`] macros,
/// depending on the intended destination of the string.
///
/// To convert a single value to a string, use the [`to_string`] method. This
/// will use the [`Display`] formatting trait.
///
/// [`std::fmt`]: ../std/fmt/index.html
/// [`print!`]: ../std/macro.print.html
/// [`write!`]: core::write
/// [`to_string`]: crate::string::ToString
/// [`Display`]: core::fmt::Display
///
/// # Panics
///
/// `format!` panics if a formatting trait implementation returns an error.
/// This indicates an incorrect implementation
/// since `fmt::Write for String` never returns an error itself.
///
/// # Examples
///
/// ```
/// format!("test");
/// format!("hello {}", "world!");
/// format!("x = {}, y = {y}", 10, y = 30);
/// ```
#[macro_export]
#[stable(feature = "rust1", since = "1.0.0")]
#[cfg_attr(not(test), rustc_diagnostic_item = "format_macro")]
macro_rules! format {
    ($($arg:tt)*) => {{
        let res = $crate::fmt::format($crate::__export::format_args!($($arg)*));
        res
    }}
}

/// Force AST node to an expression to improve diagnostics in pattern position.
#[doc(hidden)]
#[macro_export]
#[unstable(feature = "liballoc_internals", issue = "none", reason = "implementation detail")]
macro_rules! __rust_force_expr {
    ($e:expr) => {
        $e
    };
}
}

// Heaps provided for low-level allocation strategies

pub mod alloc {
//! Memory allocation APIs

#![stable(feature = "alloc_module", since = "1.28.0")]

#[cfg(not(test))]
use core::intrinsics;
use core::intrinsics::{min_align_of_val, size_of_val};

use core::ptr::Unique;
#[cfg(not(test))]
use core::ptr::{self, NonNull};

#[stable(feature = "alloc_module", since = "1.28.0")]
#[doc(inline)]
pub use core::alloc::*;

#[cfg(test)]
mod tests {
}

extern "Rust" {
    // These are the magic symbols to call the global allocator.  rustc generates
    // them to call `__rg_alloc` etc. if there is a `#[global_allocator]` attribute
    // (the code expanding that attribute macro generates those functions), or to call
    // the default implementations in libstd (`__rdl_alloc` etc. in `library/std/src/alloc.rs`)
    // otherwise.
    // The rustc fork of LLVM also special-cases these function names to be able to optimize them
    // like `malloc`, `realloc`, and `free`, respectively.
    #[rustc_allocator]
    #[rustc_allocator_nounwind]
    fn __rust_alloc(size: usize, align: usize) -> *mut u8;
    #[rustc_allocator_nounwind]
    fn __rust_dealloc(ptr: *mut u8, size: usize, align: usize);
    #[rustc_allocator_nounwind]
    fn __rust_realloc(ptr: *mut u8, old_size: usize, align: usize, new_size: usize) -> *mut u8;
    #[rustc_allocator_nounwind]
    fn __rust_alloc_zeroed(size: usize, align: usize) -> *mut u8;
}

/// The global memory allocator.
///
/// This type implements the [`Allocator`] trait by forwarding calls
/// to the allocator registered with the `#[global_allocator]` attribute
/// if there is one, or the `std` crate’s default.
///
/// Note: while this type is unstable, the functionality it provides can be
/// accessed through the [free functions in `alloc`](self#functions).
#[unstable(feature = "allocator_api", issue = "32838")]
#[derive(Copy, Clone, Default, Debug)]
#[cfg(not(test))]
pub struct Global;

#[cfg(test)]
pub use std::alloc::Global;

/// Allocate memory with the global allocator.
///
/// This function forwards calls to the [`GlobalAlloc::alloc`] method
/// of the allocator registered with the `#[global_allocator]` attribute
/// if there is one, or the `std` crate’s default.
///
/// This function is expected to be deprecated in favor of the `alloc` method
/// of the [`Global`] type when it and the [`Allocator`] trait become stable.
///
/// # Safety
///
/// See [`GlobalAlloc::alloc`].
///
/// # Examples
///
/// ```
/// use std::alloc::{alloc, dealloc, Layout};
///
/// unsafe {
///     let layout = Layout::new::<u16>();
///     let ptr = alloc(layout);
///
///     *(ptr as *mut u16) = 42;
///     assert_eq!(*(ptr as *mut u16), 42);
///
///     dealloc(ptr, layout);
/// }
/// ```
#[stable(feature = "global_alloc", since = "1.28.0")]
#[inline]
pub unsafe fn alloc(layout: Layout) -> *mut u8 {
}

/// Deallocate memory with the global allocator.
///
/// This function forwards calls to the [`GlobalAlloc::dealloc`] method
/// of the allocator registered with the `#[global_allocator]` attribute
/// if there is one, or the `std` crate’s default.
///
/// This function is expected to be deprecated in favor of the `dealloc` method
/// of the [`Global`] type when it and the [`Allocator`] trait become stable.
///
/// # Safety
///
/// See [`GlobalAlloc::dealloc`].
#[stable(feature = "global_alloc", since = "1.28.0")]
#[inline]
pub unsafe fn dealloc(ptr: *mut u8, layout: Layout) {
}

/// Reallocate memory with the global allocator.
///
/// This function forwards calls to the [`GlobalAlloc::realloc`] method
/// of the allocator registered with the `#[global_allocator]` attribute
/// if there is one, or the `std` crate’s default.
///
/// This function is expected to be deprecated in favor of the `realloc` method
/// of the [`Global`] type when it and the [`Allocator`] trait become stable.
///
/// # Safety
///
/// See [`GlobalAlloc::realloc`].
#[stable(feature = "global_alloc", since = "1.28.0")]
#[inline]
pub unsafe fn realloc(ptr: *mut u8, layout: Layout, new_size: usize) -> *mut u8 {
}

/// Allocate zero-initialized memory with the global allocator.
///
/// This function forwards calls to the [`GlobalAlloc::alloc_zeroed`] method
/// of the allocator registered with the `#[global_allocator]` attribute
/// if there is one, or the `std` crate’s default.
///
/// This function is expected to be deprecated in favor of the `alloc_zeroed` method
/// of the [`Global`] type when it and the [`Allocator`] trait become stable.
///
/// # Safety
///
/// See [`GlobalAlloc::alloc_zeroed`].
///
/// # Examples
///
/// ```
/// use std::alloc::{alloc_zeroed, dealloc, Layout};
///
/// unsafe {
///     let layout = Layout::new::<u16>();
///     let ptr = alloc_zeroed(layout);
///
///     assert_eq!(*(ptr as *mut u16), 0);
///
///     dealloc(ptr, layout);
/// }
/// ```
#[stable(feature = "global_alloc", since = "1.28.0")]
#[inline]
pub unsafe fn alloc_zeroed(layout: Layout) -> *mut u8 {
}

#[cfg(not(test))]
impl Global {
    #[inline]
    fn alloc_impl(&self, layout: Layout, zeroed: bool) -> Result<NonNull<[u8]>, AllocError> {
}

    // SAFETY: Same as `Allocator::grow`
    #[inline]
    unsafe fn grow_impl(
        &self,
        ptr: NonNull<u8>,
        old_layout: Layout,
        new_layout: Layout,
        zeroed: bool,
    ) -> Result<NonNull<[u8]>, AllocError> {
}
}

#[unstable(feature = "allocator_api", issue = "32838")]
#[cfg(not(test))]
unsafe impl Allocator for Global {
    #[inline]
    fn allocate(&self, layout: Layout) -> Result<NonNull<[u8]>, AllocError> {
}

    #[inline]
    fn allocate_zeroed(&self, layout: Layout) -> Result<NonNull<[u8]>, AllocError> {
}

    #[inline]
    unsafe fn deallocate(&self, ptr: NonNull<u8>, layout: Layout) {
}

    #[inline]
    unsafe fn grow(
        &self,
        ptr: NonNull<u8>,
        old_layout: Layout,
        new_layout: Layout,
    ) -> Result<NonNull<[u8]>, AllocError> {
}

    #[inline]
    unsafe fn grow_zeroed(
        &self,
        ptr: NonNull<u8>,
        old_layout: Layout,
        new_layout: Layout,
    ) -> Result<NonNull<[u8]>, AllocError> {
}

    #[inline]
    unsafe fn shrink(
        &self,
        ptr: NonNull<u8>,
        old_layout: Layout,
        new_layout: Layout,
    ) -> Result<NonNull<[u8]>, AllocError> {
}
}

/// The allocator for unique pointers.
// This function must not unwind. If it does, MIR codegen will fail.
#[cfg(all(not(no_global_oom_handling), not(test)))]
#[lang = "exchange_malloc"]
#[inline]
unsafe fn exchange_malloc(size: usize, align: usize) -> *mut u8 {
}

#[cfg_attr(not(test), lang = "box_free")]
#[inline]
// This signature has to be the same as `Box`, otherwise an ICE will happen.
// When an additional parameter to `Box` is added (like `A: Allocator`), this has to be added here as
// well.
// For example if `Box` is changed to  `struct Box<T: ?Sized, A: Allocator>(Unique<T>, A)`,
// this function has to be changed to `fn box_free<T: ?Sized, A: Allocator>(Unique<T>, A)` as well.
pub(crate) unsafe fn box_free<T: ?Sized, A: Allocator>(ptr: Unique<T>, alloc: A) {
}

// # Allocation error handler

#[cfg(not(no_global_oom_handling))]
extern "Rust" {
    // This is the magic symbol to call the global alloc error handler.  rustc generates
    // it to call `__rg_oom` if there is a `#[alloc_error_handler]`, or to call the
    // default implementations below (`__rdl_oom`) otherwise.
    #[rustc_allocator_nounwind]
    fn __rust_alloc_error_handler(size: usize, align: usize) -> !;
}

/// Abort on memory allocation error or failure.
///
/// Callers of memory allocation APIs wishing to abort computation
/// in response to an allocation error are encouraged to call this function,
/// rather than directly invoking `panic!` or similar.
///
/// The default behavior of this function is to print a message to standard error
/// and abort the process.
/// It can be replaced with [`set_alloc_error_hook`] and [`take_alloc_error_hook`].
///
/// [`set_alloc_error_hook`]: ../../std/alloc/fn.set_alloc_error_hook.html
/// [`take_alloc_error_hook`]: ../../std/alloc/fn.take_alloc_error_hook.html
#[stable(feature = "global_alloc", since = "1.28.0")]
#[cfg(all(not(no_global_oom_handling), not(test)))]
#[rustc_allocator_nounwind]
#[cold]
pub fn handle_alloc_error(layout: Layout) -> ! {
    unsafe {
        __rust_alloc_error_handler(layout.size(), layout.align());
    }
}

// For alloc test `std::alloc::handle_alloc_error` can be used directly.
#[cfg(all(not(no_global_oom_handling), test))]
pub use std::alloc::handle_alloc_error;

#[cfg(all(not(no_global_oom_handling), not(any(target_os = "hermit", test))))]
#[doc(hidden)]
#[allow(unused_attributes)]
#[unstable(feature = "alloc_internals", issue = "none")]
pub mod __alloc_error_handler {
    use crate::alloc::Layout;

    // called via generated `__rust_alloc_error_handler`

    // if there is no `#[alloc_error_handler]`
    #[rustc_std_internal_symbol]
    pub unsafe extern "C" fn __rdl_oom(size: usize, _align: usize) -> ! {
}

    // if there is an `#[alloc_error_handler]`
    #[rustc_std_internal_symbol]
    pub unsafe extern "C" fn __rg_oom(size: usize, align: usize) -> ! {
}
}

/// Specialize clones into pre-allocated, uninitialized memory.
/// Used by `Box::clone` and `Rc`/`Arc::make_mut`.
pub(crate) trait WriteCloneIntoRaw: Sized {
    unsafe fn write_clone_into_raw(&self, target: *mut Self);
}

impl<T: Clone> WriteCloneIntoRaw for T {
    #[inline]
    default unsafe fn write_clone_into_raw(&self, target: *mut Self) {
}
}

impl<T: Copy> WriteCloneIntoRaw for T {
    #[inline]
    unsafe fn write_clone_into_raw(&self, target: *mut Self) {
}
}
}

// Primitive types using the heaps above

// Need to conditionally define the mod from `boxed.rs` to avoid
// duplicating the lang-items when building in test cfg; but also need
// to allow code to have `use boxed::Box;` declarations.
#[cfg(not(test))]
pub mod boxed {
//! A pointer type for heap allocation.
//!
//! [`Box<T>`], casually referred to as a 'box', provides the simplest form of
//! heap allocation in Rust. Boxes provide ownership for this allocation, and
//! drop their contents when they go out of scope. Boxes also ensure that they
//! never allocate more than `isize::MAX` bytes.
//!
//! # Examples
//!
//! Move a value from the stack to the heap by creating a [`Box`]:
//!
//! ```
//! let val: u8 = 5;
//! let boxed: Box<u8> = Box::new(val);
//! ```
//!
//! Move a value from a [`Box`] back to the stack by [dereferencing]:
//!
//! ```
//! let boxed: Box<u8> = Box::new(5);
//! let val: u8 = *boxed;
//! ```
//!
//! Creating a recursive data structure:
//!
//! ```
//! #[derive(Debug)]
//! enum List<T> {
//!     Cons(T, Box<List<T>>),
//!     Nil,
//! }
//!
//! let list: List<i32> = List::Cons(1, Box::new(List::Cons(2, Box::new(List::Nil))));
//! println!("{:?}", list);
//! ```
//!
//! This will print `Cons(1, Cons(2, Nil))`.
//!
//! Recursive structures must be boxed, because if the definition of `Cons`
//! looked like this:
//!
//! ```compile_fail,E0072
//! # enum List<T> {
//! Cons(T, List<T>),
//! # }
//! ```
//!
//! It wouldn't work. This is because the size of a `List` depends on how many
//! elements are in the list, and so we don't know how much memory to allocate
//! for a `Cons`. By introducing a [`Box<T>`], which has a defined size, we know how
//! big `Cons` needs to be.
//!
//! # Memory layout
//!
//! For non-zero-sized values, a [`Box`] will use the [`Global`] allocator for
//! its allocation. It is valid to convert both ways between a [`Box`] and a
//! raw pointer allocated with the [`Global`] allocator, given that the
//! [`Layout`] used with the allocator is correct for the type. More precisely,
//! a `value: *mut T` that has been allocated with the [`Global`] allocator
//! with `Layout::for_value(&*value)` may be converted into a box using
//! [`Box::<T>::from_raw(value)`]. Conversely, the memory backing a `value: *mut
//! T` obtained from [`Box::<T>::into_raw`] may be deallocated using the
//! [`Global`] allocator with [`Layout::for_value(&*value)`].
//!
//! For zero-sized values, the `Box` pointer still has to be [valid] for reads
//! and writes and sufficiently aligned. In particular, casting any aligned
//! non-zero integer literal to a raw pointer produces a valid pointer, but a
//! pointer pointing into previously allocated memory that since got freed is
//! not valid. The recommended way to build a Box to a ZST if `Box::new` cannot
//! be used is to use [`ptr::NonNull::dangling`].
//!
//! So long as `T: Sized`, a `Box<T>` is guaranteed to be represented
//! as a single pointer and is also ABI-compatible with C pointers
//! (i.e. the C type `T*`). This means that if you have extern "C"
//! Rust functions that will be called from C, you can define those
//! Rust functions using `Box<T>` types, and use `T*` as corresponding
//! type on the C side. As an example, consider this C header which
//! declares functions that create and destroy some kind of `Foo`
//! value:
//!
//! ```c
//! /* C header */
//!
//! /* Returns ownership to the caller */
//! struct Foo* foo_new(void);
//!
//! /* Takes ownership from the caller; no-op when invoked with null */
//! void foo_delete(struct Foo*);
//! ```
//!
//! These two functions might be implemented in Rust as follows. Here, the
//! `struct Foo*` type from C is translated to `Box<Foo>`, which captures
//! the ownership constraints. Note also that the nullable argument to
//! `foo_delete` is represented in Rust as `Option<Box<Foo>>`, since `Box<Foo>`
//! cannot be null.
//!
//! ```
//! #[repr(C)]
//! pub struct Foo;
//!
//! #[no_mangle]
//! pub extern "C" fn foo_new() -> Box<Foo> {
//!     Box::new(Foo)
//! }
//!
//! #[no_mangle]
//! pub extern "C" fn foo_delete(_: Option<Box<Foo>>) {}
//! ```
//!
//! Even though `Box<T>` has the same representation and C ABI as a C pointer,
//! this does not mean that you can convert an arbitrary `T*` into a `Box<T>`
//! and expect things to work. `Box<T>` values will always be fully aligned,
//! non-null pointers. Moreover, the destructor for `Box<T>` will attempt to
//! free the value with the global allocator. In general, the best practice
//! is to only use `Box<T>` for pointers that originated from the global
//! allocator.
//!
//! **Important.** At least at present, you should avoid using
//! `Box<T>` types for functions that are defined in C but invoked
//! from Rust. In those cases, you should directly mirror the C types
//! as closely as possible. Using types like `Box<T>` where the C
//! definition is just using `T*` can lead to undefined behavior, as
//! described in [rust-lang/unsafe-code-guidelines#198][ucg#198].
//!
//! [ucg#198]: https://github.com/rust-lang/unsafe-code-guidelines/issues/198
//! [dereferencing]: core::ops::Deref
//! [`Box::<T>::from_raw(value)`]: Box::from_raw
//! [`Global`]: crate::alloc::Global
//! [`Layout`]: crate::alloc::Layout
//! [`Layout::for_value(&*value)`]: crate::alloc::Layout::for_value
//! [valid]: ptr#safety

#![stable(feature = "rust1", since = "1.0.0")]

use core::any::Any;
use core::borrow;
use core::cmp::Ordering;
use core::convert::{From, TryFrom};
use core::fmt;
use core::future::Future;
use core::hash::{Hash, Hasher};
#[cfg(not(no_global_oom_handling))]
use core::iter::FromIterator;
use core::iter::{FusedIterator, Iterator};
use core::marker::{Unpin, Unsize};
use core::mem;
use core::ops::{
    CoerceUnsized, Deref, DerefMut, DispatchFromDyn, Generator, GeneratorState, Receiver,
};
use core::pin::Pin;
use core::ptr::{self, Unique};
use core::stream::Stream;
use core::task::{Context, Poll};

#[cfg(not(no_global_oom_handling))]
use crate::alloc::{handle_alloc_error, WriteCloneIntoRaw};
use crate::alloc::{AllocError, Allocator, Global, Layout};
#[cfg(not(no_global_oom_handling))]
use crate::borrow::Cow;
use crate::raw_vec::RawVec;
#[cfg(not(no_global_oom_handling))]
use crate::str::from_boxed_utf8_unchecked;
#[cfg(not(no_global_oom_handling))]
use crate::vec::Vec;

/// A pointer type for heap allocation.
///
/// See the [module-level documentation](../../std/boxed/index.html) for more.
#[lang = "owned_box"]
#[fundamental]
#[stable(feature = "rust1", since = "1.0.0")]
pub struct Box<
    T: ?Sized,
    #[unstable(feature = "allocator_api", issue = "32838")] A: Allocator = Global,
>(Unique<T>, A);

impl<T> Box<T> {
    /// Allocates memory on the heap and then places `x` into it.
    ///
    /// This doesn't actually allocate if `T` is zero-sized.
    ///
    /// # Examples
    ///
    /// ```
    /// let five = Box::new(5);
    /// ```
    #[cfg(not(no_global_oom_handling))]
    #[inline(always)]
    #[stable(feature = "rust1", since = "1.0.0")]
    pub fn new(x: T) -> Self {
}

    /// Constructs a new box with uninitialized contents.
    ///
    /// # Examples
    ///
    /// ```
    /// #![feature(new_uninit)]
    ///
    /// let mut five = Box::<u32>::new_uninit();
    ///
    /// let five = unsafe {
    ///     // Deferred initialization:
    ///     five.as_mut_ptr().write(5);
    ///
    ///     five.assume_init()
    /// };
    ///
    /// assert_eq!(*five, 5)
    /// ```
    #[cfg(not(no_global_oom_handling))]
    #[unstable(feature = "new_uninit", issue = "63291")]
    #[inline]
    pub fn new_uninit() -> Box<mem::MaybeUninit<T>> {
}

    /// Constructs a new `Box` with uninitialized contents, with the memory
    /// being filled with `0` bytes.
    ///
    /// See [`MaybeUninit::zeroed`][zeroed] for examples of correct and incorrect usage
    /// of this method.
    ///
    /// # Examples
    ///
    /// ```
    /// #![feature(new_uninit)]
    ///
    /// let zero = Box::<u32>::new_zeroed();
    /// let zero = unsafe { zero.assume_init() };
    ///
    /// assert_eq!(*zero, 0)
    /// ```
    ///
    /// [zeroed]: mem::MaybeUninit::zeroed
    #[cfg(not(no_global_oom_handling))]
    #[inline]
    #[unstable(feature = "new_uninit", issue = "63291")]
    pub fn new_zeroed() -> Box<mem::MaybeUninit<T>> {
}

    /// Constructs a new `Pin<Box<T>>`. If `T` does not implement `Unpin`, then
    /// `x` will be pinned in memory and unable to be moved.
    #[cfg(not(no_global_oom_handling))]
    #[stable(feature = "pin", since = "1.33.0")]
    #[inline(always)]
    pub fn pin(x: T) -> Pin<Box<T>> {
}

    /// Allocates memory on the heap then places `x` into it,
    /// returning an error if the allocation fails
    ///
    /// This doesn't actually allocate if `T` is zero-sized.
    ///
    /// # Examples
    ///
    /// ```
    /// #![feature(allocator_api)]
    ///
    /// let five = Box::try_new(5)?;
    /// # Ok::<(), std::alloc::AllocError>(())
    /// ```
    #[unstable(feature = "allocator_api", issue = "32838")]
    #[inline]
    pub fn try_new(x: T) -> Result<Self, AllocError> {
}

    /// Constructs a new box with uninitialized contents on the heap,
    /// returning an error if the allocation fails
    ///
    /// # Examples
    ///
    /// ```
    /// #![feature(allocator_api, new_uninit)]
    ///
    /// let mut five = Box::<u32>::try_new_uninit()?;
    ///
    /// let five = unsafe {
    ///     // Deferred initialization:
    ///     five.as_mut_ptr().write(5);
    ///
    ///     five.assume_init()
    /// };
    ///
    /// assert_eq!(*five, 5);
    /// # Ok::<(), std::alloc::AllocError>(())
    /// ```
    #[unstable(feature = "allocator_api", issue = "32838")]
    // #[unstable(feature = "new_uninit", issue = "63291")]
    #[inline]
    pub fn try_new_uninit() -> Result<Box<mem::MaybeUninit<T>>, AllocError> {
}

    /// Constructs a new `Box` with uninitialized contents, with the memory
    /// being filled with `0` bytes on the heap
    ///
    /// See [`MaybeUninit::zeroed`][zeroed] for examples of correct and incorrect usage
    /// of this method.
    ///
    /// # Examples
    ///
    /// ```
    /// #![feature(allocator_api, new_uninit)]
    ///
    /// let zero = Box::<u32>::try_new_zeroed()?;
    /// let zero = unsafe { zero.assume_init() };
    ///
    /// assert_eq!(*zero, 0);
    /// # Ok::<(), std::alloc::AllocError>(())
    /// ```
    ///
    /// [zeroed]: mem::MaybeUninit::zeroed
    #[unstable(feature = "allocator_api", issue = "32838")]
    // #[unstable(feature = "new_uninit", issue = "63291")]
    #[inline]
    pub fn try_new_zeroed() -> Result<Box<mem::MaybeUninit<T>>, AllocError> {
}
}

impl<T, A: Allocator> Box<T, A> {
    /// Allocates memory in the given allocator then places `x` into it.
    ///
    /// This doesn't actually allocate if `T` is zero-sized.
    ///
    /// # Examples
    ///
    /// ```
    /// #![feature(allocator_api)]
    ///
    /// use std::alloc::System;
    ///
    /// let five = Box::new_in(5, System);
    /// ```
    #[cfg(not(no_global_oom_handling))]
    #[unstable(feature = "allocator_api", issue = "32838")]
    #[inline]
    pub fn new_in(x: T, alloc: A) -> Self {
}

    /// Allocates memory in the given allocator then places `x` into it,
    /// returning an error if the allocation fails
    ///
    /// This doesn't actually allocate if `T` is zero-sized.
    ///
    /// # Examples
    ///
    /// ```
    /// #![feature(allocator_api)]
    ///
    /// use std::alloc::System;
    ///
    /// let five = Box::try_new_in(5, System)?;
    /// # Ok::<(), std::alloc::AllocError>(())
    /// ```
    #[unstable(feature = "allocator_api", issue = "32838")]
    #[inline]
    pub fn try_new_in(x: T, alloc: A) -> Result<Self, AllocError> {
}

    /// Constructs a new box with uninitialized contents in the provided allocator.
    ///
    /// # Examples
    ///
    /// ```
    /// #![feature(allocator_api, new_uninit)]
    ///
    /// use std::alloc::System;
    ///
    /// let mut five = Box::<u32, _>::new_uninit_in(System);
    ///
    /// let five = unsafe {
    ///     // Deferred initialization:
    ///     five.as_mut_ptr().write(5);
    ///
    ///     five.assume_init()
    /// };
    ///
    /// assert_eq!(*five, 5)
    /// ```
    #[unstable(feature = "allocator_api", issue = "32838")]
    #[cfg(not(no_global_oom_handling))]
    // #[unstable(feature = "new_uninit", issue = "63291")]
    pub fn new_uninit_in(alloc: A) -> Box<mem::MaybeUninit<T>, A> {
}

    /// Constructs a new box with uninitialized contents in the provided allocator,
    /// returning an error if the allocation fails
    ///
    /// # Examples
    ///
    /// ```
    /// #![feature(allocator_api, new_uninit)]
    ///
    /// use std::alloc::System;
    ///
    /// let mut five = Box::<u32, _>::try_new_uninit_in(System)?;
    ///
    /// let five = unsafe {
    ///     // Deferred initialization:
    ///     five.as_mut_ptr().write(5);
    ///
    ///     five.assume_init()
    /// };
    ///
    /// assert_eq!(*five, 5);
    /// # Ok::<(), std::alloc::AllocError>(())
    /// ```
    #[unstable(feature = "allocator_api", issue = "32838")]
    // #[unstable(feature = "new_uninit", issue = "63291")]
    pub fn try_new_uninit_in(alloc: A) -> Result<Box<mem::MaybeUninit<T>, A>, AllocError> {
}

    /// Constructs a new `Box` with uninitialized contents, with the memory
    /// being filled with `0` bytes in the provided allocator.
    ///
    /// See [`MaybeUninit::zeroed`][zeroed] for examples of correct and incorrect usage
    /// of this method.
    ///
    /// # Examples
    ///
    /// ```
    /// #![feature(allocator_api, new_uninit)]
    ///
    /// use std::alloc::System;
    ///
    /// let zero = Box::<u32, _>::new_zeroed_in(System);
    /// let zero = unsafe { zero.assume_init() };
    ///
    /// assert_eq!(*zero, 0)
    /// ```
    ///
    /// [zeroed]: mem::MaybeUninit::zeroed
    #[unstable(feature = "allocator_api", issue = "32838")]
    #[cfg(not(no_global_oom_handling))]
    // #[unstable(feature = "new_uninit", issue = "63291")]
    pub fn new_zeroed_in(alloc: A) -> Box<mem::MaybeUninit<T>, A> {
}

    /// Constructs a new `Box` with uninitialized contents, with the memory
    /// being filled with `0` bytes in the provided allocator,
    /// returning an error if the allocation fails,
    ///
    /// See [`MaybeUninit::zeroed`][zeroed] for examples of correct and incorrect usage
    /// of this method.
    ///
    /// # Examples
    ///
    /// ```
    /// #![feature(allocator_api, new_uninit)]
    ///
    /// use std::alloc::System;
    ///
    /// let zero = Box::<u32, _>::try_new_zeroed_in(System)?;
    /// let zero = unsafe { zero.assume_init() };
    ///
    /// assert_eq!(*zero, 0);
    /// # Ok::<(), std::alloc::AllocError>(())
    /// ```
    ///
    /// [zeroed]: mem::MaybeUninit::zeroed
    #[unstable(feature = "allocator_api", issue = "32838")]
    // #[unstable(feature = "new_uninit", issue = "63291")]
    pub fn try_new_zeroed_in(alloc: A) -> Result<Box<mem::MaybeUninit<T>, A>, AllocError> {
}

    /// Constructs a new `Pin<Box<T, A>>`. If `T` does not implement `Unpin`, then
    /// `x` will be pinned in memory and unable to be moved.
    #[cfg(not(no_global_oom_handling))]
    #[unstable(feature = "allocator_api", issue = "32838")]
    #[inline(always)]
    pub fn pin_in(x: T, alloc: A) -> Pin<Self>
    where
        A: 'static,
    {
}

    /// Converts a `Box<T>` into a `Box<[T]>`
    ///
    /// This conversion does not allocate on the heap and happens in place.
    #[unstable(feature = "box_into_boxed_slice", issue = "71582")]
    pub fn into_boxed_slice(boxed: Self) -> Box<[T], A> {
}

    /// Consumes the `Box`, returning the wrapped value.
    ///
    /// # Examples
    ///
    /// ```
    /// #![feature(box_into_inner)]
    ///
    /// let c = Box::new(5);
    ///
    /// assert_eq!(Box::into_inner(c), 5);
    /// ```
    #[unstable(feature = "box_into_inner", issue = "80437")]
    #[inline]
    pub fn into_inner(boxed: Self) -> T {
}
}

impl<T> Box<[T]> {
    /// Constructs a new boxed slice with uninitialized contents.
    ///
    /// # Examples
    ///
    /// ```
    /// #![feature(new_uninit)]
    ///
    /// let mut values = Box::<[u32]>::new_uninit_slice(3);
    ///
    /// let values = unsafe {
    ///     // Deferred initialization:
    ///     values[0].as_mut_ptr().write(1);
    ///     values[1].as_mut_ptr().write(2);
    ///     values[2].as_mut_ptr().write(3);
    ///
    ///     values.assume_init()
    /// };
    ///
    /// assert_eq!(*values, [1, 2, 3])
    /// ```
    #[cfg(not(no_global_oom_handling))]
    #[unstable(feature = "new_uninit", issue = "63291")]
    pub fn new_uninit_slice(len: usize) -> Box<[mem::MaybeUninit<T>]> {
}

    /// Constructs a new boxed slice with uninitialized contents, with the memory
    /// being filled with `0` bytes.
    ///
    /// See [`MaybeUninit::zeroed`][zeroed] for examples of correct and incorrect usage
    /// of this method.
    ///
    /// # Examples
    ///
    /// ```
    /// #![feature(new_uninit)]
    ///
    /// let values = Box::<[u32]>::new_zeroed_slice(3);
    /// let values = unsafe { values.assume_init() };
    ///
    /// assert_eq!(*values, [0, 0, 0])
    /// ```
    ///
    /// [zeroed]: mem::MaybeUninit::zeroed
    #[cfg(not(no_global_oom_handling))]
    #[unstable(feature = "new_uninit", issue = "63291")]
    pub fn new_zeroed_slice(len: usize) -> Box<[mem::MaybeUninit<T>]> {
}

    /// Constructs a new boxed slice with uninitialized contents. Returns an error if
    /// the allocation fails
    ///
    /// # Examples
    ///
    /// ```
    /// #![feature(allocator_api, new_uninit)]
    ///
    /// let mut values = Box::<[u32]>::try_new_uninit_slice(3)?;
    /// let values = unsafe {
    ///     // Deferred initialization:
    ///     values[0].as_mut_ptr().write(1);
    ///     values[1].as_mut_ptr().write(2);
    ///     values[2].as_mut_ptr().write(3);
    ///     values.assume_init()
    /// };
    ///
    /// assert_eq!(*values, [1, 2, 3]);
    /// # Ok::<(), std::alloc::AllocError>(())
    /// ```
    #[unstable(feature = "allocator_api", issue = "32838")]
    #[inline]
    pub fn try_new_uninit_slice(len: usize) -> Result<Box<[mem::MaybeUninit<T>]>, AllocError> {
}

    /// Constructs a new boxed slice with uninitialized contents, with the memory
    /// being filled with `0` bytes. Returns an error if the allocation fails
    ///
    /// See [`MaybeUninit::zeroed`][zeroed] for examples of correct and incorrect usage
    /// of this method.
    ///
    /// # Examples
    ///
    /// ```
    /// #![feature(allocator_api, new_uninit)]
    ///
    /// let values = Box::<[u32]>::try_new_zeroed_slice(3)?;
    /// let values = unsafe { values.assume_init() };
    ///
    /// assert_eq!(*values, [0, 0, 0]);
    /// # Ok::<(), std::alloc::AllocError>(())
    /// ```
    ///
    /// [zeroed]: mem::MaybeUninit::zeroed
    #[unstable(feature = "allocator_api", issue = "32838")]
    #[inline]
    pub fn try_new_zeroed_slice(len: usize) -> Result<Box<[mem::MaybeUninit<T>]>, AllocError> {
}
}

impl<T, A: Allocator> Box<[T], A> {
    /// Constructs a new boxed slice with uninitialized contents in the provided allocator.
    ///
    /// # Examples
    ///
    /// ```
    /// #![feature(allocator_api, new_uninit)]
    ///
    /// use std::alloc::System;
    ///
    /// let mut values = Box::<[u32], _>::new_uninit_slice_in(3, System);
    ///
    /// let values = unsafe {
    ///     // Deferred initialization:
    ///     values[0].as_mut_ptr().write(1);
    ///     values[1].as_mut_ptr().write(2);
    ///     values[2].as_mut_ptr().write(3);
    ///
    ///     values.assume_init()
    /// };
    ///
    /// assert_eq!(*values, [1, 2, 3])
    /// ```
    #[cfg(not(no_global_oom_handling))]
    #[unstable(feature = "allocator_api", issue = "32838")]
    // #[unstable(feature = "new_uninit", issue = "63291")]
    pub fn new_uninit_slice_in(len: usize, alloc: A) -> Box<[mem::MaybeUninit<T>], A> {
}

    /// Constructs a new boxed slice with uninitialized contents in the provided allocator,
    /// with the memory being filled with `0` bytes.
    ///
    /// See [`MaybeUninit::zeroed`][zeroed] for examples of correct and incorrect usage
    /// of this method.
    ///
    /// # Examples
    ///
    /// ```
    /// #![feature(allocator_api, new_uninit)]
    ///
    /// use std::alloc::System;
    ///
    /// let values = Box::<[u32], _>::new_zeroed_slice_in(3, System);
    /// let values = unsafe { values.assume_init() };
    ///
    /// assert_eq!(*values, [0, 0, 0])
    /// ```
    ///
    /// [zeroed]: mem::MaybeUninit::zeroed
    #[cfg(not(no_global_oom_handling))]
    #[unstable(feature = "allocator_api", issue = "32838")]
    // #[unstable(feature = "new_uninit", issue = "63291")]
    pub fn new_zeroed_slice_in(len: usize, alloc: A) -> Box<[mem::MaybeUninit<T>], A> {
}
}

impl<T, A: Allocator> Box<mem::MaybeUninit<T>, A> {
    /// Converts to `Box<T, A>`.
    ///
    /// # Safety
    ///
    /// As with [`MaybeUninit::assume_init`],
    /// it is up to the caller to guarantee that the value
    /// really is in an initialized state.
    /// Calling this when the content is not yet fully initialized
    /// causes immediate undefined behavior.
    ///
    /// [`MaybeUninit::assume_init`]: mem::MaybeUninit::assume_init
    ///
    /// # Examples
    ///
    /// ```
    /// #![feature(new_uninit)]
    ///
    /// let mut five = Box::<u32>::new_uninit();
    ///
    /// let five: Box<u32> = unsafe {
    ///     // Deferred initialization:
    ///     five.as_mut_ptr().write(5);
    ///
    ///     five.assume_init()
    /// };
    ///
    /// assert_eq!(*five, 5)
    /// ```
    #[unstable(feature = "new_uninit", issue = "63291")]
    #[inline]
    pub unsafe fn assume_init(self) -> Box<T, A> {
}
}

impl<T, A: Allocator> Box<[mem::MaybeUninit<T>], A> {
    /// Converts to `Box<[T], A>`.
    ///
    /// # Safety
    ///
    /// As with [`MaybeUninit::assume_init`],
    /// it is up to the caller to guarantee that the values
    /// really are in an initialized state.
    /// Calling this when the content is not yet fully initialized
    /// causes immediate undefined behavior.
    ///
    /// [`MaybeUninit::assume_init`]: mem::MaybeUninit::assume_init
    ///
    /// # Examples
    ///
    /// ```
    /// #![feature(new_uninit)]
    ///
    /// let mut values = Box::<[u32]>::new_uninit_slice(3);
    ///
    /// let values = unsafe {
    ///     // Deferred initialization:
    ///     values[0].as_mut_ptr().write(1);
    ///     values[1].as_mut_ptr().write(2);
    ///     values[2].as_mut_ptr().write(3);
    ///
    ///     values.assume_init()
    /// };
    ///
    /// assert_eq!(*values, [1, 2, 3])
    /// ```
    #[unstable(feature = "new_uninit", issue = "63291")]
    #[inline]
    pub unsafe fn assume_init(self) -> Box<[T], A> {
}
}

impl<T: ?Sized> Box<T> {
    /// Constructs a box from a raw pointer.
    ///
    /// After calling this function, the raw pointer is owned by the
    /// resulting `Box`. Specifically, the `Box` destructor will call
    /// the destructor of `T` and free the allocated memory. For this
    /// to be safe, the memory must have been allocated in accordance
    /// with the [memory layout] used by `Box` .
    ///
    /// # Safety
    ///
    /// This function is unsafe because improper use may lead to
    /// memory problems. For example, a double-free may occur if the
    /// function is called twice on the same raw pointer.
    ///
    /// The safety conditions are described in the [memory layout] section.
    ///
    /// # Examples
    ///
    /// Recreate a `Box` which was previously converted to a raw pointer
    /// using [`Box::into_raw`]:
    /// ```
    /// let x = Box::new(5);
    /// let ptr = Box::into_raw(x);
    /// let x = unsafe { Box::from_raw(ptr) };
    /// ```
    /// Manually create a `Box` from scratch by using the global allocator:
    /// ```
    /// use std::alloc::{alloc, Layout};
    ///
    /// unsafe {
    ///     let ptr = alloc(Layout::new::<i32>()) as *mut i32;
    ///     // In general .write is required to avoid attempting to destruct
    ///     // the (uninitialized) previous contents of `ptr`, though for this
    ///     // simple example `*ptr = 5` would have worked as well.
    ///     ptr.write(5);
    ///     let x = Box::from_raw(ptr);
    /// }
    /// ```
    ///
    /// [memory layout]: self#memory-layout
    /// [`Layout`]: crate::Layout
    #[stable(feature = "box_raw", since = "1.4.0")]
    #[inline]
    pub unsafe fn from_raw(raw: *mut T) -> Self {
}
}

impl<T: ?Sized, A: Allocator> Box<T, A> {
    /// Constructs a box from a raw pointer in the given allocator.
    ///
    /// After calling this function, the raw pointer is owned by the
    /// resulting `Box`. Specifically, the `Box` destructor will call
    /// the destructor of `T` and free the allocated memory. For this
    /// to be safe, the memory must have been allocated in accordance
    /// with the [memory layout] used by `Box` .
    ///
    /// # Safety
    ///
    /// This function is unsafe because improper use may lead to
    /// memory problems. For example, a double-free may occur if the
    /// function is called twice on the same raw pointer.
    ///
    ///
    /// # Examples
    ///
    /// Recreate a `Box` which was previously converted to a raw pointer
    /// using [`Box::into_raw_with_allocator`]:
    /// ```
    /// #![feature(allocator_api)]
    ///
    /// use std::alloc::System;
    ///
    /// let x = Box::new_in(5, System);
    /// let (ptr, alloc) = Box::into_raw_with_allocator(x);
    /// let x = unsafe { Box::from_raw_in(ptr, alloc) };
    /// ```
    /// Manually create a `Box` from scratch by using the system allocator:
    /// ```
    /// #![feature(allocator_api, slice_ptr_get)]
    ///
    /// use std::alloc::{Allocator, Layout, System};
    ///
    /// unsafe {
    ///     let ptr = System.allocate(Layout::new::<i32>())?.as_mut_ptr() as *mut i32;
    ///     // In general .write is required to avoid attempting to destruct
    ///     // the (uninitialized) previous contents of `ptr`, though for this
    ///     // simple example `*ptr = 5` would have worked as well.
    ///     ptr.write(5);
    ///     let x = Box::from_raw_in(ptr, System);
    /// }
    /// # Ok::<(), std::alloc::AllocError>(())
    /// ```
    ///
    /// [memory layout]: self#memory-layout
    /// [`Layout`]: crate::Layout
    #[unstable(feature = "allocator_api", issue = "32838")]
    #[inline]
    pub unsafe fn from_raw_in(raw: *mut T, alloc: A) -> Self {
}

    /// Consumes the `Box`, returning a wrapped raw pointer.
    ///
    /// The pointer will be properly aligned and non-null.
    ///
    /// After calling this function, the caller is responsible for the
    /// memory previously managed by the `Box`. In particular, the
    /// caller should properly destroy `T` and release the memory, taking
    /// into account the [memory layout] used by `Box`. The easiest way to
    /// do this is to convert the raw pointer back into a `Box` with the
    /// [`Box::from_raw`] function, allowing the `Box` destructor to perform
    /// the cleanup.
    ///
    /// Note: this is an associated function, which means that you have
    /// to call it as `Box::into_raw(b)` instead of `b.into_raw()`. This
    /// is so that there is no conflict with a method on the inner type.
    ///
    /// # Examples
    /// Converting the raw pointer back into a `Box` with [`Box::from_raw`]
    /// for automatic cleanup:
    /// ```
    /// let x = Box::new(String::from("Hello"));
    /// let ptr = Box::into_raw(x);
    /// let x = unsafe { Box::from_raw(ptr) };
    /// ```
    /// Manual cleanup by explicitly running the destructor and deallocating
    /// the memory:
    /// ```
    /// use std::alloc::{dealloc, Layout};
    /// use std::ptr;
    ///
    /// let x = Box::new(String::from("Hello"));
    /// let p = Box::into_raw(x);
    /// unsafe {
    ///     ptr::drop_in_place(p);
    ///     dealloc(p as *mut u8, Layout::new::<String>());
    /// }
    /// ```
    ///
    /// [memory layout]: self#memory-layout
    #[stable(feature = "box_raw", since = "1.4.0")]
    #[inline]
    pub fn into_raw(b: Self) -> *mut T {
}

    /// Consumes the `Box`, returning a wrapped raw pointer and the allocator.
    ///
    /// The pointer will be properly aligned and non-null.
    ///
    /// After calling this function, the caller is responsible for the
    /// memory previously managed by the `Box`. In particular, the
    /// caller should properly destroy `T` and release the memory, taking
    /// into account the [memory layout] used by `Box`. The easiest way to
    /// do this is to convert the raw pointer back into a `Box` with the
    /// [`Box::from_raw_in`] function, allowing the `Box` destructor to perform
    /// the cleanup.
    ///
    /// Note: this is an associated function, which means that you have
    /// to call it as `Box::into_raw_with_allocator(b)` instead of `b.into_raw_with_allocator()`. This
    /// is so that there is no conflict with a method on the inner type.
    ///
    /// # Examples
    /// Converting the raw pointer back into a `Box` with [`Box::from_raw_in`]
    /// for automatic cleanup:
    /// ```
    /// #![feature(allocator_api)]
    ///
    /// use std::alloc::System;
    ///
    /// let x = Box::new_in(String::from("Hello"), System);
    /// let (ptr, alloc) = Box::into_raw_with_allocator(x);
    /// let x = unsafe { Box::from_raw_in(ptr, alloc) };
    /// ```
    /// Manual cleanup by explicitly running the destructor and deallocating
    /// the memory:
    /// ```
    /// #![feature(allocator_api)]
    ///
    /// use std::alloc::{Allocator, Layout, System};
    /// use std::ptr::{self, NonNull};
    ///
    /// let x = Box::new_in(String::from("Hello"), System);
    /// let (ptr, alloc) = Box::into_raw_with_allocator(x);
    /// unsafe {
    ///     ptr::drop_in_place(ptr);
    ///     let non_null = NonNull::new_unchecked(ptr);
    ///     alloc.deallocate(non_null.cast(), Layout::new::<String>());
    /// }
    /// ```
    ///
    /// [memory layout]: self#memory-layout
    #[unstable(feature = "allocator_api", issue = "32838")]
    #[inline]
    pub fn into_raw_with_allocator(b: Self) -> (*mut T, A) {
}

    #[unstable(
        feature = "ptr_internals",
        issue = "none",
        reason = "use `Box::leak(b).into()` or `Unique::from(Box::leak(b))` instead"
    )]
    #[inline]
    #[doc(hidden)]
    pub fn into_unique(b: Self) -> (Unique<T>, A) {
}

    /// Returns a reference to the underlying allocator.
    ///
    /// Note: this is an associated function, which means that you have
    /// to call it as `Box::allocator(&b)` instead of `b.allocator()`. This
    /// is so that there is no conflict with a method on the inner type.
    #[unstable(feature = "allocator_api", issue = "32838")]
    #[inline]
    pub fn allocator(b: &Self) -> &A {
}

    /// Consumes and leaks the `Box`, returning a mutable reference,
    /// `&'a mut T`. Note that the type `T` must outlive the chosen lifetime
    /// `'a`. If the type has only static references, or none at all, then this
    /// may be chosen to be `'static`.
    ///
    /// This function is mainly useful for data that lives for the remainder of
    /// the program's life. Dropping the returned reference will cause a memory
    /// leak. If this is not acceptable, the reference should first be wrapped
    /// with the [`Box::from_raw`] function producing a `Box`. This `Box` can
    /// then be dropped which will properly destroy `T` and release the
    /// allocated memory.
    ///
    /// Note: this is an associated function, which means that you have
    /// to call it as `Box::leak(b)` instead of `b.leak()`. This
    /// is so that there is no conflict with a method on the inner type.
    ///
    /// # Examples
    ///
    /// Simple usage:
    ///
    /// ```
    /// let x = Box::new(41);
    /// let static_ref: &'static mut usize = Box::leak(x);
    /// *static_ref += 1;
    /// assert_eq!(*static_ref, 42);
    /// ```
    ///
    /// Unsized data:
    ///
    /// ```
    /// let x = vec![1, 2, 3].into_boxed_slice();
    /// let static_ref = Box::leak(x);
    /// static_ref[0] = 4;
    /// assert_eq!(*static_ref, [4, 2, 3]);
    /// ```
    #[stable(feature = "box_leak", since = "1.26.0")]
    #[inline]
    pub fn leak<'a>(b: Self) -> &'a mut T
    where
        A: 'a,
    {
}

    /// Converts a `Box<T>` into a `Pin<Box<T>>`
    ///
    /// This conversion does not allocate on the heap and happens in place.
    ///
    /// This is also available via [`From`].
    #[unstable(feature = "box_into_pin", issue = "62370")]
    pub fn into_pin(boxed: Self) -> Pin<Self>
    where
        A: 'static,
    {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
unsafe impl<#[may_dangle] T: ?Sized, A: Allocator> Drop for Box<T, A> {
    fn drop(&mut self) {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<T: Default> Default for Box<T> {
    /// Creates a `Box<T>`, with the `Default` value for T.
    fn default() -> Self {
}
}

#[cfg(not(no_global_oom_handling))]
#[stable(feature = "rust1", since = "1.0.0")]
impl<T> Default for Box<[T]> {
    fn default() -> Self {
}
}

#[cfg(not(no_global_oom_handling))]
#[stable(feature = "default_box_extra", since = "1.17.0")]
impl Default for Box<str> {
    fn default() -> Self {
}
}

#[cfg(not(no_global_oom_handling))]
#[stable(feature = "rust1", since = "1.0.0")]
impl<T: Clone, A: Allocator + Clone> Clone for Box<T, A> {
    /// Returns a new box with a `clone()` of this box's contents.
    ///
    /// # Examples
    ///
    /// ```
    /// let x = Box::new(5);
    /// let y = x.clone();
    ///
    /// // The value is the same
    /// assert_eq!(x, y);
    ///
    /// // But they are unique objects
    /// assert_ne!(&*x as *const i32, &*y as *const i32);
    /// ```
    #[inline]
    fn clone(&self) -> Self {
}

    /// Copies `source`'s contents into `self` without creating a new allocation.
    ///
    /// # Examples
    ///
    /// ```
    /// let x = Box::new(5);
    /// let mut y = Box::new(10);
    /// let yp: *const i32 = &*y;
    ///
    /// y.clone_from(&x);
    ///
    /// // The value is the same
    /// assert_eq!(x, y);
    ///
    /// // And no allocation occurred
    /// assert_eq!(yp, &*y);
    /// ```
    #[inline]
    fn clone_from(&mut self, source: &Self) {
}
}

#[cfg(not(no_global_oom_handling))]
#[stable(feature = "box_slice_clone", since = "1.3.0")]
impl Clone for Box<str> {
    fn clone(&self) -> Self {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<T: ?Sized + PartialEq, A: Allocator> PartialEq for Box<T, A> {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
}
    #[inline]
    fn ne(&self, other: &Self) -> bool {
}
}
#[stable(feature = "rust1", since = "1.0.0")]
impl<T: ?Sized + PartialOrd, A: Allocator> PartialOrd for Box<T, A> {
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
}
    #[inline]
    fn lt(&self, other: &Self) -> bool {
}
    #[inline]
    fn le(&self, other: &Self) -> bool {
}
    #[inline]
    fn ge(&self, other: &Self) -> bool {
}
    #[inline]
    fn gt(&self, other: &Self) -> bool {
}
}
#[stable(feature = "rust1", since = "1.0.0")]
impl<T: ?Sized + Ord, A: Allocator> Ord for Box<T, A> {
    #[inline]
    fn cmp(&self, other: &Self) -> Ordering {
}
}
#[stable(feature = "rust1", since = "1.0.0")]
impl<T: ?Sized + Eq, A: Allocator> Eq for Box<T, A> {}

#[stable(feature = "rust1", since = "1.0.0")]
impl<T: ?Sized + Hash, A: Allocator> Hash for Box<T, A> {
    fn hash<H: Hasher>(&self, state: &mut H) {
}
}

#[stable(feature = "indirect_hasher_impl", since = "1.22.0")]
impl<T: ?Sized + Hasher, A: Allocator> Hasher for Box<T, A> {
    fn finish(&self) -> u64 {
}
    fn write(&mut self, bytes: &[u8]) {
}
    fn write_u8(&mut self, i: u8) {
}
    fn write_u16(&mut self, i: u16) {
}
    fn write_u32(&mut self, i: u32) {
}
    fn write_u64(&mut self, i: u64) {
}
    fn write_u128(&mut self, i: u128) {
}
    fn write_usize(&mut self, i: usize) {
}
    fn write_i8(&mut self, i: i8) {
}
    fn write_i16(&mut self, i: i16) {
}
    fn write_i32(&mut self, i: i32) {
}
    fn write_i64(&mut self, i: i64) {
}
    fn write_i128(&mut self, i: i128) {
}
    fn write_isize(&mut self, i: isize) {
}
}

#[cfg(not(no_global_oom_handling))]
#[stable(feature = "from_for_ptrs", since = "1.6.0")]
impl<T> From<T> for Box<T> {
    /// Converts a `T` into a `Box<T>`
    ///
    /// The conversion allocates on the heap and moves `t`
    /// from the stack into it.
    ///
    /// # Examples
    /// ```rust
    /// let x = 5;
    /// let boxed = Box::new(5);
    ///
    /// assert_eq!(Box::from(x), boxed);
    /// ```
    fn from(t: T) -> Self {
}
}

#[stable(feature = "pin", since = "1.33.0")]
impl<T: ?Sized, A: Allocator> From<Box<T, A>> for Pin<Box<T, A>>
where
    A: 'static,
{
    /// Converts a `Box<T>` into a `Pin<Box<T>>`
    ///
    /// This conversion does not allocate on the heap and happens in place.
    fn from(boxed: Box<T, A>) -> Self {
}
}

#[cfg(not(no_global_oom_handling))]
#[stable(feature = "box_from_slice", since = "1.17.0")]
impl<T: Copy> From<&[T]> for Box<[T]> {
    /// Converts a `&[T]` into a `Box<[T]>`
    ///
    /// This conversion allocates on the heap
    /// and performs a copy of `slice`.
    ///
    /// # Examples
    /// ```rust
    /// // create a &[u8] which will be used to create a Box<[u8]>
    /// let slice: &[u8] = &[104, 101, 108, 108, 111];
    /// let boxed_slice: Box<[u8]> = Box::from(slice);
    ///
    /// println!("{:?}", boxed_slice);
    /// ```
    fn from(slice: &[T]) -> Box<[T]> {
}
}

#[cfg(not(no_global_oom_handling))]
#[stable(feature = "box_from_cow", since = "1.45.0")]
impl<T: Copy> From<Cow<'_, [T]>> for Box<[T]> {
    #[inline]
    fn from(cow: Cow<'_, [T]>) -> Box<[T]> {
}
}

#[cfg(not(no_global_oom_handling))]
#[stable(feature = "box_from_slice", since = "1.17.0")]
impl From<&str> for Box<str> {
    /// Converts a `&str` into a `Box<str>`
    ///
    /// This conversion allocates on the heap
    /// and performs a copy of `s`.
    ///
    /// # Examples
    /// ```rust
    /// let boxed: Box<str> = Box::from("hello");
    /// println!("{}", boxed);
    /// ```
    #[inline]
    fn from(s: &str) -> Box<str> {
}
}

#[cfg(not(no_global_oom_handling))]
#[stable(feature = "box_from_cow", since = "1.45.0")]
impl From<Cow<'_, str>> for Box<str> {
    #[inline]
    fn from(cow: Cow<'_, str>) -> Box<str> {
}
}

#[stable(feature = "boxed_str_conv", since = "1.19.0")]
impl<A: Allocator> From<Box<str, A>> for Box<[u8], A> {
    /// Converts a `Box<str>` into a `Box<[u8]>`
    ///
    /// This conversion does not allocate on the heap and happens in place.
    ///
    /// # Examples
    /// ```rust
    /// // create a Box<str> which will be used to create a Box<[u8]>
    /// let boxed: Box<str> = Box::from("hello");
    /// let boxed_str: Box<[u8]> = Box::from(boxed);
    ///
    /// // create a &[u8] which will be used to create a Box<[u8]>
    /// let slice: &[u8] = &[104, 101, 108, 108, 111];
    /// let boxed_slice = Box::from(slice);
    ///
    /// assert_eq!(boxed_slice, boxed_str);
    /// ```
    #[inline]
    fn from(s: Box<str, A>) -> Self {
}
}

#[stable(feature = "box_from_array", since = "1.45.0")]
impl<T, const N: usize> From<[T; N]> for Box<[T]> {
    /// Converts a `[T; N]` into a `Box<[T]>`
    ///
    /// This conversion moves the array to newly heap-allocated memory.
    ///
    /// # Examples
    /// ```rust
    /// let boxed: Box<[u8]> = Box::from([4, 2]);
    /// println!("{:?}", boxed);
    /// ```
    fn from(array: [T; N]) -> Box<[T]> {
}
}

#[stable(feature = "boxed_slice_try_from", since = "1.43.0")]
impl<T, const N: usize> TryFrom<Box<[T]>> for Box<[T; N]> {
    type Error = Box<[T]>;

    fn try_from(boxed_slice: Box<[T]>) -> Result<Self, Self::Error> {
}
}

impl<A: Allocator> Box<dyn Any, A> {
    #[inline]
    #[stable(feature = "rust1", since = "1.0.0")]
    /// Attempt to downcast the box to a concrete type.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::any::Any;
    ///
    /// fn print_if_string(value: Box<dyn Any>) {
    ///     if let Ok(string) = value.downcast::<String>() {
    ///         println!("String ({}): {}", string.len(), string);
    ///     }
    /// }
    ///
    /// let my_string = "Hello World".to_string();
    /// print_if_string(Box::new(my_string));
    /// print_if_string(Box::new(0i8));
    /// ```
    pub fn downcast<T: Any>(self) -> Result<Box<T, A>, Self> {
}
}

impl<A: Allocator> Box<dyn Any + Send, A> {
    #[inline]
    #[stable(feature = "rust1", since = "1.0.0")]
    /// Attempt to downcast the box to a concrete type.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::any::Any;
    ///
    /// fn print_if_string(value: Box<dyn Any + Send>) {
    ///     if let Ok(string) = value.downcast::<String>() {
    ///         println!("String ({}): {}", string.len(), string);
    ///     }
    /// }
    ///
    /// let my_string = "Hello World".to_string();
    /// print_if_string(Box::new(my_string));
    /// print_if_string(Box::new(0i8));
    /// ```
    pub fn downcast<T: Any>(self) -> Result<Box<T, A>, Self> {
}
}

impl<A: Allocator> Box<dyn Any + Send + Sync, A> {
    #[inline]
    #[stable(feature = "box_send_sync_any_downcast", since = "1.51.0")]
    /// Attempt to downcast the box to a concrete type.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::any::Any;
    ///
    /// fn print_if_string(value: Box<dyn Any + Send + Sync>) {
    ///     if let Ok(string) = value.downcast::<String>() {
    ///         println!("String ({}): {}", string.len(), string);
    ///     }
    /// }
    ///
    /// let my_string = "Hello World".to_string();
    /// print_if_string(Box::new(my_string));
    /// print_if_string(Box::new(0i8));
    /// ```
    pub fn downcast<T: Any>(self) -> Result<Box<T, A>, Self> {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<T: fmt::Display + ?Sized, A: Allocator> fmt::Display for Box<T, A> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<T: fmt::Debug + ?Sized, A: Allocator> fmt::Debug for Box<T, A> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<T: ?Sized, A: Allocator> fmt::Pointer for Box<T, A> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<T: ?Sized, A: Allocator> Deref for Box<T, A> {
    type Target = T;

    fn deref(&self) -> &T {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<T: ?Sized, A: Allocator> DerefMut for Box<T, A> {
    fn deref_mut(&mut self) -> &mut T {
}
}

#[unstable(feature = "receiver_trait", issue = "none")]
impl<T: ?Sized, A: Allocator> Receiver for Box<T, A> {}

#[stable(feature = "rust1", since = "1.0.0")]
impl<I: Iterator + ?Sized, A: Allocator> Iterator for Box<I, A> {
    type Item = I::Item;
    fn next(&mut self) -> Option<I::Item> {
}
    fn size_hint(&self) -> (usize, Option<usize>) {
}
    fn nth(&mut self, n: usize) -> Option<I::Item> {
}
    fn last(self) -> Option<I::Item> {
}
}

trait BoxIter {
    type Item;
    fn last(self) -> Option<Self::Item>;
}

impl<I: Iterator + ?Sized, A: Allocator> BoxIter for Box<I, A> {
    type Item = I::Item;
    default fn last(self) -> Option<I::Item> {
}
}

/// Specialization for sized `I`s that uses `I`s implementation of `last()`
/// instead of the default.
#[stable(feature = "rust1", since = "1.0.0")]
impl<I: Iterator, A: Allocator> BoxIter for Box<I, A> {
    fn last(self) -> Option<I::Item> {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<I: DoubleEndedIterator + ?Sized, A: Allocator> DoubleEndedIterator for Box<I, A> {
    fn next_back(&mut self) -> Option<I::Item> {
}
    fn nth_back(&mut self, n: usize) -> Option<I::Item> {
}
}
#[stable(feature = "rust1", since = "1.0.0")]
impl<I: ExactSizeIterator + ?Sized, A: Allocator> ExactSizeIterator for Box<I, A> {
    fn len(&self) -> usize {
}
    fn is_empty(&self) -> bool {
}
}

#[stable(feature = "fused", since = "1.26.0")]
impl<I: FusedIterator + ?Sized, A: Allocator> FusedIterator for Box<I, A> {}

#[stable(feature = "boxed_closure_impls", since = "1.35.0")]
impl<Args, F: FnOnce<Args> + ?Sized, A: Allocator> FnOnce<Args> for Box<F, A> {
    type Output = <F as FnOnce<Args>>::Output;

    extern "rust-call" fn call_once(self, args: Args) -> Self::Output {
}
}

#[stable(feature = "boxed_closure_impls", since = "1.35.0")]
impl<Args, F: FnMut<Args> + ?Sized, A: Allocator> FnMut<Args> for Box<F, A> {
    extern "rust-call" fn call_mut(&mut self, args: Args) -> Self::Output {
}
}

#[stable(feature = "boxed_closure_impls", since = "1.35.0")]
impl<Args, F: Fn<Args> + ?Sized, A: Allocator> Fn<Args> for Box<F, A> {
    extern "rust-call" fn call(&self, args: Args) -> Self::Output {
}
}

#[unstable(feature = "coerce_unsized", issue = "27732")]
impl<T: ?Sized + Unsize<U>, U: ?Sized, A: Allocator> CoerceUnsized<Box<U, A>> for Box<T, A> {}

#[unstable(feature = "dispatch_from_dyn", issue = "none")]
impl<T: ?Sized + Unsize<U>, U: ?Sized> DispatchFromDyn<Box<U>> for Box<T, Global> {}

#[cfg(not(no_global_oom_handling))]
#[stable(feature = "boxed_slice_from_iter", since = "1.32.0")]
impl<I> FromIterator<I> for Box<[I]> {
    fn from_iter<T: IntoIterator<Item = I>>(iter: T) -> Self {
}
}

#[cfg(not(no_global_oom_handling))]
#[stable(feature = "box_slice_clone", since = "1.3.0")]
impl<T: Clone, A: Allocator + Clone> Clone for Box<[T], A> {
    fn clone(&self) -> Self {
}

    fn clone_from(&mut self, other: &Self) {
}
}

#[stable(feature = "box_borrow", since = "1.1.0")]
impl<T: ?Sized, A: Allocator> borrow::Borrow<T> for Box<T, A> {
    fn borrow(&self) -> &T {
}
}

#[stable(feature = "box_borrow", since = "1.1.0")]
impl<T: ?Sized, A: Allocator> borrow::BorrowMut<T> for Box<T, A> {
    fn borrow_mut(&mut self) -> &mut T {
}
}

#[stable(since = "1.5.0", feature = "smart_ptr_as_ref")]
impl<T: ?Sized, A: Allocator> AsRef<T> for Box<T, A> {
    fn as_ref(&self) -> &T {
}
}

#[stable(since = "1.5.0", feature = "smart_ptr_as_ref")]
impl<T: ?Sized, A: Allocator> AsMut<T> for Box<T, A> {
    fn as_mut(&mut self) -> &mut T {
}
}

/* Nota bene
 *
 *  We could have chosen not to add this impl, and instead have written a
 *  function of Pin<Box<T>> to Pin<T>. Such a function would not be sound,
 *  because Box<T> implements Unpin even when T does not, as a result of
 *  this impl.
 *
 *  We chose this API instead of the alternative for a few reasons:
 *      - Logically, it is helpful to understand pinning in regard to the
 *        memory region being pointed to. For this reason none of the
 *        standard library pointer types support projecting through a pin
 *        (Box<T> is the only pointer type in std for which this would be
 *        safe.)
 *      - It is in practice very useful to have Box<T> be unconditionally
 *        Unpin because of trait objects, for which the structural auto
 *        trait functionality does not apply (e.g., Box<dyn Foo> would
 *        otherwise not be Unpin).
 *
 *  Another type with the same semantics as Box but only a conditional
 *  implementation of `Unpin` (where `T: Unpin`) would be valid/safe, and
 *  could have a method to project a Pin<T> from it.
 */
#[stable(feature = "pin", since = "1.33.0")]
impl<T: ?Sized, A: Allocator> Unpin for Box<T, A> where A: 'static {}

#[unstable(feature = "generator_trait", issue = "43122")]
impl<G: ?Sized + Generator<R> + Unpin, R, A: Allocator> Generator<R> for Box<G, A>
where
    A: 'static,
{
    type Yield = G::Yield;
    type Return = G::Return;

    fn resume(mut self: Pin<&mut Self>, arg: R) -> GeneratorState<Self::Yield, Self::Return> {
}
}

#[unstable(feature = "generator_trait", issue = "43122")]
impl<G: ?Sized + Generator<R>, R, A: Allocator> Generator<R> for Pin<Box<G, A>>
where
    A: 'static,
{
    type Yield = G::Yield;
    type Return = G::Return;

    fn resume(mut self: Pin<&mut Self>, arg: R) -> GeneratorState<Self::Yield, Self::Return> {
}
}

#[stable(feature = "futures_api", since = "1.36.0")]
impl<F: ?Sized + Future + Unpin, A: Allocator> Future for Box<F, A>
where
    A: 'static,
{
    type Output = F::Output;

    fn poll(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
}
}

#[unstable(feature = "async_stream", issue = "79024")]
impl<S: ?Sized + Stream + Unpin> Stream for Box<S> {
    type Item = S::Item;

    fn poll_next(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Option<Self::Item>> {
}

    fn size_hint(&self) -> (usize, Option<usize>) {
}
}
}
#[cfg(test)]
mod boxed {
}
pub mod borrow {
//! A module for working with borrowed data.

#![stable(feature = "rust1", since = "1.0.0")]

use core::cmp::Ordering;
use core::hash::{Hash, Hasher};
use core::ops::Deref;
#[cfg(not(no_global_oom_handling))]
use core::ops::{Add, AddAssign};

#[stable(feature = "rust1", since = "1.0.0")]
pub use core::borrow::{Borrow, BorrowMut};

use crate::fmt;
#[cfg(not(no_global_oom_handling))]
use crate::string::String;

use Cow::*;

#[stable(feature = "rust1", since = "1.0.0")]
impl<'a, B: ?Sized> Borrow<B> for Cow<'a, B>
where
    B: ToOwned,
    <B as ToOwned>::Owned: 'a,
{
    fn borrow(&self) -> &B {
}
}

/// A generalization of `Clone` to borrowed data.
///
/// Some types make it possible to go from borrowed to owned, usually by
/// implementing the `Clone` trait. But `Clone` works only for going from `&T`
/// to `T`. The `ToOwned` trait generalizes `Clone` to construct owned data
/// from any borrow of a given type.
#[cfg_attr(not(test), rustc_diagnostic_item = "ToOwned")]
#[stable(feature = "rust1", since = "1.0.0")]
pub trait ToOwned {
    /// The resulting type after obtaining ownership.
    #[stable(feature = "rust1", since = "1.0.0")]
    type Owned: Borrow<Self>;

    /// Creates owned data from borrowed data, usually by cloning.
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// let s: &str = "a";
    /// let ss: String = s.to_owned();
    ///
    /// let v: &[i32] = &[1, 2];
    /// let vv: Vec<i32> = v.to_owned();
    /// ```
    #[stable(feature = "rust1", since = "1.0.0")]
    #[must_use = "cloning is often expensive and is not expected to have side effects"]
    fn to_owned(&self) -> Self::Owned;

    /// Uses borrowed data to replace owned data, usually by cloning.
    ///
    /// This is borrow-generalized version of `Clone::clone_from`.
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// # #![feature(toowned_clone_into)]
    /// let mut s: String = String::new();
    /// "hello".clone_into(&mut s);
    ///
    /// let mut v: Vec<i32> = Vec::new();
    /// [1, 2][..].clone_into(&mut v);
    /// ```
    #[unstable(feature = "toowned_clone_into", reason = "recently added", issue = "41263")]
    fn clone_into(&self, target: &mut Self::Owned) {
        *target = self.to_owned();
    }
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<T> ToOwned for T
where
    T: Clone,
{
    type Owned = T;
    fn to_owned(&self) -> T {
}

    fn clone_into(&self, target: &mut T) {
}
}

/// A clone-on-write smart pointer.
///
/// The type `Cow` is a smart pointer providing clone-on-write functionality: it
/// can enclose and provide immutable access to borrowed data, and clone the
/// data lazily when mutation or ownership is required. The type is designed to
/// work with general borrowed data via the `Borrow` trait.
///
/// `Cow` implements `Deref`, which means that you can call
/// non-mutating methods directly on the data it encloses. If mutation
/// is desired, `to_mut` will obtain a mutable reference to an owned
/// value, cloning if necessary.
///
/// If you need reference-counting pointers, note that
/// [`Rc::make_mut`][crate::rc::Rc::make_mut] and
/// [`Arc::make_mut`][crate::sync::Arc::make_mut] can provide clone-on-write
/// functionality as well.
///
/// # Examples
///
/// ```
/// use std::borrow::Cow;
///
/// fn abs_all(input: &mut Cow<[i32]>) {
///     for i in 0..input.len() {
///         let v = input[i];
///         if v < 0 {
///             // Clones into a vector if not already owned.
///             input.to_mut()[i] = -v;
///         }
///     }
/// }
///
/// // No clone occurs because `input` doesn't need to be mutated.
/// let slice = [0, 1, 2];
/// let mut input = Cow::from(&slice[..]);
/// abs_all(&mut input);
///
/// // Clone occurs because `input` needs to be mutated.
/// let slice = [-1, 0, 1];
/// let mut input = Cow::from(&slice[..]);
/// abs_all(&mut input);
///
/// // No clone occurs because `input` is already owned.
/// let mut input = Cow::from(vec![-1, 0, 1]);
/// abs_all(&mut input);
/// ```
///
/// Another example showing how to keep `Cow` in a struct:
///
/// ```
/// use std::borrow::Cow;
///
/// struct Items<'a, X: 'a> where [X]: ToOwned<Owned = Vec<X>> {
///     values: Cow<'a, [X]>,
/// }
///
/// impl<'a, X: Clone + 'a> Items<'a, X> where [X]: ToOwned<Owned = Vec<X>> {
///     fn new(v: Cow<'a, [X]>) -> Self {
///         Items { values: v }
///     }
/// }
///
/// // Creates a container from borrowed values of a slice
/// let readonly = [1, 2];
/// let borrowed = Items::new((&readonly[..]).into());
/// match borrowed {
///     Items { values: Cow::Borrowed(b) } => println!("borrowed {:?}", b),
///     _ => panic!("expect borrowed value"),
/// }
///
/// let mut clone_on_write = borrowed;
/// // Mutates the data from slice into owned vec and pushes a new value on top
/// clone_on_write.values.to_mut().push(3);
/// println!("clone_on_write = {:?}", clone_on_write.values);
///
/// // The data was mutated. Let check it out.
/// match clone_on_write {
///     Items { values: Cow::Owned(_) } => println!("clone_on_write contains owned data"),
///     _ => panic!("expect owned data"),
/// }
/// ```
#[stable(feature = "rust1", since = "1.0.0")]
#[cfg_attr(not(test), rustc_diagnostic_item = "Cow")]
pub enum Cow<'a, B: ?Sized + 'a>
where
    B: ToOwned,
{
    /// Borrowed data.
    #[stable(feature = "rust1", since = "1.0.0")]
    Borrowed(#[stable(feature = "rust1", since = "1.0.0")] &'a B),

    /// Owned data.
    #[stable(feature = "rust1", since = "1.0.0")]
    Owned(#[stable(feature = "rust1", since = "1.0.0")] <B as ToOwned>::Owned),
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<B: ?Sized + ToOwned> Clone for Cow<'_, B> {
    fn clone(&self) -> Self {
}

    fn clone_from(&mut self, source: &Self) {
}
}

impl<B: ?Sized + ToOwned> Cow<'_, B> {
    /// Returns true if the data is borrowed, i.e. if `to_mut` would require additional work.
    ///
    /// # Examples
    ///
    /// ```
    /// #![feature(cow_is_borrowed)]
    /// use std::borrow::Cow;
    ///
    /// let cow = Cow::Borrowed("moo");
    /// assert!(cow.is_borrowed());
    ///
    /// let bull: Cow<'_, str> = Cow::Owned("...moo?".to_string());
    /// assert!(!bull.is_borrowed());
    /// ```
    #[unstable(feature = "cow_is_borrowed", issue = "65143")]
    #[rustc_const_unstable(feature = "const_cow_is_borrowed", issue = "65143")]
    pub const fn is_borrowed(&self) -> bool {
}

    /// Returns true if the data is owned, i.e. if `to_mut` would be a no-op.
    ///
    /// # Examples
    ///
    /// ```
    /// #![feature(cow_is_borrowed)]
    /// use std::borrow::Cow;
    ///
    /// let cow: Cow<'_, str> = Cow::Owned("moo".to_string());
    /// assert!(cow.is_owned());
    ///
    /// let bull = Cow::Borrowed("...moo?");
    /// assert!(!bull.is_owned());
    /// ```
    #[unstable(feature = "cow_is_borrowed", issue = "65143")]
    #[rustc_const_unstable(feature = "const_cow_is_borrowed", issue = "65143")]
    pub const fn is_owned(&self) -> bool {
}

    /// Acquires a mutable reference to the owned form of the data.
    ///
    /// Clones the data if it is not already owned.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::borrow::Cow;
    ///
    /// let mut cow = Cow::Borrowed("foo");
    /// cow.to_mut().make_ascii_uppercase();
    ///
    /// assert_eq!(
    ///   cow,
    ///   Cow::Owned(String::from("FOO")) as Cow<str>
    /// );
    /// ```
    #[stable(feature = "rust1", since = "1.0.0")]
    pub fn to_mut(&mut self) -> &mut <B as ToOwned>::Owned {
}

    /// Extracts the owned data.
    ///
    /// Clones the data if it is not already owned.
    ///
    /// # Examples
    ///
    /// Calling `into_owned` on a `Cow::Borrowed` clones the underlying data
    /// and becomes a `Cow::Owned`:
    ///
    /// ```
    /// use std::borrow::Cow;
    ///
    /// let s = "Hello world!";
    /// let cow = Cow::Borrowed(s);
    ///
    /// assert_eq!(
    ///   cow.into_owned(),
    ///   String::from(s)
    /// );
    /// ```
    ///
    /// Calling `into_owned` on a `Cow::Owned` is a no-op:
    ///
    /// ```
    /// use std::borrow::Cow;
    ///
    /// let s = "Hello world!";
    /// let cow: Cow<str> = Cow::Owned(String::from(s));
    ///
    /// assert_eq!(
    ///   cow.into_owned(),
    ///   String::from(s)
    /// );
    /// ```
    #[stable(feature = "rust1", since = "1.0.0")]
    pub fn into_owned(self) -> <B as ToOwned>::Owned {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<B: ?Sized + ToOwned> Deref for Cow<'_, B> {
    type Target = B;

    fn deref(&self) -> &B {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<B: ?Sized> Eq for Cow<'_, B> where B: Eq + ToOwned {}

#[stable(feature = "rust1", since = "1.0.0")]
impl<B: ?Sized> Ord for Cow<'_, B>
where
    B: Ord + ToOwned,
{
    #[inline]
    fn cmp(&self, other: &Self) -> Ordering {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<'a, 'b, B: ?Sized, C: ?Sized> PartialEq<Cow<'b, C>> for Cow<'a, B>
where
    B: PartialEq<C> + ToOwned,
    C: ToOwned,
{
    #[inline]
    fn eq(&self, other: &Cow<'b, C>) -> bool {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<'a, B: ?Sized> PartialOrd for Cow<'a, B>
where
    B: PartialOrd + ToOwned,
{
    #[inline]
    fn partial_cmp(&self, other: &Cow<'a, B>) -> Option<Ordering> {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<B: ?Sized> fmt::Debug for Cow<'_, B>
where
    B: fmt::Debug + ToOwned<Owned: fmt::Debug>,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<B: ?Sized> fmt::Display for Cow<'_, B>
where
    B: fmt::Display + ToOwned<Owned: fmt::Display>,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
}
}

#[stable(feature = "default", since = "1.11.0")]
impl<B: ?Sized> Default for Cow<'_, B>
where
    B: ToOwned<Owned: Default>,
{
    /// Creates an owned Cow<'a, B> with the default value for the contained owned value.
    fn default() -> Self {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<B: ?Sized> Hash for Cow<'_, B>
where
    B: Hash + ToOwned,
{
    #[inline]
    fn hash<H: Hasher>(&self, state: &mut H) {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<T: ?Sized + ToOwned> AsRef<T> for Cow<'_, T> {
    fn as_ref(&self) -> &T {
}
}

#[cfg(not(no_global_oom_handling))]
#[stable(feature = "cow_add", since = "1.14.0")]
impl<'a> Add<&'a str> for Cow<'a, str> {
    type Output = Cow<'a, str>;

    #[inline]
    fn add(mut self, rhs: &'a str) -> Self::Output {
}
}

#[cfg(not(no_global_oom_handling))]
#[stable(feature = "cow_add", since = "1.14.0")]
impl<'a> Add<Cow<'a, str>> for Cow<'a, str> {
    type Output = Cow<'a, str>;

    #[inline]
    fn add(mut self, rhs: Cow<'a, str>) -> Self::Output {
}
}

#[cfg(not(no_global_oom_handling))]
#[stable(feature = "cow_add", since = "1.14.0")]
impl<'a> AddAssign<&'a str> for Cow<'a, str> {
    fn add_assign(&mut self, rhs: &'a str) {
}
}

#[cfg(not(no_global_oom_handling))]
#[stable(feature = "cow_add", since = "1.14.0")]
impl<'a> AddAssign<Cow<'a, str>> for Cow<'a, str> {
    fn add_assign(&mut self, rhs: Cow<'a, str>) {
}
}
}
pub mod collections {
//! Collection types.

#![stable(feature = "rust1", since = "1.0.0")]

#[cfg(not(no_global_oom_handling))]
pub mod binary_heap {
//! A priority queue implemented with a binary heap.
//!
//! Insertion and popping the largest element have *O*(log(*n*)) time complexity.
//! Checking the largest element is *O*(1). Converting a vector to a binary heap
//! can be done in-place, and has *O*(*n*) complexity. A binary heap can also be
//! converted to a sorted vector in-place, allowing it to be used for an *O*(*n* \* log(*n*))
//! in-place heapsort.
//!
//! # Examples
//!
//! This is a larger example that implements [Dijkstra's algorithm][dijkstra]
//! to solve the [shortest path problem][sssp] on a [directed graph][dir_graph].
//! It shows how to use [`BinaryHeap`] with custom types.
//!
//! [dijkstra]: https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm
//! [sssp]: https://en.wikipedia.org/wiki/Shortest_path_problem
//! [dir_graph]: https://en.wikipedia.org/wiki/Directed_graph
//!
//! ```
//! use std::cmp::Ordering;
//! use std::collections::BinaryHeap;
//!
//! #[derive(Copy, Clone, Eq, PartialEq)]
//! struct State {
//!     cost: usize,
//!     position: usize,
//! }
//!
//! // The priority queue depends on `Ord`.
//! // Explicitly implement the trait so the queue becomes a min-heap
//! // instead of a max-heap.
//! impl Ord for State {
//!     fn cmp(&self, other: &Self) -> Ordering {
//!         // Notice that the we flip the ordering on costs.
//!         // In case of a tie we compare positions - this step is necessary
//!         // to make implementations of `PartialEq` and `Ord` consistent.
//!         other.cost.cmp(&self.cost)
//!             .then_with(|| self.position.cmp(&other.position))
//!     }
//! }
//!
//! // `PartialOrd` needs to be implemented as well.
//! impl PartialOrd for State {
//!     fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
//!         Some(self.cmp(other))
//!     }
//! }
//!
//! // Each node is represented as a `usize`, for a shorter implementation.
//! struct Edge {
//!     node: usize,
//!     cost: usize,
//! }
//!
//! // Dijkstra's shortest path algorithm.
//!
//! // Start at `start` and use `dist` to track the current shortest distance
//! // to each node. This implementation isn't memory-efficient as it may leave duplicate
//! // nodes in the queue. It also uses `usize::MAX` as a sentinel value,
//! // for a simpler implementation.
//! fn shortest_path(adj_list: &Vec<Vec<Edge>>, start: usize, goal: usize) -> Option<usize> {
//!     // dist[node] = current shortest distance from `start` to `node`
//!     let mut dist: Vec<_> = (0..adj_list.len()).map(|_| usize::MAX).collect();
//!
//!     let mut heap = BinaryHeap::new();
//!
//!     // We're at `start`, with a zero cost
//!     dist[start] = 0;
//!     heap.push(State { cost: 0, position: start });
//!
//!     // Examine the frontier with lower cost nodes first (min-heap)
//!     while let Some(State { cost, position }) = heap.pop() {
//!         // Alternatively we could have continued to find all shortest paths
//!         if position == goal { return Some(cost); }
//!
//!         // Important as we may have already found a better way
//!         if cost > dist[position] { continue; }
//!
//!         // For each node we can reach, see if we can find a way with
//!         // a lower cost going through this node
//!         for edge in &adj_list[position] {
//!             let next = State { cost: cost + edge.cost, position: edge.node };
//!
//!             // If so, add it to the frontier and continue
//!             if next.cost < dist[next.position] {
//!                 heap.push(next);
//!                 // Relaxation, we have now found a better way
//!                 dist[next.position] = next.cost;
//!             }
//!         }
//!     }
//!
//!     // Goal not reachable
//!     None
//! }
//!
//! fn main() {
//!     // This is the directed graph we're going to use.
//!     // The node numbers correspond to the different states,
//!     // and the edge weights symbolize the cost of moving
//!     // from one node to another.
//!     // Note that the edges are one-way.
//!     //
//!     //                  7
//!     //          +-----------------+
//!     //          |                 |
//!     //          v   1        2    |  2
//!     //          0 -----> 1 -----> 3 ---> 4
//!     //          |        ^        ^      ^
//!     //          |        | 1      |      |
//!     //          |        |        | 3    | 1
//!     //          +------> 2 -------+      |
//!     //           10      |               |
//!     //                   +---------------+
//!     //
//!     // The graph is represented as an adjacency list where each index,
//!     // corresponding to a node value, has a list of outgoing edges.
//!     // Chosen for its efficiency.
//!     let graph = vec![
//!         // Node 0
//!         vec![Edge { node: 2, cost: 10 },
//!              Edge { node: 1, cost: 1 }],
//!         // Node 1
//!         vec![Edge { node: 3, cost: 2 }],
//!         // Node 2
//!         vec![Edge { node: 1, cost: 1 },
//!              Edge { node: 3, cost: 3 },
//!              Edge { node: 4, cost: 1 }],
//!         // Node 3
//!         vec![Edge { node: 0, cost: 7 },
//!              Edge { node: 4, cost: 2 }],
//!         // Node 4
//!         vec![]];
//!
//!     assert_eq!(shortest_path(&graph, 0, 1), Some(1));
//!     assert_eq!(shortest_path(&graph, 0, 3), Some(3));
//!     assert_eq!(shortest_path(&graph, 3, 0), Some(7));
//!     assert_eq!(shortest_path(&graph, 0, 4), Some(5));
//!     assert_eq!(shortest_path(&graph, 4, 0), None);
//! }
//! ```

#![allow(missing_docs)]
#![stable(feature = "rust1", since = "1.0.0")]

use core::fmt;
use core::iter::{FromIterator, FusedIterator, InPlaceIterable, SourceIter, TrustedLen};
use core::mem::{self, swap, ManuallyDrop};
use core::ops::{Deref, DerefMut};
use core::ptr;

use crate::slice;
use crate::vec::{self, AsIntoIter, Vec};

use super::SpecExtend;

/// A priority queue implemented with a binary heap.
///
/// This will be a max-heap.
///
/// It is a logic error for an item to be modified in such a way that the
/// item's ordering relative to any other item, as determined by the `Ord`
/// trait, changes while it is in the heap. This is normally only possible
/// through `Cell`, `RefCell`, global state, I/O, or unsafe code. The
/// behavior resulting from such a logic error is not specified, but will
/// not result in undefined behavior. This could include panics, incorrect
/// results, aborts, memory leaks, and non-termination.
///
/// # Examples
///
/// ```
/// use std::collections::BinaryHeap;
///
/// // Type inference lets us omit an explicit type signature (which
/// // would be `BinaryHeap<i32>` in this example).
/// let mut heap = BinaryHeap::new();
///
/// // We can use peek to look at the next item in the heap. In this case,
/// // there's no items in there yet so we get None.
/// assert_eq!(heap.peek(), None);
///
/// // Let's add some scores...
/// heap.push(1);
/// heap.push(5);
/// heap.push(2);
///
/// // Now peek shows the most important item in the heap.
/// assert_eq!(heap.peek(), Some(&5));
///
/// // We can check the length of a heap.
/// assert_eq!(heap.len(), 3);
///
/// // We can iterate over the items in the heap, although they are returned in
/// // a random order.
/// for x in &heap {
///     println!("{}", x);
/// }
///
/// // If we instead pop these scores, they should come back in order.
/// assert_eq!(heap.pop(), Some(5));
/// assert_eq!(heap.pop(), Some(2));
/// assert_eq!(heap.pop(), Some(1));
/// assert_eq!(heap.pop(), None);
///
/// // We can clear the heap of any remaining items.
/// heap.clear();
///
/// // The heap should now be empty.
/// assert!(heap.is_empty())
/// ```
///
/// A `BinaryHeap` with a known list of items can be initialized from an array:
///
/// ```
/// use std::collections::BinaryHeap;
///
/// let heap = BinaryHeap::from([1, 5, 2]);
/// ```
///
/// ## Min-heap
///
/// Either `std::cmp::Reverse` or a custom `Ord` implementation can be used to
/// make `BinaryHeap` a min-heap. This makes `heap.pop()` return the smallest
/// value instead of the greatest one.
///
/// ```
/// use std::collections::BinaryHeap;
/// use std::cmp::Reverse;
///
/// let mut heap = BinaryHeap::new();
///
/// // Wrap values in `Reverse`
/// heap.push(Reverse(1));
/// heap.push(Reverse(5));
/// heap.push(Reverse(2));
///
/// // If we pop these scores now, they should come back in the reverse order.
/// assert_eq!(heap.pop(), Some(Reverse(1)));
/// assert_eq!(heap.pop(), Some(Reverse(2)));
/// assert_eq!(heap.pop(), Some(Reverse(5)));
/// assert_eq!(heap.pop(), None);
/// ```
///
/// # Time complexity
///
/// | [push] | [pop]     | [peek]/[peek\_mut] |
/// |--------|-----------|--------------------|
/// | O(1)~  | *O*(log(*n*)) | *O*(1)               |
///
/// The value for `push` is an expected cost; the method documentation gives a
/// more detailed analysis.
///
/// [push]: BinaryHeap::push
/// [pop]: BinaryHeap::pop
/// [peek]: BinaryHeap::peek
/// [peek\_mut]: BinaryHeap::peek_mut
#[stable(feature = "rust1", since = "1.0.0")]
#[cfg_attr(not(test), rustc_diagnostic_item = "BinaryHeap")]
pub struct BinaryHeap<T> {
    data: Vec<T>,
}

/// Structure wrapping a mutable reference to the greatest item on a
/// `BinaryHeap`.
///
/// This `struct` is created by the [`peek_mut`] method on [`BinaryHeap`]. See
/// its documentation for more.
///
/// [`peek_mut`]: BinaryHeap::peek_mut
#[stable(feature = "binary_heap_peek_mut", since = "1.12.0")]
pub struct PeekMut<'a, T: 'a + Ord> {
    heap: &'a mut BinaryHeap<T>,
    sift: bool,
}

#[stable(feature = "collection_debug", since = "1.17.0")]
impl<T: Ord + fmt::Debug> fmt::Debug for PeekMut<'_, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
}
}

#[stable(feature = "binary_heap_peek_mut", since = "1.12.0")]
impl<T: Ord> Drop for PeekMut<'_, T> {
    fn drop(&mut self) {
}
}

#[stable(feature = "binary_heap_peek_mut", since = "1.12.0")]
impl<T: Ord> Deref for PeekMut<'_, T> {
    type Target = T;
    fn deref(&self) -> &T {
}
}

#[stable(feature = "binary_heap_peek_mut", since = "1.12.0")]
impl<T: Ord> DerefMut for PeekMut<'_, T> {
    fn deref_mut(&mut self) -> &mut T {
}
}

impl<'a, T: Ord> PeekMut<'a, T> {
    /// Removes the peeked value from the heap and returns it.
    #[stable(feature = "binary_heap_peek_mut_pop", since = "1.18.0")]
    pub fn pop(mut this: PeekMut<'a, T>) -> T {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<T: Clone> Clone for BinaryHeap<T> {
    fn clone(&self) -> Self {
}

    fn clone_from(&mut self, source: &Self) {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<T: Ord> Default for BinaryHeap<T> {
    /// Creates an empty `BinaryHeap<T>`.
    #[inline]
    fn default() -> BinaryHeap<T> {
}
}

#[stable(feature = "binaryheap_debug", since = "1.4.0")]
impl<T: fmt::Debug> fmt::Debug for BinaryHeap<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
}
}

impl<T: Ord> BinaryHeap<T> {
    /// Creates an empty `BinaryHeap` as a max-heap.
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// use std::collections::BinaryHeap;
    /// let mut heap = BinaryHeap::new();
    /// heap.push(4);
    /// ```
    #[stable(feature = "rust1", since = "1.0.0")]
    pub fn new() -> BinaryHeap<T> {
}

    /// Creates an empty `BinaryHeap` with a specific capacity.
    /// This preallocates enough memory for `capacity` elements,
    /// so that the `BinaryHeap` does not have to be reallocated
    /// until it contains at least that many values.
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// use std::collections::BinaryHeap;
    /// let mut heap = BinaryHeap::with_capacity(10);
    /// heap.push(4);
    /// ```
    #[stable(feature = "rust1", since = "1.0.0")]
    pub fn with_capacity(capacity: usize) -> BinaryHeap<T> {
}

    /// Returns a mutable reference to the greatest item in the binary heap, or
    /// `None` if it is empty.
    ///
    /// Note: If the `PeekMut` value is leaked, the heap may be in an
    /// inconsistent state.
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// use std::collections::BinaryHeap;
    /// let mut heap = BinaryHeap::new();
    /// assert!(heap.peek_mut().is_none());
    ///
    /// heap.push(1);
    /// heap.push(5);
    /// heap.push(2);
    /// {
    ///     let mut val = heap.peek_mut().unwrap();
    ///     *val = 0;
    /// }
    /// assert_eq!(heap.peek(), Some(&2));
    /// ```
    ///
    /// # Time complexity
    ///
    /// If the item is modified then the worst case time complexity is *O*(log(*n*)),
    /// otherwise it's *O*(1).
    #[stable(feature = "binary_heap_peek_mut", since = "1.12.0")]
    pub fn peek_mut(&mut self) -> Option<PeekMut<'_, T>> {
}

    /// Removes the greatest item from the binary heap and returns it, or `None` if it
    /// is empty.
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// use std::collections::BinaryHeap;
    /// let mut heap = BinaryHeap::from(vec![1, 3]);
    ///
    /// assert_eq!(heap.pop(), Some(3));
    /// assert_eq!(heap.pop(), Some(1));
    /// assert_eq!(heap.pop(), None);
    /// ```
    ///
    /// # Time complexity
    ///
    /// The worst case cost of `pop` on a heap containing *n* elements is *O*(log(*n*)).
    #[stable(feature = "rust1", since = "1.0.0")]
    pub fn pop(&mut self) -> Option<T> {
}

    /// Pushes an item onto the binary heap.
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// use std::collections::BinaryHeap;
    /// let mut heap = BinaryHeap::new();
    /// heap.push(3);
    /// heap.push(5);
    /// heap.push(1);
    ///
    /// assert_eq!(heap.len(), 3);
    /// assert_eq!(heap.peek(), Some(&5));
    /// ```
    ///
    /// # Time complexity
    ///
    /// The expected cost of `push`, averaged over every possible ordering of
    /// the elements being pushed, and over a sufficiently large number of
    /// pushes, is *O*(1). This is the most meaningful cost metric when pushing
    /// elements that are *not* already in any sorted pattern.
    ///
    /// The time complexity degrades if elements are pushed in predominantly
    /// ascending order. In the worst case, elements are pushed in ascending
    /// sorted order and the amortized cost per push is *O*(log(*n*)) against a heap
    /// containing *n* elements.
    ///
    /// The worst case cost of a *single* call to `push` is *O*(*n*). The worst case
    /// occurs when capacity is exhausted and needs a resize. The resize cost
    /// has been amortized in the previous figures.
    #[stable(feature = "rust1", since = "1.0.0")]
    pub fn push(&mut self, item: T) {
}

    /// Consumes the `BinaryHeap` and returns a vector in sorted
    /// (ascending) order.
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// use std::collections::BinaryHeap;
    ///
    /// let mut heap = BinaryHeap::from(vec![1, 2, 4, 5, 7]);
    /// heap.push(6);
    /// heap.push(3);
    ///
    /// let vec = heap.into_sorted_vec();
    /// assert_eq!(vec, [1, 2, 3, 4, 5, 6, 7]);
    /// ```
    #[stable(feature = "binary_heap_extras_15", since = "1.5.0")]
    pub fn into_sorted_vec(mut self) -> Vec<T> {
}

    // The implementations of sift_up and sift_down use unsafe blocks in
    // order to move an element out of the vector (leaving behind a
    // hole), shift along the others and move the removed element back into the
    // vector at the final location of the hole.
    // The `Hole` type is used to represent this, and make sure
    // the hole is filled back at the end of its scope, even on panic.
    // Using a hole reduces the constant factor compared to using swaps,
    // which involves twice as many moves.

    /// # Safety
    ///
    /// The caller must guarantee that `pos < self.len()`.
    unsafe fn sift_up(&mut self, start: usize, pos: usize) -> usize {
}

    /// Take an element at `pos` and move it down the heap,
    /// while its children are larger.
    ///
    /// # Safety
    ///
    /// The caller must guarantee that `pos < end <= self.len()`.
    unsafe fn sift_down_range(&mut self, pos: usize, end: usize) {
}

    /// # Safety
    ///
    /// The caller must guarantee that `pos < self.len()`.
    unsafe fn sift_down(&mut self, pos: usize) {
}

    /// Take an element at `pos` and move it all the way down the heap,
    /// then sift it up to its position.
    ///
    /// Note: This is faster when the element is known to be large / should
    /// be closer to the bottom.
    ///
    /// # Safety
    ///
    /// The caller must guarantee that `pos < self.len()`.
    unsafe fn sift_down_to_bottom(&mut self, mut pos: usize) {
}

    /// Rebuild assuming data[0..start] is still a proper heap.
    fn rebuild_tail(&mut self, start: usize) {
}

    fn rebuild(&mut self) {
}

    /// Moves all the elements of `other` into `self`, leaving `other` empty.
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// use std::collections::BinaryHeap;
    ///
    /// let v = vec![-10, 1, 2, 3, 3];
    /// let mut a = BinaryHeap::from(v);
    ///
    /// let v = vec![-20, 5, 43];
    /// let mut b = BinaryHeap::from(v);
    ///
    /// a.append(&mut b);
    ///
    /// assert_eq!(a.into_sorted_vec(), [-20, -10, 1, 2, 3, 3, 5, 43]);
    /// assert!(b.is_empty());
    /// ```
    #[stable(feature = "binary_heap_append", since = "1.11.0")]
    pub fn append(&mut self, other: &mut Self) {
}

    /// Returns an iterator which retrieves elements in heap order.
    /// The retrieved elements are removed from the original heap.
    /// The remaining elements will be removed on drop in heap order.
    ///
    /// Note:
    /// * `.drain_sorted()` is *O*(*n* \* log(*n*)); much slower than `.drain()`.
    ///   You should use the latter for most cases.
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// #![feature(binary_heap_drain_sorted)]
    /// use std::collections::BinaryHeap;
    ///
    /// let mut heap = BinaryHeap::from(vec![1, 2, 3, 4, 5]);
    /// assert_eq!(heap.len(), 5);
    ///
    /// drop(heap.drain_sorted()); // removes all elements in heap order
    /// assert_eq!(heap.len(), 0);
    /// ```
    #[inline]
    #[unstable(feature = "binary_heap_drain_sorted", issue = "59278")]
    pub fn drain_sorted(&mut self) -> DrainSorted<'_, T> {
}

    /// Retains only the elements specified by the predicate.
    ///
    /// In other words, remove all elements `e` such that `f(&e)` returns
    /// `false`. The elements are visited in unsorted (and unspecified) order.
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// #![feature(binary_heap_retain)]
    /// use std::collections::BinaryHeap;
    ///
    /// let mut heap = BinaryHeap::from(vec![-10, -5, 1, 2, 4, 13]);
    ///
    /// heap.retain(|x| x % 2 == 0); // only keep even numbers
    ///
    /// assert_eq!(heap.into_sorted_vec(), [-10, 2, 4])
    /// ```
    #[unstable(feature = "binary_heap_retain", issue = "71503")]
    pub fn retain<F>(&mut self, mut f: F)
    where
        F: FnMut(&T) -> bool,
    {
}
}

impl<T> BinaryHeap<T> {
    /// Returns an iterator visiting all values in the underlying vector, in
    /// arbitrary order.
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// use std::collections::BinaryHeap;
    /// let heap = BinaryHeap::from(vec![1, 2, 3, 4]);
    ///
    /// // Print 1, 2, 3, 4 in arbitrary order
    /// for x in heap.iter() {
    ///     println!("{}", x);
    /// }
    /// ```
    #[stable(feature = "rust1", since = "1.0.0")]
    pub fn iter(&self) -> Iter<'_, T> {
}

    /// Returns an iterator which retrieves elements in heap order.
    /// This method consumes the original heap.
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// #![feature(binary_heap_into_iter_sorted)]
    /// use std::collections::BinaryHeap;
    /// let heap = BinaryHeap::from(vec![1, 2, 3, 4, 5]);
    ///
    /// assert_eq!(heap.into_iter_sorted().take(2).collect::<Vec<_>>(), vec![5, 4]);
    /// ```
    #[unstable(feature = "binary_heap_into_iter_sorted", issue = "59278")]
    pub fn into_iter_sorted(self) -> IntoIterSorted<T> {
}

    /// Returns the greatest item in the binary heap, or `None` if it is empty.
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// use std::collections::BinaryHeap;
    /// let mut heap = BinaryHeap::new();
    /// assert_eq!(heap.peek(), None);
    ///
    /// heap.push(1);
    /// heap.push(5);
    /// heap.push(2);
    /// assert_eq!(heap.peek(), Some(&5));
    ///
    /// ```
    ///
    /// # Time complexity
    ///
    /// Cost is *O*(1) in the worst case.
    #[stable(feature = "rust1", since = "1.0.0")]
    pub fn peek(&self) -> Option<&T> {
}

    /// Returns the number of elements the binary heap can hold without reallocating.
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// use std::collections::BinaryHeap;
    /// let mut heap = BinaryHeap::with_capacity(100);
    /// assert!(heap.capacity() >= 100);
    /// heap.push(4);
    /// ```
    #[stable(feature = "rust1", since = "1.0.0")]
    pub fn capacity(&self) -> usize {
}

    /// Reserves the minimum capacity for exactly `additional` more elements to be inserted in the
    /// given `BinaryHeap`. Does nothing if the capacity is already sufficient.
    ///
    /// Note that the allocator may give the collection more space than it requests. Therefore
    /// capacity can not be relied upon to be precisely minimal. Prefer [`reserve`] if future
    /// insertions are expected.
    ///
    /// # Panics
    ///
    /// Panics if the new capacity overflows `usize`.
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// use std::collections::BinaryHeap;
    /// let mut heap = BinaryHeap::new();
    /// heap.reserve_exact(100);
    /// assert!(heap.capacity() >= 100);
    /// heap.push(4);
    /// ```
    ///
    /// [`reserve`]: BinaryHeap::reserve
    #[stable(feature = "rust1", since = "1.0.0")]
    pub fn reserve_exact(&mut self, additional: usize) {
}

    /// Reserves capacity for at least `additional` more elements to be inserted in the
    /// `BinaryHeap`. The collection may reserve more space to avoid frequent reallocations.
    ///
    /// # Panics
    ///
    /// Panics if the new capacity overflows `usize`.
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// use std::collections::BinaryHeap;
    /// let mut heap = BinaryHeap::new();
    /// heap.reserve(100);
    /// assert!(heap.capacity() >= 100);
    /// heap.push(4);
    /// ```
    #[stable(feature = "rust1", since = "1.0.0")]
    pub fn reserve(&mut self, additional: usize) {
}

    /// Discards as much additional capacity as possible.
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// use std::collections::BinaryHeap;
    /// let mut heap: BinaryHeap<i32> = BinaryHeap::with_capacity(100);
    ///
    /// assert!(heap.capacity() >= 100);
    /// heap.shrink_to_fit();
    /// assert!(heap.capacity() == 0);
    /// ```
    #[stable(feature = "rust1", since = "1.0.0")]
    pub fn shrink_to_fit(&mut self) {
}

    /// Discards capacity with a lower bound.
    ///
    /// The capacity will remain at least as large as both the length
    /// and the supplied value.
    ///
    /// If the current capacity is less than the lower limit, this is a no-op.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::collections::BinaryHeap;
    /// let mut heap: BinaryHeap<i32> = BinaryHeap::with_capacity(100);
    ///
    /// assert!(heap.capacity() >= 100);
    /// heap.shrink_to(10);
    /// assert!(heap.capacity() >= 10);
    /// ```
    #[inline]
    #[stable(feature = "shrink_to", since = "1.56.0")]
    pub fn shrink_to(&mut self, min_capacity: usize) {
}

    /// Returns a slice of all values in the underlying vector, in arbitrary
    /// order.
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// #![feature(binary_heap_as_slice)]
    /// use std::collections::BinaryHeap;
    /// use std::io::{self, Write};
    ///
    /// let heap = BinaryHeap::from(vec![1, 2, 3, 4, 5, 6, 7]);
    ///
    /// io::sink().write(heap.as_slice()).unwrap();
    /// ```
    #[unstable(feature = "binary_heap_as_slice", issue = "83659")]
    pub fn as_slice(&self) -> &[T] {
}

    /// Consumes the `BinaryHeap` and returns the underlying vector
    /// in arbitrary order.
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// use std::collections::BinaryHeap;
    /// let heap = BinaryHeap::from(vec![1, 2, 3, 4, 5, 6, 7]);
    /// let vec = heap.into_vec();
    ///
    /// // Will print in some order
    /// for x in vec {
    ///     println!("{}", x);
    /// }
    /// ```
    #[stable(feature = "binary_heap_extras_15", since = "1.5.0")]
    pub fn into_vec(self) -> Vec<T> {
}

    /// Returns the length of the binary heap.
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// use std::collections::BinaryHeap;
    /// let heap = BinaryHeap::from(vec![1, 3]);
    ///
    /// assert_eq!(heap.len(), 2);
    /// ```
    #[stable(feature = "rust1", since = "1.0.0")]
    pub fn len(&self) -> usize {
}

    /// Checks if the binary heap is empty.
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// use std::collections::BinaryHeap;
    /// let mut heap = BinaryHeap::new();
    ///
    /// assert!(heap.is_empty());
    ///
    /// heap.push(3);
    /// heap.push(5);
    /// heap.push(1);
    ///
    /// assert!(!heap.is_empty());
    /// ```
    #[stable(feature = "rust1", since = "1.0.0")]
    pub fn is_empty(&self) -> bool {
}

    /// Clears the binary heap, returning an iterator over the removed elements.
    ///
    /// The elements are removed in arbitrary order.
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// use std::collections::BinaryHeap;
    /// let mut heap = BinaryHeap::from(vec![1, 3]);
    ///
    /// assert!(!heap.is_empty());
    ///
    /// for x in heap.drain() {
    ///     println!("{}", x);
    /// }
    ///
    /// assert!(heap.is_empty());
    /// ```
    #[inline]
    #[stable(feature = "drain", since = "1.6.0")]
    pub fn drain(&mut self) -> Drain<'_, T> {
}

    /// Drops all items from the binary heap.
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// use std::collections::BinaryHeap;
    /// let mut heap = BinaryHeap::from(vec![1, 3]);
    ///
    /// assert!(!heap.is_empty());
    ///
    /// heap.clear();
    ///
    /// assert!(heap.is_empty());
    /// ```
    #[stable(feature = "rust1", since = "1.0.0")]
    pub fn clear(&mut self) {
}
}

/// Hole represents a hole in a slice i.e., an index without valid value
/// (because it was moved from or duplicated).
/// In drop, `Hole` will restore the slice by filling the hole
/// position with the value that was originally removed.
struct Hole<'a, T: 'a> {
    data: &'a mut [T],
    elt: ManuallyDrop<T>,
    pos: usize,
}

impl<'a, T> Hole<'a, T> {
    /// Create a new `Hole` at index `pos`.
    ///
    /// Unsafe because pos must be within the data slice.
    #[inline]
    unsafe fn new(data: &'a mut [T], pos: usize) -> Self {
}

    #[inline]
    fn pos(&self) -> usize {
}

    /// Returns a reference to the element removed.
    #[inline]
    fn element(&self) -> &T {
}

    /// Returns a reference to the element at `index`.
    ///
    /// Unsafe because index must be within the data slice and not equal to pos.
    #[inline]
    unsafe fn get(&self, index: usize) -> &T {
}

    /// Move hole to new location
    ///
    /// Unsafe because index must be within the data slice and not equal to pos.
    #[inline]
    unsafe fn move_to(&mut self, index: usize) {
}
}

impl<T> Drop for Hole<'_, T> {
    #[inline]
    fn drop(&mut self) {
}
}

/// An iterator over the elements of a `BinaryHeap`.
///
/// This `struct` is created by [`BinaryHeap::iter()`]. See its
/// documentation for more.
///
/// [`iter`]: BinaryHeap::iter
#[stable(feature = "rust1", since = "1.0.0")]
pub struct Iter<'a, T: 'a> {
    iter: slice::Iter<'a, T>,
}

#[stable(feature = "collection_debug", since = "1.17.0")]
impl<T: fmt::Debug> fmt::Debug for Iter<'_, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
}
}

// FIXME(#26925) Remove in favor of `#[derive(Clone)]`
#[stable(feature = "rust1", since = "1.0.0")]
impl<T> Clone for Iter<'_, T> {
    fn clone(&self) -> Self {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<'a, T> Iterator for Iter<'a, T> {
    type Item = &'a T;

    #[inline]
    fn next(&mut self) -> Option<&'a T> {
}

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
}

    #[inline]
    fn last(self) -> Option<&'a T> {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<'a, T> DoubleEndedIterator for Iter<'a, T> {
    #[inline]
    fn next_back(&mut self) -> Option<&'a T> {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<T> ExactSizeIterator for Iter<'_, T> {
    fn is_empty(&self) -> bool {
}
}

#[stable(feature = "fused", since = "1.26.0")]
impl<T> FusedIterator for Iter<'_, T> {}

/// An owning iterator over the elements of a `BinaryHeap`.
///
/// This `struct` is created by [`BinaryHeap::into_iter()`]
/// (provided by the `IntoIterator` trait). See its documentation for more.
///
/// [`into_iter`]: BinaryHeap::into_iter
#[stable(feature = "rust1", since = "1.0.0")]
#[derive(Clone)]
pub struct IntoIter<T> {
    iter: vec::IntoIter<T>,
}

#[stable(feature = "collection_debug", since = "1.17.0")]
impl<T: fmt::Debug> fmt::Debug for IntoIter<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<T> Iterator for IntoIter<T> {
    type Item = T;

    #[inline]
    fn next(&mut self) -> Option<T> {
}

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<T> DoubleEndedIterator for IntoIter<T> {
    #[inline]
    fn next_back(&mut self) -> Option<T> {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<T> ExactSizeIterator for IntoIter<T> {
    fn is_empty(&self) -> bool {
}
}

#[stable(feature = "fused", since = "1.26.0")]
impl<T> FusedIterator for IntoIter<T> {}

#[unstable(issue = "none", feature = "inplace_iteration")]
#[doc(hidden)]
unsafe impl<T> SourceIter for IntoIter<T> {
    type Source = IntoIter<T>;

    #[inline]
    unsafe fn as_inner(&mut self) -> &mut Self::Source {
}
}

#[unstable(issue = "none", feature = "inplace_iteration")]
#[doc(hidden)]
unsafe impl<I> InPlaceIterable for IntoIter<I> {}

impl<I> AsIntoIter for IntoIter<I> {
    type Item = I;

    fn as_into_iter(&mut self) -> &mut vec::IntoIter<Self::Item> {
}
}

#[unstable(feature = "binary_heap_into_iter_sorted", issue = "59278")]
#[derive(Clone, Debug)]
pub struct IntoIterSorted<T> {
    inner: BinaryHeap<T>,
}

#[unstable(feature = "binary_heap_into_iter_sorted", issue = "59278")]
impl<T: Ord> Iterator for IntoIterSorted<T> {
    type Item = T;

    #[inline]
    fn next(&mut self) -> Option<T> {
}

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
}
}

#[unstable(feature = "binary_heap_into_iter_sorted", issue = "59278")]
impl<T: Ord> ExactSizeIterator for IntoIterSorted<T> {}

#[unstable(feature = "binary_heap_into_iter_sorted", issue = "59278")]
impl<T: Ord> FusedIterator for IntoIterSorted<T> {}

#[unstable(feature = "trusted_len", issue = "37572")]
unsafe impl<T: Ord> TrustedLen for IntoIterSorted<T> {}

/// A draining iterator over the elements of a `BinaryHeap`.
///
/// This `struct` is created by [`BinaryHeap::drain()`]. See its
/// documentation for more.
///
/// [`drain`]: BinaryHeap::drain
#[stable(feature = "drain", since = "1.6.0")]
#[derive(Debug)]
pub struct Drain<'a, T: 'a> {
    iter: vec::Drain<'a, T>,
}

#[stable(feature = "drain", since = "1.6.0")]
impl<T> Iterator for Drain<'_, T> {
    type Item = T;

    #[inline]
    fn next(&mut self) -> Option<T> {
}

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
}
}

#[stable(feature = "drain", since = "1.6.0")]
impl<T> DoubleEndedIterator for Drain<'_, T> {
    #[inline]
    fn next_back(&mut self) -> Option<T> {
}
}

#[stable(feature = "drain", since = "1.6.0")]
impl<T> ExactSizeIterator for Drain<'_, T> {
    fn is_empty(&self) -> bool {
}
}

#[stable(feature = "fused", since = "1.26.0")]
impl<T> FusedIterator for Drain<'_, T> {}

/// A draining iterator over the elements of a `BinaryHeap`.
///
/// This `struct` is created by [`BinaryHeap::drain_sorted()`]. See its
/// documentation for more.
///
/// [`drain_sorted`]: BinaryHeap::drain_sorted
#[unstable(feature = "binary_heap_drain_sorted", issue = "59278")]
#[derive(Debug)]
pub struct DrainSorted<'a, T: Ord> {
    inner: &'a mut BinaryHeap<T>,
}

#[unstable(feature = "binary_heap_drain_sorted", issue = "59278")]
impl<'a, T: Ord> Drop for DrainSorted<'a, T> {
    /// Removes heap elements in heap order.
    fn drop(&mut self) {
}
}

#[unstable(feature = "binary_heap_drain_sorted", issue = "59278")]
impl<T: Ord> Iterator for DrainSorted<'_, T> {
    type Item = T;

    #[inline]
    fn next(&mut self) -> Option<T> {
}

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
}
}

#[unstable(feature = "binary_heap_drain_sorted", issue = "59278")]
impl<T: Ord> ExactSizeIterator for DrainSorted<'_, T> {}

#[unstable(feature = "binary_heap_drain_sorted", issue = "59278")]
impl<T: Ord> FusedIterator for DrainSorted<'_, T> {}

#[unstable(feature = "trusted_len", issue = "37572")]
unsafe impl<T: Ord> TrustedLen for DrainSorted<'_, T> {}

#[stable(feature = "binary_heap_extras_15", since = "1.5.0")]
impl<T: Ord> From<Vec<T>> for BinaryHeap<T> {
    /// Converts a `Vec<T>` into a `BinaryHeap<T>`.
    ///
    /// This conversion happens in-place, and has *O*(*n*) time complexity.
    fn from(vec: Vec<T>) -> BinaryHeap<T> {
}
}

#[stable(feature = "std_collections_from_array", since = "1.56.0")]
impl<T: Ord, const N: usize> From<[T; N]> for BinaryHeap<T> {
    /// ```
    /// use std::collections::BinaryHeap;
    ///
    /// let mut h1 = BinaryHeap::from([1, 4, 2, 3]);
    /// let mut h2: BinaryHeap<_> = [1, 4, 2, 3].into();
    /// while let Some((a, b)) = h1.pop().zip(h2.pop()) {
    ///     assert_eq!(a, b);
    /// }
    /// ```
    fn from(arr: [T; N]) -> Self {
}
}

#[stable(feature = "binary_heap_extras_15", since = "1.5.0")]
impl<T> From<BinaryHeap<T>> for Vec<T> {
    /// Converts a `BinaryHeap<T>` into a `Vec<T>`.
    ///
    /// This conversion requires no data movement or allocation, and has
    /// constant time complexity.
    fn from(heap: BinaryHeap<T>) -> Vec<T> {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<T: Ord> FromIterator<T> for BinaryHeap<T> {
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> BinaryHeap<T> {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<T> IntoIterator for BinaryHeap<T> {
    type Item = T;
    type IntoIter = IntoIter<T>;

    /// Creates a consuming iterator, that is, one that moves each value out of
    /// the binary heap in arbitrary order. The binary heap cannot be used
    /// after calling this.
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// use std::collections::BinaryHeap;
    /// let heap = BinaryHeap::from(vec![1, 2, 3, 4]);
    ///
    /// // Print 1, 2, 3, 4 in arbitrary order
    /// for x in heap.into_iter() {
    ///     // x has type i32, not &i32
    ///     println!("{}", x);
    /// }
    /// ```
    fn into_iter(self) -> IntoIter<T> {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<'a, T> IntoIterator for &'a BinaryHeap<T> {
    type Item = &'a T;
    type IntoIter = Iter<'a, T>;

    fn into_iter(self) -> Iter<'a, T> {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<T: Ord> Extend<T> for BinaryHeap<T> {
    #[inline]
    fn extend<I: IntoIterator<Item = T>>(&mut self, iter: I) {
}

    #[inline]
    fn extend_one(&mut self, item: T) {
}

    #[inline]
    fn extend_reserve(&mut self, additional: usize) {
}
}

impl<T: Ord, I: IntoIterator<Item = T>> SpecExtend<I> for BinaryHeap<T> {
    default fn spec_extend(&mut self, iter: I) {
}
}

impl<T: Ord> SpecExtend<BinaryHeap<T>> for BinaryHeap<T> {
    fn spec_extend(&mut self, ref mut other: BinaryHeap<T>) {
}
}

impl<T: Ord> BinaryHeap<T> {
    fn extend_desugared<I: IntoIterator<Item = T>>(&mut self, iter: I) {
}
}

#[stable(feature = "extend_ref", since = "1.2.0")]
impl<'a, T: 'a + Ord + Copy> Extend<&'a T> for BinaryHeap<T> {
    fn extend<I: IntoIterator<Item = &'a T>>(&mut self, iter: I) {
}

    #[inline]
    fn extend_one(&mut self, &item: &'a T) {
}

    #[inline]
    fn extend_reserve(&mut self, additional: usize) {
}
}
}
#[cfg(not(no_global_oom_handling))]
mod btree {
mod append {
use super::merge_iter::MergeIterInner;
use super::node::{self, Root};
use core::iter::FusedIterator;

impl<K, V> Root<K, V> {
    /// Appends all key-value pairs from the union of two ascending iterators,
    /// incrementing a `length` variable along the way. The latter makes it
    /// easier for the caller to avoid a leak when a drop handler panicks.
    ///
    /// If both iterators produce the same key, this method drops the pair from
    /// the left iterator and appends the pair from the right iterator.
    ///
    /// If you want the tree to end up in a strictly ascending order, like for
    /// a `BTreeMap`, both iterators should produce keys in strictly ascending
    /// order, each greater than all keys in the tree, including any keys
    /// already in the tree upon entry.
    pub fn append_from_sorted_iters<I>(&mut self, left: I, right: I, length: &mut usize)
    where
        K: Ord,
        I: Iterator<Item = (K, V)> + FusedIterator,
    {
}

    /// Pushes all key-value pairs to the end of the tree, incrementing a
    /// `length` variable along the way. The latter makes it easier for the
    /// caller to avoid a leak when the iterator panicks.
    pub fn bulk_push<I>(&mut self, iter: I, length: &mut usize)
    where
        I: Iterator<Item = (K, V)>,
    {
}
}

// An iterator for merging two sorted sequences into one
struct MergeIter<K, V, I: Iterator<Item = (K, V)>>(MergeIterInner<I>);

impl<K: Ord, V, I> Iterator for MergeIter<K, V, I>
where
    I: Iterator<Item = (K, V)> + FusedIterator,
{
    type Item = (K, V);

    /// If two keys are equal, returns the key-value pair from the right source.
    fn next(&mut self) -> Option<(K, V)> {
}
}
}
mod borrow {
use core::marker::PhantomData;
use core::ptr::NonNull;

/// Models a reborrow of some unique reference, when you know that the reborrow
/// and all its descendants (i.e., all pointers and references derived from it)
/// will not be used any more at some point, after which you want to use the
/// original unique reference again.
///
/// The borrow checker usually handles this stacking of borrows for you, but
/// some control flows that accomplish this stacking are too complicated for
/// the compiler to follow. A `DormantMutRef` allows you to check borrowing
/// yourself, while still expressing its stacked nature, and encapsulating
/// the raw pointer code needed to do this without undefined behavior.
pub struct DormantMutRef<'a, T> {
    ptr: NonNull<T>,
    _marker: PhantomData<&'a mut T>,
}

unsafe impl<'a, T> Sync for DormantMutRef<'a, T> where &'a mut T: Sync {}
unsafe impl<'a, T> Send for DormantMutRef<'a, T> where &'a mut T: Send {}

impl<'a, T> DormantMutRef<'a, T> {
    /// Capture a unique borrow, and immediately reborrow it. For the compiler,
    /// the lifetime of the new reference is the same as the lifetime of the
    /// original reference, but you promise to use it for a shorter period.
    pub fn new(t: &'a mut T) -> (&'a mut T, Self) {
}

    /// Revert to the unique borrow initially captured.
    ///
    /// # Safety
    ///
    /// The reborrow must have ended, i.e., the reference returned by `new` and
    /// all pointers and references derived from it, must not be used anymore.
    pub unsafe fn awaken(self) -> &'a mut T {
}
}

#[cfg(test)]
mod tests {
}
}
mod fix {
use super::map::MIN_LEN;
use super::node::{marker, ForceResult::*, Handle, LeftOrRight::*, NodeRef, Root};

impl<'a, K: 'a, V: 'a> NodeRef<marker::Mut<'a>, K, V, marker::LeafOrInternal> {
    /// Stocks up a possibly underfull node by merging with or stealing from a
    /// sibling. If successful but at the cost of shrinking the parent node,
    /// returns that shrunk parent node. Returns an `Err` if the node is
    /// an empty root.
    fn fix_node_through_parent(
        self,
    ) -> Result<Option<NodeRef<marker::Mut<'a>, K, V, marker::Internal>>, Self> {
}
}

impl<'a, K: 'a, V: 'a> NodeRef<marker::Mut<'a>, K, V, marker::LeafOrInternal> {
    /// Stocks up a possibly underfull node, and if that causes its parent node
    /// to shrink, stocks up the parent, recursively.
    /// Returns `true` if it fixed the tree, `false` if it couldn't because the
    /// root node became empty.
    ///
    /// This method does not expect ancestors to already be underfull upon entry
    /// and panics if it encounters an empty ancestor.
    pub fn fix_node_and_affected_ancestors(mut self) -> bool {
}
}

impl<K, V> Root<K, V> {
    /// Removes empty levels on the top, but keeps an empty leaf if the entire tree is empty.
    pub fn fix_top(&mut self) {
}

    /// Stocks up or merge away any underfull nodes on the right border of the
    /// tree. The other nodes, those that are not the root nor a rightmost edge,
    /// must already have at least MIN_LEN elements.
    pub fn fix_right_border(&mut self) {
}

    /// The symmetric clone of `fix_right_border`.
    pub fn fix_left_border(&mut self) {
}

    /// Stock up any underfull nodes on the right border of the tree.
    /// The other nodes, those that are not the root nor a rightmost edge,
    /// must be prepared to have up to MIN_LEN elements stolen.
    pub fn fix_right_border_of_plentiful(&mut self) {
}
}

impl<'a, K: 'a, V: 'a> Handle<NodeRef<marker::Mut<'a>, K, V, marker::LeafOrInternal>, marker::KV> {
    fn fix_left_border_of_left_edge(mut self) {
}

    fn fix_right_border_of_right_edge(mut self) {
}
}

impl<'a, K: 'a, V: 'a> Handle<NodeRef<marker::Mut<'a>, K, V, marker::Internal>, marker::KV> {
    /// Stocks up the left child, assuming the right child isn't underfull, and
    /// provisions an extra element to allow merging its children in turn
    /// without becoming underfull.
    /// Returns the left child.
    fn fix_left_child(self) -> NodeRef<marker::Mut<'a>, K, V, marker::LeafOrInternal> {
}

    /// Stocks up the right child, assuming the left child isn't underfull, and
    /// provisions an extra element to allow merging its children in turn
    /// without becoming underfull.
    /// Returns wherever the right child ended up.
    fn fix_right_child(self) -> NodeRef<marker::Mut<'a>, K, V, marker::LeafOrInternal> {
}
}
}
pub mod map {
use core::borrow::Borrow;
use core::cmp::Ordering;
use core::fmt::{self, Debug};
use core::hash::{Hash, Hasher};
use core::iter::{FromIterator, FusedIterator};
use core::marker::PhantomData;
use core::mem::{self, ManuallyDrop};
use core::ops::{Index, RangeBounds};
use core::ptr;

use super::borrow::DormantMutRef;
use super::navigate::{LazyLeafRange, LeafRange};
use super::node::{self, marker, ForceResult::*, Handle, NodeRef, Root};
use super::search::SearchResult::*;

mod entry {
use core::fmt::{self, Debug};
use core::marker::PhantomData;
use core::mem;

use super::super::borrow::DormantMutRef;
use super::super::node::{marker, Handle, InsertResult::*, NodeRef};
use super::BTreeMap;

use Entry::*;

/// A view into a single entry in a map, which may either be vacant or occupied.
///
/// This `enum` is constructed from the [`entry`] method on [`BTreeMap`].
///
/// [`entry`]: BTreeMap::entry
#[stable(feature = "rust1", since = "1.0.0")]
#[cfg_attr(not(test), rustc_diagnostic_item = "BTreeEntry")]
pub enum Entry<'a, K: 'a, V: 'a> {
    /// A vacant entry.
    #[stable(feature = "rust1", since = "1.0.0")]
    Vacant(#[stable(feature = "rust1", since = "1.0.0")] VacantEntry<'a, K, V>),

    /// An occupied entry.
    #[stable(feature = "rust1", since = "1.0.0")]
    Occupied(#[stable(feature = "rust1", since = "1.0.0")] OccupiedEntry<'a, K, V>),
}

#[stable(feature = "debug_btree_map", since = "1.12.0")]
impl<K: Debug + Ord, V: Debug> Debug for Entry<'_, K, V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
}
}

/// A view into a vacant entry in a `BTreeMap`.
/// It is part of the [`Entry`] enum.
#[stable(feature = "rust1", since = "1.0.0")]
pub struct VacantEntry<'a, K: 'a, V: 'a> {
    pub(super) key: K,
    pub(super) handle: Handle<NodeRef<marker::Mut<'a>, K, V, marker::Leaf>, marker::Edge>,
    pub(super) dormant_map: DormantMutRef<'a, BTreeMap<K, V>>,

    // Be invariant in `K` and `V`
    pub(super) _marker: PhantomData<&'a mut (K, V)>,
}

#[stable(feature = "debug_btree_map", since = "1.12.0")]
impl<K: Debug + Ord, V> Debug for VacantEntry<'_, K, V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
}
}

/// A view into an occupied entry in a `BTreeMap`.
/// It is part of the [`Entry`] enum.
#[stable(feature = "rust1", since = "1.0.0")]
pub struct OccupiedEntry<'a, K: 'a, V: 'a> {
    pub(super) handle: Handle<NodeRef<marker::Mut<'a>, K, V, marker::LeafOrInternal>, marker::KV>,
    pub(super) dormant_map: DormantMutRef<'a, BTreeMap<K, V>>,

    // Be invariant in `K` and `V`
    pub(super) _marker: PhantomData<&'a mut (K, V)>,
}

#[stable(feature = "debug_btree_map", since = "1.12.0")]
impl<K: Debug + Ord, V: Debug> Debug for OccupiedEntry<'_, K, V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
}
}

/// The error returned by [`try_insert`](BTreeMap::try_insert) when the key already exists.
///
/// Contains the occupied entry, and the value that was not inserted.
#[unstable(feature = "map_try_insert", issue = "82766")]
pub struct OccupiedError<'a, K: 'a, V: 'a> {
    /// The entry in the map that was already occupied.
    pub entry: OccupiedEntry<'a, K, V>,
    /// The value which was not inserted, because the entry was already occupied.
    pub value: V,
}

#[unstable(feature = "map_try_insert", issue = "82766")]
impl<K: Debug + Ord, V: Debug> Debug for OccupiedError<'_, K, V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
}
}

#[unstable(feature = "map_try_insert", issue = "82766")]
impl<'a, K: Debug + Ord, V: Debug> fmt::Display for OccupiedError<'a, K, V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
}
}

impl<'a, K: Ord, V> Entry<'a, K, V> {
    /// Ensures a value is in the entry by inserting the default if empty, and returns
    /// a mutable reference to the value in the entry.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::collections::BTreeMap;
    ///
    /// let mut map: BTreeMap<&str, usize> = BTreeMap::new();
    /// map.entry("poneyland").or_insert(12);
    ///
    /// assert_eq!(map["poneyland"], 12);
    /// ```
    #[stable(feature = "rust1", since = "1.0.0")]
    pub fn or_insert(self, default: V) -> &'a mut V {
}

    /// Ensures a value is in the entry by inserting the result of the default function if empty,
    /// and returns a mutable reference to the value in the entry.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::collections::BTreeMap;
    ///
    /// let mut map: BTreeMap<&str, String> = BTreeMap::new();
    /// let s = "hoho".to_string();
    ///
    /// map.entry("poneyland").or_insert_with(|| s);
    ///
    /// assert_eq!(map["poneyland"], "hoho".to_string());
    /// ```
    #[stable(feature = "rust1", since = "1.0.0")]
    pub fn or_insert_with<F: FnOnce() -> V>(self, default: F) -> &'a mut V {
}

    /// Ensures a value is in the entry by inserting, if empty, the result of the default function.
    /// This method allows for generating key-derived values for insertion by providing the default
    /// function a reference to the key that was moved during the `.entry(key)` method call.
    ///
    /// The reference to the moved key is provided so that cloning or copying the key is
    /// unnecessary, unlike with `.or_insert_with(|| ... )`.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::collections::BTreeMap;
    ///
    /// let mut map: BTreeMap<&str, usize> = BTreeMap::new();
    ///
    /// map.entry("poneyland").or_insert_with_key(|key| key.chars().count());
    ///
    /// assert_eq!(map["poneyland"], 9);
    /// ```
    #[inline]
    #[stable(feature = "or_insert_with_key", since = "1.50.0")]
    pub fn or_insert_with_key<F: FnOnce(&K) -> V>(self, default: F) -> &'a mut V {
}

    /// Returns a reference to this entry's key.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::collections::BTreeMap;
    ///
    /// let mut map: BTreeMap<&str, usize> = BTreeMap::new();
    /// assert_eq!(map.entry("poneyland").key(), &"poneyland");
    /// ```
    #[stable(feature = "map_entry_keys", since = "1.10.0")]
    pub fn key(&self) -> &K {
}

    /// Provides in-place mutable access to an occupied entry before any
    /// potential inserts into the map.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::collections::BTreeMap;
    ///
    /// let mut map: BTreeMap<&str, usize> = BTreeMap::new();
    ///
    /// map.entry("poneyland")
    ///    .and_modify(|e| { *e += 1 })
    ///    .or_insert(42);
    /// assert_eq!(map["poneyland"], 42);
    ///
    /// map.entry("poneyland")
    ///    .and_modify(|e| { *e += 1 })
    ///    .or_insert(42);
    /// assert_eq!(map["poneyland"], 43);
    /// ```
    #[stable(feature = "entry_and_modify", since = "1.26.0")]
    pub fn and_modify<F>(self, f: F) -> Self
    where
        F: FnOnce(&mut V),
    {
}
}

impl<'a, K: Ord, V: Default> Entry<'a, K, V> {
    #[stable(feature = "entry_or_default", since = "1.28.0")]
    /// Ensures a value is in the entry by inserting the default value if empty,
    /// and returns a mutable reference to the value in the entry.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::collections::BTreeMap;
    ///
    /// let mut map: BTreeMap<&str, Option<usize>> = BTreeMap::new();
    /// map.entry("poneyland").or_default();
    ///
    /// assert_eq!(map["poneyland"], None);
    /// ```
    pub fn or_default(self) -> &'a mut V {
}
}

impl<'a, K: Ord, V> VacantEntry<'a, K, V> {
    /// Gets a reference to the key that would be used when inserting a value
    /// through the VacantEntry.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::collections::BTreeMap;
    ///
    /// let mut map: BTreeMap<&str, usize> = BTreeMap::new();
    /// assert_eq!(map.entry("poneyland").key(), &"poneyland");
    /// ```
    #[stable(feature = "map_entry_keys", since = "1.10.0")]
    pub fn key(&self) -> &K {
}

    /// Take ownership of the key.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::collections::BTreeMap;
    /// use std::collections::btree_map::Entry;
    ///
    /// let mut map: BTreeMap<&str, usize> = BTreeMap::new();
    ///
    /// if let Entry::Vacant(v) = map.entry("poneyland") {
    ///     v.into_key();
    /// }
    /// ```
    #[stable(feature = "map_entry_recover_keys2", since = "1.12.0")]
    pub fn into_key(self) -> K {
}

    /// Sets the value of the entry with the `VacantEntry`'s key,
    /// and returns a mutable reference to it.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::collections::BTreeMap;
    /// use std::collections::btree_map::Entry;
    ///
    /// let mut map: BTreeMap<&str, u32> = BTreeMap::new();
    ///
    /// if let Entry::Vacant(o) = map.entry("poneyland") {
    ///     o.insert(37);
    /// }
    /// assert_eq!(map["poneyland"], 37);
    /// ```
    #[stable(feature = "rust1", since = "1.0.0")]
    pub fn insert(self, value: V) -> &'a mut V {
}
}

impl<'a, K: Ord, V> OccupiedEntry<'a, K, V> {
    /// Gets a reference to the key in the entry.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::collections::BTreeMap;
    ///
    /// let mut map: BTreeMap<&str, usize> = BTreeMap::new();
    /// map.entry("poneyland").or_insert(12);
    /// assert_eq!(map.entry("poneyland").key(), &"poneyland");
    /// ```
    #[stable(feature = "map_entry_keys", since = "1.10.0")]
    pub fn key(&self) -> &K {
}

    /// Take ownership of the key and value from the map.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::collections::BTreeMap;
    /// use std::collections::btree_map::Entry;
    ///
    /// let mut map: BTreeMap<&str, usize> = BTreeMap::new();
    /// map.entry("poneyland").or_insert(12);
    ///
    /// if let Entry::Occupied(o) = map.entry("poneyland") {
    ///     // We delete the entry from the map.
    ///     o.remove_entry();
    /// }
    ///
    /// // If now try to get the value, it will panic:
    /// // println!("{}", map["poneyland"]);
    /// ```
    #[stable(feature = "map_entry_recover_keys2", since = "1.12.0")]
    pub fn remove_entry(self) -> (K, V) {
}

    /// Gets a reference to the value in the entry.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::collections::BTreeMap;
    /// use std::collections::btree_map::Entry;
    ///
    /// let mut map: BTreeMap<&str, usize> = BTreeMap::new();
    /// map.entry("poneyland").or_insert(12);
    ///
    /// if let Entry::Occupied(o) = map.entry("poneyland") {
    ///     assert_eq!(o.get(), &12);
    /// }
    /// ```
    #[stable(feature = "rust1", since = "1.0.0")]
    pub fn get(&self) -> &V {
}

    /// Gets a mutable reference to the value in the entry.
    ///
    /// If you need a reference to the `OccupiedEntry` that may outlive the
    /// destruction of the `Entry` value, see [`into_mut`].
    ///
    /// [`into_mut`]: OccupiedEntry::into_mut
    ///
    /// # Examples
    ///
    /// ```
    /// use std::collections::BTreeMap;
    /// use std::collections::btree_map::Entry;
    ///
    /// let mut map: BTreeMap<&str, usize> = BTreeMap::new();
    /// map.entry("poneyland").or_insert(12);
    ///
    /// assert_eq!(map["poneyland"], 12);
    /// if let Entry::Occupied(mut o) = map.entry("poneyland") {
    ///     *o.get_mut() += 10;
    ///     assert_eq!(*o.get(), 22);
    ///
    ///     // We can use the same Entry multiple times.
    ///     *o.get_mut() += 2;
    /// }
    /// assert_eq!(map["poneyland"], 24);
    /// ```
    #[stable(feature = "rust1", since = "1.0.0")]
    pub fn get_mut(&mut self) -> &mut V {
}

    /// Converts the entry into a mutable reference to its value.
    ///
    /// If you need multiple references to the `OccupiedEntry`, see [`get_mut`].
    ///
    /// [`get_mut`]: OccupiedEntry::get_mut
    ///
    /// # Examples
    ///
    /// ```
    /// use std::collections::BTreeMap;
    /// use std::collections::btree_map::Entry;
    ///
    /// let mut map: BTreeMap<&str, usize> = BTreeMap::new();
    /// map.entry("poneyland").or_insert(12);
    ///
    /// assert_eq!(map["poneyland"], 12);
    /// if let Entry::Occupied(o) = map.entry("poneyland") {
    ///     *o.into_mut() += 10;
    /// }
    /// assert_eq!(map["poneyland"], 22);
    /// ```
    #[stable(feature = "rust1", since = "1.0.0")]
    pub fn into_mut(self) -> &'a mut V {
}

    /// Sets the value of the entry with the `OccupiedEntry`'s key,
    /// and returns the entry's old value.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::collections::BTreeMap;
    /// use std::collections::btree_map::Entry;
    ///
    /// let mut map: BTreeMap<&str, usize> = BTreeMap::new();
    /// map.entry("poneyland").or_insert(12);
    ///
    /// if let Entry::Occupied(mut o) = map.entry("poneyland") {
    ///     assert_eq!(o.insert(15), 12);
    /// }
    /// assert_eq!(map["poneyland"], 15);
    /// ```
    #[stable(feature = "rust1", since = "1.0.0")]
    pub fn insert(&mut self, value: V) -> V {
}

    /// Takes the value of the entry out of the map, and returns it.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::collections::BTreeMap;
    /// use std::collections::btree_map::Entry;
    ///
    /// let mut map: BTreeMap<&str, usize> = BTreeMap::new();
    /// map.entry("poneyland").or_insert(12);
    ///
    /// if let Entry::Occupied(o) = map.entry("poneyland") {
    ///     assert_eq!(o.remove(), 12);
    /// }
    /// // If we try to get "poneyland"'s value, it'll panic:
    /// // println!("{}", map["poneyland"]);
    /// ```
    #[stable(feature = "rust1", since = "1.0.0")]
    pub fn remove(self) -> V {
}

    // Body of `remove_entry`, probably separate because the name reflects the returned pair.
    pub(super) fn remove_kv(self) -> (K, V) {
}
}
}
pub use entry::{Entry, OccupiedEntry, OccupiedError, VacantEntry};
use Entry::*;

/// Minimum number of elements in nodes that are not a root.
/// We might temporarily have fewer elements during methods.
pub(super) const MIN_LEN: usize = node::MIN_LEN_AFTER_SPLIT;

// A tree in a `BTreeMap` is a tree in the `node` module with additional invariants:
// - Keys must appear in ascending order (according to the key's type).
// - If the root node is internal, it must contain at least 1 element.
// - Every non-root node contains at least MIN_LEN elements.
//
// An empty map may be represented both by the absence of a root node or by a
// root node that is an empty leaf.

/// A map based on a [B-Tree].
///
/// B-Trees represent a fundamental compromise between cache-efficiency and actually minimizing
/// the amount of work performed in a search. In theory, a binary search tree (BST) is the optimal
/// choice for a sorted map, as a perfectly balanced BST performs the theoretical minimum amount of
/// comparisons necessary to find an element (log<sub>2</sub>n). However, in practice the way this
/// is done is *very* inefficient for modern computer architectures. In particular, every element
/// is stored in its own individually heap-allocated node. This means that every single insertion
/// triggers a heap-allocation, and every single comparison should be a cache-miss. Since these
/// are both notably expensive things to do in practice, we are forced to at very least reconsider
/// the BST strategy.
///
/// A B-Tree instead makes each node contain B-1 to 2B-1 elements in a contiguous array. By doing
/// this, we reduce the number of allocations by a factor of B, and improve cache efficiency in
/// searches. However, this does mean that searches will have to do *more* comparisons on average.
/// The precise number of comparisons depends on the node search strategy used. For optimal cache
/// efficiency, one could search the nodes linearly. For optimal comparisons, one could search
/// the node using binary search. As a compromise, one could also perform a linear search
/// that initially only checks every i<sup>th</sup> element for some choice of i.
///
/// Currently, our implementation simply performs naive linear search. This provides excellent
/// performance on *small* nodes of elements which are cheap to compare. However in the future we
/// would like to further explore choosing the optimal search strategy based on the choice of B,
/// and possibly other factors. Using linear search, searching for a random element is expected
/// to take O(B * log(n)) comparisons, which is generally worse than a BST. In practice,
/// however, performance is excellent.
///
/// It is a logic error for a key to be modified in such a way that the key's ordering relative to
/// any other key, as determined by the [`Ord`] trait, changes while it is in the map. This is
/// normally only possible through [`Cell`], [`RefCell`], global state, I/O, or unsafe code.
/// The behavior resulting from such a logic error is not specified, but will not result in
/// undefined behavior. This could include panics, incorrect results, aborts, memory leaks, and
/// non-termination.
///
/// [B-Tree]: https://en.wikipedia.org/wiki/B-tree
/// [`Cell`]: core::cell::Cell
/// [`RefCell`]: core::cell::RefCell
///
/// # Examples
///
/// ```
/// use std::collections::BTreeMap;
///
/// // type inference lets us omit an explicit type signature (which
/// // would be `BTreeMap<&str, &str>` in this example).
/// let mut movie_reviews = BTreeMap::new();
///
/// // review some movies.
/// movie_reviews.insert("Office Space",       "Deals with real issues in the workplace.");
/// movie_reviews.insert("Pulp Fiction",       "Masterpiece.");
/// movie_reviews.insert("The Godfather",      "Very enjoyable.");
/// movie_reviews.insert("The Blues Brothers", "Eye lyked it a lot.");
///
/// // check for a specific one.
/// if !movie_reviews.contains_key("Les Misérables") {
///     println!("We've got {} reviews, but Les Misérables ain't one.",
///              movie_reviews.len());
/// }
///
/// // oops, this review has a lot of spelling mistakes, let's delete it.
/// movie_reviews.remove("The Blues Brothers");
///
/// // look up the values associated with some keys.
/// let to_find = ["Up!", "Office Space"];
/// for movie in &to_find {
///     match movie_reviews.get(movie) {
///        Some(review) => println!("{}: {}", movie, review),
///        None => println!("{} is unreviewed.", movie)
///     }
/// }
///
/// // Look up the value for a key (will panic if the key is not found).
/// println!("Movie review: {}", movie_reviews["Office Space"]);
///
/// // iterate over everything.
/// for (movie, review) in &movie_reviews {
///     println!("{}: \"{}\"", movie, review);
/// }
/// ```
///
/// A `BTreeMap` with a known list of items can be initialized from an array:
///
/// ```
/// use std::collections::BTreeMap;
///
/// let solar_distance = BTreeMap::from([
///     ("Mercury", 0.4),
///     ("Venus", 0.7),
///     ("Earth", 1.0),
///     ("Mars", 1.5),
/// ]);
/// ```
///
/// `BTreeMap` implements an [`Entry API`], which allows for complex
/// methods of getting, setting, updating and removing keys and their values:
///
/// [`Entry API`]: BTreeMap::entry
///
/// ```
/// use std::collections::BTreeMap;
///
/// // type inference lets us omit an explicit type signature (which
/// // would be `BTreeMap<&str, u8>` in this example).
/// let mut player_stats = BTreeMap::new();
///
/// fn random_stat_buff() -> u8 {
///     // could actually return some random value here - let's just return
///     // some fixed value for now
///     42
/// }
///
/// // insert a key only if it doesn't already exist
/// player_stats.entry("health").or_insert(100);
///
/// // insert a key using a function that provides a new value only if it
/// // doesn't already exist
/// player_stats.entry("defence").or_insert_with(random_stat_buff);
///
/// // update a key, guarding against the key possibly not being set
/// let stat = player_stats.entry("attack").or_insert(100);
/// *stat += random_stat_buff();
/// ```
#[stable(feature = "rust1", since = "1.0.0")]
#[cfg_attr(not(test), rustc_diagnostic_item = "BTreeMap")]
#[rustc_insignificant_dtor]
pub struct BTreeMap<K, V> {
    root: Option<Root<K, V>>,
    length: usize,
}

#[stable(feature = "btree_drop", since = "1.7.0")]
unsafe impl<#[may_dangle] K, #[may_dangle] V> Drop for BTreeMap<K, V> {
    fn drop(&mut self) {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<K: Clone, V: Clone> Clone for BTreeMap<K, V> {
    fn clone(&self) -> BTreeMap<K, V> {
}
}

impl<K, Q: ?Sized> super::Recover<Q> for BTreeMap<K, ()>
where
    K: Borrow<Q> + Ord,
    Q: Ord,
{
    type Key = K;

    fn get(&self, key: &Q) -> Option<&K> {
}

    fn take(&mut self, key: &Q) -> Option<K> {
}

    fn replace(&mut self, key: K) -> Option<K> {
}
}

/// An iterator over the entries of a `BTreeMap`.
///
/// This `struct` is created by the [`iter`] method on [`BTreeMap`]. See its
/// documentation for more.
///
/// [`iter`]: BTreeMap::iter
#[stable(feature = "rust1", since = "1.0.0")]
pub struct Iter<'a, K: 'a, V: 'a> {
    range: LazyLeafRange<marker::Immut<'a>, K, V>,
    length: usize,
}

#[stable(feature = "collection_debug", since = "1.17.0")]
impl<K: fmt::Debug, V: fmt::Debug> fmt::Debug for Iter<'_, K, V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
}
}

/// A mutable iterator over the entries of a `BTreeMap`.
///
/// This `struct` is created by the [`iter_mut`] method on [`BTreeMap`]. See its
/// documentation for more.
///
/// [`iter_mut`]: BTreeMap::iter_mut
#[stable(feature = "rust1", since = "1.0.0")]
pub struct IterMut<'a, K: 'a, V: 'a> {
    range: LazyLeafRange<marker::ValMut<'a>, K, V>,
    length: usize,

    // Be invariant in `K` and `V`
    _marker: PhantomData<&'a mut (K, V)>,
}

#[stable(feature = "collection_debug", since = "1.17.0")]
impl<K: fmt::Debug, V: fmt::Debug> fmt::Debug for IterMut<'_, K, V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
}
}

/// An owning iterator over the entries of a `BTreeMap`.
///
/// This `struct` is created by the [`into_iter`] method on [`BTreeMap`]
/// (provided by the `IntoIterator` trait). See its documentation for more.
///
/// [`into_iter`]: IntoIterator::into_iter
#[stable(feature = "rust1", since = "1.0.0")]
#[rustc_insignificant_dtor]
pub struct IntoIter<K, V> {
    range: LazyLeafRange<marker::Dying, K, V>,
    length: usize,
}

impl<K, V> IntoIter<K, V> {
    /// Returns an iterator of references over the remaining items.
    #[inline]
    pub(super) fn iter(&self) -> Iter<'_, K, V> {
}
}

#[stable(feature = "collection_debug", since = "1.17.0")]
impl<K: fmt::Debug, V: fmt::Debug> fmt::Debug for IntoIter<K, V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
}
}

/// An iterator over the keys of a `BTreeMap`.
///
/// This `struct` is created by the [`keys`] method on [`BTreeMap`]. See its
/// documentation for more.
///
/// [`keys`]: BTreeMap::keys
#[stable(feature = "rust1", since = "1.0.0")]
pub struct Keys<'a, K: 'a, V: 'a> {
    inner: Iter<'a, K, V>,
}

#[stable(feature = "collection_debug", since = "1.17.0")]
impl<K: fmt::Debug, V> fmt::Debug for Keys<'_, K, V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
}
}

/// An iterator over the values of a `BTreeMap`.
///
/// This `struct` is created by the [`values`] method on [`BTreeMap`]. See its
/// documentation for more.
///
/// [`values`]: BTreeMap::values
#[stable(feature = "rust1", since = "1.0.0")]
pub struct Values<'a, K: 'a, V: 'a> {
    inner: Iter<'a, K, V>,
}

#[stable(feature = "collection_debug", since = "1.17.0")]
impl<K, V: fmt::Debug> fmt::Debug for Values<'_, K, V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
}
}

/// A mutable iterator over the values of a `BTreeMap`.
///
/// This `struct` is created by the [`values_mut`] method on [`BTreeMap`]. See its
/// documentation for more.
///
/// [`values_mut`]: BTreeMap::values_mut
#[stable(feature = "map_values_mut", since = "1.10.0")]
pub struct ValuesMut<'a, K: 'a, V: 'a> {
    inner: IterMut<'a, K, V>,
}

#[stable(feature = "map_values_mut", since = "1.10.0")]
impl<K, V: fmt::Debug> fmt::Debug for ValuesMut<'_, K, V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
}
}

/// An owning iterator over the keys of a `BTreeMap`.
///
/// This `struct` is created by the [`into_keys`] method on [`BTreeMap`].
/// See its documentation for more.
///
/// [`into_keys`]: BTreeMap::into_keys
#[stable(feature = "map_into_keys_values", since = "1.54.0")]
pub struct IntoKeys<K, V> {
    inner: IntoIter<K, V>,
}

#[stable(feature = "map_into_keys_values", since = "1.54.0")]
impl<K: fmt::Debug, V> fmt::Debug for IntoKeys<K, V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
}
}

/// An owning iterator over the values of a `BTreeMap`.
///
/// This `struct` is created by the [`into_values`] method on [`BTreeMap`].
/// See its documentation for more.
///
/// [`into_values`]: BTreeMap::into_values
#[stable(feature = "map_into_keys_values", since = "1.54.0")]
pub struct IntoValues<K, V> {
    inner: IntoIter<K, V>,
}

#[stable(feature = "map_into_keys_values", since = "1.54.0")]
impl<K, V: fmt::Debug> fmt::Debug for IntoValues<K, V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
}
}

/// An iterator over a sub-range of entries in a `BTreeMap`.
///
/// This `struct` is created by the [`range`] method on [`BTreeMap`]. See its
/// documentation for more.
///
/// [`range`]: BTreeMap::range
#[stable(feature = "btree_range", since = "1.17.0")]
pub struct Range<'a, K: 'a, V: 'a> {
    inner: LeafRange<marker::Immut<'a>, K, V>,
}

#[stable(feature = "collection_debug", since = "1.17.0")]
impl<K: fmt::Debug, V: fmt::Debug> fmt::Debug for Range<'_, K, V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
}
}

/// A mutable iterator over a sub-range of entries in a `BTreeMap`.
///
/// This `struct` is created by the [`range_mut`] method on [`BTreeMap`]. See its
/// documentation for more.
///
/// [`range_mut`]: BTreeMap::range_mut
#[stable(feature = "btree_range", since = "1.17.0")]
pub struct RangeMut<'a, K: 'a, V: 'a> {
    inner: LeafRange<marker::ValMut<'a>, K, V>,

    // Be invariant in `K` and `V`
    _marker: PhantomData<&'a mut (K, V)>,
}

#[stable(feature = "collection_debug", since = "1.17.0")]
impl<K: fmt::Debug, V: fmt::Debug> fmt::Debug for RangeMut<'_, K, V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
}
}

impl<K, V> BTreeMap<K, V> {
    /// Makes a new, empty `BTreeMap`.
    ///
    /// Does not allocate anything on its own.
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// use std::collections::BTreeMap;
    ///
    /// let mut map = BTreeMap::new();
    ///
    /// // entries can now be inserted into the empty map
    /// map.insert(1, "a");
    /// ```
    #[stable(feature = "rust1", since = "1.0.0")]
    #[rustc_const_unstable(feature = "const_btree_new", issue = "71835")]
    pub const fn new() -> BTreeMap<K, V> {
}

    /// Clears the map, removing all elements.
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// use std::collections::BTreeMap;
    ///
    /// let mut a = BTreeMap::new();
    /// a.insert(1, "a");
    /// a.clear();
    /// assert!(a.is_empty());
    /// ```
    #[stable(feature = "rust1", since = "1.0.0")]
    pub fn clear(&mut self) {
}

    /// Returns a reference to the value corresponding to the key.
    ///
    /// The key may be any borrowed form of the map's key type, but the ordering
    /// on the borrowed form *must* match the ordering on the key type.
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// use std::collections::BTreeMap;
    ///
    /// let mut map = BTreeMap::new();
    /// map.insert(1, "a");
    /// assert_eq!(map.get(&1), Some(&"a"));
    /// assert_eq!(map.get(&2), None);
    /// ```
    #[stable(feature = "rust1", since = "1.0.0")]
    pub fn get<Q: ?Sized>(&self, key: &Q) -> Option<&V>
    where
        K: Borrow<Q> + Ord,
        Q: Ord,
    {
}

    /// Returns the key-value pair corresponding to the supplied key.
    ///
    /// The supplied key may be any borrowed form of the map's key type, but the ordering
    /// on the borrowed form *must* match the ordering on the key type.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::collections::BTreeMap;
    ///
    /// let mut map = BTreeMap::new();
    /// map.insert(1, "a");
    /// assert_eq!(map.get_key_value(&1), Some((&1, &"a")));
    /// assert_eq!(map.get_key_value(&2), None);
    /// ```
    #[stable(feature = "map_get_key_value", since = "1.40.0")]
    pub fn get_key_value<Q: ?Sized>(&self, k: &Q) -> Option<(&K, &V)>
    where
        K: Borrow<Q> + Ord,
        Q: Ord,
    {
}

    /// Returns the first key-value pair in the map.
    /// The key in this pair is the minimum key in the map.
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// #![feature(map_first_last)]
    /// use std::collections::BTreeMap;
    ///
    /// let mut map = BTreeMap::new();
    /// assert_eq!(map.first_key_value(), None);
    /// map.insert(1, "b");
    /// map.insert(2, "a");
    /// assert_eq!(map.first_key_value(), Some((&1, &"b")));
    /// ```
    #[unstable(feature = "map_first_last", issue = "62924")]
    pub fn first_key_value(&self) -> Option<(&K, &V)>
    where
        K: Ord,
    {
}

    /// Returns the first entry in the map for in-place manipulation.
    /// The key of this entry is the minimum key in the map.
    ///
    /// # Examples
    ///
    /// ```
    /// #![feature(map_first_last)]
    /// use std::collections::BTreeMap;
    ///
    /// let mut map = BTreeMap::new();
    /// map.insert(1, "a");
    /// map.insert(2, "b");
    /// if let Some(mut entry) = map.first_entry() {
    ///     if *entry.key() > 0 {
    ///         entry.insert("first");
    ///     }
    /// }
    /// assert_eq!(*map.get(&1).unwrap(), "first");
    /// assert_eq!(*map.get(&2).unwrap(), "b");
    /// ```
    #[unstable(feature = "map_first_last", issue = "62924")]
    pub fn first_entry(&mut self) -> Option<OccupiedEntry<'_, K, V>>
    where
        K: Ord,
    {
}

    /// Removes and returns the first element in the map.
    /// The key of this element is the minimum key that was in the map.
    ///
    /// # Examples
    ///
    /// Draining elements in ascending order, while keeping a usable map each iteration.
    ///
    /// ```
    /// #![feature(map_first_last)]
    /// use std::collections::BTreeMap;
    ///
    /// let mut map = BTreeMap::new();
    /// map.insert(1, "a");
    /// map.insert(2, "b");
    /// while let Some((key, _val)) = map.pop_first() {
    ///     assert!(map.iter().all(|(k, _v)| *k > key));
    /// }
    /// assert!(map.is_empty());
    /// ```
    #[unstable(feature = "map_first_last", issue = "62924")]
    pub fn pop_first(&mut self) -> Option<(K, V)>
    where
        K: Ord,
    {
}

    /// Returns the last key-value pair in the map.
    /// The key in this pair is the maximum key in the map.
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// #![feature(map_first_last)]
    /// use std::collections::BTreeMap;
    ///
    /// let mut map = BTreeMap::new();
    /// map.insert(1, "b");
    /// map.insert(2, "a");
    /// assert_eq!(map.last_key_value(), Some((&2, &"a")));
    /// ```
    #[unstable(feature = "map_first_last", issue = "62924")]
    pub fn last_key_value(&self) -> Option<(&K, &V)>
    where
        K: Ord,
    {
}

    /// Returns the last entry in the map for in-place manipulation.
    /// The key of this entry is the maximum key in the map.
    ///
    /// # Examples
    ///
    /// ```
    /// #![feature(map_first_last)]
    /// use std::collections::BTreeMap;
    ///
    /// let mut map = BTreeMap::new();
    /// map.insert(1, "a");
    /// map.insert(2, "b");
    /// if let Some(mut entry) = map.last_entry() {
    ///     if *entry.key() > 0 {
    ///         entry.insert("last");
    ///     }
    /// }
    /// assert_eq!(*map.get(&1).unwrap(), "a");
    /// assert_eq!(*map.get(&2).unwrap(), "last");
    /// ```
    #[unstable(feature = "map_first_last", issue = "62924")]
    pub fn last_entry(&mut self) -> Option<OccupiedEntry<'_, K, V>>
    where
        K: Ord,
    {
}

    /// Removes and returns the last element in the map.
    /// The key of this element is the maximum key that was in the map.
    ///
    /// # Examples
    ///
    /// Draining elements in descending order, while keeping a usable map each iteration.
    ///
    /// ```
    /// #![feature(map_first_last)]
    /// use std::collections::BTreeMap;
    ///
    /// let mut map = BTreeMap::new();
    /// map.insert(1, "a");
    /// map.insert(2, "b");
    /// while let Some((key, _val)) = map.pop_last() {
    ///     assert!(map.iter().all(|(k, _v)| *k < key));
    /// }
    /// assert!(map.is_empty());
    /// ```
    #[unstable(feature = "map_first_last", issue = "62924")]
    pub fn pop_last(&mut self) -> Option<(K, V)>
    where
        K: Ord,
    {
}

    /// Returns `true` if the map contains a value for the specified key.
    ///
    /// The key may be any borrowed form of the map's key type, but the ordering
    /// on the borrowed form *must* match the ordering on the key type.
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// use std::collections::BTreeMap;
    ///
    /// let mut map = BTreeMap::new();
    /// map.insert(1, "a");
    /// assert_eq!(map.contains_key(&1), true);
    /// assert_eq!(map.contains_key(&2), false);
    /// ```
    #[stable(feature = "rust1", since = "1.0.0")]
    pub fn contains_key<Q: ?Sized>(&self, key: &Q) -> bool
    where
        K: Borrow<Q> + Ord,
        Q: Ord,
    {
}

    /// Returns a mutable reference to the value corresponding to the key.
    ///
    /// The key may be any borrowed form of the map's key type, but the ordering
    /// on the borrowed form *must* match the ordering on the key type.
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// use std::collections::BTreeMap;
    ///
    /// let mut map = BTreeMap::new();
    /// map.insert(1, "a");
    /// if let Some(x) = map.get_mut(&1) {
    ///     *x = "b";
    /// }
    /// assert_eq!(map[&1], "b");
    /// ```
    // See `get` for implementation notes, this is basically a copy-paste with mut's added
    #[stable(feature = "rust1", since = "1.0.0")]
    pub fn get_mut<Q: ?Sized>(&mut self, key: &Q) -> Option<&mut V>
    where
        K: Borrow<Q> + Ord,
        Q: Ord,
    {
}

    /// Inserts a key-value pair into the map.
    ///
    /// If the map did not have this key present, `None` is returned.
    ///
    /// If the map did have this key present, the value is updated, and the old
    /// value is returned. The key is not updated, though; this matters for
    /// types that can be `==` without being identical. See the [module-level
    /// documentation] for more.
    ///
    /// [module-level documentation]: index.html#insert-and-complex-keys
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// use std::collections::BTreeMap;
    ///
    /// let mut map = BTreeMap::new();
    /// assert_eq!(map.insert(37, "a"), None);
    /// assert_eq!(map.is_empty(), false);
    ///
    /// map.insert(37, "b");
    /// assert_eq!(map.insert(37, "c"), Some("b"));
    /// assert_eq!(map[&37], "c");
    /// ```
    #[stable(feature = "rust1", since = "1.0.0")]
    pub fn insert(&mut self, key: K, value: V) -> Option<V>
    where
        K: Ord,
    {
}

    /// Tries to insert a key-value pair into the map, and returns
    /// a mutable reference to the value in the entry.
    ///
    /// If the map already had this key present, nothing is updated, and
    /// an error containing the occupied entry and the value is returned.
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// #![feature(map_try_insert)]
    ///
    /// use std::collections::BTreeMap;
    ///
    /// let mut map = BTreeMap::new();
    /// assert_eq!(map.try_insert(37, "a").unwrap(), &"a");
    ///
    /// let err = map.try_insert(37, "b").unwrap_err();
    /// assert_eq!(err.entry.key(), &37);
    /// assert_eq!(err.entry.get(), &"a");
    /// assert_eq!(err.value, "b");
    /// ```
    #[unstable(feature = "map_try_insert", issue = "82766")]
    pub fn try_insert(&mut self, key: K, value: V) -> Result<&mut V, OccupiedError<'_, K, V>>
    where
        K: Ord,
    {
}

    /// Removes a key from the map, returning the value at the key if the key
    /// was previously in the map.
    ///
    /// The key may be any borrowed form of the map's key type, but the ordering
    /// on the borrowed form *must* match the ordering on the key type.
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// use std::collections::BTreeMap;
    ///
    /// let mut map = BTreeMap::new();
    /// map.insert(1, "a");
    /// assert_eq!(map.remove(&1), Some("a"));
    /// assert_eq!(map.remove(&1), None);
    /// ```
    #[stable(feature = "rust1", since = "1.0.0")]
    pub fn remove<Q: ?Sized>(&mut self, key: &Q) -> Option<V>
    where
        K: Borrow<Q> + Ord,
        Q: Ord,
    {
}

    /// Removes a key from the map, returning the stored key and value if the key
    /// was previously in the map.
    ///
    /// The key may be any borrowed form of the map's key type, but the ordering
    /// on the borrowed form *must* match the ordering on the key type.
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// use std::collections::BTreeMap;
    ///
    /// let mut map = BTreeMap::new();
    /// map.insert(1, "a");
    /// assert_eq!(map.remove_entry(&1), Some((1, "a")));
    /// assert_eq!(map.remove_entry(&1), None);
    /// ```
    #[stable(feature = "btreemap_remove_entry", since = "1.45.0")]
    pub fn remove_entry<Q: ?Sized>(&mut self, key: &Q) -> Option<(K, V)>
    where
        K: Borrow<Q> + Ord,
        Q: Ord,
    {
}

    /// Retains only the elements specified by the predicate.
    ///
    /// In other words, remove all pairs `(k, v)` such that `f(&k, &mut v)` returns `false`.
    /// The elements are visited in ascending key order.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::collections::BTreeMap;
    ///
    /// let mut map: BTreeMap<i32, i32> = (0..8).map(|x| (x, x*10)).collect();
    /// // Keep only the elements with even-numbered keys.
    /// map.retain(|&k, _| k % 2 == 0);
    /// assert!(map.into_iter().eq(vec![(0, 0), (2, 20), (4, 40), (6, 60)]));
    /// ```
    #[inline]
    #[stable(feature = "btree_retain", since = "1.53.0")]
    pub fn retain<F>(&mut self, mut f: F)
    where
        K: Ord,
        F: FnMut(&K, &mut V) -> bool,
    {
}

    /// Moves all elements from `other` into `Self`, leaving `other` empty.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::collections::BTreeMap;
    ///
    /// let mut a = BTreeMap::new();
    /// a.insert(1, "a");
    /// a.insert(2, "b");
    /// a.insert(3, "c");
    ///
    /// let mut b = BTreeMap::new();
    /// b.insert(3, "d");
    /// b.insert(4, "e");
    /// b.insert(5, "f");
    ///
    /// a.append(&mut b);
    ///
    /// assert_eq!(a.len(), 5);
    /// assert_eq!(b.len(), 0);
    ///
    /// assert_eq!(a[&1], "a");
    /// assert_eq!(a[&2], "b");
    /// assert_eq!(a[&3], "d");
    /// assert_eq!(a[&4], "e");
    /// assert_eq!(a[&5], "f");
    /// ```
    #[stable(feature = "btree_append", since = "1.11.0")]
    pub fn append(&mut self, other: &mut Self)
    where
        K: Ord,
    {
}

    /// Constructs a double-ended iterator over a sub-range of elements in the map.
    /// The simplest way is to use the range syntax `min..max`, thus `range(min..max)` will
    /// yield elements from min (inclusive) to max (exclusive).
    /// The range may also be entered as `(Bound<T>, Bound<T>)`, so for example
    /// `range((Excluded(4), Included(10)))` will yield a left-exclusive, right-inclusive
    /// range from 4 to 10.
    ///
    /// # Panics
    ///
    /// Panics if range `start > end`.
    /// Panics if range `start == end` and both bounds are `Excluded`.
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// use std::collections::BTreeMap;
    /// use std::ops::Bound::Included;
    ///
    /// let mut map = BTreeMap::new();
    /// map.insert(3, "a");
    /// map.insert(5, "b");
    /// map.insert(8, "c");
    /// for (&key, &value) in map.range((Included(&4), Included(&8))) {
    ///     println!("{}: {}", key, value);
    /// }
    /// assert_eq!(Some((&5, &"b")), map.range(4..).next());
    /// ```
    #[stable(feature = "btree_range", since = "1.17.0")]
    pub fn range<T: ?Sized, R>(&self, range: R) -> Range<'_, K, V>
    where
        T: Ord,
        K: Borrow<T> + Ord,
        R: RangeBounds<T>,
    {
}

    /// Constructs a mutable double-ended iterator over a sub-range of elements in the map.
    /// The simplest way is to use the range syntax `min..max`, thus `range(min..max)` will
    /// yield elements from min (inclusive) to max (exclusive).
    /// The range may also be entered as `(Bound<T>, Bound<T>)`, so for example
    /// `range((Excluded(4), Included(10)))` will yield a left-exclusive, right-inclusive
    /// range from 4 to 10.
    ///
    /// # Panics
    ///
    /// Panics if range `start > end`.
    /// Panics if range `start == end` and both bounds are `Excluded`.
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// use std::collections::BTreeMap;
    ///
    /// let mut map: BTreeMap<&str, i32> = ["Alice", "Bob", "Carol", "Cheryl"]
    ///     .iter()
    ///     .map(|&s| (s, 0))
    ///     .collect();
    /// for (_, balance) in map.range_mut("B".."Cheryl") {
    ///     *balance += 100;
    /// }
    /// for (name, balance) in &map {
    ///     println!("{} => {}", name, balance);
    /// }
    /// ```
    #[stable(feature = "btree_range", since = "1.17.0")]
    pub fn range_mut<T: ?Sized, R>(&mut self, range: R) -> RangeMut<'_, K, V>
    where
        T: Ord,
        K: Borrow<T> + Ord,
        R: RangeBounds<T>,
    {
}

    /// Gets the given key's corresponding entry in the map for in-place manipulation.
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// use std::collections::BTreeMap;
    ///
    /// let mut count: BTreeMap<&str, usize> = BTreeMap::new();
    ///
    /// // count the number of occurrences of letters in the vec
    /// for x in vec!["a", "b", "a", "c", "a", "b"] {
    ///     *count.entry(x).or_insert(0) += 1;
    /// }
    ///
    /// assert_eq!(count["a"], 3);
    /// ```
    #[stable(feature = "rust1", since = "1.0.0")]
    pub fn entry(&mut self, key: K) -> Entry<'_, K, V>
    where
        K: Ord,
    {
}

    /// Splits the collection into two at the given key. Returns everything after the given key,
    /// including the key.
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// use std::collections::BTreeMap;
    ///
    /// let mut a = BTreeMap::new();
    /// a.insert(1, "a");
    /// a.insert(2, "b");
    /// a.insert(3, "c");
    /// a.insert(17, "d");
    /// a.insert(41, "e");
    ///
    /// let b = a.split_off(&3);
    ///
    /// assert_eq!(a.len(), 2);
    /// assert_eq!(b.len(), 3);
    ///
    /// assert_eq!(a[&1], "a");
    /// assert_eq!(a[&2], "b");
    ///
    /// assert_eq!(b[&3], "c");
    /// assert_eq!(b[&17], "d");
    /// assert_eq!(b[&41], "e");
    /// ```
    #[stable(feature = "btree_split_off", since = "1.11.0")]
    pub fn split_off<Q: ?Sized + Ord>(&mut self, key: &Q) -> Self
    where
        K: Borrow<Q> + Ord,
    {
}

    /// Creates an iterator that visits all elements (key-value pairs) in
    /// ascending key order and uses a closure to determine if an element should
    /// be removed. If the closure returns `true`, the element is removed from
    /// the map and yielded. If the closure returns `false`, or panics, the
    /// element remains in the map and will not be yielded.
    ///
    /// The iterator also lets you mutate the value of each element in the
    /// closure, regardless of whether you choose to keep or remove it.
    ///
    /// If the iterator is only partially consumed or not consumed at all, each
    /// of the remaining elements is still subjected to the closure, which may
    /// change its value and, by returning `true`, have the element removed and
    /// dropped.
    ///
    /// It is unspecified how many more elements will be subjected to the
    /// closure if a panic occurs in the closure, or a panic occurs while
    /// dropping an element, or if the `DrainFilter` value is leaked.
    ///
    /// # Examples
    ///
    /// Splitting a map into even and odd keys, reusing the original map:
    ///
    /// ```
    /// #![feature(btree_drain_filter)]
    /// use std::collections::BTreeMap;
    ///
    /// let mut map: BTreeMap<i32, i32> = (0..8).map(|x| (x, x)).collect();
    /// let evens: BTreeMap<_, _> = map.drain_filter(|k, _v| k % 2 == 0).collect();
    /// let odds = map;
    /// assert_eq!(evens.keys().copied().collect::<Vec<_>>(), vec![0, 2, 4, 6]);
    /// assert_eq!(odds.keys().copied().collect::<Vec<_>>(), vec![1, 3, 5, 7]);
    /// ```
    #[unstable(feature = "btree_drain_filter", issue = "70530")]
    pub fn drain_filter<F>(&mut self, pred: F) -> DrainFilter<'_, K, V, F>
    where
        K: Ord,
        F: FnMut(&K, &mut V) -> bool,
    {
}

    pub(super) fn drain_filter_inner(&mut self) -> DrainFilterInner<'_, K, V>
    where
        K: Ord,
    {
}

    /// Creates a consuming iterator visiting all the keys, in sorted order.
    /// The map cannot be used after calling this.
    /// The iterator element type is `K`.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::collections::BTreeMap;
    ///
    /// let mut a = BTreeMap::new();
    /// a.insert(2, "b");
    /// a.insert(1, "a");
    ///
    /// let keys: Vec<i32> = a.into_keys().collect();
    /// assert_eq!(keys, [1, 2]);
    /// ```
    #[inline]
    #[stable(feature = "map_into_keys_values", since = "1.54.0")]
    pub fn into_keys(self) -> IntoKeys<K, V> {
}

    /// Creates a consuming iterator visiting all the values, in order by key.
    /// The map cannot be used after calling this.
    /// The iterator element type is `V`.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::collections::BTreeMap;
    ///
    /// let mut a = BTreeMap::new();
    /// a.insert(1, "hello");
    /// a.insert(2, "goodbye");
    ///
    /// let values: Vec<&str> = a.into_values().collect();
    /// assert_eq!(values, ["hello", "goodbye"]);
    /// ```
    #[inline]
    #[stable(feature = "map_into_keys_values", since = "1.54.0")]
    pub fn into_values(self) -> IntoValues<K, V> {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<'a, K, V> IntoIterator for &'a BTreeMap<K, V> {
    type Item = (&'a K, &'a V);
    type IntoIter = Iter<'a, K, V>;

    fn into_iter(self) -> Iter<'a, K, V> {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<'a, K: 'a, V: 'a> Iterator for Iter<'a, K, V> {
    type Item = (&'a K, &'a V);

    fn next(&mut self) -> Option<(&'a K, &'a V)> {
}

    fn size_hint(&self) -> (usize, Option<usize>) {
}

    fn last(mut self) -> Option<(&'a K, &'a V)> {
}

    fn min(mut self) -> Option<(&'a K, &'a V)> {
}

    fn max(mut self) -> Option<(&'a K, &'a V)> {
}
}

#[stable(feature = "fused", since = "1.26.0")]
impl<K, V> FusedIterator for Iter<'_, K, V> {}

#[stable(feature = "rust1", since = "1.0.0")]
impl<'a, K: 'a, V: 'a> DoubleEndedIterator for Iter<'a, K, V> {
    fn next_back(&mut self) -> Option<(&'a K, &'a V)> {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<K, V> ExactSizeIterator for Iter<'_, K, V> {
    fn len(&self) -> usize {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<K, V> Clone for Iter<'_, K, V> {
    fn clone(&self) -> Self {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<'a, K, V> IntoIterator for &'a mut BTreeMap<K, V> {
    type Item = (&'a K, &'a mut V);
    type IntoIter = IterMut<'a, K, V>;

    fn into_iter(self) -> IterMut<'a, K, V> {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<'a, K: 'a, V: 'a> Iterator for IterMut<'a, K, V> {
    type Item = (&'a K, &'a mut V);

    fn next(&mut self) -> Option<(&'a K, &'a mut V)> {
}

    fn size_hint(&self) -> (usize, Option<usize>) {
}

    fn last(mut self) -> Option<(&'a K, &'a mut V)> {
}

    fn min(mut self) -> Option<(&'a K, &'a mut V)> {
}

    fn max(mut self) -> Option<(&'a K, &'a mut V)> {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<'a, K: 'a, V: 'a> DoubleEndedIterator for IterMut<'a, K, V> {
    fn next_back(&mut self) -> Option<(&'a K, &'a mut V)> {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<K, V> ExactSizeIterator for IterMut<'_, K, V> {
    fn len(&self) -> usize {
}
}

#[stable(feature = "fused", since = "1.26.0")]
impl<K, V> FusedIterator for IterMut<'_, K, V> {}

impl<'a, K, V> IterMut<'a, K, V> {
    /// Returns an iterator of references over the remaining items.
    #[inline]
    pub(super) fn iter(&self) -> Iter<'_, K, V> {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<K, V> IntoIterator for BTreeMap<K, V> {
    type Item = (K, V);
    type IntoIter = IntoIter<K, V>;

    fn into_iter(self) -> IntoIter<K, V> {
}
}

#[stable(feature = "btree_drop", since = "1.7.0")]
impl<K, V> Drop for IntoIter<K, V> {
    fn drop(&mut self) {
}
}

impl<K, V> IntoIter<K, V> {
    /// Core of a `next` method returning a dying KV handle,
    /// invalidated by further calls to this function and some others.
    fn dying_next(
        &mut self,
    ) -> Option<Handle<NodeRef<marker::Dying, K, V, marker::LeafOrInternal>, marker::KV>> {
}

    /// Core of a `next_back` method returning a dying KV handle,
    /// invalidated by further calls to this function and some others.
    fn dying_next_back(
        &mut self,
    ) -> Option<Handle<NodeRef<marker::Dying, K, V, marker::LeafOrInternal>, marker::KV>> {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<K, V> Iterator for IntoIter<K, V> {
    type Item = (K, V);

    fn next(&mut self) -> Option<(K, V)> {
}

    fn size_hint(&self) -> (usize, Option<usize>) {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<K, V> DoubleEndedIterator for IntoIter<K, V> {
    fn next_back(&mut self) -> Option<(K, V)> {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<K, V> ExactSizeIterator for IntoIter<K, V> {
    fn len(&self) -> usize {
}
}

#[stable(feature = "fused", since = "1.26.0")]
impl<K, V> FusedIterator for IntoIter<K, V> {}

#[stable(feature = "rust1", since = "1.0.0")]
impl<'a, K, V> Iterator for Keys<'a, K, V> {
    type Item = &'a K;

    fn next(&mut self) -> Option<&'a K> {
}

    fn size_hint(&self) -> (usize, Option<usize>) {
}

    fn last(mut self) -> Option<&'a K> {
}

    fn min(mut self) -> Option<&'a K> {
}

    fn max(mut self) -> Option<&'a K> {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<'a, K, V> DoubleEndedIterator for Keys<'a, K, V> {
    fn next_back(&mut self) -> Option<&'a K> {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<K, V> ExactSizeIterator for Keys<'_, K, V> {
    fn len(&self) -> usize {
}
}

#[stable(feature = "fused", since = "1.26.0")]
impl<K, V> FusedIterator for Keys<'_, K, V> {}

#[stable(feature = "rust1", since = "1.0.0")]
impl<K, V> Clone for Keys<'_, K, V> {
    fn clone(&self) -> Self {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<'a, K, V> Iterator for Values<'a, K, V> {
    type Item = &'a V;

    fn next(&mut self) -> Option<&'a V> {
}

    fn size_hint(&self) -> (usize, Option<usize>) {
}

    fn last(mut self) -> Option<&'a V> {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<'a, K, V> DoubleEndedIterator for Values<'a, K, V> {
    fn next_back(&mut self) -> Option<&'a V> {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<K, V> ExactSizeIterator for Values<'_, K, V> {
    fn len(&self) -> usize {
}
}

#[stable(feature = "fused", since = "1.26.0")]
impl<K, V> FusedIterator for Values<'_, K, V> {}

#[stable(feature = "rust1", since = "1.0.0")]
impl<K, V> Clone for Values<'_, K, V> {
    fn clone(&self) -> Self {
}
}

/// An iterator produced by calling `drain_filter` on BTreeMap.
#[unstable(feature = "btree_drain_filter", issue = "70530")]
pub struct DrainFilter<'a, K, V, F>
where
    K: 'a,
    V: 'a,
    F: 'a + FnMut(&K, &mut V) -> bool,
{
    pred: F,
    inner: DrainFilterInner<'a, K, V>,
}
/// Most of the implementation of DrainFilter are generic over the type
/// of the predicate, thus also serving for BTreeSet::DrainFilter.
pub(super) struct DrainFilterInner<'a, K: 'a, V: 'a> {
    /// Reference to the length field in the borrowed map, updated live.
    length: &'a mut usize,
    /// Buried reference to the root field in the borrowed map.
    /// Wrapped in `Option` to allow drop handler to `take` it.
    dormant_root: Option<DormantMutRef<'a, Root<K, V>>>,
    /// Contains a leaf edge preceding the next element to be returned, or the last leaf edge.
    /// Empty if the map has no root, if iteration went beyond the last leaf edge,
    /// or if a panic occurred in the predicate.
    cur_leaf_edge: Option<Handle<NodeRef<marker::Mut<'a>, K, V, marker::Leaf>, marker::Edge>>,
}

#[unstable(feature = "btree_drain_filter", issue = "70530")]
impl<K, V, F> Drop for DrainFilter<'_, K, V, F>
where
    F: FnMut(&K, &mut V) -> bool,
{
    fn drop(&mut self) {
}
}

#[unstable(feature = "btree_drain_filter", issue = "70530")]
impl<K, V, F> fmt::Debug for DrainFilter<'_, K, V, F>
where
    K: fmt::Debug,
    V: fmt::Debug,
    F: FnMut(&K, &mut V) -> bool,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
}
}

#[unstable(feature = "btree_drain_filter", issue = "70530")]
impl<K, V, F> Iterator for DrainFilter<'_, K, V, F>
where
    F: FnMut(&K, &mut V) -> bool,
{
    type Item = (K, V);

    fn next(&mut self) -> Option<(K, V)> {
}

    fn size_hint(&self) -> (usize, Option<usize>) {
}
}

impl<'a, K: 'a, V: 'a> DrainFilterInner<'a, K, V> {
    /// Allow Debug implementations to predict the next element.
    pub(super) fn peek(&self) -> Option<(&K, &V)> {
}

    /// Implementation of a typical `DrainFilter::next` method, given the predicate.
    pub(super) fn next<F>(&mut self, pred: &mut F) -> Option<(K, V)>
    where
        F: FnMut(&K, &mut V) -> bool,
    {
}

    /// Implementation of a typical `DrainFilter::size_hint` method.
    pub(super) fn size_hint(&self) -> (usize, Option<usize>) {
}
}

#[unstable(feature = "btree_drain_filter", issue = "70530")]
impl<K, V, F> FusedIterator for DrainFilter<'_, K, V, F> where F: FnMut(&K, &mut V) -> bool {}

#[stable(feature = "btree_range", since = "1.17.0")]
impl<'a, K, V> Iterator for Range<'a, K, V> {
    type Item = (&'a K, &'a V);

    fn next(&mut self) -> Option<(&'a K, &'a V)> {
}

    fn last(mut self) -> Option<(&'a K, &'a V)> {
}

    fn min(mut self) -> Option<(&'a K, &'a V)> {
}

    fn max(mut self) -> Option<(&'a K, &'a V)> {
}
}

#[stable(feature = "map_values_mut", since = "1.10.0")]
impl<'a, K, V> Iterator for ValuesMut<'a, K, V> {
    type Item = &'a mut V;

    fn next(&mut self) -> Option<&'a mut V> {
}

    fn size_hint(&self) -> (usize, Option<usize>) {
}

    fn last(mut self) -> Option<&'a mut V> {
}
}

#[stable(feature = "map_values_mut", since = "1.10.0")]
impl<'a, K, V> DoubleEndedIterator for ValuesMut<'a, K, V> {
    fn next_back(&mut self) -> Option<&'a mut V> {
}
}

#[stable(feature = "map_values_mut", since = "1.10.0")]
impl<K, V> ExactSizeIterator for ValuesMut<'_, K, V> {
    fn len(&self) -> usize {
}
}

#[stable(feature = "fused", since = "1.26.0")]
impl<K, V> FusedIterator for ValuesMut<'_, K, V> {}

#[stable(feature = "map_into_keys_values", since = "1.54.0")]
impl<K, V> Iterator for IntoKeys<K, V> {
    type Item = K;

    fn next(&mut self) -> Option<K> {
}

    fn size_hint(&self) -> (usize, Option<usize>) {
}

    fn last(mut self) -> Option<K> {
}

    fn min(mut self) -> Option<K> {
}

    fn max(mut self) -> Option<K> {
}
}

#[stable(feature = "map_into_keys_values", since = "1.54.0")]
impl<K, V> DoubleEndedIterator for IntoKeys<K, V> {
    fn next_back(&mut self) -> Option<K> {
}
}

#[stable(feature = "map_into_keys_values", since = "1.54.0")]
impl<K, V> ExactSizeIterator for IntoKeys<K, V> {
    fn len(&self) -> usize {
}
}

#[stable(feature = "map_into_keys_values", since = "1.54.0")]
impl<K, V> FusedIterator for IntoKeys<K, V> {}

#[stable(feature = "map_into_keys_values", since = "1.54.0")]
impl<K, V> Iterator for IntoValues<K, V> {
    type Item = V;

    fn next(&mut self) -> Option<V> {
}

    fn size_hint(&self) -> (usize, Option<usize>) {
}

    fn last(mut self) -> Option<V> {
}
}

#[stable(feature = "map_into_keys_values", since = "1.54.0")]
impl<K, V> DoubleEndedIterator for IntoValues<K, V> {
    fn next_back(&mut self) -> Option<V> {
}
}

#[stable(feature = "map_into_keys_values", since = "1.54.0")]
impl<K, V> ExactSizeIterator for IntoValues<K, V> {
    fn len(&self) -> usize {
}
}

#[stable(feature = "map_into_keys_values", since = "1.54.0")]
impl<K, V> FusedIterator for IntoValues<K, V> {}

#[stable(feature = "btree_range", since = "1.17.0")]
impl<'a, K, V> DoubleEndedIterator for Range<'a, K, V> {
    fn next_back(&mut self) -> Option<(&'a K, &'a V)> {
}
}

#[stable(feature = "fused", since = "1.26.0")]
impl<K, V> FusedIterator for Range<'_, K, V> {}

#[stable(feature = "btree_range", since = "1.17.0")]
impl<K, V> Clone for Range<'_, K, V> {
    fn clone(&self) -> Self {
}
}

#[stable(feature = "btree_range", since = "1.17.0")]
impl<'a, K, V> Iterator for RangeMut<'a, K, V> {
    type Item = (&'a K, &'a mut V);

    fn next(&mut self) -> Option<(&'a K, &'a mut V)> {
}

    fn last(mut self) -> Option<(&'a K, &'a mut V)> {
}

    fn min(mut self) -> Option<(&'a K, &'a mut V)> {
}

    fn max(mut self) -> Option<(&'a K, &'a mut V)> {
}
}

#[stable(feature = "btree_range", since = "1.17.0")]
impl<'a, K, V> DoubleEndedIterator for RangeMut<'a, K, V> {
    fn next_back(&mut self) -> Option<(&'a K, &'a mut V)> {
}
}

#[stable(feature = "fused", since = "1.26.0")]
impl<K, V> FusedIterator for RangeMut<'_, K, V> {}

#[stable(feature = "rust1", since = "1.0.0")]
impl<K: Ord, V> FromIterator<(K, V)> for BTreeMap<K, V> {
    fn from_iter<T: IntoIterator<Item = (K, V)>>(iter: T) -> BTreeMap<K, V> {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<K: Ord, V> Extend<(K, V)> for BTreeMap<K, V> {
    #[inline]
    fn extend<T: IntoIterator<Item = (K, V)>>(&mut self, iter: T) {
}

    #[inline]
    fn extend_one(&mut self, (k, v): (K, V)) {
}
}

#[stable(feature = "extend_ref", since = "1.2.0")]
impl<'a, K: Ord + Copy, V: Copy> Extend<(&'a K, &'a V)> for BTreeMap<K, V> {
    fn extend<I: IntoIterator<Item = (&'a K, &'a V)>>(&mut self, iter: I) {
}

    #[inline]
    fn extend_one(&mut self, (&k, &v): (&'a K, &'a V)) {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<K: Hash, V: Hash> Hash for BTreeMap<K, V> {
    fn hash<H: Hasher>(&self, state: &mut H) {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<K, V> Default for BTreeMap<K, V> {
    /// Creates an empty `BTreeMap`.
    fn default() -> BTreeMap<K, V> {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<K: PartialEq, V: PartialEq> PartialEq for BTreeMap<K, V> {
    fn eq(&self, other: &BTreeMap<K, V>) -> bool {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<K: Eq, V: Eq> Eq for BTreeMap<K, V> {}

#[stable(feature = "rust1", since = "1.0.0")]
impl<K: PartialOrd, V: PartialOrd> PartialOrd for BTreeMap<K, V> {
    #[inline]
    fn partial_cmp(&self, other: &BTreeMap<K, V>) -> Option<Ordering> {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<K: Ord, V: Ord> Ord for BTreeMap<K, V> {
    #[inline]
    fn cmp(&self, other: &BTreeMap<K, V>) -> Ordering {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<K: Debug, V: Debug> Debug for BTreeMap<K, V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<K, Q: ?Sized, V> Index<&Q> for BTreeMap<K, V>
where
    K: Borrow<Q> + Ord,
    Q: Ord,
{
    type Output = V;

    /// Returns a reference to the value corresponding to the supplied key.
    ///
    /// # Panics
    ///
    /// Panics if the key is not present in the `BTreeMap`.
    #[inline]
    fn index(&self, key: &Q) -> &V {
}
}

#[stable(feature = "std_collections_from_array", since = "1.56.0")]
impl<K: Ord, V, const N: usize> From<[(K, V); N]> for BTreeMap<K, V> {
    /// ```
    /// use std::collections::BTreeMap;
    ///
    /// let map1 = BTreeMap::from([(1, 2), (3, 4)]);
    /// let map2: BTreeMap<_, _> = [(1, 2), (3, 4)].into();
    /// assert_eq!(map1, map2);
    /// ```
    fn from(arr: [(K, V); N]) -> Self {
}
}

impl<K, V> BTreeMap<K, V> {
    /// Gets an iterator over the entries of the map, sorted by key.
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// use std::collections::BTreeMap;
    ///
    /// let mut map = BTreeMap::new();
    /// map.insert(3, "c");
    /// map.insert(2, "b");
    /// map.insert(1, "a");
    ///
    /// for (key, value) in map.iter() {
    ///     println!("{}: {}", key, value);
    /// }
    ///
    /// let (first_key, first_value) = map.iter().next().unwrap();
    /// assert_eq!((*first_key, *first_value), (1, "a"));
    /// ```
    #[stable(feature = "rust1", since = "1.0.0")]
    pub fn iter(&self) -> Iter<'_, K, V> {
}

    /// Gets a mutable iterator over the entries of the map, sorted by key.
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// use std::collections::BTreeMap;
    ///
    /// let mut map = BTreeMap::new();
    /// map.insert("a", 1);
    /// map.insert("b", 2);
    /// map.insert("c", 3);
    ///
    /// // add 10 to the value if the key isn't "a"
    /// for (key, value) in map.iter_mut() {
    ///     if key != &"a" {
    ///         *value += 10;
    ///     }
    /// }
    /// ```
    #[stable(feature = "rust1", since = "1.0.0")]
    pub fn iter_mut(&mut self) -> IterMut<'_, K, V> {
}

    /// Gets an iterator over the keys of the map, in sorted order.
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// use std::collections::BTreeMap;
    ///
    /// let mut a = BTreeMap::new();
    /// a.insert(2, "b");
    /// a.insert(1, "a");
    ///
    /// let keys: Vec<_> = a.keys().cloned().collect();
    /// assert_eq!(keys, [1, 2]);
    /// ```
    #[stable(feature = "rust1", since = "1.0.0")]
    pub fn keys(&self) -> Keys<'_, K, V> {
}

    /// Gets an iterator over the values of the map, in order by key.
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// use std::collections::BTreeMap;
    ///
    /// let mut a = BTreeMap::new();
    /// a.insert(1, "hello");
    /// a.insert(2, "goodbye");
    ///
    /// let values: Vec<&str> = a.values().cloned().collect();
    /// assert_eq!(values, ["hello", "goodbye"]);
    /// ```
    #[stable(feature = "rust1", since = "1.0.0")]
    pub fn values(&self) -> Values<'_, K, V> {
}

    /// Gets a mutable iterator over the values of the map, in order by key.
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// use std::collections::BTreeMap;
    ///
    /// let mut a = BTreeMap::new();
    /// a.insert(1, String::from("hello"));
    /// a.insert(2, String::from("goodbye"));
    ///
    /// for value in a.values_mut() {
    ///     value.push_str("!");
    /// }
    ///
    /// let values: Vec<String> = a.values().cloned().collect();
    /// assert_eq!(values, [String::from("hello!"),
    ///                     String::from("goodbye!")]);
    /// ```
    #[stable(feature = "map_values_mut", since = "1.10.0")]
    pub fn values_mut(&mut self) -> ValuesMut<'_, K, V> {
}

    /// Returns the number of elements in the map.
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// use std::collections::BTreeMap;
    ///
    /// let mut a = BTreeMap::new();
    /// assert_eq!(a.len(), 0);
    /// a.insert(1, "a");
    /// assert_eq!(a.len(), 1);
    /// ```
    #[stable(feature = "rust1", since = "1.0.0")]
    #[rustc_const_unstable(feature = "const_btree_new", issue = "71835")]
    pub const fn len(&self) -> usize {
}

    /// Returns `true` if the map contains no elements.
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// use std::collections::BTreeMap;
    ///
    /// let mut a = BTreeMap::new();
    /// assert!(a.is_empty());
    /// a.insert(1, "a");
    /// assert!(!a.is_empty());
    /// ```
    #[stable(feature = "rust1", since = "1.0.0")]
    #[rustc_const_unstable(feature = "const_btree_new", issue = "71835")]
    pub const fn is_empty(&self) -> bool {
}

    /// If the root node is the empty (non-allocated) root node, allocate our
    /// own node. Is an associated function to avoid borrowing the entire BTreeMap.
    fn ensure_is_owned(root: &mut Option<Root<K, V>>) -> &mut Root<K, V> {
}
}

#[cfg(test)]
mod tests {
}
}
mod mem {
use core::intrinsics;
use core::mem;
use core::ptr;

/// This replaces the value behind the `v` unique reference by calling the
/// relevant function.
///
/// If a panic occurs in the `change` closure, the entire process will be aborted.
#[allow(dead_code)] // keep as illustration and for future use
#[inline]
pub fn take_mut<T>(v: &mut T, change: impl FnOnce(T) -> T) {
}

/// This replaces the value behind the `v` unique reference by calling the
/// relevant function, and returns a result obtained along the way.
///
/// If a panic occurs in the `change` closure, the entire process will be aborted.
#[inline]
pub fn replace<T, R>(v: &mut T, change: impl FnOnce(T) -> (T, R)) -> R {
}
}
mod merge_iter {
use core::cmp::Ordering;
use core::fmt::{self, Debug};
use core::iter::FusedIterator;

/// Core of an iterator that merges the output of two strictly ascending iterators,
/// for instance a union or a symmetric difference.
pub struct MergeIterInner<I: Iterator> {
    a: I,
    b: I,
    peeked: Option<Peeked<I>>,
}

/// Benchmarks faster than wrapping both iterators in a Peekable,
/// probably because we can afford to impose a FusedIterator bound.
#[derive(Clone, Debug)]
enum Peeked<I: Iterator> {
    A(I::Item),
    B(I::Item),
}

impl<I: Iterator> Clone for MergeIterInner<I>
where
    I: Clone,
    I::Item: Clone,
{
    fn clone(&self) -> Self {
}
}

impl<I: Iterator> Debug for MergeIterInner<I>
where
    I: Debug,
    I::Item: Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
}
}

impl<I: Iterator> MergeIterInner<I> {
    /// Creates a new core for an iterator merging a pair of sources.
    pub fn new(a: I, b: I) -> Self {
}

    /// Returns the next pair of items stemming from the pair of sources
    /// being merged. If both returned options contain a value, that value
    /// is equal and occurs in both sources. If one of the returned options
    /// contains a value, that value doesn't occur in the other source (or
    /// the sources are not strictly ascending). If neither returned option
    /// contains a value, iteration has finished and subsequent calls will
    /// return the same empty pair.
    pub fn nexts<Cmp: Fn(&I::Item, &I::Item) -> Ordering>(
        &mut self,
        cmp: Cmp,
    ) -> (Option<I::Item>, Option<I::Item>)
    where
        I: FusedIterator,
    {
}

    /// Returns a pair of upper bounds for the `size_hint` of the final iterator.
    pub fn lens(&self) -> (usize, usize)
    where
        I: ExactSizeIterator,
    {
}
}
}
mod navigate {
use core::borrow::Borrow;
use core::hint;
use core::ops::RangeBounds;
use core::ptr;

use super::node::{marker, ForceResult::*, Handle, NodeRef};

// `front` and `back` are always both `None` or both `Some`.
pub struct LeafRange<BorrowType, K, V> {
    front: Option<Handle<NodeRef<BorrowType, K, V, marker::Leaf>, marker::Edge>>,
    back: Option<Handle<NodeRef<BorrowType, K, V, marker::Leaf>, marker::Edge>>,
}

impl<'a, K: 'a, V: 'a> Clone for LeafRange<marker::Immut<'a>, K, V> {
    fn clone(&self) -> Self {
}
}

impl<BorrowType, K, V> LeafRange<BorrowType, K, V> {
    pub fn none() -> Self {
}

    fn is_empty(&self) -> bool {
}

    /// Temporarily takes out another, immutable equivalent of the same range.
    pub fn reborrow(&self) -> LeafRange<marker::Immut<'_>, K, V> {
}
}

impl<'a, K, V> LeafRange<marker::Immut<'a>, K, V> {
    #[inline]
    pub fn next_checked(&mut self) -> Option<(&'a K, &'a V)> {
}

    #[inline]
    pub fn next_back_checked(&mut self) -> Option<(&'a K, &'a V)> {
}
}

impl<'a, K, V> LeafRange<marker::ValMut<'a>, K, V> {
    #[inline]
    pub fn next_checked(&mut self) -> Option<(&'a K, &'a mut V)> {
}

    #[inline]
    pub fn next_back_checked(&mut self) -> Option<(&'a K, &'a mut V)> {
}
}

impl<BorrowType: marker::BorrowType, K, V> LeafRange<BorrowType, K, V> {
    /// If possible, extract some result from the following KV and move to the edge beyond it.
    fn perform_next_checked<F, R>(&mut self, f: F) -> Option<R>
    where
        F: Fn(&Handle<NodeRef<BorrowType, K, V, marker::LeafOrInternal>, marker::KV>) -> R,
    {
}

    /// If possible, extract some result from the preceding KV and move to the edge beyond it.
    fn perform_next_back_checked<F, R>(&mut self, f: F) -> Option<R>
    where
        F: Fn(&Handle<NodeRef<BorrowType, K, V, marker::LeafOrInternal>, marker::KV>) -> R,
    {
}
}

enum LazyLeafHandle<BorrowType, K, V> {
    Root(NodeRef<BorrowType, K, V, marker::LeafOrInternal>), // not yet descended
    Edge(Handle<NodeRef<BorrowType, K, V, marker::Leaf>, marker::Edge>),
}

impl<'a, K: 'a, V: 'a> Clone for LazyLeafHandle<marker::Immut<'a>, K, V> {
    fn clone(&self) -> Self {
}
}

impl<BorrowType, K, V> LazyLeafHandle<BorrowType, K, V> {
    fn reborrow(&self) -> LazyLeafHandle<marker::Immut<'_>, K, V> {
}
}

// `front` and `back` are always both `None` or both `Some`.
pub struct LazyLeafRange<BorrowType, K, V> {
    front: Option<LazyLeafHandle<BorrowType, K, V>>,
    back: Option<LazyLeafHandle<BorrowType, K, V>>,
}

impl<'a, K: 'a, V: 'a> Clone for LazyLeafRange<marker::Immut<'a>, K, V> {
    fn clone(&self) -> Self {
}
}

impl<BorrowType, K, V> LazyLeafRange<BorrowType, K, V> {
    pub fn none() -> Self {
}

    /// Temporarily takes out another, immutable equivalent of the same range.
    pub fn reborrow(&self) -> LazyLeafRange<marker::Immut<'_>, K, V> {
}
}

impl<'a, K, V> LazyLeafRange<marker::Immut<'a>, K, V> {
    #[inline]
    pub unsafe fn next_unchecked(&mut self) -> (&'a K, &'a V) {
}

    #[inline]
    pub unsafe fn next_back_unchecked(&mut self) -> (&'a K, &'a V) {
}
}

impl<'a, K, V> LazyLeafRange<marker::ValMut<'a>, K, V> {
    #[inline]
    pub unsafe fn next_unchecked(&mut self) -> (&'a K, &'a mut V) {
}

    #[inline]
    pub unsafe fn next_back_unchecked(&mut self) -> (&'a K, &'a mut V) {
}
}

impl<K, V> LazyLeafRange<marker::Dying, K, V> {
    fn take_front(
        &mut self,
    ) -> Option<Handle<NodeRef<marker::Dying, K, V, marker::Leaf>, marker::Edge>> {
}

    #[inline]
    pub unsafe fn deallocating_next_unchecked(
        &mut self,
    ) -> Handle<NodeRef<marker::Dying, K, V, marker::LeafOrInternal>, marker::KV> {
}

    #[inline]
    pub unsafe fn deallocating_next_back_unchecked(
        &mut self,
    ) -> Handle<NodeRef<marker::Dying, K, V, marker::LeafOrInternal>, marker::KV> {
}

    #[inline]
    pub fn deallocating_end(&mut self) {
}
}

impl<BorrowType: marker::BorrowType, K, V> LazyLeafRange<BorrowType, K, V> {
    fn init_front(
        &mut self,
    ) -> Option<&mut Handle<NodeRef<BorrowType, K, V, marker::Leaf>, marker::Edge>> {
}

    fn init_back(
        &mut self,
    ) -> Option<&mut Handle<NodeRef<BorrowType, K, V, marker::Leaf>, marker::Edge>> {
}
}

impl<BorrowType: marker::BorrowType, K, V> NodeRef<BorrowType, K, V, marker::LeafOrInternal> {
    /// Finds the distinct leaf edges delimiting a specified range in a tree.
    ///
    /// If such distinct edges exist, returns them in ascending order, meaning
    /// that a non-zero number of calls to `next_unchecked` on the `front` of
    /// the result and/or calls to `next_back_unchecked` on the `back` of the
    /// result will eventually reach the same edge.
    ///
    /// If there are no such edges, i.e., if the tree contains no key within
    /// the range, returns an empty `front` and `back`.
    ///
    /// # Safety
    /// Unless `BorrowType` is `Immut`, do not use the handles to visit the same
    /// KV twice.
    unsafe fn find_leaf_edges_spanning_range<Q: ?Sized, R>(
        self,
        range: R,
    ) -> LeafRange<BorrowType, K, V>
    where
        Q: Ord,
        K: Borrow<Q>,
        R: RangeBounds<Q>,
    {
}
}

fn full_range<BorrowType: marker::BorrowType, K, V>(
    root1: NodeRef<BorrowType, K, V, marker::LeafOrInternal>,
    root2: NodeRef<BorrowType, K, V, marker::LeafOrInternal>,
) -> LazyLeafRange<BorrowType, K, V> {
}

impl<'a, K: 'a, V: 'a> NodeRef<marker::Immut<'a>, K, V, marker::LeafOrInternal> {
    /// Finds the pair of leaf edges delimiting a specific range in a tree.
    ///
    /// The result is meaningful only if the tree is ordered by key, like the tree
    /// in a `BTreeMap` is.
    pub fn range_search<Q, R>(self, range: R) -> LeafRange<marker::Immut<'a>, K, V>
    where
        Q: ?Sized + Ord,
        K: Borrow<Q>,
        R: RangeBounds<Q>,
    {
}

    /// Finds the pair of leaf edges delimiting an entire tree.
    pub fn full_range(self) -> LazyLeafRange<marker::Immut<'a>, K, V> {
}
}

impl<'a, K: 'a, V: 'a> NodeRef<marker::ValMut<'a>, K, V, marker::LeafOrInternal> {
    /// Splits a unique reference into a pair of leaf edges delimiting a specified range.
    /// The result are non-unique references allowing (some) mutation, which must be used
    /// carefully.
    ///
    /// The result is meaningful only if the tree is ordered by key, like the tree
    /// in a `BTreeMap` is.
    ///
    /// # Safety
    /// Do not use the duplicate handles to visit the same KV twice.
    pub fn range_search<Q, R>(self, range: R) -> LeafRange<marker::ValMut<'a>, K, V>
    where
        Q: ?Sized + Ord,
        K: Borrow<Q>,
        R: RangeBounds<Q>,
    {
}

    /// Splits a unique reference into a pair of leaf edges delimiting the full range of the tree.
    /// The results are non-unique references allowing mutation (of values only), so must be used
    /// with care.
    pub fn full_range(self) -> LazyLeafRange<marker::ValMut<'a>, K, V> {
}
}

impl<K, V> NodeRef<marker::Dying, K, V, marker::LeafOrInternal> {
    /// Splits a unique reference into a pair of leaf edges delimiting the full range of the tree.
    /// The results are non-unique references allowing massively destructive mutation, so must be
    /// used with the utmost care.
    pub fn full_range(self) -> LazyLeafRange<marker::Dying, K, V> {
}
}

impl<BorrowType: marker::BorrowType, K, V>
    Handle<NodeRef<BorrowType, K, V, marker::Leaf>, marker::Edge>
{
    /// Given a leaf edge handle, returns [`Result::Ok`] with a handle to the neighboring KV
    /// on the right side, which is either in the same leaf node or in an ancestor node.
    /// If the leaf edge is the last one in the tree, returns [`Result::Err`] with the root node.
    pub fn next_kv(
        self,
    ) -> Result<
        Handle<NodeRef<BorrowType, K, V, marker::LeafOrInternal>, marker::KV>,
        NodeRef<BorrowType, K, V, marker::LeafOrInternal>,
    > {
}

    /// Given a leaf edge handle, returns [`Result::Ok`] with a handle to the neighboring KV
    /// on the left side, which is either in the same leaf node or in an ancestor node.
    /// If the leaf edge is the first one in the tree, returns [`Result::Err`] with the root node.
    fn next_back_kv(
        self,
    ) -> Result<
        Handle<NodeRef<BorrowType, K, V, marker::LeafOrInternal>, marker::KV>,
        NodeRef<BorrowType, K, V, marker::LeafOrInternal>,
    > {
}
}

impl<BorrowType: marker::BorrowType, K, V>
    Handle<NodeRef<BorrowType, K, V, marker::Internal>, marker::Edge>
{
    /// Given an internal edge handle, returns [`Result::Ok`] with a handle to the neighboring KV
    /// on the right side, which is either in the same internal node or in an ancestor node.
    /// If the internal edge is the last one in the tree, returns [`Result::Err`] with the root node.
    fn next_kv(
        self,
    ) -> Result<
        Handle<NodeRef<BorrowType, K, V, marker::Internal>, marker::KV>,
        NodeRef<BorrowType, K, V, marker::Internal>,
    > {
}
}

impl<K, V> Handle<NodeRef<marker::Dying, K, V, marker::Leaf>, marker::Edge> {
    /// Given a leaf edge handle into a dying tree, returns the next leaf edge
    /// on the right side, and the key-value pair in between, if they exist.
    ///
    /// If the given edge is the last one in a leaf, this method deallocates
    /// the leaf, as well as any ancestor nodes whose last edge was reached.
    /// This implies that if no more key-value pair follows, the entire tree
    /// will have been deallocated and there is nothing left to return.
    ///
    /// # Safety
    /// - The given edge must not have been previously returned by counterpart
    ///   `deallocating_next_back`.
    /// - The returned KV handle is only valid to access the key and value,
    ///   and only valid until the next call to this method or counterpart
    ///   `deallocating_next_back`.
    unsafe fn deallocating_next(
        self,
    ) -> Option<(Self, Handle<NodeRef<marker::Dying, K, V, marker::LeafOrInternal>, marker::KV>)>
    {
}

    /// Given a leaf edge handle into a dying tree, returns the next leaf edge
    /// on the left side, and the key-value pair in between, if they exist.
    ///
    /// If the given edge is the first one in a leaf, this method deallocates
    /// the leaf, as well as any ancestor nodes whose first edge was reached.
    /// This implies that if no more key-value pair follows, the entire tree
    /// will have been deallocated and there is nothing left to return.
    ///
    /// # Safety
    /// - The given edge must not have been previously returned by counterpart
    ///   `deallocating_next`.
    /// - The returned KV handle is only valid to access the key and value,
    ///   and only valid until the next call to this method or counterpart
    ///   `deallocating_next`.
    unsafe fn deallocating_next_back(
        self,
    ) -> Option<(Self, Handle<NodeRef<marker::Dying, K, V, marker::LeafOrInternal>, marker::KV>)>
    {
}

    /// Deallocates a pile of nodes from the leaf up to the root.
    /// This is the only way to deallocate the remainder of a tree after
    /// `deallocating_next` and `deallocating_next_back` have been nibbling at
    /// both sides of the tree, and have hit the same edge. As it is intended
    /// only to be called when all keys and values have been returned,
    /// no cleanup is done on any of the keys or values.
    fn deallocating_end(self) {
}
}

impl<'a, K, V> Handle<NodeRef<marker::Immut<'a>, K, V, marker::Leaf>, marker::Edge> {
    /// Moves the leaf edge handle to the next leaf edge and returns references to the
    /// key and value in between.
    ///
    /// # Safety
    /// There must be another KV in the direction travelled.
    unsafe fn next_unchecked(&mut self) -> (&'a K, &'a V) {
}

    /// Moves the leaf edge handle to the previous leaf edge and returns references to the
    /// key and value in between.
    ///
    /// # Safety
    /// There must be another KV in the direction travelled.
    unsafe fn next_back_unchecked(&mut self) -> (&'a K, &'a V) {
}
}

impl<'a, K, V> Handle<NodeRef<marker::ValMut<'a>, K, V, marker::Leaf>, marker::Edge> {
    /// Moves the leaf edge handle to the next leaf edge and returns references to the
    /// key and value in between.
    ///
    /// # Safety
    /// There must be another KV in the direction travelled.
    unsafe fn next_unchecked(&mut self) -> (&'a K, &'a mut V) {
}

    /// Moves the leaf edge handle to the previous leaf and returns references to the
    /// key and value in between.
    ///
    /// # Safety
    /// There must be another KV in the direction travelled.
    unsafe fn next_back_unchecked(&mut self) -> (&'a K, &'a mut V) {
}
}

impl<K, V> Handle<NodeRef<marker::Dying, K, V, marker::Leaf>, marker::Edge> {
    /// Moves the leaf edge handle to the next leaf edge and returns the key and value
    /// in between, deallocating any node left behind while leaving the corresponding
    /// edge in its parent node dangling.
    ///
    /// # Safety
    /// - There must be another KV in the direction travelled.
    /// - That KV was not previously returned by counterpart
    ///   `deallocating_next_back_unchecked` on any copy of the handles
    ///   being used to traverse the tree.
    ///
    /// The only safe way to proceed with the updated handle is to compare it, drop it,
    /// or call this method or counterpart `deallocating_next_back_unchecked` again.
    unsafe fn deallocating_next_unchecked(
        &mut self,
    ) -> Handle<NodeRef<marker::Dying, K, V, marker::LeafOrInternal>, marker::KV> {
}

    /// Moves the leaf edge handle to the previous leaf edge and returns the key and value
    /// in between, deallocating any node left behind while leaving the corresponding
    /// edge in its parent node dangling.
    ///
    /// # Safety
    /// - There must be another KV in the direction travelled.
    /// - That leaf edge was not previously returned by counterpart
    ///   `deallocating_next_unchecked` on any copy of the handles
    ///   being used to traverse the tree.
    ///
    /// The only safe way to proceed with the updated handle is to compare it, drop it,
    /// or call this method or counterpart `deallocating_next_unchecked` again.
    unsafe fn deallocating_next_back_unchecked(
        &mut self,
    ) -> Handle<NodeRef<marker::Dying, K, V, marker::LeafOrInternal>, marker::KV> {
}
}

impl<BorrowType: marker::BorrowType, K, V> NodeRef<BorrowType, K, V, marker::LeafOrInternal> {
    /// Returns the leftmost leaf edge in or underneath a node - in other words, the edge
    /// you need first when navigating forward (or last when navigating backward).
    #[inline]
    pub fn first_leaf_edge(self) -> Handle<NodeRef<BorrowType, K, V, marker::Leaf>, marker::Edge> {
}

    /// Returns the rightmost leaf edge in or underneath a node - in other words, the edge
    /// you need last when navigating forward (or first when navigating backward).
    #[inline]
    pub fn last_leaf_edge(self) -> Handle<NodeRef<BorrowType, K, V, marker::Leaf>, marker::Edge> {
}
}

pub enum Position<BorrowType, K, V> {
    Leaf(NodeRef<BorrowType, K, V, marker::Leaf>),
    Internal(NodeRef<BorrowType, K, V, marker::Internal>),
    InternalKV(Handle<NodeRef<BorrowType, K, V, marker::Internal>, marker::KV>),
}

impl<'a, K: 'a, V: 'a> NodeRef<marker::Immut<'a>, K, V, marker::LeafOrInternal> {
    /// Visits leaf nodes and internal KVs in order of ascending keys, and also
    /// visits internal nodes as a whole in a depth first order, meaning that
    /// internal nodes precede their individual KVs and their child nodes.
    pub fn visit_nodes_in_order<F>(self, mut visit: F)
    where
        F: FnMut(Position<marker::Immut<'a>, K, V>),
    {
}

    /// Calculates the number of elements in a (sub)tree.
    pub fn calc_length(self) -> usize {
}
}

impl<BorrowType: marker::BorrowType, K, V>
    Handle<NodeRef<BorrowType, K, V, marker::LeafOrInternal>, marker::KV>
{
    /// Returns the leaf edge closest to a KV for forward navigation.
    pub fn next_leaf_edge(self) -> Handle<NodeRef<BorrowType, K, V, marker::Leaf>, marker::Edge> {
}

    /// Returns the leaf edge closest to a KV for backward navigation.
    fn next_back_leaf_edge(self) -> Handle<NodeRef<BorrowType, K, V, marker::Leaf>, marker::Edge> {
}
}
}
mod node {
// This is an attempt at an implementation following the ideal
//
// ```
// struct BTreeMap<K, V> {
//     height: usize,
//     root: Option<Box<Node<K, V, height>>>
// }
//
// struct Node<K, V, height: usize> {
//     keys: [K; 2 * B - 1],
//     vals: [V; 2 * B - 1],
//     edges: [if height > 0 { Box<Node<K, V, height - 1>> } else { () }; 2 * B],
//     parent: Option<(NonNull<Node<K, V, height + 1>>, u16)>,
//     len: u16,
// }
// ```
//
// Since Rust doesn't actually have dependent types and polymorphic recursion,
// we make do with lots of unsafety.

// A major goal of this module is to avoid complexity by treating the tree as a generic (if
// weirdly shaped) container and avoiding dealing with most of the B-Tree invariants. As such,
// this module doesn't care whether the entries are sorted, which nodes can be underfull, or
// even what underfull means. However, we do rely on a few invariants:
//
// - Trees must have uniform depth/height. This means that every path down to a leaf from a
//   given node has exactly the same length.
// - A node of length `n` has `n` keys, `n` values, and `n + 1` edges.
//   This implies that even an empty node has at least one edge.
//   For a leaf node, "having an edge" only means we can identify a position in the node,
//   since leaf edges are empty and need no data representation. In an internal node,
//   an edge both identifies a position and contains a pointer to a child node.

use core::marker::PhantomData;
use core::mem::{self, MaybeUninit};
use core::ptr::{self, NonNull};
use core::slice::SliceIndex;

use crate::alloc::{Allocator, Global, Layout};
use crate::boxed::Box;

const B: usize = 6;
pub const CAPACITY: usize = 2 * B - 1;
pub const MIN_LEN_AFTER_SPLIT: usize = B - 1;
const KV_IDX_CENTER: usize = B - 1;
const EDGE_IDX_LEFT_OF_CENTER: usize = B - 1;
const EDGE_IDX_RIGHT_OF_CENTER: usize = B;

/// The underlying representation of leaf nodes and part of the representation of internal nodes.
struct LeafNode<K, V> {
    /// We want to be covariant in `K` and `V`.
    parent: Option<NonNull<InternalNode<K, V>>>,

    /// This node's index into the parent node's `edges` array.
    /// `*node.parent.edges[node.parent_idx]` should be the same thing as `node`.
    /// This is only guaranteed to be initialized when `parent` is non-null.
    parent_idx: MaybeUninit<u16>,

    /// The number of keys and values this node stores.
    len: u16,

    /// The arrays storing the actual data of the node. Only the first `len` elements of each
    /// array are initialized and valid.
    keys: [MaybeUninit<K>; CAPACITY],
    vals: [MaybeUninit<V>; CAPACITY],
}

impl<K, V> LeafNode<K, V> {
    /// Initializes a new `LeafNode` in-place.
    unsafe fn init(this: *mut Self) {
}

    /// Creates a new boxed `LeafNode`.
    fn new() -> Box<Self> {
}
}

/// The underlying representation of internal nodes. As with `LeafNode`s, these should be hidden
/// behind `BoxedNode`s to prevent dropping uninitialized keys and values. Any pointer to an
/// `InternalNode` can be directly cast to a pointer to the underlying `LeafNode` portion of the
/// node, allowing code to act on leaf and internal nodes generically without having to even check
/// which of the two a pointer is pointing at. This property is enabled by the use of `repr(C)`.
#[repr(C)]
// gdb_providers.py uses this type name for introspection.
struct InternalNode<K, V> {
    data: LeafNode<K, V>,

    /// The pointers to the children of this node. `len + 1` of these are considered
    /// initialized and valid, except that near the end, while the tree is held
    /// through borrow type `Dying`, some of these pointers are dangling.
    edges: [MaybeUninit<BoxedNode<K, V>>; 2 * B],
}

impl<K, V> InternalNode<K, V> {
    /// Creates a new boxed `InternalNode`.
    ///
    /// # Safety
    /// An invariant of internal nodes is that they have at least one
    /// initialized and valid edge. This function does not set up
    /// such an edge.
    unsafe fn new() -> Box<Self> {
}
}

/// A managed, non-null pointer to a node. This is either an owned pointer to
/// `LeafNode<K, V>` or an owned pointer to `InternalNode<K, V>`.
///
/// However, `BoxedNode` contains no information as to which of the two types
/// of nodes it actually contains, and, partially due to this lack of information,
/// is not a separate type and has no destructor.
type BoxedNode<K, V> = NonNull<LeafNode<K, V>>;

// N.B. `NodeRef` is always covariant in `K` and `V`, even when the `BorrowType`
// is `Mut`. This is technically wrong, but cannot result in any unsafety due to
// internal use of `NodeRef` because we stay completely generic over `K` and `V`.
// However, whenever a public type wraps `NodeRef`, make sure that it has the
// correct variance.
///
/// A reference to a node.
///
/// This type has a number of parameters that controls how it acts:
/// - `BorrowType`: A dummy type that describes the kind of borrow and carries a lifetime.
///    - When this is `Immut<'a>`, the `NodeRef` acts roughly like `&'a Node`.
///    - When this is `ValMut<'a>`, the `NodeRef` acts roughly like `&'a Node`
///      with respect to keys and tree structure, but also allows many
///      mutable references to values throughout the tree to coexist.
///    - When this is `Mut<'a>`, the `NodeRef` acts roughly like `&'a mut Node`,
///      although insert methods allow a mutable pointer to a value to coexist.
///    - When this is `Owned`, the `NodeRef` acts roughly like `Box<Node>`,
///      but does not have a destructor, and must be cleaned up manually.
///    - When this is `Dying`, the `NodeRef` still acts roughly like `Box<Node>`,
///      but has methods to destroy the tree bit by bit, and ordinary methods,
///      while not marked as unsafe to call, can invoke UB if called incorrectly.
///   Since any `NodeRef` allows navigating through the tree, `BorrowType`
///   effectively applies to the entire tree, not just to the node itself.
/// - `K` and `V`: These are the types of keys and values stored in the nodes.
/// - `Type`: This can be `Leaf`, `Internal`, or `LeafOrInternal`. When this is
///   `Leaf`, the `NodeRef` points to a leaf node, when this is `Internal` the
///   `NodeRef` points to an internal node, and when this is `LeafOrInternal` the
///   `NodeRef` could be pointing to either type of node.
///   `Type` is named `NodeType` when used outside `NodeRef`.
///
/// Both `BorrowType` and `NodeType` restrict what methods we implement, to
/// exploit static type safety. There are limitations in the way we can apply
/// such restrictions:
/// - For each type parameter, we can only define a method either generically
///   or for one particular type. For example, we cannot define a method like
///   `into_kv` generically for all `BorrowType`, or once for all types that
///   carry a lifetime, because we want it to return `&'a` references.
///   Therefore, we define it only for the least powerful type `Immut<'a>`.
/// - We cannot get implicit coercion from say `Mut<'a>` to `Immut<'a>`.
///   Therefore, we have to explicitly call `reborrow` on a more powerful
///   `NodeRef` in order to reach a method like `into_kv`.
///
/// All methods on `NodeRef` that return some kind of reference, either:
/// - Take `self` by value, and return the lifetime carried by `BorrowType`.
///   Sometimes, to invoke such a method, we need to call `reborrow_mut`.
/// - Take `self` by reference, and (implicitly) return that reference's
///   lifetime, instead of the lifetime carried by `BorrowType`. That way,
///   the borrow checker guarantees that the `NodeRef` remains borrowed as long
///   as the returned reference is used.
///   The methods supporting insert bend this rule by returning a raw pointer,
///   i.e., a reference without any lifetime.
pub struct NodeRef<BorrowType, K, V, Type> {
    /// The number of levels that the node and the level of leaves are apart, a
    /// constant of the node that cannot be entirely described by `Type`, and that
    /// the node itself does not store. We only need to store the height of the root
    /// node, and derive every other node's height from it.
    /// Must be zero if `Type` is `Leaf` and non-zero if `Type` is `Internal`.
    height: usize,
    /// The pointer to the leaf or internal node. The definition of `InternalNode`
    /// ensures that the pointer is valid either way.
    node: NonNull<LeafNode<K, V>>,
    _marker: PhantomData<(BorrowType, Type)>,
}

/// The root node of an owned tree.
///
/// Note that this does not have a destructor, and must be cleaned up manually.
pub type Root<K, V> = NodeRef<marker::Owned, K, V, marker::LeafOrInternal>;

impl<'a, K: 'a, V: 'a, Type> Copy for NodeRef<marker::Immut<'a>, K, V, Type> {}
impl<'a, K: 'a, V: 'a, Type> Clone for NodeRef<marker::Immut<'a>, K, V, Type> {
    fn clone(&self) -> Self {
}
}

unsafe impl<BorrowType, K: Sync, V: Sync, Type> Sync for NodeRef<BorrowType, K, V, Type> {}

unsafe impl<'a, K: Sync + 'a, V: Sync + 'a, Type> Send for NodeRef<marker::Immut<'a>, K, V, Type> {}
unsafe impl<'a, K: Send + 'a, V: Send + 'a, Type> Send for NodeRef<marker::Mut<'a>, K, V, Type> {}
unsafe impl<'a, K: Send + 'a, V: Send + 'a, Type> Send for NodeRef<marker::ValMut<'a>, K, V, Type> {}
unsafe impl<K: Send, V: Send, Type> Send for NodeRef<marker::Owned, K, V, Type> {}
unsafe impl<K: Send, V: Send, Type> Send for NodeRef<marker::Dying, K, V, Type> {}

impl<K, V> NodeRef<marker::Owned, K, V, marker::Leaf> {
    fn new_leaf() -> Self {
}

    fn from_new_leaf(leaf: Box<LeafNode<K, V>>) -> Self {
}
}

impl<K, V> NodeRef<marker::Owned, K, V, marker::Internal> {
    fn new_internal(child: Root<K, V>) -> Self {
}

    /// # Safety
    /// `height` must not be zero.
    unsafe fn from_new_internal(internal: Box<InternalNode<K, V>>, height: usize) -> Self {
}
}

impl<BorrowType, K, V> NodeRef<BorrowType, K, V, marker::Internal> {
    /// Unpack a node reference that was packed as `NodeRef::parent`.
    fn from_internal(node: NonNull<InternalNode<K, V>>, height: usize) -> Self {
}
}

impl<BorrowType, K, V> NodeRef<BorrowType, K, V, marker::Internal> {
    /// Exposes the data of an internal node.
    ///
    /// Returns a raw ptr to avoid invalidating other references to this node.
    fn as_internal_ptr(this: &Self) -> *mut InternalNode<K, V> {
}
}

impl<'a, K, V> NodeRef<marker::Mut<'a>, K, V, marker::Internal> {
    /// Borrows exclusive access to the data of an internal node.
    fn as_internal_mut(&mut self) -> &mut InternalNode<K, V> {
}
}

impl<BorrowType, K, V, Type> NodeRef<BorrowType, K, V, Type> {
    /// Finds the length of the node. This is the number of keys or values.
    /// The number of edges is `len() + 1`.
    /// Note that, despite being safe, calling this function can have the side effect
    /// of invalidating mutable references that unsafe code has created.
    pub fn len(&self) -> usize {
}

    /// Returns the number of levels that the node and leaves are apart. Zero
    /// height means the node is a leaf itself. If you picture trees with the
    /// root on top, the number says at which elevation the node appears.
    /// If you picture trees with leaves on top, the number says how high
    /// the tree extends above the node.
    pub fn height(&self) -> usize {
}

    /// Temporarily takes out another, immutable reference to the same node.
    pub fn reborrow(&self) -> NodeRef<marker::Immut<'_>, K, V, Type> {
}

    /// Exposes the leaf portion of any leaf or internal node.
    ///
    /// Returns a raw ptr to avoid invalidating other references to this node.
    fn as_leaf_ptr(this: &Self) -> *mut LeafNode<K, V> {
}
}

impl<BorrowType: marker::BorrowType, K, V, Type> NodeRef<BorrowType, K, V, Type> {
    /// Finds the parent of the current node. Returns `Ok(handle)` if the current
    /// node actually has a parent, where `handle` points to the edge of the parent
    /// that points to the current node. Returns `Err(self)` if the current node has
    /// no parent, giving back the original `NodeRef`.
    ///
    /// The method name assumes you picture trees with the root node on top.
    ///
    /// `edge.descend().ascend().unwrap()` and `node.ascend().unwrap().descend()` should
    /// both, upon success, do nothing.
    pub fn ascend(
        self,
    ) -> Result<Handle<NodeRef<BorrowType, K, V, marker::Internal>, marker::Edge>, Self> {
}

    pub fn first_edge(self) -> Handle<Self, marker::Edge> {
}

    pub fn last_edge(self) -> Handle<Self, marker::Edge> {
}

    /// Note that `self` must be nonempty.
    pub fn first_kv(self) -> Handle<Self, marker::KV> {
}

    /// Note that `self` must be nonempty.
    pub fn last_kv(self) -> Handle<Self, marker::KV> {
}
}

impl<BorrowType, K, V, Type> NodeRef<BorrowType, K, V, Type> {
    /// Could be a public implementation of PartialEq, but only used in this module.
    fn eq(&self, other: &Self) -> bool {
}
}

impl<'a, K: 'a, V: 'a, Type> NodeRef<marker::Immut<'a>, K, V, Type> {
    /// Exposes the leaf portion of any leaf or internal node in an immutable tree.
    fn into_leaf(self) -> &'a LeafNode<K, V> {
}

    /// Borrows a view into the keys stored in the node.
    pub fn keys(&self) -> &[K] {
}
}

impl<K, V> NodeRef<marker::Dying, K, V, marker::LeafOrInternal> {
    /// Similar to `ascend`, gets a reference to a node's parent node, but also
    /// deallocates the current node in the process. This is unsafe because the
    /// current node will still be accessible despite being deallocated.
    pub unsafe fn deallocate_and_ascend(
        self,
    ) -> Option<Handle<NodeRef<marker::Dying, K, V, marker::Internal>, marker::Edge>> {
}
}

impl<'a, K, V, Type> NodeRef<marker::Mut<'a>, K, V, Type> {
    /// Temporarily takes out another mutable reference to the same node. Beware, as
    /// this method is very dangerous, doubly so since it might not immediately appear
    /// dangerous.
    ///
    /// Because mutable pointers can roam anywhere around the tree, the returned
    /// pointer can easily be used to make the original pointer dangling, out of
    /// bounds, or invalid under stacked borrow rules.
    // FIXME(@gereeter) consider adding yet another type parameter to `NodeRef`
    // that restricts the use of navigation methods on reborrowed pointers,
    // preventing this unsafety.
    unsafe fn reborrow_mut(&mut self) -> NodeRef<marker::Mut<'_>, K, V, Type> {
}

    /// Borrows exclusive access to the leaf portion of a leaf or internal node.
    fn as_leaf_mut(&mut self) -> &mut LeafNode<K, V> {
}

    /// Offers exclusive access to the leaf portion of a leaf or internal node.
    fn into_leaf_mut(mut self) -> &'a mut LeafNode<K, V> {
}
}

impl<K, V, Type> NodeRef<marker::Dying, K, V, Type> {
    /// Borrows exclusive access to the leaf portion of a dying leaf or internal node.
    fn as_leaf_dying(&mut self) -> &mut LeafNode<K, V> {
}
}

impl<'a, K: 'a, V: 'a, Type> NodeRef<marker::Mut<'a>, K, V, Type> {
    /// Borrows exclusive access to an element of the key storage area.
    ///
    /// # Safety
    /// `index` is in bounds of 0..CAPACITY
    unsafe fn key_area_mut<I, Output: ?Sized>(&mut self, index: I) -> &mut Output
    where
        I: SliceIndex<[MaybeUninit<K>], Output = Output>,
    {
}

    /// Borrows exclusive access to an element or slice of the node's value storage area.
    ///
    /// # Safety
    /// `index` is in bounds of 0..CAPACITY
    unsafe fn val_area_mut<I, Output: ?Sized>(&mut self, index: I) -> &mut Output
    where
        I: SliceIndex<[MaybeUninit<V>], Output = Output>,
    {
}
}

impl<'a, K: 'a, V: 'a> NodeRef<marker::Mut<'a>, K, V, marker::Internal> {
    /// Borrows exclusive access to an element or slice of the node's storage area for edge contents.
    ///
    /// # Safety
    /// `index` is in bounds of 0..CAPACITY + 1
    unsafe fn edge_area_mut<I, Output: ?Sized>(&mut self, index: I) -> &mut Output
    where
        I: SliceIndex<[MaybeUninit<BoxedNode<K, V>>], Output = Output>,
    {
}
}

impl<'a, K, V, Type> NodeRef<marker::ValMut<'a>, K, V, Type> {
    /// # Safety
    /// - The node has more than `idx` initialized elements.
    unsafe fn into_key_val_mut_at(mut self, idx: usize) -> (&'a K, &'a mut V) {
}
}

impl<'a, K: 'a, V: 'a, Type> NodeRef<marker::Mut<'a>, K, V, Type> {
    /// Borrows exclusive access to the length of the node.
    pub fn len_mut(&mut self) -> &mut u16 {
}
}

impl<'a, K, V> NodeRef<marker::Mut<'a>, K, V, marker::Internal> {
    /// # Safety
    /// Every item returned by `range` is a valid edge index for the node.
    unsafe fn correct_childrens_parent_links<R: Iterator<Item = usize>>(&mut self, range: R) {
}

    fn correct_all_childrens_parent_links(&mut self) {
}
}

impl<'a, K: 'a, V: 'a> NodeRef<marker::Mut<'a>, K, V, marker::LeafOrInternal> {
    /// Sets the node's link to its parent edge,
    /// without invalidating other references to the node.
    fn set_parent_link(&mut self, parent: NonNull<InternalNode<K, V>>, parent_idx: usize) {
}
}

impl<K, V> NodeRef<marker::Owned, K, V, marker::LeafOrInternal> {
    /// Clears the root's link to its parent edge.
    fn clear_parent_link(&mut self) {
}
}

impl<K, V> NodeRef<marker::Owned, K, V, marker::LeafOrInternal> {
    /// Returns a new owned tree, with its own root node that is initially empty.
    pub fn new() -> Self {
}

    /// Adds a new internal node with a single edge pointing to the previous root node,
    /// make that new node the root node, and return it. This increases the height by 1
    /// and is the opposite of `pop_internal_level`.
    pub fn push_internal_level(&mut self) -> NodeRef<marker::Mut<'_>, K, V, marker::Internal> {
}

    /// Removes the internal root node, using its first child as the new root node.
    /// As it is intended only to be called when the root node has only one child,
    /// no cleanup is done on any of the keys, values and other children.
    /// This decreases the height by 1 and is the opposite of `push_internal_level`.
    ///
    /// Requires exclusive access to the `Root` object but not to the root node;
    /// it will not invalidate other handles or references to the root node.
    ///
    /// Panics if there is no internal level, i.e., if the root node is a leaf.
    pub fn pop_internal_level(&mut self) {
}
}

impl<K, V, Type> NodeRef<marker::Owned, K, V, Type> {
    /// Mutably borrows the owned root node. Unlike `reborrow_mut`, this is safe
    /// because the return value cannot be used to destroy the root, and there
    /// cannot be other references to the tree.
    pub fn borrow_mut(&mut self) -> NodeRef<marker::Mut<'_>, K, V, Type> {
}

    /// Slightly mutably borrows the owned root node.
    pub fn borrow_valmut(&mut self) -> NodeRef<marker::ValMut<'_>, K, V, Type> {
}

    /// Irreversibly transitions to a reference that permits traversal and offers
    /// destructive methods and little else.
    pub fn into_dying(self) -> NodeRef<marker::Dying, K, V, Type> {
}
}

impl<'a, K: 'a, V: 'a> NodeRef<marker::Mut<'a>, K, V, marker::Leaf> {
    /// Adds a key-value pair to the end of the node.
    pub fn push(&mut self, key: K, val: V) {
}
}

impl<'a, K: 'a, V: 'a> NodeRef<marker::Mut<'a>, K, V, marker::Internal> {
    /// Adds a key-value pair, and an edge to go to the right of that pair,
    /// to the end of the node.
    pub fn push(&mut self, key: K, val: V, edge: Root<K, V>) {
}
}

impl<BorrowType, K, V> NodeRef<BorrowType, K, V, marker::Leaf> {
    /// Removes any static information asserting that this node is a `Leaf` node.
    pub fn forget_type(self) -> NodeRef<BorrowType, K, V, marker::LeafOrInternal> {
}
}

impl<BorrowType, K, V> NodeRef<BorrowType, K, V, marker::Internal> {
    /// Removes any static information asserting that this node is an `Internal` node.
    pub fn forget_type(self) -> NodeRef<BorrowType, K, V, marker::LeafOrInternal> {
}
}

impl<BorrowType, K, V> NodeRef<BorrowType, K, V, marker::LeafOrInternal> {
    /// Checks whether a node is an `Internal` node or a `Leaf` node.
    pub fn force(
        self,
    ) -> ForceResult<
        NodeRef<BorrowType, K, V, marker::Leaf>,
        NodeRef<BorrowType, K, V, marker::Internal>,
    > {
}
}

impl<'a, K, V> NodeRef<marker::Mut<'a>, K, V, marker::LeafOrInternal> {
    /// Unsafely asserts to the compiler the static information that this node is a `Leaf`.
    unsafe fn cast_to_leaf_unchecked(self) -> NodeRef<marker::Mut<'a>, K, V, marker::Leaf> {
}

    /// Unsafely asserts to the compiler the static information that this node is an `Internal`.
    unsafe fn cast_to_internal_unchecked(self) -> NodeRef<marker::Mut<'a>, K, V, marker::Internal> {
}
}

/// A reference to a specific key-value pair or edge within a node. The `Node` parameter
/// must be a `NodeRef`, while the `Type` can either be `KV` (signifying a handle on a key-value
/// pair) or `Edge` (signifying a handle on an edge).
///
/// Note that even `Leaf` nodes can have `Edge` handles. Instead of representing a pointer to
/// a child node, these represent the spaces where child pointers would go between the key-value
/// pairs. For example, in a node with length 2, there would be 3 possible edge locations - one
/// to the left of the node, one between the two pairs, and one at the right of the node.
pub struct Handle<Node, Type> {
    node: Node,
    idx: usize,
    _marker: PhantomData<Type>,
}

impl<Node: Copy, Type> Copy for Handle<Node, Type> {}
// We don't need the full generality of `#[derive(Clone)]`, as the only time `Node` will be
// `Clone`able is when it is an immutable reference and therefore `Copy`.
impl<Node: Copy, Type> Clone for Handle<Node, Type> {
    fn clone(&self) -> Self {
}
}

impl<Node, Type> Handle<Node, Type> {
    /// Retrieves the node that contains the edge or key-value pair this handle points to.
    pub fn into_node(self) -> Node {
}

    /// Returns the position of this handle in the node.
    pub fn idx(&self) -> usize {
}
}

impl<BorrowType, K, V, NodeType> Handle<NodeRef<BorrowType, K, V, NodeType>, marker::KV> {
    /// Creates a new handle to a key-value pair in `node`.
    /// Unsafe because the caller must ensure that `idx < node.len()`.
    pub unsafe fn new_kv(node: NodeRef<BorrowType, K, V, NodeType>, idx: usize) -> Self {
}

    pub fn left_edge(self) -> Handle<NodeRef<BorrowType, K, V, NodeType>, marker::Edge> {
}

    pub fn right_edge(self) -> Handle<NodeRef<BorrowType, K, V, NodeType>, marker::Edge> {
}
}

impl<BorrowType, K, V, NodeType, HandleType> PartialEq
    for Handle<NodeRef<BorrowType, K, V, NodeType>, HandleType>
{
    fn eq(&self, other: &Self) -> bool {
}
}

impl<BorrowType, K, V, NodeType, HandleType>
    Handle<NodeRef<BorrowType, K, V, NodeType>, HandleType>
{
    /// Temporarily takes out another immutable handle on the same location.
    pub fn reborrow(&self) -> Handle<NodeRef<marker::Immut<'_>, K, V, NodeType>, HandleType> {
}
}

impl<'a, K, V, NodeType, HandleType> Handle<NodeRef<marker::Mut<'a>, K, V, NodeType>, HandleType> {
    /// Temporarily takes out another mutable handle on the same location. Beware, as
    /// this method is very dangerous, doubly so since it might not immediately appear
    /// dangerous.
    ///
    /// For details, see `NodeRef::reborrow_mut`.
    pub unsafe fn reborrow_mut(
        &mut self,
    ) -> Handle<NodeRef<marker::Mut<'_>, K, V, NodeType>, HandleType> {
}
}

impl<BorrowType, K, V, NodeType> Handle<NodeRef<BorrowType, K, V, NodeType>, marker::Edge> {
    /// Creates a new handle to an edge in `node`.
    /// Unsafe because the caller must ensure that `idx <= node.len()`.
    pub unsafe fn new_edge(node: NodeRef<BorrowType, K, V, NodeType>, idx: usize) -> Self {
}

    pub fn left_kv(self) -> Result<Handle<NodeRef<BorrowType, K, V, NodeType>, marker::KV>, Self> {
}

    pub fn right_kv(self) -> Result<Handle<NodeRef<BorrowType, K, V, NodeType>, marker::KV>, Self> {
}
}

pub enum LeftOrRight<T> {
    Left(T),
    Right(T),
}

/// Given an edge index where we want to insert into a node filled to capacity,
/// computes a sensible KV index of a split point and where to perform the insertion.
/// The goal of the split point is for its key and value to end up in a parent node;
/// the keys, values and edges to the left of the split point become the left child;
/// the keys, values and edges to the right of the split point become the right child.
fn splitpoint(edge_idx: usize) -> (usize, LeftOrRight<usize>) {
}

impl<'a, K: 'a, V: 'a> Handle<NodeRef<marker::Mut<'a>, K, V, marker::Leaf>, marker::Edge> {
    /// Inserts a new key-value pair between the key-value pairs to the right and left of
    /// this edge. This method assumes that there is enough space in the node for the new
    /// pair to fit.
    ///
    /// The returned pointer points to the inserted value.
    fn insert_fit(&mut self, key: K, val: V) -> *mut V {
}
}

impl<'a, K: 'a, V: 'a> Handle<NodeRef<marker::Mut<'a>, K, V, marker::Leaf>, marker::Edge> {
    /// Inserts a new key-value pair between the key-value pairs to the right and left of
    /// this edge. This method splits the node if there isn't enough room.
    ///
    /// The returned pointer points to the inserted value.
    fn insert(mut self, key: K, val: V) -> (InsertResult<'a, K, V, marker::Leaf>, *mut V) {
}
}

impl<'a, K, V> Handle<NodeRef<marker::Mut<'a>, K, V, marker::Internal>, marker::Edge> {
    /// Fixes the parent pointer and index in the child node that this edge
    /// links to. This is useful when the ordering of edges has been changed,
    fn correct_parent_link(self) {
}
}

impl<'a, K: 'a, V: 'a> Handle<NodeRef<marker::Mut<'a>, K, V, marker::Internal>, marker::Edge> {
    /// Inserts a new key-value pair and an edge that will go to the right of that new pair
    /// between this edge and the key-value pair to the right of this edge. This method assumes
    /// that there is enough space in the node for the new pair to fit.
    fn insert_fit(&mut self, key: K, val: V, edge: Root<K, V>) {
}

    /// Inserts a new key-value pair and an edge that will go to the right of that new pair
    /// between this edge and the key-value pair to the right of this edge. This method splits
    /// the node if there isn't enough room.
    fn insert(
        mut self,
        key: K,
        val: V,
        edge: Root<K, V>,
    ) -> InsertResult<'a, K, V, marker::Internal> {
}
}

impl<'a, K: 'a, V: 'a> Handle<NodeRef<marker::Mut<'a>, K, V, marker::Leaf>, marker::Edge> {
    /// Inserts a new key-value pair between the key-value pairs to the right and left of
    /// this edge. This method splits the node if there isn't enough room, and tries to
    /// insert the split off portion into the parent node recursively, until the root is reached.
    ///
    /// If the returned result is a `Fit`, its handle's node can be this edge's node or an ancestor.
    /// If the returned result is a `Split`, the `left` field will be the root node.
    /// The returned pointer points to the inserted value.
    pub fn insert_recursing(
        self,
        key: K,
        value: V,
    ) -> (InsertResult<'a, K, V, marker::LeafOrInternal>, *mut V) {
}
}

impl<BorrowType: marker::BorrowType, K, V>
    Handle<NodeRef<BorrowType, K, V, marker::Internal>, marker::Edge>
{
    /// Finds the node pointed to by this edge.
    ///
    /// The method name assumes you picture trees with the root node on top.
    ///
    /// `edge.descend().ascend().unwrap()` and `node.ascend().unwrap().descend()` should
    /// both, upon success, do nothing.
    pub fn descend(self) -> NodeRef<BorrowType, K, V, marker::LeafOrInternal> {
}
}

impl<'a, K: 'a, V: 'a, NodeType> Handle<NodeRef<marker::Immut<'a>, K, V, NodeType>, marker::KV> {
    pub fn into_kv(self) -> (&'a K, &'a V) {
}
}

impl<'a, K: 'a, V: 'a, NodeType> Handle<NodeRef<marker::Mut<'a>, K, V, NodeType>, marker::KV> {
    pub fn key_mut(&mut self) -> &mut K {
}

    pub fn into_val_mut(self) -> &'a mut V {
}
}

impl<'a, K, V, NodeType> Handle<NodeRef<marker::ValMut<'a>, K, V, NodeType>, marker::KV> {
    pub fn into_kv_valmut(self) -> (&'a K, &'a mut V) {
}
}

impl<'a, K: 'a, V: 'a, NodeType> Handle<NodeRef<marker::Mut<'a>, K, V, NodeType>, marker::KV> {
    pub fn kv_mut(&mut self) -> (&mut K, &mut V) {
}

    /// Replaces the key and value that the KV handle refers to.
    pub fn replace_kv(&mut self, k: K, v: V) -> (K, V) {
}
}

impl<K, V, NodeType> Handle<NodeRef<marker::Dying, K, V, NodeType>, marker::KV> {
    /// Extracts the key and value that the KV handle refers to.
    /// # Safety
    /// The node that the handle refers to must not yet have been deallocated.
    pub unsafe fn into_key_val(mut self) -> (K, V) {
}

    /// Drops the key and value that the KV handle refers to.
    /// # Safety
    /// The node that the handle refers to must not yet have been deallocated.
    #[inline]
    pub unsafe fn drop_key_val(mut self) {
}
}

impl<'a, K: 'a, V: 'a, NodeType> Handle<NodeRef<marker::Mut<'a>, K, V, NodeType>, marker::KV> {
    /// Helps implementations of `split` for a particular `NodeType`,
    /// by taking care of leaf data.
    fn split_leaf_data(&mut self, new_node: &mut LeafNode<K, V>) -> (K, V) {
}
}

impl<'a, K: 'a, V: 'a> Handle<NodeRef<marker::Mut<'a>, K, V, marker::Leaf>, marker::KV> {
    /// Splits the underlying node into three parts:
    ///
    /// - The node is truncated to only contain the key-value pairs to the left of
    ///   this handle.
    /// - The key and value pointed to by this handle are extracted.
    /// - All the key-value pairs to the right of this handle are put into a newly
    ///   allocated node.
    pub fn split(mut self) -> SplitResult<'a, K, V, marker::Leaf> {
}

    /// Removes the key-value pair pointed to by this handle and returns it, along with the edge
    /// that the key-value pair collapsed into.
    pub fn remove(
        mut self,
    ) -> ((K, V), Handle<NodeRef<marker::Mut<'a>, K, V, marker::Leaf>, marker::Edge>) {
}
}

impl<'a, K: 'a, V: 'a> Handle<NodeRef<marker::Mut<'a>, K, V, marker::Internal>, marker::KV> {
    /// Splits the underlying node into three parts:
    ///
    /// - The node is truncated to only contain the edges and key-value pairs to the
    ///   left of this handle.
    /// - The key and value pointed to by this handle are extracted.
    /// - All the edges and key-value pairs to the right of this handle are put into
    ///   a newly allocated node.
    pub fn split(mut self) -> SplitResult<'a, K, V, marker::Internal> {
}
}

/// Represents a session for evaluating and performing a balancing operation
/// around an internal key-value pair.
pub struct BalancingContext<'a, K, V> {
    parent: Handle<NodeRef<marker::Mut<'a>, K, V, marker::Internal>, marker::KV>,
    left_child: NodeRef<marker::Mut<'a>, K, V, marker::LeafOrInternal>,
    right_child: NodeRef<marker::Mut<'a>, K, V, marker::LeafOrInternal>,
}

impl<'a, K, V> Handle<NodeRef<marker::Mut<'a>, K, V, marker::Internal>, marker::KV> {
    pub fn consider_for_balancing(self) -> BalancingContext<'a, K, V> {
}
}

impl<'a, K, V> NodeRef<marker::Mut<'a>, K, V, marker::LeafOrInternal> {
    /// Chooses a balancing context involving the node as a child, thus between
    /// the KV immediately to the left or to the right in the parent node.
    /// Returns an `Err` if there is no parent.
    /// Panics if the parent is empty.
    ///
    /// Prefers the left side, to be optimal if the given node is somehow
    /// underfull, meaning here only that it has fewer elements than its left
    /// sibling and than its right sibling, if they exist. In that case,
    /// merging with the left sibling is faster, since we only need to move
    /// the node's N elements, instead of shifting them to the right and moving
    /// more than N elements in front. Stealing from the left sibling is also
    /// typically faster, since we only need to shift the node's N elements to
    /// the right, instead of shifting at least N of the sibling's elements to
    /// the left.
    pub fn choose_parent_kv(self) -> Result<LeftOrRight<BalancingContext<'a, K, V>>, Self> {
}
}

impl<'a, K, V> BalancingContext<'a, K, V> {
    pub fn left_child_len(&self) -> usize {
}

    pub fn right_child_len(&self) -> usize {
}

    pub fn into_left_child(self) -> NodeRef<marker::Mut<'a>, K, V, marker::LeafOrInternal> {
}

    pub fn into_right_child(self) -> NodeRef<marker::Mut<'a>, K, V, marker::LeafOrInternal> {
}

    /// Returns whether merging is possible, i.e., whether there is enough room
    /// in a node to combine the central KV with both adjacent child nodes.
    pub fn can_merge(&self) -> bool {
}
}

impl<'a, K: 'a, V: 'a> BalancingContext<'a, K, V> {
    /// Performs a merge and lets a closure decide what to return.
    fn do_merge<
        F: FnOnce(
            NodeRef<marker::Mut<'a>, K, V, marker::Internal>,
            NodeRef<marker::Mut<'a>, K, V, marker::LeafOrInternal>,
        ) -> R,
        R,
    >(
        self,
        result: F,
    ) -> R {
}

    /// Merges the parent's key-value pair and both adjacent child nodes into
    /// the left child node and returns the shrunk parent node.
    ///
    /// Panics unless we `.can_merge()`.
    pub fn merge_tracking_parent(self) -> NodeRef<marker::Mut<'a>, K, V, marker::Internal> {
}

    /// Merges the parent's key-value pair and both adjacent child nodes into
    /// the left child node and returns that child node.
    ///
    /// Panics unless we `.can_merge()`.
    pub fn merge_tracking_child(self) -> NodeRef<marker::Mut<'a>, K, V, marker::LeafOrInternal> {
}

    /// Merges the parent's key-value pair and both adjacent child nodes into
    /// the left child node and returns the edge handle in that child node
    /// where the tracked child edge ended up,
    ///
    /// Panics unless we `.can_merge()`.
    pub fn merge_tracking_child_edge(
        self,
        track_edge_idx: LeftOrRight<usize>,
    ) -> Handle<NodeRef<marker::Mut<'a>, K, V, marker::LeafOrInternal>, marker::Edge> {
}

    /// Removes a key-value pair from the left child and places it in the key-value storage
    /// of the parent, while pushing the old parent key-value pair into the right child.
    /// Returns a handle to the edge in the right child corresponding to where the original
    /// edge specified by `track_right_edge_idx` ended up.
    pub fn steal_left(
        mut self,
        track_right_edge_idx: usize,
    ) -> Handle<NodeRef<marker::Mut<'a>, K, V, marker::LeafOrInternal>, marker::Edge> {
}

    /// Removes a key-value pair from the right child and places it in the key-value storage
    /// of the parent, while pushing the old parent key-value pair onto the left child.
    /// Returns a handle to the edge in the left child specified by `track_left_edge_idx`,
    /// which didn't move.
    pub fn steal_right(
        mut self,
        track_left_edge_idx: usize,
    ) -> Handle<NodeRef<marker::Mut<'a>, K, V, marker::LeafOrInternal>, marker::Edge> {
}

    /// This does stealing similar to `steal_left` but steals multiple elements at once.
    pub fn bulk_steal_left(&mut self, count: usize) {
}

    /// The symmetric clone of `bulk_steal_left`.
    pub fn bulk_steal_right(&mut self, count: usize) {
}
}

impl<BorrowType, K, V> Handle<NodeRef<BorrowType, K, V, marker::Leaf>, marker::Edge> {
    pub fn forget_node_type(
        self,
    ) -> Handle<NodeRef<BorrowType, K, V, marker::LeafOrInternal>, marker::Edge> {
}
}

impl<BorrowType, K, V> Handle<NodeRef<BorrowType, K, V, marker::Internal>, marker::Edge> {
    pub fn forget_node_type(
        self,
    ) -> Handle<NodeRef<BorrowType, K, V, marker::LeafOrInternal>, marker::Edge> {
}
}

impl<BorrowType, K, V> Handle<NodeRef<BorrowType, K, V, marker::Leaf>, marker::KV> {
    pub fn forget_node_type(
        self,
    ) -> Handle<NodeRef<BorrowType, K, V, marker::LeafOrInternal>, marker::KV> {
}
}

impl<BorrowType, K, V> Handle<NodeRef<BorrowType, K, V, marker::Internal>, marker::KV> {
    pub fn forget_node_type(
        self,
    ) -> Handle<NodeRef<BorrowType, K, V, marker::LeafOrInternal>, marker::KV> {
}
}

impl<BorrowType, K, V, Type> Handle<NodeRef<BorrowType, K, V, marker::LeafOrInternal>, Type> {
    /// Checks whether the underlying node is an `Internal` node or a `Leaf` node.
    pub fn force(
        self,
    ) -> ForceResult<
        Handle<NodeRef<BorrowType, K, V, marker::Leaf>, Type>,
        Handle<NodeRef<BorrowType, K, V, marker::Internal>, Type>,
    > {
}
}

impl<'a, K, V, Type> Handle<NodeRef<marker::Mut<'a>, K, V, marker::LeafOrInternal>, Type> {
    /// Unsafely asserts to the compiler the static information that the handle's node is a `Leaf`.
    pub unsafe fn cast_to_leaf_unchecked(
        self,
    ) -> Handle<NodeRef<marker::Mut<'a>, K, V, marker::Leaf>, Type> {
}
}

impl<'a, K, V> Handle<NodeRef<marker::Mut<'a>, K, V, marker::LeafOrInternal>, marker::Edge> {
    /// Move the suffix after `self` from one node to another one. `right` must be empty.
    /// The first edge of `right` remains unchanged.
    pub fn move_suffix(
        &mut self,
        right: &mut NodeRef<marker::Mut<'a>, K, V, marker::LeafOrInternal>,
    ) {
}
}

pub enum ForceResult<Leaf, Internal> {
    Leaf(Leaf),
    Internal(Internal),
}

/// Result of insertion, when a node needed to expand beyond its capacity.
pub struct SplitResult<'a, K, V, NodeType> {
    // Altered node in existing tree with elements and edges that belong to the left of `kv`.
    pub left: NodeRef<marker::Mut<'a>, K, V, NodeType>,
    // Some key and value split off, to be inserted elsewhere.
    pub kv: (K, V),
    // Owned, unattached, new node with elements and edges that belong to the right of `kv`.
    pub right: NodeRef<marker::Owned, K, V, NodeType>,
}

impl<'a, K, V> SplitResult<'a, K, V, marker::Leaf> {
    pub fn forget_node_type(self) -> SplitResult<'a, K, V, marker::LeafOrInternal> {
}
}

impl<'a, K, V> SplitResult<'a, K, V, marker::Internal> {
    pub fn forget_node_type(self) -> SplitResult<'a, K, V, marker::LeafOrInternal> {
}
}

pub enum InsertResult<'a, K, V, NodeType> {
    Fit(Handle<NodeRef<marker::Mut<'a>, K, V, NodeType>, marker::KV>),
    Split(SplitResult<'a, K, V, NodeType>),
}

pub mod marker {
    use core::marker::PhantomData;

    pub enum Leaf {}
    pub enum Internal {}
    pub enum LeafOrInternal {}

    pub enum Owned {}
    pub enum Dying {}
    pub struct Immut<'a>(PhantomData<&'a ()>);
    pub struct Mut<'a>(PhantomData<&'a mut ()>);
    pub struct ValMut<'a>(PhantomData<&'a mut ()>);

    pub trait BorrowType {
        // Whether node references of this borrow type allow traversing
        // to other nodes in the tree.
        const PERMITS_TRAVERSAL: bool = true;
    }
    impl BorrowType for Owned {
        // Traversal isn't needede, it happens using the result of `borrow_mut`.
        // By disabling traversal, and only creating new references to roots,
        // we know that every reference of the `Owned` type is to a root node.
        const PERMITS_TRAVERSAL: bool = false;
    }
    impl BorrowType for Dying {}
    impl<'a> BorrowType for Immut<'a> {}
    impl<'a> BorrowType for Mut<'a> {}
    impl<'a> BorrowType for ValMut<'a> {}

    pub enum KV {}
    pub enum Edge {}
}

/// Inserts a value into a slice of initialized elements followed by one uninitialized element.
///
/// # Safety
/// The slice has more than `idx` elements.
unsafe fn slice_insert<T>(slice: &mut [MaybeUninit<T>], idx: usize, val: T) {
}

/// Removes and returns a value from a slice of all initialized elements, leaving behind one
/// trailing uninitialized element.
///
/// # Safety
/// The slice has more than `idx` elements.
unsafe fn slice_remove<T>(slice: &mut [MaybeUninit<T>], idx: usize) -> T {
}

/// Shifts the elements in a slice `distance` positions to the left.
///
/// # Safety
/// The slice has at least `distance` elements.
unsafe fn slice_shl<T>(slice: &mut [MaybeUninit<T>], distance: usize) {
}

/// Shifts the elements in a slice `distance` positions to the right.
///
/// # Safety
/// The slice has at least `distance` elements.
unsafe fn slice_shr<T>(slice: &mut [MaybeUninit<T>], distance: usize) {
}

/// Moves all values from a slice of initialized elements to a slice
/// of uninitialized elements, leaving behind `src` as all uninitialized.
/// Works like `dst.copy_from_slice(src)` but does not require `T` to be `Copy`.
fn move_to_slice<T>(src: &mut [MaybeUninit<T>], dst: &mut [MaybeUninit<T>]) {
}

#[cfg(test)]
mod tests {
}
}
mod remove {
use super::map::MIN_LEN;
use super::node::{marker, ForceResult::*, Handle, LeftOrRight::*, NodeRef};

impl<'a, K: 'a, V: 'a> Handle<NodeRef<marker::Mut<'a>, K, V, marker::LeafOrInternal>, marker::KV> {
    /// Removes a key-value pair from the tree, and returns that pair, as well as
    /// the leaf edge corresponding to that former pair. It's possible this empties
    /// a root node that is internal, which the caller should pop from the map
    /// holding the tree. The caller should also decrement the map's length.
    pub fn remove_kv_tracking<F: FnOnce()>(
        self,
        handle_emptied_internal_root: F,
    ) -> ((K, V), Handle<NodeRef<marker::Mut<'a>, K, V, marker::Leaf>, marker::Edge>) {
}
}

impl<'a, K: 'a, V: 'a> Handle<NodeRef<marker::Mut<'a>, K, V, marker::Leaf>, marker::KV> {
    fn remove_leaf_kv<F: FnOnce()>(
        self,
        handle_emptied_internal_root: F,
    ) -> ((K, V), Handle<NodeRef<marker::Mut<'a>, K, V, marker::Leaf>, marker::Edge>) {
}
}

impl<'a, K: 'a, V: 'a> Handle<NodeRef<marker::Mut<'a>, K, V, marker::Internal>, marker::KV> {
    fn remove_internal_kv<F: FnOnce()>(
        self,
        handle_emptied_internal_root: F,
    ) -> ((K, V), Handle<NodeRef<marker::Mut<'a>, K, V, marker::Leaf>, marker::Edge>) {
}
}
}
mod search {
use core::borrow::Borrow;
use core::cmp::Ordering;
use core::ops::{Bound, RangeBounds};

use super::node::{marker, ForceResult::*, Handle, NodeRef};

use SearchBound::*;
use SearchResult::*;

pub enum SearchBound<T> {
    /// An inclusive bound to look for, just like `Bound::Included(T)`.
    Included(T),
    /// An exclusive bound to look for, just like `Bound::Excluded(T)`.
    Excluded(T),
    /// An unconditional inclusive bound, just like `Bound::Unbounded`.
    AllIncluded,
    /// An unconditional exclusive bound.
    AllExcluded,
}

impl<T> SearchBound<T> {
    pub fn from_range(range_bound: Bound<T>) -> Self {
}
}

pub enum SearchResult<BorrowType, K, V, FoundType, GoDownType> {
    Found(Handle<NodeRef<BorrowType, K, V, FoundType>, marker::KV>),
    GoDown(Handle<NodeRef<BorrowType, K, V, GoDownType>, marker::Edge>),
}

pub enum IndexResult {
    KV(usize),
    Edge(usize),
}

impl<BorrowType: marker::BorrowType, K, V> NodeRef<BorrowType, K, V, marker::LeafOrInternal> {
    /// Looks up a given key in a (sub)tree headed by the node, recursively.
    /// Returns a `Found` with the handle of the matching KV, if any. Otherwise,
    /// returns a `GoDown` with the handle of the leaf edge where the key belongs.
    ///
    /// The result is meaningful only if the tree is ordered by key, like the tree
    /// in a `BTreeMap` is.
    pub fn search_tree<Q: ?Sized>(
        mut self,
        key: &Q,
    ) -> SearchResult<BorrowType, K, V, marker::LeafOrInternal, marker::Leaf>
    where
        Q: Ord,
        K: Borrow<Q>,
    {
}

    /// Descends to the nearest node where the edge matching the lower bound
    /// of the range is different from the edge matching the upper bound, i.e.,
    /// the nearest node that has at least one key contained in the range.
    ///
    /// If found, returns an `Ok` with that node, the strictly ascending pair of
    /// edge indices in the node delimiting the range, and the corresponding
    /// pair of bounds for continuing the search in the child nodes, in case
    /// the node is internal.
    ///
    /// If not found, returns an `Err` with the leaf edge matching the entire
    /// range.
    ///
    /// As a diagnostic service, panics if the range specifies impossible bounds.
    ///
    /// The result is meaningful only if the tree is ordered by key.
    pub fn search_tree_for_bifurcation<'r, Q: ?Sized, R>(
        mut self,
        range: &'r R,
    ) -> Result<
        (
            NodeRef<BorrowType, K, V, marker::LeafOrInternal>,
            usize,
            usize,
            SearchBound<&'r Q>,
            SearchBound<&'r Q>,
        ),
        Handle<NodeRef<BorrowType, K, V, marker::Leaf>, marker::Edge>,
    >
    where
        Q: Ord,
        K: Borrow<Q>,
        R: RangeBounds<Q>,
    {
}

    /// Finds an edge in the node delimiting the lower bound of a range.
    /// Also returns the lower bound to be used for continuing the search in
    /// the matching child node, if `self` is an internal node.
    ///
    /// The result is meaningful only if the tree is ordered by key.
    pub fn find_lower_bound_edge<'r, Q>(
        self,
        bound: SearchBound<&'r Q>,
    ) -> (Handle<Self, marker::Edge>, SearchBound<&'r Q>)
    where
        Q: ?Sized + Ord,
        K: Borrow<Q>,
    {
}

    /// Clone of `find_lower_bound_edge` for the upper bound.
    pub fn find_upper_bound_edge<'r, Q>(
        self,
        bound: SearchBound<&'r Q>,
    ) -> (Handle<Self, marker::Edge>, SearchBound<&'r Q>)
    where
        Q: ?Sized + Ord,
        K: Borrow<Q>,
    {
}
}

impl<BorrowType, K, V, Type> NodeRef<BorrowType, K, V, Type> {
    /// Looks up a given key in the node, without recursion.
    /// Returns a `Found` with the handle of the matching KV, if any. Otherwise,
    /// returns a `GoDown` with the handle of the edge where the key might be found
    /// (if the node is internal) or where the key can be inserted.
    ///
    /// The result is meaningful only if the tree is ordered by key, like the tree
    /// in a `BTreeMap` is.
    pub fn search_node<Q: ?Sized>(self, key: &Q) -> SearchResult<BorrowType, K, V, Type, Type>
    where
        Q: Ord,
        K: Borrow<Q>,
    {
}

    /// Returns either the KV index in the node at which the key (or an equivalent)
    /// exists, or the edge index where the key belongs, starting from a particular index.
    ///
    /// The result is meaningful only if the tree is ordered by key, like the tree
    /// in a `BTreeMap` is.
    ///
    /// # Safety
    /// `start_index` must be a valid edge index for the node.
    unsafe fn find_key_index<Q: ?Sized>(&self, key: &Q, start_index: usize) -> IndexResult
    where
        Q: Ord,
        K: Borrow<Q>,
    {
}

    /// Finds an edge index in the node delimiting the lower bound of a range.
    /// Also returns the lower bound to be used for continuing the search in
    /// the matching child node, if `self` is an internal node.
    ///
    /// The result is meaningful only if the tree is ordered by key.
    fn find_lower_bound_index<'r, Q>(
        &self,
        bound: SearchBound<&'r Q>,
    ) -> (usize, SearchBound<&'r Q>)
    where
        Q: ?Sized + Ord,
        K: Borrow<Q>,
    {
}

    /// Mirror image of `find_lower_bound_index` for the upper bound,
    /// with an additional parameter to skip part of the key array.
    ///
    /// # Safety
    /// `start_index` must be a valid edge index for the node.
    unsafe fn find_upper_bound_index<'r, Q>(
        &self,
        bound: SearchBound<&'r Q>,
        start_index: usize,
    ) -> (usize, SearchBound<&'r Q>)
    where
        Q: ?Sized + Ord,
        K: Borrow<Q>,
    {
}
}
}
pub mod set {
// This is pretty much entirely stolen from TreeSet, since BTreeMap has an identical interface
// to TreeMap

use core::borrow::Borrow;
use core::cmp::Ordering::{Equal, Greater, Less};
use core::cmp::{max, min};
use core::fmt::{self, Debug};
use core::iter::{FromIterator, FusedIterator, Peekable};
use core::ops::{BitAnd, BitOr, BitXor, RangeBounds, Sub};

use super::map::{BTreeMap, Keys};
use super::merge_iter::MergeIterInner;
use super::Recover;

// FIXME(conventions): implement bounded iterators

/// A set based on a B-Tree.
///
/// See [`BTreeMap`]'s documentation for a detailed discussion of this collection's performance
/// benefits and drawbacks.
///
/// It is a logic error for an item to be modified in such a way that the item's ordering relative
/// to any other item, as determined by the [`Ord`] trait, changes while it is in the set. This is
/// normally only possible through [`Cell`], [`RefCell`], global state, I/O, or unsafe code.
/// The behavior resulting from such a logic error is not specified, but will not result in
/// undefined behavior. This could include panics, incorrect results, aborts, memory leaks, and
/// non-termination.
///
/// [`Ord`]: core::cmp::Ord
/// [`Cell`]: core::cell::Cell
/// [`RefCell`]: core::cell::RefCell
///
/// # Examples
///
/// ```
/// use std::collections::BTreeSet;
///
/// // Type inference lets us omit an explicit type signature (which
/// // would be `BTreeSet<&str>` in this example).
/// let mut books = BTreeSet::new();
///
/// // Add some books.
/// books.insert("A Dance With Dragons");
/// books.insert("To Kill a Mockingbird");
/// books.insert("The Odyssey");
/// books.insert("The Great Gatsby");
///
/// // Check for a specific one.
/// if !books.contains("The Winds of Winter") {
///     println!("We have {} books, but The Winds of Winter ain't one.",
///              books.len());
/// }
///
/// // Remove a book.
/// books.remove("The Odyssey");
///
/// // Iterate over everything.
/// for book in &books {
///     println!("{}", book);
/// }
/// ```
///
/// A `BTreeSet` with a known list of items can be initialized from an array:
///
/// ```
/// use std::collections::BTreeSet;
///
/// let set = BTreeSet::from([1, 2, 3]);
/// ```
#[derive(Hash, PartialEq, Eq, Ord, PartialOrd)]
#[stable(feature = "rust1", since = "1.0.0")]
#[cfg_attr(not(test), rustc_diagnostic_item = "BTreeSet")]
pub struct BTreeSet<T> {
    map: BTreeMap<T, ()>,
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<T: Clone> Clone for BTreeSet<T> {
    fn clone(&self) -> Self {
}

    fn clone_from(&mut self, other: &Self) {
}
}

/// An iterator over the items of a `BTreeSet`.
///
/// This `struct` is created by the [`iter`] method on [`BTreeSet`].
/// See its documentation for more.
///
/// [`iter`]: BTreeSet::iter
#[stable(feature = "rust1", since = "1.0.0")]
pub struct Iter<'a, T: 'a> {
    iter: Keys<'a, T, ()>,
}

#[stable(feature = "collection_debug", since = "1.17.0")]
impl<T: fmt::Debug> fmt::Debug for Iter<'_, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
}
}

/// An owning iterator over the items of a `BTreeSet`.
///
/// This `struct` is created by the [`into_iter`] method on [`BTreeSet`]
/// (provided by the `IntoIterator` trait). See its documentation for more.
///
/// [`into_iter`]: BTreeSet#method.into_iter
#[stable(feature = "rust1", since = "1.0.0")]
#[derive(Debug)]
pub struct IntoIter<T> {
    iter: super::map::IntoIter<T, ()>,
}

/// An iterator over a sub-range of items in a `BTreeSet`.
///
/// This `struct` is created by the [`range`] method on [`BTreeSet`].
/// See its documentation for more.
///
/// [`range`]: BTreeSet::range
#[derive(Debug)]
#[stable(feature = "btree_range", since = "1.17.0")]
pub struct Range<'a, T: 'a> {
    iter: super::map::Range<'a, T, ()>,
}

/// A lazy iterator producing elements in the difference of `BTreeSet`s.
///
/// This `struct` is created by the [`difference`] method on [`BTreeSet`].
/// See its documentation for more.
///
/// [`difference`]: BTreeSet::difference
#[stable(feature = "rust1", since = "1.0.0")]
pub struct Difference<'a, T: 'a> {
    inner: DifferenceInner<'a, T>,
}
#[derive(Debug)]
enum DifferenceInner<'a, T: 'a> {
    Stitch {
        // iterate all of `self` and some of `other`, spotting matches along the way
        self_iter: Iter<'a, T>,
        other_iter: Peekable<Iter<'a, T>>,
    },
    Search {
        // iterate `self`, look up in `other`
        self_iter: Iter<'a, T>,
        other_set: &'a BTreeSet<T>,
    },
    Iterate(Iter<'a, T>), // simply produce all values in `self`
}

#[stable(feature = "collection_debug", since = "1.17.0")]
impl<T: fmt::Debug> fmt::Debug for Difference<'_, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
}
}

/// A lazy iterator producing elements in the symmetric difference of `BTreeSet`s.
///
/// This `struct` is created by the [`symmetric_difference`] method on
/// [`BTreeSet`]. See its documentation for more.
///
/// [`symmetric_difference`]: BTreeSet::symmetric_difference
#[stable(feature = "rust1", since = "1.0.0")]
pub struct SymmetricDifference<'a, T: 'a>(MergeIterInner<Iter<'a, T>>);

#[stable(feature = "collection_debug", since = "1.17.0")]
impl<T: fmt::Debug> fmt::Debug for SymmetricDifference<'_, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
}
}

/// A lazy iterator producing elements in the intersection of `BTreeSet`s.
///
/// This `struct` is created by the [`intersection`] method on [`BTreeSet`].
/// See its documentation for more.
///
/// [`intersection`]: BTreeSet::intersection
#[stable(feature = "rust1", since = "1.0.0")]
pub struct Intersection<'a, T: 'a> {
    inner: IntersectionInner<'a, T>,
}
#[derive(Debug)]
enum IntersectionInner<'a, T: 'a> {
    Stitch {
        // iterate similarly sized sets jointly, spotting matches along the way
        a: Iter<'a, T>,
        b: Iter<'a, T>,
    },
    Search {
        // iterate a small set, look up in the large set
        small_iter: Iter<'a, T>,
        large_set: &'a BTreeSet<T>,
    },
    Answer(Option<&'a T>), // return a specific value or emptiness
}

#[stable(feature = "collection_debug", since = "1.17.0")]
impl<T: fmt::Debug> fmt::Debug for Intersection<'_, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
}
}

/// A lazy iterator producing elements in the union of `BTreeSet`s.
///
/// This `struct` is created by the [`union`] method on [`BTreeSet`].
/// See its documentation for more.
///
/// [`union`]: BTreeSet::union
#[stable(feature = "rust1", since = "1.0.0")]
pub struct Union<'a, T: 'a>(MergeIterInner<Iter<'a, T>>);

#[stable(feature = "collection_debug", since = "1.17.0")]
impl<T: fmt::Debug> fmt::Debug for Union<'_, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
}
}

// This constant is used by functions that compare two sets.
// It estimates the relative size at which searching performs better
// than iterating, based on the benchmarks in
// https://github.com/ssomers/rust_bench_btreeset_intersection.
// It's used to divide rather than multiply sizes, to rule out overflow,
// and it's a power of two to make that division cheap.
const ITER_PERFORMANCE_TIPPING_SIZE_DIFF: usize = 16;

impl<T> BTreeSet<T> {
    /// Makes a new, empty `BTreeSet`.
    ///
    /// Does not allocate anything on its own.
    ///
    /// # Examples
    ///
    /// ```
    /// # #![allow(unused_mut)]
    /// use std::collections::BTreeSet;
    ///
    /// let mut set: BTreeSet<i32> = BTreeSet::new();
    /// ```
    #[stable(feature = "rust1", since = "1.0.0")]
    #[rustc_const_unstable(feature = "const_btree_new", issue = "71835")]
    pub const fn new() -> BTreeSet<T> {
}

    /// Constructs a double-ended iterator over a sub-range of elements in the set.
    /// The simplest way is to use the range syntax `min..max`, thus `range(min..max)` will
    /// yield elements from min (inclusive) to max (exclusive).
    /// The range may also be entered as `(Bound<T>, Bound<T>)`, so for example
    /// `range((Excluded(4), Included(10)))` will yield a left-exclusive, right-inclusive
    /// range from 4 to 10.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::collections::BTreeSet;
    /// use std::ops::Bound::Included;
    ///
    /// let mut set = BTreeSet::new();
    /// set.insert(3);
    /// set.insert(5);
    /// set.insert(8);
    /// for &elem in set.range((Included(&4), Included(&8))) {
    ///     println!("{}", elem);
    /// }
    /// assert_eq!(Some(&5), set.range(4..).next());
    /// ```
    #[stable(feature = "btree_range", since = "1.17.0")]
    pub fn range<K: ?Sized, R>(&self, range: R) -> Range<'_, T>
    where
        K: Ord,
        T: Borrow<K> + Ord,
        R: RangeBounds<K>,
    {
}

    /// Visits the values representing the difference,
    /// i.e., the values that are in `self` but not in `other`,
    /// in ascending order.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::collections::BTreeSet;
    ///
    /// let mut a = BTreeSet::new();
    /// a.insert(1);
    /// a.insert(2);
    ///
    /// let mut b = BTreeSet::new();
    /// b.insert(2);
    /// b.insert(3);
    ///
    /// let diff: Vec<_> = a.difference(&b).cloned().collect();
    /// assert_eq!(diff, [1]);
    /// ```
    #[stable(feature = "rust1", since = "1.0.0")]
    pub fn difference<'a>(&'a self, other: &'a BTreeSet<T>) -> Difference<'a, T>
    where
        T: Ord,
    {
}

    /// Visits the values representing the symmetric difference,
    /// i.e., the values that are in `self` or in `other` but not in both,
    /// in ascending order.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::collections::BTreeSet;
    ///
    /// let mut a = BTreeSet::new();
    /// a.insert(1);
    /// a.insert(2);
    ///
    /// let mut b = BTreeSet::new();
    /// b.insert(2);
    /// b.insert(3);
    ///
    /// let sym_diff: Vec<_> = a.symmetric_difference(&b).cloned().collect();
    /// assert_eq!(sym_diff, [1, 3]);
    /// ```
    #[stable(feature = "rust1", since = "1.0.0")]
    pub fn symmetric_difference<'a>(&'a self, other: &'a BTreeSet<T>) -> SymmetricDifference<'a, T>
    where
        T: Ord,
    {
}

    /// Visits the values representing the intersection,
    /// i.e., the values that are both in `self` and `other`,
    /// in ascending order.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::collections::BTreeSet;
    ///
    /// let mut a = BTreeSet::new();
    /// a.insert(1);
    /// a.insert(2);
    ///
    /// let mut b = BTreeSet::new();
    /// b.insert(2);
    /// b.insert(3);
    ///
    /// let intersection: Vec<_> = a.intersection(&b).cloned().collect();
    /// assert_eq!(intersection, [2]);
    /// ```
    #[stable(feature = "rust1", since = "1.0.0")]
    pub fn intersection<'a>(&'a self, other: &'a BTreeSet<T>) -> Intersection<'a, T>
    where
        T: Ord,
    {
}

    /// Visits the values representing the union,
    /// i.e., all the values in `self` or `other`, without duplicates,
    /// in ascending order.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::collections::BTreeSet;
    ///
    /// let mut a = BTreeSet::new();
    /// a.insert(1);
    ///
    /// let mut b = BTreeSet::new();
    /// b.insert(2);
    ///
    /// let union: Vec<_> = a.union(&b).cloned().collect();
    /// assert_eq!(union, [1, 2]);
    /// ```
    #[stable(feature = "rust1", since = "1.0.0")]
    pub fn union<'a>(&'a self, other: &'a BTreeSet<T>) -> Union<'a, T>
    where
        T: Ord,
    {
}

    /// Clears the set, removing all values.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::collections::BTreeSet;
    ///
    /// let mut v = BTreeSet::new();
    /// v.insert(1);
    /// v.clear();
    /// assert!(v.is_empty());
    /// ```
    #[stable(feature = "rust1", since = "1.0.0")]
    pub fn clear(&mut self) {
}

    /// Returns `true` if the set contains a value.
    ///
    /// The value may be any borrowed form of the set's value type,
    /// but the ordering on the borrowed form *must* match the
    /// ordering on the value type.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::collections::BTreeSet;
    ///
    /// let set: BTreeSet<_> = [1, 2, 3].iter().cloned().collect();
    /// assert_eq!(set.contains(&1), true);
    /// assert_eq!(set.contains(&4), false);
    /// ```
    #[stable(feature = "rust1", since = "1.0.0")]
    pub fn contains<Q: ?Sized>(&self, value: &Q) -> bool
    where
        T: Borrow<Q> + Ord,
        Q: Ord,
    {
}

    /// Returns a reference to the value in the set, if any, that is equal to the given value.
    ///
    /// The value may be any borrowed form of the set's value type,
    /// but the ordering on the borrowed form *must* match the
    /// ordering on the value type.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::collections::BTreeSet;
    ///
    /// let set: BTreeSet<_> = [1, 2, 3].iter().cloned().collect();
    /// assert_eq!(set.get(&2), Some(&2));
    /// assert_eq!(set.get(&4), None);
    /// ```
    #[stable(feature = "set_recovery", since = "1.9.0")]
    pub fn get<Q: ?Sized>(&self, value: &Q) -> Option<&T>
    where
        T: Borrow<Q> + Ord,
        Q: Ord,
    {
}

    /// Returns `true` if `self` has no elements in common with `other`.
    /// This is equivalent to checking for an empty intersection.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::collections::BTreeSet;
    ///
    /// let a: BTreeSet<_> = [1, 2, 3].iter().cloned().collect();
    /// let mut b = BTreeSet::new();
    ///
    /// assert_eq!(a.is_disjoint(&b), true);
    /// b.insert(4);
    /// assert_eq!(a.is_disjoint(&b), true);
    /// b.insert(1);
    /// assert_eq!(a.is_disjoint(&b), false);
    /// ```
    #[stable(feature = "rust1", since = "1.0.0")]
    pub fn is_disjoint(&self, other: &BTreeSet<T>) -> bool
    where
        T: Ord,
    {
}

    /// Returns `true` if the set is a subset of another,
    /// i.e., `other` contains at least all the values in `self`.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::collections::BTreeSet;
    ///
    /// let sup: BTreeSet<_> = [1, 2, 3].iter().cloned().collect();
    /// let mut set = BTreeSet::new();
    ///
    /// assert_eq!(set.is_subset(&sup), true);
    /// set.insert(2);
    /// assert_eq!(set.is_subset(&sup), true);
    /// set.insert(4);
    /// assert_eq!(set.is_subset(&sup), false);
    /// ```
    #[stable(feature = "rust1", since = "1.0.0")]
    pub fn is_subset(&self, other: &BTreeSet<T>) -> bool
    where
        T: Ord,
    {
}

    /// Returns `true` if the set is a superset of another,
    /// i.e., `self` contains at least all the values in `other`.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::collections::BTreeSet;
    ///
    /// let sub: BTreeSet<_> = [1, 2].iter().cloned().collect();
    /// let mut set = BTreeSet::new();
    ///
    /// assert_eq!(set.is_superset(&sub), false);
    ///
    /// set.insert(0);
    /// set.insert(1);
    /// assert_eq!(set.is_superset(&sub), false);
    ///
    /// set.insert(2);
    /// assert_eq!(set.is_superset(&sub), true);
    /// ```
    #[stable(feature = "rust1", since = "1.0.0")]
    pub fn is_superset(&self, other: &BTreeSet<T>) -> bool
    where
        T: Ord,
    {
}

    /// Returns a reference to the first value in the set, if any.
    /// This value is always the minimum of all values in the set.
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// #![feature(map_first_last)]
    /// use std::collections::BTreeSet;
    ///
    /// let mut set = BTreeSet::new();
    /// assert_eq!(set.first(), None);
    /// set.insert(1);
    /// assert_eq!(set.first(), Some(&1));
    /// set.insert(2);
    /// assert_eq!(set.first(), Some(&1));
    /// ```
    #[unstable(feature = "map_first_last", issue = "62924")]
    pub fn first(&self) -> Option<&T>
    where
        T: Ord,
    {
}

    /// Returns a reference to the last value in the set, if any.
    /// This value is always the maximum of all values in the set.
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// #![feature(map_first_last)]
    /// use std::collections::BTreeSet;
    ///
    /// let mut set = BTreeSet::new();
    /// assert_eq!(set.last(), None);
    /// set.insert(1);
    /// assert_eq!(set.last(), Some(&1));
    /// set.insert(2);
    /// assert_eq!(set.last(), Some(&2));
    /// ```
    #[unstable(feature = "map_first_last", issue = "62924")]
    pub fn last(&self) -> Option<&T>
    where
        T: Ord,
    {
}

    /// Removes the first value from the set and returns it, if any.
    /// The first value is always the minimum value in the set.
    ///
    /// # Examples
    ///
    /// ```
    /// #![feature(map_first_last)]
    /// use std::collections::BTreeSet;
    ///
    /// let mut set = BTreeSet::new();
    ///
    /// set.insert(1);
    /// while let Some(n) = set.pop_first() {
    ///     assert_eq!(n, 1);
    /// }
    /// assert!(set.is_empty());
    /// ```
    #[unstable(feature = "map_first_last", issue = "62924")]
    pub fn pop_first(&mut self) -> Option<T>
    where
        T: Ord,
    {
}

    /// Removes the last value from the set and returns it, if any.
    /// The last value is always the maximum value in the set.
    ///
    /// # Examples
    ///
    /// ```
    /// #![feature(map_first_last)]
    /// use std::collections::BTreeSet;
    ///
    /// let mut set = BTreeSet::new();
    ///
    /// set.insert(1);
    /// while let Some(n) = set.pop_last() {
    ///     assert_eq!(n, 1);
    /// }
    /// assert!(set.is_empty());
    /// ```
    #[unstable(feature = "map_first_last", issue = "62924")]
    pub fn pop_last(&mut self) -> Option<T>
    where
        T: Ord,
    {
}

    /// Adds a value to the set.
    ///
    /// If the set did not have this value present, `true` is returned.
    ///
    /// If the set did have this value present, `false` is returned, and the
    /// entry is not updated. See the [module-level documentation] for more.
    ///
    /// [module-level documentation]: index.html#insert-and-complex-keys
    ///
    /// # Examples
    ///
    /// ```
    /// use std::collections::BTreeSet;
    ///
    /// let mut set = BTreeSet::new();
    ///
    /// assert_eq!(set.insert(2), true);
    /// assert_eq!(set.insert(2), false);
    /// assert_eq!(set.len(), 1);
    /// ```
    #[stable(feature = "rust1", since = "1.0.0")]
    pub fn insert(&mut self, value: T) -> bool
    where
        T: Ord,
    {
}

    /// Adds a value to the set, replacing the existing value, if any, that is equal to the given
    /// one. Returns the replaced value.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::collections::BTreeSet;
    ///
    /// let mut set = BTreeSet::new();
    /// set.insert(Vec::<i32>::new());
    ///
    /// assert_eq!(set.get(&[][..]).unwrap().capacity(), 0);
    /// set.replace(Vec::with_capacity(10));
    /// assert_eq!(set.get(&[][..]).unwrap().capacity(), 10);
    /// ```
    #[stable(feature = "set_recovery", since = "1.9.0")]
    pub fn replace(&mut self, value: T) -> Option<T>
    where
        T: Ord,
    {
}

    /// Removes a value from the set. Returns whether the value was
    /// present in the set.
    ///
    /// The value may be any borrowed form of the set's value type,
    /// but the ordering on the borrowed form *must* match the
    /// ordering on the value type.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::collections::BTreeSet;
    ///
    /// let mut set = BTreeSet::new();
    ///
    /// set.insert(2);
    /// assert_eq!(set.remove(&2), true);
    /// assert_eq!(set.remove(&2), false);
    /// ```
    #[stable(feature = "rust1", since = "1.0.0")]
    pub fn remove<Q: ?Sized>(&mut self, value: &Q) -> bool
    where
        T: Borrow<Q> + Ord,
        Q: Ord,
    {
}

    /// Removes and returns the value in the set, if any, that is equal to the given one.
    ///
    /// The value may be any borrowed form of the set's value type,
    /// but the ordering on the borrowed form *must* match the
    /// ordering on the value type.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::collections::BTreeSet;
    ///
    /// let mut set: BTreeSet<_> = [1, 2, 3].iter().cloned().collect();
    /// assert_eq!(set.take(&2), Some(2));
    /// assert_eq!(set.take(&2), None);
    /// ```
    #[stable(feature = "set_recovery", since = "1.9.0")]
    pub fn take<Q: ?Sized>(&mut self, value: &Q) -> Option<T>
    where
        T: Borrow<Q> + Ord,
        Q: Ord,
    {
}

    /// Retains only the elements specified by the predicate.
    ///
    /// In other words, remove all elements `e` such that `f(&e)` returns `false`.
    /// The elements are visited in ascending order.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::collections::BTreeSet;
    ///
    /// let xs = [1, 2, 3, 4, 5, 6];
    /// let mut set: BTreeSet<i32> = xs.iter().cloned().collect();
    /// // Keep only the even numbers.
    /// set.retain(|&k| k % 2 == 0);
    /// assert!(set.iter().eq([2, 4, 6].iter()));
    /// ```
    #[stable(feature = "btree_retain", since = "1.53.0")]
    pub fn retain<F>(&mut self, mut f: F)
    where
        T: Ord,
        F: FnMut(&T) -> bool,
    {
}

    /// Moves all elements from `other` into `Self`, leaving `other` empty.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::collections::BTreeSet;
    ///
    /// let mut a = BTreeSet::new();
    /// a.insert(1);
    /// a.insert(2);
    /// a.insert(3);
    ///
    /// let mut b = BTreeSet::new();
    /// b.insert(3);
    /// b.insert(4);
    /// b.insert(5);
    ///
    /// a.append(&mut b);
    ///
    /// assert_eq!(a.len(), 5);
    /// assert_eq!(b.len(), 0);
    ///
    /// assert!(a.contains(&1));
    /// assert!(a.contains(&2));
    /// assert!(a.contains(&3));
    /// assert!(a.contains(&4));
    /// assert!(a.contains(&5));
    /// ```
    #[stable(feature = "btree_append", since = "1.11.0")]
    pub fn append(&mut self, other: &mut Self)
    where
        T: Ord,
    {
}

    /// Splits the collection into two at the given value. Returns everything after the given value,
    /// including the value.
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// use std::collections::BTreeSet;
    ///
    /// let mut a = BTreeSet::new();
    /// a.insert(1);
    /// a.insert(2);
    /// a.insert(3);
    /// a.insert(17);
    /// a.insert(41);
    ///
    /// let b = a.split_off(&3);
    ///
    /// assert_eq!(a.len(), 2);
    /// assert_eq!(b.len(), 3);
    ///
    /// assert!(a.contains(&1));
    /// assert!(a.contains(&2));
    ///
    /// assert!(b.contains(&3));
    /// assert!(b.contains(&17));
    /// assert!(b.contains(&41));
    /// ```
    #[stable(feature = "btree_split_off", since = "1.11.0")]
    pub fn split_off<Q: ?Sized + Ord>(&mut self, value: &Q) -> Self
    where
        T: Borrow<Q> + Ord,
    {
}

    /// Creates an iterator that visits all values in ascending order and uses a closure
    /// to determine if a value should be removed.
    ///
    /// If the closure returns `true`, the value is removed from the set and yielded. If
    /// the closure returns `false`, or panics, the value remains in the set and will
    /// not be yielded.
    ///
    /// If the iterator is only partially consumed or not consumed at all, each of the
    /// remaining values is still subjected to the closure and removed and dropped if it
    /// returns `true`.
    ///
    /// It is unspecified how many more values will be subjected to the closure if a
    /// panic occurs in the closure, or if a panic occurs while dropping a value, or if
    /// the `DrainFilter` itself is leaked.
    ///
    /// # Examples
    ///
    /// Splitting a set into even and odd values, reusing the original set:
    ///
    /// ```
    /// #![feature(btree_drain_filter)]
    /// use std::collections::BTreeSet;
    ///
    /// let mut set: BTreeSet<i32> = (0..8).collect();
    /// let evens: BTreeSet<_> = set.drain_filter(|v| v % 2 == 0).collect();
    /// let odds = set;
    /// assert_eq!(evens.into_iter().collect::<Vec<_>>(), vec![0, 2, 4, 6]);
    /// assert_eq!(odds.into_iter().collect::<Vec<_>>(), vec![1, 3, 5, 7]);
    /// ```
    #[unstable(feature = "btree_drain_filter", issue = "70530")]
    pub fn drain_filter<'a, F>(&'a mut self, pred: F) -> DrainFilter<'a, T, F>
    where
        T: Ord,
        F: 'a + FnMut(&T) -> bool,
    {
}

    /// Gets an iterator that visits the values in the `BTreeSet` in ascending order.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::collections::BTreeSet;
    ///
    /// let set: BTreeSet<usize> = [1, 2, 3].iter().cloned().collect();
    /// let mut set_iter = set.iter();
    /// assert_eq!(set_iter.next(), Some(&1));
    /// assert_eq!(set_iter.next(), Some(&2));
    /// assert_eq!(set_iter.next(), Some(&3));
    /// assert_eq!(set_iter.next(), None);
    /// ```
    ///
    /// Values returned by the iterator are returned in ascending order:
    ///
    /// ```
    /// use std::collections::BTreeSet;
    ///
    /// let set: BTreeSet<usize> = [3, 1, 2].iter().cloned().collect();
    /// let mut set_iter = set.iter();
    /// assert_eq!(set_iter.next(), Some(&1));
    /// assert_eq!(set_iter.next(), Some(&2));
    /// assert_eq!(set_iter.next(), Some(&3));
    /// assert_eq!(set_iter.next(), None);
    /// ```
    #[stable(feature = "rust1", since = "1.0.0")]
    pub fn iter(&self) -> Iter<'_, T> {
}

    /// Returns the number of elements in the set.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::collections::BTreeSet;
    ///
    /// let mut v = BTreeSet::new();
    /// assert_eq!(v.len(), 0);
    /// v.insert(1);
    /// assert_eq!(v.len(), 1);
    /// ```
    #[stable(feature = "rust1", since = "1.0.0")]
    #[rustc_const_unstable(feature = "const_btree_new", issue = "71835")]
    pub const fn len(&self) -> usize {
}

    /// Returns `true` if the set contains no elements.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::collections::BTreeSet;
    ///
    /// let mut v = BTreeSet::new();
    /// assert!(v.is_empty());
    /// v.insert(1);
    /// assert!(!v.is_empty());
    /// ```
    #[stable(feature = "rust1", since = "1.0.0")]
    #[rustc_const_unstable(feature = "const_btree_new", issue = "71835")]
    pub const fn is_empty(&self) -> bool {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<T: Ord> FromIterator<T> for BTreeSet<T> {
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> BTreeSet<T> {
}
}

#[stable(feature = "std_collections_from_array", since = "1.56.0")]
impl<T: Ord, const N: usize> From<[T; N]> for BTreeSet<T> {
    /// ```
    /// use std::collections::BTreeSet;
    ///
    /// let set1 = BTreeSet::from([1, 2, 3, 4]);
    /// let set2: BTreeSet<_> = [1, 2, 3, 4].into();
    /// assert_eq!(set1, set2);
    /// ```
    fn from(arr: [T; N]) -> Self {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<T> IntoIterator for BTreeSet<T> {
    type Item = T;
    type IntoIter = IntoIter<T>;

    /// Gets an iterator for moving out the `BTreeSet`'s contents.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::collections::BTreeSet;
    ///
    /// let set: BTreeSet<usize> = [1, 2, 3, 4].iter().cloned().collect();
    ///
    /// let v: Vec<_> = set.into_iter().collect();
    /// assert_eq!(v, [1, 2, 3, 4]);
    /// ```
    fn into_iter(self) -> IntoIter<T> {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<'a, T> IntoIterator for &'a BTreeSet<T> {
    type Item = &'a T;
    type IntoIter = Iter<'a, T>;

    fn into_iter(self) -> Iter<'a, T> {
}
}

/// An iterator produced by calling `drain_filter` on BTreeSet.
#[unstable(feature = "btree_drain_filter", issue = "70530")]
pub struct DrainFilter<'a, T, F>
where
    T: 'a,
    F: 'a + FnMut(&T) -> bool,
{
    pred: F,
    inner: super::map::DrainFilterInner<'a, T, ()>,
}

#[unstable(feature = "btree_drain_filter", issue = "70530")]
impl<T, F> Drop for DrainFilter<'_, T, F>
where
    F: FnMut(&T) -> bool,
{
    fn drop(&mut self) {
}
}

#[unstable(feature = "btree_drain_filter", issue = "70530")]
impl<T, F> fmt::Debug for DrainFilter<'_, T, F>
where
    T: fmt::Debug,
    F: FnMut(&T) -> bool,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
}
}

#[unstable(feature = "btree_drain_filter", issue = "70530")]
impl<'a, T, F> Iterator for DrainFilter<'_, T, F>
where
    F: 'a + FnMut(&T) -> bool,
{
    type Item = T;

    fn next(&mut self) -> Option<T> {
}

    fn size_hint(&self) -> (usize, Option<usize>) {
}
}

#[unstable(feature = "btree_drain_filter", issue = "70530")]
impl<T, F> FusedIterator for DrainFilter<'_, T, F> where F: FnMut(&T) -> bool {}

#[stable(feature = "rust1", since = "1.0.0")]
impl<T: Ord> Extend<T> for BTreeSet<T> {
    #[inline]
    fn extend<Iter: IntoIterator<Item = T>>(&mut self, iter: Iter) {
}

    #[inline]
    fn extend_one(&mut self, elem: T) {
}
}

#[stable(feature = "extend_ref", since = "1.2.0")]
impl<'a, T: 'a + Ord + Copy> Extend<&'a T> for BTreeSet<T> {
    fn extend<I: IntoIterator<Item = &'a T>>(&mut self, iter: I) {
}

    #[inline]
    fn extend_one(&mut self, &elem: &'a T) {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<T> Default for BTreeSet<T> {
    /// Creates an empty `BTreeSet`.
    fn default() -> BTreeSet<T> {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<T: Ord + Clone> Sub<&BTreeSet<T>> for &BTreeSet<T> {
    type Output = BTreeSet<T>;

    /// Returns the difference of `self` and `rhs` as a new `BTreeSet<T>`.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::collections::BTreeSet;
    ///
    /// let a: BTreeSet<_> = vec![1, 2, 3].into_iter().collect();
    /// let b: BTreeSet<_> = vec![3, 4, 5].into_iter().collect();
    ///
    /// let result = &a - &b;
    /// let result_vec: Vec<_> = result.into_iter().collect();
    /// assert_eq!(result_vec, [1, 2]);
    /// ```
    fn sub(self, rhs: &BTreeSet<T>) -> BTreeSet<T> {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<T: Ord + Clone> BitXor<&BTreeSet<T>> for &BTreeSet<T> {
    type Output = BTreeSet<T>;

    /// Returns the symmetric difference of `self` and `rhs` as a new `BTreeSet<T>`.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::collections::BTreeSet;
    ///
    /// let a: BTreeSet<_> = vec![1, 2, 3].into_iter().collect();
    /// let b: BTreeSet<_> = vec![2, 3, 4].into_iter().collect();
    ///
    /// let result = &a ^ &b;
    /// let result_vec: Vec<_> = result.into_iter().collect();
    /// assert_eq!(result_vec, [1, 4]);
    /// ```
    fn bitxor(self, rhs: &BTreeSet<T>) -> BTreeSet<T> {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<T: Ord + Clone> BitAnd<&BTreeSet<T>> for &BTreeSet<T> {
    type Output = BTreeSet<T>;

    /// Returns the intersection of `self` and `rhs` as a new `BTreeSet<T>`.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::collections::BTreeSet;
    ///
    /// let a: BTreeSet<_> = vec![1, 2, 3].into_iter().collect();
    /// let b: BTreeSet<_> = vec![2, 3, 4].into_iter().collect();
    ///
    /// let result = &a & &b;
    /// let result_vec: Vec<_> = result.into_iter().collect();
    /// assert_eq!(result_vec, [2, 3]);
    /// ```
    fn bitand(self, rhs: &BTreeSet<T>) -> BTreeSet<T> {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<T: Ord + Clone> BitOr<&BTreeSet<T>> for &BTreeSet<T> {
    type Output = BTreeSet<T>;

    /// Returns the union of `self` and `rhs` as a new `BTreeSet<T>`.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::collections::BTreeSet;
    ///
    /// let a: BTreeSet<_> = vec![1, 2, 3].into_iter().collect();
    /// let b: BTreeSet<_> = vec![3, 4, 5].into_iter().collect();
    ///
    /// let result = &a | &b;
    /// let result_vec: Vec<_> = result.into_iter().collect();
    /// assert_eq!(result_vec, [1, 2, 3, 4, 5]);
    /// ```
    fn bitor(self, rhs: &BTreeSet<T>) -> BTreeSet<T> {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<T: Debug> Debug for BTreeSet<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<T> Clone for Iter<'_, T> {
    fn clone(&self) -> Self {
}
}
#[stable(feature = "rust1", since = "1.0.0")]
impl<'a, T> Iterator for Iter<'a, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<&'a T> {
}

    fn size_hint(&self) -> (usize, Option<usize>) {
}

    fn last(mut self) -> Option<&'a T> {
}

    fn min(mut self) -> Option<&'a T> {
}

    fn max(mut self) -> Option<&'a T> {
}
}
#[stable(feature = "rust1", since = "1.0.0")]
impl<'a, T> DoubleEndedIterator for Iter<'a, T> {
    fn next_back(&mut self) -> Option<&'a T> {
}
}
#[stable(feature = "rust1", since = "1.0.0")]
impl<T> ExactSizeIterator for Iter<'_, T> {
    fn len(&self) -> usize {
}
}

#[stable(feature = "fused", since = "1.26.0")]
impl<T> FusedIterator for Iter<'_, T> {}

#[stable(feature = "rust1", since = "1.0.0")]
impl<T> Iterator for IntoIter<T> {
    type Item = T;

    fn next(&mut self) -> Option<T> {
}

    fn size_hint(&self) -> (usize, Option<usize>) {
}
}
#[stable(feature = "rust1", since = "1.0.0")]
impl<T> DoubleEndedIterator for IntoIter<T> {
    fn next_back(&mut self) -> Option<T> {
}
}
#[stable(feature = "rust1", since = "1.0.0")]
impl<T> ExactSizeIterator for IntoIter<T> {
    fn len(&self) -> usize {
}
}

#[stable(feature = "fused", since = "1.26.0")]
impl<T> FusedIterator for IntoIter<T> {}

#[stable(feature = "btree_range", since = "1.17.0")]
impl<T> Clone for Range<'_, T> {
    fn clone(&self) -> Self {
}
}

#[stable(feature = "btree_range", since = "1.17.0")]
impl<'a, T> Iterator for Range<'a, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<&'a T> {
}

    fn last(mut self) -> Option<&'a T> {
}

    fn min(mut self) -> Option<&'a T> {
}

    fn max(mut self) -> Option<&'a T> {
}
}

#[stable(feature = "btree_range", since = "1.17.0")]
impl<'a, T> DoubleEndedIterator for Range<'a, T> {
    fn next_back(&mut self) -> Option<&'a T> {
}
}

#[stable(feature = "fused", since = "1.26.0")]
impl<T> FusedIterator for Range<'_, T> {}

#[stable(feature = "rust1", since = "1.0.0")]
impl<T> Clone for Difference<'_, T> {
    fn clone(&self) -> Self {
}
}
#[stable(feature = "rust1", since = "1.0.0")]
impl<'a, T: Ord> Iterator for Difference<'a, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<&'a T> {
}

    fn size_hint(&self) -> (usize, Option<usize>) {
}

    fn min(mut self) -> Option<&'a T> {
}
}

#[stable(feature = "fused", since = "1.26.0")]
impl<T: Ord> FusedIterator for Difference<'_, T> {}

#[stable(feature = "rust1", since = "1.0.0")]
impl<T> Clone for SymmetricDifference<'_, T> {
    fn clone(&self) -> Self {
}
}
#[stable(feature = "rust1", since = "1.0.0")]
impl<'a, T: Ord> Iterator for SymmetricDifference<'a, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<&'a T> {
}

    fn size_hint(&self) -> (usize, Option<usize>) {
}

    fn min(mut self) -> Option<&'a T> {
}
}

#[stable(feature = "fused", since = "1.26.0")]
impl<T: Ord> FusedIterator for SymmetricDifference<'_, T> {}

#[stable(feature = "rust1", since = "1.0.0")]
impl<T> Clone for Intersection<'_, T> {
    fn clone(&self) -> Self {
}
}
#[stable(feature = "rust1", since = "1.0.0")]
impl<'a, T: Ord> Iterator for Intersection<'a, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<&'a T> {
}

    fn size_hint(&self) -> (usize, Option<usize>) {
}

    fn min(mut self) -> Option<&'a T> {
}
}

#[stable(feature = "fused", since = "1.26.0")]
impl<T: Ord> FusedIterator for Intersection<'_, T> {}

#[stable(feature = "rust1", since = "1.0.0")]
impl<T> Clone for Union<'_, T> {
    fn clone(&self) -> Self {
}
}
#[stable(feature = "rust1", since = "1.0.0")]
impl<'a, T: Ord> Iterator for Union<'a, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<&'a T> {
}

    fn size_hint(&self) -> (usize, Option<usize>) {
}

    fn min(mut self) -> Option<&'a T> {
}
}

#[stable(feature = "fused", since = "1.26.0")]
impl<T: Ord> FusedIterator for Union<'_, T> {}

#[cfg(test)]
mod tests {
}
}
mod split {
use super::node::{ForceResult::*, Root};
use super::search::SearchResult::*;
use core::borrow::Borrow;

impl<K, V> Root<K, V> {
    /// Calculates the length of both trees that result from splitting up
    /// a given number of distinct key-value pairs.
    pub fn calc_split_length(
        total_num: usize,
        root_a: &Root<K, V>,
        root_b: &Root<K, V>,
    ) -> (usize, usize) {
}

    /// Split off a tree with key-value pairs at and after the given key.
    /// The result is meaningful only if the tree is ordered by key,
    /// and if the ordering of `Q` corresponds to that of `K`.
    /// If `self` respects all `BTreeMap` tree invariants, then both
    /// `self` and the returned tree will respect those invariants.
    pub fn split_off<Q: ?Sized + Ord>(&mut self, key: &Q) -> Self
    where
        K: Borrow<Q>,
    {
}

    /// Creates a tree consisting of empty nodes.
    fn new_pillar(height: usize) -> Self {
}
}
}

#[doc(hidden)]
trait Recover<Q: ?Sized> {
    type Key;

    fn get(&self, key: &Q) -> Option<&Self::Key>;
    fn take(&mut self, key: &Q) -> Option<Self::Key>;
    fn replace(&mut self, key: Self::Key) -> Option<Self::Key>;
}

#[cfg(test)]
mod testing {
}
}
#[cfg(not(no_global_oom_handling))]
pub mod linked_list {
//! A doubly-linked list with owned nodes.
//!
//! The `LinkedList` allows pushing and popping elements at either end
//! in constant time.
//!
//! NOTE: It is almost always better to use [`Vec`] or [`VecDeque`] because
//! array-based containers are generally faster,
//! more memory efficient, and make better use of CPU cache.
//!
//! [`Vec`]: crate::vec::Vec
//! [`VecDeque`]: super::vec_deque::VecDeque

#![stable(feature = "rust1", since = "1.0.0")]

use core::cmp::Ordering;
use core::fmt;
use core::hash::{Hash, Hasher};
use core::iter::{FromIterator, FusedIterator};
use core::marker::PhantomData;
use core::mem;
use core::ptr::NonNull;

use super::SpecExtend;
use crate::boxed::Box;

#[cfg(test)]
mod tests {
}

/// A doubly-linked list with owned nodes.
///
/// The `LinkedList` allows pushing and popping elements at either end
/// in constant time.
///
/// A `LinkedList` with a known list of items can be initialized from an array:
/// ```
/// use std::collections::LinkedList;
///
/// let list = LinkedList::from([1, 2, 3]);
/// ```
///
/// NOTE: It is almost always better to use `Vec` or `VecDeque` because
/// array-based containers are generally faster,
/// more memory efficient, and make better use of CPU cache.
#[stable(feature = "rust1", since = "1.0.0")]
#[cfg_attr(not(test), rustc_diagnostic_item = "LinkedList")]
#[rustc_insignificant_dtor]
pub struct LinkedList<T> {
    head: Option<NonNull<Node<T>>>,
    tail: Option<NonNull<Node<T>>>,
    len: usize,
    marker: PhantomData<Box<Node<T>>>,
}

struct Node<T> {
    next: Option<NonNull<Node<T>>>,
    prev: Option<NonNull<Node<T>>>,
    element: T,
}

/// An iterator over the elements of a `LinkedList`.
///
/// This `struct` is created by [`LinkedList::iter()`]. See its
/// documentation for more.
#[stable(feature = "rust1", since = "1.0.0")]
pub struct Iter<'a, T: 'a> {
    head: Option<NonNull<Node<T>>>,
    tail: Option<NonNull<Node<T>>>,
    len: usize,
    marker: PhantomData<&'a Node<T>>,
}

#[stable(feature = "collection_debug", since = "1.17.0")]
impl<T: fmt::Debug> fmt::Debug for Iter<'_, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
}
}

// FIXME(#26925) Remove in favor of `#[derive(Clone)]`
#[stable(feature = "rust1", since = "1.0.0")]
impl<T> Clone for Iter<'_, T> {
    fn clone(&self) -> Self {
}
}

/// A mutable iterator over the elements of a `LinkedList`.
///
/// This `struct` is created by [`LinkedList::iter_mut()`]. See its
/// documentation for more.
#[stable(feature = "rust1", since = "1.0.0")]
pub struct IterMut<'a, T: 'a> {
    head: Option<NonNull<Node<T>>>,
    tail: Option<NonNull<Node<T>>>,
    len: usize,
    marker: PhantomData<&'a mut Node<T>>,
}

#[stable(feature = "collection_debug", since = "1.17.0")]
impl<T: fmt::Debug> fmt::Debug for IterMut<'_, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
}
}

/// An owning iterator over the elements of a `LinkedList`.
///
/// This `struct` is created by the [`into_iter`] method on [`LinkedList`]
/// (provided by the `IntoIterator` trait). See its documentation for more.
///
/// [`into_iter`]: LinkedList::into_iter
#[derive(Clone)]
#[stable(feature = "rust1", since = "1.0.0")]
pub struct IntoIter<T> {
    list: LinkedList<T>,
}

#[stable(feature = "collection_debug", since = "1.17.0")]
impl<T: fmt::Debug> fmt::Debug for IntoIter<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
}
}

impl<T> Node<T> {
    fn new(element: T) -> Self {
}

    fn into_element(self: Box<Self>) -> T {
}
}

// private methods
impl<T> LinkedList<T> {
    /// Adds the given node to the front of the list.
    #[inline]
    fn push_front_node(&mut self, mut node: Box<Node<T>>) {
}

    /// Removes and returns the node at the front of the list.
    #[inline]
    fn pop_front_node(&mut self) -> Option<Box<Node<T>>> {
}

    /// Adds the given node to the back of the list.
    #[inline]
    fn push_back_node(&mut self, mut node: Box<Node<T>>) {
}

    /// Removes and returns the node at the back of the list.
    #[inline]
    fn pop_back_node(&mut self) -> Option<Box<Node<T>>> {
}

    /// Unlinks the specified node from the current list.
    ///
    /// Warning: this will not check that the provided node belongs to the current list.
    ///
    /// This method takes care not to create mutable references to `element`, to
    /// maintain validity of aliasing pointers.
    #[inline]
    unsafe fn unlink_node(&mut self, mut node: NonNull<Node<T>>) {
}

    /// Splices a series of nodes between two existing nodes.
    ///
    /// Warning: this will not check that the provided node belongs to the two existing lists.
    #[inline]
    unsafe fn splice_nodes(
        &mut self,
        existing_prev: Option<NonNull<Node<T>>>,
        existing_next: Option<NonNull<Node<T>>>,
        mut splice_start: NonNull<Node<T>>,
        mut splice_end: NonNull<Node<T>>,
        splice_length: usize,
    ) {
}

    /// Detaches all nodes from a linked list as a series of nodes.
    #[inline]
    fn detach_all_nodes(mut self) -> Option<(NonNull<Node<T>>, NonNull<Node<T>>, usize)> {
}

    #[inline]
    unsafe fn split_off_before_node(
        &mut self,
        split_node: Option<NonNull<Node<T>>>,
        at: usize,
    ) -> Self {
}

    #[inline]
    unsafe fn split_off_after_node(
        &mut self,
        split_node: Option<NonNull<Node<T>>>,
        at: usize,
    ) -> Self {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<T> Default for LinkedList<T> {
    /// Creates an empty `LinkedList<T>`.
    #[inline]
    fn default() -> Self {
}
}

impl<T> LinkedList<T> {
    /// Creates an empty `LinkedList`.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::collections::LinkedList;
    ///
    /// let list: LinkedList<u32> = LinkedList::new();
    /// ```
    #[inline]
    #[rustc_const_stable(feature = "const_linked_list_new", since = "1.32.0")]
    #[stable(feature = "rust1", since = "1.0.0")]
    pub const fn new() -> Self {
}

    /// Moves all elements from `other` to the end of the list.
    ///
    /// This reuses all the nodes from `other` and moves them into `self`. After
    /// this operation, `other` becomes empty.
    ///
    /// This operation should compute in *O*(1) time and *O*(1) memory.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::collections::LinkedList;
    ///
    /// let mut list1 = LinkedList::new();
    /// list1.push_back('a');
    ///
    /// let mut list2 = LinkedList::new();
    /// list2.push_back('b');
    /// list2.push_back('c');
    ///
    /// list1.append(&mut list2);
    ///
    /// let mut iter = list1.iter();
    /// assert_eq!(iter.next(), Some(&'a'));
    /// assert_eq!(iter.next(), Some(&'b'));
    /// assert_eq!(iter.next(), Some(&'c'));
    /// assert!(iter.next().is_none());
    ///
    /// assert!(list2.is_empty());
    /// ```
    #[stable(feature = "rust1", since = "1.0.0")]
    pub fn append(&mut self, other: &mut Self) {
}

    /// Provides a forward iterator.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::collections::LinkedList;
    ///
    /// let mut list: LinkedList<u32> = LinkedList::new();
    ///
    /// list.push_back(0);
    /// list.push_back(1);
    /// list.push_back(2);
    ///
    /// let mut iter = list.iter();
    /// assert_eq!(iter.next(), Some(&0));
    /// assert_eq!(iter.next(), Some(&1));
    /// assert_eq!(iter.next(), Some(&2));
    /// assert_eq!(iter.next(), None);
    /// ```
    #[inline]
    #[stable(feature = "rust1", since = "1.0.0")]
    pub fn iter(&self) -> Iter<'_, T> {
}

    /// Provides a forward iterator with mutable references.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::collections::LinkedList;
    ///
    /// let mut list: LinkedList<u32> = LinkedList::new();
    ///
    /// list.push_back(0);
    /// list.push_back(1);
    /// list.push_back(2);
    ///
    /// for element in list.iter_mut() {
    ///     *element += 10;
    /// }
    ///
    /// let mut iter = list.iter();
    /// assert_eq!(iter.next(), Some(&10));
    /// assert_eq!(iter.next(), Some(&11));
    /// assert_eq!(iter.next(), Some(&12));
    /// assert_eq!(iter.next(), None);
    /// ```
    #[inline]
    #[stable(feature = "rust1", since = "1.0.0")]
    pub fn iter_mut(&mut self) -> IterMut<'_, T> {
}

    /// Provides a cursor at the front element.
    ///
    /// The cursor is pointing to the "ghost" non-element if the list is empty.
    #[inline]
    #[unstable(feature = "linked_list_cursors", issue = "58533")]
    pub fn cursor_front(&self) -> Cursor<'_, T> {
}

    /// Provides a cursor with editing operations at the front element.
    ///
    /// The cursor is pointing to the "ghost" non-element if the list is empty.
    #[inline]
    #[unstable(feature = "linked_list_cursors", issue = "58533")]
    pub fn cursor_front_mut(&mut self) -> CursorMut<'_, T> {
}

    /// Provides a cursor at the back element.
    ///
    /// The cursor is pointing to the "ghost" non-element if the list is empty.
    #[inline]
    #[unstable(feature = "linked_list_cursors", issue = "58533")]
    pub fn cursor_back(&self) -> Cursor<'_, T> {
}

    /// Provides a cursor with editing operations at the back element.
    ///
    /// The cursor is pointing to the "ghost" non-element if the list is empty.
    #[inline]
    #[unstable(feature = "linked_list_cursors", issue = "58533")]
    pub fn cursor_back_mut(&mut self) -> CursorMut<'_, T> {
}

    /// Returns `true` if the `LinkedList` is empty.
    ///
    /// This operation should compute in *O*(1) time.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::collections::LinkedList;
    ///
    /// let mut dl = LinkedList::new();
    /// assert!(dl.is_empty());
    ///
    /// dl.push_front("foo");
    /// assert!(!dl.is_empty());
    /// ```
    #[inline]
    #[stable(feature = "rust1", since = "1.0.0")]
    pub fn is_empty(&self) -> bool {
}

    /// Returns the length of the `LinkedList`.
    ///
    /// This operation should compute in *O*(1) time.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::collections::LinkedList;
    ///
    /// let mut dl = LinkedList::new();
    ///
    /// dl.push_front(2);
    /// assert_eq!(dl.len(), 1);
    ///
    /// dl.push_front(1);
    /// assert_eq!(dl.len(), 2);
    ///
    /// dl.push_back(3);
    /// assert_eq!(dl.len(), 3);
    /// ```
    #[inline]
    #[stable(feature = "rust1", since = "1.0.0")]
    pub fn len(&self) -> usize {
}

    /// Removes all elements from the `LinkedList`.
    ///
    /// This operation should compute in *O*(*n*) time.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::collections::LinkedList;
    ///
    /// let mut dl = LinkedList::new();
    ///
    /// dl.push_front(2);
    /// dl.push_front(1);
    /// assert_eq!(dl.len(), 2);
    /// assert_eq!(dl.front(), Some(&1));
    ///
    /// dl.clear();
    /// assert_eq!(dl.len(), 0);
    /// assert_eq!(dl.front(), None);
    /// ```
    #[inline]
    #[stable(feature = "rust1", since = "1.0.0")]
    pub fn clear(&mut self) {
}

    /// Returns `true` if the `LinkedList` contains an element equal to the
    /// given value.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::collections::LinkedList;
    ///
    /// let mut list: LinkedList<u32> = LinkedList::new();
    ///
    /// list.push_back(0);
    /// list.push_back(1);
    /// list.push_back(2);
    ///
    /// assert_eq!(list.contains(&0), true);
    /// assert_eq!(list.contains(&10), false);
    /// ```
    #[stable(feature = "linked_list_contains", since = "1.12.0")]
    pub fn contains(&self, x: &T) -> bool
    where
        T: PartialEq<T>,
    {
}

    /// Provides a reference to the front element, or `None` if the list is
    /// empty.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::collections::LinkedList;
    ///
    /// let mut dl = LinkedList::new();
    /// assert_eq!(dl.front(), None);
    ///
    /// dl.push_front(1);
    /// assert_eq!(dl.front(), Some(&1));
    /// ```
    #[inline]
    #[stable(feature = "rust1", since = "1.0.0")]
    pub fn front(&self) -> Option<&T> {
}

    /// Provides a mutable reference to the front element, or `None` if the list
    /// is empty.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::collections::LinkedList;
    ///
    /// let mut dl = LinkedList::new();
    /// assert_eq!(dl.front(), None);
    ///
    /// dl.push_front(1);
    /// assert_eq!(dl.front(), Some(&1));
    ///
    /// match dl.front_mut() {
    ///     None => {},
    ///     Some(x) => *x = 5,
    /// }
    /// assert_eq!(dl.front(), Some(&5));
    /// ```
    #[inline]
    #[stable(feature = "rust1", since = "1.0.0")]
    pub fn front_mut(&mut self) -> Option<&mut T> {
}

    /// Provides a reference to the back element, or `None` if the list is
    /// empty.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::collections::LinkedList;
    ///
    /// let mut dl = LinkedList::new();
    /// assert_eq!(dl.back(), None);
    ///
    /// dl.push_back(1);
    /// assert_eq!(dl.back(), Some(&1));
    /// ```
    #[inline]
    #[stable(feature = "rust1", since = "1.0.0")]
    pub fn back(&self) -> Option<&T> {
}

    /// Provides a mutable reference to the back element, or `None` if the list
    /// is empty.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::collections::LinkedList;
    ///
    /// let mut dl = LinkedList::new();
    /// assert_eq!(dl.back(), None);
    ///
    /// dl.push_back(1);
    /// assert_eq!(dl.back(), Some(&1));
    ///
    /// match dl.back_mut() {
    ///     None => {},
    ///     Some(x) => *x = 5,
    /// }
    /// assert_eq!(dl.back(), Some(&5));
    /// ```
    #[inline]
    #[stable(feature = "rust1", since = "1.0.0")]
    pub fn back_mut(&mut self) -> Option<&mut T> {
}

    /// Adds an element first in the list.
    ///
    /// This operation should compute in *O*(1) time.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::collections::LinkedList;
    ///
    /// let mut dl = LinkedList::new();
    ///
    /// dl.push_front(2);
    /// assert_eq!(dl.front().unwrap(), &2);
    ///
    /// dl.push_front(1);
    /// assert_eq!(dl.front().unwrap(), &1);
    /// ```
    #[stable(feature = "rust1", since = "1.0.0")]
    pub fn push_front(&mut self, elt: T) {
}

    /// Removes the first element and returns it, or `None` if the list is
    /// empty.
    ///
    /// This operation should compute in *O*(1) time.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::collections::LinkedList;
    ///
    /// let mut d = LinkedList::new();
    /// assert_eq!(d.pop_front(), None);
    ///
    /// d.push_front(1);
    /// d.push_front(3);
    /// assert_eq!(d.pop_front(), Some(3));
    /// assert_eq!(d.pop_front(), Some(1));
    /// assert_eq!(d.pop_front(), None);
    /// ```
    #[stable(feature = "rust1", since = "1.0.0")]
    pub fn pop_front(&mut self) -> Option<T> {
}

    /// Appends an element to the back of a list.
    ///
    /// This operation should compute in *O*(1) time.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::collections::LinkedList;
    ///
    /// let mut d = LinkedList::new();
    /// d.push_back(1);
    /// d.push_back(3);
    /// assert_eq!(3, *d.back().unwrap());
    /// ```
    #[stable(feature = "rust1", since = "1.0.0")]
    pub fn push_back(&mut self, elt: T) {
}

    /// Removes the last element from a list and returns it, or `None` if
    /// it is empty.
    ///
    /// This operation should compute in *O*(1) time.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::collections::LinkedList;
    ///
    /// let mut d = LinkedList::new();
    /// assert_eq!(d.pop_back(), None);
    /// d.push_back(1);
    /// d.push_back(3);
    /// assert_eq!(d.pop_back(), Some(3));
    /// ```
    #[stable(feature = "rust1", since = "1.0.0")]
    pub fn pop_back(&mut self) -> Option<T> {
}

    /// Splits the list into two at the given index. Returns everything after the given index,
    /// including the index.
    ///
    /// This operation should compute in *O*(*n*) time.
    ///
    /// # Panics
    ///
    /// Panics if `at > len`.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::collections::LinkedList;
    ///
    /// let mut d = LinkedList::new();
    ///
    /// d.push_front(1);
    /// d.push_front(2);
    /// d.push_front(3);
    ///
    /// let mut split = d.split_off(2);
    ///
    /// assert_eq!(split.pop_front(), Some(1));
    /// assert_eq!(split.pop_front(), None);
    /// ```
    #[stable(feature = "rust1", since = "1.0.0")]
    pub fn split_off(&mut self, at: usize) -> LinkedList<T> {
}

    /// Removes the element at the given index and returns it.
    ///
    /// This operation should compute in *O*(*n*) time.
    ///
    /// # Panics
    /// Panics if at >= len
    ///
    /// # Examples
    ///
    /// ```
    /// #![feature(linked_list_remove)]
    /// use std::collections::LinkedList;
    ///
    /// let mut d = LinkedList::new();
    ///
    /// d.push_front(1);
    /// d.push_front(2);
    /// d.push_front(3);
    ///
    /// assert_eq!(d.remove(1), 2);
    /// assert_eq!(d.remove(0), 3);
    /// assert_eq!(d.remove(0), 1);
    /// ```
    #[unstable(feature = "linked_list_remove", issue = "69210")]
    pub fn remove(&mut self, at: usize) -> T {
}

    /// Creates an iterator which uses a closure to determine if an element should be removed.
    ///
    /// If the closure returns true, then the element is removed and yielded.
    /// If the closure returns false, the element will remain in the list and will not be yielded
    /// by the iterator.
    ///
    /// Note that `drain_filter` lets you mutate every element in the filter closure, regardless of
    /// whether you choose to keep or remove it.
    ///
    /// # Examples
    ///
    /// Splitting a list into evens and odds, reusing the original list:
    ///
    /// ```
    /// #![feature(drain_filter)]
    /// use std::collections::LinkedList;
    ///
    /// let mut numbers: LinkedList<u32> = LinkedList::new();
    /// numbers.extend(&[1, 2, 3, 4, 5, 6, 8, 9, 11, 13, 14, 15]);
    ///
    /// let evens = numbers.drain_filter(|x| *x % 2 == 0).collect::<LinkedList<_>>();
    /// let odds = numbers;
    ///
    /// assert_eq!(evens.into_iter().collect::<Vec<_>>(), vec![2, 4, 6, 8, 14]);
    /// assert_eq!(odds.into_iter().collect::<Vec<_>>(), vec![1, 3, 5, 9, 11, 13, 15]);
    /// ```
    #[unstable(feature = "drain_filter", reason = "recently added", issue = "43244")]
    pub fn drain_filter<F>(&mut self, filter: F) -> DrainFilter<'_, T, F>
    where
        F: FnMut(&mut T) -> bool,
    {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
unsafe impl<#[may_dangle] T> Drop for LinkedList<T> {
    fn drop(&mut self) {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<'a, T> Iterator for Iter<'a, T> {
    type Item = &'a T;

    #[inline]
    fn next(&mut self) -> Option<&'a T> {
}

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
}

    #[inline]
    fn last(mut self) -> Option<&'a T> {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<'a, T> DoubleEndedIterator for Iter<'a, T> {
    #[inline]
    fn next_back(&mut self) -> Option<&'a T> {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<T> ExactSizeIterator for Iter<'_, T> {}

#[stable(feature = "fused", since = "1.26.0")]
impl<T> FusedIterator for Iter<'_, T> {}

#[stable(feature = "rust1", since = "1.0.0")]
impl<'a, T> Iterator for IterMut<'a, T> {
    type Item = &'a mut T;

    #[inline]
    fn next(&mut self) -> Option<&'a mut T> {
}

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
}

    #[inline]
    fn last(mut self) -> Option<&'a mut T> {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<'a, T> DoubleEndedIterator for IterMut<'a, T> {
    #[inline]
    fn next_back(&mut self) -> Option<&'a mut T> {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<T> ExactSizeIterator for IterMut<'_, T> {}

#[stable(feature = "fused", since = "1.26.0")]
impl<T> FusedIterator for IterMut<'_, T> {}

/// A cursor over a `LinkedList`.
///
/// A `Cursor` is like an iterator, except that it can freely seek back-and-forth.
///
/// Cursors always rest between two elements in the list, and index in a logically circular way.
/// To accommodate this, there is a "ghost" non-element that yields `None` between the head and
/// tail of the list.
///
/// When created, cursors start at the front of the list, or the "ghost" non-element if the list is empty.
#[unstable(feature = "linked_list_cursors", issue = "58533")]
pub struct Cursor<'a, T: 'a> {
    index: usize,
    current: Option<NonNull<Node<T>>>,
    list: &'a LinkedList<T>,
}

#[unstable(feature = "linked_list_cursors", issue = "58533")]
impl<T> Clone for Cursor<'_, T> {
    fn clone(&self) -> Self {
}
}

#[unstable(feature = "linked_list_cursors", issue = "58533")]
impl<T: fmt::Debug> fmt::Debug for Cursor<'_, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
}
}

/// A cursor over a `LinkedList` with editing operations.
///
/// A `Cursor` is like an iterator, except that it can freely seek back-and-forth, and can
/// safely mutate the list during iteration. This is because the lifetime of its yielded
/// references is tied to its own lifetime, instead of just the underlying list. This means
/// cursors cannot yield multiple elements at once.
///
/// Cursors always rest between two elements in the list, and index in a logically circular way.
/// To accommodate this, there is a "ghost" non-element that yields `None` between the head and
/// tail of the list.
#[unstable(feature = "linked_list_cursors", issue = "58533")]
pub struct CursorMut<'a, T: 'a> {
    index: usize,
    current: Option<NonNull<Node<T>>>,
    list: &'a mut LinkedList<T>,
}

#[unstable(feature = "linked_list_cursors", issue = "58533")]
impl<T: fmt::Debug> fmt::Debug for CursorMut<'_, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
}
}

impl<'a, T> Cursor<'a, T> {
    /// Returns the cursor position index within the `LinkedList`.
    ///
    /// This returns `None` if the cursor is currently pointing to the
    /// "ghost" non-element.
    #[unstable(feature = "linked_list_cursors", issue = "58533")]
    pub fn index(&self) -> Option<usize> {
}

    /// Moves the cursor to the next element of the `LinkedList`.
    ///
    /// If the cursor is pointing to the "ghost" non-element then this will move it to
    /// the first element of the `LinkedList`. If it is pointing to the last
    /// element of the `LinkedList` then this will move it to the "ghost" non-element.
    #[unstable(feature = "linked_list_cursors", issue = "58533")]
    pub fn move_next(&mut self) {
}

    /// Moves the cursor to the previous element of the `LinkedList`.
    ///
    /// If the cursor is pointing to the "ghost" non-element then this will move it to
    /// the last element of the `LinkedList`. If it is pointing to the first
    /// element of the `LinkedList` then this will move it to the "ghost" non-element.
    #[unstable(feature = "linked_list_cursors", issue = "58533")]
    pub fn move_prev(&mut self) {
}

    /// Returns a reference to the element that the cursor is currently
    /// pointing to.
    ///
    /// This returns `None` if the cursor is currently pointing to the
    /// "ghost" non-element.
    #[unstable(feature = "linked_list_cursors", issue = "58533")]
    pub fn current(&self) -> Option<&'a T> {
}

    /// Returns a reference to the next element.
    ///
    /// If the cursor is pointing to the "ghost" non-element then this returns
    /// the first element of the `LinkedList`. If it is pointing to the last
    /// element of the `LinkedList` then this returns `None`.
    #[unstable(feature = "linked_list_cursors", issue = "58533")]
    pub fn peek_next(&self) -> Option<&'a T> {
}

    /// Returns a reference to the previous element.
    ///
    /// If the cursor is pointing to the "ghost" non-element then this returns
    /// the last element of the `LinkedList`. If it is pointing to the first
    /// element of the `LinkedList` then this returns `None`.
    #[unstable(feature = "linked_list_cursors", issue = "58533")]
    pub fn peek_prev(&self) -> Option<&'a T> {
}

    /// Provides a reference to the front element of the cursor's parent list,
    /// or None if the list is empty.
    #[unstable(feature = "linked_list_cursors", issue = "58533")]
    pub fn front(&self) -> Option<&'a T> {
}

    /// Provides a reference to the back element of the cursor's parent list,
    /// or None if the list is empty.
    #[unstable(feature = "linked_list_cursors", issue = "58533")]
    pub fn back(&self) -> Option<&'a T> {
}
}

impl<'a, T> CursorMut<'a, T> {
    /// Returns the cursor position index within the `LinkedList`.
    ///
    /// This returns `None` if the cursor is currently pointing to the
    /// "ghost" non-element.
    #[unstable(feature = "linked_list_cursors", issue = "58533")]
    pub fn index(&self) -> Option<usize> {
}

    /// Moves the cursor to the next element of the `LinkedList`.
    ///
    /// If the cursor is pointing to the "ghost" non-element then this will move it to
    /// the first element of the `LinkedList`. If it is pointing to the last
    /// element of the `LinkedList` then this will move it to the "ghost" non-element.
    #[unstable(feature = "linked_list_cursors", issue = "58533")]
    pub fn move_next(&mut self) {
}

    /// Moves the cursor to the previous element of the `LinkedList`.
    ///
    /// If the cursor is pointing to the "ghost" non-element then this will move it to
    /// the last element of the `LinkedList`. If it is pointing to the first
    /// element of the `LinkedList` then this will move it to the "ghost" non-element.
    #[unstable(feature = "linked_list_cursors", issue = "58533")]
    pub fn move_prev(&mut self) {
}

    /// Returns a reference to the element that the cursor is currently
    /// pointing to.
    ///
    /// This returns `None` if the cursor is currently pointing to the
    /// "ghost" non-element.
    #[unstable(feature = "linked_list_cursors", issue = "58533")]
    pub fn current(&mut self) -> Option<&mut T> {
}

    /// Returns a reference to the next element.
    ///
    /// If the cursor is pointing to the "ghost" non-element then this returns
    /// the first element of the `LinkedList`. If it is pointing to the last
    /// element of the `LinkedList` then this returns `None`.
    #[unstable(feature = "linked_list_cursors", issue = "58533")]
    pub fn peek_next(&mut self) -> Option<&mut T> {
}

    /// Returns a reference to the previous element.
    ///
    /// If the cursor is pointing to the "ghost" non-element then this returns
    /// the last element of the `LinkedList`. If it is pointing to the first
    /// element of the `LinkedList` then this returns `None`.
    #[unstable(feature = "linked_list_cursors", issue = "58533")]
    pub fn peek_prev(&mut self) -> Option<&mut T> {
}

    /// Returns a read-only cursor pointing to the current element.
    ///
    /// The lifetime of the returned `Cursor` is bound to that of the
    /// `CursorMut`, which means it cannot outlive the `CursorMut` and that the
    /// `CursorMut` is frozen for the lifetime of the `Cursor`.
    #[unstable(feature = "linked_list_cursors", issue = "58533")]
    pub fn as_cursor(&self) -> Cursor<'_, T> {
}
}

// Now the list editing operations

impl<'a, T> CursorMut<'a, T> {
    /// Inserts a new element into the `LinkedList` after the current one.
    ///
    /// If the cursor is pointing at the "ghost" non-element then the new element is
    /// inserted at the front of the `LinkedList`.
    #[unstable(feature = "linked_list_cursors", issue = "58533")]
    pub fn insert_after(&mut self, item: T) {
}

    /// Inserts a new element into the `LinkedList` before the current one.
    ///
    /// If the cursor is pointing at the "ghost" non-element then the new element is
    /// inserted at the end of the `LinkedList`.
    #[unstable(feature = "linked_list_cursors", issue = "58533")]
    pub fn insert_before(&mut self, item: T) {
}

    /// Removes the current element from the `LinkedList`.
    ///
    /// The element that was removed is returned, and the cursor is
    /// moved to point to the next element in the `LinkedList`.
    ///
    /// If the cursor is currently pointing to the "ghost" non-element then no element
    /// is removed and `None` is returned.
    #[unstable(feature = "linked_list_cursors", issue = "58533")]
    pub fn remove_current(&mut self) -> Option<T> {
}

    /// Removes the current element from the `LinkedList` without deallocating the list node.
    ///
    /// The node that was removed is returned as a new `LinkedList` containing only this node.
    /// The cursor is moved to point to the next element in the current `LinkedList`.
    ///
    /// If the cursor is currently pointing to the "ghost" non-element then no element
    /// is removed and `None` is returned.
    #[unstable(feature = "linked_list_cursors", issue = "58533")]
    pub fn remove_current_as_list(&mut self) -> Option<LinkedList<T>> {
}

    /// Inserts the elements from the given `LinkedList` after the current one.
    ///
    /// If the cursor is pointing at the "ghost" non-element then the new elements are
    /// inserted at the start of the `LinkedList`.
    #[unstable(feature = "linked_list_cursors", issue = "58533")]
    pub fn splice_after(&mut self, list: LinkedList<T>) {
}

    /// Inserts the elements from the given `LinkedList` before the current one.
    ///
    /// If the cursor is pointing at the "ghost" non-element then the new elements are
    /// inserted at the end of the `LinkedList`.
    #[unstable(feature = "linked_list_cursors", issue = "58533")]
    pub fn splice_before(&mut self, list: LinkedList<T>) {
}

    /// Splits the list into two after the current element. This will return a
    /// new list consisting of everything after the cursor, with the original
    /// list retaining everything before.
    ///
    /// If the cursor is pointing at the "ghost" non-element then the entire contents
    /// of the `LinkedList` are moved.
    #[unstable(feature = "linked_list_cursors", issue = "58533")]
    pub fn split_after(&mut self) -> LinkedList<T> {
}

    /// Splits the list into two before the current element. This will return a
    /// new list consisting of everything before the cursor, with the original
    /// list retaining everything after.
    ///
    /// If the cursor is pointing at the "ghost" non-element then the entire contents
    /// of the `LinkedList` are moved.
    #[unstable(feature = "linked_list_cursors", issue = "58533")]
    pub fn split_before(&mut self) -> LinkedList<T> {
}

    /// Appends an element to the front of the cursor's parent list. The node
    /// that the cursor points to is unchanged, even if it is the "ghost" node.
    ///
    /// This operation should compute in O(1) time.
    // `push_front` continues to point to "ghost" when it addes a node to mimic
    // the behavior of `insert_before` on an empty list.
    #[unstable(feature = "linked_list_cursors", issue = "58533")]
    pub fn push_front(&mut self, elt: T) {
}

    /// Appends an element to the back of the cursor's parent list. The node
    /// that the cursor points to is unchanged, even if it is the "ghost" node.
    ///
    /// This operation should compute in O(1) time.
    #[unstable(feature = "linked_list_cursors", issue = "58533")]
    pub fn push_back(&mut self, elt: T) {
}

    /// Removes the first element from the cursor's parent list and returns it,
    /// or None if the list is empty. The element the cursor points to remains
    /// unchanged, unless it was pointing to the front element. In that case, it
    /// points to the new front element.
    ///
    /// This operation should compute in O(1) time.
    #[unstable(feature = "linked_list_cursors", issue = "58533")]
    pub fn pop_front(&mut self) -> Option<T> {
}

    /// Removes the last element from the cursor's parent list and returns it,
    /// or None if the list is empty. The element the cursor points to remains
    /// unchanged, unless it was pointing to the back element. In that case, it
    /// points to the "ghost" element.
    ///
    /// This operation should compute in O(1) time.
    #[unstable(feature = "linked_list_cursors", issue = "58533")]
    pub fn pop_back(&mut self) -> Option<T> {
}

    /// Provides a reference to the front element of the cursor's parent list,
    /// or None if the list is empty.
    #[unstable(feature = "linked_list_cursors", issue = "58533")]
    pub fn front(&self) -> Option<&T> {
}

    /// Provides a mutable reference to the front element of the cursor's
    /// parent list, or None if the list is empty.
    #[unstable(feature = "linked_list_cursors", issue = "58533")]
    pub fn front_mut(&mut self) -> Option<&mut T> {
}

    /// Provides a reference to the back element of the cursor's parent list,
    /// or None if the list is empty.
    #[unstable(feature = "linked_list_cursors", issue = "58533")]
    pub fn back(&self) -> Option<&T> {
}

    /// Provides a mutable reference to back element of the cursor's parent
    /// list, or `None` if the list is empty.
    ///
    /// # Examples
    /// Building and mutating a list with a cursor, then getting the back element:
    /// ```
    /// #![feature(linked_list_cursors)]
    /// use std::collections::LinkedList;
    /// let mut dl = LinkedList::new();
    /// dl.push_front(3);
    /// dl.push_front(2);
    /// dl.push_front(1);
    /// let mut cursor = dl.cursor_front_mut();
    /// *cursor.current().unwrap() = 99;
    /// *cursor.back_mut().unwrap() = 0;
    /// let mut contents = dl.into_iter();
    /// assert_eq!(contents.next(), Some(99));
    /// assert_eq!(contents.next(), Some(2));
    /// assert_eq!(contents.next(), Some(0));
    /// assert_eq!(contents.next(), None);
    /// ```
    #[unstable(feature = "linked_list_cursors", issue = "58533")]
    pub fn back_mut(&mut self) -> Option<&mut T> {
}
}

/// An iterator produced by calling `drain_filter` on LinkedList.
#[unstable(feature = "drain_filter", reason = "recently added", issue = "43244")]
pub struct DrainFilter<'a, T: 'a, F: 'a>
where
    F: FnMut(&mut T) -> bool,
{
    list: &'a mut LinkedList<T>,
    it: Option<NonNull<Node<T>>>,
    pred: F,
    idx: usize,
    old_len: usize,
}

#[unstable(feature = "drain_filter", reason = "recently added", issue = "43244")]
impl<T, F> Iterator for DrainFilter<'_, T, F>
where
    F: FnMut(&mut T) -> bool,
{
    type Item = T;

    fn next(&mut self) -> Option<T> {
}

    fn size_hint(&self) -> (usize, Option<usize>) {
}
}

#[unstable(feature = "drain_filter", reason = "recently added", issue = "43244")]
impl<T, F> Drop for DrainFilter<'_, T, F>
where
    F: FnMut(&mut T) -> bool,
{
    fn drop(&mut self) {
}
}

#[unstable(feature = "drain_filter", reason = "recently added", issue = "43244")]
impl<T: fmt::Debug, F> fmt::Debug for DrainFilter<'_, T, F>
where
    F: FnMut(&mut T) -> bool,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<T> Iterator for IntoIter<T> {
    type Item = T;

    #[inline]
    fn next(&mut self) -> Option<T> {
}

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<T> DoubleEndedIterator for IntoIter<T> {
    #[inline]
    fn next_back(&mut self) -> Option<T> {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<T> ExactSizeIterator for IntoIter<T> {}

#[stable(feature = "fused", since = "1.26.0")]
impl<T> FusedIterator for IntoIter<T> {}

#[stable(feature = "rust1", since = "1.0.0")]
impl<T> FromIterator<T> for LinkedList<T> {
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<T> IntoIterator for LinkedList<T> {
    type Item = T;
    type IntoIter = IntoIter<T>;

    /// Consumes the list into an iterator yielding elements by value.
    #[inline]
    fn into_iter(self) -> IntoIter<T> {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<'a, T> IntoIterator for &'a LinkedList<T> {
    type Item = &'a T;
    type IntoIter = Iter<'a, T>;

    fn into_iter(self) -> Iter<'a, T> {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<'a, T> IntoIterator for &'a mut LinkedList<T> {
    type Item = &'a mut T;
    type IntoIter = IterMut<'a, T>;

    fn into_iter(self) -> IterMut<'a, T> {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<T> Extend<T> for LinkedList<T> {
    fn extend<I: IntoIterator<Item = T>>(&mut self, iter: I) {
}

    #[inline]
    fn extend_one(&mut self, elem: T) {
}
}

impl<I: IntoIterator> SpecExtend<I> for LinkedList<I::Item> {
    default fn spec_extend(&mut self, iter: I) {
}
}

impl<T> SpecExtend<LinkedList<T>> for LinkedList<T> {
    fn spec_extend(&mut self, ref mut other: LinkedList<T>) {
}
}

#[stable(feature = "extend_ref", since = "1.2.0")]
impl<'a, T: 'a + Copy> Extend<&'a T> for LinkedList<T> {
    fn extend<I: IntoIterator<Item = &'a T>>(&mut self, iter: I) {
}

    #[inline]
    fn extend_one(&mut self, &elem: &'a T) {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<T: PartialEq> PartialEq for LinkedList<T> {
    fn eq(&self, other: &Self) -> bool {
}

    fn ne(&self, other: &Self) -> bool {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<T: Eq> Eq for LinkedList<T> {}

#[stable(feature = "rust1", since = "1.0.0")]
impl<T: PartialOrd> PartialOrd for LinkedList<T> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<T: Ord> Ord for LinkedList<T> {
    #[inline]
    fn cmp(&self, other: &Self) -> Ordering {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<T: Clone> Clone for LinkedList<T> {
    fn clone(&self) -> Self {
}

    fn clone_from(&mut self, other: &Self) {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<T: fmt::Debug> fmt::Debug for LinkedList<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<T: Hash> Hash for LinkedList<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
}
}

#[stable(feature = "std_collections_from_array", since = "1.56.0")]
impl<T, const N: usize> From<[T; N]> for LinkedList<T> {
    /// ```
    /// use std::collections::LinkedList;
    ///
    /// let list1 = LinkedList::from([1, 2, 3, 4]);
    /// let list2: LinkedList<_> = [1, 2, 3, 4].into();
    /// assert_eq!(list1, list2);
    /// ```
    fn from(arr: [T; N]) -> Self {
}
}

// Ensure that `LinkedList` and its read-only iterators are covariant in their type parameters.
#[allow(dead_code)]
fn assert_covariance() {
}

#[stable(feature = "rust1", since = "1.0.0")]
unsafe impl<T: Send> Send for LinkedList<T> {}

#[stable(feature = "rust1", since = "1.0.0")]
unsafe impl<T: Sync> Sync for LinkedList<T> {}

#[stable(feature = "rust1", since = "1.0.0")]
unsafe impl<T: Sync> Send for Iter<'_, T> {}

#[stable(feature = "rust1", since = "1.0.0")]
unsafe impl<T: Sync> Sync for Iter<'_, T> {}

#[stable(feature = "rust1", since = "1.0.0")]
unsafe impl<T: Send> Send for IterMut<'_, T> {}

#[stable(feature = "rust1", since = "1.0.0")]
unsafe impl<T: Sync> Sync for IterMut<'_, T> {}

#[unstable(feature = "linked_list_cursors", issue = "58533")]
unsafe impl<T: Sync> Send for Cursor<'_, T> {}

#[unstable(feature = "linked_list_cursors", issue = "58533")]
unsafe impl<T: Sync> Sync for Cursor<'_, T> {}

#[unstable(feature = "linked_list_cursors", issue = "58533")]
unsafe impl<T: Send> Send for CursorMut<'_, T> {}

#[unstable(feature = "linked_list_cursors", issue = "58533")]
unsafe impl<T: Sync> Sync for CursorMut<'_, T> {}
}
#[cfg(not(no_global_oom_handling))]
pub mod vec_deque {
//! A double-ended queue implemented with a growable ring buffer.
//!
//! This queue has *O*(1) amortized inserts and removals from both ends of the
//! container. It also has *O*(1) indexing like a vector. The contained elements
//! are not required to be copyable, and the queue will be sendable if the
//! contained type is sendable.

#![stable(feature = "rust1", since = "1.0.0")]

use core::cmp::{self, Ordering};
use core::fmt;
use core::hash::{Hash, Hasher};
use core::iter::{repeat_with, FromIterator};
use core::marker::PhantomData;
use core::mem::{self, ManuallyDrop};
use core::ops::{Index, IndexMut, Range, RangeBounds};
use core::ptr::{self, NonNull};
use core::slice;

use crate::alloc::{Allocator, Global};
use crate::collections::TryReserveError;
use crate::collections::TryReserveErrorKind;
use crate::raw_vec::RawVec;
use crate::vec::Vec;

#[macro_use]
mod macros {
macro_rules! __impl_slice_eq1 {
    ([$($vars:tt)*] $lhs:ty, $rhs:ty, $($constraints:tt)*) => {
        #[stable(feature = "vec_deque_partial_eq_slice", since = "1.17.0")]
        impl<T, U, A: Allocator, $($vars)*> PartialEq<$rhs> for $lhs
        where
            T: PartialEq<U>,
            $($constraints)*
        {
            fn eq(&self, other: &$rhs) -> bool {
}
        }
    }
}
}

#[stable(feature = "drain", since = "1.6.0")]
pub use self::drain::Drain;

mod drain {
use core::iter::FusedIterator;
use core::ptr::{self, NonNull};
use core::{fmt, mem};

use crate::alloc::{Allocator, Global};

use super::{count, Iter, VecDeque};

/// A draining iterator over the elements of a `VecDeque`.
///
/// This `struct` is created by the [`drain`] method on [`VecDeque`]. See its
/// documentation for more.
///
/// [`drain`]: VecDeque::drain
#[stable(feature = "drain", since = "1.6.0")]
pub struct Drain<
    'a,
    T: 'a,
    #[unstable(feature = "allocator_api", issue = "32838")] A: Allocator = Global,
> {
    pub(crate) after_tail: usize,
    pub(crate) after_head: usize,
    pub(crate) iter: Iter<'a, T>,
    pub(crate) deque: NonNull<VecDeque<T, A>>,
}

#[stable(feature = "collection_debug", since = "1.17.0")]
impl<T: fmt::Debug, A: Allocator> fmt::Debug for Drain<'_, T, A> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
}
}

#[stable(feature = "drain", since = "1.6.0")]
unsafe impl<T: Sync, A: Allocator + Sync> Sync for Drain<'_, T, A> {}
#[stable(feature = "drain", since = "1.6.0")]
unsafe impl<T: Send, A: Allocator + Send> Send for Drain<'_, T, A> {}

#[stable(feature = "drain", since = "1.6.0")]
impl<T, A: Allocator> Drop for Drain<'_, T, A> {
    fn drop(&mut self) {
}
}

#[stable(feature = "drain", since = "1.6.0")]
impl<T, A: Allocator> Iterator for Drain<'_, T, A> {
    type Item = T;

    #[inline]
    fn next(&mut self) -> Option<T> {
}

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
}
}

#[stable(feature = "drain", since = "1.6.0")]
impl<T, A: Allocator> DoubleEndedIterator for Drain<'_, T, A> {
    #[inline]
    fn next_back(&mut self) -> Option<T> {
}
}

#[stable(feature = "drain", since = "1.6.0")]
impl<T, A: Allocator> ExactSizeIterator for Drain<'_, T, A> {}

#[stable(feature = "fused", since = "1.26.0")]
impl<T, A: Allocator> FusedIterator for Drain<'_, T, A> {}
}

#[stable(feature = "rust1", since = "1.0.0")]
pub use self::iter_mut::IterMut;

mod iter_mut {
use core::fmt;
use core::iter::{FusedIterator, TrustedLen, TrustedRandomAccess, TrustedRandomAccessNoCoerce};
use core::marker::PhantomData;

use super::{count, wrap_index, RingSlices};

/// A mutable iterator over the elements of a `VecDeque`.
///
/// This `struct` is created by the [`iter_mut`] method on [`super::VecDeque`]. See its
/// documentation for more.
///
/// [`iter_mut`]: super::VecDeque::iter_mut
#[stable(feature = "rust1", since = "1.0.0")]
pub struct IterMut<'a, T: 'a> {
    // Internal safety invariant: the entire slice is dereferencable.
    pub(crate) ring: *mut [T],
    pub(crate) tail: usize,
    pub(crate) head: usize,
    pub(crate) phantom: PhantomData<&'a mut [T]>,
}

// SAFETY: we do nothing thread-local and there is no interior mutability,
// so the usual structural `Send`/`Sync` apply.
#[stable(feature = "rust1", since = "1.0.0")]
unsafe impl<T: Send> Send for IterMut<'_, T> {}
#[stable(feature = "rust1", since = "1.0.0")]
unsafe impl<T: Sync> Sync for IterMut<'_, T> {}

#[stable(feature = "collection_debug", since = "1.17.0")]
impl<T: fmt::Debug> fmt::Debug for IterMut<'_, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<'a, T> Iterator for IterMut<'a, T> {
    type Item = &'a mut T;

    #[inline]
    fn next(&mut self) -> Option<&'a mut T> {
}

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
}

    fn fold<Acc, F>(self, mut accum: Acc, mut f: F) -> Acc
    where
        F: FnMut(Acc, Self::Item) -> Acc,
    {
}

    fn nth(&mut self, n: usize) -> Option<Self::Item> {
}

    #[inline]
    fn last(mut self) -> Option<&'a mut T> {
}

    #[inline]
    #[doc(hidden)]
    unsafe fn __iterator_get_unchecked(&mut self, idx: usize) -> Self::Item {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<'a, T> DoubleEndedIterator for IterMut<'a, T> {
    #[inline]
    fn next_back(&mut self) -> Option<&'a mut T> {
}

    fn rfold<Acc, F>(self, mut accum: Acc, mut f: F) -> Acc
    where
        F: FnMut(Acc, Self::Item) -> Acc,
    {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<T> ExactSizeIterator for IterMut<'_, T> {
    fn is_empty(&self) -> bool {
}
}

#[stable(feature = "fused", since = "1.26.0")]
impl<T> FusedIterator for IterMut<'_, T> {}

#[unstable(feature = "trusted_len", issue = "37572")]
unsafe impl<T> TrustedLen for IterMut<'_, T> {}

#[doc(hidden)]
#[unstable(feature = "trusted_random_access", issue = "none")]
unsafe impl<T> TrustedRandomAccess for IterMut<'_, T> {}

#[doc(hidden)]
#[unstable(feature = "trusted_random_access", issue = "none")]
unsafe impl<T> TrustedRandomAccessNoCoerce for IterMut<'_, T> {
    const MAY_HAVE_SIDE_EFFECT: bool = false;
}
}

#[stable(feature = "rust1", since = "1.0.0")]
pub use self::into_iter::IntoIter;

mod into_iter {
use core::fmt;
use core::iter::{FusedIterator, TrustedLen};

use crate::alloc::{Allocator, Global};

use super::VecDeque;

/// An owning iterator over the elements of a `VecDeque`.
///
/// This `struct` is created by the [`into_iter`] method on [`VecDeque`]
/// (provided by the `IntoIterator` trait). See its documentation for more.
///
/// [`into_iter`]: VecDeque::into_iter
#[derive(Clone)]
#[stable(feature = "rust1", since = "1.0.0")]
pub struct IntoIter<
    T,
    #[unstable(feature = "allocator_api", issue = "32838")] A: Allocator = Global,
> {
    pub(crate) inner: VecDeque<T, A>,
}

#[stable(feature = "collection_debug", since = "1.17.0")]
impl<T: fmt::Debug, A: Allocator> fmt::Debug for IntoIter<T, A> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<T, A: Allocator> Iterator for IntoIter<T, A> {
    type Item = T;

    #[inline]
    fn next(&mut self) -> Option<T> {
}

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<T, A: Allocator> DoubleEndedIterator for IntoIter<T, A> {
    #[inline]
    fn next_back(&mut self) -> Option<T> {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<T, A: Allocator> ExactSizeIterator for IntoIter<T, A> {
    fn is_empty(&self) -> bool {
}
}

#[stable(feature = "fused", since = "1.26.0")]
impl<T, A: Allocator> FusedIterator for IntoIter<T, A> {}

#[unstable(feature = "trusted_len", issue = "37572")]
unsafe impl<T, A: Allocator> TrustedLen for IntoIter<T, A> {}
}

#[stable(feature = "rust1", since = "1.0.0")]
pub use self::iter::Iter;

mod iter {
use core::fmt;
use core::iter::{FusedIterator, TrustedLen, TrustedRandomAccess, TrustedRandomAccessNoCoerce};
use core::ops::Try;

use super::{count, wrap_index, RingSlices};

/// An iterator over the elements of a `VecDeque`.
///
/// This `struct` is created by the [`iter`] method on [`super::VecDeque`]. See its
/// documentation for more.
///
/// [`iter`]: super::VecDeque::iter
#[stable(feature = "rust1", since = "1.0.0")]
pub struct Iter<'a, T: 'a> {
    pub(crate) ring: &'a [T],
    pub(crate) tail: usize,
    pub(crate) head: usize,
}

#[stable(feature = "collection_debug", since = "1.17.0")]
impl<T: fmt::Debug> fmt::Debug for Iter<'_, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
}
}

// FIXME(#26925) Remove in favor of `#[derive(Clone)]`
#[stable(feature = "rust1", since = "1.0.0")]
impl<T> Clone for Iter<'_, T> {
    fn clone(&self) -> Self {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<'a, T> Iterator for Iter<'a, T> {
    type Item = &'a T;

    #[inline]
    fn next(&mut self) -> Option<&'a T> {
}

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
}

    fn fold<Acc, F>(self, mut accum: Acc, mut f: F) -> Acc
    where
        F: FnMut(Acc, Self::Item) -> Acc,
    {
}

    fn try_fold<B, F, R>(&mut self, init: B, mut f: F) -> R
    where
        Self: Sized,
        F: FnMut(B, Self::Item) -> R,
        R: Try<Output = B>,
    {
}

    fn nth(&mut self, n: usize) -> Option<Self::Item> {
}

    #[inline]
    fn last(mut self) -> Option<&'a T> {
}

    #[inline]
    #[doc(hidden)]
    unsafe fn __iterator_get_unchecked(&mut self, idx: usize) -> Self::Item {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<'a, T> DoubleEndedIterator for Iter<'a, T> {
    #[inline]
    fn next_back(&mut self) -> Option<&'a T> {
}

    fn rfold<Acc, F>(self, mut accum: Acc, mut f: F) -> Acc
    where
        F: FnMut(Acc, Self::Item) -> Acc,
    {
}

    fn try_rfold<B, F, R>(&mut self, init: B, mut f: F) -> R
    where
        Self: Sized,
        F: FnMut(B, Self::Item) -> R,
        R: Try<Output = B>,
    {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<T> ExactSizeIterator for Iter<'_, T> {
    fn is_empty(&self) -> bool {
}
}

#[stable(feature = "fused", since = "1.26.0")]
impl<T> FusedIterator for Iter<'_, T> {}

#[unstable(feature = "trusted_len", issue = "37572")]
unsafe impl<T> TrustedLen for Iter<'_, T> {}

#[doc(hidden)]
#[unstable(feature = "trusted_random_access", issue = "none")]
unsafe impl<T> TrustedRandomAccess for Iter<'_, T> {}

#[doc(hidden)]
#[unstable(feature = "trusted_random_access", issue = "none")]
unsafe impl<T> TrustedRandomAccessNoCoerce for Iter<'_, T> {
    const MAY_HAVE_SIDE_EFFECT: bool = false;
}
}

use self::pair_slices::PairSlices;

mod pair_slices {
use core::cmp::{self};
use core::mem::replace;

use crate::alloc::Allocator;

use super::VecDeque;

/// PairSlices pairs up equal length slice parts of two deques
///
/// For example, given deques "A" and "B" with the following division into slices:
///
/// A: [0 1 2] [3 4 5]
/// B: [a b] [c d e]
///
/// It produces the following sequence of matching slices:
///
/// ([0 1], [a b])
/// (\[2\], \[c\])
/// ([3 4], [d e])
///
/// and the uneven remainder of either A or B is skipped.
pub struct PairSlices<'a, 'b, T> {
    pub(crate) a0: &'a mut [T],
    pub(crate) a1: &'a mut [T],
    pub(crate) b0: &'b [T],
    pub(crate) b1: &'b [T],
}

impl<'a, 'b, T> PairSlices<'a, 'b, T> {
    pub fn from<A: Allocator>(to: &'a mut VecDeque<T, A>, from: &'b VecDeque<T, A>) -> Self {
}

    pub fn has_remainder(&self) -> bool {
}

    pub fn remainder(self) -> impl Iterator<Item = &'b [T]> {
}
}

impl<'a, 'b, T> Iterator for PairSlices<'a, 'b, T> {
    type Item = (&'a mut [T], &'b [T]);
    fn next(&mut self) -> Option<Self::Item> {
}
}
}

use self::ring_slices::RingSlices;

mod ring_slices {
use core::ptr::{self};

/// Returns the two slices that cover the `VecDeque`'s valid range
pub trait RingSlices: Sized {
    fn slice(self, from: usize, to: usize) -> Self;
    fn split_at(self, i: usize) -> (Self, Self);

    fn ring_slices(buf: Self, head: usize, tail: usize) -> (Self, Self) {
        let contiguous = tail <= head;
        if contiguous {
            let (empty, buf) = buf.split_at(0);
            (buf.slice(tail, head), empty)
        } else {
            let (mid, right) = buf.split_at(tail);
            let (left, _) = mid.split_at(head);
            (right, left)
        }
    }
}

impl<T> RingSlices for &[T] {
    fn slice(self, from: usize, to: usize) -> Self {
}
    fn split_at(self, i: usize) -> (Self, Self) {
}
}

impl<T> RingSlices for &mut [T] {
    fn slice(self, from: usize, to: usize) -> Self {
}
    fn split_at(self, i: usize) -> (Self, Self) {
}
}

impl<T> RingSlices for *mut [T] {
    fn slice(self, from: usize, to: usize) -> Self {
}

    fn split_at(self, mid: usize) -> (Self, Self) {
}
}
}

#[cfg(test)]
mod tests {
}

const INITIAL_CAPACITY: usize = 7; // 2^3 - 1
const MINIMUM_CAPACITY: usize = 1; // 2 - 1

const MAXIMUM_ZST_CAPACITY: usize = 1 << (usize::BITS - 1); // Largest possible power of two

/// A double-ended queue implemented with a growable ring buffer.
///
/// The "default" usage of this type as a queue is to use [`push_back`] to add to
/// the queue, and [`pop_front`] to remove from the queue. [`extend`] and [`append`]
/// push onto the back in this manner, and iterating over `VecDeque` goes front
/// to back.
///
/// A `VecDeque` with a known list of items can be initialized from an array:
///
/// ```
/// use std::collections::VecDeque;
///
/// let deq = VecDeque::from([-1, 0, 1]);
/// ```
///
/// Since `VecDeque` is a ring buffer, its elements are not necessarily contiguous
/// in memory. If you want to access the elements as a single slice, such as for
/// efficient sorting, you can use [`make_contiguous`]. It rotates the `VecDeque`
/// so that its elements do not wrap, and returns a mutable slice to the
/// now-contiguous element sequence.
///
/// [`push_back`]: VecDeque::push_back
/// [`pop_front`]: VecDeque::pop_front
/// [`extend`]: VecDeque::extend
/// [`append`]: VecDeque::append
/// [`make_contiguous`]: VecDeque::make_contiguous
#[cfg_attr(not(test), rustc_diagnostic_item = "vecdeque_type")]
#[stable(feature = "rust1", since = "1.0.0")]
#[rustc_insignificant_dtor]
pub struct VecDeque<
    T,
    #[unstable(feature = "allocator_api", issue = "32838")] A: Allocator = Global,
> {
    // tail and head are pointers into the buffer. Tail always points
    // to the first element that could be read, Head always points
    // to where data should be written.
    // If tail == head the buffer is empty. The length of the ringbuffer
    // is defined as the distance between the two.
    tail: usize,
    head: usize,
    buf: RawVec<T, A>,
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<T: Clone, A: Allocator + Clone> Clone for VecDeque<T, A> {
    fn clone(&self) -> Self {
}

    fn clone_from(&mut self, other: &Self) {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
unsafe impl<#[may_dangle] T, A: Allocator> Drop for VecDeque<T, A> {
    fn drop(&mut self) {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<T> Default for VecDeque<T> {
    /// Creates an empty `VecDeque<T>`.
    #[inline]
    fn default() -> VecDeque<T> {
}
}

impl<T, A: Allocator> VecDeque<T, A> {
    /// Marginally more convenient
    #[inline]
    fn ptr(&self) -> *mut T {
}

    /// Marginally more convenient
    #[inline]
    fn cap(&self) -> usize {
}

    /// Turn ptr into a slice
    #[inline]
    unsafe fn buffer_as_slice(&self) -> &[T] {
}

    /// Turn ptr into a mut slice
    #[inline]
    unsafe fn buffer_as_mut_slice(&mut self) -> &mut [T] {
}

    /// Moves an element out of the buffer
    #[inline]
    unsafe fn buffer_read(&mut self, off: usize) -> T {
}

    /// Writes an element into the buffer, moving it.
    #[inline]
    unsafe fn buffer_write(&mut self, off: usize, value: T) {
}

    /// Returns `true` if the buffer is at full capacity.
    #[inline]
    fn is_full(&self) -> bool {
}

    /// Returns the index in the underlying buffer for a given logical element
    /// index.
    #[inline]
    fn wrap_index(&self, idx: usize) -> usize {
}

    /// Returns the index in the underlying buffer for a given logical element
    /// index + addend.
    #[inline]
    fn wrap_add(&self, idx: usize, addend: usize) -> usize {
}

    /// Returns the index in the underlying buffer for a given logical element
    /// index - subtrahend.
    #[inline]
    fn wrap_sub(&self, idx: usize, subtrahend: usize) -> usize {
}

    /// Copies a contiguous block of memory len long from src to dst
    #[inline]
    unsafe fn copy(&self, dst: usize, src: usize, len: usize) {
}

    /// Copies a contiguous block of memory len long from src to dst
    #[inline]
    unsafe fn copy_nonoverlapping(&self, dst: usize, src: usize, len: usize) {
}

    /// Copies a potentially wrapping block of memory len long from src to dest.
    /// (abs(dst - src) + len) must be no larger than cap() (There must be at
    /// most one continuous overlapping region between src and dest).
    unsafe fn wrap_copy(&self, dst: usize, src: usize, len: usize) {
}

    /// Frobs the head and tail sections around to handle the fact that we
    /// just reallocated. Unsafe because it trusts old_capacity.
    #[inline]
    unsafe fn handle_capacity_increase(&mut self, old_capacity: usize) {
}
}

impl<T> VecDeque<T> {
    /// Creates an empty `VecDeque`.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::collections::VecDeque;
    ///
    /// let vector: VecDeque<u32> = VecDeque::new();
    /// ```
    #[inline]
    #[stable(feature = "rust1", since = "1.0.0")]
    pub fn new() -> VecDeque<T> {
}

    /// Creates an empty `VecDeque` with space for at least `capacity` elements.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::collections::VecDeque;
    ///
    /// let vector: VecDeque<u32> = VecDeque::with_capacity(10);
    /// ```
    #[inline]
    #[stable(feature = "rust1", since = "1.0.0")]
    pub fn with_capacity(capacity: usize) -> VecDeque<T> {
}
}

impl<T, A: Allocator> VecDeque<T, A> {
    /// Creates an empty `VecDeque`.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::collections::VecDeque;
    ///
    /// let vector: VecDeque<u32> = VecDeque::new();
    /// ```
    #[inline]
    #[unstable(feature = "allocator_api", issue = "32838")]
    pub fn new_in(alloc: A) -> VecDeque<T, A> {
}

    /// Creates an empty `VecDeque` with space for at least `capacity` elements.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::collections::VecDeque;
    ///
    /// let vector: VecDeque<u32> = VecDeque::with_capacity(10);
    /// ```
    #[unstable(feature = "allocator_api", issue = "32838")]
    pub fn with_capacity_in(capacity: usize, alloc: A) -> VecDeque<T, A> {
}

    /// Provides a reference to the element at the given index.
    ///
    /// Element at index 0 is the front of the queue.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::collections::VecDeque;
    ///
    /// let mut buf = VecDeque::new();
    /// buf.push_back(3);
    /// buf.push_back(4);
    /// buf.push_back(5);
    /// assert_eq!(buf.get(1), Some(&4));
    /// ```
    #[stable(feature = "rust1", since = "1.0.0")]
    pub fn get(&self, index: usize) -> Option<&T> {
}

    /// Provides a mutable reference to the element at the given index.
    ///
    /// Element at index 0 is the front of the queue.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::collections::VecDeque;
    ///
    /// let mut buf = VecDeque::new();
    /// buf.push_back(3);
    /// buf.push_back(4);
    /// buf.push_back(5);
    /// if let Some(elem) = buf.get_mut(1) {
    ///     *elem = 7;
    /// }
    ///
    /// assert_eq!(buf[1], 7);
    /// ```
    #[stable(feature = "rust1", since = "1.0.0")]
    pub fn get_mut(&mut self, index: usize) -> Option<&mut T> {
}

    /// Swaps elements at indices `i` and `j`.
    ///
    /// `i` and `j` may be equal.
    ///
    /// Element at index 0 is the front of the queue.
    ///
    /// # Panics
    ///
    /// Panics if either index is out of bounds.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::collections::VecDeque;
    ///
    /// let mut buf = VecDeque::new();
    /// buf.push_back(3);
    /// buf.push_back(4);
    /// buf.push_back(5);
    /// assert_eq!(buf, [3, 4, 5]);
    /// buf.swap(0, 2);
    /// assert_eq!(buf, [5, 4, 3]);
    /// ```
    #[stable(feature = "rust1", since = "1.0.0")]
    pub fn swap(&mut self, i: usize, j: usize) {
}

    /// Returns the number of elements the `VecDeque` can hold without
    /// reallocating.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::collections::VecDeque;
    ///
    /// let buf: VecDeque<i32> = VecDeque::with_capacity(10);
    /// assert!(buf.capacity() >= 10);
    /// ```
    #[inline]
    #[stable(feature = "rust1", since = "1.0.0")]
    pub fn capacity(&self) -> usize {
}

    /// Reserves the minimum capacity for exactly `additional` more elements to be inserted in the
    /// given `VecDeque`. Does nothing if the capacity is already sufficient.
    ///
    /// Note that the allocator may give the collection more space than it requests. Therefore
    /// capacity can not be relied upon to be precisely minimal. Prefer [`reserve`] if future
    /// insertions are expected.
    ///
    /// # Panics
    ///
    /// Panics if the new capacity overflows `usize`.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::collections::VecDeque;
    ///
    /// let mut buf: VecDeque<i32> = vec![1].into_iter().collect();
    /// buf.reserve_exact(10);
    /// assert!(buf.capacity() >= 11);
    /// ```
    ///
    /// [`reserve`]: VecDeque::reserve
    #[stable(feature = "rust1", since = "1.0.0")]
    pub fn reserve_exact(&mut self, additional: usize) {
}

    /// Reserves capacity for at least `additional` more elements to be inserted in the given
    /// `VecDeque`. The collection may reserve more space to avoid frequent reallocations.
    ///
    /// # Panics
    ///
    /// Panics if the new capacity overflows `usize`.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::collections::VecDeque;
    ///
    /// let mut buf: VecDeque<i32> = vec![1].into_iter().collect();
    /// buf.reserve(10);
    /// assert!(buf.capacity() >= 11);
    /// ```
    #[stable(feature = "rust1", since = "1.0.0")]
    pub fn reserve(&mut self, additional: usize) {
}

    /// Tries to reserve the minimum capacity for exactly `additional` more elements to
    /// be inserted in the given `VecDeque<T>`. After calling `try_reserve_exact`,
    /// capacity will be greater than or equal to `self.len() + additional`.
    /// Does nothing if the capacity is already sufficient.
    ///
    /// Note that the allocator may give the collection more space than it
    /// requests. Therefore, capacity can not be relied upon to be precisely
    /// minimal. Prefer [`reserve`] if future insertions are expected.
    ///
    /// [`reserve`]: VecDeque::reserve
    ///
    /// # Errors
    ///
    /// If the capacity overflows `usize`, or the allocator reports a failure, then an error
    /// is returned.
    ///
    /// # Examples
    ///
    /// ```
    /// #![feature(try_reserve)]
    /// use std::collections::TryReserveError;
    /// use std::collections::VecDeque;
    ///
    /// fn process_data(data: &[u32]) -> Result<VecDeque<u32>, TryReserveError> {
    ///     let mut output = VecDeque::new();
    ///
    ///     // Pre-reserve the memory, exiting if we can't
    ///     output.try_reserve_exact(data.len())?;
    ///
    ///     // Now we know this can't OOM(Out-Of-Memory) in the middle of our complex work
    ///     output.extend(data.iter().map(|&val| {
    ///         val * 2 + 5 // very complicated
    ///     }));
    ///
    ///     Ok(output)
    /// }
    /// # process_data(&[1, 2, 3]).expect("why is the test harness OOMing on 12 bytes?");
    /// ```
    #[unstable(feature = "try_reserve", reason = "new API", issue = "48043")]
    pub fn try_reserve_exact(&mut self, additional: usize) -> Result<(), TryReserveError> {
}

    /// Tries to reserve capacity for at least `additional` more elements to be inserted
    /// in the given `VecDeque<T>`. The collection may reserve more space to avoid
    /// frequent reallocations. After calling `try_reserve`, capacity will be
    /// greater than or equal to `self.len() + additional`. Does nothing if
    /// capacity is already sufficient.
    ///
    /// # Errors
    ///
    /// If the capacity overflows `usize`, or the allocator reports a failure, then an error
    /// is returned.
    ///
    /// # Examples
    ///
    /// ```
    /// #![feature(try_reserve)]
    /// use std::collections::TryReserveError;
    /// use std::collections::VecDeque;
    ///
    /// fn process_data(data: &[u32]) -> Result<VecDeque<u32>, TryReserveError> {
    ///     let mut output = VecDeque::new();
    ///
    ///     // Pre-reserve the memory, exiting if we can't
    ///     output.try_reserve(data.len())?;
    ///
    ///     // Now we know this can't OOM in the middle of our complex work
    ///     output.extend(data.iter().map(|&val| {
    ///         val * 2 + 5 // very complicated
    ///     }));
    ///
    ///     Ok(output)
    /// }
    /// # process_data(&[1, 2, 3]).expect("why is the test harness OOMing on 12 bytes?");
    /// ```
    #[unstable(feature = "try_reserve", reason = "new API", issue = "48043")]
    pub fn try_reserve(&mut self, additional: usize) -> Result<(), TryReserveError> {
}

    /// Shrinks the capacity of the `VecDeque` as much as possible.
    ///
    /// It will drop down as close as possible to the length but the allocator may still inform the
    /// `VecDeque` that there is space for a few more elements.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::collections::VecDeque;
    ///
    /// let mut buf = VecDeque::with_capacity(15);
    /// buf.extend(0..4);
    /// assert_eq!(buf.capacity(), 15);
    /// buf.shrink_to_fit();
    /// assert!(buf.capacity() >= 4);
    /// ```
    #[stable(feature = "deque_extras_15", since = "1.5.0")]
    pub fn shrink_to_fit(&mut self) {
}

    /// Shrinks the capacity of the `VecDeque` with a lower bound.
    ///
    /// The capacity will remain at least as large as both the length
    /// and the supplied value.
    ///
    /// If the current capacity is less than the lower limit, this is a no-op.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::collections::VecDeque;
    ///
    /// let mut buf = VecDeque::with_capacity(15);
    /// buf.extend(0..4);
    /// assert_eq!(buf.capacity(), 15);
    /// buf.shrink_to(6);
    /// assert!(buf.capacity() >= 6);
    /// buf.shrink_to(0);
    /// assert!(buf.capacity() >= 4);
    /// ```
    #[stable(feature = "shrink_to", since = "1.56.0")]
    pub fn shrink_to(&mut self, min_capacity: usize) {
}

    /// Shortens the `VecDeque`, keeping the first `len` elements and dropping
    /// the rest.
    ///
    /// If `len` is greater than the `VecDeque`'s current length, this has no
    /// effect.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::collections::VecDeque;
    ///
    /// let mut buf = VecDeque::new();
    /// buf.push_back(5);
    /// buf.push_back(10);
    /// buf.push_back(15);
    /// assert_eq!(buf, [5, 10, 15]);
    /// buf.truncate(1);
    /// assert_eq!(buf, [5]);
    /// ```
    #[stable(feature = "deque_extras", since = "1.16.0")]
    pub fn truncate(&mut self, len: usize) {
}

    /// Returns a reference to the underlying allocator.
    #[unstable(feature = "allocator_api", issue = "32838")]
    #[inline]
    pub fn allocator(&self) -> &A {
}

    /// Returns a front-to-back iterator.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::collections::VecDeque;
    ///
    /// let mut buf = VecDeque::new();
    /// buf.push_back(5);
    /// buf.push_back(3);
    /// buf.push_back(4);
    /// let b: &[_] = &[&5, &3, &4];
    /// let c: Vec<&i32> = buf.iter().collect();
    /// assert_eq!(&c[..], b);
    /// ```
    #[stable(feature = "rust1", since = "1.0.0")]
    pub fn iter(&self) -> Iter<'_, T> {
}

    /// Returns a front-to-back iterator that returns mutable references.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::collections::VecDeque;
    ///
    /// let mut buf = VecDeque::new();
    /// buf.push_back(5);
    /// buf.push_back(3);
    /// buf.push_back(4);
    /// for num in buf.iter_mut() {
    ///     *num = *num - 2;
    /// }
    /// let b: &[_] = &[&mut 3, &mut 1, &mut 2];
    /// assert_eq!(&buf.iter_mut().collect::<Vec<&mut i32>>()[..], b);
    /// ```
    #[stable(feature = "rust1", since = "1.0.0")]
    pub fn iter_mut(&mut self) -> IterMut<'_, T> {
}

    /// Returns a pair of slices which contain, in order, the contents of the
    /// `VecDeque`.
    ///
    /// If [`make_contiguous`] was previously called, all elements of the
    /// `VecDeque` will be in the first slice and the second slice will be empty.
    ///
    /// [`make_contiguous`]: VecDeque::make_contiguous
    ///
    /// # Examples
    ///
    /// ```
    /// use std::collections::VecDeque;
    ///
    /// let mut vector = VecDeque::new();
    ///
    /// vector.push_back(0);
    /// vector.push_back(1);
    /// vector.push_back(2);
    ///
    /// assert_eq!(vector.as_slices(), (&[0, 1, 2][..], &[][..]));
    ///
    /// vector.push_front(10);
    /// vector.push_front(9);
    ///
    /// assert_eq!(vector.as_slices(), (&[9, 10][..], &[0, 1, 2][..]));
    /// ```
    #[inline]
    #[stable(feature = "deque_extras_15", since = "1.5.0")]
    pub fn as_slices(&self) -> (&[T], &[T]) {
}

    /// Returns a pair of slices which contain, in order, the contents of the
    /// `VecDeque`.
    ///
    /// If [`make_contiguous`] was previously called, all elements of the
    /// `VecDeque` will be in the first slice and the second slice will be empty.
    ///
    /// [`make_contiguous`]: VecDeque::make_contiguous
    ///
    /// # Examples
    ///
    /// ```
    /// use std::collections::VecDeque;
    ///
    /// let mut vector = VecDeque::new();
    ///
    /// vector.push_back(0);
    /// vector.push_back(1);
    ///
    /// vector.push_front(10);
    /// vector.push_front(9);
    ///
    /// vector.as_mut_slices().0[0] = 42;
    /// vector.as_mut_slices().1[0] = 24;
    /// assert_eq!(vector.as_slices(), (&[42, 10][..], &[24, 1][..]));
    /// ```
    #[inline]
    #[stable(feature = "deque_extras_15", since = "1.5.0")]
    pub fn as_mut_slices(&mut self) -> (&mut [T], &mut [T]) {
}

    /// Returns the number of elements in the `VecDeque`.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::collections::VecDeque;
    ///
    /// let mut v = VecDeque::new();
    /// assert_eq!(v.len(), 0);
    /// v.push_back(1);
    /// assert_eq!(v.len(), 1);
    /// ```
    #[stable(feature = "rust1", since = "1.0.0")]
    pub fn len(&self) -> usize {
}

    /// Returns `true` if the `VecDeque` is empty.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::collections::VecDeque;
    ///
    /// let mut v = VecDeque::new();
    /// assert!(v.is_empty());
    /// v.push_front(1);
    /// assert!(!v.is_empty());
    /// ```
    #[stable(feature = "rust1", since = "1.0.0")]
    pub fn is_empty(&self) -> bool {
}

    fn range_tail_head<R>(&self, range: R) -> (usize, usize)
    where
        R: RangeBounds<usize>,
    {
}

    /// Creates an iterator that covers the specified range in the `VecDeque`.
    ///
    /// # Panics
    ///
    /// Panics if the starting point is greater than the end point or if
    /// the end point is greater than the length of the vector.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::collections::VecDeque;
    ///
    /// let v: VecDeque<_> = vec![1, 2, 3].into_iter().collect();
    /// let range = v.range(2..).copied().collect::<VecDeque<_>>();
    /// assert_eq!(range, [3]);
    ///
    /// // A full range covers all contents
    /// let all = v.range(..);
    /// assert_eq!(all.len(), 3);
    /// ```
    #[inline]
    #[stable(feature = "deque_range", since = "1.51.0")]
    pub fn range<R>(&self, range: R) -> Iter<'_, T>
    where
        R: RangeBounds<usize>,
    {
}

    /// Creates an iterator that covers the specified mutable range in the `VecDeque`.
    ///
    /// # Panics
    ///
    /// Panics if the starting point is greater than the end point or if
    /// the end point is greater than the length of the vector.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::collections::VecDeque;
    ///
    /// let mut v: VecDeque<_> = vec![1, 2, 3].into_iter().collect();
    /// for v in v.range_mut(2..) {
    ///   *v *= 2;
    /// }
    /// assert_eq!(v, vec![1, 2, 6]);
    ///
    /// // A full range covers all contents
    /// for v in v.range_mut(..) {
    ///   *v *= 2;
    /// }
    /// assert_eq!(v, vec![2, 4, 12]);
    /// ```
    #[inline]
    #[stable(feature = "deque_range", since = "1.51.0")]
    pub fn range_mut<R>(&mut self, range: R) -> IterMut<'_, T>
    where
        R: RangeBounds<usize>,
    {
}

    /// Creates a draining iterator that removes the specified range in the
    /// `VecDeque` and yields the removed items.
    ///
    /// Note 1: The element range is removed even if the iterator is not
    /// consumed until the end.
    ///
    /// Note 2: It is unspecified how many elements are removed from the deque,
    /// if the `Drain` value is not dropped, but the borrow it holds expires
    /// (e.g., due to `mem::forget`).
    ///
    /// # Panics
    ///
    /// Panics if the starting point is greater than the end point or if
    /// the end point is greater than the length of the vector.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::collections::VecDeque;
    ///
    /// let mut v: VecDeque<_> = vec![1, 2, 3].into_iter().collect();
    /// let drained = v.drain(2..).collect::<VecDeque<_>>();
    /// assert_eq!(drained, [3]);
    /// assert_eq!(v, [1, 2]);
    ///
    /// // A full range clears all contents
    /// v.drain(..);
    /// assert!(v.is_empty());
    /// ```
    #[inline]
    #[stable(feature = "drain", since = "1.6.0")]
    pub fn drain<R>(&mut self, range: R) -> Drain<'_, T, A>
    where
        R: RangeBounds<usize>,
    {
}

    /// Clears the `VecDeque`, removing all values.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::collections::VecDeque;
    ///
    /// let mut v = VecDeque::new();
    /// v.push_back(1);
    /// v.clear();
    /// assert!(v.is_empty());
    /// ```
    #[stable(feature = "rust1", since = "1.0.0")]
    #[inline]
    pub fn clear(&mut self) {
}

    /// Returns `true` if the `VecDeque` contains an element equal to the
    /// given value.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::collections::VecDeque;
    ///
    /// let mut vector: VecDeque<u32> = VecDeque::new();
    ///
    /// vector.push_back(0);
    /// vector.push_back(1);
    ///
    /// assert_eq!(vector.contains(&1), true);
    /// assert_eq!(vector.contains(&10), false);
    /// ```
    #[stable(feature = "vec_deque_contains", since = "1.12.0")]
    pub fn contains(&self, x: &T) -> bool
    where
        T: PartialEq<T>,
    {
}

    /// Provides a reference to the front element, or `None` if the `VecDeque` is
    /// empty.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::collections::VecDeque;
    ///
    /// let mut d = VecDeque::new();
    /// assert_eq!(d.front(), None);
    ///
    /// d.push_back(1);
    /// d.push_back(2);
    /// assert_eq!(d.front(), Some(&1));
    /// ```
    #[stable(feature = "rust1", since = "1.0.0")]
    pub fn front(&self) -> Option<&T> {
}

    /// Provides a mutable reference to the front element, or `None` if the
    /// `VecDeque` is empty.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::collections::VecDeque;
    ///
    /// let mut d = VecDeque::new();
    /// assert_eq!(d.front_mut(), None);
    ///
    /// d.push_back(1);
    /// d.push_back(2);
    /// match d.front_mut() {
    ///     Some(x) => *x = 9,
    ///     None => (),
    /// }
    /// assert_eq!(d.front(), Some(&9));
    /// ```
    #[stable(feature = "rust1", since = "1.0.0")]
    pub fn front_mut(&mut self) -> Option<&mut T> {
}

    /// Provides a reference to the back element, or `None` if the `VecDeque` is
    /// empty.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::collections::VecDeque;
    ///
    /// let mut d = VecDeque::new();
    /// assert_eq!(d.back(), None);
    ///
    /// d.push_back(1);
    /// d.push_back(2);
    /// assert_eq!(d.back(), Some(&2));
    /// ```
    #[stable(feature = "rust1", since = "1.0.0")]
    pub fn back(&self) -> Option<&T> {
}

    /// Provides a mutable reference to the back element, or `None` if the
    /// `VecDeque` is empty.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::collections::VecDeque;
    ///
    /// let mut d = VecDeque::new();
    /// assert_eq!(d.back(), None);
    ///
    /// d.push_back(1);
    /// d.push_back(2);
    /// match d.back_mut() {
    ///     Some(x) => *x = 9,
    ///     None => (),
    /// }
    /// assert_eq!(d.back(), Some(&9));
    /// ```
    #[stable(feature = "rust1", since = "1.0.0")]
    pub fn back_mut(&mut self) -> Option<&mut T> {
}

    /// Removes the first element and returns it, or `None` if the `VecDeque` is
    /// empty.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::collections::VecDeque;
    ///
    /// let mut d = VecDeque::new();
    /// d.push_back(1);
    /// d.push_back(2);
    ///
    /// assert_eq!(d.pop_front(), Some(1));
    /// assert_eq!(d.pop_front(), Some(2));
    /// assert_eq!(d.pop_front(), None);
    /// ```
    #[stable(feature = "rust1", since = "1.0.0")]
    pub fn pop_front(&mut self) -> Option<T> {
}

    /// Removes the last element from the `VecDeque` and returns it, or `None` if
    /// it is empty.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::collections::VecDeque;
    ///
    /// let mut buf = VecDeque::new();
    /// assert_eq!(buf.pop_back(), None);
    /// buf.push_back(1);
    /// buf.push_back(3);
    /// assert_eq!(buf.pop_back(), Some(3));
    /// ```
    #[stable(feature = "rust1", since = "1.0.0")]
    pub fn pop_back(&mut self) -> Option<T> {
}

    /// Prepends an element to the `VecDeque`.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::collections::VecDeque;
    ///
    /// let mut d = VecDeque::new();
    /// d.push_front(1);
    /// d.push_front(2);
    /// assert_eq!(d.front(), Some(&2));
    /// ```
    #[stable(feature = "rust1", since = "1.0.0")]
    pub fn push_front(&mut self, value: T) {
}

    /// Appends an element to the back of the `VecDeque`.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::collections::VecDeque;
    ///
    /// let mut buf = VecDeque::new();
    /// buf.push_back(1);
    /// buf.push_back(3);
    /// assert_eq!(3, *buf.back().unwrap());
    /// ```
    #[stable(feature = "rust1", since = "1.0.0")]
    pub fn push_back(&mut self, value: T) {
}

    #[inline]
    fn is_contiguous(&self) -> bool {
}

    /// Removes an element from anywhere in the `VecDeque` and returns it,
    /// replacing it with the first element.
    ///
    /// This does not preserve ordering, but is *O*(1).
    ///
    /// Returns `None` if `index` is out of bounds.
    ///
    /// Element at index 0 is the front of the queue.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::collections::VecDeque;
    ///
    /// let mut buf = VecDeque::new();
    /// assert_eq!(buf.swap_remove_front(0), None);
    /// buf.push_back(1);
    /// buf.push_back(2);
    /// buf.push_back(3);
    /// assert_eq!(buf, [1, 2, 3]);
    ///
    /// assert_eq!(buf.swap_remove_front(2), Some(3));
    /// assert_eq!(buf, [2, 1]);
    /// ```
    #[stable(feature = "deque_extras_15", since = "1.5.0")]
    pub fn swap_remove_front(&mut self, index: usize) -> Option<T> {
}

    /// Removes an element from anywhere in the `VecDeque` and returns it, replacing it with the
    /// last element.
    ///
    /// This does not preserve ordering, but is *O*(1).
    ///
    /// Returns `None` if `index` is out of bounds.
    ///
    /// Element at index 0 is the front of the queue.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::collections::VecDeque;
    ///
    /// let mut buf = VecDeque::new();
    /// assert_eq!(buf.swap_remove_back(0), None);
    /// buf.push_back(1);
    /// buf.push_back(2);
    /// buf.push_back(3);
    /// assert_eq!(buf, [1, 2, 3]);
    ///
    /// assert_eq!(buf.swap_remove_back(0), Some(1));
    /// assert_eq!(buf, [3, 2]);
    /// ```
    #[stable(feature = "deque_extras_15", since = "1.5.0")]
    pub fn swap_remove_back(&mut self, index: usize) -> Option<T> {
}

    /// Inserts an element at `index` within the `VecDeque`, shifting all elements with indices
    /// greater than or equal to `index` towards the back.
    ///
    /// Element at index 0 is the front of the queue.
    ///
    /// # Panics
    ///
    /// Panics if `index` is greater than `VecDeque`'s length
    ///
    /// # Examples
    ///
    /// ```
    /// use std::collections::VecDeque;
    ///
    /// let mut vec_deque = VecDeque::new();
    /// vec_deque.push_back('a');
    /// vec_deque.push_back('b');
    /// vec_deque.push_back('c');
    /// assert_eq!(vec_deque, &['a', 'b', 'c']);
    ///
    /// vec_deque.insert(1, 'd');
    /// assert_eq!(vec_deque, &['a', 'd', 'b', 'c']);
    /// ```
    #[stable(feature = "deque_extras_15", since = "1.5.0")]
    pub fn insert(&mut self, index: usize, value: T) {
}

    /// Removes and returns the element at `index` from the `VecDeque`.
    /// Whichever end is closer to the removal point will be moved to make
    /// room, and all the affected elements will be moved to new positions.
    /// Returns `None` if `index` is out of bounds.
    ///
    /// Element at index 0 is the front of the queue.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::collections::VecDeque;
    ///
    /// let mut buf = VecDeque::new();
    /// buf.push_back(1);
    /// buf.push_back(2);
    /// buf.push_back(3);
    /// assert_eq!(buf, [1, 2, 3]);
    ///
    /// assert_eq!(buf.remove(1), Some(2));
    /// assert_eq!(buf, [1, 3]);
    /// ```
    #[stable(feature = "rust1", since = "1.0.0")]
    pub fn remove(&mut self, index: usize) -> Option<T> {
}

    /// Splits the `VecDeque` into two at the given index.
    ///
    /// Returns a newly allocated `VecDeque`. `self` contains elements `[0, at)`,
    /// and the returned `VecDeque` contains elements `[at, len)`.
    ///
    /// Note that the capacity of `self` does not change.
    ///
    /// Element at index 0 is the front of the queue.
    ///
    /// # Panics
    ///
    /// Panics if `at > len`.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::collections::VecDeque;
    ///
    /// let mut buf: VecDeque<_> = vec![1, 2, 3].into_iter().collect();
    /// let buf2 = buf.split_off(1);
    /// assert_eq!(buf, [1]);
    /// assert_eq!(buf2, [2, 3]);
    /// ```
    #[inline]
    #[must_use = "use `.truncate()` if you don't need the other half"]
    #[stable(feature = "split_off", since = "1.4.0")]
    pub fn split_off(&mut self, at: usize) -> Self
    where
        A: Clone,
    {
}

    /// Moves all the elements of `other` into `self`, leaving `other` empty.
    ///
    /// # Panics
    ///
    /// Panics if the new number of elements in self overflows a `usize`.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::collections::VecDeque;
    ///
    /// let mut buf: VecDeque<_> = vec![1, 2].into_iter().collect();
    /// let mut buf2: VecDeque<_> = vec![3, 4].into_iter().collect();
    /// buf.append(&mut buf2);
    /// assert_eq!(buf, [1, 2, 3, 4]);
    /// assert_eq!(buf2, []);
    /// ```
    #[inline]
    #[stable(feature = "append", since = "1.4.0")]
    pub fn append(&mut self, other: &mut Self) {
}

    /// Retains only the elements specified by the predicate.
    ///
    /// In other words, remove all elements `e` such that `f(&e)` returns false.
    /// This method operates in place, visiting each element exactly once in the
    /// original order, and preserves the order of the retained elements.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::collections::VecDeque;
    ///
    /// let mut buf = VecDeque::new();
    /// buf.extend(1..5);
    /// buf.retain(|&x| x % 2 == 0);
    /// assert_eq!(buf, [2, 4]);
    /// ```
    ///
    /// Because the elements are visited exactly once in the original order,
    /// external state may be used to decide which elements to keep.
    ///
    /// ```
    /// use std::collections::VecDeque;
    ///
    /// let mut buf = VecDeque::new();
    /// buf.extend(1..6);
    ///
    /// let keep = [false, true, true, false, true];
    /// let mut iter = keep.iter();
    /// buf.retain(|_| *iter.next().unwrap());
    /// assert_eq!(buf, [2, 3, 5]);
    /// ```
    #[stable(feature = "vec_deque_retain", since = "1.4.0")]
    pub fn retain<F>(&mut self, mut f: F)
    where
        F: FnMut(&T) -> bool,
    {
}

    // This may panic or abort
    #[inline(never)]
    fn grow(&mut self) {
}

    /// Modifies the `VecDeque` in-place so that `len()` is equal to `new_len`,
    /// either by removing excess elements from the back or by appending
    /// elements generated by calling `generator` to the back.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::collections::VecDeque;
    ///
    /// let mut buf = VecDeque::new();
    /// buf.push_back(5);
    /// buf.push_back(10);
    /// buf.push_back(15);
    /// assert_eq!(buf, [5, 10, 15]);
    ///
    /// buf.resize_with(5, Default::default);
    /// assert_eq!(buf, [5, 10, 15, 0, 0]);
    ///
    /// buf.resize_with(2, || unreachable!());
    /// assert_eq!(buf, [5, 10]);
    ///
    /// let mut state = 100;
    /// buf.resize_with(5, || { state += 1; state });
    /// assert_eq!(buf, [5, 10, 101, 102, 103]);
    /// ```
    #[stable(feature = "vec_resize_with", since = "1.33.0")]
    pub fn resize_with(&mut self, new_len: usize, generator: impl FnMut() -> T) {
}

    /// Rearranges the internal storage of this deque so it is one contiguous
    /// slice, which is then returned.
    ///
    /// This method does not allocate and does not change the order of the
    /// inserted elements. As it returns a mutable slice, this can be used to
    /// sort a deque.
    ///
    /// Once the internal storage is contiguous, the [`as_slices`] and
    /// [`as_mut_slices`] methods will return the entire contents of the
    /// `VecDeque` in a single slice.
    ///
    /// [`as_slices`]: VecDeque::as_slices
    /// [`as_mut_slices`]: VecDeque::as_mut_slices
    ///
    /// # Examples
    ///
    /// Sorting the content of a deque.
    ///
    /// ```
    /// use std::collections::VecDeque;
    ///
    /// let mut buf = VecDeque::with_capacity(15);
    ///
    /// buf.push_back(2);
    /// buf.push_back(1);
    /// buf.push_front(3);
    ///
    /// // sorting the deque
    /// buf.make_contiguous().sort();
    /// assert_eq!(buf.as_slices(), (&[1, 2, 3] as &[_], &[] as &[_]));
    ///
    /// // sorting it in reverse order
    /// buf.make_contiguous().sort_by(|a, b| b.cmp(a));
    /// assert_eq!(buf.as_slices(), (&[3, 2, 1] as &[_], &[] as &[_]));
    /// ```
    ///
    /// Getting immutable access to the contiguous slice.
    ///
    /// ```rust
    /// use std::collections::VecDeque;
    ///
    /// let mut buf = VecDeque::new();
    ///
    /// buf.push_back(2);
    /// buf.push_back(1);
    /// buf.push_front(3);
    ///
    /// buf.make_contiguous();
    /// if let (slice, &[]) = buf.as_slices() {
    ///     // we can now be sure that `slice` contains all elements of the deque,
    ///     // while still having immutable access to `buf`.
    ///     assert_eq!(buf.len(), slice.len());
    ///     assert_eq!(slice, &[3, 2, 1] as &[_]);
    /// }
    /// ```
    #[stable(feature = "deque_make_contiguous", since = "1.48.0")]
    pub fn make_contiguous(&mut self) -> &mut [T] {
}

    /// Rotates the double-ended queue `mid` places to the left.
    ///
    /// Equivalently,
    /// - Rotates item `mid` into the first position.
    /// - Pops the first `mid` items and pushes them to the end.
    /// - Rotates `len() - mid` places to the right.
    ///
    /// # Panics
    ///
    /// If `mid` is greater than `len()`. Note that `mid == len()`
    /// does _not_ panic and is a no-op rotation.
    ///
    /// # Complexity
    ///
    /// Takes `*O*(min(mid, len() - mid))` time and no extra space.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::collections::VecDeque;
    ///
    /// let mut buf: VecDeque<_> = (0..10).collect();
    ///
    /// buf.rotate_left(3);
    /// assert_eq!(buf, [3, 4, 5, 6, 7, 8, 9, 0, 1, 2]);
    ///
    /// for i in 1..10 {
    ///     assert_eq!(i * 3 % 10, buf[0]);
    ///     buf.rotate_left(3);
    /// }
    /// assert_eq!(buf, [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]);
    /// ```
    #[stable(feature = "vecdeque_rotate", since = "1.36.0")]
    pub fn rotate_left(&mut self, mid: usize) {
}

    /// Rotates the double-ended queue `k` places to the right.
    ///
    /// Equivalently,
    /// - Rotates the first item into position `k`.
    /// - Pops the last `k` items and pushes them to the front.
    /// - Rotates `len() - k` places to the left.
    ///
    /// # Panics
    ///
    /// If `k` is greater than `len()`. Note that `k == len()`
    /// does _not_ panic and is a no-op rotation.
    ///
    /// # Complexity
    ///
    /// Takes `*O*(min(k, len() - k))` time and no extra space.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::collections::VecDeque;
    ///
    /// let mut buf: VecDeque<_> = (0..10).collect();
    ///
    /// buf.rotate_right(3);
    /// assert_eq!(buf, [7, 8, 9, 0, 1, 2, 3, 4, 5, 6]);
    ///
    /// for i in 1..10 {
    ///     assert_eq!(0, buf[i * 3 % 10]);
    ///     buf.rotate_right(3);
    /// }
    /// assert_eq!(buf, [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]);
    /// ```
    #[stable(feature = "vecdeque_rotate", since = "1.36.0")]
    pub fn rotate_right(&mut self, k: usize) {
}

    // SAFETY: the following two methods require that the rotation amount
    // be less than half the length of the deque.
    //
    // `wrap_copy` requires that `min(x, cap() - x) + copy_len <= cap()`,
    // but than `min` is never more than half the capacity, regardless of x,
    // so it's sound to call here because we're calling with something
    // less than half the length, which is never above half the capacity.

    unsafe fn rotate_left_inner(&mut self, mid: usize) {
}

    unsafe fn rotate_right_inner(&mut self, k: usize) {
}

    /// Binary searches this sorted `VecDeque` for a given element.
    ///
    /// If the value is found then [`Result::Ok`] is returned, containing the
    /// index of the matching element. If there are multiple matches, then any
    /// one of the matches could be returned. If the value is not found then
    /// [`Result::Err`] is returned, containing the index where a matching
    /// element could be inserted while maintaining sorted order.
    ///
    /// See also [`binary_search_by`], [`binary_search_by_key`], and [`partition_point`].
    ///
    /// [`binary_search_by`]: VecDeque::binary_search_by
    /// [`binary_search_by_key`]: VecDeque::binary_search_by_key
    /// [`partition_point`]: VecDeque::partition_point
    ///
    /// # Examples
    ///
    /// Looks up a series of four elements. The first is found, with a
    /// uniquely determined position; the second and third are not
    /// found; the fourth could match any position in `[1, 4]`.
    ///
    /// ```
    /// use std::collections::VecDeque;
    ///
    /// let deque: VecDeque<_> = vec![0, 1, 1, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55].into();
    ///
    /// assert_eq!(deque.binary_search(&13),  Ok(9));
    /// assert_eq!(deque.binary_search(&4),   Err(7));
    /// assert_eq!(deque.binary_search(&100), Err(13));
    /// let r = deque.binary_search(&1);
    /// assert!(matches!(r, Ok(1..=4)));
    /// ```
    ///
    /// If you want to insert an item to a sorted `VecDeque`, while maintaining
    /// sort order:
    ///
    /// ```
    /// use std::collections::VecDeque;
    ///
    /// let mut deque: VecDeque<_> = vec![0, 1, 1, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55].into();
    /// let num = 42;
    /// let idx = deque.binary_search(&num).unwrap_or_else(|x| x);
    /// deque.insert(idx, num);
    /// assert_eq!(deque, &[0, 1, 1, 1, 1, 2, 3, 5, 8, 13, 21, 34, 42, 55]);
    /// ```
    #[stable(feature = "vecdeque_binary_search", since = "1.54.0")]
    #[inline]
    pub fn binary_search(&self, x: &T) -> Result<usize, usize>
    where
        T: Ord,
    {
}

    /// Binary searches this sorted `VecDeque` with a comparator function.
    ///
    /// The comparator function should implement an order consistent
    /// with the sort order of the underlying `VecDeque`, returning an
    /// order code that indicates whether its argument is `Less`,
    /// `Equal` or `Greater` than the desired target.
    ///
    /// If the value is found then [`Result::Ok`] is returned, containing the
    /// index of the matching element. If there are multiple matches, then any
    /// one of the matches could be returned. If the value is not found then
    /// [`Result::Err`] is returned, containing the index where a matching
    /// element could be inserted while maintaining sorted order.
    ///
    /// See also [`binary_search`], [`binary_search_by_key`], and [`partition_point`].
    ///
    /// [`binary_search`]: VecDeque::binary_search
    /// [`binary_search_by_key`]: VecDeque::binary_search_by_key
    /// [`partition_point`]: VecDeque::partition_point
    ///
    /// # Examples
    ///
    /// Looks up a series of four elements. The first is found, with a
    /// uniquely determined position; the second and third are not
    /// found; the fourth could match any position in `[1, 4]`.
    ///
    /// ```
    /// use std::collections::VecDeque;
    ///
    /// let deque: VecDeque<_> = vec![0, 1, 1, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55].into();
    ///
    /// assert_eq!(deque.binary_search_by(|x| x.cmp(&13)),  Ok(9));
    /// assert_eq!(deque.binary_search_by(|x| x.cmp(&4)),   Err(7));
    /// assert_eq!(deque.binary_search_by(|x| x.cmp(&100)), Err(13));
    /// let r = deque.binary_search_by(|x| x.cmp(&1));
    /// assert!(matches!(r, Ok(1..=4)));
    /// ```
    #[stable(feature = "vecdeque_binary_search", since = "1.54.0")]
    pub fn binary_search_by<'a, F>(&'a self, mut f: F) -> Result<usize, usize>
    where
        F: FnMut(&'a T) -> Ordering,
    {
}

    /// Binary searches this sorted `VecDeque` with a key extraction function.
    ///
    /// Assumes that the `VecDeque` is sorted by the key, for instance with
    /// [`make_contiguous().sort_by_key()`] using the same key extraction function.
    ///
    /// If the value is found then [`Result::Ok`] is returned, containing the
    /// index of the matching element. If there are multiple matches, then any
    /// one of the matches could be returned. If the value is not found then
    /// [`Result::Err`] is returned, containing the index where a matching
    /// element could be inserted while maintaining sorted order.
    ///
    /// See also [`binary_search`], [`binary_search_by`], and [`partition_point`].
    ///
    /// [`make_contiguous().sort_by_key()`]: VecDeque::make_contiguous
    /// [`binary_search`]: VecDeque::binary_search
    /// [`binary_search_by`]: VecDeque::binary_search_by
    /// [`partition_point`]: VecDeque::partition_point
    ///
    /// # Examples
    ///
    /// Looks up a series of four elements in a slice of pairs sorted by
    /// their second elements. The first is found, with a uniquely
    /// determined position; the second and third are not found; the
    /// fourth could match any position in `[1, 4]`.
    ///
    /// ```
    /// use std::collections::VecDeque;
    ///
    /// let deque: VecDeque<_> = vec![(0, 0), (2, 1), (4, 1), (5, 1),
    ///          (3, 1), (1, 2), (2, 3), (4, 5), (5, 8), (3, 13),
    ///          (1, 21), (2, 34), (4, 55)].into();
    ///
    /// assert_eq!(deque.binary_search_by_key(&13, |&(a, b)| b),  Ok(9));
    /// assert_eq!(deque.binary_search_by_key(&4, |&(a, b)| b),   Err(7));
    /// assert_eq!(deque.binary_search_by_key(&100, |&(a, b)| b), Err(13));
    /// let r = deque.binary_search_by_key(&1, |&(a, b)| b);
    /// assert!(matches!(r, Ok(1..=4)));
    /// ```
    #[stable(feature = "vecdeque_binary_search", since = "1.54.0")]
    #[inline]
    pub fn binary_search_by_key<'a, B, F>(&'a self, b: &B, mut f: F) -> Result<usize, usize>
    where
        F: FnMut(&'a T) -> B,
        B: Ord,
    {
}

    /// Returns the index of the partition point according to the given predicate
    /// (the index of the first element of the second partition).
    ///
    /// The deque is assumed to be partitioned according to the given predicate.
    /// This means that all elements for which the predicate returns true are at the start of the deque
    /// and all elements for which the predicate returns false are at the end.
    /// For example, [7, 15, 3, 5, 4, 12, 6] is a partitioned under the predicate x % 2 != 0
    /// (all odd numbers are at the start, all even at the end).
    ///
    /// If this deque is not partitioned, the returned result is unspecified and meaningless,
    /// as this method performs a kind of binary search.
    ///
    /// See also [`binary_search`], [`binary_search_by`], and [`binary_search_by_key`].
    ///
    /// [`binary_search`]: VecDeque::binary_search
    /// [`binary_search_by`]: VecDeque::binary_search_by
    /// [`binary_search_by_key`]: VecDeque::binary_search_by_key
    ///
    /// # Examples
    ///
    /// ```
    /// use std::collections::VecDeque;
    ///
    /// let deque: VecDeque<_> = vec![1, 2, 3, 3, 5, 6, 7].into();
    /// let i = deque.partition_point(|&x| x < 5);
    ///
    /// assert_eq!(i, 4);
    /// assert!(deque.iter().take(i).all(|&x| x < 5));
    /// assert!(deque.iter().skip(i).all(|&x| !(x < 5)));
    /// ```
    #[stable(feature = "vecdeque_binary_search", since = "1.54.0")]
    pub fn partition_point<P>(&self, mut pred: P) -> usize
    where
        P: FnMut(&T) -> bool,
    {
}
}

impl<T: Clone, A: Allocator> VecDeque<T, A> {
    /// Modifies the `VecDeque` in-place so that `len()` is equal to new_len,
    /// either by removing excess elements from the back or by appending clones of `value`
    /// to the back.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::collections::VecDeque;
    ///
    /// let mut buf = VecDeque::new();
    /// buf.push_back(5);
    /// buf.push_back(10);
    /// buf.push_back(15);
    /// assert_eq!(buf, [5, 10, 15]);
    ///
    /// buf.resize(2, 0);
    /// assert_eq!(buf, [5, 10]);
    ///
    /// buf.resize(5, 20);
    /// assert_eq!(buf, [5, 10, 20, 20, 20]);
    /// ```
    #[stable(feature = "deque_extras", since = "1.16.0")]
    pub fn resize(&mut self, new_len: usize, value: T) {
}
}

/// Returns the index in the underlying buffer for a given logical element index.
#[inline]
fn wrap_index(index: usize, size: usize) -> usize {
}

/// Calculate the number of elements left to be read in the buffer
#[inline]
fn count(tail: usize, head: usize, size: usize) -> usize {
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<T: PartialEq, A: Allocator> PartialEq for VecDeque<T, A> {
    fn eq(&self, other: &Self) -> bool {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<T: Eq, A: Allocator> Eq for VecDeque<T, A> {}

__impl_slice_eq1! { [] VecDeque<T, A>, Vec<U, A>, }
__impl_slice_eq1! { [] VecDeque<T, A>, &[U], }
__impl_slice_eq1! { [] VecDeque<T, A>, &mut [U], }
__impl_slice_eq1! { [const N: usize] VecDeque<T, A>, [U; N], }
__impl_slice_eq1! { [const N: usize] VecDeque<T, A>, &[U; N], }
__impl_slice_eq1! { [const N: usize] VecDeque<T, A>, &mut [U; N], }

#[stable(feature = "rust1", since = "1.0.0")]
impl<T: PartialOrd, A: Allocator> PartialOrd for VecDeque<T, A> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<T: Ord, A: Allocator> Ord for VecDeque<T, A> {
    #[inline]
    fn cmp(&self, other: &Self) -> Ordering {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<T: Hash, A: Allocator> Hash for VecDeque<T, A> {
    fn hash<H: Hasher>(&self, state: &mut H) {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<T, A: Allocator> Index<usize> for VecDeque<T, A> {
    type Output = T;

    #[inline]
    fn index(&self, index: usize) -> &T {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<T, A: Allocator> IndexMut<usize> for VecDeque<T, A> {
    #[inline]
    fn index_mut(&mut self, index: usize) -> &mut T {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<T> FromIterator<T> for VecDeque<T> {
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> VecDeque<T> {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<T, A: Allocator> IntoIterator for VecDeque<T, A> {
    type Item = T;
    type IntoIter = IntoIter<T, A>;

    /// Consumes the `VecDeque` into a front-to-back iterator yielding elements by
    /// value.
    fn into_iter(self) -> IntoIter<T, A> {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<'a, T, A: Allocator> IntoIterator for &'a VecDeque<T, A> {
    type Item = &'a T;
    type IntoIter = Iter<'a, T>;

    fn into_iter(self) -> Iter<'a, T> {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<'a, T, A: Allocator> IntoIterator for &'a mut VecDeque<T, A> {
    type Item = &'a mut T;
    type IntoIter = IterMut<'a, T>;

    fn into_iter(self) -> IterMut<'a, T> {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<T, A: Allocator> Extend<T> for VecDeque<T, A> {
    fn extend<I: IntoIterator<Item = T>>(&mut self, iter: I) {
}

    #[inline]
    fn extend_one(&mut self, elem: T) {
}

    #[inline]
    fn extend_reserve(&mut self, additional: usize) {
}
}

#[stable(feature = "extend_ref", since = "1.2.0")]
impl<'a, T: 'a + Copy, A: Allocator> Extend<&'a T> for VecDeque<T, A> {
    fn extend<I: IntoIterator<Item = &'a T>>(&mut self, iter: I) {
}

    #[inline]
    fn extend_one(&mut self, &elem: &T) {
}

    #[inline]
    fn extend_reserve(&mut self, additional: usize) {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<T: fmt::Debug, A: Allocator> fmt::Debug for VecDeque<T, A> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
}
}

#[stable(feature = "vecdeque_vec_conversions", since = "1.10.0")]
impl<T, A: Allocator> From<Vec<T, A>> for VecDeque<T, A> {
    /// Turn a [`Vec<T>`] into a [`VecDeque<T>`].
    ///
    /// [`Vec<T>`]: crate::vec::Vec
    /// [`VecDeque<T>`]: crate::collections::VecDeque
    ///
    /// This avoids reallocating where possible, but the conditions for that are
    /// strict, and subject to change, and so shouldn't be relied upon unless the
    /// `Vec<T>` came from `From<VecDeque<T>>` and hasn't been reallocated.
    fn from(mut other: Vec<T, A>) -> Self {
}
}

#[stable(feature = "vecdeque_vec_conversions", since = "1.10.0")]
impl<T, A: Allocator> From<VecDeque<T, A>> for Vec<T, A> {
    /// Turn a [`VecDeque<T>`] into a [`Vec<T>`].
    ///
    /// [`Vec<T>`]: crate::vec::Vec
    /// [`VecDeque<T>`]: crate::collections::VecDeque
    ///
    /// This never needs to re-allocate, but does need to do *O*(*n*) data movement if
    /// the circular buffer doesn't happen to be at the beginning of the allocation.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::collections::VecDeque;
    ///
    /// // This one is *O*(1).
    /// let deque: VecDeque<_> = (1..5).collect();
    /// let ptr = deque.as_slices().0.as_ptr();
    /// let vec = Vec::from(deque);
    /// assert_eq!(vec, [1, 2, 3, 4]);
    /// assert_eq!(vec.as_ptr(), ptr);
    ///
    /// // This one needs data rearranging.
    /// let mut deque: VecDeque<_> = (1..5).collect();
    /// deque.push_front(9);
    /// deque.push_front(8);
    /// let ptr = deque.as_slices().1.as_ptr();
    /// let vec = Vec::from(deque);
    /// assert_eq!(vec, [8, 9, 1, 2, 3, 4]);
    /// assert_eq!(vec.as_ptr(), ptr);
    /// ```
    fn from(mut other: VecDeque<T, A>) -> Self {
}
}

#[stable(feature = "std_collections_from_array", since = "1.56.0")]
impl<T, const N: usize> From<[T; N]> for VecDeque<T> {
    /// ```
    /// use std::collections::VecDeque;
    ///
    /// let deq1 = VecDeque::from([1, 2, 3, 4]);
    /// let deq2: VecDeque<_> = [1, 2, 3, 4].into();
    /// assert_eq!(deq1, deq2);
    /// ```
    fn from(arr: [T; N]) -> Self {
}
}
}

#[cfg(not(no_global_oom_handling))]
#[stable(feature = "rust1", since = "1.0.0")]
pub mod btree_map {
    //! A map based on a B-Tree.
    #[stable(feature = "rust1", since = "1.0.0")]
    pub use super::btree::map::*;
}

#[cfg(not(no_global_oom_handling))]
#[stable(feature = "rust1", since = "1.0.0")]
pub mod btree_set {
    //! A set based on a B-Tree.
    #[stable(feature = "rust1", since = "1.0.0")]
    pub use super::btree::set::*;
}

#[cfg(not(no_global_oom_handling))]
#[stable(feature = "rust1", since = "1.0.0")]
#[doc(no_inline)]
pub use binary_heap::BinaryHeap;

#[cfg(not(no_global_oom_handling))]
#[stable(feature = "rust1", since = "1.0.0")]
#[doc(no_inline)]
pub use btree_map::BTreeMap;

#[cfg(not(no_global_oom_handling))]
#[stable(feature = "rust1", since = "1.0.0")]
#[doc(no_inline)]
pub use btree_set::BTreeSet;

#[cfg(not(no_global_oom_handling))]
#[stable(feature = "rust1", since = "1.0.0")]
#[doc(no_inline)]
pub use linked_list::LinkedList;

#[cfg(not(no_global_oom_handling))]
#[stable(feature = "rust1", since = "1.0.0")]
#[doc(no_inline)]
pub use vec_deque::VecDeque;

use crate::alloc::{Layout, LayoutError};
use core::fmt::Display;

/// The error type for `try_reserve` methods.
#[derive(Clone, PartialEq, Eq, Debug)]
#[unstable(feature = "try_reserve", reason = "new API", issue = "48043")]
pub struct TryReserveError {
    kind: TryReserveErrorKind,
}

impl TryReserveError {
    /// Details about the allocation that caused the error
    #[inline]
    #[unstable(
        feature = "try_reserve_kind",
        reason = "Uncertain how much info should be exposed",
        issue = "48043"
    )]
    pub fn kind(&self) -> TryReserveErrorKind {
}
}

/// Details of the allocation that caused a `TryReserveError`
#[derive(Clone, PartialEq, Eq, Debug)]
#[unstable(
    feature = "try_reserve_kind",
    reason = "Uncertain how much info should be exposed",
    issue = "48043"
)]
pub enum TryReserveErrorKind {
    /// Error due to the computed capacity exceeding the collection's maximum
    /// (usually `isize::MAX` bytes).
    CapacityOverflow,

    /// The memory allocator returned an error
    AllocError {
        /// The layout of allocation request that failed
        layout: Layout,

        #[doc(hidden)]
        #[unstable(
            feature = "container_error_extra",
            issue = "none",
            reason = "\
            Enable exposing the allocator’s custom error value \
            if an associated type is added in the future: \
            https://github.com/rust-lang/wg-allocators/issues/23"
        )]
        non_exhaustive: (),
    },
}

#[unstable(
    feature = "try_reserve_kind",
    reason = "Uncertain how much info should be exposed",
    issue = "48043"
)]
impl From<TryReserveErrorKind> for TryReserveError {
    #[inline]
    fn from(kind: TryReserveErrorKind) -> Self {
}
}

#[unstable(feature = "try_reserve_kind", reason = "new API", issue = "48043")]
impl From<LayoutError> for TryReserveErrorKind {
    /// Always evaluates to [`TryReserveErrorKind::CapacityOverflow`].
    #[inline]
    fn from(_: LayoutError) -> Self {
}
}

#[unstable(feature = "try_reserve", reason = "new API", issue = "48043")]
impl Display for TryReserveError {
    fn fmt(
        &self,
        fmt: &mut core::fmt::Formatter<'_>,
    ) -> core::result::Result<(), core::fmt::Error> {
}
}

/// An intermediate trait for specialization of `Extend`.
#[doc(hidden)]
trait SpecExtend<I: IntoIterator> {
    /// Extends `self` with the contents of the given iterator.
    fn spec_extend(&mut self, iter: I);
}
}
pub mod fmt {
//! Utilities for formatting and printing `String`s.
//!
//! This module contains the runtime support for the [`format!`] syntax extension.
//! This macro is implemented in the compiler to emit calls to this module in
//! order to format arguments at runtime into strings.
//!
//! # Usage
//!
//! The [`format!`] macro is intended to be familiar to those coming from C's
//! `printf`/`fprintf` functions or Python's `str.format` function.
//!
//! Some examples of the [`format!`] extension are:
//!
//! ```
//! format!("Hello");                 // => "Hello"
//! format!("Hello, {}!", "world");   // => "Hello, world!"
//! format!("The number is {}", 1);   // => "The number is 1"
//! format!("{:?}", (3, 4));          // => "(3, 4)"
//! format!("{value}", value=4);      // => "4"
//! format!("{} {}", 1, 2);           // => "1 2"
//! format!("{:04}", 42);             // => "0042" with leading zeros
//! format!("{:#?}", (100, 200));     // => "(
//!                                   //       100,
//!                                   //       200,
//!                                   //     )"
//! ```
//!
//! From these, you can see that the first argument is a format string. It is
//! required by the compiler for this to be a string literal; it cannot be a
//! variable passed in (in order to perform validity checking). The compiler
//! will then parse the format string and determine if the list of arguments
//! provided is suitable to pass to this format string.
//!
//! To convert a single value to a string, use the [`to_string`] method. This
//! will use the [`Display`] formatting trait.
//!
//! ## Positional parameters
//!
//! Each formatting argument is allowed to specify which value argument it's
//! referencing, and if omitted it is assumed to be "the next argument". For
//! example, the format string `{} {} {}` would take three parameters, and they
//! would be formatted in the same order as they're given. The format string
//! `{2} {1} {0}`, however, would format arguments in reverse order.
//!
//! Things can get a little tricky once you start intermingling the two types of
//! positional specifiers. The "next argument" specifier can be thought of as an
//! iterator over the argument. Each time a "next argument" specifier is seen,
//! the iterator advances. This leads to behavior like this:
//!
//! ```
//! format!("{1} {} {0} {}", 1, 2); // => "2 1 1 2"
//! ```
//!
//! The internal iterator over the argument has not been advanced by the time
//! the first `{}` is seen, so it prints the first argument. Then upon reaching
//! the second `{}`, the iterator has advanced forward to the second argument.
//! Essentially, parameters that explicitly name their argument do not affect
//! parameters that do not name an argument in terms of positional specifiers.
//!
//! A format string is required to use all of its arguments, otherwise it is a
//! compile-time error. You may refer to the same argument more than once in the
//! format string.
//!
//! ## Named parameters
//!
//! Rust itself does not have a Python-like equivalent of named parameters to a
//! function, but the [`format!`] macro is a syntax extension that allows it to
//! leverage named parameters. Named parameters are listed at the end of the
//! argument list and have the syntax:
//!
//! ```text
//! identifier '=' expression
//! ```
//!
//! For example, the following [`format!`] expressions all use named argument:
//!
//! ```
//! format!("{argument}", argument = "test");   // => "test"
//! format!("{name} {}", 1, name = 2);          // => "2 1"
//! format!("{a} {c} {b}", a="a", b='b', c=3);  // => "a 3 b"
//! ```
//!
//! It is not valid to put positional parameters (those without names) after
//! arguments that have names. Like with positional parameters, it is not
//! valid to provide named parameters that are unused by the format string.
//!
//! # Formatting Parameters
//!
//! Each argument being formatted can be transformed by a number of formatting
//! parameters (corresponding to `format_spec` in [the syntax](#syntax)). These
//! parameters affect the string representation of what's being formatted.
//!
//! ## Width
//!
//! ```
//! // All of these print "Hello x    !"
//! println!("Hello {:5}!", "x");
//! println!("Hello {:1$}!", "x", 5);
//! println!("Hello {1:0$}!", 5, "x");
//! println!("Hello {:width$}!", "x", width = 5);
//! ```
//!
//! This is a parameter for the "minimum width" that the format should take up.
//! If the value's string does not fill up this many characters, then the
//! padding specified by fill/alignment will be used to take up the required
//! space (see below).
//!
//! The value for the width can also be provided as a [`usize`] in the list of
//! parameters by adding a postfix `$`, indicating that the second argument is
//! a [`usize`] specifying the width.
//!
//! Referring to an argument with the dollar syntax does not affect the "next
//! argument" counter, so it's usually a good idea to refer to arguments by
//! position, or use named arguments.
//!
//! ## Fill/Alignment
//!
//! ```
//! assert_eq!(format!("Hello {:<5}!", "x"),  "Hello x    !");
//! assert_eq!(format!("Hello {:-<5}!", "x"), "Hello x----!");
//! assert_eq!(format!("Hello {:^5}!", "x"),  "Hello   x  !");
//! assert_eq!(format!("Hello {:>5}!", "x"),  "Hello     x!");
//! ```
//!
//! The optional fill character and alignment is provided normally in conjunction with the
//! [`width`](#width) parameter. It must be defined before `width`, right after the `:`.
//! This indicates that if the value being formatted is smaller than
//! `width` some extra characters will be printed around it.
//! Filling comes in the following variants for different alignments:
//!
//! * `[fill]<` - the argument is left-aligned in `width` columns
//! * `[fill]^` - the argument is center-aligned in `width` columns
//! * `[fill]>` - the argument is right-aligned in `width` columns
//!
//! The default [fill/alignment](#fillalignment) for non-numerics is a space and
//! left-aligned. The
//! default for numeric formatters is also a space character but with right-alignment. If
//! the `0` flag (see below) is specified for numerics, then the implicit fill character is
//! `0`.
//!
//! Note that alignment might not be implemented by some types. In particular, it
//! is not generally implemented for the `Debug` trait.  A good way to ensure
//! padding is applied is to format your input, then pad this resulting string
//! to obtain your output:
//!
//! ```
//! println!("Hello {:^15}!", format!("{:?}", Some("hi"))); // => "Hello   Some("hi")   !"
//! ```
//!
//! ## Sign/`#`/`0`
//!
//! ```
//! assert_eq!(format!("Hello {:+}!", 5), "Hello +5!");
//! assert_eq!(format!("{:#x}!", 27), "0x1b!");
//! assert_eq!(format!("Hello {:05}!", 5),  "Hello 00005!");
//! assert_eq!(format!("Hello {:05}!", -5), "Hello -0005!");
//! assert_eq!(format!("{:#010x}!", 27), "0x0000001b!");
//! ```
//!
//! These are all flags altering the behavior of the formatter.
//!
//! * `+` - This is intended for numeric types and indicates that the sign
//!         should always be printed. Positive signs are never printed by
//!         default, and the negative sign is only printed by default for signed values.
//!         This flag indicates that the correct sign (`+` or `-`) should always be printed.
//! * `-` - Currently not used
//! * `#` - This flag indicates that the "alternate" form of printing should
//!         be used. The alternate forms are:
//!     * `#?` - pretty-print the [`Debug`] formatting (adds linebreaks and indentation)
//!     * `#x` - precedes the argument with a `0x`
//!     * `#X` - precedes the argument with a `0x`
//!     * `#b` - precedes the argument with a `0b`
//!     * `#o` - precedes the argument with a `0o`
//! * `0` - This is used to indicate for integer formats that the padding to `width` should
//!         both be done with a `0` character as well as be sign-aware. A format
//!         like `{:08}` would yield `00000001` for the integer `1`, while the
//!         same format would yield `-0000001` for the integer `-1`. Notice that
//!         the negative version has one fewer zero than the positive version.
//!         Note that padding zeros are always placed after the sign (if any)
//!         and before the digits. When used together with the `#` flag, a similar
//!         rule applies: padding zeros are inserted after the prefix but before
//!         the digits. The prefix is included in the total width.
//!
//! ## Precision
//!
//! For non-numeric types, this can be considered a "maximum width". If the resulting string is
//! longer than this width, then it is truncated down to this many characters and that truncated
//! value is emitted with proper `fill`, `alignment` and `width` if those parameters are set.
//!
//! For integral types, this is ignored.
//!
//! For floating-point types, this indicates how many digits after the decimal point should be
//! printed.
//!
//! There are three possible ways to specify the desired `precision`:
//!
//! 1. An integer `.N`:
//!
//!    the integer `N` itself is the precision.
//!
//! 2. An integer or name followed by dollar sign `.N$`:
//!
//!    use format *argument* `N` (which must be a `usize`) as the precision.
//!
//! 3. An asterisk `.*`:
//!
//!    `.*` means that this `{...}` is associated with *two* format inputs rather than one: the
//!    first input holds the `usize` precision, and the second holds the value to print. Note that
//!    in this case, if one uses the format string `{<arg>:<spec>.*}`, then the `<arg>` part refers
//!    to the *value* to print, and the `precision` must come in the input preceding `<arg>`.
//!
//! For example, the following calls all print the same thing `Hello x is 0.01000`:
//!
//! ```
//! // Hello {arg 0 ("x")} is {arg 1 (0.01) with precision specified inline (5)}
//! println!("Hello {0} is {1:.5}", "x", 0.01);
//!
//! // Hello {arg 1 ("x")} is {arg 2 (0.01) with precision specified in arg 0 (5)}
//! println!("Hello {1} is {2:.0$}", 5, "x", 0.01);
//!
//! // Hello {arg 0 ("x")} is {arg 2 (0.01) with precision specified in arg 1 (5)}
//! println!("Hello {0} is {2:.1$}", "x", 5, 0.01);
//!
//! // Hello {next arg ("x")} is {second of next two args (0.01) with precision
//! //                          specified in first of next two args (5)}
//! println!("Hello {} is {:.*}",    "x", 5, 0.01);
//!
//! // Hello {next arg ("x")} is {arg 2 (0.01) with precision
//! //                          specified in its predecessor (5)}
//! println!("Hello {} is {2:.*}",   "x", 5, 0.01);
//!
//! // Hello {next arg ("x")} is {arg "number" (0.01) with precision specified
//! //                          in arg "prec" (5)}
//! println!("Hello {} is {number:.prec$}", "x", prec = 5, number = 0.01);
//! ```
//!
//! While these:
//!
//! ```
//! println!("{}, `{name:.*}` has 3 fractional digits", "Hello", 3, name=1234.56);
//! println!("{}, `{name:.*}` has 3 characters", "Hello", 3, name="1234.56");
//! println!("{}, `{name:>8.*}` has 3 right-aligned characters", "Hello", 3, name="1234.56");
//! ```
//!
//! print three significantly different things:
//!
//! ```text
//! Hello, `1234.560` has 3 fractional digits
//! Hello, `123` has 3 characters
//! Hello, `     123` has 3 right-aligned characters
//! ```
//!
//! ## Localization
//!
//! In some programming languages, the behavior of string formatting functions
//! depends on the operating system's locale setting. The format functions
//! provided by Rust's standard library do not have any concept of locale and
//! will produce the same results on all systems regardless of user
//! configuration.
//!
//! For example, the following code will always print `1.5` even if the system
//! locale uses a decimal separator other than a dot.
//!
//! ```
//! println!("The value is {}", 1.5);
//! ```
//!
//! # Escaping
//!
//! The literal characters `{` and `}` may be included in a string by preceding
//! them with the same character. For example, the `{` character is escaped with
//! `{{` and the `}` character is escaped with `}}`.
//!
//! ```
//! assert_eq!(format!("Hello {{}}"), "Hello {}");
//! assert_eq!(format!("{{ Hello"), "{ Hello");
//! ```
//!
//! # Syntax
//!
//! To summarize, here you can find the full grammar of format strings.
//! The syntax for the formatting language used is drawn from other languages,
//! so it should not be too alien. Arguments are formatted with Python-like
//! syntax, meaning that arguments are surrounded by `{}` instead of the C-like
//! `%`. The actual grammar for the formatting syntax is:
//!
//! ```text
//! format_string := text [ maybe_format text ] *
//! maybe_format := '{' '{' | '}' '}' | format
//! format := '{' [ argument ] [ ':' format_spec ] '}'
//! argument := integer | identifier
//!
//! format_spec := [[fill]align][sign]['#']['0'][width]['.' precision]type
//! fill := character
//! align := '<' | '^' | '>'
//! sign := '+' | '-'
//! width := count
//! precision := count | '*'
//! type := '' | '?' | 'x?' | 'X?' | identifier
//! count := parameter | integer
//! parameter := argument '$'
//! ```
//! In the above grammar, `text` must not contain any `'{'` or `'}'` characters.
//!
//! # Formatting traits
//!
//! When requesting that an argument be formatted with a particular type, you
//! are actually requesting that an argument ascribes to a particular trait.
//! This allows multiple actual types to be formatted via `{:x}` (like [`i8`] as
//! well as [`isize`]). The current mapping of types to traits is:
//!
//! * *nothing* ⇒ [`Display`]
//! * `?` ⇒ [`Debug`]
//! * `x?` ⇒ [`Debug`] with lower-case hexadecimal integers
//! * `X?` ⇒ [`Debug`] with upper-case hexadecimal integers
//! * `o` ⇒ [`Octal`]
//! * `x` ⇒ [`LowerHex`]
//! * `X` ⇒ [`UpperHex`]
//! * `p` ⇒ [`Pointer`]
//! * `b` ⇒ [`Binary`]
//! * `e` ⇒ [`LowerExp`]
//! * `E` ⇒ [`UpperExp`]
//!
//! What this means is that any type of argument which implements the
//! [`fmt::Binary`][`Binary`] trait can then be formatted with `{:b}`. Implementations
//! are provided for these traits for a number of primitive types by the
//! standard library as well. If no format is specified (as in `{}` or `{:6}`),
//! then the format trait used is the [`Display`] trait.
//!
//! When implementing a format trait for your own type, you will have to
//! implement a method of the signature:
//!
//! ```
//! # #![allow(dead_code)]
//! # use std::fmt;
//! # struct Foo; // our custom type
//! # impl fmt::Display for Foo {
//! fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
//! # write!(f, "testing, testing")
//! # } }
//! ```
//!
//! Your type will be passed as `self` by-reference, and then the function
//! should emit output into the `f.buf` stream. It is up to each format trait
//! implementation to correctly adhere to the requested formatting parameters.
//! The values of these parameters will be listed in the fields of the
//! [`Formatter`] struct. In order to help with this, the [`Formatter`] struct also
//! provides some helper methods.
//!
//! Additionally, the return value of this function is [`fmt::Result`] which is a
//! type alias of [`Result`]`<(), `[`std::fmt::Error`]`>`. Formatting implementations
//! should ensure that they propagate errors from the [`Formatter`] (e.g., when
//! calling [`write!`]). However, they should never return errors spuriously. That
//! is, a formatting implementation must and may only return an error if the
//! passed-in [`Formatter`] returns an error. This is because, contrary to what
//! the function signature might suggest, string formatting is an infallible
//! operation. This function only returns a result because writing to the
//! underlying stream might fail and it must provide a way to propagate the fact
//! that an error has occurred back up the stack.
//!
//! An example of implementing the formatting traits would look
//! like:
//!
//! ```
//! use std::fmt;
//!
//! #[derive(Debug)]
//! struct Vector2D {
//!     x: isize,
//!     y: isize,
//! }
//!
//! impl fmt::Display for Vector2D {
//!     fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
//!         // The `f` value implements the `Write` trait, which is what the
//!         // write! macro is expecting. Note that this formatting ignores the
//!         // various flags provided to format strings.
//!         write!(f, "({}, {})", self.x, self.y)
//!     }
//! }
//!
//! // Different traits allow different forms of output of a type. The meaning
//! // of this format is to print the magnitude of a vector.
//! impl fmt::Binary for Vector2D {
//!     fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
//!         let magnitude = (self.x * self.x + self.y * self.y) as f64;
//!         let magnitude = magnitude.sqrt();
//!
//!         // Respect the formatting flags by using the helper method
//!         // `pad_integral` on the Formatter object. See the method
//!         // documentation for details, and the function `pad` can be used
//!         // to pad strings.
//!         let decimals = f.precision().unwrap_or(3);
//!         let string = format!("{:.*}", decimals, magnitude);
//!         f.pad_integral(true, "", &string)
//!     }
//! }
//!
//! fn main() {
//!     let myvector = Vector2D { x: 3, y: 4 };
//!
//!     println!("{}", myvector);       // => "(3, 4)"
//!     println!("{:?}", myvector);     // => "Vector2D {x: 3, y:4}"
//!     println!("{:10.3b}", myvector); // => "     5.000"
//! }
//! ```
//!
//! ### `fmt::Display` vs `fmt::Debug`
//!
//! These two formatting traits have distinct purposes:
//!
//! - [`fmt::Display`][`Display`] implementations assert that the type can be faithfully
//!   represented as a UTF-8 string at all times. It is **not** expected that
//!   all types implement the [`Display`] trait.
//! - [`fmt::Debug`][`Debug`] implementations should be implemented for **all** public types.
//!   Output will typically represent the internal state as faithfully as possible.
//!   The purpose of the [`Debug`] trait is to facilitate debugging Rust code. In
//!   most cases, using `#[derive(Debug)]` is sufficient and recommended.
//!
//! Some examples of the output from both traits:
//!
//! ```
//! assert_eq!(format!("{} {:?}", 3, 4), "3 4");
//! assert_eq!(format!("{} {:?}", 'a', 'b'), "a 'b'");
//! assert_eq!(format!("{} {:?}", "foo\n", "bar\n"), "foo\n \"bar\\n\"");
//! ```
//!
//! # Related macros
//!
//! There are a number of related macros in the [`format!`] family. The ones that
//! are currently implemented are:
//!
//! ```ignore (only-for-syntax-highlight)
//! format!      // described above
//! write!       // first argument is a &mut io::Write, the destination
//! writeln!     // same as write but appends a newline
//! print!       // the format string is printed to the standard output
//! println!     // same as print but appends a newline
//! eprint!      // the format string is printed to the standard error
//! eprintln!    // same as eprint but appends a newline
//! format_args! // described below.
//! ```
//!
//! ### `write!`
//!
//! This and [`writeln!`] are two macros which are used to emit the format string
//! to a specified stream. This is used to prevent intermediate allocations of
//! format strings and instead directly write the output. Under the hood, this
//! function is actually invoking the [`write_fmt`] function defined on the
//! [`std::io::Write`] trait. Example usage is:
//!
//! ```
//! # #![allow(unused_must_use)]
//! use std::io::Write;
//! let mut w = Vec::new();
//! write!(&mut w, "Hello {}!", "world");
//! ```
//!
//! ### `print!`
//!
//! This and [`println!`] emit their output to stdout. Similarly to the [`write!`]
//! macro, the goal of these macros is to avoid intermediate allocations when
//! printing output. Example usage is:
//!
//! ```
//! print!("Hello {}!", "world");
//! println!("I have a newline {}", "character at the end");
//! ```
//! ### `eprint!`
//!
//! The [`eprint!`] and [`eprintln!`] macros are identical to
//! [`print!`] and [`println!`], respectively, except they emit their
//! output to stderr.
//!
//! ### `format_args!`
//!
//! This is a curious macro used to safely pass around
//! an opaque object describing the format string. This object
//! does not require any heap allocations to create, and it only
//! references information on the stack. Under the hood, all of
//! the related macros are implemented in terms of this. First
//! off, some example usage is:
//!
//! ```
//! # #![allow(unused_must_use)]
//! use std::fmt;
//! use std::io::{self, Write};
//!
//! let mut some_writer = io::stdout();
//! write!(&mut some_writer, "{}", format_args!("print with a {}", "macro"));
//!
//! fn my_fmt_fn(args: fmt::Arguments) {
//!     write!(&mut io::stdout(), "{}", args);
//! }
//! my_fmt_fn(format_args!(", or a {} too", "function"));
//! ```
//!
//! The result of the [`format_args!`] macro is a value of type [`fmt::Arguments`].
//! This structure can then be passed to the [`write`] and [`format`] functions
//! inside this module in order to process the format string.
//! The goal of this macro is to even further prevent intermediate allocations
//! when dealing with formatting strings.
//!
//! For example, a logging library could use the standard formatting syntax, but
//! it would internally pass around this structure until it has been determined
//! where output should go to.
//!
//! [`fmt::Result`]: Result
//! [`Result`]: core::result::Result
//! [`std::fmt::Error`]: Error
//! [`write!`]: core::write
//! [`write`]: core::write
//! [`format!`]: crate::format
//! [`to_string`]: crate::string::ToString
//! [`writeln!`]: core::writeln
//! [`write_fmt`]: ../../std/io/trait.Write.html#method.write_fmt
//! [`std::io::Write`]: ../../std/io/trait.Write.html
//! [`print!`]: ../../std/macro.print.html
//! [`println!`]: ../../std/macro.println.html
//! [`eprint!`]: ../../std/macro.eprint.html
//! [`eprintln!`]: ../../std/macro.eprintln.html
//! [`format_args!`]: core::format_args
//! [`fmt::Arguments`]: Arguments
//! [`format`]: crate::format

#![stable(feature = "rust1", since = "1.0.0")]

#[unstable(feature = "fmt_internals", issue = "none")]
pub use core::fmt::rt;
#[stable(feature = "fmt_flags_align", since = "1.28.0")]
pub use core::fmt::Alignment;
#[stable(feature = "rust1", since = "1.0.0")]
pub use core::fmt::Error;
#[stable(feature = "rust1", since = "1.0.0")]
pub use core::fmt::{write, ArgumentV1, Arguments};
#[stable(feature = "rust1", since = "1.0.0")]
pub use core::fmt::{Binary, Octal};
#[stable(feature = "rust1", since = "1.0.0")]
pub use core::fmt::{Debug, Display};
#[stable(feature = "rust1", since = "1.0.0")]
pub use core::fmt::{DebugList, DebugMap, DebugSet, DebugStruct, DebugTuple};
#[stable(feature = "rust1", since = "1.0.0")]
pub use core::fmt::{Formatter, Result, Write};
#[stable(feature = "rust1", since = "1.0.0")]
pub use core::fmt::{LowerExp, UpperExp};
#[stable(feature = "rust1", since = "1.0.0")]
pub use core::fmt::{LowerHex, Pointer, UpperHex};

#[cfg(not(no_global_oom_handling))]
use crate::string;

/// The `format` function takes an [`Arguments`] struct and returns the resulting
/// formatted string.
///
/// The [`Arguments`] instance can be created with the [`format_args!`] macro.
///
/// # Examples
///
/// Basic usage:
///
/// ```
/// use std::fmt;
///
/// let s = fmt::format(format_args!("Hello, {}!", "world"));
/// assert_eq!(s, "Hello, world!");
/// ```
///
/// Please note that using [`format!`] might be preferable.
/// Example:
///
/// ```
/// let s = format!("Hello, {}!", "world");
/// assert_eq!(s, "Hello, world!");
/// ```
///
/// [`format_args!`]: core::format_args
/// [`format!`]: crate::format
#[cfg(not(no_global_oom_handling))]
#[stable(feature = "rust1", since = "1.0.0")]
pub fn format(args: Arguments<'_>) -> string::String {
}
}
pub mod prelude {
//! The alloc Prelude
//!
//! The purpose of this module is to alleviate imports of commonly-used
//! items of the `alloc` crate by adding a glob import to the top of modules:
//!
//! ```
//! # #![allow(unused_imports)]
//! #![feature(alloc_prelude)]
//! extern crate alloc;
//! use alloc::prelude::v1::*;
//! ```

#![unstable(feature = "alloc_prelude", issue = "58935")]

pub mod v1 {
//! The first version of the prelude of `alloc` crate.
//!
//! See the [module-level documentation](../index.html) for more.

#![unstable(feature = "alloc_prelude", issue = "58935")]

#[unstable(feature = "alloc_prelude", issue = "58935")]
pub use crate::borrow::ToOwned;
#[unstable(feature = "alloc_prelude", issue = "58935")]
pub use crate::boxed::Box;
#[unstable(feature = "alloc_prelude", issue = "58935")]
pub use crate::string::{String, ToString};
#[unstable(feature = "alloc_prelude", issue = "58935")]
pub use crate::vec::Vec;
}
}
pub mod raw_vec {
#![unstable(feature = "raw_vec_internals", reason = "implementation detail", issue = "none")]
#![doc(hidden)]

use core::alloc::LayoutError;
use core::cmp;
use core::intrinsics;
use core::mem::{self, ManuallyDrop, MaybeUninit};
use core::ops::Drop;
use core::ptr::{self, NonNull, Unique};
use core::slice;

#[cfg(not(no_global_oom_handling))]
use crate::alloc::handle_alloc_error;
use crate::alloc::{Allocator, Global, Layout};
use crate::boxed::Box;
use crate::collections::TryReserveError;
use crate::collections::TryReserveErrorKind::*;

#[cfg(test)]
mod tests {
}

#[cfg(not(no_global_oom_handling))]
enum AllocInit {
    /// The contents of the new memory are uninitialized.
    Uninitialized,
    /// The new memory is guaranteed to be zeroed.
    Zeroed,
}

/// A low-level utility for more ergonomically allocating, reallocating, and deallocating
/// a buffer of memory on the heap without having to worry about all the corner cases
/// involved. This type is excellent for building your own data structures like Vec and VecDeque.
/// In particular:
///
/// * Produces `Unique::dangling()` on zero-sized types.
/// * Produces `Unique::dangling()` on zero-length allocations.
/// * Avoids freeing `Unique::dangling()`.
/// * Catches all overflows in capacity computations (promotes them to "capacity overflow" panics).
/// * Guards against 32-bit systems allocating more than isize::MAX bytes.
/// * Guards against overflowing your length.
/// * Calls `handle_alloc_error` for fallible allocations.
/// * Contains a `ptr::Unique` and thus endows the user with all related benefits.
/// * Uses the excess returned from the allocator to use the largest available capacity.
///
/// This type does not in anyway inspect the memory that it manages. When dropped it *will*
/// free its memory, but it *won't* try to drop its contents. It is up to the user of `RawVec`
/// to handle the actual things *stored* inside of a `RawVec`.
///
/// Note that the excess of a zero-sized types is always infinite, so `capacity()` always returns
/// `usize::MAX`. This means that you need to be careful when round-tripping this type with a
/// `Box<[T]>`, since `capacity()` won't yield the length.
#[allow(missing_debug_implementations)]
pub struct RawVec<T, A: Allocator = Global> {
    ptr: Unique<T>,
    cap: usize,
    alloc: A,
}

impl<T> RawVec<T, Global> {
    /// HACK(Centril): This exists because stable `const fn` can only call stable `const fn`, so
    /// they cannot call `Self::new()`.
    ///
    /// If you change `RawVec<T>::new` or dependencies, please take care to not introduce anything
    /// that would truly const-call something unstable.
    pub const NEW: Self = Self::new();

    /// Creates the biggest possible `RawVec` (on the system heap)
    /// without allocating. If `T` has positive size, then this makes a
    /// `RawVec` with capacity `0`. If `T` is zero-sized, then it makes a
    /// `RawVec` with capacity `usize::MAX`. Useful for implementing
    /// delayed allocation.
    pub const fn new() -> Self {
}

    /// Creates a `RawVec` (on the system heap) with exactly the
    /// capacity and alignment requirements for a `[T; capacity]`. This is
    /// equivalent to calling `RawVec::new` when `capacity` is `0` or `T` is
    /// zero-sized. Note that if `T` is zero-sized this means you will
    /// *not* get a `RawVec` with the requested capacity.
    ///
    /// # Panics
    ///
    /// Panics if the requested capacity exceeds `isize::MAX` bytes.
    ///
    /// # Aborts
    ///
    /// Aborts on OOM.
    #[cfg(not(no_global_oom_handling))]
    #[inline]
    pub fn with_capacity(capacity: usize) -> Self {
}

    /// Like `with_capacity`, but guarantees the buffer is zeroed.
    #[cfg(not(no_global_oom_handling))]
    #[inline]
    pub fn with_capacity_zeroed(capacity: usize) -> Self {
}

    /// Reconstitutes a `RawVec` from a pointer and capacity.
    ///
    /// # Safety
    ///
    /// The `ptr` must be allocated (on the system heap), and with the given `capacity`.
    /// The `capacity` cannot exceed `isize::MAX` for sized types. (only a concern on 32-bit
    /// systems). ZST vectors may have a capacity up to `usize::MAX`.
    /// If the `ptr` and `capacity` come from a `RawVec`, then this is guaranteed.
    #[inline]
    pub unsafe fn from_raw_parts(ptr: *mut T, capacity: usize) -> Self {
}
}

impl<T, A: Allocator> RawVec<T, A> {
    // Tiny Vecs are dumb. Skip to:
    // - 8 if the element size is 1, because any heap allocators is likely
    //   to round up a request of less than 8 bytes to at least 8 bytes.
    // - 4 if elements are moderate-sized (<= 1 KiB).
    // - 1 otherwise, to avoid wasting too much space for very short Vecs.
    const MIN_NON_ZERO_CAP: usize = if mem::size_of::<T>() == 1 {
        8
    } else if mem::size_of::<T>() <= 1024 {
        4
    } else {
        1
    };

    /// Like `new`, but parameterized over the choice of allocator for
    /// the returned `RawVec`.
    #[rustc_allow_const_fn_unstable(const_fn)]
    pub const fn new_in(alloc: A) -> Self {
}

    /// Like `with_capacity`, but parameterized over the choice of
    /// allocator for the returned `RawVec`.
    #[cfg(not(no_global_oom_handling))]
    #[inline]
    pub fn with_capacity_in(capacity: usize, alloc: A) -> Self {
}

    /// Like `with_capacity_zeroed`, but parameterized over the choice
    /// of allocator for the returned `RawVec`.
    #[cfg(not(no_global_oom_handling))]
    #[inline]
    pub fn with_capacity_zeroed_in(capacity: usize, alloc: A) -> Self {
}

    /// Converts a `Box<[T]>` into a `RawVec<T>`.
    pub fn from_box(slice: Box<[T], A>) -> Self {
}

    /// Converts the entire buffer into `Box<[MaybeUninit<T>]>` with the specified `len`.
    ///
    /// Note that this will correctly reconstitute any `cap` changes
    /// that may have been performed. (See description of type for details.)
    ///
    /// # Safety
    ///
    /// * `len` must be greater than or equal to the most recently requested capacity, and
    /// * `len` must be less than or equal to `self.capacity()`.
    ///
    /// Note, that the requested capacity and `self.capacity()` could differ, as
    /// an allocator could overallocate and return a greater memory block than requested.
    pub unsafe fn into_box(self, len: usize) -> Box<[MaybeUninit<T>], A> {
}

    #[cfg(not(no_global_oom_handling))]
    fn allocate_in(capacity: usize, init: AllocInit, alloc: A) -> Self {
}

    /// Reconstitutes a `RawVec` from a pointer, capacity, and allocator.
    ///
    /// # Safety
    ///
    /// The `ptr` must be allocated (via the given allocator `alloc`), and with the given
    /// `capacity`.
    /// The `capacity` cannot exceed `isize::MAX` for sized types. (only a concern on 32-bit
    /// systems). ZST vectors may have a capacity up to `usize::MAX`.
    /// If the `ptr` and `capacity` come from a `RawVec` created via `alloc`, then this is
    /// guaranteed.
    #[inline]
    pub unsafe fn from_raw_parts_in(ptr: *mut T, capacity: usize, alloc: A) -> Self {
}

    /// Gets a raw pointer to the start of the allocation. Note that this is
    /// `Unique::dangling()` if `capacity == 0` or `T` is zero-sized. In the former case, you must
    /// be careful.
    #[inline]
    pub fn ptr(&self) -> *mut T {
}

    /// Gets the capacity of the allocation.
    ///
    /// This will always be `usize::MAX` if `T` is zero-sized.
    #[inline(always)]
    pub fn capacity(&self) -> usize {
}

    /// Returns a shared reference to the allocator backing this `RawVec`.
    pub fn allocator(&self) -> &A {
}

    fn current_memory(&self) -> Option<(NonNull<u8>, Layout)> {
}

    /// Ensures that the buffer contains at least enough space to hold `len +
    /// additional` elements. If it doesn't already have enough capacity, will
    /// reallocate enough space plus comfortable slack space to get amortized
    /// *O*(1) behavior. Will limit this behavior if it would needlessly cause
    /// itself to panic.
    ///
    /// If `len` exceeds `self.capacity()`, this may fail to actually allocate
    /// the requested space. This is not really unsafe, but the unsafe
    /// code *you* write that relies on the behavior of this function may break.
    ///
    /// This is ideal for implementing a bulk-push operation like `extend`.
    ///
    /// # Panics
    ///
    /// Panics if the new capacity exceeds `isize::MAX` bytes.
    ///
    /// # Aborts
    ///
    /// Aborts on OOM.
    ///
    /// # Examples
    ///
    /// ```
    /// # #![feature(raw_vec_internals)]
    /// # extern crate alloc;
    /// # use std::ptr;
    /// # use alloc::raw_vec::RawVec;
    /// struct MyVec<T> {
    ///     buf: RawVec<T>,
    ///     len: usize,
    /// }
    ///
    /// impl<T: Clone> MyVec<T> {
    ///     pub fn push_all(&mut self, elems: &[T]) {
    ///         self.buf.reserve(self.len, elems.len());
    ///         // reserve would have aborted or panicked if the len exceeded
    ///         // `isize::MAX` so this is safe to do unchecked now.
    ///         for x in elems {
    ///             unsafe {
    ///                 ptr::write(self.buf.ptr().add(self.len), x.clone());
    ///             }
    ///             self.len += 1;
    ///         }
    ///     }
    /// }
    /// # fn main() {
    /// #   let mut vector = MyVec { buf: RawVec::new(), len: 0 };
    /// #   vector.push_all(&[1, 3, 5, 7, 9]);
    /// # }
    /// ```
    #[cfg(not(no_global_oom_handling))]
    #[inline]
    pub fn reserve(&mut self, len: usize, additional: usize) {
}

    /// The same as `reserve`, but returns on errors instead of panicking or aborting.
    pub fn try_reserve(&mut self, len: usize, additional: usize) -> Result<(), TryReserveError> {
}

    /// Ensures that the buffer contains at least enough space to hold `len +
    /// additional` elements. If it doesn't already, will reallocate the
    /// minimum possible amount of memory necessary. Generally this will be
    /// exactly the amount of memory necessary, but in principle the allocator
    /// is free to give back more than we asked for.
    ///
    /// If `len` exceeds `self.capacity()`, this may fail to actually allocate
    /// the requested space. This is not really unsafe, but the unsafe code
    /// *you* write that relies on the behavior of this function may break.
    ///
    /// # Panics
    ///
    /// Panics if the new capacity exceeds `isize::MAX` bytes.
    ///
    /// # Aborts
    ///
    /// Aborts on OOM.
    #[cfg(not(no_global_oom_handling))]
    pub fn reserve_exact(&mut self, len: usize, additional: usize) {
}

    /// The same as `reserve_exact`, but returns on errors instead of panicking or aborting.
    pub fn try_reserve_exact(
        &mut self,
        len: usize,
        additional: usize,
    ) -> Result<(), TryReserveError> {
}

    /// Shrinks the allocation down to the specified amount. If the given amount
    /// is 0, actually completely deallocates.
    ///
    /// # Panics
    ///
    /// Panics if the given amount is *larger* than the current capacity.
    ///
    /// # Aborts
    ///
    /// Aborts on OOM.
    #[cfg(not(no_global_oom_handling))]
    pub fn shrink_to_fit(&mut self, amount: usize) {
}
}

impl<T, A: Allocator> RawVec<T, A> {
    /// Returns if the buffer needs to grow to fulfill the needed extra capacity.
    /// Mainly used to make inlining reserve-calls possible without inlining `grow`.
    fn needs_to_grow(&self, len: usize, additional: usize) -> bool {
}

    fn capacity_from_bytes(excess: usize) -> usize {
}

    fn set_ptr(&mut self, ptr: NonNull<[u8]>) {
}

    // This method is usually instantiated many times. So we want it to be as
    // small as possible, to improve compile times. But we also want as much of
    // its contents to be statically computable as possible, to make the
    // generated code run faster. Therefore, this method is carefully written
    // so that all of the code that depends on `T` is within it, while as much
    // of the code that doesn't depend on `T` as possible is in functions that
    // are non-generic over `T`.
    fn grow_amortized(&mut self, len: usize, additional: usize) -> Result<(), TryReserveError> {
}

    // The constraints on this method are much the same as those on
    // `grow_amortized`, but this method is usually instantiated less often so
    // it's less critical.
    fn grow_exact(&mut self, len: usize, additional: usize) -> Result<(), TryReserveError> {
}

    fn shrink(&mut self, amount: usize) -> Result<(), TryReserveError> {
}
}

// This function is outside `RawVec` to minimize compile times. See the comment
// above `RawVec::grow_amortized` for details. (The `A` parameter isn't
// significant, because the number of different `A` types seen in practice is
// much smaller than the number of `T` types.)
#[inline(never)]
fn finish_grow<A>(
    new_layout: Result<Layout, LayoutError>,
    current_memory: Option<(NonNull<u8>, Layout)>,
    alloc: &mut A,
) -> Result<NonNull<[u8]>, TryReserveError>
where
    A: Allocator,
{
}

unsafe impl<#[may_dangle] T, A: Allocator> Drop for RawVec<T, A> {
    /// Frees the memory owned by the `RawVec` *without* trying to drop its contents.
    fn drop(&mut self) {
}
}

// Central function for reserve error handling.
#[cfg(not(no_global_oom_handling))]
#[inline]
fn handle_reserve(result: Result<(), TryReserveError>) {
}

// We need to guarantee the following:
// * We don't ever allocate `> isize::MAX` byte-size objects.
// * We don't overflow `usize::MAX` and actually allocate too little.
//
// On 64-bit we just need to check for overflow since trying to allocate
// `> isize::MAX` bytes will surely fail. On 32-bit and 16-bit we need to add
// an extra guard for this in case we're running on a platform which can use
// all 4GB in user-space, e.g., PAE or x32.

#[inline]
fn alloc_guard(alloc_size: usize) -> Result<(), TryReserveError> {
}

// One central function responsible for reporting capacity overflows. This'll
// ensure that the code generation related to these panics is minimal as there's
// only one location which panics rather than a bunch throughout the module.
#[cfg(not(no_global_oom_handling))]
fn capacity_overflow() -> ! {
}
}
pub mod rc {
//! Single-threaded reference-counting pointers. 'Rc' stands for 'Reference
//! Counted'.
//!
//! The type [`Rc<T>`][`Rc`] provides shared ownership of a value of type `T`,
//! allocated in the heap. Invoking [`clone`][clone] on [`Rc`] produces a new
//! pointer to the same allocation in the heap. When the last [`Rc`] pointer to a
//! given allocation is destroyed, the value stored in that allocation (often
//! referred to as "inner value") is also dropped.
//!
//! Shared references in Rust disallow mutation by default, and [`Rc`]
//! is no exception: you cannot generally obtain a mutable reference to
//! something inside an [`Rc`]. If you need mutability, put a [`Cell`]
//! or [`RefCell`] inside the [`Rc`]; see [an example of mutability
//! inside an `Rc`][mutability].
//!
//! [`Rc`] uses non-atomic reference counting. This means that overhead is very
//! low, but an [`Rc`] cannot be sent between threads, and consequently [`Rc`]
//! does not implement [`Send`][send]. As a result, the Rust compiler
//! will check *at compile time* that you are not sending [`Rc`]s between
//! threads. If you need multi-threaded, atomic reference counting, use
//! [`sync::Arc`][arc].
//!
//! The [`downgrade`][downgrade] method can be used to create a non-owning
//! [`Weak`] pointer. A [`Weak`] pointer can be [`upgrade`][upgrade]d
//! to an [`Rc`], but this will return [`None`] if the value stored in the allocation has
//! already been dropped. In other words, `Weak` pointers do not keep the value
//! inside the allocation alive; however, they *do* keep the allocation
//! (the backing store for the inner value) alive.
//!
//! A cycle between [`Rc`] pointers will never be deallocated. For this reason,
//! [`Weak`] is used to break cycles. For example, a tree could have strong
//! [`Rc`] pointers from parent nodes to children, and [`Weak`] pointers from
//! children back to their parents.
//!
//! `Rc<T>` automatically dereferences to `T` (via the [`Deref`] trait),
//! so you can call `T`'s methods on a value of type [`Rc<T>`][`Rc`]. To avoid name
//! clashes with `T`'s methods, the methods of [`Rc<T>`][`Rc`] itself are associated
//! functions, called using [fully qualified syntax]:
//!
//! ```
//! use std::rc::Rc;
//!
//! let my_rc = Rc::new(());
//! Rc::downgrade(&my_rc);
//! ```
//!
//! `Rc<T>`'s implementations of traits like `Clone` may also be called using
//! fully qualified syntax. Some people prefer to use fully qualified syntax,
//! while others prefer using method-call syntax.
//!
//! ```
//! use std::rc::Rc;
//!
//! let rc = Rc::new(());
//! // Method-call syntax
//! let rc2 = rc.clone();
//! // Fully qualified syntax
//! let rc3 = Rc::clone(&rc);
//! ```
//!
//! [`Weak<T>`][`Weak`] does not auto-dereference to `T`, because the inner value may have
//! already been dropped.
//!
//! # Cloning references
//!
//! Creating a new reference to the same allocation as an existing reference counted pointer
//! is done using the `Clone` trait implemented for [`Rc<T>`][`Rc`] and [`Weak<T>`][`Weak`].
//!
//! ```
//! use std::rc::Rc;
//!
//! let foo = Rc::new(vec![1.0, 2.0, 3.0]);
//! // The two syntaxes below are equivalent.
//! let a = foo.clone();
//! let b = Rc::clone(&foo);
//! // a and b both point to the same memory location as foo.
//! ```
//!
//! The `Rc::clone(&from)` syntax is the most idiomatic because it conveys more explicitly
//! the meaning of the code. In the example above, this syntax makes it easier to see that
//! this code is creating a new reference rather than copying the whole content of foo.
//!
//! # Examples
//!
//! Consider a scenario where a set of `Gadget`s are owned by a given `Owner`.
//! We want to have our `Gadget`s point to their `Owner`. We can't do this with
//! unique ownership, because more than one gadget may belong to the same
//! `Owner`. [`Rc`] allows us to share an `Owner` between multiple `Gadget`s,
//! and have the `Owner` remain allocated as long as any `Gadget` points at it.
//!
//! ```
//! use std::rc::Rc;
//!
//! struct Owner {
//!     name: String,
//!     // ...other fields
//! }
//!
//! struct Gadget {
//!     id: i32,
//!     owner: Rc<Owner>,
//!     // ...other fields
//! }
//!
//! fn main() {
//!     // Create a reference-counted `Owner`.
//!     let gadget_owner: Rc<Owner> = Rc::new(
//!         Owner {
//!             name: "Gadget Man".to_string(),
//!         }
//!     );
//!
//!     // Create `Gadget`s belonging to `gadget_owner`. Cloning the `Rc<Owner>`
//!     // gives us a new pointer to the same `Owner` allocation, incrementing
//!     // the reference count in the process.
//!     let gadget1 = Gadget {
//!         id: 1,
//!         owner: Rc::clone(&gadget_owner),
//!     };
//!     let gadget2 = Gadget {
//!         id: 2,
//!         owner: Rc::clone(&gadget_owner),
//!     };
//!
//!     // Dispose of our local variable `gadget_owner`.
//!     drop(gadget_owner);
//!
//!     // Despite dropping `gadget_owner`, we're still able to print out the name
//!     // of the `Owner` of the `Gadget`s. This is because we've only dropped a
//!     // single `Rc<Owner>`, not the `Owner` it points to. As long as there are
//!     // other `Rc<Owner>` pointing at the same `Owner` allocation, it will remain
//!     // live. The field projection `gadget1.owner.name` works because
//!     // `Rc<Owner>` automatically dereferences to `Owner`.
//!     println!("Gadget {} owned by {}", gadget1.id, gadget1.owner.name);
//!     println!("Gadget {} owned by {}", gadget2.id, gadget2.owner.name);
//!
//!     // At the end of the function, `gadget1` and `gadget2` are destroyed, and
//!     // with them the last counted references to our `Owner`. Gadget Man now
//!     // gets destroyed as well.
//! }
//! ```
//!
//! If our requirements change, and we also need to be able to traverse from
//! `Owner` to `Gadget`, we will run into problems. An [`Rc`] pointer from `Owner`
//! to `Gadget` introduces a cycle. This means that their
//! reference counts can never reach 0, and the allocation will never be destroyed:
//! a memory leak. In order to get around this, we can use [`Weak`]
//! pointers.
//!
//! Rust actually makes it somewhat difficult to produce this loop in the first
//! place. In order to end up with two values that point at each other, one of
//! them needs to be mutable. This is difficult because [`Rc`] enforces
//! memory safety by only giving out shared references to the value it wraps,
//! and these don't allow direct mutation. We need to wrap the part of the
//! value we wish to mutate in a [`RefCell`], which provides *interior
//! mutability*: a method to achieve mutability through a shared reference.
//! [`RefCell`] enforces Rust's borrowing rules at runtime.
//!
//! ```
//! use std::rc::Rc;
//! use std::rc::Weak;
//! use std::cell::RefCell;
//!
//! struct Owner {
//!     name: String,
//!     gadgets: RefCell<Vec<Weak<Gadget>>>,
//!     // ...other fields
//! }
//!
//! struct Gadget {
//!     id: i32,
//!     owner: Rc<Owner>,
//!     // ...other fields
//! }
//!
//! fn main() {
//!     // Create a reference-counted `Owner`. Note that we've put the `Owner`'s
//!     // vector of `Gadget`s inside a `RefCell` so that we can mutate it through
//!     // a shared reference.
//!     let gadget_owner: Rc<Owner> = Rc::new(
//!         Owner {
//!             name: "Gadget Man".to_string(),
//!             gadgets: RefCell::new(vec![]),
//!         }
//!     );
//!
//!     // Create `Gadget`s belonging to `gadget_owner`, as before.
//!     let gadget1 = Rc::new(
//!         Gadget {
//!             id: 1,
//!             owner: Rc::clone(&gadget_owner),
//!         }
//!     );
//!     let gadget2 = Rc::new(
//!         Gadget {
//!             id: 2,
//!             owner: Rc::clone(&gadget_owner),
//!         }
//!     );
//!
//!     // Add the `Gadget`s to their `Owner`.
//!     {
//!         let mut gadgets = gadget_owner.gadgets.borrow_mut();
//!         gadgets.push(Rc::downgrade(&gadget1));
//!         gadgets.push(Rc::downgrade(&gadget2));
//!
//!         // `RefCell` dynamic borrow ends here.
//!     }
//!
//!     // Iterate over our `Gadget`s, printing their details out.
//!     for gadget_weak in gadget_owner.gadgets.borrow().iter() {
//!
//!         // `gadget_weak` is a `Weak<Gadget>`. Since `Weak` pointers can't
//!         // guarantee the allocation still exists, we need to call
//!         // `upgrade`, which returns an `Option<Rc<Gadget>>`.
//!         //
//!         // In this case we know the allocation still exists, so we simply
//!         // `unwrap` the `Option`. In a more complicated program, you might
//!         // need graceful error handling for a `None` result.
//!
//!         let gadget = gadget_weak.upgrade().unwrap();
//!         println!("Gadget {} owned by {}", gadget.id, gadget.owner.name);
//!     }
//!
//!     // At the end of the function, `gadget_owner`, `gadget1`, and `gadget2`
//!     // are destroyed. There are now no strong (`Rc`) pointers to the
//!     // gadgets, so they are destroyed. This zeroes the reference count on
//!     // Gadget Man, so he gets destroyed as well.
//! }
//! ```
//!
//! [clone]: Clone::clone
//! [`Cell`]: core::cell::Cell
//! [`RefCell`]: core::cell::RefCell
//! [send]: core::marker::Send
//! [arc]: crate::sync::Arc
//! [`Deref`]: core::ops::Deref
//! [downgrade]: Rc::downgrade
//! [upgrade]: Weak::upgrade
//! [mutability]: core::cell#introducing-mutability-inside-of-something-immutable
//! [fully qualified syntax]: https://doc.rust-lang.org/book/ch19-03-advanced-traits.html#fully-qualified-syntax-for-disambiguation-calling-methods-with-the-same-name

#![stable(feature = "rust1", since = "1.0.0")]

#[cfg(not(test))]
use crate::boxed::Box;
#[cfg(test)]
use std::boxed::Box;

use core::any::Any;
use core::borrow;
use core::cell::Cell;
use core::cmp::Ordering;
use core::convert::{From, TryFrom};
use core::fmt;
use core::hash::{Hash, Hasher};
use core::intrinsics::abort;
#[cfg(not(no_global_oom_handling))]
use core::iter;
use core::marker::{self, PhantomData, Unpin, Unsize};
#[cfg(not(no_global_oom_handling))]
use core::mem::size_of_val;
use core::mem::{self, align_of_val_raw, forget};
use core::ops::{CoerceUnsized, Deref, DispatchFromDyn, Receiver};
use core::panic::{RefUnwindSafe, UnwindSafe};
#[cfg(not(no_global_oom_handling))]
use core::pin::Pin;
use core::ptr::{self, NonNull};
#[cfg(not(no_global_oom_handling))]
use core::slice::from_raw_parts_mut;

#[cfg(not(no_global_oom_handling))]
use crate::alloc::handle_alloc_error;
#[cfg(not(no_global_oom_handling))]
use crate::alloc::{box_free, WriteCloneIntoRaw};
use crate::alloc::{AllocError, Allocator, Global, Layout};
use crate::borrow::{Cow, ToOwned};
#[cfg(not(no_global_oom_handling))]
use crate::string::String;
#[cfg(not(no_global_oom_handling))]
use crate::vec::Vec;

#[cfg(test)]
mod tests {
}

// This is repr(C) to future-proof against possible field-reordering, which
// would interfere with otherwise safe [into|from]_raw() of transmutable
// inner types.
#[repr(C)]
struct RcBox<T: ?Sized> {
    strong: Cell<usize>,
    weak: Cell<usize>,
    value: T,
}

/// A single-threaded reference-counting pointer. 'Rc' stands for 'Reference
/// Counted'.
///
/// See the [module-level documentation](./index.html) for more details.
///
/// The inherent methods of `Rc` are all associated functions, which means
/// that you have to call them as e.g., [`Rc::get_mut(&mut value)`][get_mut] instead of
/// `value.get_mut()`. This avoids conflicts with methods of the inner type `T`.
///
/// [get_mut]: Rc::get_mut
#[cfg_attr(not(test), rustc_diagnostic_item = "Rc")]
#[stable(feature = "rust1", since = "1.0.0")]
#[rustc_insignificant_dtor]
pub struct Rc<T: ?Sized> {
    ptr: NonNull<RcBox<T>>,
    phantom: PhantomData<RcBox<T>>,
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<T: ?Sized> !marker::Send for Rc<T> {}
#[stable(feature = "rust1", since = "1.0.0")]
impl<T: ?Sized> !marker::Sync for Rc<T> {}

#[stable(feature = "catch_unwind", since = "1.9.0")]
impl<T: RefUnwindSafe + ?Sized> UnwindSafe for Rc<T> {}

#[unstable(feature = "coerce_unsized", issue = "27732")]
impl<T: ?Sized + Unsize<U>, U: ?Sized> CoerceUnsized<Rc<U>> for Rc<T> {}

#[unstable(feature = "dispatch_from_dyn", issue = "none")]
impl<T: ?Sized + Unsize<U>, U: ?Sized> DispatchFromDyn<Rc<U>> for Rc<T> {}

impl<T: ?Sized> Rc<T> {
    #[inline(always)]
    fn inner(&self) -> &RcBox<T> {
}

    fn from_inner(ptr: NonNull<RcBox<T>>) -> Self {
}

    unsafe fn from_ptr(ptr: *mut RcBox<T>) -> Self {
}
}

impl<T> Rc<T> {
    /// Constructs a new `Rc<T>`.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::rc::Rc;
    ///
    /// let five = Rc::new(5);
    /// ```
    #[cfg(not(no_global_oom_handling))]
    #[stable(feature = "rust1", since = "1.0.0")]
    pub fn new(value: T) -> Rc<T> {
}

    /// Constructs a new `Rc<T>` using a weak reference to itself. Attempting
    /// to upgrade the weak reference before this function returns will result
    /// in a `None` value. However, the weak reference may be cloned freely and
    /// stored for use at a later time.
    ///
    /// # Examples
    ///
    /// ```
    /// #![feature(arc_new_cyclic)]
    /// #![allow(dead_code)]
    /// use std::rc::{Rc, Weak};
    ///
    /// struct Gadget {
    ///     self_weak: Weak<Self>,
    ///     // ... more fields
    /// }
    /// impl Gadget {
    ///     pub fn new() -> Rc<Self> {
    ///         Rc::new_cyclic(|self_weak| {
    ///             Gadget { self_weak: self_weak.clone(), /* ... */ }
    ///         })
    ///     }
    /// }
    /// ```
    #[cfg(not(no_global_oom_handling))]
    #[unstable(feature = "arc_new_cyclic", issue = "75861")]
    pub fn new_cyclic(data_fn: impl FnOnce(&Weak<T>) -> T) -> Rc<T> {
}

    /// Constructs a new `Rc` with uninitialized contents.
    ///
    /// # Examples
    ///
    /// ```
    /// #![feature(new_uninit)]
    /// #![feature(get_mut_unchecked)]
    ///
    /// use std::rc::Rc;
    ///
    /// let mut five = Rc::<u32>::new_uninit();
    ///
    /// let five = unsafe {
    ///     // Deferred initialization:
    ///     Rc::get_mut_unchecked(&mut five).as_mut_ptr().write(5);
    ///
    ///     five.assume_init()
    /// };
    ///
    /// assert_eq!(*five, 5)
    /// ```
    #[cfg(not(no_global_oom_handling))]
    #[unstable(feature = "new_uninit", issue = "63291")]
    pub fn new_uninit() -> Rc<mem::MaybeUninit<T>> {
}

    /// Constructs a new `Rc` with uninitialized contents, with the memory
    /// being filled with `0` bytes.
    ///
    /// See [`MaybeUninit::zeroed`][zeroed] for examples of correct and
    /// incorrect usage of this method.
    ///
    /// # Examples
    ///
    /// ```
    /// #![feature(new_uninit)]
    ///
    /// use std::rc::Rc;
    ///
    /// let zero = Rc::<u32>::new_zeroed();
    /// let zero = unsafe { zero.assume_init() };
    ///
    /// assert_eq!(*zero, 0)
    /// ```
    ///
    /// [zeroed]: mem::MaybeUninit::zeroed
    #[cfg(not(no_global_oom_handling))]
    #[unstable(feature = "new_uninit", issue = "63291")]
    pub fn new_zeroed() -> Rc<mem::MaybeUninit<T>> {
}

    /// Constructs a new `Rc<T>`, returning an error if the allocation fails
    ///
    /// # Examples
    ///
    /// ```
    /// #![feature(allocator_api)]
    /// use std::rc::Rc;
    ///
    /// let five = Rc::try_new(5);
    /// # Ok::<(), std::alloc::AllocError>(())
    /// ```
    #[unstable(feature = "allocator_api", issue = "32838")]
    pub fn try_new(value: T) -> Result<Rc<T>, AllocError> {
}

    /// Constructs a new `Rc` with uninitialized contents, returning an error if the allocation fails
    ///
    /// # Examples
    ///
    /// ```
    /// #![feature(allocator_api, new_uninit)]
    /// #![feature(get_mut_unchecked)]
    ///
    /// use std::rc::Rc;
    ///
    /// let mut five = Rc::<u32>::try_new_uninit()?;
    ///
    /// let five = unsafe {
    ///     // Deferred initialization:
    ///     Rc::get_mut_unchecked(&mut five).as_mut_ptr().write(5);
    ///
    ///     five.assume_init()
    /// };
    ///
    /// assert_eq!(*five, 5);
    /// # Ok::<(), std::alloc::AllocError>(())
    /// ```
    #[unstable(feature = "allocator_api", issue = "32838")]
    // #[unstable(feature = "new_uninit", issue = "63291")]
    pub fn try_new_uninit() -> Result<Rc<mem::MaybeUninit<T>>, AllocError> {
}

    /// Constructs a new `Rc` with uninitialized contents, with the memory
    /// being filled with `0` bytes, returning an error if the allocation fails
    ///
    /// See [`MaybeUninit::zeroed`][zeroed] for examples of correct and
    /// incorrect usage of this method.
    ///
    /// # Examples
    ///
    /// ```
    /// #![feature(allocator_api, new_uninit)]
    ///
    /// use std::rc::Rc;
    ///
    /// let zero = Rc::<u32>::try_new_zeroed()?;
    /// let zero = unsafe { zero.assume_init() };
    ///
    /// assert_eq!(*zero, 0);
    /// # Ok::<(), std::alloc::AllocError>(())
    /// ```
    ///
    /// [zeroed]: mem::MaybeUninit::zeroed
    #[unstable(feature = "allocator_api", issue = "32838")]
    //#[unstable(feature = "new_uninit", issue = "63291")]
    pub fn try_new_zeroed() -> Result<Rc<mem::MaybeUninit<T>>, AllocError> {
}
    /// Constructs a new `Pin<Rc<T>>`. If `T` does not implement `Unpin`, then
    /// `value` will be pinned in memory and unable to be moved.
    #[cfg(not(no_global_oom_handling))]
    #[stable(feature = "pin", since = "1.33.0")]
    pub fn pin(value: T) -> Pin<Rc<T>> {
}

    /// Returns the inner value, if the `Rc` has exactly one strong reference.
    ///
    /// Otherwise, an [`Err`] is returned with the same `Rc` that was
    /// passed in.
    ///
    /// This will succeed even if there are outstanding weak references.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::rc::Rc;
    ///
    /// let x = Rc::new(3);
    /// assert_eq!(Rc::try_unwrap(x), Ok(3));
    ///
    /// let x = Rc::new(4);
    /// let _y = Rc::clone(&x);
    /// assert_eq!(*Rc::try_unwrap(x).unwrap_err(), 4);
    /// ```
    #[inline]
    #[stable(feature = "rc_unique", since = "1.4.0")]
    pub fn try_unwrap(this: Self) -> Result<T, Self> {
}
}

impl<T> Rc<[T]> {
    /// Constructs a new reference-counted slice with uninitialized contents.
    ///
    /// # Examples
    ///
    /// ```
    /// #![feature(new_uninit)]
    /// #![feature(get_mut_unchecked)]
    ///
    /// use std::rc::Rc;
    ///
    /// let mut values = Rc::<[u32]>::new_uninit_slice(3);
    ///
    /// let values = unsafe {
    ///     // Deferred initialization:
    ///     Rc::get_mut_unchecked(&mut values)[0].as_mut_ptr().write(1);
    ///     Rc::get_mut_unchecked(&mut values)[1].as_mut_ptr().write(2);
    ///     Rc::get_mut_unchecked(&mut values)[2].as_mut_ptr().write(3);
    ///
    ///     values.assume_init()
    /// };
    ///
    /// assert_eq!(*values, [1, 2, 3])
    /// ```
    #[cfg(not(no_global_oom_handling))]
    #[unstable(feature = "new_uninit", issue = "63291")]
    pub fn new_uninit_slice(len: usize) -> Rc<[mem::MaybeUninit<T>]> {
}

    /// Constructs a new reference-counted slice with uninitialized contents, with the memory being
    /// filled with `0` bytes.
    ///
    /// See [`MaybeUninit::zeroed`][zeroed] for examples of correct and
    /// incorrect usage of this method.
    ///
    /// # Examples
    ///
    /// ```
    /// #![feature(new_uninit)]
    ///
    /// use std::rc::Rc;
    ///
    /// let values = Rc::<[u32]>::new_zeroed_slice(3);
    /// let values = unsafe { values.assume_init() };
    ///
    /// assert_eq!(*values, [0, 0, 0])
    /// ```
    ///
    /// [zeroed]: mem::MaybeUninit::zeroed
    #[cfg(not(no_global_oom_handling))]
    #[unstable(feature = "new_uninit", issue = "63291")]
    pub fn new_zeroed_slice(len: usize) -> Rc<[mem::MaybeUninit<T>]> {
}
}

impl<T> Rc<mem::MaybeUninit<T>> {
    /// Converts to `Rc<T>`.
    ///
    /// # Safety
    ///
    /// As with [`MaybeUninit::assume_init`],
    /// it is up to the caller to guarantee that the inner value
    /// really is in an initialized state.
    /// Calling this when the content is not yet fully initialized
    /// causes immediate undefined behavior.
    ///
    /// [`MaybeUninit::assume_init`]: mem::MaybeUninit::assume_init
    ///
    /// # Examples
    ///
    /// ```
    /// #![feature(new_uninit)]
    /// #![feature(get_mut_unchecked)]
    ///
    /// use std::rc::Rc;
    ///
    /// let mut five = Rc::<u32>::new_uninit();
    ///
    /// let five = unsafe {
    ///     // Deferred initialization:
    ///     Rc::get_mut_unchecked(&mut five).as_mut_ptr().write(5);
    ///
    ///     five.assume_init()
    /// };
    ///
    /// assert_eq!(*five, 5)
    /// ```
    #[unstable(feature = "new_uninit", issue = "63291")]
    #[inline]
    pub unsafe fn assume_init(self) -> Rc<T> {
}
}

impl<T> Rc<[mem::MaybeUninit<T>]> {
    /// Converts to `Rc<[T]>`.
    ///
    /// # Safety
    ///
    /// As with [`MaybeUninit::assume_init`],
    /// it is up to the caller to guarantee that the inner value
    /// really is in an initialized state.
    /// Calling this when the content is not yet fully initialized
    /// causes immediate undefined behavior.
    ///
    /// [`MaybeUninit::assume_init`]: mem::MaybeUninit::assume_init
    ///
    /// # Examples
    ///
    /// ```
    /// #![feature(new_uninit)]
    /// #![feature(get_mut_unchecked)]
    ///
    /// use std::rc::Rc;
    ///
    /// let mut values = Rc::<[u32]>::new_uninit_slice(3);
    ///
    /// let values = unsafe {
    ///     // Deferred initialization:
    ///     Rc::get_mut_unchecked(&mut values)[0].as_mut_ptr().write(1);
    ///     Rc::get_mut_unchecked(&mut values)[1].as_mut_ptr().write(2);
    ///     Rc::get_mut_unchecked(&mut values)[2].as_mut_ptr().write(3);
    ///
    ///     values.assume_init()
    /// };
    ///
    /// assert_eq!(*values, [1, 2, 3])
    /// ```
    #[unstable(feature = "new_uninit", issue = "63291")]
    #[inline]
    pub unsafe fn assume_init(self) -> Rc<[T]> {
}
}

impl<T: ?Sized> Rc<T> {
    /// Consumes the `Rc`, returning the wrapped pointer.
    ///
    /// To avoid a memory leak the pointer must be converted back to an `Rc` using
    /// [`Rc::from_raw`][from_raw].
    ///
    /// [from_raw]: Rc::from_raw
    ///
    /// # Examples
    ///
    /// ```
    /// use std::rc::Rc;
    ///
    /// let x = Rc::new("hello".to_owned());
    /// let x_ptr = Rc::into_raw(x);
    /// assert_eq!(unsafe { &*x_ptr }, "hello");
    /// ```
    #[stable(feature = "rc_raw", since = "1.17.0")]
    pub fn into_raw(this: Self) -> *const T {
}

    /// Provides a raw pointer to the data.
    ///
    /// The counts are not affected in any way and the `Rc` is not consumed. The pointer is valid
    /// for as long there are strong counts in the `Rc`.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::rc::Rc;
    ///
    /// let x = Rc::new("hello".to_owned());
    /// let y = Rc::clone(&x);
    /// let x_ptr = Rc::as_ptr(&x);
    /// assert_eq!(x_ptr, Rc::as_ptr(&y));
    /// assert_eq!(unsafe { &*x_ptr }, "hello");
    /// ```
    #[stable(feature = "weak_into_raw", since = "1.45.0")]
    pub fn as_ptr(this: &Self) -> *const T {
}

    /// Constructs an `Rc<T>` from a raw pointer.
    ///
    /// The raw pointer must have been previously returned by a call to
    /// [`Rc<U>::into_raw`][into_raw] where `U` must have the same size
    /// and alignment as `T`. This is trivially true if `U` is `T`.
    /// Note that if `U` is not `T` but has the same size and alignment, this is
    /// basically like transmuting references of different types. See
    /// [`mem::transmute`][transmute] for more information on what
    /// restrictions apply in this case.
    ///
    /// The user of `from_raw` has to make sure a specific value of `T` is only
    /// dropped once.
    ///
    /// This function is unsafe because improper use may lead to memory unsafety,
    /// even if the returned `Rc<T>` is never accessed.
    ///
    /// [into_raw]: Rc::into_raw
    /// [transmute]: core::mem::transmute
    ///
    /// # Examples
    ///
    /// ```
    /// use std::rc::Rc;
    ///
    /// let x = Rc::new("hello".to_owned());
    /// let x_ptr = Rc::into_raw(x);
    ///
    /// unsafe {
    ///     // Convert back to an `Rc` to prevent leak.
    ///     let x = Rc::from_raw(x_ptr);
    ///     assert_eq!(&*x, "hello");
    ///
    ///     // Further calls to `Rc::from_raw(x_ptr)` would be memory-unsafe.
    /// }
    ///
    /// // The memory was freed when `x` went out of scope above, so `x_ptr` is now dangling!
    /// ```
    #[stable(feature = "rc_raw", since = "1.17.0")]
    pub unsafe fn from_raw(ptr: *const T) -> Self {
}

    /// Creates a new [`Weak`] pointer to this allocation.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::rc::Rc;
    ///
    /// let five = Rc::new(5);
    ///
    /// let weak_five = Rc::downgrade(&five);
    /// ```
    #[stable(feature = "rc_weak", since = "1.4.0")]
    pub fn downgrade(this: &Self) -> Weak<T> {
}

    /// Gets the number of [`Weak`] pointers to this allocation.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::rc::Rc;
    ///
    /// let five = Rc::new(5);
    /// let _weak_five = Rc::downgrade(&five);
    ///
    /// assert_eq!(1, Rc::weak_count(&five));
    /// ```
    #[inline]
    #[stable(feature = "rc_counts", since = "1.15.0")]
    pub fn weak_count(this: &Self) -> usize {
}

    /// Gets the number of strong (`Rc`) pointers to this allocation.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::rc::Rc;
    ///
    /// let five = Rc::new(5);
    /// let _also_five = Rc::clone(&five);
    ///
    /// assert_eq!(2, Rc::strong_count(&five));
    /// ```
    #[inline]
    #[stable(feature = "rc_counts", since = "1.15.0")]
    pub fn strong_count(this: &Self) -> usize {
}

    /// Increments the strong reference count on the `Rc<T>` associated with the
    /// provided pointer by one.
    ///
    /// # Safety
    ///
    /// The pointer must have been obtained through `Rc::into_raw`, and the
    /// associated `Rc` instance must be valid (i.e. the strong count must be at
    /// least 1) for the duration of this method.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::rc::Rc;
    ///
    /// let five = Rc::new(5);
    ///
    /// unsafe {
    ///     let ptr = Rc::into_raw(five);
    ///     Rc::increment_strong_count(ptr);
    ///
    ///     let five = Rc::from_raw(ptr);
    ///     assert_eq!(2, Rc::strong_count(&five));
    /// }
    /// ```
    #[inline]
    #[stable(feature = "rc_mutate_strong_count", since = "1.53.0")]
    pub unsafe fn increment_strong_count(ptr: *const T) {
}

    /// Decrements the strong reference count on the `Rc<T>` associated with the
    /// provided pointer by one.
    ///
    /// # Safety
    ///
    /// The pointer must have been obtained through `Rc::into_raw`, and the
    /// associated `Rc` instance must be valid (i.e. the strong count must be at
    /// least 1) when invoking this method. This method can be used to release
    /// the final `Rc` and backing storage, but **should not** be called after
    /// the final `Rc` has been released.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::rc::Rc;
    ///
    /// let five = Rc::new(5);
    ///
    /// unsafe {
    ///     let ptr = Rc::into_raw(five);
    ///     Rc::increment_strong_count(ptr);
    ///
    ///     let five = Rc::from_raw(ptr);
    ///     assert_eq!(2, Rc::strong_count(&five));
    ///     Rc::decrement_strong_count(ptr);
    ///     assert_eq!(1, Rc::strong_count(&five));
    /// }
    /// ```
    #[inline]
    #[stable(feature = "rc_mutate_strong_count", since = "1.53.0")]
    pub unsafe fn decrement_strong_count(ptr: *const T) {
}

    /// Returns `true` if there are no other `Rc` or [`Weak`] pointers to
    /// this allocation.
    #[inline]
    fn is_unique(this: &Self) -> bool {
}

    /// Returns a mutable reference into the given `Rc`, if there are
    /// no other `Rc` or [`Weak`] pointers to the same allocation.
    ///
    /// Returns [`None`] otherwise, because it is not safe to
    /// mutate a shared value.
    ///
    /// See also [`make_mut`][make_mut], which will [`clone`][clone]
    /// the inner value when there are other `Rc` pointers.
    ///
    /// [make_mut]: Rc::make_mut
    /// [clone]: Clone::clone
    ///
    /// # Examples
    ///
    /// ```
    /// use std::rc::Rc;
    ///
    /// let mut x = Rc::new(3);
    /// *Rc::get_mut(&mut x).unwrap() = 4;
    /// assert_eq!(*x, 4);
    ///
    /// let _y = Rc::clone(&x);
    /// assert!(Rc::get_mut(&mut x).is_none());
    /// ```
    #[inline]
    #[stable(feature = "rc_unique", since = "1.4.0")]
    pub fn get_mut(this: &mut Self) -> Option<&mut T> {
}

    /// Returns a mutable reference into the given `Rc`,
    /// without any check.
    ///
    /// See also [`get_mut`], which is safe and does appropriate checks.
    ///
    /// [`get_mut`]: Rc::get_mut
    ///
    /// # Safety
    ///
    /// Any other `Rc` or [`Weak`] pointers to the same allocation must not be dereferenced
    /// for the duration of the returned borrow.
    /// This is trivially the case if no such pointers exist,
    /// for example immediately after `Rc::new`.
    ///
    /// # Examples
    ///
    /// ```
    /// #![feature(get_mut_unchecked)]
    ///
    /// use std::rc::Rc;
    ///
    /// let mut x = Rc::new(String::new());
    /// unsafe {
    ///     Rc::get_mut_unchecked(&mut x).push_str("foo")
    /// }
    /// assert_eq!(*x, "foo");
    /// ```
    #[inline]
    #[unstable(feature = "get_mut_unchecked", issue = "63292")]
    pub unsafe fn get_mut_unchecked(this: &mut Self) -> &mut T {
}

    #[inline]
    #[stable(feature = "ptr_eq", since = "1.17.0")]
    /// Returns `true` if the two `Rc`s point to the same allocation
    /// (in a vein similar to [`ptr::eq`]).
    ///
    /// # Examples
    ///
    /// ```
    /// use std::rc::Rc;
    ///
    /// let five = Rc::new(5);
    /// let same_five = Rc::clone(&five);
    /// let other_five = Rc::new(5);
    ///
    /// assert!(Rc::ptr_eq(&five, &same_five));
    /// assert!(!Rc::ptr_eq(&five, &other_five));
    /// ```
    ///
    /// [`ptr::eq`]: core::ptr::eq
    pub fn ptr_eq(this: &Self, other: &Self) -> bool {
}
}

impl<T: Clone> Rc<T> {
    /// Makes a mutable reference into the given `Rc`.
    ///
    /// If there are other `Rc` pointers to the same allocation, then `make_mut` will
    /// [`clone`] the inner value to a new allocation to ensure unique ownership.  This is also
    /// referred to as clone-on-write.
    ///
    /// However, if there are no other `Rc` pointers to this allocation, but some [`Weak`]
    /// pointers, then the [`Weak`] pointers will be disassociated and the inner value will not
    /// be cloned.
    ///
    /// See also [`get_mut`], which will fail rather than cloning the inner value
    /// or diassociating [`Weak`] pointers.
    ///
    /// [`clone`]: Clone::clone
    /// [`get_mut`]: Rc::get_mut
    ///
    /// # Examples
    ///
    /// ```
    /// use std::rc::Rc;
    ///
    /// let mut data = Rc::new(5);
    ///
    /// *Rc::make_mut(&mut data) += 1;         // Won't clone anything
    /// let mut other_data = Rc::clone(&data); // Won't clone inner data
    /// *Rc::make_mut(&mut data) += 1;         // Clones inner data
    /// *Rc::make_mut(&mut data) += 1;         // Won't clone anything
    /// *Rc::make_mut(&mut other_data) *= 2;   // Won't clone anything
    ///
    /// // Now `data` and `other_data` point to different allocations.
    /// assert_eq!(*data, 8);
    /// assert_eq!(*other_data, 12);
    /// ```
    ///
    /// [`Weak`] pointers will be disassociated:
    ///
    /// ```
    /// use std::rc::Rc;
    ///
    /// let mut data = Rc::new(75);
    /// let weak = Rc::downgrade(&data);
    ///
    /// assert!(75 == *data);
    /// assert!(75 == *weak.upgrade().unwrap());
    ///
    /// *Rc::make_mut(&mut data) += 1;
    ///
    /// assert!(76 == *data);
    /// assert!(weak.upgrade().is_none());
    /// ```
    #[cfg(not(no_global_oom_handling))]
    #[inline]
    #[stable(feature = "rc_unique", since = "1.4.0")]
    pub fn make_mut(this: &mut Self) -> &mut T {
}
}

impl Rc<dyn Any> {
    #[inline]
    #[stable(feature = "rc_downcast", since = "1.29.0")]
    /// Attempt to downcast the `Rc<dyn Any>` to a concrete type.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::any::Any;
    /// use std::rc::Rc;
    ///
    /// fn print_if_string(value: Rc<dyn Any>) {
    ///     if let Ok(string) = value.downcast::<String>() {
    ///         println!("String ({}): {}", string.len(), string);
    ///     }
    /// }
    ///
    /// let my_string = "Hello World".to_string();
    /// print_if_string(Rc::new(my_string));
    /// print_if_string(Rc::new(0i8));
    /// ```
    pub fn downcast<T: Any>(self) -> Result<Rc<T>, Rc<dyn Any>> {
}
}

impl<T: ?Sized> Rc<T> {
    /// Allocates an `RcBox<T>` with sufficient space for
    /// a possibly-unsized inner value where the value has the layout provided.
    ///
    /// The function `mem_to_rcbox` is called with the data pointer
    /// and must return back a (potentially fat)-pointer for the `RcBox<T>`.
    #[cfg(not(no_global_oom_handling))]
    unsafe fn allocate_for_layout(
        value_layout: Layout,
        allocate: impl FnOnce(Layout) -> Result<NonNull<[u8]>, AllocError>,
        mem_to_rcbox: impl FnOnce(*mut u8) -> *mut RcBox<T>,
    ) -> *mut RcBox<T> {
}

    /// Allocates an `RcBox<T>` with sufficient space for
    /// a possibly-unsized inner value where the value has the layout provided,
    /// returning an error if allocation fails.
    ///
    /// The function `mem_to_rcbox` is called with the data pointer
    /// and must return back a (potentially fat)-pointer for the `RcBox<T>`.
    #[inline]
    unsafe fn try_allocate_for_layout(
        value_layout: Layout,
        allocate: impl FnOnce(Layout) -> Result<NonNull<[u8]>, AllocError>,
        mem_to_rcbox: impl FnOnce(*mut u8) -> *mut RcBox<T>,
    ) -> Result<*mut RcBox<T>, AllocError> {
}

    /// Allocates an `RcBox<T>` with sufficient space for an unsized inner value
    #[cfg(not(no_global_oom_handling))]
    unsafe fn allocate_for_ptr(ptr: *const T) -> *mut RcBox<T> {
}

    #[cfg(not(no_global_oom_handling))]
    fn from_box(v: Box<T>) -> Rc<T> {
}
}

impl<T> Rc<[T]> {
    /// Allocates an `RcBox<[T]>` with the given length.
    #[cfg(not(no_global_oom_handling))]
    unsafe fn allocate_for_slice(len: usize) -> *mut RcBox<[T]> {
}

    /// Copy elements from slice into newly allocated Rc<\[T\]>
    ///
    /// Unsafe because the caller must either take ownership or bind `T: Copy`
    #[cfg(not(no_global_oom_handling))]
    unsafe fn copy_from_slice(v: &[T]) -> Rc<[T]> {
}

    /// Constructs an `Rc<[T]>` from an iterator known to be of a certain size.
    ///
    /// Behavior is undefined should the size be wrong.
    #[cfg(not(no_global_oom_handling))]
    unsafe fn from_iter_exact(iter: impl iter::Iterator<Item = T>, len: usize) -> Rc<[T]> {
}
}

/// Specialization trait used for `From<&[T]>`.
trait RcFromSlice<T> {
    fn from_slice(slice: &[T]) -> Self;
}

#[cfg(not(no_global_oom_handling))]
impl<T: Clone> RcFromSlice<T> for Rc<[T]> {
    #[inline]
    default fn from_slice(v: &[T]) -> Self {
}
}

#[cfg(not(no_global_oom_handling))]
impl<T: Copy> RcFromSlice<T> for Rc<[T]> {
    #[inline]
    fn from_slice(v: &[T]) -> Self {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<T: ?Sized> Deref for Rc<T> {
    type Target = T;

    #[inline(always)]
    fn deref(&self) -> &T {
}
}

#[unstable(feature = "receiver_trait", issue = "none")]
impl<T: ?Sized> Receiver for Rc<T> {}

#[stable(feature = "rust1", since = "1.0.0")]
unsafe impl<#[may_dangle] T: ?Sized> Drop for Rc<T> {
    /// Drops the `Rc`.
    ///
    /// This will decrement the strong reference count. If the strong reference
    /// count reaches zero then the only other references (if any) are
    /// [`Weak`], so we `drop` the inner value.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::rc::Rc;
    ///
    /// struct Foo;
    ///
    /// impl Drop for Foo {
    ///     fn drop(&mut self) {
    ///         println!("dropped!");
    ///     }
    /// }
    ///
    /// let foo  = Rc::new(Foo);
    /// let foo2 = Rc::clone(&foo);
    ///
    /// drop(foo);    // Doesn't print anything
    /// drop(foo2);   // Prints "dropped!"
    /// ```
    fn drop(&mut self) {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<T: ?Sized> Clone for Rc<T> {
    /// Makes a clone of the `Rc` pointer.
    ///
    /// This creates another pointer to the same allocation, increasing the
    /// strong reference count.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::rc::Rc;
    ///
    /// let five = Rc::new(5);
    ///
    /// let _ = Rc::clone(&five);
    /// ```
    #[inline]
    fn clone(&self) -> Rc<T> {
}
}

#[cfg(not(no_global_oom_handling))]
#[stable(feature = "rust1", since = "1.0.0")]
impl<T: Default> Default for Rc<T> {
    /// Creates a new `Rc<T>`, with the `Default` value for `T`.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::rc::Rc;
    ///
    /// let x: Rc<i32> = Default::default();
    /// assert_eq!(*x, 0);
    /// ```
    #[inline]
    fn default() -> Rc<T> {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
trait RcEqIdent<T: ?Sized + PartialEq> {
    fn eq(&self, other: &Rc<T>) -> bool;
    fn ne(&self, other: &Rc<T>) -> bool;
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<T: ?Sized + PartialEq> RcEqIdent<T> for Rc<T> {
    #[inline]
    default fn eq(&self, other: &Rc<T>) -> bool {
}

    #[inline]
    default fn ne(&self, other: &Rc<T>) -> bool {
}
}

// Hack to allow specializing on `Eq` even though `Eq` has a method.
#[rustc_unsafe_specialization_marker]
pub(crate) trait MarkerEq: PartialEq<Self> {}

impl<T: Eq> MarkerEq for T {}

/// We're doing this specialization here, and not as a more general optimization on `&T`, because it
/// would otherwise add a cost to all equality checks on refs. We assume that `Rc`s are used to
/// store large values, that are slow to clone, but also heavy to check for equality, causing this
/// cost to pay off more easily. It's also more likely to have two `Rc` clones, that point to
/// the same value, than two `&T`s.
///
/// We can only do this when `T: Eq` as a `PartialEq` might be deliberately irreflexive.
#[stable(feature = "rust1", since = "1.0.0")]
impl<T: ?Sized + MarkerEq> RcEqIdent<T> for Rc<T> {
    #[inline]
    fn eq(&self, other: &Rc<T>) -> bool {
}

    #[inline]
    fn ne(&self, other: &Rc<T>) -> bool {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<T: ?Sized + PartialEq> PartialEq for Rc<T> {
    /// Equality for two `Rc`s.
    ///
    /// Two `Rc`s are equal if their inner values are equal, even if they are
    /// stored in different allocation.
    ///
    /// If `T` also implements `Eq` (implying reflexivity of equality),
    /// two `Rc`s that point to the same allocation are
    /// always equal.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::rc::Rc;
    ///
    /// let five = Rc::new(5);
    ///
    /// assert!(five == Rc::new(5));
    /// ```
    #[inline]
    fn eq(&self, other: &Rc<T>) -> bool {
}

    /// Inequality for two `Rc`s.
    ///
    /// Two `Rc`s are unequal if their inner values are unequal.
    ///
    /// If `T` also implements `Eq` (implying reflexivity of equality),
    /// two `Rc`s that point to the same allocation are
    /// never unequal.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::rc::Rc;
    ///
    /// let five = Rc::new(5);
    ///
    /// assert!(five != Rc::new(6));
    /// ```
    #[inline]
    fn ne(&self, other: &Rc<T>) -> bool {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<T: ?Sized + Eq> Eq for Rc<T> {}

#[stable(feature = "rust1", since = "1.0.0")]
impl<T: ?Sized + PartialOrd> PartialOrd for Rc<T> {
    /// Partial comparison for two `Rc`s.
    ///
    /// The two are compared by calling `partial_cmp()` on their inner values.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::rc::Rc;
    /// use std::cmp::Ordering;
    ///
    /// let five = Rc::new(5);
    ///
    /// assert_eq!(Some(Ordering::Less), five.partial_cmp(&Rc::new(6)));
    /// ```
    #[inline(always)]
    fn partial_cmp(&self, other: &Rc<T>) -> Option<Ordering> {
}

    /// Less-than comparison for two `Rc`s.
    ///
    /// The two are compared by calling `<` on their inner values.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::rc::Rc;
    ///
    /// let five = Rc::new(5);
    ///
    /// assert!(five < Rc::new(6));
    /// ```
    #[inline(always)]
    fn lt(&self, other: &Rc<T>) -> bool {
}

    /// 'Less than or equal to' comparison for two `Rc`s.
    ///
    /// The two are compared by calling `<=` on their inner values.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::rc::Rc;
    ///
    /// let five = Rc::new(5);
    ///
    /// assert!(five <= Rc::new(5));
    /// ```
    #[inline(always)]
    fn le(&self, other: &Rc<T>) -> bool {
}

    /// Greater-than comparison for two `Rc`s.
    ///
    /// The two are compared by calling `>` on their inner values.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::rc::Rc;
    ///
    /// let five = Rc::new(5);
    ///
    /// assert!(five > Rc::new(4));
    /// ```
    #[inline(always)]
    fn gt(&self, other: &Rc<T>) -> bool {
}

    /// 'Greater than or equal to' comparison for two `Rc`s.
    ///
    /// The two are compared by calling `>=` on their inner values.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::rc::Rc;
    ///
    /// let five = Rc::new(5);
    ///
    /// assert!(five >= Rc::new(5));
    /// ```
    #[inline(always)]
    fn ge(&self, other: &Rc<T>) -> bool {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<T: ?Sized + Ord> Ord for Rc<T> {
    /// Comparison for two `Rc`s.
    ///
    /// The two are compared by calling `cmp()` on their inner values.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::rc::Rc;
    /// use std::cmp::Ordering;
    ///
    /// let five = Rc::new(5);
    ///
    /// assert_eq!(Ordering::Less, five.cmp(&Rc::new(6)));
    /// ```
    #[inline]
    fn cmp(&self, other: &Rc<T>) -> Ordering {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<T: ?Sized + Hash> Hash for Rc<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<T: ?Sized + fmt::Display> fmt::Display for Rc<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<T: ?Sized + fmt::Debug> fmt::Debug for Rc<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<T: ?Sized> fmt::Pointer for Rc<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
}
}

#[cfg(not(no_global_oom_handling))]
#[stable(feature = "from_for_ptrs", since = "1.6.0")]
impl<T> From<T> for Rc<T> {
    /// Converts a generic type `T` into an `Rc<T>`
    ///
    /// The conversion allocates on the heap and moves `t`
    /// from the stack into it.
    ///
    /// # Example
    /// ```rust
    /// # use std::rc::Rc;
    /// let x = 5;
    /// let rc = Rc::new(5);
    ///
    /// assert_eq!(Rc::from(x), rc);
    /// ```
    fn from(t: T) -> Self {
}
}

#[cfg(not(no_global_oom_handling))]
#[stable(feature = "shared_from_slice", since = "1.21.0")]
impl<T: Clone> From<&[T]> for Rc<[T]> {
    /// Allocate a reference-counted slice and fill it by cloning `v`'s items.
    ///
    /// # Example
    ///
    /// ```
    /// # use std::rc::Rc;
    /// let original: &[i32] = &[1, 2, 3];
    /// let shared: Rc<[i32]> = Rc::from(original);
    /// assert_eq!(&[1, 2, 3], &shared[..]);
    /// ```
    #[inline]
    fn from(v: &[T]) -> Rc<[T]> {
}
}

#[cfg(not(no_global_oom_handling))]
#[stable(feature = "shared_from_slice", since = "1.21.0")]
impl From<&str> for Rc<str> {
    /// Allocate a reference-counted string slice and copy `v` into it.
    ///
    /// # Example
    ///
    /// ```
    /// # use std::rc::Rc;
    /// let shared: Rc<str> = Rc::from("statue");
    /// assert_eq!("statue", &shared[..]);
    /// ```
    #[inline]
    fn from(v: &str) -> Rc<str> {
}
}

#[cfg(not(no_global_oom_handling))]
#[stable(feature = "shared_from_slice", since = "1.21.0")]
impl From<String> for Rc<str> {
    /// Allocate a reference-counted string slice and copy `v` into it.
    ///
    /// # Example
    ///
    /// ```
    /// # use std::rc::Rc;
    /// let original: String = "statue".to_owned();
    /// let shared: Rc<str> = Rc::from(original);
    /// assert_eq!("statue", &shared[..]);
    /// ```
    #[inline]
    fn from(v: String) -> Rc<str> {
}
}

#[cfg(not(no_global_oom_handling))]
#[stable(feature = "shared_from_slice", since = "1.21.0")]
impl<T: ?Sized> From<Box<T>> for Rc<T> {
    /// Move a boxed object to a new, reference counted, allocation.
    ///
    /// # Example
    ///
    /// ```
    /// # use std::rc::Rc;
    /// let original: Box<i32> = Box::new(1);
    /// let shared: Rc<i32> = Rc::from(original);
    /// assert_eq!(1, *shared);
    /// ```
    #[inline]
    fn from(v: Box<T>) -> Rc<T> {
}
}

#[cfg(not(no_global_oom_handling))]
#[stable(feature = "shared_from_slice", since = "1.21.0")]
impl<T> From<Vec<T>> for Rc<[T]> {
    /// Allocate a reference-counted slice and move `v`'s items into it.
    ///
    /// # Example
    ///
    /// ```
    /// # use std::rc::Rc;
    /// let original: Box<Vec<i32>> = Box::new(vec![1, 2, 3]);
    /// let shared: Rc<Vec<i32>> = Rc::from(original);
    /// assert_eq!(vec![1, 2, 3], *shared);
    /// ```
    #[inline]
    fn from(mut v: Vec<T>) -> Rc<[T]> {
}
}

#[stable(feature = "shared_from_cow", since = "1.45.0")]
impl<'a, B> From<Cow<'a, B>> for Rc<B>
where
    B: ToOwned + ?Sized,
    Rc<B>: From<&'a B> + From<B::Owned>,
{
    /// Create a reference-counted pointer from
    /// a clone-on-write pointer by copying its content.
    ///
    /// # Example
    ///
    /// ```rust
    /// # use std::rc::Rc;
    /// # use std::borrow::Cow;
    /// let cow: Cow<str> = Cow::Borrowed("eggplant");
    /// let shared: Rc<str> = Rc::from(cow);
    /// assert_eq!("eggplant", &shared[..]);
    /// ```
    #[inline]
    fn from(cow: Cow<'a, B>) -> Rc<B> {
}
}

#[stable(feature = "boxed_slice_try_from", since = "1.43.0")]
impl<T, const N: usize> TryFrom<Rc<[T]>> for Rc<[T; N]> {
    type Error = Rc<[T]>;

    fn try_from(boxed_slice: Rc<[T]>) -> Result<Self, Self::Error> {
}
}

#[cfg(not(no_global_oom_handling))]
#[stable(feature = "shared_from_iter", since = "1.37.0")]
impl<T> iter::FromIterator<T> for Rc<[T]> {
    /// Takes each element in the `Iterator` and collects it into an `Rc<[T]>`.
    ///
    /// # Performance characteristics
    ///
    /// ## The general case
    ///
    /// In the general case, collecting into `Rc<[T]>` is done by first
    /// collecting into a `Vec<T>`. That is, when writing the following:
    ///
    /// ```rust
    /// # use std::rc::Rc;
    /// let evens: Rc<[u8]> = (0..10).filter(|&x| x % 2 == 0).collect();
    /// # assert_eq!(&*evens, &[0, 2, 4, 6, 8]);
    /// ```
    ///
    /// this behaves as if we wrote:
    ///
    /// ```rust
    /// # use std::rc::Rc;
    /// let evens: Rc<[u8]> = (0..10).filter(|&x| x % 2 == 0)
    ///     .collect::<Vec<_>>() // The first set of allocations happens here.
    ///     .into(); // A second allocation for `Rc<[T]>` happens here.
    /// # assert_eq!(&*evens, &[0, 2, 4, 6, 8]);
    /// ```
    ///
    /// This will allocate as many times as needed for constructing the `Vec<T>`
    /// and then it will allocate once for turning the `Vec<T>` into the `Rc<[T]>`.
    ///
    /// ## Iterators of known length
    ///
    /// When your `Iterator` implements `TrustedLen` and is of an exact size,
    /// a single allocation will be made for the `Rc<[T]>`. For example:
    ///
    /// ```rust
    /// # use std::rc::Rc;
    /// let evens: Rc<[u8]> = (0..10).collect(); // Just a single allocation happens here.
    /// # assert_eq!(&*evens, &*(0..10).collect::<Vec<_>>());
    /// ```
    fn from_iter<I: iter::IntoIterator<Item = T>>(iter: I) -> Self {
}
}

/// Specialization trait used for collecting into `Rc<[T]>`.
#[cfg(not(no_global_oom_handling))]
trait ToRcSlice<T>: Iterator<Item = T> + Sized {
    fn to_rc_slice(self) -> Rc<[T]>;
}

#[cfg(not(no_global_oom_handling))]
impl<T, I: Iterator<Item = T>> ToRcSlice<T> for I {
    default fn to_rc_slice(self) -> Rc<[T]> {
}
}

#[cfg(not(no_global_oom_handling))]
impl<T, I: iter::TrustedLen<Item = T>> ToRcSlice<T> for I {
    fn to_rc_slice(self) -> Rc<[T]> {
}
}

/// `Weak` is a version of [`Rc`] that holds a non-owning reference to the
/// managed allocation. The allocation is accessed by calling [`upgrade`] on the `Weak`
/// pointer, which returns an [`Option`]`<`[`Rc`]`<T>>`.
///
/// Since a `Weak` reference does not count towards ownership, it will not
/// prevent the value stored in the allocation from being dropped, and `Weak` itself makes no
/// guarantees about the value still being present. Thus it may return [`None`]
/// when [`upgrade`]d. Note however that a `Weak` reference *does* prevent the allocation
/// itself (the backing store) from being deallocated.
///
/// A `Weak` pointer is useful for keeping a temporary reference to the allocation
/// managed by [`Rc`] without preventing its inner value from being dropped. It is also used to
/// prevent circular references between [`Rc`] pointers, since mutual owning references
/// would never allow either [`Rc`] to be dropped. For example, a tree could
/// have strong [`Rc`] pointers from parent nodes to children, and `Weak`
/// pointers from children back to their parents.
///
/// The typical way to obtain a `Weak` pointer is to call [`Rc::downgrade`].
///
/// [`upgrade`]: Weak::upgrade
#[stable(feature = "rc_weak", since = "1.4.0")]
pub struct Weak<T: ?Sized> {
    // This is a `NonNull` to allow optimizing the size of this type in enums,
    // but it is not necessarily a valid pointer.
    // `Weak::new` sets this to `usize::MAX` so that it doesn’t need
    // to allocate space on the heap.  That's not a value a real pointer
    // will ever have because RcBox has alignment at least 2.
    // This is only possible when `T: Sized`; unsized `T` never dangle.
    ptr: NonNull<RcBox<T>>,
}

#[stable(feature = "rc_weak", since = "1.4.0")]
impl<T: ?Sized> !marker::Send for Weak<T> {}
#[stable(feature = "rc_weak", since = "1.4.0")]
impl<T: ?Sized> !marker::Sync for Weak<T> {}

#[unstable(feature = "coerce_unsized", issue = "27732")]
impl<T: ?Sized + Unsize<U>, U: ?Sized> CoerceUnsized<Weak<U>> for Weak<T> {}

#[unstable(feature = "dispatch_from_dyn", issue = "none")]
impl<T: ?Sized + Unsize<U>, U: ?Sized> DispatchFromDyn<Weak<U>> for Weak<T> {}

impl<T> Weak<T> {
    /// Constructs a new `Weak<T>`, without allocating any memory.
    /// Calling [`upgrade`] on the return value always gives [`None`].
    ///
    /// [`upgrade`]: Weak::upgrade
    ///
    /// # Examples
    ///
    /// ```
    /// use std::rc::Weak;
    ///
    /// let empty: Weak<i64> = Weak::new();
    /// assert!(empty.upgrade().is_none());
    /// ```
    #[stable(feature = "downgraded_weak", since = "1.10.0")]
    pub fn new() -> Weak<T> {
}
}

pub(crate) fn is_dangling<T: ?Sized>(ptr: *mut T) -> bool {
}

/// Helper type to allow accessing the reference counts without
/// making any assertions about the data field.
struct WeakInner<'a> {
    weak: &'a Cell<usize>,
    strong: &'a Cell<usize>,
}

impl<T: ?Sized> Weak<T> {
    /// Returns a raw pointer to the object `T` pointed to by this `Weak<T>`.
    ///
    /// The pointer is valid only if there are some strong references. The pointer may be dangling,
    /// unaligned or even [`null`] otherwise.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::rc::Rc;
    /// use std::ptr;
    ///
    /// let strong = Rc::new("hello".to_owned());
    /// let weak = Rc::downgrade(&strong);
    /// // Both point to the same object
    /// assert!(ptr::eq(&*strong, weak.as_ptr()));
    /// // The strong here keeps it alive, so we can still access the object.
    /// assert_eq!("hello", unsafe { &*weak.as_ptr() });
    ///
    /// drop(strong);
    /// // But not any more. We can do weak.as_ptr(), but accessing the pointer would lead to
    /// // undefined behaviour.
    /// // assert_eq!("hello", unsafe { &*weak.as_ptr() });
    /// ```
    ///
    /// [`null`]: core::ptr::null
    #[stable(feature = "rc_as_ptr", since = "1.45.0")]
    pub fn as_ptr(&self) -> *const T {
}

    /// Consumes the `Weak<T>` and turns it into a raw pointer.
    ///
    /// This converts the weak pointer into a raw pointer, while still preserving the ownership of
    /// one weak reference (the weak count is not modified by this operation). It can be turned
    /// back into the `Weak<T>` with [`from_raw`].
    ///
    /// The same restrictions of accessing the target of the pointer as with
    /// [`as_ptr`] apply.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::rc::{Rc, Weak};
    ///
    /// let strong = Rc::new("hello".to_owned());
    /// let weak = Rc::downgrade(&strong);
    /// let raw = weak.into_raw();
    ///
    /// assert_eq!(1, Rc::weak_count(&strong));
    /// assert_eq!("hello", unsafe { &*raw });
    ///
    /// drop(unsafe { Weak::from_raw(raw) });
    /// assert_eq!(0, Rc::weak_count(&strong));
    /// ```
    ///
    /// [`from_raw`]: Weak::from_raw
    /// [`as_ptr`]: Weak::as_ptr
    #[stable(feature = "weak_into_raw", since = "1.45.0")]
    pub fn into_raw(self) -> *const T {
}

    /// Converts a raw pointer previously created by [`into_raw`] back into `Weak<T>`.
    ///
    /// This can be used to safely get a strong reference (by calling [`upgrade`]
    /// later) or to deallocate the weak count by dropping the `Weak<T>`.
    ///
    /// It takes ownership of one weak reference (with the exception of pointers created by [`new`],
    /// as these don't own anything; the method still works on them).
    ///
    /// # Safety
    ///
    /// The pointer must have originated from the [`into_raw`] and must still own its potential
    /// weak reference.
    ///
    /// It is allowed for the strong count to be 0 at the time of calling this. Nevertheless, this
    /// takes ownership of one weak reference currently represented as a raw pointer (the weak
    /// count is not modified by this operation) and therefore it must be paired with a previous
    /// call to [`into_raw`].
    ///
    /// # Examples
    ///
    /// ```
    /// use std::rc::{Rc, Weak};
    ///
    /// let strong = Rc::new("hello".to_owned());
    ///
    /// let raw_1 = Rc::downgrade(&strong).into_raw();
    /// let raw_2 = Rc::downgrade(&strong).into_raw();
    ///
    /// assert_eq!(2, Rc::weak_count(&strong));
    ///
    /// assert_eq!("hello", &*unsafe { Weak::from_raw(raw_1) }.upgrade().unwrap());
    /// assert_eq!(1, Rc::weak_count(&strong));
    ///
    /// drop(strong);
    ///
    /// // Decrement the last weak count.
    /// assert!(unsafe { Weak::from_raw(raw_2) }.upgrade().is_none());
    /// ```
    ///
    /// [`into_raw`]: Weak::into_raw
    /// [`upgrade`]: Weak::upgrade
    /// [`new`]: Weak::new
    #[stable(feature = "weak_into_raw", since = "1.45.0")]
    pub unsafe fn from_raw(ptr: *const T) -> Self {
}

    /// Attempts to upgrade the `Weak` pointer to an [`Rc`], delaying
    /// dropping of the inner value if successful.
    ///
    /// Returns [`None`] if the inner value has since been dropped.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::rc::Rc;
    ///
    /// let five = Rc::new(5);
    ///
    /// let weak_five = Rc::downgrade(&five);
    ///
    /// let strong_five: Option<Rc<_>> = weak_five.upgrade();
    /// assert!(strong_five.is_some());
    ///
    /// // Destroy all strong pointers.
    /// drop(strong_five);
    /// drop(five);
    ///
    /// assert!(weak_five.upgrade().is_none());
    /// ```
    #[stable(feature = "rc_weak", since = "1.4.0")]
    pub fn upgrade(&self) -> Option<Rc<T>> {
}

    /// Gets the number of strong (`Rc`) pointers pointing to this allocation.
    ///
    /// If `self` was created using [`Weak::new`], this will return 0.
    #[stable(feature = "weak_counts", since = "1.41.0")]
    pub fn strong_count(&self) -> usize {
}

    /// Gets the number of `Weak` pointers pointing to this allocation.
    ///
    /// If no strong pointers remain, this will return zero.
    #[stable(feature = "weak_counts", since = "1.41.0")]
    pub fn weak_count(&self) -> usize {
}

    /// Returns `None` when the pointer is dangling and there is no allocated `RcBox`,
    /// (i.e., when this `Weak` was created by `Weak::new`).
    #[inline]
    fn inner(&self) -> Option<WeakInner<'_>> {
}

    /// Returns `true` if the two `Weak`s point to the same allocation (similar to
    /// [`ptr::eq`]), or if both don't point to any allocation
    /// (because they were created with `Weak::new()`).
    ///
    /// # Notes
    ///
    /// Since this compares pointers it means that `Weak::new()` will equal each
    /// other, even though they don't point to any allocation.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::rc::Rc;
    ///
    /// let first_rc = Rc::new(5);
    /// let first = Rc::downgrade(&first_rc);
    /// let second = Rc::downgrade(&first_rc);
    ///
    /// assert!(first.ptr_eq(&second));
    ///
    /// let third_rc = Rc::new(5);
    /// let third = Rc::downgrade(&third_rc);
    ///
    /// assert!(!first.ptr_eq(&third));
    /// ```
    ///
    /// Comparing `Weak::new`.
    ///
    /// ```
    /// use std::rc::{Rc, Weak};
    ///
    /// let first = Weak::new();
    /// let second = Weak::new();
    /// assert!(first.ptr_eq(&second));
    ///
    /// let third_rc = Rc::new(());
    /// let third = Rc::downgrade(&third_rc);
    /// assert!(!first.ptr_eq(&third));
    /// ```
    ///
    /// [`ptr::eq`]: core::ptr::eq
    #[inline]
    #[stable(feature = "weak_ptr_eq", since = "1.39.0")]
    pub fn ptr_eq(&self, other: &Self) -> bool {
}
}

#[stable(feature = "rc_weak", since = "1.4.0")]
unsafe impl<#[may_dangle] T: ?Sized> Drop for Weak<T> {
    /// Drops the `Weak` pointer.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::rc::{Rc, Weak};
    ///
    /// struct Foo;
    ///
    /// impl Drop for Foo {
    ///     fn drop(&mut self) {
    ///         println!("dropped!");
    ///     }
    /// }
    ///
    /// let foo = Rc::new(Foo);
    /// let weak_foo = Rc::downgrade(&foo);
    /// let other_weak_foo = Weak::clone(&weak_foo);
    ///
    /// drop(weak_foo);   // Doesn't print anything
    /// drop(foo);        // Prints "dropped!"
    ///
    /// assert!(other_weak_foo.upgrade().is_none());
    /// ```
    fn drop(&mut self) {
}
}

#[stable(feature = "rc_weak", since = "1.4.0")]
impl<T: ?Sized> Clone for Weak<T> {
    /// Makes a clone of the `Weak` pointer that points to the same allocation.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::rc::{Rc, Weak};
    ///
    /// let weak_five = Rc::downgrade(&Rc::new(5));
    ///
    /// let _ = Weak::clone(&weak_five);
    /// ```
    #[inline]
    fn clone(&self) -> Weak<T> {
}
}

#[stable(feature = "rc_weak", since = "1.4.0")]
impl<T: ?Sized + fmt::Debug> fmt::Debug for Weak<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
}
}

#[stable(feature = "downgraded_weak", since = "1.10.0")]
impl<T> Default for Weak<T> {
    /// Constructs a new `Weak<T>`, without allocating any memory.
    /// Calling [`upgrade`] on the return value always gives [`None`].
    ///
    /// [`None`]: Option
    /// [`upgrade`]: Weak::upgrade
    ///
    /// # Examples
    ///
    /// ```
    /// use std::rc::Weak;
    ///
    /// let empty: Weak<i64> = Default::default();
    /// assert!(empty.upgrade().is_none());
    /// ```
    fn default() -> Weak<T> {
}
}

// NOTE: We checked_add here to deal with mem::forget safely. In particular
// if you mem::forget Rcs (or Weaks), the ref-count can overflow, and then
// you can free the allocation while outstanding Rcs (or Weaks) exist.
// We abort because this is such a degenerate scenario that we don't care about
// what happens -- no real program should ever experience this.
//
// This should have negligible overhead since you don't actually need to
// clone these much in Rust thanks to ownership and move-semantics.

#[doc(hidden)]
trait RcInnerPtr {
    fn weak_ref(&self) -> &Cell<usize>;
    fn strong_ref(&self) -> &Cell<usize>;

    #[inline]
    fn strong(&self) -> usize {
        self.strong_ref().get()
    }

    #[inline]
    fn inc_strong(&self) {
}

    #[inline]
    fn dec_strong(&self) {
}

    #[inline]
    fn weak(&self) -> usize {
}

    #[inline]
    fn inc_weak(&self) {
}

    #[inline]
    fn dec_weak(&self) {
}
}

impl<T: ?Sized> RcInnerPtr for RcBox<T> {
    #[inline(always)]
    fn weak_ref(&self) -> &Cell<usize> {
}

    #[inline(always)]
    fn strong_ref(&self) -> &Cell<usize> {
}
}

impl<'a> RcInnerPtr for WeakInner<'a> {
    #[inline(always)]
    fn weak_ref(&self) -> &Cell<usize> {
}

    #[inline(always)]
    fn strong_ref(&self) -> &Cell<usize> {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<T: ?Sized> borrow::Borrow<T> for Rc<T> {
    fn borrow(&self) -> &T {
}
}

#[stable(since = "1.5.0", feature = "smart_ptr_as_ref")]
impl<T: ?Sized> AsRef<T> for Rc<T> {
    fn as_ref(&self) -> &T {
}
}

#[stable(feature = "pin", since = "1.33.0")]
impl<T: ?Sized> Unpin for Rc<T> {}

/// Get the offset within an `RcBox` for the payload behind a pointer.
///
/// # Safety
///
/// The pointer must point to (and have valid metadata for) a previously
/// valid instance of T, but the T is allowed to be dropped.
unsafe fn data_offset<T: ?Sized>(ptr: *const T) -> isize {
}

#[inline]
fn data_offset_align(align: usize) -> isize {
}
}
pub mod slice {
//! A dynamically-sized view into a contiguous sequence, `[T]`.
//!
//! *[See also the slice primitive type](slice).*
//!
//! Slices are a view into a block of memory represented as a pointer and a
//! length.
//!
//! ```
//! // slicing a Vec
//! let vec = vec![1, 2, 3];
//! let int_slice = &vec[..];
//! // coercing an array to a slice
//! let str_slice: &[&str] = &["one", "two", "three"];
//! ```
//!
//! Slices are either mutable or shared. The shared slice type is `&[T]`,
//! while the mutable slice type is `&mut [T]`, where `T` represents the element
//! type. For example, you can mutate the block of memory that a mutable slice
//! points to:
//!
//! ```
//! let x = &mut [1, 2, 3];
//! x[1] = 7;
//! assert_eq!(x, &[1, 7, 3]);
//! ```
//!
//! Here are some of the things this module contains:
//!
//! ## Structs
//!
//! There are several structs that are useful for slices, such as [`Iter`], which
//! represents iteration over a slice.
//!
//! ## Trait Implementations
//!
//! There are several implementations of common traits for slices. Some examples
//! include:
//!
//! * [`Clone`]
//! * [`Eq`], [`Ord`] - for slices whose element type are [`Eq`] or [`Ord`].
//! * [`Hash`] - for slices whose element type is [`Hash`].
//!
//! ## Iteration
//!
//! The slices implement `IntoIterator`. The iterator yields references to the
//! slice elements.
//!
//! ```
//! let numbers = &[0, 1, 2];
//! for n in numbers {
//!     println!("{} is a number!", n);
//! }
//! ```
//!
//! The mutable slice yields mutable references to the elements:
//!
//! ```
//! let mut scores = [7, 8, 9];
//! for score in &mut scores[..] {
//!     *score += 1;
//! }
//! ```
//!
//! This iterator yields mutable references to the slice's elements, so while
//! the element type of the slice is `i32`, the element type of the iterator is
//! `&mut i32`.
//!
//! * [`.iter`] and [`.iter_mut`] are the explicit methods to return the default
//!   iterators.
//! * Further methods that return iterators are [`.split`], [`.splitn`],
//!   [`.chunks`], [`.windows`] and more.
//!
//! [`Hash`]: core::hash::Hash
//! [`.iter`]: slice::iter
//! [`.iter_mut`]: slice::iter_mut
//! [`.split`]: slice::split
//! [`.splitn`]: slice::splitn
//! [`.chunks`]: slice::chunks
//! [`.windows`]: slice::windows
#![stable(feature = "rust1", since = "1.0.0")]
// Many of the usings in this module are only used in the test configuration.
// It's cleaner to just turn off the unused_imports warning than to fix them.
#![cfg_attr(test, allow(unused_imports, dead_code))]

use core::borrow::{Borrow, BorrowMut};
#[cfg(not(no_global_oom_handling))]
use core::cmp::Ordering::{self, Less};
#[cfg(not(no_global_oom_handling))]
use core::mem;
#[cfg(not(no_global_oom_handling))]
use core::mem::size_of;
#[cfg(not(no_global_oom_handling))]
use core::ptr;

use crate::alloc::Allocator;
#[cfg(not(no_global_oom_handling))]
use crate::alloc::Global;
#[cfg(not(no_global_oom_handling))]
use crate::borrow::ToOwned;
use crate::boxed::Box;
use crate::vec::Vec;

#[unstable(feature = "slice_range", issue = "76393")]
pub use core::slice::range;
#[unstable(feature = "array_chunks", issue = "74985")]
pub use core::slice::ArrayChunks;
#[unstable(feature = "array_chunks", issue = "74985")]
pub use core::slice::ArrayChunksMut;
#[unstable(feature = "array_windows", issue = "75027")]
pub use core::slice::ArrayWindows;
#[stable(feature = "slice_get_slice", since = "1.28.0")]
pub use core::slice::SliceIndex;
#[stable(feature = "from_ref", since = "1.28.0")]
pub use core::slice::{from_mut, from_ref};
#[stable(feature = "rust1", since = "1.0.0")]
pub use core::slice::{from_raw_parts, from_raw_parts_mut};
#[stable(feature = "rust1", since = "1.0.0")]
pub use core::slice::{Chunks, Windows};
#[stable(feature = "chunks_exact", since = "1.31.0")]
pub use core::slice::{ChunksExact, ChunksExactMut};
#[stable(feature = "rust1", since = "1.0.0")]
pub use core::slice::{ChunksMut, Split, SplitMut};
#[unstable(feature = "slice_group_by", issue = "80552")]
pub use core::slice::{GroupBy, GroupByMut};
#[stable(feature = "rust1", since = "1.0.0")]
pub use core::slice::{Iter, IterMut};
#[stable(feature = "rchunks", since = "1.31.0")]
pub use core::slice::{RChunks, RChunksExact, RChunksExactMut, RChunksMut};
#[stable(feature = "slice_rsplit", since = "1.27.0")]
pub use core::slice::{RSplit, RSplitMut};
#[stable(feature = "rust1", since = "1.0.0")]
pub use core::slice::{RSplitN, RSplitNMut, SplitN, SplitNMut};

////////////////////////////////////////////////////////////////////////////////
// Basic slice extension methods
////////////////////////////////////////////////////////////////////////////////

// HACK(japaric) needed for the implementation of `vec!` macro during testing
// N.B., see the `hack` module in this file for more details.
#[cfg(test)]
pub use hack::into_vec;

// HACK(japaric) needed for the implementation of `Vec::clone` during testing
// N.B., see the `hack` module in this file for more details.
#[cfg(test)]
pub use hack::to_vec;

// HACK(japaric): With cfg(test) `impl [T]` is not available, these three
// functions are actually methods that are in `impl [T]` but not in
// `core::slice::SliceExt` - we need to supply these functions for the
// `test_permutations` test
mod hack {
    use core::alloc::Allocator;

    use crate::boxed::Box;
    use crate::vec::Vec;

    // We shouldn't add inline attribute to this since this is used in
    // `vec!` macro mostly and causes perf regression. See #71204 for
    // discussion and perf results.
    pub fn into_vec<T, A: Allocator>(b: Box<[T], A>) -> Vec<T, A> {
}

    #[cfg(not(no_global_oom_handling))]
    #[inline]
    pub fn to_vec<T: ConvertVec, A: Allocator>(s: &[T], alloc: A) -> Vec<T, A> {
}

    #[cfg(not(no_global_oom_handling))]
    pub trait ConvertVec {
        fn to_vec<A: Allocator>(s: &[Self], alloc: A) -> Vec<Self, A>
        where
            Self: Sized;
    }

    #[cfg(not(no_global_oom_handling))]
    impl<T: Clone> ConvertVec for T {
        #[inline]
        default fn to_vec<A: Allocator>(s: &[Self], alloc: A) -> Vec<Self, A> {
}
    }

    #[cfg(not(no_global_oom_handling))]
    impl<T: Copy> ConvertVec for T {
        #[inline]
        fn to_vec<A: Allocator>(s: &[Self], alloc: A) -> Vec<Self, A> {
}
    }
}

#[lang = "slice_alloc"]
#[cfg(not(test))]
impl<T> [T] {
    /// Sorts the slice.
    ///
    /// This sort is stable (i.e., does not reorder equal elements) and *O*(*n* \* log(*n*)) worst-case.
    ///
    /// When applicable, unstable sorting is preferred because it is generally faster than stable
    /// sorting and it doesn't allocate auxiliary memory.
    /// See [`sort_unstable`](slice::sort_unstable).
    ///
    /// # Current implementation
    ///
    /// The current algorithm is an adaptive, iterative merge sort inspired by
    /// [timsort](https://en.wikipedia.org/wiki/Timsort).
    /// It is designed to be very fast in cases where the slice is nearly sorted, or consists of
    /// two or more sorted sequences concatenated one after another.
    ///
    /// Also, it allocates temporary storage half the size of `self`, but for short slices a
    /// non-allocating insertion sort is used instead.
    ///
    /// # Examples
    ///
    /// ```
    /// let mut v = [-5, 4, 1, -3, 2];
    ///
    /// v.sort();
    /// assert!(v == [-5, -3, 1, 2, 4]);
    /// ```
    #[cfg(not(no_global_oom_handling))]
    #[stable(feature = "rust1", since = "1.0.0")]
    #[inline]
    pub fn sort(&mut self)
    where
        T: Ord,
    {
}

    /// Sorts the slice with a comparator function.
    ///
    /// This sort is stable (i.e., does not reorder equal elements) and *O*(*n* \* log(*n*)) worst-case.
    ///
    /// The comparator function must define a total ordering for the elements in the slice. If
    /// the ordering is not total, the order of the elements is unspecified. An order is a
    /// total order if it is (for all `a`, `b` and `c`):
    ///
    /// * total and antisymmetric: exactly one of `a < b`, `a == b` or `a > b` is true, and
    /// * transitive, `a < b` and `b < c` implies `a < c`. The same must hold for both `==` and `>`.
    ///
    /// For example, while [`f64`] doesn't implement [`Ord`] because `NaN != NaN`, we can use
    /// `partial_cmp` as our sort function when we know the slice doesn't contain a `NaN`.
    ///
    /// ```
    /// let mut floats = [5f64, 4.0, 1.0, 3.0, 2.0];
    /// floats.sort_by(|a, b| a.partial_cmp(b).unwrap());
    /// assert_eq!(floats, [1.0, 2.0, 3.0, 4.0, 5.0]);
    /// ```
    ///
    /// When applicable, unstable sorting is preferred because it is generally faster than stable
    /// sorting and it doesn't allocate auxiliary memory.
    /// See [`sort_unstable_by`](slice::sort_unstable_by).
    ///
    /// # Current implementation
    ///
    /// The current algorithm is an adaptive, iterative merge sort inspired by
    /// [timsort](https://en.wikipedia.org/wiki/Timsort).
    /// It is designed to be very fast in cases where the slice is nearly sorted, or consists of
    /// two or more sorted sequences concatenated one after another.
    ///
    /// Also, it allocates temporary storage half the size of `self`, but for short slices a
    /// non-allocating insertion sort is used instead.
    ///
    /// # Examples
    ///
    /// ```
    /// let mut v = [5, 4, 1, 3, 2];
    /// v.sort_by(|a, b| a.cmp(b));
    /// assert!(v == [1, 2, 3, 4, 5]);
    ///
    /// // reverse sorting
    /// v.sort_by(|a, b| b.cmp(a));
    /// assert!(v == [5, 4, 3, 2, 1]);
    /// ```
    #[cfg(not(no_global_oom_handling))]
    #[stable(feature = "rust1", since = "1.0.0")]
    #[inline]
    pub fn sort_by<F>(&mut self, mut compare: F)
    where
        F: FnMut(&T, &T) -> Ordering,
    {
}

    /// Sorts the slice with a key extraction function.
    ///
    /// This sort is stable (i.e., does not reorder equal elements) and *O*(*m* \* *n* \* log(*n*))
    /// worst-case, where the key function is *O*(*m*).
    ///
    /// For expensive key functions (e.g. functions that are not simple property accesses or
    /// basic operations), [`sort_by_cached_key`](slice::sort_by_cached_key) is likely to be
    /// significantly faster, as it does not recompute element keys.
    ///
    /// When applicable, unstable sorting is preferred because it is generally faster than stable
    /// sorting and it doesn't allocate auxiliary memory.
    /// See [`sort_unstable_by_key`](slice::sort_unstable_by_key).
    ///
    /// # Current implementation
    ///
    /// The current algorithm is an adaptive, iterative merge sort inspired by
    /// [timsort](https://en.wikipedia.org/wiki/Timsort).
    /// It is designed to be very fast in cases where the slice is nearly sorted, or consists of
    /// two or more sorted sequences concatenated one after another.
    ///
    /// Also, it allocates temporary storage half the size of `self`, but for short slices a
    /// non-allocating insertion sort is used instead.
    ///
    /// # Examples
    ///
    /// ```
    /// let mut v = [-5i32, 4, 1, -3, 2];
    ///
    /// v.sort_by_key(|k| k.abs());
    /// assert!(v == [1, 2, -3, 4, -5]);
    /// ```
    #[cfg(not(no_global_oom_handling))]
    #[stable(feature = "slice_sort_by_key", since = "1.7.0")]
    #[inline]
    pub fn sort_by_key<K, F>(&mut self, mut f: F)
    where
        F: FnMut(&T) -> K,
        K: Ord,
    {
}

    /// Sorts the slice with a key extraction function.
    ///
    /// During sorting, the key function is called only once per element.
    ///
    /// This sort is stable (i.e., does not reorder equal elements) and *O*(*m* \* *n* + *n* \* log(*n*))
    /// worst-case, where the key function is *O*(*m*).
    ///
    /// For simple key functions (e.g., functions that are property accesses or
    /// basic operations), [`sort_by_key`](slice::sort_by_key) is likely to be
    /// faster.
    ///
    /// # Current implementation
    ///
    /// The current algorithm is based on [pattern-defeating quicksort][pdqsort] by Orson Peters,
    /// which combines the fast average case of randomized quicksort with the fast worst case of
    /// heapsort, while achieving linear time on slices with certain patterns. It uses some
    /// randomization to avoid degenerate cases, but with a fixed seed to always provide
    /// deterministic behavior.
    ///
    /// In the worst case, the algorithm allocates temporary storage in a `Vec<(K, usize)>` the
    /// length of the slice.
    ///
    /// # Examples
    ///
    /// ```
    /// let mut v = [-5i32, 4, 32, -3, 2];
    ///
    /// v.sort_by_cached_key(|k| k.to_string());
    /// assert!(v == [-3, -5, 2, 32, 4]);
    /// ```
    ///
    /// [pdqsort]: https://github.com/orlp/pdqsort
    #[cfg(not(no_global_oom_handling))]
    #[stable(feature = "slice_sort_by_cached_key", since = "1.34.0")]
    #[inline]
    pub fn sort_by_cached_key<K, F>(&mut self, f: F)
    where
        F: FnMut(&T) -> K,
        K: Ord,
    {
}

    /// Copies `self` into a new `Vec`.
    ///
    /// # Examples
    ///
    /// ```
    /// let s = [10, 40, 30];
    /// let x = s.to_vec();
    /// // Here, `s` and `x` can be modified independently.
    /// ```
    #[cfg(not(no_global_oom_handling))]
    #[rustc_conversion_suggestion]
    #[stable(feature = "rust1", since = "1.0.0")]
    #[inline]
    pub fn to_vec(&self) -> Vec<T>
    where
        T: Clone,
    {
}

    /// Copies `self` into a new `Vec` with an allocator.
    ///
    /// # Examples
    ///
    /// ```
    /// #![feature(allocator_api)]
    ///
    /// use std::alloc::System;
    ///
    /// let s = [10, 40, 30];
    /// let x = s.to_vec_in(System);
    /// // Here, `s` and `x` can be modified independently.
    /// ```
    #[cfg(not(no_global_oom_handling))]
    #[inline]
    #[unstable(feature = "allocator_api", issue = "32838")]
    pub fn to_vec_in<A: Allocator>(&self, alloc: A) -> Vec<T, A>
    where
        T: Clone,
    {
}

    /// Converts `self` into a vector without clones or allocation.
    ///
    /// The resulting vector can be converted back into a box via
    /// `Vec<T>`'s `into_boxed_slice` method.
    ///
    /// # Examples
    ///
    /// ```
    /// let s: Box<[i32]> = Box::new([10, 40, 30]);
    /// let x = s.into_vec();
    /// // `s` cannot be used anymore because it has been converted into `x`.
    ///
    /// assert_eq!(x, vec![10, 40, 30]);
    /// ```
    #[stable(feature = "rust1", since = "1.0.0")]
    #[inline]
    pub fn into_vec<A: Allocator>(self: Box<Self, A>) -> Vec<T, A> {
}

    /// Creates a vector by repeating a slice `n` times.
    ///
    /// # Panics
    ///
    /// This function will panic if the capacity would overflow.
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// assert_eq!([1, 2].repeat(3), vec![1, 2, 1, 2, 1, 2]);
    /// ```
    ///
    /// A panic upon overflow:
    ///
    /// ```should_panic
    /// // this will panic at runtime
    /// b"0123456789abcdef".repeat(usize::MAX);
    /// ```
    #[cfg(not(no_global_oom_handling))]
    #[stable(feature = "repeat_generic_slice", since = "1.40.0")]
    pub fn repeat(&self, n: usize) -> Vec<T>
    where
        T: Copy,
    {
}

    /// Flattens a slice of `T` into a single value `Self::Output`.
    ///
    /// # Examples
    ///
    /// ```
    /// assert_eq!(["hello", "world"].concat(), "helloworld");
    /// assert_eq!([[1, 2], [3, 4]].concat(), [1, 2, 3, 4]);
    /// ```
    #[stable(feature = "rust1", since = "1.0.0")]
    pub fn concat<Item: ?Sized>(&self) -> <Self as Concat<Item>>::Output
    where
        Self: Concat<Item>,
    {
}

    /// Flattens a slice of `T` into a single value `Self::Output`, placing a
    /// given separator between each.
    ///
    /// # Examples
    ///
    /// ```
    /// assert_eq!(["hello", "world"].join(" "), "hello world");
    /// assert_eq!([[1, 2], [3, 4]].join(&0), [1, 2, 0, 3, 4]);
    /// assert_eq!([[1, 2], [3, 4]].join(&[0, 0][..]), [1, 2, 0, 0, 3, 4]);
    /// ```
    #[stable(feature = "rename_connect_to_join", since = "1.3.0")]
    pub fn join<Separator>(&self, sep: Separator) -> <Self as Join<Separator>>::Output
    where
        Self: Join<Separator>,
    {
}

    /// Flattens a slice of `T` into a single value `Self::Output`, placing a
    /// given separator between each.
    ///
    /// # Examples
    ///
    /// ```
    /// # #![allow(deprecated)]
    /// assert_eq!(["hello", "world"].connect(" "), "hello world");
    /// assert_eq!([[1, 2], [3, 4]].connect(&0), [1, 2, 0, 3, 4]);
    /// ```
    #[stable(feature = "rust1", since = "1.0.0")]
    #[rustc_deprecated(since = "1.3.0", reason = "renamed to join")]
    pub fn connect<Separator>(&self, sep: Separator) -> <Self as Join<Separator>>::Output
    where
        Self: Join<Separator>,
    {
}
}

#[lang = "slice_u8_alloc"]
#[cfg(not(test))]
impl [u8] {
    /// Returns a vector containing a copy of this slice where each byte
    /// is mapped to its ASCII upper case equivalent.
    ///
    /// ASCII letters 'a' to 'z' are mapped to 'A' to 'Z',
    /// but non-ASCII letters are unchanged.
    ///
    /// To uppercase the value in-place, use [`make_ascii_uppercase`].
    ///
    /// [`make_ascii_uppercase`]: slice::make_ascii_uppercase
    #[cfg(not(no_global_oom_handling))]
    #[stable(feature = "ascii_methods_on_intrinsics", since = "1.23.0")]
    #[inline]
    pub fn to_ascii_uppercase(&self) -> Vec<u8> {
}

    /// Returns a vector containing a copy of this slice where each byte
    /// is mapped to its ASCII lower case equivalent.
    ///
    /// ASCII letters 'A' to 'Z' are mapped to 'a' to 'z',
    /// but non-ASCII letters are unchanged.
    ///
    /// To lowercase the value in-place, use [`make_ascii_lowercase`].
    ///
    /// [`make_ascii_lowercase`]: slice::make_ascii_lowercase
    #[cfg(not(no_global_oom_handling))]
    #[stable(feature = "ascii_methods_on_intrinsics", since = "1.23.0")]
    #[inline]
    pub fn to_ascii_lowercase(&self) -> Vec<u8> {
}
}

////////////////////////////////////////////////////////////////////////////////
// Extension traits for slices over specific kinds of data
////////////////////////////////////////////////////////////////////////////////

/// Helper trait for [`[T]::concat`](slice::concat).
///
/// Note: the `Item` type parameter is not used in this trait,
/// but it allows impls to be more generic.
/// Without it, we get this error:
///
/// ```error
/// error[E0207]: the type parameter `T` is not constrained by the impl trait, self type, or predica
///    --> src/liballoc/slice.rs:608:6
///     |
/// 608 | impl<T: Clone, V: Borrow<[T]>> Concat for [V] {
///     |      ^ unconstrained type parameter
/// ```
///
/// This is because there could exist `V` types with multiple `Borrow<[_]>` impls,
/// such that multiple `T` types would apply:
///
/// ```
/// # #[allow(dead_code)]
/// pub struct Foo(Vec<u32>, Vec<String>);
///
/// impl std::borrow::Borrow<[u32]> for Foo {
///     fn borrow(&self) -> &[u32] { &self.0 }
/// }
///
/// impl std::borrow::Borrow<[String]> for Foo {
///     fn borrow(&self) -> &[String] { &self.1 }
/// }
/// ```
#[unstable(feature = "slice_concat_trait", issue = "27747")]
pub trait Concat<Item: ?Sized> {
    #[unstable(feature = "slice_concat_trait", issue = "27747")]
    /// The resulting type after concatenation
    type Output;

    /// Implementation of [`[T]::concat`](slice::concat)
    #[unstable(feature = "slice_concat_trait", issue = "27747")]
    fn concat(slice: &Self) -> Self::Output;
}

/// Helper trait for [`[T]::join`](slice::join)
#[unstable(feature = "slice_concat_trait", issue = "27747")]
pub trait Join<Separator> {
    #[unstable(feature = "slice_concat_trait", issue = "27747")]
    /// The resulting type after concatenation
    type Output;

    /// Implementation of [`[T]::join`](slice::join)
    #[unstable(feature = "slice_concat_trait", issue = "27747")]
    fn join(slice: &Self, sep: Separator) -> Self::Output;
}

#[cfg(not(no_global_oom_handling))]
#[unstable(feature = "slice_concat_ext", issue = "27747")]
impl<T: Clone, V: Borrow<[T]>> Concat<T> for [V] {
    type Output = Vec<T>;

    fn concat(slice: &Self) -> Vec<T> {
}
}

#[cfg(not(no_global_oom_handling))]
#[unstable(feature = "slice_concat_ext", issue = "27747")]
impl<T: Clone, V: Borrow<[T]>> Join<&T> for [V] {
    type Output = Vec<T>;

    fn join(slice: &Self, sep: &T) -> Vec<T> {
}
}

#[cfg(not(no_global_oom_handling))]
#[unstable(feature = "slice_concat_ext", issue = "27747")]
impl<T: Clone, V: Borrow<[T]>> Join<&[T]> for [V] {
    type Output = Vec<T>;

    fn join(slice: &Self, sep: &[T]) -> Vec<T> {
}
}

////////////////////////////////////////////////////////////////////////////////
// Standard trait implementations for slices
////////////////////////////////////////////////////////////////////////////////

#[stable(feature = "rust1", since = "1.0.0")]
impl<T> Borrow<[T]> for Vec<T> {
    fn borrow(&self) -> &[T] {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<T> BorrowMut<[T]> for Vec<T> {
    fn borrow_mut(&mut self) -> &mut [T] {
}
}

#[cfg(not(no_global_oom_handling))]
#[stable(feature = "rust1", since = "1.0.0")]
impl<T: Clone> ToOwned for [T] {
    type Owned = Vec<T>;
    #[cfg(not(test))]
    fn to_owned(&self) -> Vec<T> {
}

    #[cfg(test)]
    fn to_owned(&self) -> Vec<T> {
}

    fn clone_into(&self, target: &mut Vec<T>) {
}
}

////////////////////////////////////////////////////////////////////////////////
// Sorting
////////////////////////////////////////////////////////////////////////////////

/// Inserts `v[0]` into pre-sorted sequence `v[1..]` so that whole `v[..]` becomes sorted.
///
/// This is the integral subroutine of insertion sort.
#[cfg(not(no_global_oom_handling))]
fn insert_head<T, F>(v: &mut [T], is_less: &mut F)
where
    F: FnMut(&T, &T) -> bool,
{
}

/// Merges non-decreasing runs `v[..mid]` and `v[mid..]` using `buf` as temporary storage, and
/// stores the result into `v[..]`.
///
/// # Safety
///
/// The two slices must be non-empty and `mid` must be in bounds. Buffer `buf` must be long enough
/// to hold a copy of the shorter slice. Also, `T` must not be a zero-sized type.
#[cfg(not(no_global_oom_handling))]
unsafe fn merge<T, F>(v: &mut [T], mid: usize, buf: *mut T, is_less: &mut F)
where
    F: FnMut(&T, &T) -> bool,
{
}

/// This merge sort borrows some (but not all) ideas from TimSort, which is described in detail
/// [here](https://github.com/python/cpython/blob/main/Objects/listsort.txt).
///
/// The algorithm identifies strictly descending and non-descending subsequences, which are called
/// natural runs. There is a stack of pending runs yet to be merged. Each newly found run is pushed
/// onto the stack, and then some pairs of adjacent runs are merged until these two invariants are
/// satisfied:
///
/// 1. for every `i` in `1..runs.len()`: `runs[i - 1].len > runs[i].len`
/// 2. for every `i` in `2..runs.len()`: `runs[i - 2].len > runs[i - 1].len + runs[i].len`
///
/// The invariants ensure that the total running time is *O*(*n* \* log(*n*)) worst-case.
#[cfg(not(no_global_oom_handling))]
fn merge_sort<T, F>(v: &mut [T], mut is_less: F)
where
    F: FnMut(&T, &T) -> bool,
{
}
}
pub mod str {
//! Unicode string slices.
//!
//! *[See also the `str` primitive type](str).*
//!
//! The `&str` type is one of the two main string types, the other being `String`.
//! Unlike its `String` counterpart, its contents are borrowed.
//!
//! # Basic Usage
//!
//! A basic string declaration of `&str` type:
//!
//! ```
//! let hello_world = "Hello, World!";
//! ```
//!
//! Here we have declared a string literal, also known as a string slice.
//! String literals have a static lifetime, which means the string `hello_world`
//! is guaranteed to be valid for the duration of the entire program.
//! We can explicitly specify `hello_world`'s lifetime as well:
//!
//! ```
//! let hello_world: &'static str = "Hello, world!";
//! ```

#![stable(feature = "rust1", since = "1.0.0")]
// Many of the usings in this module are only used in the test configuration.
// It's cleaner to just turn off the unused_imports warning than to fix them.
#![allow(unused_imports)]

use core::borrow::{Borrow, BorrowMut};
use core::iter::FusedIterator;
use core::mem;
use core::ptr;
use core::str::pattern::{DoubleEndedSearcher, Pattern, ReverseSearcher, Searcher};
use core::unicode::conversions;

use crate::borrow::ToOwned;
use crate::boxed::Box;
use crate::slice::{Concat, Join, SliceIndex};
use crate::string::String;
use crate::vec::Vec;

#[stable(feature = "rust1", since = "1.0.0")]
pub use core::str::pattern;
#[stable(feature = "encode_utf16", since = "1.8.0")]
pub use core::str::EncodeUtf16;
#[stable(feature = "split_ascii_whitespace", since = "1.34.0")]
pub use core::str::SplitAsciiWhitespace;
#[stable(feature = "split_inclusive", since = "1.53.0")]
pub use core::str::SplitInclusive;
#[stable(feature = "rust1", since = "1.0.0")]
pub use core::str::SplitWhitespace;
#[stable(feature = "rust1", since = "1.0.0")]
pub use core::str::{from_utf8, from_utf8_mut, Bytes, CharIndices, Chars};
#[stable(feature = "rust1", since = "1.0.0")]
pub use core::str::{from_utf8_unchecked, from_utf8_unchecked_mut, ParseBoolError};
#[stable(feature = "str_escape", since = "1.34.0")]
pub use core::str::{EscapeDebug, EscapeDefault, EscapeUnicode};
#[stable(feature = "rust1", since = "1.0.0")]
pub use core::str::{FromStr, Utf8Error};
#[allow(deprecated)]
#[stable(feature = "rust1", since = "1.0.0")]
pub use core::str::{Lines, LinesAny};
#[stable(feature = "rust1", since = "1.0.0")]
pub use core::str::{MatchIndices, RMatchIndices};
#[stable(feature = "rust1", since = "1.0.0")]
pub use core::str::{Matches, RMatches};
#[stable(feature = "rust1", since = "1.0.0")]
pub use core::str::{RSplit, Split};
#[stable(feature = "rust1", since = "1.0.0")]
pub use core::str::{RSplitN, SplitN};
#[stable(feature = "rust1", since = "1.0.0")]
pub use core::str::{RSplitTerminator, SplitTerminator};

/// Note: `str` in `Concat<str>` is not meaningful here.
/// This type parameter of the trait only exists to enable another impl.
#[cfg(not(no_global_oom_handling))]
#[unstable(feature = "slice_concat_ext", issue = "27747")]
impl<S: Borrow<str>> Concat<str> for [S] {
    type Output = String;

    fn concat(slice: &Self) -> String {
}
}

#[cfg(not(no_global_oom_handling))]
#[unstable(feature = "slice_concat_ext", issue = "27747")]
impl<S: Borrow<str>> Join<&str> for [S] {
    type Output = String;

    fn join(slice: &Self, sep: &str) -> String {
}
}

#[cfg(not(no_global_oom_handling))]
macro_rules! specialize_for_lengths {
    ($separator:expr, $target:expr, $iter:expr; $($num:expr),*) => {{
        let mut target = $target;
        let iter = $iter;
        let sep_bytes = $separator;
        match $separator.len() {
            $(
                // loops with hardcoded sizes run much faster
                // specialize the cases with small separator lengths
                $num => {
                    for s in iter {
                        copy_slice_and_advance!(target, sep_bytes);
                        let content_bytes = s.borrow().as_ref();
                        copy_slice_and_advance!(target, content_bytes);
                    }
                },
            )*
            _ => {
                // arbitrary non-zero size fallback
                for s in iter {
                    copy_slice_and_advance!(target, sep_bytes);
                    let content_bytes = s.borrow().as_ref();
                    copy_slice_and_advance!(target, content_bytes);
                }
            }
        }
        target
    }}
}

#[cfg(not(no_global_oom_handling))]
macro_rules! copy_slice_and_advance {
    ($target:expr, $bytes:expr) => {
        let len = $bytes.len();
        let (head, tail) = { $target }.split_at_mut(len);
        head.copy_from_slice($bytes);
        $target = tail;
    };
}

// Optimized join implementation that works for both Vec<T> (T: Copy) and String's inner vec
// Currently (2018-05-13) there is a bug with type inference and specialization (see issue #36262)
// For this reason SliceConcat<T> is not specialized for T: Copy and SliceConcat<str> is the
// only user of this function. It is left in place for the time when that is fixed.
//
// the bounds for String-join are S: Borrow<str> and for Vec-join Borrow<[T]>
// [T] and str both impl AsRef<[T]> for some T
// => s.borrow().as_ref() and we always have slices
#[cfg(not(no_global_oom_handling))]
fn join_generic_copy<B, T, S>(slice: &[S], sep: &[T]) -> Vec<T>
where
    T: Copy,
    B: AsRef<[T]> + ?Sized,
    S: Borrow<B>,
{
}

#[stable(feature = "rust1", since = "1.0.0")]
impl Borrow<str> for String {
    #[inline]
    fn borrow(&self) -> &str {
}
}

#[stable(feature = "string_borrow_mut", since = "1.36.0")]
impl BorrowMut<str> for String {
    #[inline]
    fn borrow_mut(&mut self) -> &mut str {
}
}

#[cfg(not(no_global_oom_handling))]
#[stable(feature = "rust1", since = "1.0.0")]
impl ToOwned for str {
    type Owned = String;
    #[inline]
    fn to_owned(&self) -> String {
}

    fn clone_into(&self, target: &mut String) {
}
}

/// Methods for string slices.
#[lang = "str_alloc"]
#[cfg(not(test))]
impl str {
    /// Converts a `Box<str>` into a `Box<[u8]>` without copying or allocating.
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// let s = "this is a string";
    /// let boxed_str = s.to_owned().into_boxed_str();
    /// let boxed_bytes = boxed_str.into_boxed_bytes();
    /// assert_eq!(*boxed_bytes, *s.as_bytes());
    /// ```
    #[stable(feature = "str_box_extras", since = "1.20.0")]
    #[inline]
    pub fn into_boxed_bytes(self: Box<str>) -> Box<[u8]> {
}

    /// Replaces all matches of a pattern with another string.
    ///
    /// `replace` creates a new [`String`], and copies the data from this string slice into it.
    /// While doing so, it attempts to find matches of a pattern. If it finds any, it
    /// replaces them with the replacement string slice.
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// let s = "this is old";
    ///
    /// assert_eq!("this is new", s.replace("old", "new"));
    /// ```
    ///
    /// When the pattern doesn't match:
    ///
    /// ```
    /// let s = "this is old";
    /// assert_eq!(s, s.replace("cookie monster", "little lamb"));
    /// ```
    #[cfg(not(no_global_oom_handling))]
    #[must_use = "this returns the replaced string as a new allocation, \
                  without modifying the original"]
    #[stable(feature = "rust1", since = "1.0.0")]
    #[inline]
    pub fn replace<'a, P: Pattern<'a>>(&'a self, from: P, to: &str) -> String {
}

    /// Replaces first N matches of a pattern with another string.
    ///
    /// `replacen` creates a new [`String`], and copies the data from this string slice into it.
    /// While doing so, it attempts to find matches of a pattern. If it finds any, it
    /// replaces them with the replacement string slice at most `count` times.
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// let s = "foo foo 123 foo";
    /// assert_eq!("new new 123 foo", s.replacen("foo", "new", 2));
    /// assert_eq!("faa fao 123 foo", s.replacen('o', "a", 3));
    /// assert_eq!("foo foo new23 foo", s.replacen(char::is_numeric, "new", 1));
    /// ```
    ///
    /// When the pattern doesn't match:
    ///
    /// ```
    /// let s = "this is old";
    /// assert_eq!(s, s.replacen("cookie monster", "little lamb", 10));
    /// ```
    #[cfg(not(no_global_oom_handling))]
    #[must_use = "this returns the replaced string as a new allocation, \
                  without modifying the original"]
    #[stable(feature = "str_replacen", since = "1.16.0")]
    pub fn replacen<'a, P: Pattern<'a>>(&'a self, pat: P, to: &str, count: usize) -> String {
}

    /// Returns the lowercase equivalent of this string slice, as a new [`String`].
    ///
    /// 'Lowercase' is defined according to the terms of the Unicode Derived Core Property
    /// `Lowercase`.
    ///
    /// Since some characters can expand into multiple characters when changing
    /// the case, this function returns a [`String`] instead of modifying the
    /// parameter in-place.
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// let s = "HELLO";
    ///
    /// assert_eq!("hello", s.to_lowercase());
    /// ```
    ///
    /// A tricky example, with sigma:
    ///
    /// ```
    /// let sigma = "Σ";
    ///
    /// assert_eq!("σ", sigma.to_lowercase());
    ///
    /// // but at the end of a word, it's ς, not σ:
    /// let odysseus = "ὈΔΥΣΣΕΎΣ";
    ///
    /// assert_eq!("ὀδυσσεύς", odysseus.to_lowercase());
    /// ```
    ///
    /// Languages without case are not changed:
    ///
    /// ```
    /// let new_year = "农历新年";
    ///
    /// assert_eq!(new_year, new_year.to_lowercase());
    /// ```
    #[cfg(not(no_global_oom_handling))]
    #[stable(feature = "unicode_case_mapping", since = "1.2.0")]
    pub fn to_lowercase(&self) -> String {
}

    /// Returns the uppercase equivalent of this string slice, as a new [`String`].
    ///
    /// 'Uppercase' is defined according to the terms of the Unicode Derived Core Property
    /// `Uppercase`.
    ///
    /// Since some characters can expand into multiple characters when changing
    /// the case, this function returns a [`String`] instead of modifying the
    /// parameter in-place.
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// let s = "hello";
    ///
    /// assert_eq!("HELLO", s.to_uppercase());
    /// ```
    ///
    /// Scripts without case are not changed:
    ///
    /// ```
    /// let new_year = "农历新年";
    ///
    /// assert_eq!(new_year, new_year.to_uppercase());
    /// ```
    ///
    /// One character can become multiple:
    /// ```
    /// let s = "tschüß";
    ///
    /// assert_eq!("TSCHÜSS", s.to_uppercase());
    /// ```
    #[cfg(not(no_global_oom_handling))]
    #[stable(feature = "unicode_case_mapping", since = "1.2.0")]
    pub fn to_uppercase(&self) -> String {
}

    /// Converts a [`Box<str>`] into a [`String`] without copying or allocating.
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// let string = String::from("birthday gift");
    /// let boxed_str = string.clone().into_boxed_str();
    ///
    /// assert_eq!(boxed_str.into_string(), string);
    /// ```
    #[stable(feature = "box_str", since = "1.4.0")]
    #[inline]
    pub fn into_string(self: Box<str>) -> String {
}

    /// Creates a new [`String`] by repeating a string `n` times.
    ///
    /// # Panics
    ///
    /// This function will panic if the capacity would overflow.
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// assert_eq!("abc".repeat(4), String::from("abcabcabcabc"));
    /// ```
    ///
    /// A panic upon overflow:
    ///
    /// ```should_panic
    /// // this will panic at runtime
    /// "0123456789abcdef".repeat(usize::MAX);
    /// ```
    #[cfg(not(no_global_oom_handling))]
    #[stable(feature = "repeat_str", since = "1.16.0")]
    pub fn repeat(&self, n: usize) -> String {
}

    /// Returns a copy of this string where each character is mapped to its
    /// ASCII upper case equivalent.
    ///
    /// ASCII letters 'a' to 'z' are mapped to 'A' to 'Z',
    /// but non-ASCII letters are unchanged.
    ///
    /// To uppercase the value in-place, use [`make_ascii_uppercase`].
    ///
    /// To uppercase ASCII characters in addition to non-ASCII characters, use
    /// [`to_uppercase`].
    ///
    /// # Examples
    ///
    /// ```
    /// let s = "Grüße, Jürgen ❤";
    ///
    /// assert_eq!("GRüßE, JüRGEN ❤", s.to_ascii_uppercase());
    /// ```
    ///
    /// [`make_ascii_uppercase`]: str::make_ascii_uppercase
    /// [`to_uppercase`]: #method.to_uppercase
    #[cfg(not(no_global_oom_handling))]
    #[stable(feature = "ascii_methods_on_intrinsics", since = "1.23.0")]
    #[inline]
    pub fn to_ascii_uppercase(&self) -> String {
}

    /// Returns a copy of this string where each character is mapped to its
    /// ASCII lower case equivalent.
    ///
    /// ASCII letters 'A' to 'Z' are mapped to 'a' to 'z',
    /// but non-ASCII letters are unchanged.
    ///
    /// To lowercase the value in-place, use [`make_ascii_lowercase`].
    ///
    /// To lowercase ASCII characters in addition to non-ASCII characters, use
    /// [`to_lowercase`].
    ///
    /// # Examples
    ///
    /// ```
    /// let s = "Grüße, Jürgen ❤";
    ///
    /// assert_eq!("grüße, jürgen ❤", s.to_ascii_lowercase());
    /// ```
    ///
    /// [`make_ascii_lowercase`]: str::make_ascii_lowercase
    /// [`to_lowercase`]: #method.to_lowercase
    #[cfg(not(no_global_oom_handling))]
    #[stable(feature = "ascii_methods_on_intrinsics", since = "1.23.0")]
    #[inline]
    pub fn to_ascii_lowercase(&self) -> String {
}
}

/// Converts a boxed slice of bytes to a boxed string slice without checking
/// that the string contains valid UTF-8.
///
/// # Examples
///
/// Basic usage:
///
/// ```
/// let smile_utf8 = Box::new([226, 152, 186]);
/// let smile = unsafe { std::str::from_boxed_utf8_unchecked(smile_utf8) };
///
/// assert_eq!("☺", &*smile);
/// ```
#[stable(feature = "str_box_extras", since = "1.20.0")]
#[inline]
pub unsafe fn from_boxed_utf8_unchecked(v: Box<[u8]>) -> Box<str> {
}
}
pub mod string {
//! A UTF-8–encoded, growable string.
//!
//! This module contains the [`String`] type, the [`ToString`] trait for
//! converting to strings, and several error types that may result from
//! working with [`String`]s.
//!
//! # Examples
//!
//! There are multiple ways to create a new [`String`] from a string literal:
//!
//! ```
//! let s = "Hello".to_string();
//!
//! let s = String::from("world");
//! let s: String = "also this".into();
//! ```
//!
//! You can create a new [`String`] from an existing one by concatenating with
//! `+`:
//!
//! ```
//! let s = "Hello".to_string();
//!
//! let message = s + " world!";
//! ```
//!
//! If you have a vector of valid UTF-8 bytes, you can make a [`String`] out of
//! it. You can do the reverse too.
//!
//! ```
//! let sparkle_heart = vec![240, 159, 146, 150];
//!
//! // We know these bytes are valid, so we'll use `unwrap()`.
//! let sparkle_heart = String::from_utf8(sparkle_heart).unwrap();
//!
//! assert_eq!("💖", sparkle_heart);
//!
//! let bytes = sparkle_heart.into_bytes();
//!
//! assert_eq!(bytes, [240, 159, 146, 150]);
//! ```

#![stable(feature = "rust1", since = "1.0.0")]

#[cfg(not(no_global_oom_handling))]
use core::char::{decode_utf16, REPLACEMENT_CHARACTER};
use core::fmt;
use core::hash;
#[cfg(not(no_global_oom_handling))]
use core::iter::FromIterator;
use core::iter::{from_fn, FusedIterator};
#[cfg(not(no_global_oom_handling))]
use core::ops::Add;
#[cfg(not(no_global_oom_handling))]
use core::ops::AddAssign;
#[cfg(not(no_global_oom_handling))]
use core::ops::Bound::{Excluded, Included, Unbounded};
use core::ops::{self, Index, IndexMut, Range, RangeBounds};
use core::ptr;
use core::slice;
#[cfg(not(no_global_oom_handling))]
use core::str::lossy;
use core::str::pattern::Pattern;

#[cfg(not(no_global_oom_handling))]
use crate::borrow::{Cow, ToOwned};
use crate::boxed::Box;
use crate::collections::TryReserveError;
use crate::str::{self, Chars, Utf8Error};
#[cfg(not(no_global_oom_handling))]
use crate::str::{from_boxed_utf8_unchecked, FromStr};
use crate::vec::Vec;

/// A UTF-8–encoded, growable string.
///
/// The `String` type is the most common string type that has ownership over the
/// contents of the string. It has a close relationship with its borrowed
/// counterpart, the primitive [`str`].
///
/// # Examples
///
/// You can create a `String` from [a literal string][`str`] with [`String::from`]:
///
/// [`String::from`]: From::from
///
/// ```
/// let hello = String::from("Hello, world!");
/// ```
///
/// You can append a [`char`] to a `String` with the [`push`] method, and
/// append a [`&str`] with the [`push_str`] method:
///
/// ```
/// let mut hello = String::from("Hello, ");
///
/// hello.push('w');
/// hello.push_str("orld!");
/// ```
///
/// [`push`]: String::push
/// [`push_str`]: String::push_str
///
/// If you have a vector of UTF-8 bytes, you can create a `String` from it with
/// the [`from_utf8`] method:
///
/// ```
/// // some bytes, in a vector
/// let sparkle_heart = vec![240, 159, 146, 150];
///
/// // We know these bytes are valid, so we'll use `unwrap()`.
/// let sparkle_heart = String::from_utf8(sparkle_heart).unwrap();
///
/// assert_eq!("💖", sparkle_heart);
/// ```
///
/// [`from_utf8`]: String::from_utf8
///
/// # UTF-8
///
/// `String`s are always valid UTF-8. This has a few implications, the first of
/// which is that if you need a non-UTF-8 string, consider [`OsString`]. It is
/// similar, but without the UTF-8 constraint. The second implication is that
/// you cannot index into a `String`:
///
/// ```compile_fail,E0277
/// let s = "hello";
///
/// println!("The first letter of s is {}", s[0]); // ERROR!!!
/// ```
///
/// [`OsString`]: ../../std/ffi/struct.OsString.html
///
/// Indexing is intended to be a constant-time operation, but UTF-8 encoding
/// does not allow us to do this. Furthermore, it's not clear what sort of
/// thing the index should return: a byte, a codepoint, or a grapheme cluster.
/// The [`bytes`] and [`chars`] methods return iterators over the first
/// two, respectively.
///
/// [`bytes`]: str::bytes
/// [`chars`]: str::chars
///
/// # Deref
///
/// `String`s implement [`Deref`]`<Target=str>`, and so inherit all of [`str`]'s
/// methods. In addition, this means that you can pass a `String` to a
/// function which takes a [`&str`] by using an ampersand (`&`):
///
/// ```
/// fn takes_str(s: &str) { }
///
/// let s = String::from("Hello");
///
/// takes_str(&s);
/// ```
///
/// This will create a [`&str`] from the `String` and pass it in. This
/// conversion is very inexpensive, and so generally, functions will accept
/// [`&str`]s as arguments unless they need a `String` for some specific
/// reason.
///
/// In certain cases Rust doesn't have enough information to make this
/// conversion, known as [`Deref`] coercion. In the following example a string
/// slice [`&'a str`][`&str`] implements the trait `TraitExample`, and the function
/// `example_func` takes anything that implements the trait. In this case Rust
/// would need to make two implicit conversions, which Rust doesn't have the
/// means to do. For that reason, the following example will not compile.
///
/// ```compile_fail,E0277
/// trait TraitExample {}
///
/// impl<'a> TraitExample for &'a str {}
///
/// fn example_func<A: TraitExample>(example_arg: A) {}
///
/// let example_string = String::from("example_string");
/// example_func(&example_string);
/// ```
///
/// There are two options that would work instead. The first would be to
/// change the line `example_func(&example_string);` to
/// `example_func(example_string.as_str());`, using the method [`as_str()`]
/// to explicitly extract the string slice containing the string. The second
/// way changes `example_func(&example_string);` to
/// `example_func(&*example_string);`. In this case we are dereferencing a
/// `String` to a [`str`][`&str`], then referencing the [`str`][`&str`] back to
/// [`&str`]. The second way is more idiomatic, however both work to do the
/// conversion explicitly rather than relying on the implicit conversion.
///
/// # Representation
///
/// A `String` is made up of three components: a pointer to some bytes, a
/// length, and a capacity. The pointer points to an internal buffer `String`
/// uses to store its data. The length is the number of bytes currently stored
/// in the buffer, and the capacity is the size of the buffer in bytes. As such,
/// the length will always be less than or equal to the capacity.
///
/// This buffer is always stored on the heap.
///
/// You can look at these with the [`as_ptr`], [`len`], and [`capacity`]
/// methods:
///
/// ```
/// use std::mem;
///
/// let story = String::from("Once upon a time...");
///
// FIXME Update this when vec_into_raw_parts is stabilized
/// // Prevent automatically dropping the String's data
/// let mut story = mem::ManuallyDrop::new(story);
///
/// let ptr = story.as_mut_ptr();
/// let len = story.len();
/// let capacity = story.capacity();
///
/// // story has nineteen bytes
/// assert_eq!(19, len);
///
/// // We can re-build a String out of ptr, len, and capacity. This is all
/// // unsafe because we are responsible for making sure the components are
/// // valid:
/// let s = unsafe { String::from_raw_parts(ptr, len, capacity) } ;
///
/// assert_eq!(String::from("Once upon a time..."), s);
/// ```
///
/// [`as_ptr`]: str::as_ptr
/// [`len`]: String::len
/// [`capacity`]: String::capacity
///
/// If a `String` has enough capacity, adding elements to it will not
/// re-allocate. For example, consider this program:
///
/// ```
/// let mut s = String::new();
///
/// println!("{}", s.capacity());
///
/// for _ in 0..5 {
///     s.push_str("hello");
///     println!("{}", s.capacity());
/// }
/// ```
///
/// This will output the following:
///
/// ```text
/// 0
/// 5
/// 10
/// 20
/// 20
/// 40
/// ```
///
/// At first, we have no memory allocated at all, but as we append to the
/// string, it increases its capacity appropriately. If we instead use the
/// [`with_capacity`] method to allocate the correct capacity initially:
///
/// ```
/// let mut s = String::with_capacity(25);
///
/// println!("{}", s.capacity());
///
/// for _ in 0..5 {
///     s.push_str("hello");
///     println!("{}", s.capacity());
/// }
/// ```
///
/// [`with_capacity`]: String::with_capacity
///
/// We end up with a different output:
///
/// ```text
/// 25
/// 25
/// 25
/// 25
/// 25
/// 25
/// ```
///
/// Here, there's no need to allocate more memory inside the loop.
///
/// [`str`]: prim@str
/// [`&str`]: prim@str
/// [`Deref`]: core::ops::Deref
/// [`as_str()`]: String::as_str
#[derive(PartialOrd, Eq, Ord)]
#[cfg_attr(not(test), rustc_diagnostic_item = "string_type")]
#[stable(feature = "rust1", since = "1.0.0")]
pub struct String {
    vec: Vec<u8>,
}

/// A possible error value when converting a `String` from a UTF-8 byte vector.
///
/// This type is the error type for the [`from_utf8`] method on [`String`]. It
/// is designed in such a way to carefully avoid reallocations: the
/// [`into_bytes`] method will give back the byte vector that was used in the
/// conversion attempt.
///
/// [`from_utf8`]: String::from_utf8
/// [`into_bytes`]: FromUtf8Error::into_bytes
///
/// The [`Utf8Error`] type provided by [`std::str`] represents an error that may
/// occur when converting a slice of [`u8`]s to a [`&str`]. In this sense, it's
/// an analogue to `FromUtf8Error`, and you can get one from a `FromUtf8Error`
/// through the [`utf8_error`] method.
///
/// [`Utf8Error`]: core::str::Utf8Error
/// [`std::str`]: core::str
/// [`&str`]: prim@str
/// [`utf8_error`]: Self::utf8_error
///
/// # Examples
///
/// Basic usage:
///
/// ```
/// // some invalid bytes, in a vector
/// let bytes = vec![0, 159];
///
/// let value = String::from_utf8(bytes);
///
/// assert!(value.is_err());
/// assert_eq!(vec![0, 159], value.unwrap_err().into_bytes());
/// ```
#[stable(feature = "rust1", since = "1.0.0")]
#[cfg_attr(not(no_global_oom_handling), derive(Clone))]
#[derive(Debug, PartialEq, Eq)]
pub struct FromUtf8Error {
    bytes: Vec<u8>,
    error: Utf8Error,
}

/// A possible error value when converting a `String` from a UTF-16 byte slice.
///
/// This type is the error type for the [`from_utf16`] method on [`String`].
///
/// [`from_utf16`]: String::from_utf16
/// # Examples
///
/// Basic usage:
///
/// ```
/// // 𝄞mu<invalid>ic
/// let v = &[0xD834, 0xDD1E, 0x006d, 0x0075,
///           0xD800, 0x0069, 0x0063];
///
/// assert!(String::from_utf16(v).is_err());
/// ```
#[stable(feature = "rust1", since = "1.0.0")]
#[derive(Debug)]
pub struct FromUtf16Error(());

impl String {
    /// Creates a new empty `String`.
    ///
    /// Given that the `String` is empty, this will not allocate any initial
    /// buffer. While that means that this initial operation is very
    /// inexpensive, it may cause excessive allocation later when you add
    /// data. If you have an idea of how much data the `String` will hold,
    /// consider the [`with_capacity`] method to prevent excessive
    /// re-allocation.
    ///
    /// [`with_capacity`]: String::with_capacity
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// let s = String::new();
    /// ```
    #[inline]
    #[rustc_const_stable(feature = "const_string_new", since = "1.39.0")]
    #[stable(feature = "rust1", since = "1.0.0")]
    pub const fn new() -> String {
}

    /// Creates a new empty `String` with a particular capacity.
    ///
    /// `String`s have an internal buffer to hold their data. The capacity is
    /// the length of that buffer, and can be queried with the [`capacity`]
    /// method. This method creates an empty `String`, but one with an initial
    /// buffer that can hold `capacity` bytes. This is useful when you may be
    /// appending a bunch of data to the `String`, reducing the number of
    /// reallocations it needs to do.
    ///
    /// [`capacity`]: String::capacity
    ///
    /// If the given capacity is `0`, no allocation will occur, and this method
    /// is identical to the [`new`] method.
    ///
    /// [`new`]: String::new
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// let mut s = String::with_capacity(10);
    ///
    /// // The String contains no chars, even though it has capacity for more
    /// assert_eq!(s.len(), 0);
    ///
    /// // These are all done without reallocating...
    /// let cap = s.capacity();
    /// for _ in 0..10 {
    ///     s.push('a');
    /// }
    ///
    /// assert_eq!(s.capacity(), cap);
    ///
    /// // ...but this may make the string reallocate
    /// s.push('a');
    /// ```
    #[cfg(not(no_global_oom_handling))]
    #[inline]
    #[stable(feature = "rust1", since = "1.0.0")]
    pub fn with_capacity(capacity: usize) -> String {
}

    // HACK(japaric): with cfg(test) the inherent `[T]::to_vec` method, which is
    // required for this method definition, is not available. Since we don't
    // require this method for testing purposes, I'll just stub it
    // NB see the slice::hack module in slice.rs for more information
    #[inline]
    #[cfg(test)]
    pub fn from_str(_: &str) -> String {
}

    /// Converts a vector of bytes to a `String`.
    ///
    /// A string ([`String`]) is made of bytes ([`u8`]), and a vector of bytes
    /// ([`Vec<u8>`]) is made of bytes, so this function converts between the
    /// two. Not all byte slices are valid `String`s, however: `String`
    /// requires that it is valid UTF-8. `from_utf8()` checks to ensure that
    /// the bytes are valid UTF-8, and then does the conversion.
    ///
    /// If you are sure that the byte slice is valid UTF-8, and you don't want
    /// to incur the overhead of the validity check, there is an unsafe version
    /// of this function, [`from_utf8_unchecked`], which has the same behavior
    /// but skips the check.
    ///
    /// This method will take care to not copy the vector, for efficiency's
    /// sake.
    ///
    /// If you need a [`&str`] instead of a `String`, consider
    /// [`str::from_utf8`].
    ///
    /// The inverse of this method is [`into_bytes`].
    ///
    /// # Errors
    ///
    /// Returns [`Err`] if the slice is not UTF-8 with a description as to why the
    /// provided bytes are not UTF-8. The vector you moved in is also included.
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// // some bytes, in a vector
    /// let sparkle_heart = vec![240, 159, 146, 150];
    ///
    /// // We know these bytes are valid, so we'll use `unwrap()`.
    /// let sparkle_heart = String::from_utf8(sparkle_heart).unwrap();
    ///
    /// assert_eq!("💖", sparkle_heart);
    /// ```
    ///
    /// Incorrect bytes:
    ///
    /// ```
    /// // some invalid bytes, in a vector
    /// let sparkle_heart = vec![0, 159, 146, 150];
    ///
    /// assert!(String::from_utf8(sparkle_heart).is_err());
    /// ```
    ///
    /// See the docs for [`FromUtf8Error`] for more details on what you can do
    /// with this error.
    ///
    /// [`from_utf8_unchecked`]: String::from_utf8_unchecked
    /// [`Vec<u8>`]: crate::vec::Vec
    /// [`&str`]: prim@str
    /// [`into_bytes`]: String::into_bytes
    #[inline]
    #[stable(feature = "rust1", since = "1.0.0")]
    pub fn from_utf8(vec: Vec<u8>) -> Result<String, FromUtf8Error> {
}

    /// Converts a slice of bytes to a string, including invalid characters.
    ///
    /// Strings are made of bytes ([`u8`]), and a slice of bytes
    /// ([`&[u8]`][byteslice]) is made of bytes, so this function converts
    /// between the two. Not all byte slices are valid strings, however: strings
    /// are required to be valid UTF-8. During this conversion,
    /// `from_utf8_lossy()` will replace any invalid UTF-8 sequences with
    /// [`U+FFFD REPLACEMENT CHARACTER`][U+FFFD], which looks like this: �
    ///
    /// [byteslice]: prim@slice
    /// [U+FFFD]: core::char::REPLACEMENT_CHARACTER
    ///
    /// If you are sure that the byte slice is valid UTF-8, and you don't want
    /// to incur the overhead of the conversion, there is an unsafe version
    /// of this function, [`from_utf8_unchecked`], which has the same behavior
    /// but skips the checks.
    ///
    /// [`from_utf8_unchecked`]: String::from_utf8_unchecked
    ///
    /// This function returns a [`Cow<'a, str>`]. If our byte slice is invalid
    /// UTF-8, then we need to insert the replacement characters, which will
    /// change the size of the string, and hence, require a `String`. But if
    /// it's already valid UTF-8, we don't need a new allocation. This return
    /// type allows us to handle both cases.
    ///
    /// [`Cow<'a, str>`]: crate::borrow::Cow
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// // some bytes, in a vector
    /// let sparkle_heart = vec![240, 159, 146, 150];
    ///
    /// let sparkle_heart = String::from_utf8_lossy(&sparkle_heart);
    ///
    /// assert_eq!("💖", sparkle_heart);
    /// ```
    ///
    /// Incorrect bytes:
    ///
    /// ```
    /// // some invalid bytes
    /// let input = b"Hello \xF0\x90\x80World";
    /// let output = String::from_utf8_lossy(input);
    ///
    /// assert_eq!("Hello �World", output);
    /// ```
    #[cfg(not(no_global_oom_handling))]
    #[stable(feature = "rust1", since = "1.0.0")]
    pub fn from_utf8_lossy(v: &[u8]) -> Cow<'_, str> {
}

    /// Decode a UTF-16–encoded vector `v` into a `String`, returning [`Err`]
    /// if `v` contains any invalid data.
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// // 𝄞music
    /// let v = &[0xD834, 0xDD1E, 0x006d, 0x0075,
    ///           0x0073, 0x0069, 0x0063];
    /// assert_eq!(String::from("𝄞music"),
    ///            String::from_utf16(v).unwrap());
    ///
    /// // 𝄞mu<invalid>ic
    /// let v = &[0xD834, 0xDD1E, 0x006d, 0x0075,
    ///           0xD800, 0x0069, 0x0063];
    /// assert!(String::from_utf16(v).is_err());
    /// ```
    #[cfg(not(no_global_oom_handling))]
    #[stable(feature = "rust1", since = "1.0.0")]
    pub fn from_utf16(v: &[u16]) -> Result<String, FromUtf16Error> {
}

    /// Decode a UTF-16–encoded slice `v` into a `String`, replacing
    /// invalid data with [the replacement character (`U+FFFD`)][U+FFFD].
    ///
    /// Unlike [`from_utf8_lossy`] which returns a [`Cow<'a, str>`],
    /// `from_utf16_lossy` returns a `String` since the UTF-16 to UTF-8
    /// conversion requires a memory allocation.
    ///
    /// [`from_utf8_lossy`]: String::from_utf8_lossy
    /// [`Cow<'a, str>`]: crate::borrow::Cow
    /// [U+FFFD]: core::char::REPLACEMENT_CHARACTER
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// // 𝄞mus<invalid>ic<invalid>
    /// let v = &[0xD834, 0xDD1E, 0x006d, 0x0075,
    ///           0x0073, 0xDD1E, 0x0069, 0x0063,
    ///           0xD834];
    ///
    /// assert_eq!(String::from("𝄞mus\u{FFFD}ic\u{FFFD}"),
    ///            String::from_utf16_lossy(v));
    /// ```
    #[cfg(not(no_global_oom_handling))]
    #[inline]
    #[stable(feature = "rust1", since = "1.0.0")]
    pub fn from_utf16_lossy(v: &[u16]) -> String {
}

    /// Decomposes a `String` into its raw components.
    ///
    /// Returns the raw pointer to the underlying data, the length of
    /// the string (in bytes), and the allocated capacity of the data
    /// (in bytes). These are the same arguments in the same order as
    /// the arguments to [`from_raw_parts`].
    ///
    /// After calling this function, the caller is responsible for the
    /// memory previously managed by the `String`. The only way to do
    /// this is to convert the raw pointer, length, and capacity back
    /// into a `String` with the [`from_raw_parts`] function, allowing
    /// the destructor to perform the cleanup.
    ///
    /// [`from_raw_parts`]: String::from_raw_parts
    ///
    /// # Examples
    ///
    /// ```
    /// #![feature(vec_into_raw_parts)]
    /// let s = String::from("hello");
    ///
    /// let (ptr, len, cap) = s.into_raw_parts();
    ///
    /// let rebuilt = unsafe { String::from_raw_parts(ptr, len, cap) };
    /// assert_eq!(rebuilt, "hello");
    /// ```
    #[unstable(feature = "vec_into_raw_parts", reason = "new API", issue = "65816")]
    pub fn into_raw_parts(self) -> (*mut u8, usize, usize) {
}

    /// Creates a new `String` from a length, capacity, and pointer.
    ///
    /// # Safety
    ///
    /// This is highly unsafe, due to the number of invariants that aren't
    /// checked:
    ///
    /// * The memory at `buf` needs to have been previously allocated by the
    ///   same allocator the standard library uses, with a required alignment of exactly 1.
    /// * `length` needs to be less than or equal to `capacity`.
    /// * `capacity` needs to be the correct value.
    /// * The first `length` bytes at `buf` need to be valid UTF-8.
    ///
    /// Violating these may cause problems like corrupting the allocator's
    /// internal data structures.
    ///
    /// The ownership of `buf` is effectively transferred to the
    /// `String` which may then deallocate, reallocate or change the
    /// contents of memory pointed to by the pointer at will. Ensure
    /// that nothing else uses the pointer after calling this
    /// function.
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// use std::mem;
    ///
    /// unsafe {
    ///     let s = String::from("hello");
    ///
    // FIXME Update this when vec_into_raw_parts is stabilized
    ///     // Prevent automatically dropping the String's data
    ///     let mut s = mem::ManuallyDrop::new(s);
    ///
    ///     let ptr = s.as_mut_ptr();
    ///     let len = s.len();
    ///     let capacity = s.capacity();
    ///
    ///     let s = String::from_raw_parts(ptr, len, capacity);
    ///
    ///     assert_eq!(String::from("hello"), s);
    /// }
    /// ```
    #[inline]
    #[stable(feature = "rust1", since = "1.0.0")]
    pub unsafe fn from_raw_parts(buf: *mut u8, length: usize, capacity: usize) -> String {
}

    /// Converts a vector of bytes to a `String` without checking that the
    /// string contains valid UTF-8.
    ///
    /// See the safe version, [`from_utf8`], for more details.
    ///
    /// [`from_utf8`]: String::from_utf8
    ///
    /// # Safety
    ///
    /// This function is unsafe because it does not check that the bytes passed
    /// to it are valid UTF-8. If this constraint is violated, it may cause
    /// memory unsafety issues with future users of the `String`, as the rest of
    /// the standard library assumes that `String`s are valid UTF-8.
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// // some bytes, in a vector
    /// let sparkle_heart = vec![240, 159, 146, 150];
    ///
    /// let sparkle_heart = unsafe {
    ///     String::from_utf8_unchecked(sparkle_heart)
    /// };
    ///
    /// assert_eq!("💖", sparkle_heart);
    /// ```
    #[inline]
    #[stable(feature = "rust1", since = "1.0.0")]
    pub unsafe fn from_utf8_unchecked(bytes: Vec<u8>) -> String {
}

    /// Converts a `String` into a byte vector.
    ///
    /// This consumes the `String`, so we do not need to copy its contents.
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// let s = String::from("hello");
    /// let bytes = s.into_bytes();
    ///
    /// assert_eq!(&[104, 101, 108, 108, 111][..], &bytes[..]);
    /// ```
    #[inline]
    #[stable(feature = "rust1", since = "1.0.0")]
    pub fn into_bytes(self) -> Vec<u8> {
}

    /// Extracts a string slice containing the entire `String`.
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// let s = String::from("foo");
    ///
    /// assert_eq!("foo", s.as_str());
    /// ```
    #[inline]
    #[stable(feature = "string_as_str", since = "1.7.0")]
    pub fn as_str(&self) -> &str {
}

    /// Converts a `String` into a mutable string slice.
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// let mut s = String::from("foobar");
    /// let s_mut_str = s.as_mut_str();
    ///
    /// s_mut_str.make_ascii_uppercase();
    ///
    /// assert_eq!("FOOBAR", s_mut_str);
    /// ```
    #[inline]
    #[stable(feature = "string_as_str", since = "1.7.0")]
    pub fn as_mut_str(&mut self) -> &mut str {
}

    /// Appends a given string slice onto the end of this `String`.
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// let mut s = String::from("foo");
    ///
    /// s.push_str("bar");
    ///
    /// assert_eq!("foobar", s);
    /// ```
    #[cfg(not(no_global_oom_handling))]
    #[inline]
    #[stable(feature = "rust1", since = "1.0.0")]
    pub fn push_str(&mut self, string: &str) {
}

    /// Copies elements from `src` range to the end of the string.
    ///
    /// ## Panics
    ///
    /// Panics if the starting point or end point do not lie on a [`char`]
    /// boundary, or if they're out of bounds.
    ///
    /// ## Examples
    ///
    /// ```
    /// #![feature(string_extend_from_within)]
    /// let mut string = String::from("abcde");
    ///
    /// string.extend_from_within(2..);
    /// assert_eq!(string, "abcdecde");
    ///
    /// string.extend_from_within(..2);
    /// assert_eq!(string, "abcdecdeab");
    ///
    /// string.extend_from_within(4..8);
    /// assert_eq!(string, "abcdecdeabecde");
    /// ```
    #[cfg(not(no_global_oom_handling))]
    #[unstable(feature = "string_extend_from_within", issue = "none")]
    pub fn extend_from_within<R>(&mut self, src: R)
    where
        R: RangeBounds<usize>,
    {
}

    /// Returns this `String`'s capacity, in bytes.
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// let s = String::with_capacity(10);
    ///
    /// assert!(s.capacity() >= 10);
    /// ```
    #[inline]
    #[stable(feature = "rust1", since = "1.0.0")]
    pub fn capacity(&self) -> usize {
}

    /// Ensures that this `String`'s capacity is at least `additional` bytes
    /// larger than its length.
    ///
    /// The capacity may be increased by more than `additional` bytes if it
    /// chooses, to prevent frequent reallocations.
    ///
    /// If you do not want this "at least" behavior, see the [`reserve_exact`]
    /// method.
    ///
    /// # Panics
    ///
    /// Panics if the new capacity overflows [`usize`].
    ///
    /// [`reserve_exact`]: String::reserve_exact
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// let mut s = String::new();
    ///
    /// s.reserve(10);
    ///
    /// assert!(s.capacity() >= 10);
    /// ```
    ///
    /// This might not actually increase the capacity:
    ///
    /// ```
    /// let mut s = String::with_capacity(10);
    /// s.push('a');
    /// s.push('b');
    ///
    /// // s now has a length of 2 and a capacity of 10
    /// assert_eq!(2, s.len());
    /// assert_eq!(10, s.capacity());
    ///
    /// // Since we already have an extra 8 capacity, calling this...
    /// s.reserve(8);
    ///
    /// // ... doesn't actually increase.
    /// assert_eq!(10, s.capacity());
    /// ```
    #[cfg(not(no_global_oom_handling))]
    #[inline]
    #[stable(feature = "rust1", since = "1.0.0")]
    pub fn reserve(&mut self, additional: usize) {
}

    /// Ensures that this `String`'s capacity is `additional` bytes
    /// larger than its length.
    ///
    /// Consider using the [`reserve`] method unless you absolutely know
    /// better than the allocator.
    ///
    /// [`reserve`]: String::reserve
    ///
    /// # Panics
    ///
    /// Panics if the new capacity overflows `usize`.
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// let mut s = String::new();
    ///
    /// s.reserve_exact(10);
    ///
    /// assert!(s.capacity() >= 10);
    /// ```
    ///
    /// This might not actually increase the capacity:
    ///
    /// ```
    /// let mut s = String::with_capacity(10);
    /// s.push('a');
    /// s.push('b');
    ///
    /// // s now has a length of 2 and a capacity of 10
    /// assert_eq!(2, s.len());
    /// assert_eq!(10, s.capacity());
    ///
    /// // Since we already have an extra 8 capacity, calling this...
    /// s.reserve_exact(8);
    ///
    /// // ... doesn't actually increase.
    /// assert_eq!(10, s.capacity());
    /// ```
    #[cfg(not(no_global_oom_handling))]
    #[inline]
    #[stable(feature = "rust1", since = "1.0.0")]
    pub fn reserve_exact(&mut self, additional: usize) {
}

    /// Tries to reserve capacity for at least `additional` more elements to be inserted
    /// in the given `String`. The collection may reserve more space to avoid
    /// frequent reallocations. After calling `reserve`, capacity will be
    /// greater than or equal to `self.len() + additional`. Does nothing if
    /// capacity is already sufficient.
    ///
    /// # Errors
    ///
    /// If the capacity overflows, or the allocator reports a failure, then an error
    /// is returned.
    ///
    /// # Examples
    ///
    /// ```
    /// #![feature(try_reserve)]
    /// use std::collections::TryReserveError;
    ///
    /// fn process_data(data: &str) -> Result<String, TryReserveError> {
    ///     let mut output = String::new();
    ///
    ///     // Pre-reserve the memory, exiting if we can't
    ///     output.try_reserve(data.len())?;
    ///
    ///     // Now we know this can't OOM in the middle of our complex work
    ///     output.push_str(data);
    ///
    ///     Ok(output)
    /// }
    /// # process_data("rust").expect("why is the test harness OOMing on 4 bytes?");
    /// ```
    #[unstable(feature = "try_reserve", reason = "new API", issue = "48043")]
    pub fn try_reserve(&mut self, additional: usize) -> Result<(), TryReserveError> {
}

    /// Tries to reserve the minimum capacity for exactly `additional` more elements to
    /// be inserted in the given `String`. After calling `reserve_exact`,
    /// capacity will be greater than or equal to `self.len() + additional`.
    /// Does nothing if the capacity is already sufficient.
    ///
    /// Note that the allocator may give the collection more space than it
    /// requests. Therefore, capacity can not be relied upon to be precisely
    /// minimal. Prefer [`reserve`] if future insertions are expected.
    ///
    /// [`reserve`]: String::reserve
    ///
    /// # Errors
    ///
    /// If the capacity overflows, or the allocator reports a failure, then an error
    /// is returned.
    ///
    /// # Examples
    ///
    /// ```
    /// #![feature(try_reserve)]
    /// use std::collections::TryReserveError;
    ///
    /// fn process_data(data: &str) -> Result<String, TryReserveError> {
    ///     let mut output = String::new();
    ///
    ///     // Pre-reserve the memory, exiting if we can't
    ///     output.try_reserve(data.len())?;
    ///
    ///     // Now we know this can't OOM in the middle of our complex work
    ///     output.push_str(data);
    ///
    ///     Ok(output)
    /// }
    /// # process_data("rust").expect("why is the test harness OOMing on 4 bytes?");
    /// ```
    #[unstable(feature = "try_reserve", reason = "new API", issue = "48043")]
    pub fn try_reserve_exact(&mut self, additional: usize) -> Result<(), TryReserveError> {
}

    /// Shrinks the capacity of this `String` to match its length.
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// let mut s = String::from("foo");
    ///
    /// s.reserve(100);
    /// assert!(s.capacity() >= 100);
    ///
    /// s.shrink_to_fit();
    /// assert_eq!(3, s.capacity());
    /// ```
    #[cfg(not(no_global_oom_handling))]
    #[inline]
    #[stable(feature = "rust1", since = "1.0.0")]
    pub fn shrink_to_fit(&mut self) {
}

    /// Shrinks the capacity of this `String` with a lower bound.
    ///
    /// The capacity will remain at least as large as both the length
    /// and the supplied value.
    ///
    /// If the current capacity is less than the lower limit, this is a no-op.
    ///
    /// # Examples
    ///
    /// ```
    /// let mut s = String::from("foo");
    ///
    /// s.reserve(100);
    /// assert!(s.capacity() >= 100);
    ///
    /// s.shrink_to(10);
    /// assert!(s.capacity() >= 10);
    /// s.shrink_to(0);
    /// assert!(s.capacity() >= 3);
    /// ```
    #[cfg(not(no_global_oom_handling))]
    #[inline]
    #[stable(feature = "shrink_to", since = "1.56.0")]
    pub fn shrink_to(&mut self, min_capacity: usize) {
}

    /// Appends the given [`char`] to the end of this `String`.
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// let mut s = String::from("abc");
    ///
    /// s.push('1');
    /// s.push('2');
    /// s.push('3');
    ///
    /// assert_eq!("abc123", s);
    /// ```
    #[cfg(not(no_global_oom_handling))]
    #[inline]
    #[stable(feature = "rust1", since = "1.0.0")]
    pub fn push(&mut self, ch: char) {
}

    /// Returns a byte slice of this `String`'s contents.
    ///
    /// The inverse of this method is [`from_utf8`].
    ///
    /// [`from_utf8`]: String::from_utf8
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// let s = String::from("hello");
    ///
    /// assert_eq!(&[104, 101, 108, 108, 111], s.as_bytes());
    /// ```
    #[inline]
    #[stable(feature = "rust1", since = "1.0.0")]
    pub fn as_bytes(&self) -> &[u8] {
}

    /// Shortens this `String` to the specified length.
    ///
    /// If `new_len` is greater than the string's current length, this has no
    /// effect.
    ///
    /// Note that this method has no effect on the allocated capacity
    /// of the string
    ///
    /// # Panics
    ///
    /// Panics if `new_len` does not lie on a [`char`] boundary.
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// let mut s = String::from("hello");
    ///
    /// s.truncate(2);
    ///
    /// assert_eq!("he", s);
    /// ```
    #[inline]
    #[stable(feature = "rust1", since = "1.0.0")]
    pub fn truncate(&mut self, new_len: usize) {
}

    /// Removes the last character from the string buffer and returns it.
    ///
    /// Returns [`None`] if this `String` is empty.
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// let mut s = String::from("foo");
    ///
    /// assert_eq!(s.pop(), Some('o'));
    /// assert_eq!(s.pop(), Some('o'));
    /// assert_eq!(s.pop(), Some('f'));
    ///
    /// assert_eq!(s.pop(), None);
    /// ```
    #[inline]
    #[stable(feature = "rust1", since = "1.0.0")]
    pub fn pop(&mut self) -> Option<char> {
}

    /// Removes a [`char`] from this `String` at a byte position and returns it.
    ///
    /// This is an *O*(*n*) operation, as it requires copying every element in the
    /// buffer.
    ///
    /// # Panics
    ///
    /// Panics if `idx` is larger than or equal to the `String`'s length,
    /// or if it does not lie on a [`char`] boundary.
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// let mut s = String::from("foo");
    ///
    /// assert_eq!(s.remove(0), 'f');
    /// assert_eq!(s.remove(1), 'o');
    /// assert_eq!(s.remove(0), 'o');
    /// ```
    #[inline]
    #[stable(feature = "rust1", since = "1.0.0")]
    pub fn remove(&mut self, idx: usize) -> char {
}

    /// Remove all matches of pattern `pat` in the `String`.
    ///
    /// # Examples
    ///
    /// ```
    /// #![feature(string_remove_matches)]
    /// let mut s = String::from("Trees are not green, the sky is not blue.");
    /// s.remove_matches("not ");
    /// assert_eq!("Trees are green, the sky is blue.", s);
    /// ```
    ///
    /// Matches will be detected and removed iteratively, so in cases where
    /// patterns overlap, only the first pattern will be removed:
    ///
    /// ```
    /// #![feature(string_remove_matches)]
    /// let mut s = String::from("banana");
    /// s.remove_matches("ana");
    /// assert_eq!("bna", s);
    /// ```
    #[cfg(not(no_global_oom_handling))]
    #[unstable(feature = "string_remove_matches", reason = "new API", issue = "72826")]
    pub fn remove_matches<'a, P>(&'a mut self, pat: P)
    where
        P: for<'x> Pattern<'x>,
    {
}

    /// Retains only the characters specified by the predicate.
    ///
    /// In other words, remove all characters `c` such that `f(c)` returns `false`.
    /// This method operates in place, visiting each character exactly once in the
    /// original order, and preserves the order of the retained characters.
    ///
    /// # Examples
    ///
    /// ```
    /// let mut s = String::from("f_o_ob_ar");
    ///
    /// s.retain(|c| c != '_');
    ///
    /// assert_eq!(s, "foobar");
    /// ```
    ///
    /// Because the elements are visited exactly once in the original order,
    /// external state may be used to decide which elements to keep.
    ///
    /// ```
    /// let mut s = String::from("abcde");
    /// let keep = [false, true, true, false, true];
    /// let mut iter = keep.iter();
    /// s.retain(|_| *iter.next().unwrap());
    /// assert_eq!(s, "bce");
    /// ```
    #[inline]
    #[stable(feature = "string_retain", since = "1.26.0")]
    pub fn retain<F>(&mut self, mut f: F)
    where
        F: FnMut(char) -> bool,
    {
}

    /// Inserts a character into this `String` at a byte position.
    ///
    /// This is an *O*(*n*) operation as it requires copying every element in the
    /// buffer.
    ///
    /// # Panics
    ///
    /// Panics if `idx` is larger than the `String`'s length, or if it does not
    /// lie on a [`char`] boundary.
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// let mut s = String::with_capacity(3);
    ///
    /// s.insert(0, 'f');
    /// s.insert(1, 'o');
    /// s.insert(2, 'o');
    ///
    /// assert_eq!("foo", s);
    /// ```
    #[cfg(not(no_global_oom_handling))]
    #[inline]
    #[stable(feature = "rust1", since = "1.0.0")]
    pub fn insert(&mut self, idx: usize, ch: char) {
}

    #[cfg(not(no_global_oom_handling))]
    unsafe fn insert_bytes(&mut self, idx: usize, bytes: &[u8]) {
}

    /// Inserts a string slice into this `String` at a byte position.
    ///
    /// This is an *O*(*n*) operation as it requires copying every element in the
    /// buffer.
    ///
    /// # Panics
    ///
    /// Panics if `idx` is larger than the `String`'s length, or if it does not
    /// lie on a [`char`] boundary.
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// let mut s = String::from("bar");
    ///
    /// s.insert_str(0, "foo");
    ///
    /// assert_eq!("foobar", s);
    /// ```
    #[cfg(not(no_global_oom_handling))]
    #[inline]
    #[stable(feature = "insert_str", since = "1.16.0")]
    pub fn insert_str(&mut self, idx: usize, string: &str) {
}

    /// Returns a mutable reference to the contents of this `String`.
    ///
    /// # Safety
    ///
    /// This function is unsafe because it does not check that the bytes passed
    /// to it are valid UTF-8. If this constraint is violated, it may cause
    /// memory unsafety issues with future users of the `String`, as the rest of
    /// the standard library assumes that `String`s are valid UTF-8.
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// let mut s = String::from("hello");
    ///
    /// unsafe {
    ///     let vec = s.as_mut_vec();
    ///     assert_eq!(&[104, 101, 108, 108, 111][..], &vec[..]);
    ///
    ///     vec.reverse();
    /// }
    /// assert_eq!(s, "olleh");
    /// ```
    #[inline]
    #[stable(feature = "rust1", since = "1.0.0")]
    pub unsafe fn as_mut_vec(&mut self) -> &mut Vec<u8> {
}

    /// Returns the length of this `String`, in bytes, not [`char`]s or
    /// graphemes. In other words, it might not be what a human considers the
    /// length of the string.
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// let a = String::from("foo");
    /// assert_eq!(a.len(), 3);
    ///
    /// let fancy_f = String::from("ƒoo");
    /// assert_eq!(fancy_f.len(), 4);
    /// assert_eq!(fancy_f.chars().count(), 3);
    /// ```
    #[inline]
    #[stable(feature = "rust1", since = "1.0.0")]
    pub fn len(&self) -> usize {
}

    /// Returns `true` if this `String` has a length of zero, and `false` otherwise.
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// let mut v = String::new();
    /// assert!(v.is_empty());
    ///
    /// v.push('a');
    /// assert!(!v.is_empty());
    /// ```
    #[inline]
    #[stable(feature = "rust1", since = "1.0.0")]
    pub fn is_empty(&self) -> bool {
}

    /// Splits the string into two at the given byte index.
    ///
    /// Returns a newly allocated `String`. `self` contains bytes `[0, at)`, and
    /// the returned `String` contains bytes `[at, len)`. `at` must be on the
    /// boundary of a UTF-8 code point.
    ///
    /// Note that the capacity of `self` does not change.
    ///
    /// # Panics
    ///
    /// Panics if `at` is not on a `UTF-8` code point boundary, or if it is beyond the last
    /// code point of the string.
    ///
    /// # Examples
    ///
    /// ```
    /// # fn main() {
    /// let mut hello = String::from("Hello, World!");
    /// let world = hello.split_off(7);
    /// assert_eq!(hello, "Hello, ");
    /// assert_eq!(world, "World!");
    /// # }
    /// ```
    #[cfg(not(no_global_oom_handling))]
    #[inline]
    #[stable(feature = "string_split_off", since = "1.16.0")]
    #[must_use = "use `.truncate()` if you don't need the other half"]
    pub fn split_off(&mut self, at: usize) -> String {
}

    /// Truncates this `String`, removing all contents.
    ///
    /// While this means the `String` will have a length of zero, it does not
    /// touch its capacity.
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// let mut s = String::from("foo");
    ///
    /// s.clear();
    ///
    /// assert!(s.is_empty());
    /// assert_eq!(0, s.len());
    /// assert_eq!(3, s.capacity());
    /// ```
    #[inline]
    #[stable(feature = "rust1", since = "1.0.0")]
    pub fn clear(&mut self) {
}

    /// Creates a draining iterator that removes the specified range in the `String`
    /// and yields the removed `chars`.
    ///
    /// Note: The element range is removed even if the iterator is not
    /// consumed until the end.
    ///
    /// # Panics
    ///
    /// Panics if the starting point or end point do not lie on a [`char`]
    /// boundary, or if they're out of bounds.
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// let mut s = String::from("α is alpha, β is beta");
    /// let beta_offset = s.find('β').unwrap_or(s.len());
    ///
    /// // Remove the range up until the β from the string
    /// let t: String = s.drain(..beta_offset).collect();
    /// assert_eq!(t, "α is alpha, ");
    /// assert_eq!(s, "β is beta");
    ///
    /// // A full range clears the string
    /// s.drain(..);
    /// assert_eq!(s, "");
    /// ```
    #[stable(feature = "drain", since = "1.6.0")]
    pub fn drain<R>(&mut self, range: R) -> Drain<'_>
    where
        R: RangeBounds<usize>,
    {
}

    /// Removes the specified range in the string,
    /// and replaces it with the given string.
    /// The given string doesn't need to be the same length as the range.
    ///
    /// # Panics
    ///
    /// Panics if the starting point or end point do not lie on a [`char`]
    /// boundary, or if they're out of bounds.
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// let mut s = String::from("α is alpha, β is beta");
    /// let beta_offset = s.find('β').unwrap_or(s.len());
    ///
    /// // Replace the range up until the β from the string
    /// s.replace_range(..beta_offset, "Α is capital alpha; ");
    /// assert_eq!(s, "Α is capital alpha; β is beta");
    /// ```
    #[cfg(not(no_global_oom_handling))]
    #[stable(feature = "splice", since = "1.27.0")]
    pub fn replace_range<R>(&mut self, range: R, replace_with: &str)
    where
        R: RangeBounds<usize>,
    {
}

    /// Converts this `String` into a [`Box`]`<`[`str`]`>`.
    ///
    /// This will drop any excess capacity.
    ///
    /// [`str`]: prim@str
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// let s = String::from("hello");
    ///
    /// let b = s.into_boxed_str();
    /// ```
    #[cfg(not(no_global_oom_handling))]
    #[stable(feature = "box_str", since = "1.4.0")]
    #[inline]
    pub fn into_boxed_str(self) -> Box<str> {
}
}

impl FromUtf8Error {
    /// Returns a slice of [`u8`]s bytes that were attempted to convert to a `String`.
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// // some invalid bytes, in a vector
    /// let bytes = vec![0, 159];
    ///
    /// let value = String::from_utf8(bytes);
    ///
    /// assert_eq!(&[0, 159], value.unwrap_err().as_bytes());
    /// ```
    #[stable(feature = "from_utf8_error_as_bytes", since = "1.26.0")]
    pub fn as_bytes(&self) -> &[u8] {
}

    /// Returns the bytes that were attempted to convert to a `String`.
    ///
    /// This method is carefully constructed to avoid allocation. It will
    /// consume the error, moving out the bytes, so that a copy of the bytes
    /// does not need to be made.
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// // some invalid bytes, in a vector
    /// let bytes = vec![0, 159];
    ///
    /// let value = String::from_utf8(bytes);
    ///
    /// assert_eq!(vec![0, 159], value.unwrap_err().into_bytes());
    /// ```
    #[stable(feature = "rust1", since = "1.0.0")]
    pub fn into_bytes(self) -> Vec<u8> {
}

    /// Fetch a `Utf8Error` to get more details about the conversion failure.
    ///
    /// The [`Utf8Error`] type provided by [`std::str`] represents an error that may
    /// occur when converting a slice of [`u8`]s to a [`&str`]. In this sense, it's
    /// an analogue to `FromUtf8Error`. See its documentation for more details
    /// on using it.
    ///
    /// [`std::str`]: core::str
    /// [`&str`]: prim@str
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// // some invalid bytes, in a vector
    /// let bytes = vec![0, 159];
    ///
    /// let error = String::from_utf8(bytes).unwrap_err().utf8_error();
    ///
    /// // the first byte is invalid here
    /// assert_eq!(1, error.valid_up_to());
    /// ```
    #[stable(feature = "rust1", since = "1.0.0")]
    pub fn utf8_error(&self) -> Utf8Error {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl fmt::Display for FromUtf8Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl fmt::Display for FromUtf16Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
}
}

#[cfg(not(no_global_oom_handling))]
#[stable(feature = "rust1", since = "1.0.0")]
impl Clone for String {
    fn clone(&self) -> Self {
}

    fn clone_from(&mut self, source: &Self) {
}
}

#[cfg(not(no_global_oom_handling))]
#[stable(feature = "rust1", since = "1.0.0")]
impl FromIterator<char> for String {
    fn from_iter<I: IntoIterator<Item = char>>(iter: I) -> String {
}
}

#[cfg(not(no_global_oom_handling))]
#[stable(feature = "string_from_iter_by_ref", since = "1.17.0")]
impl<'a> FromIterator<&'a char> for String {
    fn from_iter<I: IntoIterator<Item = &'a char>>(iter: I) -> String {
}
}

#[cfg(not(no_global_oom_handling))]
#[stable(feature = "rust1", since = "1.0.0")]
impl<'a> FromIterator<&'a str> for String {
    fn from_iter<I: IntoIterator<Item = &'a str>>(iter: I) -> String {
}
}

#[cfg(not(no_global_oom_handling))]
#[stable(feature = "extend_string", since = "1.4.0")]
impl FromIterator<String> for String {
    fn from_iter<I: IntoIterator<Item = String>>(iter: I) -> String {
}
}

#[cfg(not(no_global_oom_handling))]
#[stable(feature = "box_str2", since = "1.45.0")]
impl FromIterator<Box<str>> for String {
    fn from_iter<I: IntoIterator<Item = Box<str>>>(iter: I) -> String {
}
}

#[cfg(not(no_global_oom_handling))]
#[stable(feature = "herd_cows", since = "1.19.0")]
impl<'a> FromIterator<Cow<'a, str>> for String {
    fn from_iter<I: IntoIterator<Item = Cow<'a, str>>>(iter: I) -> String {
}
}

#[cfg(not(no_global_oom_handling))]
#[stable(feature = "rust1", since = "1.0.0")]
impl Extend<char> for String {
    fn extend<I: IntoIterator<Item = char>>(&mut self, iter: I) {
}

    #[inline]
    fn extend_one(&mut self, c: char) {
}

    #[inline]
    fn extend_reserve(&mut self, additional: usize) {
}
}

#[cfg(not(no_global_oom_handling))]
#[stable(feature = "extend_ref", since = "1.2.0")]
impl<'a> Extend<&'a char> for String {
    fn extend<I: IntoIterator<Item = &'a char>>(&mut self, iter: I) {
}

    #[inline]
    fn extend_one(&mut self, &c: &'a char) {
}

    #[inline]
    fn extend_reserve(&mut self, additional: usize) {
}
}

#[cfg(not(no_global_oom_handling))]
#[stable(feature = "rust1", since = "1.0.0")]
impl<'a> Extend<&'a str> for String {
    fn extend<I: IntoIterator<Item = &'a str>>(&mut self, iter: I) {
}

    #[inline]
    fn extend_one(&mut self, s: &'a str) {
}
}

#[cfg(not(no_global_oom_handling))]
#[stable(feature = "box_str2", since = "1.45.0")]
impl Extend<Box<str>> for String {
    fn extend<I: IntoIterator<Item = Box<str>>>(&mut self, iter: I) {
}
}

#[cfg(not(no_global_oom_handling))]
#[stable(feature = "extend_string", since = "1.4.0")]
impl Extend<String> for String {
    fn extend<I: IntoIterator<Item = String>>(&mut self, iter: I) {
}

    #[inline]
    fn extend_one(&mut self, s: String) {
}
}

#[cfg(not(no_global_oom_handling))]
#[stable(feature = "herd_cows", since = "1.19.0")]
impl<'a> Extend<Cow<'a, str>> for String {
    fn extend<I: IntoIterator<Item = Cow<'a, str>>>(&mut self, iter: I) {
}

    #[inline]
    fn extend_one(&mut self, s: Cow<'a, str>) {
}
}

/// A convenience impl that delegates to the impl for `&str`.
///
/// # Examples
///
/// ```
/// assert_eq!(String::from("Hello world").find("world"), Some(6));
/// ```
#[unstable(
    feature = "pattern",
    reason = "API not fully fleshed out and ready to be stabilized",
    issue = "27721"
)]
impl<'a, 'b> Pattern<'a> for &'b String {
    type Searcher = <&'b str as Pattern<'a>>::Searcher;

    fn into_searcher(self, haystack: &'a str) -> <&'b str as Pattern<'a>>::Searcher {
}

    #[inline]
    fn is_contained_in(self, haystack: &'a str) -> bool {
}

    #[inline]
    fn is_prefix_of(self, haystack: &'a str) -> bool {
}

    #[inline]
    fn strip_prefix_of(self, haystack: &'a str) -> Option<&'a str> {
}

    #[inline]
    fn is_suffix_of(self, haystack: &'a str) -> bool {
}

    #[inline]
    fn strip_suffix_of(self, haystack: &'a str) -> Option<&'a str> {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl PartialEq for String {
    #[inline]
    fn eq(&self, other: &String) -> bool {
}
    #[inline]
    fn ne(&self, other: &String) -> bool {
}
}

macro_rules! impl_eq {
    ($lhs:ty, $rhs: ty) => {
        #[stable(feature = "rust1", since = "1.0.0")]
        #[allow(unused_lifetimes)]
        impl<'a, 'b> PartialEq<$rhs> for $lhs {
            #[inline]
            fn eq(&self, other: &$rhs) -> bool {
}
            #[inline]
            fn ne(&self, other: &$rhs) -> bool {
}
        }

        #[stable(feature = "rust1", since = "1.0.0")]
        #[allow(unused_lifetimes)]
        impl<'a, 'b> PartialEq<$lhs> for $rhs {
            #[inline]
            fn eq(&self, other: &$lhs) -> bool {
}
            #[inline]
            fn ne(&self, other: &$lhs) -> bool {
}
        }
    };
}

impl_eq! { String, str }
impl_eq! { String, &'a str }
#[cfg(not(no_global_oom_handling))]
impl_eq! { Cow<'a, str>, str }
#[cfg(not(no_global_oom_handling))]
impl_eq! { Cow<'a, str>, &'b str }
#[cfg(not(no_global_oom_handling))]
impl_eq! { Cow<'a, str>, String }

#[stable(feature = "rust1", since = "1.0.0")]
#[rustc_const_unstable(feature = "const_default_impls", issue = "87864")]
impl const Default for String {
    /// Creates an empty `String`.
    #[inline]
    fn default() -> String {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl fmt::Display for String {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl fmt::Debug for String {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl hash::Hash for String {
    #[inline]
    fn hash<H: hash::Hasher>(&self, hasher: &mut H) {
}
}

/// Implements the `+` operator for concatenating two strings.
///
/// This consumes the `String` on the left-hand side and re-uses its buffer (growing it if
/// necessary). This is done to avoid allocating a new `String` and copying the entire contents on
/// every operation, which would lead to *O*(*n*^2) running time when building an *n*-byte string by
/// repeated concatenation.
///
/// The string on the right-hand side is only borrowed; its contents are copied into the returned
/// `String`.
///
/// # Examples
///
/// Concatenating two `String`s takes the first by value and borrows the second:
///
/// ```
/// let a = String::from("hello");
/// let b = String::from(" world");
/// let c = a + &b;
/// // `a` is moved and can no longer be used here.
/// ```
///
/// If you want to keep using the first `String`, you can clone it and append to the clone instead:
///
/// ```
/// let a = String::from("hello");
/// let b = String::from(" world");
/// let c = a.clone() + &b;
/// // `a` is still valid here.
/// ```
///
/// Concatenating `&str` slices can be done by converting the first to a `String`:
///
/// ```
/// let a = "hello";
/// let b = " world";
/// let c = a.to_string() + b;
/// ```
#[cfg(not(no_global_oom_handling))]
#[stable(feature = "rust1", since = "1.0.0")]
impl Add<&str> for String {
    type Output = String;

    #[inline]
    fn add(mut self, other: &str) -> String {
}
}

/// Implements the `+=` operator for appending to a `String`.
///
/// This has the same behavior as the [`push_str`][String::push_str] method.
#[cfg(not(no_global_oom_handling))]
#[stable(feature = "stringaddassign", since = "1.12.0")]
impl AddAssign<&str> for String {
    #[inline]
    fn add_assign(&mut self, other: &str) {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl ops::Index<ops::Range<usize>> for String {
    type Output = str;

    #[inline]
    fn index(&self, index: ops::Range<usize>) -> &str {
}
}
#[stable(feature = "rust1", since = "1.0.0")]
impl ops::Index<ops::RangeTo<usize>> for String {
    type Output = str;

    #[inline]
    fn index(&self, index: ops::RangeTo<usize>) -> &str {
}
}
#[stable(feature = "rust1", since = "1.0.0")]
impl ops::Index<ops::RangeFrom<usize>> for String {
    type Output = str;

    #[inline]
    fn index(&self, index: ops::RangeFrom<usize>) -> &str {
}
}
#[stable(feature = "rust1", since = "1.0.0")]
impl ops::Index<ops::RangeFull> for String {
    type Output = str;

    #[inline]
    fn index(&self, _index: ops::RangeFull) -> &str {
}
}
#[stable(feature = "inclusive_range", since = "1.26.0")]
impl ops::Index<ops::RangeInclusive<usize>> for String {
    type Output = str;

    #[inline]
    fn index(&self, index: ops::RangeInclusive<usize>) -> &str {
}
}
#[stable(feature = "inclusive_range", since = "1.26.0")]
impl ops::Index<ops::RangeToInclusive<usize>> for String {
    type Output = str;

    #[inline]
    fn index(&self, index: ops::RangeToInclusive<usize>) -> &str {
}
}

#[stable(feature = "derefmut_for_string", since = "1.3.0")]
impl ops::IndexMut<ops::Range<usize>> for String {
    #[inline]
    fn index_mut(&mut self, index: ops::Range<usize>) -> &mut str {
}
}
#[stable(feature = "derefmut_for_string", since = "1.3.0")]
impl ops::IndexMut<ops::RangeTo<usize>> for String {
    #[inline]
    fn index_mut(&mut self, index: ops::RangeTo<usize>) -> &mut str {
}
}
#[stable(feature = "derefmut_for_string", since = "1.3.0")]
impl ops::IndexMut<ops::RangeFrom<usize>> for String {
    #[inline]
    fn index_mut(&mut self, index: ops::RangeFrom<usize>) -> &mut str {
}
}
#[stable(feature = "derefmut_for_string", since = "1.3.0")]
impl ops::IndexMut<ops::RangeFull> for String {
    #[inline]
    fn index_mut(&mut self, _index: ops::RangeFull) -> &mut str {
}
}
#[stable(feature = "inclusive_range", since = "1.26.0")]
impl ops::IndexMut<ops::RangeInclusive<usize>> for String {
    #[inline]
    fn index_mut(&mut self, index: ops::RangeInclusive<usize>) -> &mut str {
}
}
#[stable(feature = "inclusive_range", since = "1.26.0")]
impl ops::IndexMut<ops::RangeToInclusive<usize>> for String {
    #[inline]
    fn index_mut(&mut self, index: ops::RangeToInclusive<usize>) -> &mut str {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl ops::Deref for String {
    type Target = str;

    #[inline]
    fn deref(&self) -> &str {
}
}

#[stable(feature = "derefmut_for_string", since = "1.3.0")]
impl ops::DerefMut for String {
    #[inline]
    fn deref_mut(&mut self) -> &mut str {
}
}

/// A type alias for [`Infallible`].
///
/// This alias exists for backwards compatibility, and may be eventually deprecated.
///
/// [`Infallible`]: core::convert::Infallible
#[stable(feature = "str_parse_error", since = "1.5.0")]
pub type ParseError = core::convert::Infallible;

#[cfg(not(no_global_oom_handling))]
#[stable(feature = "rust1", since = "1.0.0")]
impl FromStr for String {
    type Err = core::convert::Infallible;
    #[inline]
    fn from_str(s: &str) -> Result<String, Self::Err> {
}
}

/// A trait for converting a value to a `String`.
///
/// This trait is automatically implemented for any type which implements the
/// [`Display`] trait. As such, `ToString` shouldn't be implemented directly:
/// [`Display`] should be implemented instead, and you get the `ToString`
/// implementation for free.
///
/// [`Display`]: fmt::Display
#[cfg_attr(not(test), rustc_diagnostic_item = "ToString")]
#[stable(feature = "rust1", since = "1.0.0")]
pub trait ToString {
    /// Converts the given value to a `String`.
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// let i = 5;
    /// let five = String::from("5");
    ///
    /// assert_eq!(five, i.to_string());
    /// ```
    #[rustc_conversion_suggestion]
    #[stable(feature = "rust1", since = "1.0.0")]
    fn to_string(&self) -> String;
}

/// # Panics
///
/// In this implementation, the `to_string` method panics
/// if the `Display` implementation returns an error.
/// This indicates an incorrect `Display` implementation
/// since `fmt::Write for String` never returns an error itself.
#[cfg(not(no_global_oom_handling))]
#[stable(feature = "rust1", since = "1.0.0")]
impl<T: fmt::Display + ?Sized> ToString for T {
    // A common guideline is to not inline generic functions. However,
    // removing `#[inline]` from this method causes non-negligible regressions.
    // See <https://github.com/rust-lang/rust/pull/74852>, the last attempt
    // to try to remove it.
    #[inline]
    default fn to_string(&self) -> String {
}
}

#[cfg(not(no_global_oom_handling))]
#[stable(feature = "char_to_string_specialization", since = "1.46.0")]
impl ToString for char {
    #[inline]
    fn to_string(&self) -> String {
}
}

#[cfg(not(no_global_oom_handling))]
#[stable(feature = "u8_to_string_specialization", since = "1.54.0")]
impl ToString for u8 {
    #[inline]
    fn to_string(&self) -> String {
}
}

#[cfg(not(no_global_oom_handling))]
#[stable(feature = "i8_to_string_specialization", since = "1.54.0")]
impl ToString for i8 {
    #[inline]
    fn to_string(&self) -> String {
}
}

#[cfg(not(no_global_oom_handling))]
#[stable(feature = "str_to_string_specialization", since = "1.9.0")]
impl ToString for str {
    #[inline]
    fn to_string(&self) -> String {
}
}

#[cfg(not(no_global_oom_handling))]
#[stable(feature = "cow_str_to_string_specialization", since = "1.17.0")]
impl ToString for Cow<'_, str> {
    #[inline]
    fn to_string(&self) -> String {
}
}

#[cfg(not(no_global_oom_handling))]
#[stable(feature = "string_to_string_specialization", since = "1.17.0")]
impl ToString for String {
    #[inline]
    fn to_string(&self) -> String {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl AsRef<str> for String {
    #[inline]
    fn as_ref(&self) -> &str {
}
}

#[stable(feature = "string_as_mut", since = "1.43.0")]
impl AsMut<str> for String {
    #[inline]
    fn as_mut(&mut self) -> &mut str {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl AsRef<[u8]> for String {
    #[inline]
    fn as_ref(&self) -> &[u8] {
}
}

#[cfg(not(no_global_oom_handling))]
#[stable(feature = "rust1", since = "1.0.0")]
impl From<&str> for String {
    /// Converts a `&str` into a [`String`].
    ///
    /// The result is allocated on the heap.
    #[inline]
    fn from(s: &str) -> String {
}
}

#[cfg(not(no_global_oom_handling))]
#[stable(feature = "from_mut_str_for_string", since = "1.44.0")]
impl From<&mut str> for String {
    /// Converts a `&mut str` into a [`String`].
    ///
    /// The result is allocated on the heap.
    #[inline]
    fn from(s: &mut str) -> String {
}
}

#[cfg(not(no_global_oom_handling))]
#[stable(feature = "from_ref_string", since = "1.35.0")]
impl From<&String> for String {
    /// Converts a `&String` into a [`String`].
    ///
    /// This clones `s` and returns the clone.
    #[inline]
    fn from(s: &String) -> String {
}
}

// note: test pulls in libstd, which causes errors here
#[cfg(not(test))]
#[stable(feature = "string_from_box", since = "1.18.0")]
impl From<Box<str>> for String {
    /// Converts the given boxed `str` slice to a [`String`].
    /// It is notable that the `str` slice is owned.
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// let s1: String = String::from("hello world");
    /// let s2: Box<str> = s1.into_boxed_str();
    /// let s3: String = String::from(s2);
    ///
    /// assert_eq!("hello world", s3)
    /// ```
    fn from(s: Box<str>) -> String {
}
}

#[cfg(not(no_global_oom_handling))]
#[stable(feature = "box_from_str", since = "1.20.0")]
impl From<String> for Box<str> {
    /// Converts the given [`String`] to a boxed `str` slice that is owned.
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// let s1: String = String::from("hello world");
    /// let s2: Box<str> = Box::from(s1);
    /// let s3: String = String::from(s2);
    ///
    /// assert_eq!("hello world", s3)
    /// ```
    fn from(s: String) -> Box<str> {
}
}

#[cfg(not(no_global_oom_handling))]
#[stable(feature = "string_from_cow_str", since = "1.14.0")]
impl<'a> From<Cow<'a, str>> for String {
    /// Converts a clone-on-write string to an owned
    /// instance of [`String`].
    ///
    /// This extracts the owned string,
    /// clones the string if it is not already owned.
    ///
    /// # Example
    ///
    /// ```
    /// # use std::borrow::Cow;
    /// // If the string is not owned...
    /// let cow: Cow<str> = Cow::Borrowed("eggplant");
    /// // It will allocate on the heap and copy the string.
    /// let owned: String = String::from(cow);
    /// assert_eq!(&owned[..], "eggplant");
    /// ```
    fn from(s: Cow<'a, str>) -> String {
}
}

#[cfg(not(no_global_oom_handling))]
#[stable(feature = "rust1", since = "1.0.0")]
impl<'a> From<&'a str> for Cow<'a, str> {
    /// Converts a string slice into a [`Borrowed`] variant.
    /// No heap allocation is performed, and the string
    /// is not copied.
    ///
    /// # Example
    ///
    /// ```
    /// # use std::borrow::Cow;
    /// assert_eq!(Cow::from("eggplant"), Cow::Borrowed("eggplant"));
    /// ```
    ///
    /// [`Borrowed`]: crate::borrow::Cow::Borrowed
    #[inline]
    fn from(s: &'a str) -> Cow<'a, str> {
}
}

#[cfg(not(no_global_oom_handling))]
#[stable(feature = "rust1", since = "1.0.0")]
impl<'a> From<String> for Cow<'a, str> {
    /// Converts a [`String`] into an [`Owned`] variant.
    /// No heap allocation is performed, and the string
    /// is not copied.
    ///
    /// # Example
    ///
    /// ```
    /// # use std::borrow::Cow;
    /// let s = "eggplant".to_string();
    /// let s2 = "eggplant".to_string();
    /// assert_eq!(Cow::from(s), Cow::<'static, str>::Owned(s2));
    /// ```
    ///
    /// [`Owned`]: crate::borrow::Cow::Owned
    #[inline]
    fn from(s: String) -> Cow<'a, str> {
}
}

#[cfg(not(no_global_oom_handling))]
#[stable(feature = "cow_from_string_ref", since = "1.28.0")]
impl<'a> From<&'a String> for Cow<'a, str> {
    /// Converts a [`String`] reference into a [`Borrowed`] variant.
    /// No heap allocation is performed, and the string
    /// is not copied.
    ///
    /// # Example
    ///
    /// ```
    /// # use std::borrow::Cow;
    /// let s = "eggplant".to_string();
    /// assert_eq!(Cow::from(&s), Cow::Borrowed("eggplant"));
    /// ```
    ///
    /// [`Borrowed`]: crate::borrow::Cow::Borrowed
    #[inline]
    fn from(s: &'a String) -> Cow<'a, str> {
}
}

#[cfg(not(no_global_oom_handling))]
#[stable(feature = "cow_str_from_iter", since = "1.12.0")]
impl<'a> FromIterator<char> for Cow<'a, str> {
    fn from_iter<I: IntoIterator<Item = char>>(it: I) -> Cow<'a, str> {
}
}

#[cfg(not(no_global_oom_handling))]
#[stable(feature = "cow_str_from_iter", since = "1.12.0")]
impl<'a, 'b> FromIterator<&'b str> for Cow<'a, str> {
    fn from_iter<I: IntoIterator<Item = &'b str>>(it: I) -> Cow<'a, str> {
}
}

#[cfg(not(no_global_oom_handling))]
#[stable(feature = "cow_str_from_iter", since = "1.12.0")]
impl<'a> FromIterator<String> for Cow<'a, str> {
    fn from_iter<I: IntoIterator<Item = String>>(it: I) -> Cow<'a, str> {
}
}

#[stable(feature = "from_string_for_vec_u8", since = "1.14.0")]
impl From<String> for Vec<u8> {
    /// Converts the given [`String`] to a vector [`Vec`] that holds values of type [`u8`].
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// let s1 = String::from("hello world");
    /// let v1 = Vec::from(s1);
    ///
    /// for b in v1 {
    ///     println!("{}", b);
    /// }
    /// ```
    fn from(string: String) -> Vec<u8> {
}
}

#[cfg(not(no_global_oom_handling))]
#[stable(feature = "rust1", since = "1.0.0")]
impl fmt::Write for String {
    #[inline]
    fn write_str(&mut self, s: &str) -> fmt::Result {
}

    #[inline]
    fn write_char(&mut self, c: char) -> fmt::Result {
}
}

/// A draining iterator for `String`.
///
/// This struct is created by the [`drain`] method on [`String`]. See its
/// documentation for more.
///
/// [`drain`]: String::drain
#[stable(feature = "drain", since = "1.6.0")]
pub struct Drain<'a> {
    /// Will be used as &'a mut String in the destructor
    string: *mut String,
    /// Start of part to remove
    start: usize,
    /// End of part to remove
    end: usize,
    /// Current remaining range to remove
    iter: Chars<'a>,
}

#[stable(feature = "collection_debug", since = "1.17.0")]
impl fmt::Debug for Drain<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
}
}

#[stable(feature = "drain", since = "1.6.0")]
unsafe impl Sync for Drain<'_> {}
#[stable(feature = "drain", since = "1.6.0")]
unsafe impl Send for Drain<'_> {}

#[stable(feature = "drain", since = "1.6.0")]
impl Drop for Drain<'_> {
    fn drop(&mut self) {
}
}

impl<'a> Drain<'a> {
    /// Returns the remaining (sub)string of this iterator as a slice.
    ///
    /// # Examples
    ///
    /// ```
    /// let mut s = String::from("abc");
    /// let mut drain = s.drain(..);
    /// assert_eq!(drain.as_str(), "abc");
    /// let _ = drain.next().unwrap();
    /// assert_eq!(drain.as_str(), "bc");
    /// ```
    #[stable(feature = "string_drain_as_str", since = "1.55.0")]
    pub fn as_str(&self) -> &str {
}
}

#[stable(feature = "string_drain_as_str", since = "1.55.0")]
impl<'a> AsRef<str> for Drain<'a> {
    fn as_ref(&self) -> &str {
}
}

#[stable(feature = "string_drain_as_str", since = "1.55.0")]
impl<'a> AsRef<[u8]> for Drain<'a> {
    fn as_ref(&self) -> &[u8] {
}
}

#[stable(feature = "drain", since = "1.6.0")]
impl Iterator for Drain<'_> {
    type Item = char;

    #[inline]
    fn next(&mut self) -> Option<char> {
}

    fn size_hint(&self) -> (usize, Option<usize>) {
}

    #[inline]
    fn last(mut self) -> Option<char> {
}
}

#[stable(feature = "drain", since = "1.6.0")]
impl DoubleEndedIterator for Drain<'_> {
    #[inline]
    fn next_back(&mut self) -> Option<char> {
}
}

#[stable(feature = "fused", since = "1.26.0")]
impl FusedIterator for Drain<'_> {}

#[cfg(not(no_global_oom_handling))]
#[stable(feature = "from_char_for_string", since = "1.46.0")]
impl From<char> for String {
    /// Allocates an owned [`String`] from a single character.
    ///
    /// # Example
    /// ```rust
    /// let c: char = 'a';
    /// let s: String = String::from(c);
    /// assert_eq!("a", &s[..]);
    /// ```
    #[inline]
    fn from(c: char) -> Self {
}
}
}
#[cfg(target_has_atomic = "ptr")]
pub mod sync {
#![stable(feature = "rust1", since = "1.0.0")]

//! Thread-safe reference-counting pointers.
//!
//! See the [`Arc<T>`][Arc] documentation for more details.

use core::any::Any;
use core::borrow;
use core::cmp::Ordering;
use core::convert::{From, TryFrom};
use core::fmt;
use core::hash::{Hash, Hasher};
use core::hint;
use core::intrinsics::abort;
#[cfg(not(no_global_oom_handling))]
use core::iter;
use core::marker::{PhantomData, Unpin, Unsize};
#[cfg(not(no_global_oom_handling))]
use core::mem::size_of_val;
use core::mem::{self, align_of_val_raw};
use core::ops::{CoerceUnsized, Deref, DispatchFromDyn, Receiver};
use core::panic::{RefUnwindSafe, UnwindSafe};
use core::pin::Pin;
use core::ptr::{self, NonNull};
#[cfg(not(no_global_oom_handling))]
use core::slice::from_raw_parts_mut;
use core::sync::atomic;
use core::sync::atomic::Ordering::{Acquire, Relaxed, Release, SeqCst};

#[cfg(not(no_global_oom_handling))]
use crate::alloc::handle_alloc_error;
#[cfg(not(no_global_oom_handling))]
use crate::alloc::{box_free, WriteCloneIntoRaw};
use crate::alloc::{AllocError, Allocator, Global, Layout};
use crate::borrow::{Cow, ToOwned};
use crate::boxed::Box;
use crate::rc::is_dangling;
#[cfg(not(no_global_oom_handling))]
use crate::string::String;
#[cfg(not(no_global_oom_handling))]
use crate::vec::Vec;

#[cfg(test)]
mod tests {
}

/// A soft limit on the amount of references that may be made to an `Arc`.
///
/// Going above this limit will abort your program (although not
/// necessarily) at _exactly_ `MAX_REFCOUNT + 1` references.
const MAX_REFCOUNT: usize = (isize::MAX) as usize;

#[cfg(not(sanitize = "thread"))]
macro_rules! acquire {
    ($x:expr) => {
        atomic::fence(Acquire)
    };
}

// ThreadSanitizer does not support memory fences. To avoid false positive
// reports in Arc / Weak implementation use atomic loads for synchronization
// instead.
#[cfg(sanitize = "thread")]
macro_rules! acquire {
    ($x:expr) => {
        $x.load(Acquire)
    };
}

/// A thread-safe reference-counting pointer. 'Arc' stands for 'Atomically
/// Reference Counted'.
///
/// The type `Arc<T>` provides shared ownership of a value of type `T`,
/// allocated in the heap. Invoking [`clone`][clone] on `Arc` produces
/// a new `Arc` instance, which points to the same allocation on the heap as the
/// source `Arc`, while increasing a reference count. When the last `Arc`
/// pointer to a given allocation is destroyed, the value stored in that allocation (often
/// referred to as "inner value") is also dropped.
///
/// Shared references in Rust disallow mutation by default, and `Arc` is no
/// exception: you cannot generally obtain a mutable reference to something
/// inside an `Arc`. If you need to mutate through an `Arc`, use
/// [`Mutex`][mutex], [`RwLock`][rwlock], or one of the [`Atomic`][atomic]
/// types.
///
/// ## Thread Safety
///
/// Unlike [`Rc<T>`], `Arc<T>` uses atomic operations for its reference
/// counting. This means that it is thread-safe. The disadvantage is that
/// atomic operations are more expensive than ordinary memory accesses. If you
/// are not sharing reference-counted allocations between threads, consider using
/// [`Rc<T>`] for lower overhead. [`Rc<T>`] is a safe default, because the
/// compiler will catch any attempt to send an [`Rc<T>`] between threads.
/// However, a library might choose `Arc<T>` in order to give library consumers
/// more flexibility.
///
/// `Arc<T>` will implement [`Send`] and [`Sync`] as long as the `T` implements
/// [`Send`] and [`Sync`]. Why can't you put a non-thread-safe type `T` in an
/// `Arc<T>` to make it thread-safe? This may be a bit counter-intuitive at
/// first: after all, isn't the point of `Arc<T>` thread safety? The key is
/// this: `Arc<T>` makes it thread safe to have multiple ownership of the same
/// data, but it  doesn't add thread safety to its data. Consider
/// `Arc<`[`RefCell<T>`]`>`. [`RefCell<T>`] isn't [`Sync`], and if `Arc<T>` was always
/// [`Send`], `Arc<`[`RefCell<T>`]`>` would be as well. But then we'd have a problem:
/// [`RefCell<T>`] is not thread safe; it keeps track of the borrowing count using
/// non-atomic operations.
///
/// In the end, this means that you may need to pair `Arc<T>` with some sort of
/// [`std::sync`] type, usually [`Mutex<T>`][mutex].
///
/// ## Breaking cycles with `Weak`
///
/// The [`downgrade`][downgrade] method can be used to create a non-owning
/// [`Weak`] pointer. A [`Weak`] pointer can be [`upgrade`][upgrade]d
/// to an `Arc`, but this will return [`None`] if the value stored in the allocation has
/// already been dropped. In other words, `Weak` pointers do not keep the value
/// inside the allocation alive; however, they *do* keep the allocation
/// (the backing store for the value) alive.
///
/// A cycle between `Arc` pointers will never be deallocated. For this reason,
/// [`Weak`] is used to break cycles. For example, a tree could have
/// strong `Arc` pointers from parent nodes to children, and [`Weak`]
/// pointers from children back to their parents.
///
/// # Cloning references
///
/// Creating a new reference from an existing reference-counted pointer is done using the
/// `Clone` trait implemented for [`Arc<T>`][Arc] and [`Weak<T>`][Weak].
///
/// ```
/// use std::sync::Arc;
/// let foo = Arc::new(vec![1.0, 2.0, 3.0]);
/// // The two syntaxes below are equivalent.
/// let a = foo.clone();
/// let b = Arc::clone(&foo);
/// // a, b, and foo are all Arcs that point to the same memory location
/// ```
///
/// ## `Deref` behavior
///
/// `Arc<T>` automatically dereferences to `T` (via the [`Deref`][deref] trait),
/// so you can call `T`'s methods on a value of type `Arc<T>`. To avoid name
/// clashes with `T`'s methods, the methods of `Arc<T>` itself are associated
/// functions, called using [fully qualified syntax]:
///
/// ```
/// use std::sync::Arc;
///
/// let my_arc = Arc::new(());
/// Arc::downgrade(&my_arc);
/// ```
///
/// `Arc<T>`'s implementations of traits like `Clone` may also be called using
/// fully qualified syntax. Some people prefer to use fully qualified syntax,
/// while others prefer using method-call syntax.
///
/// ```
/// use std::sync::Arc;
///
/// let arc = Arc::new(());
/// // Method-call syntax
/// let arc2 = arc.clone();
/// // Fully qualified syntax
/// let arc3 = Arc::clone(&arc);
/// ```
///
/// [`Weak<T>`][Weak] does not auto-dereference to `T`, because the inner value may have
/// already been dropped.
///
/// [`Rc<T>`]: crate::rc::Rc
/// [clone]: Clone::clone
/// [mutex]: ../../std/sync/struct.Mutex.html
/// [rwlock]: ../../std/sync/struct.RwLock.html
/// [atomic]: core::sync::atomic
/// [`Send`]: core::marker::Send
/// [`Sync`]: core::marker::Sync
/// [deref]: core::ops::Deref
/// [downgrade]: Arc::downgrade
/// [upgrade]: Weak::upgrade
/// [`RefCell<T>`]: core::cell::RefCell
/// [`std::sync`]: ../../std/sync/index.html
/// [`Arc::clone(&from)`]: Arc::clone
/// [fully qualified syntax]: https://doc.rust-lang.org/book/ch19-03-advanced-traits.html#fully-qualified-syntax-for-disambiguation-calling-methods-with-the-same-name
///
/// # Examples
///
/// Sharing some immutable data between threads:
///
// Note that we **do not** run these tests here. The windows builders get super
// unhappy if a thread outlives the main thread and then exits at the same time
// (something deadlocks) so we just avoid this entirely by not running these
// tests.
/// ```no_run
/// use std::sync::Arc;
/// use std::thread;
///
/// let five = Arc::new(5);
///
/// for _ in 0..10 {
///     let five = Arc::clone(&five);
///
///     thread::spawn(move || {
///         println!("{:?}", five);
///     });
/// }
/// ```
///
/// Sharing a mutable [`AtomicUsize`]:
///
/// [`AtomicUsize`]: core::sync::atomic::AtomicUsize
///
/// ```no_run
/// use std::sync::Arc;
/// use std::sync::atomic::{AtomicUsize, Ordering};
/// use std::thread;
///
/// let val = Arc::new(AtomicUsize::new(5));
///
/// for _ in 0..10 {
///     let val = Arc::clone(&val);
///
///     thread::spawn(move || {
///         let v = val.fetch_add(1, Ordering::SeqCst);
///         println!("{:?}", v);
///     });
/// }
/// ```
///
/// See the [`rc` documentation][rc_examples] for more examples of reference
/// counting in general.
///
/// [rc_examples]: crate::rc#examples
#[cfg_attr(not(test), rustc_diagnostic_item = "Arc")]
#[stable(feature = "rust1", since = "1.0.0")]
pub struct Arc<T: ?Sized> {
    ptr: NonNull<ArcInner<T>>,
    phantom: PhantomData<ArcInner<T>>,
}

#[stable(feature = "rust1", since = "1.0.0")]
unsafe impl<T: ?Sized + Sync + Send> Send for Arc<T> {}
#[stable(feature = "rust1", since = "1.0.0")]
unsafe impl<T: ?Sized + Sync + Send> Sync for Arc<T> {}

#[stable(feature = "catch_unwind", since = "1.9.0")]
impl<T: RefUnwindSafe + ?Sized> UnwindSafe for Arc<T> {}

#[unstable(feature = "coerce_unsized", issue = "27732")]
impl<T: ?Sized + Unsize<U>, U: ?Sized> CoerceUnsized<Arc<U>> for Arc<T> {}

#[unstable(feature = "dispatch_from_dyn", issue = "none")]
impl<T: ?Sized + Unsize<U>, U: ?Sized> DispatchFromDyn<Arc<U>> for Arc<T> {}

impl<T: ?Sized> Arc<T> {
    fn from_inner(ptr: NonNull<ArcInner<T>>) -> Self {
}

    unsafe fn from_ptr(ptr: *mut ArcInner<T>) -> Self {
}
}

/// `Weak` is a version of [`Arc`] that holds a non-owning reference to the
/// managed allocation. The allocation is accessed by calling [`upgrade`] on the `Weak`
/// pointer, which returns an [`Option`]`<`[`Arc`]`<T>>`.
///
/// Since a `Weak` reference does not count towards ownership, it will not
/// prevent the value stored in the allocation from being dropped, and `Weak` itself makes no
/// guarantees about the value still being present. Thus it may return [`None`]
/// when [`upgrade`]d. Note however that a `Weak` reference *does* prevent the allocation
/// itself (the backing store) from being deallocated.
///
/// A `Weak` pointer is useful for keeping a temporary reference to the allocation
/// managed by [`Arc`] without preventing its inner value from being dropped. It is also used to
/// prevent circular references between [`Arc`] pointers, since mutual owning references
/// would never allow either [`Arc`] to be dropped. For example, a tree could
/// have strong [`Arc`] pointers from parent nodes to children, and `Weak`
/// pointers from children back to their parents.
///
/// The typical way to obtain a `Weak` pointer is to call [`Arc::downgrade`].
///
/// [`upgrade`]: Weak::upgrade
#[stable(feature = "arc_weak", since = "1.4.0")]
pub struct Weak<T: ?Sized> {
    // This is a `NonNull` to allow optimizing the size of this type in enums,
    // but it is not necessarily a valid pointer.
    // `Weak::new` sets this to `usize::MAX` so that it doesn’t need
    // to allocate space on the heap.  That's not a value a real pointer
    // will ever have because RcBox has alignment at least 2.
    // This is only possible when `T: Sized`; unsized `T` never dangle.
    ptr: NonNull<ArcInner<T>>,
}

#[stable(feature = "arc_weak", since = "1.4.0")]
unsafe impl<T: ?Sized + Sync + Send> Send for Weak<T> {}
#[stable(feature = "arc_weak", since = "1.4.0")]
unsafe impl<T: ?Sized + Sync + Send> Sync for Weak<T> {}

#[unstable(feature = "coerce_unsized", issue = "27732")]
impl<T: ?Sized + Unsize<U>, U: ?Sized> CoerceUnsized<Weak<U>> for Weak<T> {}
#[unstable(feature = "dispatch_from_dyn", issue = "none")]
impl<T: ?Sized + Unsize<U>, U: ?Sized> DispatchFromDyn<Weak<U>> for Weak<T> {}

#[stable(feature = "arc_weak", since = "1.4.0")]
impl<T: ?Sized + fmt::Debug> fmt::Debug for Weak<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
}
}

// This is repr(C) to future-proof against possible field-reordering, which
// would interfere with otherwise safe [into|from]_raw() of transmutable
// inner types.
#[repr(C)]
struct ArcInner<T: ?Sized> {
    strong: atomic::AtomicUsize,

    // the value usize::MAX acts as a sentinel for temporarily "locking" the
    // ability to upgrade weak pointers or downgrade strong ones; this is used
    // to avoid races in `make_mut` and `get_mut`.
    weak: atomic::AtomicUsize,

    data: T,
}

unsafe impl<T: ?Sized + Sync + Send> Send for ArcInner<T> {}
unsafe impl<T: ?Sized + Sync + Send> Sync for ArcInner<T> {}

impl<T> Arc<T> {
    /// Constructs a new `Arc<T>`.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::sync::Arc;
    ///
    /// let five = Arc::new(5);
    /// ```
    #[cfg(not(no_global_oom_handling))]
    #[inline]
    #[stable(feature = "rust1", since = "1.0.0")]
    pub fn new(data: T) -> Arc<T> {
}

    /// Constructs a new `Arc<T>` using a weak reference to itself. Attempting
    /// to upgrade the weak reference before this function returns will result
    /// in a `None` value. However, the weak reference may be cloned freely and
    /// stored for use at a later time.
    ///
    /// # Examples
    /// ```
    /// #![feature(arc_new_cyclic)]
    /// #![allow(dead_code)]
    ///
    /// use std::sync::{Arc, Weak};
    ///
    /// struct Foo {
    ///     me: Weak<Foo>,
    /// }
    ///
    /// let foo = Arc::new_cyclic(|me| Foo {
    ///     me: me.clone(),
    /// });
    /// ```
    #[cfg(not(no_global_oom_handling))]
    #[inline]
    #[unstable(feature = "arc_new_cyclic", issue = "75861")]
    pub fn new_cyclic(data_fn: impl FnOnce(&Weak<T>) -> T) -> Arc<T> {
}

    /// Constructs a new `Arc` with uninitialized contents.
    ///
    /// # Examples
    ///
    /// ```
    /// #![feature(new_uninit)]
    /// #![feature(get_mut_unchecked)]
    ///
    /// use std::sync::Arc;
    ///
    /// let mut five = Arc::<u32>::new_uninit();
    ///
    /// let five = unsafe {
    ///     // Deferred initialization:
    ///     Arc::get_mut_unchecked(&mut five).as_mut_ptr().write(5);
    ///
    ///     five.assume_init()
    /// };
    ///
    /// assert_eq!(*five, 5)
    /// ```
    #[cfg(not(no_global_oom_handling))]
    #[unstable(feature = "new_uninit", issue = "63291")]
    pub fn new_uninit() -> Arc<mem::MaybeUninit<T>> {
}

    /// Constructs a new `Arc` with uninitialized contents, with the memory
    /// being filled with `0` bytes.
    ///
    /// See [`MaybeUninit::zeroed`][zeroed] for examples of correct and incorrect usage
    /// of this method.
    ///
    /// # Examples
    ///
    /// ```
    /// #![feature(new_uninit)]
    ///
    /// use std::sync::Arc;
    ///
    /// let zero = Arc::<u32>::new_zeroed();
    /// let zero = unsafe { zero.assume_init() };
    ///
    /// assert_eq!(*zero, 0)
    /// ```
    ///
    /// [zeroed]: ../../std/mem/union.MaybeUninit.html#method.zeroed
    #[cfg(not(no_global_oom_handling))]
    #[unstable(feature = "new_uninit", issue = "63291")]
    pub fn new_zeroed() -> Arc<mem::MaybeUninit<T>> {
}

    /// Constructs a new `Pin<Arc<T>>`. If `T` does not implement `Unpin`, then
    /// `data` will be pinned in memory and unable to be moved.
    #[cfg(not(no_global_oom_handling))]
    #[stable(feature = "pin", since = "1.33.0")]
    pub fn pin(data: T) -> Pin<Arc<T>> {
}

    /// Constructs a new `Pin<Arc<T>>`, return an error if allocation fails.
    #[unstable(feature = "allocator_api", issue = "32838")]
    #[inline]
    pub fn try_pin(data: T) -> Result<Pin<Arc<T>>, AllocError> {
}

    /// Constructs a new `Arc<T>`, returning an error if allocation fails.
    ///
    /// # Examples
    ///
    /// ```
    /// #![feature(allocator_api)]
    /// use std::sync::Arc;
    ///
    /// let five = Arc::try_new(5)?;
    /// # Ok::<(), std::alloc::AllocError>(())
    /// ```
    #[unstable(feature = "allocator_api", issue = "32838")]
    #[inline]
    pub fn try_new(data: T) -> Result<Arc<T>, AllocError> {
}

    /// Constructs a new `Arc` with uninitialized contents, returning an error
    /// if allocation fails.
    ///
    /// # Examples
    ///
    /// ```
    /// #![feature(new_uninit, allocator_api)]
    /// #![feature(get_mut_unchecked)]
    ///
    /// use std::sync::Arc;
    ///
    /// let mut five = Arc::<u32>::try_new_uninit()?;
    ///
    /// let five = unsafe {
    ///     // Deferred initialization:
    ///     Arc::get_mut_unchecked(&mut five).as_mut_ptr().write(5);
    ///
    ///     five.assume_init()
    /// };
    ///
    /// assert_eq!(*five, 5);
    /// # Ok::<(), std::alloc::AllocError>(())
    /// ```
    #[unstable(feature = "allocator_api", issue = "32838")]
    // #[unstable(feature = "new_uninit", issue = "63291")]
    pub fn try_new_uninit() -> Result<Arc<mem::MaybeUninit<T>>, AllocError> {
}

    /// Constructs a new `Arc` with uninitialized contents, with the memory
    /// being filled with `0` bytes, returning an error if allocation fails.
    ///
    /// See [`MaybeUninit::zeroed`][zeroed] for examples of correct and incorrect usage
    /// of this method.
    ///
    /// # Examples
    ///
    /// ```
    /// #![feature(new_uninit, allocator_api)]
    ///
    /// use std::sync::Arc;
    ///
    /// let zero = Arc::<u32>::try_new_zeroed()?;
    /// let zero = unsafe { zero.assume_init() };
    ///
    /// assert_eq!(*zero, 0);
    /// # Ok::<(), std::alloc::AllocError>(())
    /// ```
    ///
    /// [zeroed]: mem::MaybeUninit::zeroed
    #[unstable(feature = "allocator_api", issue = "32838")]
    // #[unstable(feature = "new_uninit", issue = "63291")]
    pub fn try_new_zeroed() -> Result<Arc<mem::MaybeUninit<T>>, AllocError> {
}
    /// Returns the inner value, if the `Arc` has exactly one strong reference.
    ///
    /// Otherwise, an [`Err`] is returned with the same `Arc` that was
    /// passed in.
    ///
    /// This will succeed even if there are outstanding weak references.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::sync::Arc;
    ///
    /// let x = Arc::new(3);
    /// assert_eq!(Arc::try_unwrap(x), Ok(3));
    ///
    /// let x = Arc::new(4);
    /// let _y = Arc::clone(&x);
    /// assert_eq!(*Arc::try_unwrap(x).unwrap_err(), 4);
    /// ```
    #[inline]
    #[stable(feature = "arc_unique", since = "1.4.0")]
    pub fn try_unwrap(this: Self) -> Result<T, Self> {
}
}

impl<T> Arc<[T]> {
    /// Constructs a new atomically reference-counted slice with uninitialized contents.
    ///
    /// # Examples
    ///
    /// ```
    /// #![feature(new_uninit)]
    /// #![feature(get_mut_unchecked)]
    ///
    /// use std::sync::Arc;
    ///
    /// let mut values = Arc::<[u32]>::new_uninit_slice(3);
    ///
    /// let values = unsafe {
    ///     // Deferred initialization:
    ///     Arc::get_mut_unchecked(&mut values)[0].as_mut_ptr().write(1);
    ///     Arc::get_mut_unchecked(&mut values)[1].as_mut_ptr().write(2);
    ///     Arc::get_mut_unchecked(&mut values)[2].as_mut_ptr().write(3);
    ///
    ///     values.assume_init()
    /// };
    ///
    /// assert_eq!(*values, [1, 2, 3])
    /// ```
    #[cfg(not(no_global_oom_handling))]
    #[unstable(feature = "new_uninit", issue = "63291")]
    pub fn new_uninit_slice(len: usize) -> Arc<[mem::MaybeUninit<T>]> {
}

    /// Constructs a new atomically reference-counted slice with uninitialized contents, with the memory being
    /// filled with `0` bytes.
    ///
    /// See [`MaybeUninit::zeroed`][zeroed] for examples of correct and
    /// incorrect usage of this method.
    ///
    /// # Examples
    ///
    /// ```
    /// #![feature(new_uninit)]
    ///
    /// use std::sync::Arc;
    ///
    /// let values = Arc::<[u32]>::new_zeroed_slice(3);
    /// let values = unsafe { values.assume_init() };
    ///
    /// assert_eq!(*values, [0, 0, 0])
    /// ```
    ///
    /// [zeroed]: ../../std/mem/union.MaybeUninit.html#method.zeroed
    #[cfg(not(no_global_oom_handling))]
    #[unstable(feature = "new_uninit", issue = "63291")]
    pub fn new_zeroed_slice(len: usize) -> Arc<[mem::MaybeUninit<T>]> {
}
}

impl<T> Arc<mem::MaybeUninit<T>> {
    /// Converts to `Arc<T>`.
    ///
    /// # Safety
    ///
    /// As with [`MaybeUninit::assume_init`],
    /// it is up to the caller to guarantee that the inner value
    /// really is in an initialized state.
    /// Calling this when the content is not yet fully initialized
    /// causes immediate undefined behavior.
    ///
    /// [`MaybeUninit::assume_init`]: ../../std/mem/union.MaybeUninit.html#method.assume_init
    ///
    /// # Examples
    ///
    /// ```
    /// #![feature(new_uninit)]
    /// #![feature(get_mut_unchecked)]
    ///
    /// use std::sync::Arc;
    ///
    /// let mut five = Arc::<u32>::new_uninit();
    ///
    /// let five = unsafe {
    ///     // Deferred initialization:
    ///     Arc::get_mut_unchecked(&mut five).as_mut_ptr().write(5);
    ///
    ///     five.assume_init()
    /// };
    ///
    /// assert_eq!(*five, 5)
    /// ```
    #[unstable(feature = "new_uninit", issue = "63291")]
    #[inline]
    pub unsafe fn assume_init(self) -> Arc<T> {
}
}

impl<T> Arc<[mem::MaybeUninit<T>]> {
    /// Converts to `Arc<[T]>`.
    ///
    /// # Safety
    ///
    /// As with [`MaybeUninit::assume_init`],
    /// it is up to the caller to guarantee that the inner value
    /// really is in an initialized state.
    /// Calling this when the content is not yet fully initialized
    /// causes immediate undefined behavior.
    ///
    /// [`MaybeUninit::assume_init`]: ../../std/mem/union.MaybeUninit.html#method.assume_init
    ///
    /// # Examples
    ///
    /// ```
    /// #![feature(new_uninit)]
    /// #![feature(get_mut_unchecked)]
    ///
    /// use std::sync::Arc;
    ///
    /// let mut values = Arc::<[u32]>::new_uninit_slice(3);
    ///
    /// let values = unsafe {
    ///     // Deferred initialization:
    ///     Arc::get_mut_unchecked(&mut values)[0].as_mut_ptr().write(1);
    ///     Arc::get_mut_unchecked(&mut values)[1].as_mut_ptr().write(2);
    ///     Arc::get_mut_unchecked(&mut values)[2].as_mut_ptr().write(3);
    ///
    ///     values.assume_init()
    /// };
    ///
    /// assert_eq!(*values, [1, 2, 3])
    /// ```
    #[unstable(feature = "new_uninit", issue = "63291")]
    #[inline]
    pub unsafe fn assume_init(self) -> Arc<[T]> {
}
}

impl<T: ?Sized> Arc<T> {
    /// Consumes the `Arc`, returning the wrapped pointer.
    ///
    /// To avoid a memory leak the pointer must be converted back to an `Arc` using
    /// [`Arc::from_raw`].
    ///
    /// # Examples
    ///
    /// ```
    /// use std::sync::Arc;
    ///
    /// let x = Arc::new("hello".to_owned());
    /// let x_ptr = Arc::into_raw(x);
    /// assert_eq!(unsafe { &*x_ptr }, "hello");
    /// ```
    #[stable(feature = "rc_raw", since = "1.17.0")]
    pub fn into_raw(this: Self) -> *const T {
}

    /// Provides a raw pointer to the data.
    ///
    /// The counts are not affected in any way and the `Arc` is not consumed. The pointer is valid for
    /// as long as there are strong counts in the `Arc`.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::sync::Arc;
    ///
    /// let x = Arc::new("hello".to_owned());
    /// let y = Arc::clone(&x);
    /// let x_ptr = Arc::as_ptr(&x);
    /// assert_eq!(x_ptr, Arc::as_ptr(&y));
    /// assert_eq!(unsafe { &*x_ptr }, "hello");
    /// ```
    #[stable(feature = "rc_as_ptr", since = "1.45.0")]
    pub fn as_ptr(this: &Self) -> *const T {
}

    /// Constructs an `Arc<T>` from a raw pointer.
    ///
    /// The raw pointer must have been previously returned by a call to
    /// [`Arc<U>::into_raw`][into_raw] where `U` must have the same size and
    /// alignment as `T`. This is trivially true if `U` is `T`.
    /// Note that if `U` is not `T` but has the same size and alignment, this is
    /// basically like transmuting references of different types. See
    /// [`mem::transmute`][transmute] for more information on what
    /// restrictions apply in this case.
    ///
    /// The user of `from_raw` has to make sure a specific value of `T` is only
    /// dropped once.
    ///
    /// This function is unsafe because improper use may lead to memory unsafety,
    /// even if the returned `Arc<T>` is never accessed.
    ///
    /// [into_raw]: Arc::into_raw
    /// [transmute]: core::mem::transmute
    ///
    /// # Examples
    ///
    /// ```
    /// use std::sync::Arc;
    ///
    /// let x = Arc::new("hello".to_owned());
    /// let x_ptr = Arc::into_raw(x);
    ///
    /// unsafe {
    ///     // Convert back to an `Arc` to prevent leak.
    ///     let x = Arc::from_raw(x_ptr);
    ///     assert_eq!(&*x, "hello");
    ///
    ///     // Further calls to `Arc::from_raw(x_ptr)` would be memory-unsafe.
    /// }
    ///
    /// // The memory was freed when `x` went out of scope above, so `x_ptr` is now dangling!
    /// ```
    #[stable(feature = "rc_raw", since = "1.17.0")]
    pub unsafe fn from_raw(ptr: *const T) -> Self {
}

    /// Creates a new [`Weak`] pointer to this allocation.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::sync::Arc;
    ///
    /// let five = Arc::new(5);
    ///
    /// let weak_five = Arc::downgrade(&five);
    /// ```
    #[stable(feature = "arc_weak", since = "1.4.0")]
    pub fn downgrade(this: &Self) -> Weak<T> {
}

    /// Gets the number of [`Weak`] pointers to this allocation.
    ///
    /// # Safety
    ///
    /// This method by itself is safe, but using it correctly requires extra care.
    /// Another thread can change the weak count at any time,
    /// including potentially between calling this method and acting on the result.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::sync::Arc;
    ///
    /// let five = Arc::new(5);
    /// let _weak_five = Arc::downgrade(&five);
    ///
    /// // This assertion is deterministic because we haven't shared
    /// // the `Arc` or `Weak` between threads.
    /// assert_eq!(1, Arc::weak_count(&five));
    /// ```
    #[inline]
    #[stable(feature = "arc_counts", since = "1.15.0")]
    pub fn weak_count(this: &Self) -> usize {
}

    /// Gets the number of strong (`Arc`) pointers to this allocation.
    ///
    /// # Safety
    ///
    /// This method by itself is safe, but using it correctly requires extra care.
    /// Another thread can change the strong count at any time,
    /// including potentially between calling this method and acting on the result.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::sync::Arc;
    ///
    /// let five = Arc::new(5);
    /// let _also_five = Arc::clone(&five);
    ///
    /// // This assertion is deterministic because we haven't shared
    /// // the `Arc` between threads.
    /// assert_eq!(2, Arc::strong_count(&five));
    /// ```
    #[inline]
    #[stable(feature = "arc_counts", since = "1.15.0")]
    pub fn strong_count(this: &Self) -> usize {
}

    /// Increments the strong reference count on the `Arc<T>` associated with the
    /// provided pointer by one.
    ///
    /// # Safety
    ///
    /// The pointer must have been obtained through `Arc::into_raw`, and the
    /// associated `Arc` instance must be valid (i.e. the strong count must be at
    /// least 1) for the duration of this method.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::sync::Arc;
    ///
    /// let five = Arc::new(5);
    ///
    /// unsafe {
    ///     let ptr = Arc::into_raw(five);
    ///     Arc::increment_strong_count(ptr);
    ///
    ///     // This assertion is deterministic because we haven't shared
    ///     // the `Arc` between threads.
    ///     let five = Arc::from_raw(ptr);
    ///     assert_eq!(2, Arc::strong_count(&five));
    /// }
    /// ```
    #[inline]
    #[stable(feature = "arc_mutate_strong_count", since = "1.51.0")]
    pub unsafe fn increment_strong_count(ptr: *const T) {
}

    /// Decrements the strong reference count on the `Arc<T>` associated with the
    /// provided pointer by one.
    ///
    /// # Safety
    ///
    /// The pointer must have been obtained through `Arc::into_raw`, and the
    /// associated `Arc` instance must be valid (i.e. the strong count must be at
    /// least 1) when invoking this method. This method can be used to release the final
    /// `Arc` and backing storage, but **should not** be called after the final `Arc` has been
    /// released.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::sync::Arc;
    ///
    /// let five = Arc::new(5);
    ///
    /// unsafe {
    ///     let ptr = Arc::into_raw(five);
    ///     Arc::increment_strong_count(ptr);
    ///
    ///     // Those assertions are deterministic because we haven't shared
    ///     // the `Arc` between threads.
    ///     let five = Arc::from_raw(ptr);
    ///     assert_eq!(2, Arc::strong_count(&five));
    ///     Arc::decrement_strong_count(ptr);
    ///     assert_eq!(1, Arc::strong_count(&five));
    /// }
    /// ```
    #[inline]
    #[stable(feature = "arc_mutate_strong_count", since = "1.51.0")]
    pub unsafe fn decrement_strong_count(ptr: *const T) {
}

    #[inline]
    fn inner(&self) -> &ArcInner<T> {
}

    // Non-inlined part of `drop`.
    #[inline(never)]
    unsafe fn drop_slow(&mut self) {
}

    #[inline]
    #[stable(feature = "ptr_eq", since = "1.17.0")]
    /// Returns `true` if the two `Arc`s point to the same allocation
    /// (in a vein similar to [`ptr::eq`]).
    ///
    /// # Examples
    ///
    /// ```
    /// use std::sync::Arc;
    ///
    /// let five = Arc::new(5);
    /// let same_five = Arc::clone(&five);
    /// let other_five = Arc::new(5);
    ///
    /// assert!(Arc::ptr_eq(&five, &same_five));
    /// assert!(!Arc::ptr_eq(&five, &other_five));
    /// ```
    ///
    /// [`ptr::eq`]: core::ptr::eq
    pub fn ptr_eq(this: &Self, other: &Self) -> bool {
}
}

impl<T: ?Sized> Arc<T> {
    /// Allocates an `ArcInner<T>` with sufficient space for
    /// a possibly-unsized inner value where the value has the layout provided.
    ///
    /// The function `mem_to_arcinner` is called with the data pointer
    /// and must return back a (potentially fat)-pointer for the `ArcInner<T>`.
    #[cfg(not(no_global_oom_handling))]
    unsafe fn allocate_for_layout(
        value_layout: Layout,
        allocate: impl FnOnce(Layout) -> Result<NonNull<[u8]>, AllocError>,
        mem_to_arcinner: impl FnOnce(*mut u8) -> *mut ArcInner<T>,
    ) -> *mut ArcInner<T> {
}

    /// Allocates an `ArcInner<T>` with sufficient space for
    /// a possibly-unsized inner value where the value has the layout provided,
    /// returning an error if allocation fails.
    ///
    /// The function `mem_to_arcinner` is called with the data pointer
    /// and must return back a (potentially fat)-pointer for the `ArcInner<T>`.
    unsafe fn try_allocate_for_layout(
        value_layout: Layout,
        allocate: impl FnOnce(Layout) -> Result<NonNull<[u8]>, AllocError>,
        mem_to_arcinner: impl FnOnce(*mut u8) -> *mut ArcInner<T>,
    ) -> Result<*mut ArcInner<T>, AllocError> {
}

    /// Allocates an `ArcInner<T>` with sufficient space for an unsized inner value.
    #[cfg(not(no_global_oom_handling))]
    unsafe fn allocate_for_ptr(ptr: *const T) -> *mut ArcInner<T> {
}

    #[cfg(not(no_global_oom_handling))]
    fn from_box(v: Box<T>) -> Arc<T> {
}
}

impl<T> Arc<[T]> {
    /// Allocates an `ArcInner<[T]>` with the given length.
    #[cfg(not(no_global_oom_handling))]
    unsafe fn allocate_for_slice(len: usize) -> *mut ArcInner<[T]> {
}

    /// Copy elements from slice into newly allocated Arc<\[T\]>
    ///
    /// Unsafe because the caller must either take ownership or bind `T: Copy`.
    #[cfg(not(no_global_oom_handling))]
    unsafe fn copy_from_slice(v: &[T]) -> Arc<[T]> {
}

    /// Constructs an `Arc<[T]>` from an iterator known to be of a certain size.
    ///
    /// Behavior is undefined should the size be wrong.
    #[cfg(not(no_global_oom_handling))]
    unsafe fn from_iter_exact(iter: impl iter::Iterator<Item = T>, len: usize) -> Arc<[T]> {
}
}

/// Specialization trait used for `From<&[T]>`.
#[cfg(not(no_global_oom_handling))]
trait ArcFromSlice<T> {
    fn from_slice(slice: &[T]) -> Self;
}

#[cfg(not(no_global_oom_handling))]
impl<T: Clone> ArcFromSlice<T> for Arc<[T]> {
    #[inline]
    default fn from_slice(v: &[T]) -> Self {
}
}

#[cfg(not(no_global_oom_handling))]
impl<T: Copy> ArcFromSlice<T> for Arc<[T]> {
    #[inline]
    fn from_slice(v: &[T]) -> Self {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<T: ?Sized> Clone for Arc<T> {
    /// Makes a clone of the `Arc` pointer.
    ///
    /// This creates another pointer to the same allocation, increasing the
    /// strong reference count.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::sync::Arc;
    ///
    /// let five = Arc::new(5);
    ///
    /// let _ = Arc::clone(&five);
    /// ```
    #[inline]
    fn clone(&self) -> Arc<T> {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<T: ?Sized> Deref for Arc<T> {
    type Target = T;

    #[inline]
    fn deref(&self) -> &T {
}
}

#[unstable(feature = "receiver_trait", issue = "none")]
impl<T: ?Sized> Receiver for Arc<T> {}

impl<T: Clone> Arc<T> {
    /// Makes a mutable reference into the given `Arc`.
    ///
    /// If there are other `Arc` pointers to the same allocation, then `make_mut` will
    /// [`clone`] the inner value to a new allocation to ensure unique ownership.  This is also
    /// referred to as clone-on-write.
    ///
    /// However, if there are no other `Arc` pointers to this allocation, but some [`Weak`]
    /// pointers, then the [`Weak`] pointers will be disassociated and the inner value will not
    /// be cloned.
    ///
    /// See also [`get_mut`], which will fail rather than cloning the inner value
    /// or diassociating [`Weak`] pointers.
    ///
    /// [`clone`]: Clone::clone
    /// [`get_mut`]: Arc::get_mut
    ///
    /// # Examples
    ///
    /// ```
    /// use std::sync::Arc;
    ///
    /// let mut data = Arc::new(5);
    ///
    /// *Arc::make_mut(&mut data) += 1;         // Won't clone anything
    /// let mut other_data = Arc::clone(&data); // Won't clone inner data
    /// *Arc::make_mut(&mut data) += 1;         // Clones inner data
    /// *Arc::make_mut(&mut data) += 1;         // Won't clone anything
    /// *Arc::make_mut(&mut other_data) *= 2;   // Won't clone anything
    ///
    /// // Now `data` and `other_data` point to different allocations.
    /// assert_eq!(*data, 8);
    /// assert_eq!(*other_data, 12);
    /// ```
    ///
    /// [`Weak`] pointers will be disassociated:
    ///
    /// ```
    /// use std::sync::Arc;
    ///
    /// let mut data = Arc::new(75);
    /// let weak = Arc::downgrade(&data);
    ///
    /// assert!(75 == *data);
    /// assert!(75 == *weak.upgrade().unwrap());
    ///
    /// *Arc::make_mut(&mut data) += 1;
    ///
    /// assert!(76 == *data);
    /// assert!(weak.upgrade().is_none());
    /// ```
    #[cfg(not(no_global_oom_handling))]
    #[inline]
    #[stable(feature = "arc_unique", since = "1.4.0")]
    pub fn make_mut(this: &mut Self) -> &mut T {
}
}

impl<T: ?Sized> Arc<T> {
    /// Returns a mutable reference into the given `Arc`, if there are
    /// no other `Arc` or [`Weak`] pointers to the same allocation.
    ///
    /// Returns [`None`] otherwise, because it is not safe to
    /// mutate a shared value.
    ///
    /// See also [`make_mut`][make_mut], which will [`clone`][clone]
    /// the inner value when there are other `Arc` pointers.
    ///
    /// [make_mut]: Arc::make_mut
    /// [clone]: Clone::clone
    ///
    /// # Examples
    ///
    /// ```
    /// use std::sync::Arc;
    ///
    /// let mut x = Arc::new(3);
    /// *Arc::get_mut(&mut x).unwrap() = 4;
    /// assert_eq!(*x, 4);
    ///
    /// let _y = Arc::clone(&x);
    /// assert!(Arc::get_mut(&mut x).is_none());
    /// ```
    #[inline]
    #[stable(feature = "arc_unique", since = "1.4.0")]
    pub fn get_mut(this: &mut Self) -> Option<&mut T> {
}

    /// Returns a mutable reference into the given `Arc`,
    /// without any check.
    ///
    /// See also [`get_mut`], which is safe and does appropriate checks.
    ///
    /// [`get_mut`]: Arc::get_mut
    ///
    /// # Safety
    ///
    /// Any other `Arc` or [`Weak`] pointers to the same allocation must not be dereferenced
    /// for the duration of the returned borrow.
    /// This is trivially the case if no such pointers exist,
    /// for example immediately after `Arc::new`.
    ///
    /// # Examples
    ///
    /// ```
    /// #![feature(get_mut_unchecked)]
    ///
    /// use std::sync::Arc;
    ///
    /// let mut x = Arc::new(String::new());
    /// unsafe {
    ///     Arc::get_mut_unchecked(&mut x).push_str("foo")
    /// }
    /// assert_eq!(*x, "foo");
    /// ```
    #[inline]
    #[unstable(feature = "get_mut_unchecked", issue = "63292")]
    pub unsafe fn get_mut_unchecked(this: &mut Self) -> &mut T {
}

    /// Determine whether this is the unique reference (including weak refs) to
    /// the underlying data.
    ///
    /// Note that this requires locking the weak ref count.
    fn is_unique(&mut self) -> bool {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
unsafe impl<#[may_dangle] T: ?Sized> Drop for Arc<T> {
    /// Drops the `Arc`.
    ///
    /// This will decrement the strong reference count. If the strong reference
    /// count reaches zero then the only other references (if any) are
    /// [`Weak`], so we `drop` the inner value.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::sync::Arc;
    ///
    /// struct Foo;
    ///
    /// impl Drop for Foo {
    ///     fn drop(&mut self) {
    ///         println!("dropped!");
    ///     }
    /// }
    ///
    /// let foo  = Arc::new(Foo);
    /// let foo2 = Arc::clone(&foo);
    ///
    /// drop(foo);    // Doesn't print anything
    /// drop(foo2);   // Prints "dropped!"
    /// ```
    #[inline]
    fn drop(&mut self) {
}
}

impl Arc<dyn Any + Send + Sync> {
    #[inline]
    #[stable(feature = "rc_downcast", since = "1.29.0")]
    /// Attempt to downcast the `Arc<dyn Any + Send + Sync>` to a concrete type.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::any::Any;
    /// use std::sync::Arc;
    ///
    /// fn print_if_string(value: Arc<dyn Any + Send + Sync>) {
    ///     if let Ok(string) = value.downcast::<String>() {
    ///         println!("String ({}): {}", string.len(), string);
    ///     }
    /// }
    ///
    /// let my_string = "Hello World".to_string();
    /// print_if_string(Arc::new(my_string));
    /// print_if_string(Arc::new(0i8));
    /// ```
    pub fn downcast<T>(self) -> Result<Arc<T>, Self>
    where
        T: Any + Send + Sync + 'static,
    {
}
}

impl<T> Weak<T> {
    /// Constructs a new `Weak<T>`, without allocating any memory.
    /// Calling [`upgrade`] on the return value always gives [`None`].
    ///
    /// [`upgrade`]: Weak::upgrade
    ///
    /// # Examples
    ///
    /// ```
    /// use std::sync::Weak;
    ///
    /// let empty: Weak<i64> = Weak::new();
    /// assert!(empty.upgrade().is_none());
    /// ```
    #[stable(feature = "downgraded_weak", since = "1.10.0")]
    pub fn new() -> Weak<T> {
}
}

/// Helper type to allow accessing the reference counts without
/// making any assertions about the data field.
struct WeakInner<'a> {
    weak: &'a atomic::AtomicUsize,
    strong: &'a atomic::AtomicUsize,
}

impl<T: ?Sized> Weak<T> {
    /// Returns a raw pointer to the object `T` pointed to by this `Weak<T>`.
    ///
    /// The pointer is valid only if there are some strong references. The pointer may be dangling,
    /// unaligned or even [`null`] otherwise.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::sync::Arc;
    /// use std::ptr;
    ///
    /// let strong = Arc::new("hello".to_owned());
    /// let weak = Arc::downgrade(&strong);
    /// // Both point to the same object
    /// assert!(ptr::eq(&*strong, weak.as_ptr()));
    /// // The strong here keeps it alive, so we can still access the object.
    /// assert_eq!("hello", unsafe { &*weak.as_ptr() });
    ///
    /// drop(strong);
    /// // But not any more. We can do weak.as_ptr(), but accessing the pointer would lead to
    /// // undefined behaviour.
    /// // assert_eq!("hello", unsafe { &*weak.as_ptr() });
    /// ```
    ///
    /// [`null`]: core::ptr::null
    #[stable(feature = "weak_into_raw", since = "1.45.0")]
    pub fn as_ptr(&self) -> *const T {
}

    /// Consumes the `Weak<T>` and turns it into a raw pointer.
    ///
    /// This converts the weak pointer into a raw pointer, while still preserving the ownership of
    /// one weak reference (the weak count is not modified by this operation). It can be turned
    /// back into the `Weak<T>` with [`from_raw`].
    ///
    /// The same restrictions of accessing the target of the pointer as with
    /// [`as_ptr`] apply.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::sync::{Arc, Weak};
    ///
    /// let strong = Arc::new("hello".to_owned());
    /// let weak = Arc::downgrade(&strong);
    /// let raw = weak.into_raw();
    ///
    /// assert_eq!(1, Arc::weak_count(&strong));
    /// assert_eq!("hello", unsafe { &*raw });
    ///
    /// drop(unsafe { Weak::from_raw(raw) });
    /// assert_eq!(0, Arc::weak_count(&strong));
    /// ```
    ///
    /// [`from_raw`]: Weak::from_raw
    /// [`as_ptr`]: Weak::as_ptr
    #[stable(feature = "weak_into_raw", since = "1.45.0")]
    pub fn into_raw(self) -> *const T {
}

    /// Converts a raw pointer previously created by [`into_raw`] back into `Weak<T>`.
    ///
    /// This can be used to safely get a strong reference (by calling [`upgrade`]
    /// later) or to deallocate the weak count by dropping the `Weak<T>`.
    ///
    /// It takes ownership of one weak reference (with the exception of pointers created by [`new`],
    /// as these don't own anything; the method still works on them).
    ///
    /// # Safety
    ///
    /// The pointer must have originated from the [`into_raw`] and must still own its potential
    /// weak reference.
    ///
    /// It is allowed for the strong count to be 0 at the time of calling this. Nevertheless, this
    /// takes ownership of one weak reference currently represented as a raw pointer (the weak
    /// count is not modified by this operation) and therefore it must be paired with a previous
    /// call to [`into_raw`].
    /// # Examples
    ///
    /// ```
    /// use std::sync::{Arc, Weak};
    ///
    /// let strong = Arc::new("hello".to_owned());
    ///
    /// let raw_1 = Arc::downgrade(&strong).into_raw();
    /// let raw_2 = Arc::downgrade(&strong).into_raw();
    ///
    /// assert_eq!(2, Arc::weak_count(&strong));
    ///
    /// assert_eq!("hello", &*unsafe { Weak::from_raw(raw_1) }.upgrade().unwrap());
    /// assert_eq!(1, Arc::weak_count(&strong));
    ///
    /// drop(strong);
    ///
    /// // Decrement the last weak count.
    /// assert!(unsafe { Weak::from_raw(raw_2) }.upgrade().is_none());
    /// ```
    ///
    /// [`new`]: Weak::new
    /// [`into_raw`]: Weak::into_raw
    /// [`upgrade`]: Weak::upgrade
    /// [`forget`]: std::mem::forget
    #[stable(feature = "weak_into_raw", since = "1.45.0")]
    pub unsafe fn from_raw(ptr: *const T) -> Self {
}
}

impl<T: ?Sized> Weak<T> {
    /// Attempts to upgrade the `Weak` pointer to an [`Arc`], delaying
    /// dropping of the inner value if successful.
    ///
    /// Returns [`None`] if the inner value has since been dropped.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::sync::Arc;
    ///
    /// let five = Arc::new(5);
    ///
    /// let weak_five = Arc::downgrade(&five);
    ///
    /// let strong_five: Option<Arc<_>> = weak_five.upgrade();
    /// assert!(strong_five.is_some());
    ///
    /// // Destroy all strong pointers.
    /// drop(strong_five);
    /// drop(five);
    ///
    /// assert!(weak_five.upgrade().is_none());
    /// ```
    #[stable(feature = "arc_weak", since = "1.4.0")]
    pub fn upgrade(&self) -> Option<Arc<T>> {
}

    /// Gets the number of strong (`Arc`) pointers pointing to this allocation.
    ///
    /// If `self` was created using [`Weak::new`], this will return 0.
    #[stable(feature = "weak_counts", since = "1.41.0")]
    pub fn strong_count(&self) -> usize {
}

    /// Gets an approximation of the number of `Weak` pointers pointing to this
    /// allocation.
    ///
    /// If `self` was created using [`Weak::new`], or if there are no remaining
    /// strong pointers, this will return 0.
    ///
    /// # Accuracy
    ///
    /// Due to implementation details, the returned value can be off by 1 in
    /// either direction when other threads are manipulating any `Arc`s or
    /// `Weak`s pointing to the same allocation.
    #[stable(feature = "weak_counts", since = "1.41.0")]
    pub fn weak_count(&self) -> usize {
}

    /// Returns `None` when the pointer is dangling and there is no allocated `ArcInner`,
    /// (i.e., when this `Weak` was created by `Weak::new`).
    #[inline]
    fn inner(&self) -> Option<WeakInner<'_>> {
}

    /// Returns `true` if the two `Weak`s point to the same allocation (similar to
    /// [`ptr::eq`]), or if both don't point to any allocation
    /// (because they were created with `Weak::new()`).
    ///
    /// # Notes
    ///
    /// Since this compares pointers it means that `Weak::new()` will equal each
    /// other, even though they don't point to any allocation.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::sync::Arc;
    ///
    /// let first_rc = Arc::new(5);
    /// let first = Arc::downgrade(&first_rc);
    /// let second = Arc::downgrade(&first_rc);
    ///
    /// assert!(first.ptr_eq(&second));
    ///
    /// let third_rc = Arc::new(5);
    /// let third = Arc::downgrade(&third_rc);
    ///
    /// assert!(!first.ptr_eq(&third));
    /// ```
    ///
    /// Comparing `Weak::new`.
    ///
    /// ```
    /// use std::sync::{Arc, Weak};
    ///
    /// let first = Weak::new();
    /// let second = Weak::new();
    /// assert!(first.ptr_eq(&second));
    ///
    /// let third_rc = Arc::new(());
    /// let third = Arc::downgrade(&third_rc);
    /// assert!(!first.ptr_eq(&third));
    /// ```
    ///
    /// [`ptr::eq`]: core::ptr::eq
    #[inline]
    #[stable(feature = "weak_ptr_eq", since = "1.39.0")]
    pub fn ptr_eq(&self, other: &Self) -> bool {
}
}

#[stable(feature = "arc_weak", since = "1.4.0")]
impl<T: ?Sized> Clone for Weak<T> {
    /// Makes a clone of the `Weak` pointer that points to the same allocation.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::sync::{Arc, Weak};
    ///
    /// let weak_five = Arc::downgrade(&Arc::new(5));
    ///
    /// let _ = Weak::clone(&weak_five);
    /// ```
    #[inline]
    fn clone(&self) -> Weak<T> {
}
}

#[stable(feature = "downgraded_weak", since = "1.10.0")]
impl<T> Default for Weak<T> {
    /// Constructs a new `Weak<T>`, without allocating memory.
    /// Calling [`upgrade`] on the return value always
    /// gives [`None`].
    ///
    /// [`upgrade`]: Weak::upgrade
    ///
    /// # Examples
    ///
    /// ```
    /// use std::sync::Weak;
    ///
    /// let empty: Weak<i64> = Default::default();
    /// assert!(empty.upgrade().is_none());
    /// ```
    fn default() -> Weak<T> {
}
}

#[stable(feature = "arc_weak", since = "1.4.0")]
unsafe impl<#[may_dangle] T: ?Sized> Drop for Weak<T> {
    /// Drops the `Weak` pointer.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::sync::{Arc, Weak};
    ///
    /// struct Foo;
    ///
    /// impl Drop for Foo {
    ///     fn drop(&mut self) {
    ///         println!("dropped!");
    ///     }
    /// }
    ///
    /// let foo = Arc::new(Foo);
    /// let weak_foo = Arc::downgrade(&foo);
    /// let other_weak_foo = Weak::clone(&weak_foo);
    ///
    /// drop(weak_foo);   // Doesn't print anything
    /// drop(foo);        // Prints "dropped!"
    ///
    /// assert!(other_weak_foo.upgrade().is_none());
    /// ```
    fn drop(&mut self) {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
trait ArcEqIdent<T: ?Sized + PartialEq> {
    fn eq(&self, other: &Arc<T>) -> bool;
    fn ne(&self, other: &Arc<T>) -> bool;
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<T: ?Sized + PartialEq> ArcEqIdent<T> for Arc<T> {
    #[inline]
    default fn eq(&self, other: &Arc<T>) -> bool {
}
    #[inline]
    default fn ne(&self, other: &Arc<T>) -> bool {
}
}

/// We're doing this specialization here, and not as a more general optimization on `&T`, because it
/// would otherwise add a cost to all equality checks on refs. We assume that `Arc`s are used to
/// store large values, that are slow to clone, but also heavy to check for equality, causing this
/// cost to pay off more easily. It's also more likely to have two `Arc` clones, that point to
/// the same value, than two `&T`s.
///
/// We can only do this when `T: Eq` as a `PartialEq` might be deliberately irreflexive.
#[stable(feature = "rust1", since = "1.0.0")]
impl<T: ?Sized + crate::rc::MarkerEq> ArcEqIdent<T> for Arc<T> {
    #[inline]
    fn eq(&self, other: &Arc<T>) -> bool {
}

    #[inline]
    fn ne(&self, other: &Arc<T>) -> bool {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<T: ?Sized + PartialEq> PartialEq for Arc<T> {
    /// Equality for two `Arc`s.
    ///
    /// Two `Arc`s are equal if their inner values are equal, even if they are
    /// stored in different allocation.
    ///
    /// If `T` also implements `Eq` (implying reflexivity of equality),
    /// two `Arc`s that point to the same allocation are always equal.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::sync::Arc;
    ///
    /// let five = Arc::new(5);
    ///
    /// assert!(five == Arc::new(5));
    /// ```
    #[inline]
    fn eq(&self, other: &Arc<T>) -> bool {
}

    /// Inequality for two `Arc`s.
    ///
    /// Two `Arc`s are unequal if their inner values are unequal.
    ///
    /// If `T` also implements `Eq` (implying reflexivity of equality),
    /// two `Arc`s that point to the same value are never unequal.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::sync::Arc;
    ///
    /// let five = Arc::new(5);
    ///
    /// assert!(five != Arc::new(6));
    /// ```
    #[inline]
    fn ne(&self, other: &Arc<T>) -> bool {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<T: ?Sized + PartialOrd> PartialOrd for Arc<T> {
    /// Partial comparison for two `Arc`s.
    ///
    /// The two are compared by calling `partial_cmp()` on their inner values.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::sync::Arc;
    /// use std::cmp::Ordering;
    ///
    /// let five = Arc::new(5);
    ///
    /// assert_eq!(Some(Ordering::Less), five.partial_cmp(&Arc::new(6)));
    /// ```
    fn partial_cmp(&self, other: &Arc<T>) -> Option<Ordering> {
}

    /// Less-than comparison for two `Arc`s.
    ///
    /// The two are compared by calling `<` on their inner values.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::sync::Arc;
    ///
    /// let five = Arc::new(5);
    ///
    /// assert!(five < Arc::new(6));
    /// ```
    fn lt(&self, other: &Arc<T>) -> bool {
}

    /// 'Less than or equal to' comparison for two `Arc`s.
    ///
    /// The two are compared by calling `<=` on their inner values.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::sync::Arc;
    ///
    /// let five = Arc::new(5);
    ///
    /// assert!(five <= Arc::new(5));
    /// ```
    fn le(&self, other: &Arc<T>) -> bool {
}

    /// Greater-than comparison for two `Arc`s.
    ///
    /// The two are compared by calling `>` on their inner values.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::sync::Arc;
    ///
    /// let five = Arc::new(5);
    ///
    /// assert!(five > Arc::new(4));
    /// ```
    fn gt(&self, other: &Arc<T>) -> bool {
}

    /// 'Greater than or equal to' comparison for two `Arc`s.
    ///
    /// The two are compared by calling `>=` on their inner values.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::sync::Arc;
    ///
    /// let five = Arc::new(5);
    ///
    /// assert!(five >= Arc::new(5));
    /// ```
    fn ge(&self, other: &Arc<T>) -> bool {
}
}
#[stable(feature = "rust1", since = "1.0.0")]
impl<T: ?Sized + Ord> Ord for Arc<T> {
    /// Comparison for two `Arc`s.
    ///
    /// The two are compared by calling `cmp()` on their inner values.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::sync::Arc;
    /// use std::cmp::Ordering;
    ///
    /// let five = Arc::new(5);
    ///
    /// assert_eq!(Ordering::Less, five.cmp(&Arc::new(6)));
    /// ```
    fn cmp(&self, other: &Arc<T>) -> Ordering {
}
}
#[stable(feature = "rust1", since = "1.0.0")]
impl<T: ?Sized + Eq> Eq for Arc<T> {}

#[stable(feature = "rust1", since = "1.0.0")]
impl<T: ?Sized + fmt::Display> fmt::Display for Arc<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<T: ?Sized + fmt::Debug> fmt::Debug for Arc<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<T: ?Sized> fmt::Pointer for Arc<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
}
}

#[cfg(not(no_global_oom_handling))]
#[stable(feature = "rust1", since = "1.0.0")]
impl<T: Default> Default for Arc<T> {
    /// Creates a new `Arc<T>`, with the `Default` value for `T`.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::sync::Arc;
    ///
    /// let x: Arc<i32> = Default::default();
    /// assert_eq!(*x, 0);
    /// ```
    fn default() -> Arc<T> {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<T: ?Sized + Hash> Hash for Arc<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
}
}

#[cfg(not(no_global_oom_handling))]
#[stable(feature = "from_for_ptrs", since = "1.6.0")]
impl<T> From<T> for Arc<T> {
    /// Converts a `T` into an `Arc<T>`
    ///
    /// The conversion moves the value into a
    /// newly allocated `Arc`. It is equivalent to
    /// calling `Arc::new(t)`.
    ///
    /// # Example
    /// ```rust
    /// # use std::sync::Arc;
    /// let x = 5;
    /// let arc = Arc::new(5);
    ///
    /// assert_eq!(Arc::from(x), arc);
    /// ```
    fn from(t: T) -> Self {
}
}

#[cfg(not(no_global_oom_handling))]
#[stable(feature = "shared_from_slice", since = "1.21.0")]
impl<T: Clone> From<&[T]> for Arc<[T]> {
    /// Allocate a reference-counted slice and fill it by cloning `v`'s items.
    ///
    /// # Example
    ///
    /// ```
    /// # use std::sync::Arc;
    /// let original: &[i32] = &[1, 2, 3];
    /// let shared: Arc<[i32]> = Arc::from(original);
    /// assert_eq!(&[1, 2, 3], &shared[..]);
    /// ```
    #[inline]
    fn from(v: &[T]) -> Arc<[T]> {
}
}

#[cfg(not(no_global_oom_handling))]
#[stable(feature = "shared_from_slice", since = "1.21.0")]
impl From<&str> for Arc<str> {
    /// Allocate a reference-counted `str` and copy `v` into it.
    ///
    /// # Example
    ///
    /// ```
    /// # use std::sync::Arc;
    /// let shared: Arc<str> = Arc::from("eggplant");
    /// assert_eq!("eggplant", &shared[..]);
    /// ```
    #[inline]
    fn from(v: &str) -> Arc<str> {
}
}

#[cfg(not(no_global_oom_handling))]
#[stable(feature = "shared_from_slice", since = "1.21.0")]
impl From<String> for Arc<str> {
    /// Allocate a reference-counted `str` and copy `v` into it.
    ///
    /// # Example
    ///
    /// ```
    /// # use std::sync::Arc;
    /// let unique: String = "eggplant".to_owned();
    /// let shared: Arc<str> = Arc::from(unique);
    /// assert_eq!("eggplant", &shared[..]);
    /// ```
    #[inline]
    fn from(v: String) -> Arc<str> {
}
}

#[cfg(not(no_global_oom_handling))]
#[stable(feature = "shared_from_slice", since = "1.21.0")]
impl<T: ?Sized> From<Box<T>> for Arc<T> {
    /// Move a boxed object to a new, reference-counted allocation.
    ///
    /// # Example
    ///
    /// ```
    /// # use std::sync::Arc;
    /// let unique: Box<str> = Box::from("eggplant");
    /// let shared: Arc<str> = Arc::from(unique);
    /// assert_eq!("eggplant", &shared[..]);
    /// ```
    #[inline]
    fn from(v: Box<T>) -> Arc<T> {
}
}

#[cfg(not(no_global_oom_handling))]
#[stable(feature = "shared_from_slice", since = "1.21.0")]
impl<T> From<Vec<T>> for Arc<[T]> {
    /// Allocate a reference-counted slice and move `v`'s items into it.
    ///
    /// # Example
    ///
    /// ```
    /// # use std::sync::Arc;
    /// let unique: Vec<i32> = vec![1, 2, 3];
    /// let shared: Arc<[i32]> = Arc::from(unique);
    /// assert_eq!(&[1, 2, 3], &shared[..]);
    /// ```
    #[inline]
    fn from(mut v: Vec<T>) -> Arc<[T]> {
}
}

#[stable(feature = "shared_from_cow", since = "1.45.0")]
impl<'a, B> From<Cow<'a, B>> for Arc<B>
where
    B: ToOwned + ?Sized,
    Arc<B>: From<&'a B> + From<B::Owned>,
{
    /// Create an atomically reference-counted pointer from
    /// a clone-on-write pointer by copying its content.
    ///
    /// # Example
    ///
    /// ```rust
    /// # use std::sync::Arc;
    /// # use std::borrow::Cow;
    /// let cow: Cow<str> = Cow::Borrowed("eggplant");
    /// let shared: Arc<str> = Arc::from(cow);
    /// assert_eq!("eggplant", &shared[..]);
    /// ```
    #[inline]
    fn from(cow: Cow<'a, B>) -> Arc<B> {
}
}

#[stable(feature = "boxed_slice_try_from", since = "1.43.0")]
impl<T, const N: usize> TryFrom<Arc<[T]>> for Arc<[T; N]> {
    type Error = Arc<[T]>;

    fn try_from(boxed_slice: Arc<[T]>) -> Result<Self, Self::Error> {
}
}

#[cfg(not(no_global_oom_handling))]
#[stable(feature = "shared_from_iter", since = "1.37.0")]
impl<T> iter::FromIterator<T> for Arc<[T]> {
    /// Takes each element in the `Iterator` and collects it into an `Arc<[T]>`.
    ///
    /// # Performance characteristics
    ///
    /// ## The general case
    ///
    /// In the general case, collecting into `Arc<[T]>` is done by first
    /// collecting into a `Vec<T>`. That is, when writing the following:
    ///
    /// ```rust
    /// # use std::sync::Arc;
    /// let evens: Arc<[u8]> = (0..10).filter(|&x| x % 2 == 0).collect();
    /// # assert_eq!(&*evens, &[0, 2, 4, 6, 8]);
    /// ```
    ///
    /// this behaves as if we wrote:
    ///
    /// ```rust
    /// # use std::sync::Arc;
    /// let evens: Arc<[u8]> = (0..10).filter(|&x| x % 2 == 0)
    ///     .collect::<Vec<_>>() // The first set of allocations happens here.
    ///     .into(); // A second allocation for `Arc<[T]>` happens here.
    /// # assert_eq!(&*evens, &[0, 2, 4, 6, 8]);
    /// ```
    ///
    /// This will allocate as many times as needed for constructing the `Vec<T>`
    /// and then it will allocate once for turning the `Vec<T>` into the `Arc<[T]>`.
    ///
    /// ## Iterators of known length
    ///
    /// When your `Iterator` implements `TrustedLen` and is of an exact size,
    /// a single allocation will be made for the `Arc<[T]>`. For example:
    ///
    /// ```rust
    /// # use std::sync::Arc;
    /// let evens: Arc<[u8]> = (0..10).collect(); // Just a single allocation happens here.
    /// # assert_eq!(&*evens, &*(0..10).collect::<Vec<_>>());
    /// ```
    fn from_iter<I: iter::IntoIterator<Item = T>>(iter: I) -> Self {
}
}

/// Specialization trait used for collecting into `Arc<[T]>`.
trait ToArcSlice<T>: Iterator<Item = T> + Sized {
    fn to_arc_slice(self) -> Arc<[T]>;
}

#[cfg(not(no_global_oom_handling))]
impl<T, I: Iterator<Item = T>> ToArcSlice<T> for I {
    default fn to_arc_slice(self) -> Arc<[T]> {
}
}

#[cfg(not(no_global_oom_handling))]
impl<T, I: iter::TrustedLen<Item = T>> ToArcSlice<T> for I {
    fn to_arc_slice(self) -> Arc<[T]> {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<T: ?Sized> borrow::Borrow<T> for Arc<T> {
    fn borrow(&self) -> &T {
}
}

#[stable(since = "1.5.0", feature = "smart_ptr_as_ref")]
impl<T: ?Sized> AsRef<T> for Arc<T> {
    fn as_ref(&self) -> &T {
}
}

#[stable(feature = "pin", since = "1.33.0")]
impl<T: ?Sized> Unpin for Arc<T> {}

/// Get the offset within an `ArcInner` for the payload behind a pointer.
///
/// # Safety
///
/// The pointer must point to (and have valid metadata for) a previously
/// valid instance of T, but the T is allowed to be dropped.
unsafe fn data_offset<T: ?Sized>(ptr: *const T) -> isize {
}

#[inline]
fn data_offset_align(align: usize) -> isize {
}
}
#[cfg(all(not(no_global_oom_handling), target_has_atomic = "ptr"))]
pub mod task {
#![stable(feature = "wake_trait", since = "1.51.0")]
//! Types and Traits for working with asynchronous tasks.
use core::mem::ManuallyDrop;
use core::task::{RawWaker, RawWakerVTable, Waker};

use crate::sync::Arc;

/// The implementation of waking a task on an executor.
///
/// This trait can be used to create a [`Waker`]. An executor can define an
/// implementation of this trait, and use that to construct a Waker to pass
/// to the tasks that are executed on that executor.
///
/// This trait is a memory-safe and ergonomic alternative to constructing a
/// [`RawWaker`]. It supports the common executor design in which the data used
/// to wake up a task is stored in an [`Arc`]. Some executors (especially
/// those for embedded systems) cannot use this API, which is why [`RawWaker`]
/// exists as an alternative for those systems.
///
/// [arc]: ../../std/sync/struct.Arc.html
///
/// # Examples
///
/// A basic `block_on` function that takes a future and runs it to completion on
/// the current thread.
///
/// **Note:** This example trades correctness for simplicity. In order to prevent
/// deadlocks, production-grade implementations will also need to handle
/// intermediate calls to `thread::unpark` as well as nested invocations.
///
/// ```rust
/// use std::future::Future;
/// use std::sync::Arc;
/// use std::task::{Context, Poll, Wake};
/// use std::thread::{self, Thread};
///
/// /// A waker that wakes up the current thread when called.
/// struct ThreadWaker(Thread);
///
/// impl Wake for ThreadWaker {
///     fn wake(self: Arc<Self>) {
///         self.0.unpark();
///     }
/// }
///
/// /// Run a future to completion on the current thread.
/// fn block_on<T>(fut: impl Future<Output = T>) -> T {
///     // Pin the future so it can be polled.
///     let mut fut = Box::pin(fut);
///
///     // Create a new context to be passed to the future.
///     let t = thread::current();
///     let waker = Arc::new(ThreadWaker(t)).into();
///     let mut cx = Context::from_waker(&waker);
///
///     // Run the future to completion.
///     loop {
///         match fut.as_mut().poll(&mut cx) {
///             Poll::Ready(res) => return res,
///             Poll::Pending => thread::park(),
///         }
///     }
/// }
///
/// block_on(async {
///     println!("Hi from inside a future!");
/// });
/// ```
#[stable(feature = "wake_trait", since = "1.51.0")]
pub trait Wake {
    /// Wake this task.
    #[stable(feature = "wake_trait", since = "1.51.0")]
    fn wake(self: Arc<Self>);

    /// Wake this task without consuming the waker.
    ///
    /// If an executor supports a cheaper way to wake without consuming the
    /// waker, it should override this method. By default, it clones the
    /// [`Arc`] and calls [`wake`] on the clone.
    ///
    /// [`wake`]: Wake::wake
    #[stable(feature = "wake_trait", since = "1.51.0")]
    fn wake_by_ref(self: &Arc<Self>) {
        self.clone().wake();
    }
}

#[stable(feature = "wake_trait", since = "1.51.0")]
impl<W: Wake + Send + Sync + 'static> From<Arc<W>> for Waker {
    /// Use a `Wake`-able type as a `Waker`.
    ///
    /// No heap allocations or atomic operations are used for this conversion.
    fn from(waker: Arc<W>) -> Waker {
}
}

#[stable(feature = "wake_trait", since = "1.51.0")]
impl<W: Wake + Send + Sync + 'static> From<Arc<W>> for RawWaker {
    /// Use a `Wake`-able type as a `RawWaker`.
    ///
    /// No heap allocations or atomic operations are used for this conversion.
    fn from(waker: Arc<W>) -> RawWaker {
}
}

// NB: This private function for constructing a RawWaker is used, rather than
// inlining this into the `From<Arc<W>> for RawWaker` impl, to ensure that
// the safety of `From<Arc<W>> for Waker` does not depend on the correct
// trait dispatch - instead both impls call this function directly and
// explicitly.
#[inline(always)]
fn raw_waker<W: Wake + Send + Sync + 'static>(waker: Arc<W>) -> RawWaker {
}
}
#[cfg(test)]
mod tests {
}
pub mod vec {
//! A contiguous growable array type with heap-allocated contents, written
//! `Vec<T>`.
//!
//! Vectors have `O(1)` indexing, amortized `O(1)` push (to the end) and
//! `O(1)` pop (from the end).
//!
//! Vectors ensure they never allocate more than `isize::MAX` bytes.
//!
//! # Examples
//!
//! You can explicitly create a [`Vec`] with [`Vec::new`]:
//!
//! ```
//! let v: Vec<i32> = Vec::new();
//! ```
//!
//! ...or by using the [`vec!`] macro:
//!
//! ```
//! let v: Vec<i32> = vec![];
//!
//! let v = vec![1, 2, 3, 4, 5];
//!
//! let v = vec![0; 10]; // ten zeroes
//! ```
//!
//! You can [`push`] values onto the end of a vector (which will grow the vector
//! as needed):
//!
//! ```
//! let mut v = vec![1, 2];
//!
//! v.push(3);
//! ```
//!
//! Popping values works in much the same way:
//!
//! ```
//! let mut v = vec![1, 2];
//!
//! let two = v.pop();
//! ```
//!
//! Vectors also support indexing (through the [`Index`] and [`IndexMut`] traits):
//!
//! ```
//! let mut v = vec![1, 2, 3];
//! let three = v[2];
//! v[1] = v[1] + 5;
//! ```
//!
//! [`push`]: Vec::push

#![stable(feature = "rust1", since = "1.0.0")]

#[cfg(not(no_global_oom_handling))]
use core::cmp;
use core::cmp::Ordering;
use core::convert::TryFrom;
use core::fmt;
use core::hash::{Hash, Hasher};
use core::intrinsics::{arith_offset, assume};
use core::iter;
#[cfg(not(no_global_oom_handling))]
use core::iter::FromIterator;
use core::marker::PhantomData;
use core::mem::{self, ManuallyDrop, MaybeUninit};
use core::ops::{self, Index, IndexMut, Range, RangeBounds};
use core::ptr::{self, NonNull};
use core::slice::{self, SliceIndex};

use crate::alloc::{Allocator, Global};
use crate::borrow::{Cow, ToOwned};
use crate::boxed::Box;
use crate::collections::TryReserveError;
use crate::raw_vec::RawVec;

#[unstable(feature = "drain_filter", reason = "recently added", issue = "43244")]
pub use self::drain_filter::DrainFilter;

mod drain_filter {
use crate::alloc::{Allocator, Global};
use core::ptr::{self};
use core::slice::{self};

use super::Vec;

/// An iterator which uses a closure to determine if an element should be removed.
///
/// This struct is created by [`Vec::drain_filter`].
/// See its documentation for more.
///
/// # Example
///
/// ```
/// #![feature(drain_filter)]
///
/// let mut v = vec![0, 1, 2];
/// let iter: std::vec::DrainFilter<_, _> = v.drain_filter(|x| *x % 2 == 0);
/// ```
#[unstable(feature = "drain_filter", reason = "recently added", issue = "43244")]
#[derive(Debug)]
pub struct DrainFilter<
    'a,
    T,
    F,
    #[unstable(feature = "allocator_api", issue = "32838")] A: Allocator = Global,
> where
    F: FnMut(&mut T) -> bool,
{
    pub(super) vec: &'a mut Vec<T, A>,
    /// The index of the item that will be inspected by the next call to `next`.
    pub(super) idx: usize,
    /// The number of items that have been drained (removed) thus far.
    pub(super) del: usize,
    /// The original length of `vec` prior to draining.
    pub(super) old_len: usize,
    /// The filter test predicate.
    pub(super) pred: F,
    /// A flag that indicates a panic has occurred in the filter test predicate.
    /// This is used as a hint in the drop implementation to prevent consumption
    /// of the remainder of the `DrainFilter`. Any unprocessed items will be
    /// backshifted in the `vec`, but no further items will be dropped or
    /// tested by the filter predicate.
    pub(super) panic_flag: bool,
}

impl<T, F, A: Allocator> DrainFilter<'_, T, F, A>
where
    F: FnMut(&mut T) -> bool,
{
    /// Returns a reference to the underlying allocator.
    #[unstable(feature = "allocator_api", issue = "32838")]
    #[inline]
    pub fn allocator(&self) -> &A {
}
}

#[unstable(feature = "drain_filter", reason = "recently added", issue = "43244")]
impl<T, F, A: Allocator> Iterator for DrainFilter<'_, T, F, A>
where
    F: FnMut(&mut T) -> bool,
{
    type Item = T;

    fn next(&mut self) -> Option<T> {
}

    fn size_hint(&self) -> (usize, Option<usize>) {
}
}

#[unstable(feature = "drain_filter", reason = "recently added", issue = "43244")]
impl<T, F, A: Allocator> Drop for DrainFilter<'_, T, F, A>
where
    F: FnMut(&mut T) -> bool,
{
    fn drop(&mut self) {
}
}
}

#[cfg(not(no_global_oom_handling))]
#[stable(feature = "vec_splice", since = "1.21.0")]
pub use self::splice::Splice;

#[cfg(not(no_global_oom_handling))]
mod splice {
use crate::alloc::{Allocator, Global};
use core::ptr::{self};
use core::slice::{self};

use super::{Drain, Vec};

/// A splicing iterator for `Vec`.
///
/// This struct is created by [`Vec::splice()`].
/// See its documentation for more.
///
/// # Example
///
/// ```
/// let mut v = vec![0, 1, 2];
/// let new = [7, 8];
/// let iter: std::vec::Splice<_> = v.splice(1.., new);
/// ```
#[derive(Debug)]
#[stable(feature = "vec_splice", since = "1.21.0")]
pub struct Splice<
    'a,
    I: Iterator + 'a,
    #[unstable(feature = "allocator_api", issue = "32838")] A: Allocator + 'a = Global,
> {
    pub(super) drain: Drain<'a, I::Item, A>,
    pub(super) replace_with: I,
}

#[stable(feature = "vec_splice", since = "1.21.0")]
impl<I: Iterator, A: Allocator> Iterator for Splice<'_, I, A> {
    type Item = I::Item;

    fn next(&mut self) -> Option<Self::Item> {
}

    fn size_hint(&self) -> (usize, Option<usize>) {
}
}

#[stable(feature = "vec_splice", since = "1.21.0")]
impl<I: Iterator, A: Allocator> DoubleEndedIterator for Splice<'_, I, A> {
    fn next_back(&mut self) -> Option<Self::Item> {
}
}

#[stable(feature = "vec_splice", since = "1.21.0")]
impl<I: Iterator, A: Allocator> ExactSizeIterator for Splice<'_, I, A> {}

#[stable(feature = "vec_splice", since = "1.21.0")]
impl<I: Iterator, A: Allocator> Drop for Splice<'_, I, A> {
    fn drop(&mut self) {
}
}

/// Private helper methods for `Splice::drop`
impl<T, A: Allocator> Drain<'_, T, A> {
    /// The range from `self.vec.len` to `self.tail_start` contains elements
    /// that have been moved out.
    /// Fill that range as much as possible with new elements from the `replace_with` iterator.
    /// Returns `true` if we filled the entire range. (`replace_with.next()` didn’t return `None`.)
    unsafe fn fill<I: Iterator<Item = T>>(&mut self, replace_with: &mut I) -> bool {
}

    /// Makes room for inserting more elements before the tail.
    unsafe fn move_tail(&mut self, additional: usize) {
}
}
}

#[stable(feature = "drain", since = "1.6.0")]
pub use self::drain::Drain;

mod drain {
use crate::alloc::{Allocator, Global};
use core::fmt;
use core::iter::{FusedIterator, TrustedLen};
use core::mem::{self};
use core::ptr::{self, NonNull};
use core::slice::{self};

use super::Vec;

/// A draining iterator for `Vec<T>`.
///
/// This `struct` is created by [`Vec::drain`].
/// See its documentation for more.
///
/// # Example
///
/// ```
/// let mut v = vec![0, 1, 2];
/// let iter: std::vec::Drain<_> = v.drain(..);
/// ```
#[stable(feature = "drain", since = "1.6.0")]
pub struct Drain<
    'a,
    T: 'a,
    #[unstable(feature = "allocator_api", issue = "32838")] A: Allocator + 'a = Global,
> {
    /// Index of tail to preserve
    pub(super) tail_start: usize,
    /// Length of tail
    pub(super) tail_len: usize,
    /// Current remaining range to remove
    pub(super) iter: slice::Iter<'a, T>,
    pub(super) vec: NonNull<Vec<T, A>>,
}

#[stable(feature = "collection_debug", since = "1.17.0")]
impl<T: fmt::Debug, A: Allocator> fmt::Debug for Drain<'_, T, A> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
}
}

impl<'a, T, A: Allocator> Drain<'a, T, A> {
    /// Returns the remaining items of this iterator as a slice.
    ///
    /// # Examples
    ///
    /// ```
    /// let mut vec = vec!['a', 'b', 'c'];
    /// let mut drain = vec.drain(..);
    /// assert_eq!(drain.as_slice(), &['a', 'b', 'c']);
    /// let _ = drain.next().unwrap();
    /// assert_eq!(drain.as_slice(), &['b', 'c']);
    /// ```
    #[stable(feature = "vec_drain_as_slice", since = "1.46.0")]
    pub fn as_slice(&self) -> &[T] {
}

    /// Returns a reference to the underlying allocator.
    #[unstable(feature = "allocator_api", issue = "32838")]
    #[inline]
    pub fn allocator(&self) -> &A {
}
}

#[stable(feature = "vec_drain_as_slice", since = "1.46.0")]
impl<'a, T, A: Allocator> AsRef<[T]> for Drain<'a, T, A> {
    fn as_ref(&self) -> &[T] {
}
}

#[stable(feature = "drain", since = "1.6.0")]
unsafe impl<T: Sync, A: Sync + Allocator> Sync for Drain<'_, T, A> {}
#[stable(feature = "drain", since = "1.6.0")]
unsafe impl<T: Send, A: Send + Allocator> Send for Drain<'_, T, A> {}

#[stable(feature = "drain", since = "1.6.0")]
impl<T, A: Allocator> Iterator for Drain<'_, T, A> {
    type Item = T;

    #[inline]
    fn next(&mut self) -> Option<T> {
}

    fn size_hint(&self) -> (usize, Option<usize>) {
}
}

#[stable(feature = "drain", since = "1.6.0")]
impl<T, A: Allocator> DoubleEndedIterator for Drain<'_, T, A> {
    #[inline]
    fn next_back(&mut self) -> Option<T> {
}
}

#[stable(feature = "drain", since = "1.6.0")]
impl<T, A: Allocator> Drop for Drain<'_, T, A> {
    fn drop(&mut self) {
}
}

#[stable(feature = "drain", since = "1.6.0")]
impl<T, A: Allocator> ExactSizeIterator for Drain<'_, T, A> {
    fn is_empty(&self) -> bool {
}
}

#[unstable(feature = "trusted_len", issue = "37572")]
unsafe impl<T, A: Allocator> TrustedLen for Drain<'_, T, A> {}

#[stable(feature = "fused", since = "1.26.0")]
impl<T, A: Allocator> FusedIterator for Drain<'_, T, A> {}
}

#[cfg(not(no_global_oom_handling))]
mod cow {
use crate::borrow::Cow;
use core::iter::FromIterator;

use super::Vec;

#[stable(feature = "cow_from_vec", since = "1.8.0")]
impl<'a, T: Clone> From<&'a [T]> for Cow<'a, [T]> {
    /// Creates a [`Borrowed`] variant of [`Cow`]
    /// from a slice.
    ///
    /// This conversion does not allocate or clone the data.
    ///
    /// [`Borrowed`]: crate::borrow::Cow::Borrowed
    fn from(s: &'a [T]) -> Cow<'a, [T]> {
}
}

#[stable(feature = "cow_from_vec", since = "1.8.0")]
impl<'a, T: Clone> From<Vec<T>> for Cow<'a, [T]> {
    /// Creates an [`Owned`] variant of [`Cow`]
    /// from an owned instance of [`Vec`].
    ///
    /// This conversion does not allocate or clone the data.
    ///
    /// [`Owned`]: crate::borrow::Cow::Owned
    fn from(v: Vec<T>) -> Cow<'a, [T]> {
}
}

#[stable(feature = "cow_from_vec_ref", since = "1.28.0")]
impl<'a, T: Clone> From<&'a Vec<T>> for Cow<'a, [T]> {
    /// Creates a [`Borrowed`] variant of [`Cow`]
    /// from a reference to [`Vec`].
    ///
    /// This conversion does not allocate or clone the data.
    ///
    /// [`Borrowed`]: crate::borrow::Cow::Borrowed
    fn from(v: &'a Vec<T>) -> Cow<'a, [T]> {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<'a, T> FromIterator<T> for Cow<'a, [T]>
where
    T: Clone,
{
    fn from_iter<I: IntoIterator<Item = T>>(it: I) -> Cow<'a, [T]> {
}
}
}

#[cfg(not(no_global_oom_handling))]
pub(crate) use self::into_iter::AsIntoIter;
#[stable(feature = "rust1", since = "1.0.0")]
pub use self::into_iter::IntoIter;

mod into_iter {
use crate::alloc::{Allocator, Global};
use crate::raw_vec::RawVec;
use core::fmt;
use core::intrinsics::arith_offset;
use core::iter::{
    FusedIterator, InPlaceIterable, SourceIter, TrustedLen, TrustedRandomAccessNoCoerce,
};
use core::marker::PhantomData;
use core::mem::{self};
use core::ptr::{self, NonNull};
use core::slice::{self};

/// An iterator that moves out of a vector.
///
/// This `struct` is created by the `into_iter` method on [`Vec`](super::Vec)
/// (provided by the [`IntoIterator`] trait).
///
/// # Example
///
/// ```
/// let v = vec![0, 1, 2];
/// let iter: std::vec::IntoIter<_> = v.into_iter();
/// ```
#[stable(feature = "rust1", since = "1.0.0")]
#[rustc_insignificant_dtor]
pub struct IntoIter<
    T,
    #[unstable(feature = "allocator_api", issue = "32838")] A: Allocator = Global,
> {
    pub(super) buf: NonNull<T>,
    pub(super) phantom: PhantomData<T>,
    pub(super) cap: usize,
    pub(super) alloc: A,
    pub(super) ptr: *const T,
    pub(super) end: *const T,
}

#[stable(feature = "vec_intoiter_debug", since = "1.13.0")]
impl<T: fmt::Debug, A: Allocator> fmt::Debug for IntoIter<T, A> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
}
}

impl<T, A: Allocator> IntoIter<T, A> {
    /// Returns the remaining items of this iterator as a slice.
    ///
    /// # Examples
    ///
    /// ```
    /// let vec = vec!['a', 'b', 'c'];
    /// let mut into_iter = vec.into_iter();
    /// assert_eq!(into_iter.as_slice(), &['a', 'b', 'c']);
    /// let _ = into_iter.next().unwrap();
    /// assert_eq!(into_iter.as_slice(), &['b', 'c']);
    /// ```
    #[stable(feature = "vec_into_iter_as_slice", since = "1.15.0")]
    pub fn as_slice(&self) -> &[T] {
}

    /// Returns the remaining items of this iterator as a mutable slice.
    ///
    /// # Examples
    ///
    /// ```
    /// let vec = vec!['a', 'b', 'c'];
    /// let mut into_iter = vec.into_iter();
    /// assert_eq!(into_iter.as_slice(), &['a', 'b', 'c']);
    /// into_iter.as_mut_slice()[2] = 'z';
    /// assert_eq!(into_iter.next().unwrap(), 'a');
    /// assert_eq!(into_iter.next().unwrap(), 'b');
    /// assert_eq!(into_iter.next().unwrap(), 'z');
    /// ```
    #[stable(feature = "vec_into_iter_as_slice", since = "1.15.0")]
    pub fn as_mut_slice(&mut self) -> &mut [T] {
}

    /// Returns a reference to the underlying allocator.
    #[unstable(feature = "allocator_api", issue = "32838")]
    #[inline]
    pub fn allocator(&self) -> &A {
}

    fn as_raw_mut_slice(&mut self) -> *mut [T] {
}

    /// Drops remaining elements and relinquishes the backing allocation.
    ///
    /// This is roughly equivalent to the following, but more efficient
    ///
    /// ```
    /// # let mut into_iter = Vec::<u8>::with_capacity(10).into_iter();
    /// (&mut into_iter).for_each(core::mem::drop);
    /// unsafe { core::ptr::write(&mut into_iter, Vec::new().into_iter()); }
    /// ```
    #[cfg(not(no_global_oom_handling))]
    pub(super) fn forget_allocation_drop_remaining(&mut self) {
}
}

#[stable(feature = "vec_intoiter_as_ref", since = "1.46.0")]
impl<T, A: Allocator> AsRef<[T]> for IntoIter<T, A> {
    fn as_ref(&self) -> &[T] {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
unsafe impl<T: Send, A: Allocator + Send> Send for IntoIter<T, A> {}
#[stable(feature = "rust1", since = "1.0.0")]
unsafe impl<T: Sync, A: Allocator> Sync for IntoIter<T, A> {}

#[stable(feature = "rust1", since = "1.0.0")]
impl<T, A: Allocator> Iterator for IntoIter<T, A> {
    type Item = T;

    #[inline]
    fn next(&mut self) -> Option<T> {
}

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
}

    #[inline]
    fn count(self) -> usize {
}

    #[doc(hidden)]
    unsafe fn __iterator_get_unchecked(&mut self, i: usize) -> Self::Item
    where
        Self: TrustedRandomAccessNoCoerce,
    {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<T, A: Allocator> DoubleEndedIterator for IntoIter<T, A> {
    #[inline]
    fn next_back(&mut self) -> Option<T> {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<T, A: Allocator> ExactSizeIterator for IntoIter<T, A> {
    fn is_empty(&self) -> bool {
}
}

#[stable(feature = "fused", since = "1.26.0")]
impl<T, A: Allocator> FusedIterator for IntoIter<T, A> {}

#[unstable(feature = "trusted_len", issue = "37572")]
unsafe impl<T, A: Allocator> TrustedLen for IntoIter<T, A> {}

#[doc(hidden)]
#[unstable(issue = "none", feature = "std_internals")]
// T: Copy as approximation for !Drop since get_unchecked does not advance self.ptr
// and thus we can't implement drop-handling
//
// TrustedRandomAccess (without NoCoerce) must not be implemented because
// subtypes/supertypes of `T` might not be `Copy`
unsafe impl<T, A: Allocator> TrustedRandomAccessNoCoerce for IntoIter<T, A>
where
    T: Copy,
{
    const MAY_HAVE_SIDE_EFFECT: bool = false;
}

#[cfg(not(no_global_oom_handling))]
#[stable(feature = "vec_into_iter_clone", since = "1.8.0")]
impl<T: Clone, A: Allocator + Clone> Clone for IntoIter<T, A> {
    #[cfg(not(test))]
    fn clone(&self) -> Self {
}
    #[cfg(test)]
    fn clone(&self) -> Self {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
unsafe impl<#[may_dangle] T, A: Allocator> Drop for IntoIter<T, A> {
    fn drop(&mut self) {
}
}

#[unstable(issue = "none", feature = "inplace_iteration")]
#[doc(hidden)]
unsafe impl<T, A: Allocator> InPlaceIterable for IntoIter<T, A> {}

#[unstable(issue = "none", feature = "inplace_iteration")]
#[doc(hidden)]
unsafe impl<T, A: Allocator> SourceIter for IntoIter<T, A> {
    type Source = Self;

    #[inline]
    unsafe fn as_inner(&mut self) -> &mut Self::Source {
}
}

// internal helper trait for in-place iteration specialization.
#[rustc_specialization_trait]
pub(crate) trait AsIntoIter {
    type Item;
    fn as_into_iter(&mut self) -> &mut IntoIter<Self::Item>;
}

impl<T> AsIntoIter for IntoIter<T> {
    type Item = T;

    fn as_into_iter(&mut self) -> &mut IntoIter<Self::Item> {
}
}
}

#[cfg(not(no_global_oom_handling))]
use self::is_zero::IsZero;

mod is_zero {
use crate::boxed::Box;

#[rustc_specialization_trait]
pub(super) unsafe trait IsZero {
    /// Whether this value is zero
    fn is_zero(&self) -> bool;
}

macro_rules! impl_is_zero {
    ($t:ty, $is_zero:expr) => {
        unsafe impl IsZero for $t {
            #[inline]
            fn is_zero(&self) -> bool {
}
        }
    };
}

impl_is_zero!(i16, |x| x == 0);
impl_is_zero!(i32, |x| x == 0);
impl_is_zero!(i64, |x| x == 0);
impl_is_zero!(i128, |x| x == 0);
impl_is_zero!(isize, |x| x == 0);

impl_is_zero!(u16, |x| x == 0);
impl_is_zero!(u32, |x| x == 0);
impl_is_zero!(u64, |x| x == 0);
impl_is_zero!(u128, |x| x == 0);
impl_is_zero!(usize, |x| x == 0);

impl_is_zero!(bool, |x| x == false);
impl_is_zero!(char, |x| x == '\0');

impl_is_zero!(f32, |x: f32| x.to_bits() == 0);
impl_is_zero!(f64, |x: f64| x.to_bits() == 0);

unsafe impl<T> IsZero for *const T {
    #[inline]
    fn is_zero(&self) -> bool {
}
}

unsafe impl<T> IsZero for *mut T {
    #[inline]
    fn is_zero(&self) -> bool {
}
}

// `Option<&T>` and `Option<Box<T>>` are guaranteed to represent `None` as null.
// For fat pointers, the bytes that would be the pointer metadata in the `Some`
// variant are padding in the `None` variant, so ignoring them and
// zero-initializing instead is ok.
// `Option<&mut T>` never implements `Clone`, so there's no need for an impl of
// `SpecFromElem`.

unsafe impl<T: ?Sized> IsZero for Option<&T> {
    #[inline]
    fn is_zero(&self) -> bool {
}
}

unsafe impl<T: ?Sized> IsZero for Option<Box<T>> {
    #[inline]
    fn is_zero(&self) -> bool {
}
}

// `Option<num::NonZeroU32>` and similar have a representation guarantee that
// they're the same size as the corresponding `u32` type, as well as a guarantee
// that transmuting between `NonZeroU32` and `Option<num::NonZeroU32>` works.
// While the documentation officially makes it UB to transmute from `None`,
// we're the standard library so we can make extra inferences, and we know that
// the only niche available to represent `None` is the one that's all zeros.

macro_rules! impl_is_zero_option_of_nonzero {
    ($($t:ident,)+) => {$(
        unsafe impl IsZero for Option<core::num::$t> {
            #[inline]
            fn is_zero(&self) -> bool {
}
        }
    )+};
}

impl_is_zero_option_of_nonzero!(
    NonZeroU8,
    NonZeroU16,
    NonZeroU32,
    NonZeroU64,
    NonZeroU128,
    NonZeroI8,
    NonZeroI16,
    NonZeroI32,
    NonZeroI64,
    NonZeroI128,
    NonZeroUsize,
    NonZeroIsize,
);
}

#[cfg(not(no_global_oom_handling))]
mod source_iter_marker {
use core::iter::{InPlaceIterable, SourceIter, TrustedRandomAccessNoCoerce};
use core::mem::{self, ManuallyDrop};
use core::ptr::{self};

use super::{AsIntoIter, InPlaceDrop, SpecFromIter, SpecFromIterNested, Vec};

/// Specialization marker for collecting an iterator pipeline into a Vec while reusing the
/// source allocation, i.e. executing the pipeline in place.
///
/// The SourceIter parent trait is necessary for the specializing function to access the allocation
/// which is to be reused. But it is not sufficient for the specialization to be valid. See
/// additional bounds on the impl.
#[rustc_unsafe_specialization_marker]
pub(super) trait SourceIterMarker: SourceIter<Source: AsIntoIter> {}

// The std-internal SourceIter/InPlaceIterable traits are only implemented by chains of
// Adapter<Adapter<Adapter<IntoIter>>> (all owned by core/std). Additional bounds
// on the adapter implementations (beyond `impl<I: Trait> Trait for Adapter<I>`) only depend on other
// traits already marked as specialization traits (Copy, TrustedRandomAccess, FusedIterator).
// I.e. the marker does not depend on lifetimes of user-supplied types. Modulo the Copy hole, which
// several other specializations already depend on.
impl<T> SourceIterMarker for T where T: SourceIter<Source: AsIntoIter> + InPlaceIterable {}

impl<T, I> SpecFromIter<T, I> for Vec<T>
where
    I: Iterator<Item = T> + SourceIterMarker,
{
    default fn from_iter(mut iterator: I) -> Self {
}
}

fn write_in_place_with_drop<T>(
    src_end: *const T,
) -> impl FnMut(InPlaceDrop<T>, T) -> Result<InPlaceDrop<T>, !> {
}

/// Helper trait to hold specialized implementations of the in-place iterate-collect loop
trait SpecInPlaceCollect<T, I>: Iterator<Item = T> {
    /// Collects an iterator (`self`) into the destination buffer (`dst`) and returns the number of items
    /// collected. `end` is the last writable element of the allocation and used for bounds checks.
    ///
    /// This method is specialized and one of its implementations makes use of
    /// `Iterator::__iterator_get_unchecked` calls with a `TrustedRandomAccessNoCoerce` bound
    /// on `I` which means the caller of this method must take the safety conditions
    /// of that trait into consideration.
    fn collect_in_place(&mut self, dst: *mut T, end: *const T) -> usize;
}

impl<T, I> SpecInPlaceCollect<T, I> for I
where
    I: Iterator<Item = T>,
{
    #[inline]
    default fn collect_in_place(&mut self, dst_buf: *mut T, end: *const T) -> usize {
}
}

impl<T, I> SpecInPlaceCollect<T, I> for I
where
    I: Iterator<Item = T> + TrustedRandomAccessNoCoerce,
{
    #[inline]
    fn collect_in_place(&mut self, dst_buf: *mut T, end: *const T) -> usize {
}
}
}

mod partial_eq {
use crate::alloc::Allocator;
#[cfg(not(no_global_oom_handling))]
use crate::borrow::Cow;

use super::Vec;

macro_rules! __impl_slice_eq1 {
    ([$($vars:tt)*] $lhs:ty, $rhs:ty $(where $ty:ty: $bound:ident)?, #[$stability:meta]) => {
        #[$stability]
        impl<T, U, $($vars)*> PartialEq<$rhs> for $lhs
        where
            T: PartialEq<U>,
            $($ty: $bound)?
        {
            #[inline]
            fn eq(&self, other: &$rhs) -> bool { }
            #[inline]
            fn ne(&self, other: &$rhs) -> bool { }
        }
    }
}

__impl_slice_eq1! { [A: Allocator] Vec<T, A>, Vec<U, A>, #[stable(feature = "rust1", since = "1.0.0")] }
__impl_slice_eq1! { [A: Allocator] Vec<T, A>, &[U], #[stable(feature = "rust1", since = "1.0.0")] }
__impl_slice_eq1! { [A: Allocator] Vec<T, A>, &mut [U], #[stable(feature = "rust1", since = "1.0.0")] }
__impl_slice_eq1! { [A: Allocator] &[T], Vec<U, A>, #[stable(feature = "partialeq_vec_for_ref_slice", since = "1.46.0")] }
__impl_slice_eq1! { [A: Allocator] &mut [T], Vec<U, A>, #[stable(feature = "partialeq_vec_for_ref_slice", since = "1.46.0")] }
__impl_slice_eq1! { [A: Allocator] Vec<T, A>, [U], #[stable(feature = "partialeq_vec_for_slice", since = "1.48.0")]  }
__impl_slice_eq1! { [A: Allocator] [T], Vec<U, A>, #[stable(feature = "partialeq_vec_for_slice", since = "1.48.0")]  }
#[cfg(not(no_global_oom_handling))]
__impl_slice_eq1! { [A: Allocator] Cow<'_, [T]>, Vec<U, A> where T: Clone, #[stable(feature = "rust1", since = "1.0.0")] }
#[cfg(not(no_global_oom_handling))]
__impl_slice_eq1! { [] Cow<'_, [T]>, &[U] where T: Clone, #[stable(feature = "rust1", since = "1.0.0")] }
#[cfg(not(no_global_oom_handling))]
__impl_slice_eq1! { [] Cow<'_, [T]>, &mut [U] where T: Clone, #[stable(feature = "rust1", since = "1.0.0")] }
__impl_slice_eq1! { [A: Allocator, const N: usize] Vec<T, A>, [U; N], #[stable(feature = "rust1", since = "1.0.0")] }
__impl_slice_eq1! { [A: Allocator, const N: usize] Vec<T, A>, &[U; N], #[stable(feature = "rust1", since = "1.0.0")] }

// NOTE: some less important impls are omitted to reduce code bloat
// FIXME(Centril): Reconsider this?
//__impl_slice_eq1! { [const N: usize] Vec<A>, &mut [B; N], }
//__impl_slice_eq1! { [const N: usize] [A; N], Vec<B>, }
//__impl_slice_eq1! { [const N: usize] &[A; N], Vec<B>, }
//__impl_slice_eq1! { [const N: usize] &mut [A; N], Vec<B>, }
//__impl_slice_eq1! { [const N: usize] Cow<'a, [A]>, [B; N], }
//__impl_slice_eq1! { [const N: usize] Cow<'a, [A]>, &[B; N], }
//__impl_slice_eq1! { [const N: usize] Cow<'a, [A]>, &mut [B; N], }
}

#[cfg(not(no_global_oom_handling))]
use self::spec_from_elem::SpecFromElem;

#[cfg(not(no_global_oom_handling))]
mod spec_from_elem {
use crate::alloc::Allocator;
use crate::raw_vec::RawVec;
use core::ptr::{self};

use super::{ExtendElement, IsZero, Vec};

// Specialization trait used for Vec::from_elem
pub(super) trait SpecFromElem: Sized {
    fn from_elem<A: Allocator>(elem: Self, n: usize, alloc: A) -> Vec<Self, A>;
}

impl<T: Clone> SpecFromElem for T {
    default fn from_elem<A: Allocator>(elem: Self, n: usize, alloc: A) -> Vec<Self, A> {
}
}

impl SpecFromElem for i8 {
    #[inline]
    fn from_elem<A: Allocator>(elem: i8, n: usize, alloc: A) -> Vec<i8, A> {
}
}

impl SpecFromElem for u8 {
    #[inline]
    fn from_elem<A: Allocator>(elem: u8, n: usize, alloc: A) -> Vec<u8, A> {
}
}

impl<T: Clone + IsZero> SpecFromElem for T {
    #[inline]
    fn from_elem<A: Allocator>(elem: T, n: usize, alloc: A) -> Vec<T, A> {
}
}
}

#[cfg(not(no_global_oom_handling))]
use self::set_len_on_drop::SetLenOnDrop;

#[cfg(not(no_global_oom_handling))]
mod set_len_on_drop {
// Set the length of the vec when the `SetLenOnDrop` value goes out of scope.
//
// The idea is: The length field in SetLenOnDrop is a local variable
// that the optimizer will see does not alias with any stores through the Vec's data
// pointer. This is a workaround for alias analysis issue #32155
pub(super) struct SetLenOnDrop<'a> {
    len: &'a mut usize,
    local_len: usize,
}

impl<'a> SetLenOnDrop<'a> {
    #[inline]
    pub(super) fn new(len: &'a mut usize) -> Self {
}

    #[inline]
    pub(super) fn increment_len(&mut self, increment: usize) {
}
}

impl Drop for SetLenOnDrop<'_> {
    #[inline]
    fn drop(&mut self) {
}
}
}

#[cfg(not(no_global_oom_handling))]
use self::in_place_drop::InPlaceDrop;

#[cfg(not(no_global_oom_handling))]
mod in_place_drop {
use core::ptr::{self};
use core::slice::{self};

// A helper struct for in-place iteration that drops the destination slice of iteration,
// i.e. the head. The source slice (the tail) is dropped by IntoIter.
pub(super) struct InPlaceDrop<T> {
    pub(super) inner: *mut T,
    pub(super) dst: *mut T,
}

impl<T> InPlaceDrop<T> {
    fn len(&self) -> usize {
}
}

impl<T> Drop for InPlaceDrop<T> {
    #[inline]
    fn drop(&mut self) {
}
}
}

#[cfg(not(no_global_oom_handling))]
use self::spec_from_iter_nested::SpecFromIterNested;

#[cfg(not(no_global_oom_handling))]
mod spec_from_iter_nested {
use core::iter::TrustedLen;
use core::ptr::{self};

use super::{SpecExtend, Vec};

/// Another specialization trait for Vec::from_iter
/// necessary to manually prioritize overlapping specializations
/// see [`SpecFromIter`](super::SpecFromIter) for details.
pub(super) trait SpecFromIterNested<T, I> {
    fn from_iter(iter: I) -> Self;
}

impl<T, I> SpecFromIterNested<T, I> for Vec<T>
where
    I: Iterator<Item = T>,
{
    default fn from_iter(mut iterator: I) -> Self {
}
}

impl<T, I> SpecFromIterNested<T, I> for Vec<T>
where
    I: TrustedLen<Item = T>,
{
    fn from_iter(iterator: I) -> Self {
}
}
}

#[cfg(not(no_global_oom_handling))]
use self::spec_from_iter::SpecFromIter;

#[cfg(not(no_global_oom_handling))]
mod spec_from_iter {
use core::mem::ManuallyDrop;
use core::ptr::{self};

use super::{IntoIter, SpecExtend, SpecFromIterNested, Vec};

/// Specialization trait used for Vec::from_iter
///
/// ## The delegation graph:
///
/// ```text
/// +-------------+
/// |FromIterator |
/// +-+-----------+
///   |
///   v
/// +-+-------------------------------+  +---------------------+
/// |SpecFromIter                  +---->+SpecFromIterNested   |
/// |where I:                      |  |  |where I:             |
/// |  Iterator (default)----------+  |  |  Iterator (default) |
/// |  vec::IntoIter               |  |  |  TrustedLen         |
/// |  SourceIterMarker---fallback-+  |  +---------------------+
/// +---------------------------------+
/// ```
pub(super) trait SpecFromIter<T, I> {
    fn from_iter(iter: I) -> Self;
}

impl<T, I> SpecFromIter<T, I> for Vec<T>
where
    I: Iterator<Item = T>,
{
    default fn from_iter(iterator: I) -> Self {
}
}

impl<T> SpecFromIter<T, IntoIter<T>> for Vec<T> {
    fn from_iter(iterator: IntoIter<T>) -> Self {
}
}
}

#[cfg(not(no_global_oom_handling))]
use self::spec_extend::SpecExtend;

#[cfg(not(no_global_oom_handling))]
mod spec_extend {
use crate::alloc::Allocator;
use core::iter::TrustedLen;
use core::ptr::{self};
use core::slice::{self};

use super::{IntoIter, SetLenOnDrop, Vec};

// Specialization trait used for Vec::extend
pub(super) trait SpecExtend<T, I> {
    fn spec_extend(&mut self, iter: I);
}

impl<T, I, A: Allocator> SpecExtend<T, I> for Vec<T, A>
where
    I: Iterator<Item = T>,
{
    default fn spec_extend(&mut self, iter: I) {
}
}

impl<T, I, A: Allocator> SpecExtend<T, I> for Vec<T, A>
where
    I: TrustedLen<Item = T>,
{
    default fn spec_extend(&mut self, iterator: I) {
}
}

impl<T, A: Allocator> SpecExtend<T, IntoIter<T>> for Vec<T, A> {
    fn spec_extend(&mut self, mut iterator: IntoIter<T>) {
}
}

impl<'a, T: 'a, I, A: Allocator + 'a> SpecExtend<&'a T, I> for Vec<T, A>
where
    I: Iterator<Item = &'a T>,
    T: Clone,
{
    default fn spec_extend(&mut self, iterator: I) {
}
}

impl<'a, T: 'a, A: Allocator + 'a> SpecExtend<&'a T, slice::Iter<'a, T>> for Vec<T, A>
where
    T: Copy,
{
    fn spec_extend(&mut self, iterator: slice::Iter<'a, T>) {
}
}
}

/// A contiguous growable array type, written as `Vec<T>` and pronounced 'vector'.
///
/// # Examples
///
/// ```
/// let mut vec = Vec::new();
/// vec.push(1);
/// vec.push(2);
///
/// assert_eq!(vec.len(), 2);
/// assert_eq!(vec[0], 1);
///
/// assert_eq!(vec.pop(), Some(2));
/// assert_eq!(vec.len(), 1);
///
/// vec[0] = 7;
/// assert_eq!(vec[0], 7);
///
/// vec.extend([1, 2, 3].iter().copied());
///
/// for x in &vec {
///     println!("{}", x);
/// }
/// assert_eq!(vec, [7, 1, 2, 3]);
/// ```
///
/// The [`vec!`] macro is provided for convenient initialization:
///
/// ```
/// let mut vec1 = vec![1, 2, 3];
/// vec1.push(4);
/// let vec2 = Vec::from([1, 2, 3, 4]);
/// assert_eq!(vec1, vec2);
/// ```
///
/// It can also initialize each element of a `Vec<T>` with a given value.
/// This may be more efficient than performing allocation and initialization
/// in separate steps, especially when initializing a vector of zeros:
///
/// ```
/// let vec = vec![0; 5];
/// assert_eq!(vec, [0, 0, 0, 0, 0]);
///
/// // The following is equivalent, but potentially slower:
/// let mut vec = Vec::with_capacity(5);
/// vec.resize(5, 0);
/// assert_eq!(vec, [0, 0, 0, 0, 0]);
/// ```
///
/// For more information, see
/// [Capacity and Reallocation](#capacity-and-reallocation).
///
/// Use a `Vec<T>` as an efficient stack:
///
/// ```
/// let mut stack = Vec::new();
///
/// stack.push(1);
/// stack.push(2);
/// stack.push(3);
///
/// while let Some(top) = stack.pop() {
///     // Prints 3, 2, 1
///     println!("{}", top);
/// }
/// ```
///
/// # Indexing
///
/// The `Vec` type allows to access values by index, because it implements the
/// [`Index`] trait. An example will be more explicit:
///
/// ```
/// let v = vec![0, 2, 4, 6];
/// println!("{}", v[1]); // it will display '2'
/// ```
///
/// However be careful: if you try to access an index which isn't in the `Vec`,
/// your software will panic! You cannot do this:
///
/// ```should_panic
/// let v = vec![0, 2, 4, 6];
/// println!("{}", v[6]); // it will panic!
/// ```
///
/// Use [`get`] and [`get_mut`] if you want to check whether the index is in
/// the `Vec`.
///
/// # Slicing
///
/// A `Vec` can be mutable. On the other hand, slices are read-only objects.
/// To get a [slice][prim@slice], use [`&`]. Example:
///
/// ```
/// fn read_slice(slice: &[usize]) {
///     // ...
/// }
///
/// let v = vec![0, 1];
/// read_slice(&v);
///
/// // ... and that's all!
/// // you can also do it like this:
/// let u: &[usize] = &v;
/// // or like this:
/// let u: &[_] = &v;
/// ```
///
/// In Rust, it's more common to pass slices as arguments rather than vectors
/// when you just want to provide read access. The same goes for [`String`] and
/// [`&str`].
///
/// # Capacity and reallocation
///
/// The capacity of a vector is the amount of space allocated for any future
/// elements that will be added onto the vector. This is not to be confused with
/// the *length* of a vector, which specifies the number of actual elements
/// within the vector. If a vector's length exceeds its capacity, its capacity
/// will automatically be increased, but its elements will have to be
/// reallocated.
///
/// For example, a vector with capacity 10 and length 0 would be an empty vector
/// with space for 10 more elements. Pushing 10 or fewer elements onto the
/// vector will not change its capacity or cause reallocation to occur. However,
/// if the vector's length is increased to 11, it will have to reallocate, which
/// can be slow. For this reason, it is recommended to use [`Vec::with_capacity`]
/// whenever possible to specify how big the vector is expected to get.
///
/// # Guarantees
///
/// Due to its incredibly fundamental nature, `Vec` makes a lot of guarantees
/// about its design. This ensures that it's as low-overhead as possible in
/// the general case, and can be correctly manipulated in primitive ways
/// by unsafe code. Note that these guarantees refer to an unqualified `Vec<T>`.
/// If additional type parameters are added (e.g., to support custom allocators),
/// overriding their defaults may change the behavior.
///
/// Most fundamentally, `Vec` is and always will be a (pointer, capacity, length)
/// triplet. No more, no less. The order of these fields is completely
/// unspecified, and you should use the appropriate methods to modify these.
/// The pointer will never be null, so this type is null-pointer-optimized.
///
/// However, the pointer might not actually point to allocated memory. In particular,
/// if you construct a `Vec` with capacity 0 via [`Vec::new`], [`vec![]`][`vec!`],
/// [`Vec::with_capacity(0)`][`Vec::with_capacity`], or by calling [`shrink_to_fit`]
/// on an empty Vec, it will not allocate memory. Similarly, if you store zero-sized
/// types inside a `Vec`, it will not allocate space for them. *Note that in this case
/// the `Vec` might not report a [`capacity`] of 0*. `Vec` will allocate if and only
/// if [`mem::size_of::<T>`]`() * capacity() > 0`. In general, `Vec`'s allocation
/// details are very subtle &mdash; if you intend to allocate memory using a `Vec`
/// and use it for something else (either to pass to unsafe code, or to build your
/// own memory-backed collection), be sure to deallocate this memory by using
/// `from_raw_parts` to recover the `Vec` and then dropping it.
///
/// If a `Vec` *has* allocated memory, then the memory it points to is on the heap
/// (as defined by the allocator Rust is configured to use by default), and its
/// pointer points to [`len`] initialized, contiguous elements in order (what
/// you would see if you coerced it to a slice), followed by [`capacity`]` -
/// `[`len`] logically uninitialized, contiguous elements.
///
/// A vector containing the elements `'a'` and `'b'` with capacity 4 can be
/// visualized as below. The top part is the `Vec` struct, it contains a
/// pointer to the head of the allocation in the heap, length and capacity.
/// The bottom part is the allocation on the heap, a contiguous memory block.
///
/// ```text
///             ptr      len  capacity
///        +--------+--------+--------+
///        | 0x0123 |      2 |      4 |
///        +--------+--------+--------+
///             |
///             v
/// Heap   +--------+--------+--------+--------+
///        |    'a' |    'b' | uninit | uninit |
///        +--------+--------+--------+--------+
/// ```
///
/// - **uninit** represents memory that is not initialized, see [`MaybeUninit`].
/// - Note: the ABI is not stable and `Vec` makes no guarantees about its memory
///   layout (including the order of fields).
///
/// `Vec` will never perform a "small optimization" where elements are actually
/// stored on the stack for two reasons:
///
/// * It would make it more difficult for unsafe code to correctly manipulate
///   a `Vec`. The contents of a `Vec` wouldn't have a stable address if it were
///   only moved, and it would be more difficult to determine if a `Vec` had
///   actually allocated memory.
///
/// * It would penalize the general case, incurring an additional branch
///   on every access.
///
/// `Vec` will never automatically shrink itself, even if completely empty. This
/// ensures no unnecessary allocations or deallocations occur. Emptying a `Vec`
/// and then filling it back up to the same [`len`] should incur no calls to
/// the allocator. If you wish to free up unused memory, use
/// [`shrink_to_fit`] or [`shrink_to`].
///
/// [`push`] and [`insert`] will never (re)allocate if the reported capacity is
/// sufficient. [`push`] and [`insert`] *will* (re)allocate if
/// [`len`]` == `[`capacity`]. That is, the reported capacity is completely
/// accurate, and can be relied on. It can even be used to manually free the memory
/// allocated by a `Vec` if desired. Bulk insertion methods *may* reallocate, even
/// when not necessary.
///
/// `Vec` does not guarantee any particular growth strategy when reallocating
/// when full, nor when [`reserve`] is called. The current strategy is basic
/// and it may prove desirable to use a non-constant growth factor. Whatever
/// strategy is used will of course guarantee *O*(1) amortized [`push`].
///
/// `vec![x; n]`, `vec![a, b, c, d]`, and
/// [`Vec::with_capacity(n)`][`Vec::with_capacity`], will all produce a `Vec`
/// with exactly the requested capacity. If [`len`]` == `[`capacity`],
/// (as is the case for the [`vec!`] macro), then a `Vec<T>` can be converted to
/// and from a [`Box<[T]>`][owned slice] without reallocating or moving the elements.
///
/// `Vec` will not specifically overwrite any data that is removed from it,
/// but also won't specifically preserve it. Its uninitialized memory is
/// scratch space that it may use however it wants. It will generally just do
/// whatever is most efficient or otherwise easy to implement. Do not rely on
/// removed data to be erased for security purposes. Even if you drop a `Vec`, its
/// buffer may simply be reused by another `Vec`. Even if you zero a `Vec`'s memory
/// first, that might not actually happen because the optimizer does not consider
/// this a side-effect that must be preserved. There is one case which we will
/// not break, however: using `unsafe` code to write to the excess capacity,
/// and then increasing the length to match, is always valid.
///
/// Currently, `Vec` does not guarantee the order in which elements are dropped.
/// The order has changed in the past and may change again.
///
/// [`get`]: ../../std/vec/struct.Vec.html#method.get
/// [`get_mut`]: ../../std/vec/struct.Vec.html#method.get_mut
/// [`String`]: crate::string::String
/// [`&str`]: type@str
/// [`shrink_to_fit`]: Vec::shrink_to_fit
/// [`shrink_to`]: Vec::shrink_to
/// [`capacity`]: Vec::capacity
/// [`mem::size_of::<T>`]: core::mem::size_of
/// [`len`]: Vec::len
/// [`push`]: Vec::push
/// [`insert`]: Vec::insert
/// [`reserve`]: Vec::reserve
/// [`MaybeUninit`]: core::mem::MaybeUninit
/// [owned slice]: Box
#[stable(feature = "rust1", since = "1.0.0")]
#[cfg_attr(not(test), rustc_diagnostic_item = "vec_type")]
#[rustc_insignificant_dtor]
pub struct Vec<T, #[unstable(feature = "allocator_api", issue = "32838")] A: Allocator = Global> {
    buf: RawVec<T, A>,
    len: usize,
}

////////////////////////////////////////////////////////////////////////////////
// Inherent methods
////////////////////////////////////////////////////////////////////////////////

impl<T> Vec<T> {
    /// Constructs a new, empty `Vec<T>`.
    ///
    /// The vector will not allocate until elements are pushed onto it.
    ///
    /// # Examples
    ///
    /// ```
    /// # #![allow(unused_mut)]
    /// let mut vec: Vec<i32> = Vec::new();
    /// ```
    #[inline]
    #[rustc_const_stable(feature = "const_vec_new", since = "1.39.0")]
    #[stable(feature = "rust1", since = "1.0.0")]
    pub const fn new() -> Self {
}

    /// Constructs a new, empty `Vec<T>` with the specified capacity.
    ///
    /// The vector will be able to hold exactly `capacity` elements without
    /// reallocating. If `capacity` is 0, the vector will not allocate.
    ///
    /// It is important to note that although the returned vector has the
    /// *capacity* specified, the vector will have a zero *length*. For an
    /// explanation of the difference between length and capacity, see
    /// *[Capacity and reallocation]*.
    ///
    /// [Capacity and reallocation]: #capacity-and-reallocation
    ///
    /// # Panics
    ///
    /// Panics if the new capacity exceeds `isize::MAX` bytes.
    ///
    /// # Examples
    ///
    /// ```
    /// let mut vec = Vec::with_capacity(10);
    ///
    /// // The vector contains no items, even though it has capacity for more
    /// assert_eq!(vec.len(), 0);
    /// assert_eq!(vec.capacity(), 10);
    ///
    /// // These are all done without reallocating...
    /// for i in 0..10 {
    ///     vec.push(i);
    /// }
    /// assert_eq!(vec.len(), 10);
    /// assert_eq!(vec.capacity(), 10);
    ///
    /// // ...but this may make the vector reallocate
    /// vec.push(11);
    /// assert_eq!(vec.len(), 11);
    /// assert!(vec.capacity() >= 11);
    /// ```
    #[cfg(not(no_global_oom_handling))]
    #[inline]
    #[stable(feature = "rust1", since = "1.0.0")]
    pub fn with_capacity(capacity: usize) -> Self {
}

    /// Creates a `Vec<T>` directly from the raw components of another vector.
    ///
    /// # Safety
    ///
    /// This is highly unsafe, due to the number of invariants that aren't
    /// checked:
    ///
    /// * `ptr` needs to have been previously allocated via [`String`]/`Vec<T>`
    ///   (at least, it's highly likely to be incorrect if it wasn't).
    /// * `T` needs to have the same size and alignment as what `ptr` was allocated with.
    ///   (`T` having a less strict alignment is not sufficient, the alignment really
    ///   needs to be equal to satisfy the [`dealloc`] requirement that memory must be
    ///   allocated and deallocated with the same layout.)
    /// * `length` needs to be less than or equal to `capacity`.
    /// * `capacity` needs to be the capacity that the pointer was allocated with.
    ///
    /// Violating these may cause problems like corrupting the allocator's
    /// internal data structures. For example it is **not** safe
    /// to build a `Vec<u8>` from a pointer to a C `char` array with length `size_t`.
    /// It's also not safe to build one from a `Vec<u16>` and its length, because
    /// the allocator cares about the alignment, and these two types have different
    /// alignments. The buffer was allocated with alignment 2 (for `u16`), but after
    /// turning it into a `Vec<u8>` it'll be deallocated with alignment 1.
    ///
    /// The ownership of `ptr` is effectively transferred to the
    /// `Vec<T>` which may then deallocate, reallocate or change the
    /// contents of memory pointed to by the pointer at will. Ensure
    /// that nothing else uses the pointer after calling this
    /// function.
    ///
    /// [`String`]: crate::string::String
    /// [`dealloc`]: crate::alloc::GlobalAlloc::dealloc
    ///
    /// # Examples
    ///
    /// ```
    /// use std::ptr;
    /// use std::mem;
    ///
    /// let v = vec![1, 2, 3];
    ///
    // FIXME Update this when vec_into_raw_parts is stabilized
    /// // Prevent running `v`'s destructor so we are in complete control
    /// // of the allocation.
    /// let mut v = mem::ManuallyDrop::new(v);
    ///
    /// // Pull out the various important pieces of information about `v`
    /// let p = v.as_mut_ptr();
    /// let len = v.len();
    /// let cap = v.capacity();
    ///
    /// unsafe {
    ///     // Overwrite memory with 4, 5, 6
    ///     for i in 0..len as isize {
    ///         ptr::write(p.offset(i), 4 + i);
    ///     }
    ///
    ///     // Put everything back together into a Vec
    ///     let rebuilt = Vec::from_raw_parts(p, len, cap);
    ///     assert_eq!(rebuilt, [4, 5, 6]);
    /// }
    /// ```
    #[inline]
    #[stable(feature = "rust1", since = "1.0.0")]
    pub unsafe fn from_raw_parts(ptr: *mut T, length: usize, capacity: usize) -> Self {
}
}

impl<T, A: Allocator> Vec<T, A> {
    /// Constructs a new, empty `Vec<T, A>`.
    ///
    /// The vector will not allocate until elements are pushed onto it.
    ///
    /// # Examples
    ///
    /// ```
    /// #![feature(allocator_api)]
    ///
    /// use std::alloc::System;
    ///
    /// # #[allow(unused_mut)]
    /// let mut vec: Vec<i32, _> = Vec::new_in(System);
    /// ```
    #[inline]
    #[unstable(feature = "allocator_api", issue = "32838")]
    pub const fn new_in(alloc: A) -> Self {
}

    /// Constructs a new, empty `Vec<T, A>` with the specified capacity with the provided
    /// allocator.
    ///
    /// The vector will be able to hold exactly `capacity` elements without
    /// reallocating. If `capacity` is 0, the vector will not allocate.
    ///
    /// It is important to note that although the returned vector has the
    /// *capacity* specified, the vector will have a zero *length*. For an
    /// explanation of the difference between length and capacity, see
    /// *[Capacity and reallocation]*.
    ///
    /// [Capacity and reallocation]: #capacity-and-reallocation
    ///
    /// # Panics
    ///
    /// Panics if the new capacity exceeds `isize::MAX` bytes.
    ///
    /// # Examples
    ///
    /// ```
    /// #![feature(allocator_api)]
    ///
    /// use std::alloc::System;
    ///
    /// let mut vec = Vec::with_capacity_in(10, System);
    ///
    /// // The vector contains no items, even though it has capacity for more
    /// assert_eq!(vec.len(), 0);
    /// assert_eq!(vec.capacity(), 10);
    ///
    /// // These are all done without reallocating...
    /// for i in 0..10 {
    ///     vec.push(i);
    /// }
    /// assert_eq!(vec.len(), 10);
    /// assert_eq!(vec.capacity(), 10);
    ///
    /// // ...but this may make the vector reallocate
    /// vec.push(11);
    /// assert_eq!(vec.len(), 11);
    /// assert!(vec.capacity() >= 11);
    /// ```
    #[cfg(not(no_global_oom_handling))]
    #[inline]
    #[unstable(feature = "allocator_api", issue = "32838")]
    pub fn with_capacity_in(capacity: usize, alloc: A) -> Self {
}

    /// Creates a `Vec<T, A>` directly from the raw components of another vector.
    ///
    /// # Safety
    ///
    /// This is highly unsafe, due to the number of invariants that aren't
    /// checked:
    ///
    /// * `ptr` needs to have been previously allocated via [`String`]/`Vec<T>`
    ///   (at least, it's highly likely to be incorrect if it wasn't).
    /// * `T` needs to have the same size and alignment as what `ptr` was allocated with.
    ///   (`T` having a less strict alignment is not sufficient, the alignment really
    ///   needs to be equal to satisfy the [`dealloc`] requirement that memory must be
    ///   allocated and deallocated with the same layout.)
    /// * `length` needs to be less than or equal to `capacity`.
    /// * `capacity` needs to be the capacity that the pointer was allocated with.
    ///
    /// Violating these may cause problems like corrupting the allocator's
    /// internal data structures. For example it is **not** safe
    /// to build a `Vec<u8>` from a pointer to a C `char` array with length `size_t`.
    /// It's also not safe to build one from a `Vec<u16>` and its length, because
    /// the allocator cares about the alignment, and these two types have different
    /// alignments. The buffer was allocated with alignment 2 (for `u16`), but after
    /// turning it into a `Vec<u8>` it'll be deallocated with alignment 1.
    ///
    /// The ownership of `ptr` is effectively transferred to the
    /// `Vec<T>` which may then deallocate, reallocate or change the
    /// contents of memory pointed to by the pointer at will. Ensure
    /// that nothing else uses the pointer after calling this
    /// function.
    ///
    /// [`String`]: crate::string::String
    /// [`dealloc`]: crate::alloc::GlobalAlloc::dealloc
    ///
    /// # Examples
    ///
    /// ```
    /// #![feature(allocator_api)]
    ///
    /// use std::alloc::System;
    ///
    /// use std::ptr;
    /// use std::mem;
    ///
    /// let mut v = Vec::with_capacity_in(3, System);
    /// v.push(1);
    /// v.push(2);
    /// v.push(3);
    ///
    // FIXME Update this when vec_into_raw_parts is stabilized
    /// // Prevent running `v`'s destructor so we are in complete control
    /// // of the allocation.
    /// let mut v = mem::ManuallyDrop::new(v);
    ///
    /// // Pull out the various important pieces of information about `v`
    /// let p = v.as_mut_ptr();
    /// let len = v.len();
    /// let cap = v.capacity();
    /// let alloc = v.allocator();
    ///
    /// unsafe {
    ///     // Overwrite memory with 4, 5, 6
    ///     for i in 0..len as isize {
    ///         ptr::write(p.offset(i), 4 + i);
    ///     }
    ///
    ///     // Put everything back together into a Vec
    ///     let rebuilt = Vec::from_raw_parts_in(p, len, cap, alloc.clone());
    ///     assert_eq!(rebuilt, [4, 5, 6]);
    /// }
    /// ```
    #[inline]
    #[unstable(feature = "allocator_api", issue = "32838")]
    pub unsafe fn from_raw_parts_in(ptr: *mut T, length: usize, capacity: usize, alloc: A) -> Self {
}

    /// Decomposes a `Vec<T>` into its raw components.
    ///
    /// Returns the raw pointer to the underlying data, the length of
    /// the vector (in elements), and the allocated capacity of the
    /// data (in elements). These are the same arguments in the same
    /// order as the arguments to [`from_raw_parts`].
    ///
    /// After calling this function, the caller is responsible for the
    /// memory previously managed by the `Vec`. The only way to do
    /// this is to convert the raw pointer, length, and capacity back
    /// into a `Vec` with the [`from_raw_parts`] function, allowing
    /// the destructor to perform the cleanup.
    ///
    /// [`from_raw_parts`]: Vec::from_raw_parts
    ///
    /// # Examples
    ///
    /// ```
    /// #![feature(vec_into_raw_parts)]
    /// let v: Vec<i32> = vec![-1, 0, 1];
    ///
    /// let (ptr, len, cap) = v.into_raw_parts();
    ///
    /// let rebuilt = unsafe {
    ///     // We can now make changes to the components, such as
    ///     // transmuting the raw pointer to a compatible type.
    ///     let ptr = ptr as *mut u32;
    ///
    ///     Vec::from_raw_parts(ptr, len, cap)
    /// };
    /// assert_eq!(rebuilt, [4294967295, 0, 1]);
    /// ```
    #[unstable(feature = "vec_into_raw_parts", reason = "new API", issue = "65816")]
    pub fn into_raw_parts(self) -> (*mut T, usize, usize) {
}

    /// Decomposes a `Vec<T>` into its raw components.
    ///
    /// Returns the raw pointer to the underlying data, the length of the vector (in elements),
    /// the allocated capacity of the data (in elements), and the allocator. These are the same
    /// arguments in the same order as the arguments to [`from_raw_parts_in`].
    ///
    /// After calling this function, the caller is responsible for the
    /// memory previously managed by the `Vec`. The only way to do
    /// this is to convert the raw pointer, length, and capacity back
    /// into a `Vec` with the [`from_raw_parts_in`] function, allowing
    /// the destructor to perform the cleanup.
    ///
    /// [`from_raw_parts_in`]: Vec::from_raw_parts_in
    ///
    /// # Examples
    ///
    /// ```
    /// #![feature(allocator_api, vec_into_raw_parts)]
    ///
    /// use std::alloc::System;
    ///
    /// let mut v: Vec<i32, System> = Vec::new_in(System);
    /// v.push(-1);
    /// v.push(0);
    /// v.push(1);
    ///
    /// let (ptr, len, cap, alloc) = v.into_raw_parts_with_alloc();
    ///
    /// let rebuilt = unsafe {
    ///     // We can now make changes to the components, such as
    ///     // transmuting the raw pointer to a compatible type.
    ///     let ptr = ptr as *mut u32;
    ///
    ///     Vec::from_raw_parts_in(ptr, len, cap, alloc)
    /// };
    /// assert_eq!(rebuilt, [4294967295, 0, 1]);
    /// ```
    #[unstable(feature = "allocator_api", issue = "32838")]
    // #[unstable(feature = "vec_into_raw_parts", reason = "new API", issue = "65816")]
    pub fn into_raw_parts_with_alloc(self) -> (*mut T, usize, usize, A) {
}

    /// Returns the number of elements the vector can hold without
    /// reallocating.
    ///
    /// # Examples
    ///
    /// ```
    /// let vec: Vec<i32> = Vec::with_capacity(10);
    /// assert_eq!(vec.capacity(), 10);
    /// ```
    #[inline]
    #[stable(feature = "rust1", since = "1.0.0")]
    pub fn capacity(&self) -> usize {
}

    /// Reserves capacity for at least `additional` more elements to be inserted
    /// in the given `Vec<T>`. The collection may reserve more space to avoid
    /// frequent reallocations. After calling `reserve`, capacity will be
    /// greater than or equal to `self.len() + additional`. Does nothing if
    /// capacity is already sufficient.
    ///
    /// # Panics
    ///
    /// Panics if the new capacity exceeds `isize::MAX` bytes.
    ///
    /// # Examples
    ///
    /// ```
    /// let mut vec = vec![1];
    /// vec.reserve(10);
    /// assert!(vec.capacity() >= 11);
    /// ```
    #[cfg(not(no_global_oom_handling))]
    #[stable(feature = "rust1", since = "1.0.0")]
    pub fn reserve(&mut self, additional: usize) {
}

    /// Reserves the minimum capacity for exactly `additional` more elements to
    /// be inserted in the given `Vec<T>`. After calling `reserve_exact`,
    /// capacity will be greater than or equal to `self.len() + additional`.
    /// Does nothing if the capacity is already sufficient.
    ///
    /// Note that the allocator may give the collection more space than it
    /// requests. Therefore, capacity can not be relied upon to be precisely
    /// minimal. Prefer [`reserve`] if future insertions are expected.
    ///
    /// [`reserve`]: Vec::reserve
    ///
    /// # Panics
    ///
    /// Panics if the new capacity overflows `usize`.
    ///
    /// # Examples
    ///
    /// ```
    /// let mut vec = vec![1];
    /// vec.reserve_exact(10);
    /// assert!(vec.capacity() >= 11);
    /// ```
    #[cfg(not(no_global_oom_handling))]
    #[stable(feature = "rust1", since = "1.0.0")]
    pub fn reserve_exact(&mut self, additional: usize) {
}

    /// Tries to reserve capacity for at least `additional` more elements to be inserted
    /// in the given `Vec<T>`. The collection may reserve more space to avoid
    /// frequent reallocations. After calling `try_reserve`, capacity will be
    /// greater than or equal to `self.len() + additional`. Does nothing if
    /// capacity is already sufficient.
    ///
    /// # Errors
    ///
    /// If the capacity overflows, or the allocator reports a failure, then an error
    /// is returned.
    ///
    /// # Examples
    ///
    /// ```
    /// #![feature(try_reserve)]
    /// use std::collections::TryReserveError;
    ///
    /// fn process_data(data: &[u32]) -> Result<Vec<u32>, TryReserveError> {
    ///     let mut output = Vec::new();
    ///
    ///     // Pre-reserve the memory, exiting if we can't
    ///     output.try_reserve(data.len())?;
    ///
    ///     // Now we know this can't OOM in the middle of our complex work
    ///     output.extend(data.iter().map(|&val| {
    ///         val * 2 + 5 // very complicated
    ///     }));
    ///
    ///     Ok(output)
    /// }
    /// # process_data(&[1, 2, 3]).expect("why is the test harness OOMing on 12 bytes?");
    /// ```
    #[unstable(feature = "try_reserve", reason = "new API", issue = "48043")]
    pub fn try_reserve(&mut self, additional: usize) -> Result<(), TryReserveError> {
}

    /// Tries to reserve the minimum capacity for exactly `additional`
    /// elements to be inserted in the given `Vec<T>`. After calling
    /// `try_reserve_exact`, capacity will be greater than or equal to
    /// `self.len() + additional` if it returns `Ok(())`.
    /// Does nothing if the capacity is already sufficient.
    ///
    /// Note that the allocator may give the collection more space than it
    /// requests. Therefore, capacity can not be relied upon to be precisely
    /// minimal. Prefer [`reserve`] if future insertions are expected.
    ///
    /// [`reserve`]: Vec::reserve
    ///
    /// # Errors
    ///
    /// If the capacity overflows, or the allocator reports a failure, then an error
    /// is returned.
    ///
    /// # Examples
    ///
    /// ```
    /// #![feature(try_reserve)]
    /// use std::collections::TryReserveError;
    ///
    /// fn process_data(data: &[u32]) -> Result<Vec<u32>, TryReserveError> {
    ///     let mut output = Vec::new();
    ///
    ///     // Pre-reserve the memory, exiting if we can't
    ///     output.try_reserve_exact(data.len())?;
    ///
    ///     // Now we know this can't OOM in the middle of our complex work
    ///     output.extend(data.iter().map(|&val| {
    ///         val * 2 + 5 // very complicated
    ///     }));
    ///
    ///     Ok(output)
    /// }
    /// # process_data(&[1, 2, 3]).expect("why is the test harness OOMing on 12 bytes?");
    /// ```
    #[unstable(feature = "try_reserve", reason = "new API", issue = "48043")]
    pub fn try_reserve_exact(&mut self, additional: usize) -> Result<(), TryReserveError> {
}

    /// Shrinks the capacity of the vector as much as possible.
    ///
    /// It will drop down as close as possible to the length but the allocator
    /// may still inform the vector that there is space for a few more elements.
    ///
    /// # Examples
    ///
    /// ```
    /// let mut vec = Vec::with_capacity(10);
    /// vec.extend([1, 2, 3]);
    /// assert_eq!(vec.capacity(), 10);
    /// vec.shrink_to_fit();
    /// assert!(vec.capacity() >= 3);
    /// ```
    #[cfg(not(no_global_oom_handling))]
    #[stable(feature = "rust1", since = "1.0.0")]
    pub fn shrink_to_fit(&mut self) {
}

    /// Shrinks the capacity of the vector with a lower bound.
    ///
    /// The capacity will remain at least as large as both the length
    /// and the supplied value.
    ///
    /// If the current capacity is less than the lower limit, this is a no-op.
    ///
    /// # Examples
    ///
    /// ```
    /// let mut vec = Vec::with_capacity(10);
    /// vec.extend([1, 2, 3]);
    /// assert_eq!(vec.capacity(), 10);
    /// vec.shrink_to(4);
    /// assert!(vec.capacity() >= 4);
    /// vec.shrink_to(0);
    /// assert!(vec.capacity() >= 3);
    /// ```
    #[cfg(not(no_global_oom_handling))]
    #[stable(feature = "shrink_to", since = "1.56.0")]
    pub fn shrink_to(&mut self, min_capacity: usize) {
}

    /// Converts the vector into [`Box<[T]>`][owned slice].
    ///
    /// Note that this will drop any excess capacity.
    ///
    /// [owned slice]: Box
    ///
    /// # Examples
    ///
    /// ```
    /// let v = vec![1, 2, 3];
    ///
    /// let slice = v.into_boxed_slice();
    /// ```
    ///
    /// Any excess capacity is removed:
    ///
    /// ```
    /// let mut vec = Vec::with_capacity(10);
    /// vec.extend([1, 2, 3]);
    ///
    /// assert_eq!(vec.capacity(), 10);
    /// let slice = vec.into_boxed_slice();
    /// assert_eq!(slice.into_vec().capacity(), 3);
    /// ```
    #[cfg(not(no_global_oom_handling))]
    #[stable(feature = "rust1", since = "1.0.0")]
    pub fn into_boxed_slice(mut self) -> Box<[T], A> {
}

    /// Shortens the vector, keeping the first `len` elements and dropping
    /// the rest.
    ///
    /// If `len` is greater than the vector's current length, this has no
    /// effect.
    ///
    /// The [`drain`] method can emulate `truncate`, but causes the excess
    /// elements to be returned instead of dropped.
    ///
    /// Note that this method has no effect on the allocated capacity
    /// of the vector.
    ///
    /// # Examples
    ///
    /// Truncating a five element vector to two elements:
    ///
    /// ```
    /// let mut vec = vec![1, 2, 3, 4, 5];
    /// vec.truncate(2);
    /// assert_eq!(vec, [1, 2]);
    /// ```
    ///
    /// No truncation occurs when `len` is greater than the vector's current
    /// length:
    ///
    /// ```
    /// let mut vec = vec![1, 2, 3];
    /// vec.truncate(8);
    /// assert_eq!(vec, [1, 2, 3]);
    /// ```
    ///
    /// Truncating when `len == 0` is equivalent to calling the [`clear`]
    /// method.
    ///
    /// ```
    /// let mut vec = vec![1, 2, 3];
    /// vec.truncate(0);
    /// assert_eq!(vec, []);
    /// ```
    ///
    /// [`clear`]: Vec::clear
    /// [`drain`]: Vec::drain
    #[stable(feature = "rust1", since = "1.0.0")]
    pub fn truncate(&mut self, len: usize) {
}

    /// Extracts a slice containing the entire vector.
    ///
    /// Equivalent to `&s[..]`.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::io::{self, Write};
    /// let buffer = vec![1, 2, 3, 5, 8];
    /// io::sink().write(buffer.as_slice()).unwrap();
    /// ```
    #[inline]
    #[stable(feature = "vec_as_slice", since = "1.7.0")]
    pub fn as_slice(&self) -> &[T] {
}

    /// Extracts a mutable slice of the entire vector.
    ///
    /// Equivalent to `&mut s[..]`.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::io::{self, Read};
    /// let mut buffer = vec![0; 3];
    /// io::repeat(0b101).read_exact(buffer.as_mut_slice()).unwrap();
    /// ```
    #[inline]
    #[stable(feature = "vec_as_slice", since = "1.7.0")]
    pub fn as_mut_slice(&mut self) -> &mut [T] {
}

    /// Returns a raw pointer to the vector's buffer.
    ///
    /// The caller must ensure that the vector outlives the pointer this
    /// function returns, or else it will end up pointing to garbage.
    /// Modifying the vector may cause its buffer to be reallocated,
    /// which would also make any pointers to it invalid.
    ///
    /// The caller must also ensure that the memory the pointer (non-transitively) points to
    /// is never written to (except inside an `UnsafeCell`) using this pointer or any pointer
    /// derived from it. If you need to mutate the contents of the slice, use [`as_mut_ptr`].
    ///
    /// # Examples
    ///
    /// ```
    /// let x = vec![1, 2, 4];
    /// let x_ptr = x.as_ptr();
    ///
    /// unsafe {
    ///     for i in 0..x.len() {
    ///         assert_eq!(*x_ptr.add(i), 1 << i);
    ///     }
    /// }
    /// ```
    ///
    /// [`as_mut_ptr`]: Vec::as_mut_ptr
    #[stable(feature = "vec_as_ptr", since = "1.37.0")]
    #[inline]
    pub fn as_ptr(&self) -> *const T {
}

    /// Returns an unsafe mutable pointer to the vector's buffer.
    ///
    /// The caller must ensure that the vector outlives the pointer this
    /// function returns, or else it will end up pointing to garbage.
    /// Modifying the vector may cause its buffer to be reallocated,
    /// which would also make any pointers to it invalid.
    ///
    /// # Examples
    ///
    /// ```
    /// // Allocate vector big enough for 4 elements.
    /// let size = 4;
    /// let mut x: Vec<i32> = Vec::with_capacity(size);
    /// let x_ptr = x.as_mut_ptr();
    ///
    /// // Initialize elements via raw pointer writes, then set length.
    /// unsafe {
    ///     for i in 0..size {
    ///         *x_ptr.add(i) = i as i32;
    ///     }
    ///     x.set_len(size);
    /// }
    /// assert_eq!(&*x, &[0, 1, 2, 3]);
    /// ```
    #[stable(feature = "vec_as_ptr", since = "1.37.0")]
    #[inline]
    pub fn as_mut_ptr(&mut self) -> *mut T {
}

    /// Returns a reference to the underlying allocator.
    #[unstable(feature = "allocator_api", issue = "32838")]
    #[inline]
    pub fn allocator(&self) -> &A {
}

    /// Forces the length of the vector to `new_len`.
    ///
    /// This is a low-level operation that maintains none of the normal
    /// invariants of the type. Normally changing the length of a vector
    /// is done using one of the safe operations instead, such as
    /// [`truncate`], [`resize`], [`extend`], or [`clear`].
    ///
    /// [`truncate`]: Vec::truncate
    /// [`resize`]: Vec::resize
    /// [`extend`]: Extend::extend
    /// [`clear`]: Vec::clear
    ///
    /// # Safety
    ///
    /// - `new_len` must be less than or equal to [`capacity()`].
    /// - The elements at `old_len..new_len` must be initialized.
    ///
    /// [`capacity()`]: Vec::capacity
    ///
    /// # Examples
    ///
    /// This method can be useful for situations in which the vector
    /// is serving as a buffer for other code, particularly over FFI:
    ///
    /// ```no_run
    /// # #![allow(dead_code)]
    /// # // This is just a minimal skeleton for the doc example;
    /// # // don't use this as a starting point for a real library.
    /// # pub struct StreamWrapper { strm: *mut std::ffi::c_void }
    /// # const Z_OK: i32 = 0;
    /// # extern "C" {
    /// #     fn deflateGetDictionary(
    /// #         strm: *mut std::ffi::c_void,
    /// #         dictionary: *mut u8,
    /// #         dictLength: *mut usize,
    /// #     ) -> i32;
    /// # }
    /// # impl StreamWrapper {
    /// pub fn get_dictionary(&self) -> Option<Vec<u8>> {
    ///     // Per the FFI method's docs, "32768 bytes is always enough".
    ///     let mut dict = Vec::with_capacity(32_768);
    ///     let mut dict_length = 0;
    ///     // SAFETY: When `deflateGetDictionary` returns `Z_OK`, it holds that:
    ///     // 1. `dict_length` elements were initialized.
    ///     // 2. `dict_length` <= the capacity (32_768)
    ///     // which makes `set_len` safe to call.
    ///     unsafe {
    ///         // Make the FFI call...
    ///         let r = deflateGetDictionary(self.strm, dict.as_mut_ptr(), &mut dict_length);
    ///         if r == Z_OK {
    ///             // ...and update the length to what was initialized.
    ///             dict.set_len(dict_length);
    ///             Some(dict)
    ///         } else {
    ///             None
    ///         }
    ///     }
    /// }
    /// # }
    /// ```
    ///
    /// While the following example is sound, there is a memory leak since
    /// the inner vectors were not freed prior to the `set_len` call:
    ///
    /// ```
    /// let mut vec = vec![vec![1, 0, 0],
    ///                    vec![0, 1, 0],
    ///                    vec![0, 0, 1]];
    /// // SAFETY:
    /// // 1. `old_len..0` is empty so no elements need to be initialized.
    /// // 2. `0 <= capacity` always holds whatever `capacity` is.
    /// unsafe {
    ///     vec.set_len(0);
    /// }
    /// ```
    ///
    /// Normally, here, one would use [`clear`] instead to correctly drop
    /// the contents and thus not leak memory.
    #[inline]
    #[stable(feature = "rust1", since = "1.0.0")]
    pub unsafe fn set_len(&mut self, new_len: usize) {
}

    /// Removes an element from the vector and returns it.
    ///
    /// The removed element is replaced by the last element of the vector.
    ///
    /// This does not preserve ordering, but is O(1).
    ///
    /// # Panics
    ///
    /// Panics if `index` is out of bounds.
    ///
    /// # Examples
    ///
    /// ```
    /// let mut v = vec!["foo", "bar", "baz", "qux"];
    ///
    /// assert_eq!(v.swap_remove(1), "bar");
    /// assert_eq!(v, ["foo", "qux", "baz"]);
    ///
    /// assert_eq!(v.swap_remove(0), "foo");
    /// assert_eq!(v, ["baz", "qux"]);
    /// ```
    #[inline]
    #[stable(feature = "rust1", since = "1.0.0")]
    pub fn swap_remove(&mut self, index: usize) -> T {
}

    /// Inserts an element at position `index` within the vector, shifting all
    /// elements after it to the right.
    ///
    /// # Panics
    ///
    /// Panics if `index > len`.
    ///
    /// # Examples
    ///
    /// ```
    /// let mut vec = vec![1, 2, 3];
    /// vec.insert(1, 4);
    /// assert_eq!(vec, [1, 4, 2, 3]);
    /// vec.insert(4, 5);
    /// assert_eq!(vec, [1, 4, 2, 3, 5]);
    /// ```
    #[cfg(not(no_global_oom_handling))]
    #[stable(feature = "rust1", since = "1.0.0")]
    pub fn insert(&mut self, index: usize, element: T) {
}

    /// Removes and returns the element at position `index` within the vector,
    /// shifting all elements after it to the left.
    ///
    /// Note: Because this shifts over the remaining elements, it has a
    /// worst-case performance of O(n). If you don't need the order of elements
    /// to be preserved, use [`swap_remove`] instead.
    ///
    /// [`swap_remove`]: Vec::swap_remove
    ///
    /// # Panics
    ///
    /// Panics if `index` is out of bounds.
    ///
    /// # Examples
    ///
    /// ```
    /// let mut v = vec![1, 2, 3];
    /// assert_eq!(v.remove(1), 2);
    /// assert_eq!(v, [1, 3]);
    /// ```
    #[stable(feature = "rust1", since = "1.0.0")]
    #[track_caller]
    pub fn remove(&mut self, index: usize) -> T {
}

    /// Retains only the elements specified by the predicate.
    ///
    /// In other words, remove all elements `e` such that `f(&e)` returns `false`.
    /// This method operates in place, visiting each element exactly once in the
    /// original order, and preserves the order of the retained elements.
    ///
    /// # Examples
    ///
    /// ```
    /// let mut vec = vec![1, 2, 3, 4];
    /// vec.retain(|&x| x % 2 == 0);
    /// assert_eq!(vec, [2, 4]);
    /// ```
    ///
    /// Because the elements are visited exactly once in the original order,
    /// external state may be used to decide which elements to keep.
    ///
    /// ```
    /// let mut vec = vec![1, 2, 3, 4, 5];
    /// let keep = [false, true, true, false, true];
    /// let mut iter = keep.iter();
    /// vec.retain(|_| *iter.next().unwrap());
    /// assert_eq!(vec, [2, 3, 5]);
    /// ```
    #[stable(feature = "rust1", since = "1.0.0")]
    pub fn retain<F>(&mut self, mut f: F)
    where
        F: FnMut(&T) -> bool,
    {
}

    /// Removes all but the first of consecutive elements in the vector that resolve to the same
    /// key.
    ///
    /// If the vector is sorted, this removes all duplicates.
    ///
    /// # Examples
    ///
    /// ```
    /// let mut vec = vec![10, 20, 21, 30, 20];
    ///
    /// vec.dedup_by_key(|i| *i / 10);
    ///
    /// assert_eq!(vec, [10, 20, 30, 20]);
    /// ```
    #[stable(feature = "dedup_by", since = "1.16.0")]
    #[inline]
    pub fn dedup_by_key<F, K>(&mut self, mut key: F)
    where
        F: FnMut(&mut T) -> K,
        K: PartialEq,
    {
}

    /// Removes all but the first of consecutive elements in the vector satisfying a given equality
    /// relation.
    ///
    /// The `same_bucket` function is passed references to two elements from the vector and
    /// must determine if the elements compare equal. The elements are passed in opposite order
    /// from their order in the slice, so if `same_bucket(a, b)` returns `true`, `a` is removed.
    ///
    /// If the vector is sorted, this removes all duplicates.
    ///
    /// # Examples
    ///
    /// ```
    /// let mut vec = vec!["foo", "bar", "Bar", "baz", "bar"];
    ///
    /// vec.dedup_by(|a, b| a.eq_ignore_ascii_case(b));
    ///
    /// assert_eq!(vec, ["foo", "bar", "baz", "bar"]);
    /// ```
    #[stable(feature = "dedup_by", since = "1.16.0")]
    pub fn dedup_by<F>(&mut self, mut same_bucket: F)
    where
        F: FnMut(&mut T, &mut T) -> bool,
    {
}

    /// Appends an element to the back of a collection.
    ///
    /// # Panics
    ///
    /// Panics if the new capacity exceeds `isize::MAX` bytes.
    ///
    /// # Examples
    ///
    /// ```
    /// let mut vec = vec![1, 2];
    /// vec.push(3);
    /// assert_eq!(vec, [1, 2, 3]);
    /// ```
    #[cfg(not(no_global_oom_handling))]
    #[inline]
    #[stable(feature = "rust1", since = "1.0.0")]
    pub fn push(&mut self, value: T) {
}

    /// Removes the last element from a vector and returns it, or [`None`] if it
    /// is empty.
    ///
    /// # Examples
    ///
    /// ```
    /// let mut vec = vec![1, 2, 3];
    /// assert_eq!(vec.pop(), Some(3));
    /// assert_eq!(vec, [1, 2]);
    /// ```
    #[inline]
    #[stable(feature = "rust1", since = "1.0.0")]
    pub fn pop(&mut self) -> Option<T> {
}

    /// Moves all the elements of `other` into `Self`, leaving `other` empty.
    ///
    /// # Panics
    ///
    /// Panics if the number of elements in the vector overflows a `usize`.
    ///
    /// # Examples
    ///
    /// ```
    /// let mut vec = vec![1, 2, 3];
    /// let mut vec2 = vec![4, 5, 6];
    /// vec.append(&mut vec2);
    /// assert_eq!(vec, [1, 2, 3, 4, 5, 6]);
    /// assert_eq!(vec2, []);
    /// ```
    #[cfg(not(no_global_oom_handling))]
    #[inline]
    #[stable(feature = "append", since = "1.4.0")]
    pub fn append(&mut self, other: &mut Self) {
}

    /// Appends elements to `Self` from other buffer.
    #[cfg(not(no_global_oom_handling))]
    #[inline]
    unsafe fn append_elements(&mut self, other: *const [T]) {
}

    /// Creates a draining iterator that removes the specified range in the vector
    /// and yields the removed items.
    ///
    /// When the iterator **is** dropped, all elements in the range are removed
    /// from the vector, even if the iterator was not fully consumed. If the
    /// iterator **is not** dropped (with [`mem::forget`] for example), it is
    /// unspecified how many elements are removed.
    ///
    /// # Panics
    ///
    /// Panics if the starting point is greater than the end point or if
    /// the end point is greater than the length of the vector.
    ///
    /// # Examples
    ///
    /// ```
    /// let mut v = vec![1, 2, 3];
    /// let u: Vec<_> = v.drain(1..).collect();
    /// assert_eq!(v, &[1]);
    /// assert_eq!(u, &[2, 3]);
    ///
    /// // A full range clears the vector
    /// v.drain(..);
    /// assert_eq!(v, &[]);
    /// ```
    #[stable(feature = "drain", since = "1.6.0")]
    pub fn drain<R>(&mut self, range: R) -> Drain<'_, T, A>
    where
        R: RangeBounds<usize>,
    {
}

    /// Clears the vector, removing all values.
    ///
    /// Note that this method has no effect on the allocated capacity
    /// of the vector.
    ///
    /// # Examples
    ///
    /// ```
    /// let mut v = vec![1, 2, 3];
    ///
    /// v.clear();
    ///
    /// assert!(v.is_empty());
    /// ```
    #[inline]
    #[stable(feature = "rust1", since = "1.0.0")]
    pub fn clear(&mut self) {
}

    /// Returns the number of elements in the vector, also referred to
    /// as its 'length'.
    ///
    /// # Examples
    ///
    /// ```
    /// let a = vec![1, 2, 3];
    /// assert_eq!(a.len(), 3);
    /// ```
    #[inline]
    #[stable(feature = "rust1", since = "1.0.0")]
    pub fn len(&self) -> usize {
}

    /// Returns `true` if the vector contains no elements.
    ///
    /// # Examples
    ///
    /// ```
    /// let mut v = Vec::new();
    /// assert!(v.is_empty());
    ///
    /// v.push(1);
    /// assert!(!v.is_empty());
    /// ```
    #[stable(feature = "rust1", since = "1.0.0")]
    pub fn is_empty(&self) -> bool {
}

    /// Splits the collection into two at the given index.
    ///
    /// Returns a newly allocated vector containing the elements in the range
    /// `[at, len)`. After the call, the original vector will be left containing
    /// the elements `[0, at)` with its previous capacity unchanged.
    ///
    /// # Panics
    ///
    /// Panics if `at > len`.
    ///
    /// # Examples
    ///
    /// ```
    /// let mut vec = vec![1, 2, 3];
    /// let vec2 = vec.split_off(1);
    /// assert_eq!(vec, [1]);
    /// assert_eq!(vec2, [2, 3]);
    /// ```
    #[cfg(not(no_global_oom_handling))]
    #[inline]
    #[must_use = "use `.truncate()` if you don't need the other half"]
    #[stable(feature = "split_off", since = "1.4.0")]
    pub fn split_off(&mut self, at: usize) -> Self
    where
        A: Clone,
    {
}

    /// Resizes the `Vec` in-place so that `len` is equal to `new_len`.
    ///
    /// If `new_len` is greater than `len`, the `Vec` is extended by the
    /// difference, with each additional slot filled with the result of
    /// calling the closure `f`. The return values from `f` will end up
    /// in the `Vec` in the order they have been generated.
    ///
    /// If `new_len` is less than `len`, the `Vec` is simply truncated.
    ///
    /// This method uses a closure to create new values on every push. If
    /// you'd rather [`Clone`] a given value, use [`Vec::resize`]. If you
    /// want to use the [`Default`] trait to generate values, you can
    /// pass [`Default::default`] as the second argument.
    ///
    /// # Examples
    ///
    /// ```
    /// let mut vec = vec![1, 2, 3];
    /// vec.resize_with(5, Default::default);
    /// assert_eq!(vec, [1, 2, 3, 0, 0]);
    ///
    /// let mut vec = vec![];
    /// let mut p = 1;
    /// vec.resize_with(4, || { p *= 2; p });
    /// assert_eq!(vec, [2, 4, 8, 16]);
    /// ```
    #[cfg(not(no_global_oom_handling))]
    #[stable(feature = "vec_resize_with", since = "1.33.0")]
    pub fn resize_with<F>(&mut self, new_len: usize, f: F)
    where
        F: FnMut() -> T,
    {
}

    /// Consumes and leaks the `Vec`, returning a mutable reference to the contents,
    /// `&'a mut [T]`. Note that the type `T` must outlive the chosen lifetime
    /// `'a`. If the type has only static references, or none at all, then this
    /// may be chosen to be `'static`.
    ///
    /// This function is similar to the [`leak`][Box::leak] function on [`Box`]
    /// except that there is no way to recover the leaked memory.
    ///
    /// This function is mainly useful for data that lives for the remainder of
    /// the program's life. Dropping the returned reference will cause a memory
    /// leak.
    ///
    /// # Examples
    ///
    /// Simple usage:
    ///
    /// ```
    /// let x = vec![1, 2, 3];
    /// let static_ref: &'static mut [usize] = x.leak();
    /// static_ref[0] += 1;
    /// assert_eq!(static_ref, &[2, 2, 3]);
    /// ```
    #[cfg(not(no_global_oom_handling))]
    #[stable(feature = "vec_leak", since = "1.47.0")]
    #[inline]
    pub fn leak<'a>(self) -> &'a mut [T]
    where
        A: 'a,
    {
}

    /// Returns the remaining spare capacity of the vector as a slice of
    /// `MaybeUninit<T>`.
    ///
    /// The returned slice can be used to fill the vector with data (e.g. by
    /// reading from a file) before marking the data as initialized using the
    /// [`set_len`] method.
    ///
    /// [`set_len`]: Vec::set_len
    ///
    /// # Examples
    ///
    /// ```
    /// #![feature(vec_spare_capacity, maybe_uninit_extra)]
    ///
    /// // Allocate vector big enough for 10 elements.
    /// let mut v = Vec::with_capacity(10);
    ///
    /// // Fill in the first 3 elements.
    /// let uninit = v.spare_capacity_mut();
    /// uninit[0].write(0);
    /// uninit[1].write(1);
    /// uninit[2].write(2);
    ///
    /// // Mark the first 3 elements of the vector as being initialized.
    /// unsafe {
    ///     v.set_len(3);
    /// }
    ///
    /// assert_eq!(&v, &[0, 1, 2]);
    /// ```
    #[unstable(feature = "vec_spare_capacity", issue = "75017")]
    #[inline]
    pub fn spare_capacity_mut(&mut self) -> &mut [MaybeUninit<T>] {
}

    /// Returns vector content as a slice of `T`, along with the remaining spare
    /// capacity of the vector as a slice of `MaybeUninit<T>`.
    ///
    /// The returned spare capacity slice can be used to fill the vector with data
    /// (e.g. by reading from a file) before marking the data as initialized using
    /// the [`set_len`] method.
    ///
    /// [`set_len`]: Vec::set_len
    ///
    /// Note that this is a low-level API, which should be used with care for
    /// optimization purposes. If you need to append data to a `Vec`
    /// you can use [`push`], [`extend`], [`extend_from_slice`],
    /// [`extend_from_within`], [`insert`], [`append`], [`resize`] or
    /// [`resize_with`], depending on your exact needs.
    ///
    /// [`push`]: Vec::push
    /// [`extend`]: Vec::extend
    /// [`extend_from_slice`]: Vec::extend_from_slice
    /// [`extend_from_within`]: Vec::extend_from_within
    /// [`insert`]: Vec::insert
    /// [`append`]: Vec::append
    /// [`resize`]: Vec::resize
    /// [`resize_with`]: Vec::resize_with
    ///
    /// # Examples
    ///
    /// ```
    /// #![feature(vec_split_at_spare, maybe_uninit_extra)]
    ///
    /// let mut v = vec![1, 1, 2];
    ///
    /// // Reserve additional space big enough for 10 elements.
    /// v.reserve(10);
    ///
    /// let (init, uninit) = v.split_at_spare_mut();
    /// let sum = init.iter().copied().sum::<u32>();
    ///
    /// // Fill in the next 4 elements.
    /// uninit[0].write(sum);
    /// uninit[1].write(sum * 2);
    /// uninit[2].write(sum * 3);
    /// uninit[3].write(sum * 4);
    ///
    /// // Mark the 4 elements of the vector as being initialized.
    /// unsafe {
    ///     let len = v.len();
    ///     v.set_len(len + 4);
    /// }
    ///
    /// assert_eq!(&v, &[1, 1, 2, 4, 8, 12, 16]);
    /// ```
    #[unstable(feature = "vec_split_at_spare", issue = "81944")]
    #[inline]
    pub fn split_at_spare_mut(&mut self) -> (&mut [T], &mut [MaybeUninit<T>]) {
}

    /// Safety: changing returned .2 (&mut usize) is considered the same as calling `.set_len(_)`.
    ///
    /// This method provides unique access to all vec parts at once in `extend_from_within`.
    unsafe fn split_at_spare_mut_with_len(
        &mut self,
    ) -> (&mut [T], &mut [MaybeUninit<T>], &mut usize) {
}
}

impl<T: Clone, A: Allocator> Vec<T, A> {
    /// Resizes the `Vec` in-place so that `len` is equal to `new_len`.
    ///
    /// If `new_len` is greater than `len`, the `Vec` is extended by the
    /// difference, with each additional slot filled with `value`.
    /// If `new_len` is less than `len`, the `Vec` is simply truncated.
    ///
    /// This method requires `T` to implement [`Clone`],
    /// in order to be able to clone the passed value.
    /// If you need more flexibility (or want to rely on [`Default`] instead of
    /// [`Clone`]), use [`Vec::resize_with`].
    ///
    /// # Examples
    ///
    /// ```
    /// let mut vec = vec!["hello"];
    /// vec.resize(3, "world");
    /// assert_eq!(vec, ["hello", "world", "world"]);
    ///
    /// let mut vec = vec![1, 2, 3, 4];
    /// vec.resize(2, 0);
    /// assert_eq!(vec, [1, 2]);
    /// ```
    #[cfg(not(no_global_oom_handling))]
    #[stable(feature = "vec_resize", since = "1.5.0")]
    pub fn resize(&mut self, new_len: usize, value: T) {
}

    /// Clones and appends all elements in a slice to the `Vec`.
    ///
    /// Iterates over the slice `other`, clones each element, and then appends
    /// it to this `Vec`. The `other` vector is traversed in-order.
    ///
    /// Note that this function is same as [`extend`] except that it is
    /// specialized to work with slices instead. If and when Rust gets
    /// specialization this function will likely be deprecated (but still
    /// available).
    ///
    /// # Examples
    ///
    /// ```
    /// let mut vec = vec![1];
    /// vec.extend_from_slice(&[2, 3, 4]);
    /// assert_eq!(vec, [1, 2, 3, 4]);
    /// ```
    ///
    /// [`extend`]: Vec::extend
    #[cfg(not(no_global_oom_handling))]
    #[stable(feature = "vec_extend_from_slice", since = "1.6.0")]
    pub fn extend_from_slice(&mut self, other: &[T]) {
}

    /// Copies elements from `src` range to the end of the vector.
    ///
    /// ## Examples
    ///
    /// ```
    /// let mut vec = vec![0, 1, 2, 3, 4];
    ///
    /// vec.extend_from_within(2..);
    /// assert_eq!(vec, [0, 1, 2, 3, 4, 2, 3, 4]);
    ///
    /// vec.extend_from_within(..2);
    /// assert_eq!(vec, [0, 1, 2, 3, 4, 2, 3, 4, 0, 1]);
    ///
    /// vec.extend_from_within(4..8);
    /// assert_eq!(vec, [0, 1, 2, 3, 4, 2, 3, 4, 0, 1, 4, 2, 3, 4]);
    /// ```
    #[cfg(not(no_global_oom_handling))]
    #[stable(feature = "vec_extend_from_within", since = "1.53.0")]
    pub fn extend_from_within<R>(&mut self, src: R)
    where
        R: RangeBounds<usize>,
    {
}
}

// This code generalizes `extend_with_{element,default}`.
trait ExtendWith<T> {
    fn next(&mut self) -> T;
    fn last(self) -> T;
}

struct ExtendElement<T>(T);
impl<T: Clone> ExtendWith<T> for ExtendElement<T> {
    fn next(&mut self) -> T {
}
    fn last(self) -> T {
}
}

struct ExtendDefault;
impl<T: Default> ExtendWith<T> for ExtendDefault {
    fn next(&mut self) -> T {
}
    fn last(self) -> T {
}
}

struct ExtendFunc<F>(F);
impl<T, F: FnMut() -> T> ExtendWith<T> for ExtendFunc<F> {
    fn next(&mut self) -> T {
}
    fn last(mut self) -> T {
}
}

impl<T, A: Allocator> Vec<T, A> {
    #[cfg(not(no_global_oom_handling))]
    /// Extend the vector by `n` values, using the given generator.
    fn extend_with<E: ExtendWith<T>>(&mut self, n: usize, mut value: E) {
}
}

impl<T: PartialEq, A: Allocator> Vec<T, A> {
    /// Removes consecutive repeated elements in the vector according to the
    /// [`PartialEq`] trait implementation.
    ///
    /// If the vector is sorted, this removes all duplicates.
    ///
    /// # Examples
    ///
    /// ```
    /// let mut vec = vec![1, 2, 2, 3, 2];
    ///
    /// vec.dedup();
    ///
    /// assert_eq!(vec, [1, 2, 3, 2]);
    /// ```
    #[stable(feature = "rust1", since = "1.0.0")]
    #[inline]
    pub fn dedup(&mut self) {
}
}

////////////////////////////////////////////////////////////////////////////////
// Internal methods and functions
////////////////////////////////////////////////////////////////////////////////

#[doc(hidden)]
#[cfg(not(no_global_oom_handling))]
#[stable(feature = "rust1", since = "1.0.0")]
pub fn from_elem<T: Clone>(elem: T, n: usize) -> Vec<T> {
}

#[doc(hidden)]
#[cfg(not(no_global_oom_handling))]
#[unstable(feature = "allocator_api", issue = "32838")]
pub fn from_elem_in<T: Clone, A: Allocator>(elem: T, n: usize, alloc: A) -> Vec<T, A> {
}

trait ExtendFromWithinSpec {
    /// # Safety
    ///
    /// - `src` needs to be valid index
    /// - `self.capacity() - self.len()` must be `>= src.len()`
    unsafe fn spec_extend_from_within(&mut self, src: Range<usize>);
}

impl<T: Clone, A: Allocator> ExtendFromWithinSpec for Vec<T, A> {
    default unsafe fn spec_extend_from_within(&mut self, src: Range<usize>) {
}
}

impl<T: Copy, A: Allocator> ExtendFromWithinSpec for Vec<T, A> {
    unsafe fn spec_extend_from_within(&mut self, src: Range<usize>) {
}
}

////////////////////////////////////////////////////////////////////////////////
// Common trait implementations for Vec
////////////////////////////////////////////////////////////////////////////////

#[stable(feature = "rust1", since = "1.0.0")]
impl<T, A: Allocator> ops::Deref for Vec<T, A> {
    type Target = [T];

    fn deref(&self) -> &[T] {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<T, A: Allocator> ops::DerefMut for Vec<T, A> {
    fn deref_mut(&mut self) -> &mut [T] {
}
}

#[cfg(not(no_global_oom_handling))]
trait SpecCloneFrom {
    fn clone_from(this: &mut Self, other: &Self);
}

#[cfg(not(no_global_oom_handling))]
impl<T: Clone, A: Allocator> SpecCloneFrom for Vec<T, A> {
    default fn clone_from(this: &mut Self, other: &Self) {
}
}

#[cfg(not(no_global_oom_handling))]
impl<T: Copy, A: Allocator> SpecCloneFrom for Vec<T, A> {
    fn clone_from(this: &mut Self, other: &Self) {
}
}

#[cfg(not(no_global_oom_handling))]
#[stable(feature = "rust1", since = "1.0.0")]
impl<T: Clone, A: Allocator + Clone> Clone for Vec<T, A> {
    #[cfg(not(test))]
    fn clone(&self) -> Self {
}

    // HACK(japaric): with cfg(test) the inherent `[T]::to_vec` method, which is
    // required for this method definition, is not available. Instead use the
    // `slice::to_vec`  function which is only available with cfg(test)
    // NB see the slice::hack module in slice.rs for more information
    #[cfg(test)]
    fn clone(&self) -> Self {
}

    fn clone_from(&mut self, other: &Self) {
}
}

/// The hash of a vector is the same as that of the corresponding slice,
/// as required by the `core::borrow::Borrow` implementation.
///
/// ```
/// #![feature(build_hasher_simple_hash_one)]
/// use std::hash::BuildHasher;
///
/// let b = std::collections::hash_map::RandomState::new();
/// let v: Vec<u8> = vec![0xa8, 0x3c, 0x09];
/// let s: &[u8] = &[0xa8, 0x3c, 0x09];
/// assert_eq!(b.hash_one(v), b.hash_one(s));
/// ```
#[stable(feature = "rust1", since = "1.0.0")]
impl<T: Hash, A: Allocator> Hash for Vec<T, A> {
    #[inline]
    fn hash<H: Hasher>(&self, state: &mut H) {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
#[rustc_on_unimplemented(
    message = "vector indices are of type `usize` or ranges of `usize`",
    label = "vector indices are of type `usize` or ranges of `usize`"
)]
impl<T, I: SliceIndex<[T]>, A: Allocator> Index<I> for Vec<T, A> {
    type Output = I::Output;

    #[inline]
    fn index(&self, index: I) -> &Self::Output {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
#[rustc_on_unimplemented(
    message = "vector indices are of type `usize` or ranges of `usize`",
    label = "vector indices are of type `usize` or ranges of `usize`"
)]
impl<T, I: SliceIndex<[T]>, A: Allocator> IndexMut<I> for Vec<T, A> {
    #[inline]
    fn index_mut(&mut self, index: I) -> &mut Self::Output {
}
}

#[cfg(not(no_global_oom_handling))]
#[stable(feature = "rust1", since = "1.0.0")]
impl<T> FromIterator<T> for Vec<T> {
    #[inline]
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Vec<T> {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<T, A: Allocator> IntoIterator for Vec<T, A> {
    type Item = T;
    type IntoIter = IntoIter<T, A>;

    /// Creates a consuming iterator, that is, one that moves each value out of
    /// the vector (from start to end). The vector cannot be used after calling
    /// this.
    ///
    /// # Examples
    ///
    /// ```
    /// let v = vec!["a".to_string(), "b".to_string()];
    /// for s in v.into_iter() {
    ///     // s has type String, not &String
    ///     println!("{}", s);
    /// }
    /// ```
    #[inline]
    fn into_iter(self) -> IntoIter<T, A> {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<'a, T, A: Allocator> IntoIterator for &'a Vec<T, A> {
    type Item = &'a T;
    type IntoIter = slice::Iter<'a, T>;

    fn into_iter(self) -> slice::Iter<'a, T> {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<'a, T, A: Allocator> IntoIterator for &'a mut Vec<T, A> {
    type Item = &'a mut T;
    type IntoIter = slice::IterMut<'a, T>;

    fn into_iter(self) -> slice::IterMut<'a, T> {
}
}

#[cfg(not(no_global_oom_handling))]
#[stable(feature = "rust1", since = "1.0.0")]
impl<T, A: Allocator> Extend<T> for Vec<T, A> {
    #[inline]
    fn extend<I: IntoIterator<Item = T>>(&mut self, iter: I) {
}

    #[inline]
    fn extend_one(&mut self, item: T) {
}

    #[inline]
    fn extend_reserve(&mut self, additional: usize) {
}
}

impl<T, A: Allocator> Vec<T, A> {
    // leaf method to which various SpecFrom/SpecExtend implementations delegate when
    // they have no further optimizations to apply
    #[cfg(not(no_global_oom_handling))]
    fn extend_desugared<I: Iterator<Item = T>>(&mut self, mut iterator: I) {
}

    /// Creates a splicing iterator that replaces the specified range in the vector
    /// with the given `replace_with` iterator and yields the removed items.
    /// `replace_with` does not need to be the same length as `range`.
    ///
    /// `range` is removed even if the iterator is not consumed until the end.
    ///
    /// It is unspecified how many elements are removed from the vector
    /// if the `Splice` value is leaked.
    ///
    /// The input iterator `replace_with` is only consumed when the `Splice` value is dropped.
    ///
    /// This is optimal if:
    ///
    /// * The tail (elements in the vector after `range`) is empty,
    /// * or `replace_with` yields fewer or equal elements than `range`’s length
    /// * or the lower bound of its `size_hint()` is exact.
    ///
    /// Otherwise, a temporary vector is allocated and the tail is moved twice.
    ///
    /// # Panics
    ///
    /// Panics if the starting point is greater than the end point or if
    /// the end point is greater than the length of the vector.
    ///
    /// # Examples
    ///
    /// ```
    /// let mut v = vec![1, 2, 3];
    /// let new = [7, 8];
    /// let u: Vec<_> = v.splice(..2, new).collect();
    /// assert_eq!(v, &[7, 8, 3]);
    /// assert_eq!(u, &[1, 2]);
    /// ```
    #[cfg(not(no_global_oom_handling))]
    #[inline]
    #[stable(feature = "vec_splice", since = "1.21.0")]
    pub fn splice<R, I>(&mut self, range: R, replace_with: I) -> Splice<'_, I::IntoIter, A>
    where
        R: RangeBounds<usize>,
        I: IntoIterator<Item = T>,
    {
}

    /// Creates an iterator which uses a closure to determine if an element should be removed.
    ///
    /// If the closure returns true, then the element is removed and yielded.
    /// If the closure returns false, the element will remain in the vector and will not be yielded
    /// by the iterator.
    ///
    /// Using this method is equivalent to the following code:
    ///
    /// ```
    /// # let some_predicate = |x: &mut i32| { *x == 2 || *x == 3 || *x == 6 };
    /// # let mut vec = vec![1, 2, 3, 4, 5, 6];
    /// let mut i = 0;
    /// while i < vec.len() {
    ///     if some_predicate(&mut vec[i]) {
    ///         let val = vec.remove(i);
    ///         // your code here
    ///     } else {
    ///         i += 1;
    ///     }
    /// }
    ///
    /// # assert_eq!(vec, vec![1, 4, 5]);
    /// ```
    ///
    /// But `drain_filter` is easier to use. `drain_filter` is also more efficient,
    /// because it can backshift the elements of the array in bulk.
    ///
    /// Note that `drain_filter` also lets you mutate every element in the filter closure,
    /// regardless of whether you choose to keep or remove it.
    ///
    /// # Examples
    ///
    /// Splitting an array into evens and odds, reusing the original allocation:
    ///
    /// ```
    /// #![feature(drain_filter)]
    /// let mut numbers = vec![1, 2, 3, 4, 5, 6, 8, 9, 11, 13, 14, 15];
    ///
    /// let evens = numbers.drain_filter(|x| *x % 2 == 0).collect::<Vec<_>>();
    /// let odds = numbers;
    ///
    /// assert_eq!(evens, vec![2, 4, 6, 8, 14]);
    /// assert_eq!(odds, vec![1, 3, 5, 9, 11, 13, 15]);
    /// ```
    #[unstable(feature = "drain_filter", reason = "recently added", issue = "43244")]
    pub fn drain_filter<F>(&mut self, filter: F) -> DrainFilter<'_, T, F, A>
    where
        F: FnMut(&mut T) -> bool,
    {
}
}

/// Extend implementation that copies elements out of references before pushing them onto the Vec.
///
/// This implementation is specialized for slice iterators, where it uses [`copy_from_slice`] to
/// append the entire slice at once.
///
/// [`copy_from_slice`]: slice::copy_from_slice
#[cfg(not(no_global_oom_handling))]
#[stable(feature = "extend_ref", since = "1.2.0")]
impl<'a, T: Copy + 'a, A: Allocator + 'a> Extend<&'a T> for Vec<T, A> {
    fn extend<I: IntoIterator<Item = &'a T>>(&mut self, iter: I) {
}

    #[inline]
    fn extend_one(&mut self, &item: &'a T) {
}

    #[inline]
    fn extend_reserve(&mut self, additional: usize) {
}
}

/// Implements comparison of vectors, [lexicographically](core::cmp::Ord#lexicographical-comparison).
#[stable(feature = "rust1", since = "1.0.0")]
impl<T: PartialOrd, A: Allocator> PartialOrd for Vec<T, A> {
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<T: Eq, A: Allocator> Eq for Vec<T, A> {}

/// Implements ordering of vectors, [lexicographically](core::cmp::Ord#lexicographical-comparison).
#[stable(feature = "rust1", since = "1.0.0")]
impl<T: Ord, A: Allocator> Ord for Vec<T, A> {
    #[inline]
    fn cmp(&self, other: &Self) -> Ordering {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
unsafe impl<#[may_dangle] T, A: Allocator> Drop for Vec<T, A> {
    fn drop(&mut self) {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
#[rustc_const_unstable(feature = "const_default_impls", issue = "87864")]
impl<T> const Default for Vec<T> {
    /// Creates an empty `Vec<T>`.
    fn default() -> Vec<T> {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<T: fmt::Debug, A: Allocator> fmt::Debug for Vec<T, A> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<T, A: Allocator> AsRef<Vec<T, A>> for Vec<T, A> {
    fn as_ref(&self) -> &Vec<T, A> {
}
}

#[stable(feature = "vec_as_mut", since = "1.5.0")]
impl<T, A: Allocator> AsMut<Vec<T, A>> for Vec<T, A> {
    fn as_mut(&mut self) -> &mut Vec<T, A> {
}
}

#[stable(feature = "rust1", since = "1.0.0")]
impl<T, A: Allocator> AsRef<[T]> for Vec<T, A> {
    fn as_ref(&self) -> &[T] {
}
}

#[stable(feature = "vec_as_mut", since = "1.5.0")]
impl<T, A: Allocator> AsMut<[T]> for Vec<T, A> {
    fn as_mut(&mut self) -> &mut [T] {
}
}

#[cfg(not(no_global_oom_handling))]
#[stable(feature = "rust1", since = "1.0.0")]
impl<T: Clone> From<&[T]> for Vec<T> {
    /// Allocate a `Vec<T>` and fill it by cloning `s`'s items.
    ///
    /// # Examples
    ///
    /// ```
    /// assert_eq!(Vec::from(&[1, 2, 3][..]), vec![1, 2, 3]);
    /// ```
    #[cfg(not(test))]
    fn from(s: &[T]) -> Vec<T> {
}
    #[cfg(test)]
    fn from(s: &[T]) -> Vec<T> {
}
}

#[cfg(not(no_global_oom_handling))]
#[stable(feature = "vec_from_mut", since = "1.19.0")]
impl<T: Clone> From<&mut [T]> for Vec<T> {
    /// Allocate a `Vec<T>` and fill it by cloning `s`'s items.
    ///
    /// # Examples
    ///
    /// ```
    /// assert_eq!(Vec::from(&mut [1, 2, 3][..]), vec![1, 2, 3]);
    /// ```
    #[cfg(not(test))]
    fn from(s: &mut [T]) -> Vec<T> {
}
    #[cfg(test)]
    fn from(s: &mut [T]) -> Vec<T> {
}
}

#[stable(feature = "vec_from_array", since = "1.44.0")]
impl<T, const N: usize> From<[T; N]> for Vec<T> {
    #[cfg(not(test))]
    fn from(s: [T; N]) -> Vec<T> {
}
    /// Allocate a `Vec<T>` and move `s`'s items into it.
    ///
    /// # Examples
    ///
    /// ```
    /// assert_eq!(Vec::from([1, 2, 3]), vec![1, 2, 3]);
    /// ```
    #[cfg(test)]
    fn from(s: [T; N]) -> Vec<T> {
}
}

#[stable(feature = "vec_from_cow_slice", since = "1.14.0")]
impl<'a, T> From<Cow<'a, [T]>> for Vec<T>
where
    [T]: ToOwned<Owned = Vec<T>>,
{
    /// Convert a clone-on-write slice into a vector.
    ///
    /// If `s` already owns a `Vec<T>`, it will be returned directly.
    /// If `s` is borrowing a slice, a new `Vec<T>` will be allocated and
    /// filled by cloning `s`'s items into it.
    ///
    /// # Examples
    ///
    /// ```
    /// # use std::borrow::Cow;
    /// let o: Cow<[i32]> = Cow::Owned(vec![1, 2, 3]);
    /// let b: Cow<[i32]> = Cow::Borrowed(&[1, 2, 3]);
    /// assert_eq!(Vec::from(o), Vec::from(b));
    /// ```
    fn from(s: Cow<'a, [T]>) -> Vec<T> {
}
}

// note: test pulls in libstd, which causes errors here
#[cfg(not(test))]
#[stable(feature = "vec_from_box", since = "1.18.0")]
impl<T, A: Allocator> From<Box<[T], A>> for Vec<T, A> {
    /// Convert a boxed slice into a vector by transferring ownership of
    /// the existing heap allocation.
    ///
    /// # Examples
    ///
    /// ```
    /// let b: Box<[i32]> = vec![1, 2, 3].into_boxed_slice();
    /// assert_eq!(Vec::from(b), vec![1, 2, 3]);
    /// ```
    fn from(s: Box<[T], A>) -> Self {
}
}

// note: test pulls in libstd, which causes errors here
#[cfg(not(no_global_oom_handling))]
#[cfg(not(test))]
#[stable(feature = "box_from_vec", since = "1.20.0")]
impl<T, A: Allocator> From<Vec<T, A>> for Box<[T], A> {
    /// Convert a vector into a boxed slice.
    ///
    /// If `v` has excess capacity, its items will be moved into a
    /// newly-allocated buffer with exactly the right capacity.
    ///
    /// # Examples
    ///
    /// ```
    /// assert_eq!(Box::from(vec![1, 2, 3]), vec![1, 2, 3].into_boxed_slice());
    /// ```
    fn from(v: Vec<T, A>) -> Self {
}
}

#[cfg(not(no_global_oom_handling))]
#[stable(feature = "rust1", since = "1.0.0")]
impl From<&str> for Vec<u8> {
    /// Allocate a `Vec<u8>` and fill it with a UTF-8 string.
    ///
    /// # Examples
    ///
    /// ```
    /// assert_eq!(Vec::from("123"), vec![b'1', b'2', b'3']);
    /// ```
    fn from(s: &str) -> Vec<u8> {
}
}

#[stable(feature = "array_try_from_vec", since = "1.48.0")]
impl<T, A: Allocator, const N: usize> TryFrom<Vec<T, A>> for [T; N] {
    type Error = Vec<T, A>;

    /// Gets the entire contents of the `Vec<T>` as an array,
    /// if its size exactly matches that of the requested array.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::convert::TryInto;
    /// assert_eq!(vec![1, 2, 3].try_into(), Ok([1, 2, 3]));
    /// assert_eq!(<Vec<i32>>::new().try_into(), Ok([]));
    /// ```
    ///
    /// If the length doesn't match, the input comes back in `Err`:
    /// ```
    /// use std::convert::TryInto;
    /// let r: Result<[i32; 4], _> = (0..10).collect::<Vec<_>>().try_into();
    /// assert_eq!(r, Err(vec![0, 1, 2, 3, 4, 5, 6, 7, 8, 9]));
    /// ```
    ///
    /// If you're fine with just getting a prefix of the `Vec<T>`,
    /// you can call [`.truncate(N)`](Vec::truncate) first.
    /// ```
    /// use std::convert::TryInto;
    /// let mut v = String::from("hello world").into_bytes();
    /// v.sort();
    /// v.truncate(2);
    /// let [a, b]: [_; 2] = v.try_into().unwrap();
    /// assert_eq!(a, b' ');
    /// assert_eq!(b, b'd');
    /// ```
    fn try_from(mut vec: Vec<T, A>) -> Result<[T; N], Vec<T, A>> {
}
}
}

#[doc(hidden)]
#[unstable(feature = "liballoc_internals", issue = "none", reason = "implementation detail")]
pub mod __export {
    pub use core::format_args;
}
