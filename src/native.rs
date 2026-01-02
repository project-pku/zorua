//! Native types that can be stored in zorua structs with endian-aware storage.
//!
//! This module provides the [`ZoruaNative`] trait, which allows "native" Rust types
//! (types that don't care about endianness in normal usage) to be converted to/from
//! endian-aware storage types for binary I/O.
//!
//! # Example
//!
//! ```rust
//! use zorua::prelude::*;
//!
//! /// A Pokémon's personality value - just a u32 in normal usage
//! #[derive(Clone, Copy, Debug, PartialEq, Eq)]
//! pub struct Pid(pub u32);
//!
//! impl Pid {
//!     pub fn new(val: u32) -> Self { Pid(val) }
//!     pub fn value(&self) -> u32 { self.0 }
//! }
//!
//! // Implement ZoruaNative for Pid, backed by U32
//! impl<E: Endian> ZoruaNative<U32<E>> for Pid {
//!     const IS_FALLIBLE: bool = false;
//!
//!     fn try_from_storage(storage: U32<E>) -> Result<Self, U32<E>> {
//!         Ok(Pid(storage.value()))
//!     }
//!     fn to_storage(self) -> U32<E> {
//!         U32::new(self.0)
//!     }
//! }
//! ```

use crate::data_type::Endian;

/// A trait for types that use native representation internally but can be
/// converted to/from endian-aware storage types for use in zorua structs.
///
/// This solves the problem of wanting a type like `Pid(u32)` that:
/// 1. Works as a normal Rust type without endianness concerns
/// 2. Can be stored in zorua binary structures with proper endian handling
///
/// # Fallible vs Infallible Conversion
///
/// The trait provides both fallible and infallible conversion:
/// - [`try_from_storage`](Self::try_from_storage) - Returns `Result`, for types that may have invalid storage values
/// - [`from_storage`](Self::from_storage) - Panics on invalid values, for types where all storage values are valid
///
/// For types where all storage values are valid (like `Pid(u32)`), `try_from_storage`
/// should return `Ok(...)` for all inputs.
///
/// The [`IS_FALLIBLE`](Self::IS_FALLIBLE) constant indicates whether `try_from_storage`
/// can return `Err`. This is used by the `bitfields!` macro to require `#[fallible]`
/// annotations for type safety.
///
/// # Implementing for Custom Types
///
/// Implement this trait for your native types that need to be stored in zorua structs.
/// The generic parameter is the storage type (like `U32<E>` for endian-aware u32).
///
/// For types where conversion is always valid:
/// ```ignore
/// impl<E: Endian> ZoruaNative<U32<E>> for Pid {
///     const IS_FALLIBLE: bool = false;
///     fn try_from_storage(storage: U32<E>) -> Result<Self, U32<E>> {
///         Ok(Pid(storage.value()))
///     }
///     fn to_storage(self) -> U32<E> {
///         U32::new(self.0)
///     }
/// }
/// ```
///
/// For types where some values are invalid (like enums):
/// ```ignore
/// impl ZoruaNative<u8> for MyEnum {
///     const IS_FALLIBLE: bool = true;
///     fn try_from_storage(storage: u8) -> Result<Self, u8> {
///         match storage {
///             0 => Ok(MyEnum::A),
///             1 => Ok(MyEnum::B),
///             _ => Err(storage),
///         }
///     }
///     fn to_storage(self) -> u8 {
///         self as u8
///     }
/// }
/// ```
pub trait ZoruaNative<Storage>: Sized + Clone {
    /// Whether `try_from_storage` can return `Err` for some storage values.
    ///
    /// This is `true` for enums where the storage type has more possible values
    /// than enum variants, and `false` when all storage values are valid.
    ///
    /// Used by the `bitfields!` macro to require `#[fallible]` annotation.
    const IS_FALLIBLE: bool;

    /// Try to convert from storage representation to native representation.
    ///
    /// Returns `Err(storage)` if the storage value is not valid for this type.
    fn try_from_storage(storage: Storage) -> Result<Self, Storage>;

    /// Convert native representation to storage representation.
    fn to_storage(self) -> Storage;

    /// Convert from storage representation to native representation.
    ///
    /// # Panics
    /// Panics if the storage value is not valid. Use [`try_from_storage`](Self::try_from_storage)
    /// for fallible conversion.
    fn from_storage(storage: Storage) -> Self
    where
        Storage: Clone + core::fmt::Debug,
    {
        Self::try_from_storage(storage.clone()).unwrap_or_else(|_| {
            panic!(
                "Invalid storage value {:?} for type {}",
                storage,
                core::any::type_name::<Self>()
            )
        })
    }
}

// ============================================================================
// Implementations for primitive types
// ============================================================================

macro_rules! impl_zorua_native_for_primitives {
    ($native:ty, $wrapper:ident) => {
        impl<E: Endian> ZoruaNative<crate::data_type::$wrapper<E>> for $native {
            const IS_FALLIBLE: bool = false;

            #[inline]
            fn try_from_storage(
                storage: crate::data_type::$wrapper<E>,
            ) -> Result<Self, crate::data_type::$wrapper<E>> {
                Ok(storage.value())
            }

            #[inline]
            fn to_storage(self) -> crate::data_type::$wrapper<E> {
                crate::data_type::$wrapper::new(self)
            }
        }
    };
}

impl_zorua_native_for_primitives!(u16, U16);
impl_zorua_native_for_primitives!(u32, U32);
impl_zorua_native_for_primitives!(u64, U64);

macro_rules! impl_zorua_native_for_ux2 {
    ($($ty:ident),* ; $backing:ty) => {
        $(
            impl ZoruaNative<$backing> for crate::data_type::$ty {
                // ux2 types can fail conversion from larger backing type
                const IS_FALLIBLE: bool = true;

                fn try_from_storage(storage: $backing) -> Result<Self, $backing> {
                    const MAX: $backing = (1 << <crate::data_type::$ty>::BITS) - 1;
                    if storage > MAX {
                        return Err(storage);
                    }
                    Ok(crate::data_type::$ty::new(storage as _))
                }
                fn to_storage(self) -> $backing {
                    <$backing>::from(self)
                }
            }
        )*
    };
}

impl_zorua_native_for_ux2!(u1, u2, u3, u4, u5, u6, u7; u8);
impl_zorua_native_for_ux2!(u9, u10, u11, u12, u13, u14, u15; u16);

// ============================================================================
// Identity implementations
// ============================================================================

macro_rules! impl_zorua_native_identity {
    ($($ty:ident),*) => {
        $(
            impl ZoruaNative<crate::data_type::$ty> for crate::data_type::$ty {
                const IS_FALLIBLE: bool = false;

                #[inline]
                fn try_from_storage(storage: Self) -> Result<Self, Self> {
                    Ok(storage)
                }

                #[inline]
                fn to_storage(self) -> Self {
                    self
                }
            }
        )*
    };
}

impl_zorua_native_identity!(u1, u2, u3, u4, u5, u6, u7, u9, u10, u11, u12, u13, u14, u15);

macro_rules! impl_zorua_native_identity_primitive {
    ($($ty:ty),*) => {
        $(
            impl ZoruaNative<$ty> for $ty {
                const IS_FALLIBLE: bool = false;

                #[inline]
                fn try_from_storage(storage: Self) -> Result<Self, Self> {
                    Ok(storage)
                }

                #[inline]
                fn to_storage(self) -> Self {
                    self
                }
            }
        )*
    };
}

// u16, u32, u64 do not implement ZoruaField, so they need explicit identity impls to be used in bitfields.
// Added u8 here as well after removing the blanket implementation.
impl_zorua_native_identity_primitive!(u8, u16, u32, u64);

// (Blanket implementation removed due to conflicts with explicit identity impls)

#[cfg(test)]
mod tests {
    use super::*;
    use crate::data_type::{Big, Little, U32};

    #[derive(Clone, Copy, Debug, PartialEq, Eq)]
    struct Pid(u32);

    impl<E: Endian> ZoruaNative<U32<E>> for Pid {
        const IS_FALLIBLE: bool = false;

        fn try_from_storage(storage: U32<E>) -> Result<Self, U32<E>> {
            Ok(Pid(storage.value()))
        }
        fn to_storage(self) -> U32<E> {
            U32::new(self.0)
        }
    }

    #[test]
    fn test_pid_little_endian_roundtrip() {
        let pid = Pid(0x12345678);
        let storage: U32<Little> = pid.to_storage();
        let recovered: Pid = ZoruaNative::from_storage(storage);
        assert_eq!(pid, recovered);
    }

    #[test]
    fn test_pid_big_endian_roundtrip() {
        let pid = Pid(0x12345678);
        let storage: U32<Big> = pid.to_storage();
        let recovered: Pid = ZoruaNative::from_storage(storage);
        assert_eq!(pid, recovered);
    }

    #[test]
    fn test_primitive_native_roundtrip() {
        let val: u32 = 0xDEADBEEF;
        let storage: U32<Little> = val.to_storage();
        let recovered: u32 = ZoruaNative::from_storage(storage);
        assert_eq!(val, recovered);
    }

    // Test for the bitfields! macro native field syntax
    mod bitfield_native_syntax {
        use super::*;
        use crate::prelude::*;
        use zorua_macro::bitfields;

        // Define a simple native type
        #[derive(Clone, Copy, Debug, PartialEq, Eq)]
        struct MyPid(u32);

        impl MyPid {
            fn new(val: u32) -> Self {
                MyPid(val)
            }
            fn value(&self) -> u32 {
                self.0
            }
        }

        impl<E: Endian> ZoruaNative<U32<E>> for MyPid {
            const IS_FALLIBLE: bool = false;

            fn try_from_storage(storage: U32<E>) -> Result<Self, U32<E>> {
                Ok(MyPid(storage.value()))
            }
            fn to_storage(self) -> U32<E> {
                U32::new(self.0)
            }
        }

        // Test struct with backed field syntax (uses `as`)
        bitfields! {
            #[repr(C)]
            #[derive(ZoruaField, Clone, Debug, PartialEq)]
            struct TestNativeStruct {
                pub pid: MyPid as u32_le,
                pub data: u32_le,
            }
        }

        #[test]
        fn test_native_backed_field_getters() {
            let test = TestNativeStruct {
                pid_raw: u32_le::new(0x12345678),
                data: u32_le::new(0xCAFEBABE),
            };

            // Native getters should work
            assert_eq!(test.pid().value(), 0x12345678);
        }

        #[test]
        fn test_native_backed_field_setters() {
            let mut test = TestNativeStruct {
                pid_raw: u32_le::new(0),
                data: u32_le::new(0),
            };

            // Setters should work
            test.set_pid(MyPid::new(0xDEADBEEF));

            assert_eq!(test.pid_raw.value(), 0xDEADBEEF);
        }

        #[test]
        fn test_struct_layout() {
            // Verify the struct has expected layout
            let test = TestNativeStruct {
                pid_raw: u32_le::new(0xCAFEBABE),
                data: u32_le::new(0x12345678),
            };

            // Direct field access (no _raw suffix for regular fields)
            assert_eq!(test.data.value(), 0x12345678);
        }

        bitfields! {
            #[repr(C)]
            #[derive(Clone, Debug, PartialEq, Default)]
            struct TestArrayBitfield {
                flags: u16_le {
                    pub nibbles: [u4; 4]@0,
                },
            }
        }

        #[test]
        fn test_array_bitfield_roundtrip() {
            let mut test = TestArrayBitfield::default();
            let vals = [u4::new(0xA), u4::new(0xB), u4::new(0xC), u4::new(0xD)];

            // Set each element using indexed setter
            for (i, &val) in vals.iter().enumerate() {
                test.set_nibbles(i, val);
            }

            // Read back each element using indexed getter
            for (i, &expected) in vals.iter().enumerate() {
                assert_eq!(test.nibbles(i), expected);
            }

            assert_eq!(test.flags.value(), 0xDCBA);
        }
    }
}
