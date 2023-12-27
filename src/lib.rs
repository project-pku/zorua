pub mod data_type {
    use crate::prelude::{BackingBitField, BackingField, ZoruaBitField, ZoruaField};
    pub use arbitrary_int::*;
    use std::fmt::{Debug, Formatter};

    pub trait ZoruaFallible: Copy + Debug {
        type ByteRepr: BackingField;
        type BitRepr: BackingBitField;

        fn is_valid(value: Self::ByteRepr) -> bool;
    }

    #[derive(Clone, Copy)]
    pub union Fallible<T: ZoruaFallible> {
        pub as_enum: T,
        pub as_byte_repr: T::ByteRepr,
        pub as_bit_repr: T::BitRepr,
    }
    impl<T: ZoruaFallible> Fallible<T> {
        pub fn value_or_byte_repr(self) -> Result<T, T::ByteRepr> {
            if T::is_valid(unsafe { self.as_byte_repr }) {
                Ok(unsafe { self.as_enum })
            } else {
                Err(unsafe { self.as_byte_repr })
            }
        }
        pub fn value_or_bit_repr(self) -> Result<T, T::BitRepr> {
            if T::is_valid(unsafe { self.as_byte_repr }) {
                Ok(unsafe { self.as_enum })
            } else {
                Err(unsafe { self.as_bit_repr })
            }
        }
        pub fn as_byte_repr(self) -> T::ByteRepr {
            unsafe { self.as_byte_repr }
        }
        pub fn as_bit_repr(self) -> T::BitRepr {
            unsafe { self.as_bit_repr }
        }
    }
    impl<T: ZoruaFallible> From<T> for Fallible<T> {
        fn from(value: T) -> Self {
            Fallible { as_enum: value }
        }
    }
    impl<T: ZoruaFallible + PartialEq> PartialEq for Fallible<T> {
        fn eq(&self, other: &Self) -> bool {
            self.as_byte_repr() == other.as_byte_repr()
        }
    }
    impl<T: ZoruaFallible + Debug> Debug for Fallible<T> {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            self.value_or_byte_repr().fmt(f)
        }
    }
    impl<T: ZoruaFallible> ZoruaField for Fallible<T> {
        fn swap_bytes_mut(&mut self) {
            T::ByteRepr::swap_bytes_mut(unsafe { &mut self.as_byte_repr })
        }
    }
    impl<T: ZoruaFallible> ZoruaBitField for Fallible<T> {
        type BitRepr = T::BitRepr;
        fn to_repr(self) -> Self::BitRepr {
            self.as_bit_repr()
        }
        fn from_repr(value: Self::BitRepr) -> Self {
            Fallible { as_bit_repr: value }
        }
    }
}

/// Automates boilerplate for implementing ZoruaField
/// and BackingField on built-in int types
macro_rules! impl_backing {
    ($($ty:ty),*) => {
        $(
            impl BackingField for $ty {
                fn get_bits<T: BackingBitField, const INDEX: u32>(self) -> T
                where Self: From<T::ByteRepr> + TryInto<T::ByteRepr> {
                    T::from_backed(
                        ((self & (Into::<$ty>::into(T::MASK) << INDEX)) >> INDEX).try_into().unwrap_or_else(
                            |_| {
                                panic!("Zorua Error: The BitRepr::MASK of type {} must be wrong",
                                    std::any::type_name::<$ty>())
                            }
                        )
                    )
                }
                fn set_bits<T: BackingBitField, const INDEX: u32>(&mut self, value: T)
                where Self: From<T::ByteRepr> + TryInto<T::ByteRepr> {
                    *self &= !(Into::<$ty>::into(T::MASK) << INDEX);
                    *self |= (Into::<$ty>::into(value.to_backed())) << INDEX;
                }
            }
            impl ZoruaField for $ty {
                fn swap_bytes_mut(&mut self) {
                    *self = (*self).swap_bytes();
                }
            }
        )*
    };
}

/// Automates boilerplate for implementing ZoruaBitField
/// and BackingBitField on arbitrary-int & built-in int types
macro_rules! impl_bit_backing {
    ("arbitrary", $($ty:ty),*) => {
        $(
            impl ZoruaBitField for $ty {
                type BitRepr = Self;
                fn to_repr(self) -> Self::BitRepr { self }
                fn from_repr(value: Self::BitRepr) -> Self { value }
            }
            impl BackingBitField for $ty {
                type ByteRepr = <$ty as Number>::UnderlyingType;
                const MASK: Self::ByteRepr = <$ty>::MASK;
                fn to_backed(self) -> Self::ByteRepr {
                    self.value()
                }
                fn from_backed(value: Self::ByteRepr) -> Self {
                    Self::new(value)
                }
            }
        )*
    };
    ("native", $($ty:ty),*) => {
        $(
            impl ZoruaBitField for $ty {
                type BitRepr = Self;
                fn to_repr(self) -> Self::BitRepr { self }
                fn from_repr(value: Self::BitRepr) -> Self { value }
            }
            impl BackingBitField for $ty {
                type ByteRepr = Self;
                const MASK: Self::ByteRepr = Self::MAX;
                fn to_backed(self) -> Self::ByteRepr { self }
                fn from_backed(value: Self::ByteRepr) -> Self { value }
            }
        )*
    };
}

#[macro_use]
pub mod prelude {
    pub use crate::data_type::*;
    pub use paste::paste;

    //------------- Field trait + impls -------------
    /// A type that can be used within a ZoruaStruct.
    pub trait ZoruaField {
        fn swap_bytes_mut(&mut self);

        fn to_le_mut(&mut self) {
            //Assumption: All targets are either little or big endian.
            #[cfg(target_endian = "big")]
            {
                self.swap_bytes_mut();
            }
        }

        fn to_be_mut(&mut self) {
            //Assumption: All targets are either little or big endian.
            #[cfg(target_endian = "little")]
            {
                self.swap_bytes_mut();
            }
        }
    }

    /// A special kind of [ZoruaField] that can house [ZoruaBitField]s.
    pub trait BackingField: ZoruaField + Copy + std::fmt::Debug + PartialEq {
        fn get_bits<T: BackingBitField, const INDEX: u32>(self) -> T
        where
            Self: From<T::ByteRepr> + TryInto<T::ByteRepr>;

        fn set_bits<T: BackingBitField, const INDEX: u32>(&mut self, value: T)
        where
            Self: From<T::ByteRepr> + TryInto<T::ByteRepr>;
    }
    impl_backing!(u8, u16, u32, u64, u128);
    impl ZoruaField for bool {
        fn swap_bytes_mut(&mut self) {}
    }
    impl ZoruaField for () {
        fn swap_bytes_mut(&mut self) {}
    }
    impl<const N: usize, T: ZoruaField> ZoruaField for [T; N] {
        fn swap_bytes_mut(&mut self) {
            self.iter_mut().for_each(|value| {
                value.swap_bytes_mut();
            });
        }
    }
    //-----------------------------------------------

    //----------- BitField trait + impls -----------
    pub trait ZoruaBitField {
        /// The smallest bit representation of this type.
        type BitRepr: BackingBitField;

        /// Returns a copy of this type as its N bit [BitRepr][ZoruaBitField].
        fn to_repr(self) -> Self::BitRepr;

        /// Packs this type into its N bit [BitRepr][ZoruaBitField].
        fn from_repr(value: Self::BitRepr) -> Self;
    }
    pub trait BackingBitField: ZoruaBitField + Copy {
        type ByteRepr: BackingField;
        const MASK: Self::ByteRepr;
        fn to_backed(self) -> Self::ByteRepr;
        fn from_backed(value: Self::ByteRepr) -> Self;
    }
    impl_bit_backing!("arbitrary", u1, u2, u3, u4, u5, u6, u7);
    impl_bit_backing!("native", u8, u16, u32, u64, u128);
    impl ZoruaBitField for bool {
        type BitRepr = u1;
        fn to_repr(self) -> Self::BitRepr {
            u1::new(if self { 1 } else { 0 })
        }
        fn from_repr(value: Self::BitRepr) -> Self {
            value == u1::MAX
        }
    }
    //---------------------------------------------

    #[macro_export]
    macro_rules! zorua {
        (impl "subfield_impl", $f:ident, $sfv:vis, $sf:ident, $sfi:literal, $sft:tt, $($sftg:tt)?) => {
            paste! {
                $sfv fn $sf(&self) -> $sft$(<$sftg>)? {
                    let bit_repr = self.$f.get_bits::<<$sft$(<$sftg>)? as ZoruaBitField>::BitRepr, $sfi>();
                    <$sft$(<$sftg>)? as ZoruaBitField>::from_repr(bit_repr)
                }
                $sfv fn [<set_ $sf>](&mut self, val: $sft$(<$sftg>)?) {
                    let bit_repr = val.to_repr();
                    self.$f.set_bits::<<$sft$(<$sftg>)? as ZoruaBitField>::BitRepr, $sfi>(bit_repr);
                }
            }
        };

        //Regular struct macro
        //Generic support courtesy of: https://stackoverflow.com/a/61189128/10910105
        (
            $(#[$struct_meta:meta])*
            $sv:vis struct $s:ident$(<$($g:tt$(:$gt:tt$(+$gtx:tt)*)?),+>)? {
                $($fv:vis $f:ident : $ft:ty,
                    $($(|$sfv:vis $sf:ident : $sft:tt$(<$sftg:tt>)?@$sfi:literal,)+)?
                )*
            };
        ) => {
                // Define the struct
                $(#[$struct_meta])*
                #[derive(Debug, PartialEq, Clone)]
                $sv struct $s$(<$($g $(:$gt$(+$gtx)*)?),+>)? {
                    $($fv $f: $ft),*
                }
                // Generate the impl block
                impl$(<$($g$(:$gt$(+$gtx)*)?),+>)? $s$(<$($g),+>)? {
                    $($($(
                        zorua!(impl "subfield_impl", $f, $sfv, $sf, $sfi, $sft, $($sftg)?);
                    )+)?)*
                }
                impl$(<$($g$(:$gt$(+$gtx)*)?),+>)? ZoruaField for $s$(<$($g),+>)? {
                    fn swap_bytes_mut(&mut self) {
                        $(self.$f.swap_bytes_mut();)*
                    }
                }
        };

        // single tuple struct w/ optional non-const generics
        {
            $(#[$struct_meta:meta])*
            $sv:vis struct $s:ident$(<$($g:tt$(:$gt:tt$(+$gtx:tt)*)?),+>)? (
                $fv:vis $ft:ty
            );
        }=> {
            $(#[$struct_meta])*
            #[derive(Debug, PartialEq, Clone)]
            $sv struct $s$(<$($g:tt$(:$gt:tt$(+$gtx:tt)*)?),+>)? ($fv $ft);

            impl$(<$($g$(:$gt$(+$gtx)*)?),+>)? ZoruaField for $s$(<$($g),+>)? {
                #[inline]
                fn swap_bytes_mut(&mut self) {
                    self.0.swap_bytes_mut();
                }
            }
        };

        // single tuple struct w/ single const generic
        {
            $(#[$struct_meta:meta])*
            $sv:vis struct $s:ident<const $N:ident : $Nt:ty> (
                $fv:vis $ft:ty
            );
        }=> {
            $(#[$struct_meta])*
            #[derive(Debug, PartialEq, Clone)]
            $sv struct $s<const $N : $Nt> ($fv $ft);

            impl<const $N: $Nt> ZoruaField for $s<$N> {
                #[inline]
                fn swap_bytes_mut(&mut self) {
                    self.0.swap_bytes_mut();
                }
            }
        };

        // c-like byte exhaustive enum
        {
            =$byterepr:ty,
            $(#[$struct_meta:meta])*
            $ev:vis enum $e:ident {
                $($v:ident $(=$vv:literal)?),*$(,)?
            };
        } => {
            $(#[$struct_meta])*
            #[derive(Debug, Clone, Copy, PartialEq)]
            #[repr($byterepr)]
            $ev enum $e {
                $($v $(=$vv)?),*
            }
            impl ZoruaField for $e {
                fn swap_bytes_mut(&mut self) {
                    //must be safe because enum is exhaustive over repr
                    <$byterepr as ZoruaField>::swap_bytes_mut(unsafe {std::mem::transmute(self)});
                }
            }
            impl ZoruaBitField for $e {
                type BitRepr = $byterepr;
                fn to_repr(self) -> Self::BitRepr {
                    Self::BitRepr::from_backed(self as <Self::BitRepr as BackingBitField>::ByteRepr)
                }
                fn from_repr(value: Self::BitRepr) -> Self {
                    unsafe { std::mem::transmute(value.to_backed() as <Self::BitRepr as BackingBitField>::ByteRepr) }
                }
            }
        };

        // c-like bit exhaustive enum
        {
            =$bitrepr:ty, $byterepr:ty,
            $(#[$struct_meta:meta])*
            $ev:vis enum $e:ident {
                $($v:ident $(= $vv:literal)?),*$(,)?
            };
        } => {
            $(#[$struct_meta])*
            #[repr($byterepr)]
            #[derive(Debug, Clone, Copy, PartialEq)]
            $ev enum $e {
                $($v $(=$vv)?),*
            }
            impl ZoruaFallible for $e {
                type BitRepr = $bitrepr;
                type ByteRepr = $byterepr;

                fn is_valid(value: Self::ByteRepr) -> bool {
                    $(value == unsafe {std::mem::transmute($e::$v)})||*
                }
            }
            impl ZoruaBitField for $e {
                type BitRepr = $bitrepr;
                fn to_repr(self) -> Self::BitRepr {
                    Self::BitRepr::from_backed(self as <Self::BitRepr as BackingBitField>::ByteRepr)
                }
                fn from_repr(value: Self::BitRepr) -> Self {
                    unsafe { std::mem::transmute(value.to_backed() as <Self::BitRepr as BackingBitField>::ByteRepr) }
                }
            }
            impl TryInto<$e> for Fallible<$e> {
                type Error = $byterepr;
                fn try_into(self) -> Result<$e, $byterepr> {
                    self.value_or_byte_repr()
                }
            }
        };

        // c-like non-exhaustive enum
        {
            $bitrepr:ty, $byterepr:ty,
            $(#[$struct_meta:meta])*
            $ev:vis enum $e:ident {
                $($v:ident $(= $vv:literal)?),*$(,)?
            };
        } => {
            $(#[$struct_meta])*
            #[repr($byterepr)]
            #[derive(Debug, Clone, Copy, PartialEq)]
            $ev enum $e {
                $($v $(=$vv)?),*
            }
            unsafe impl ZoruaFallible for $e {
                type BitRepr = $bitrepr;
                type ByteRepr = $byterepr;

                fn is_valid(value: Self::ByteRepr) -> bool {
                    $(value == unsafe {std::mem::transmute($e::$v)})||*
                }
            }
            impl TryInto<$e> for Fallible<$e> {
                type Error = $byterepr;
                fn try_into(self) -> Result<$e, $byterepr> {
                    self.value_or_byte_repr()
                }
            }
        };
    }
    pub use zorua;
}
