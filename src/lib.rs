pub mod data_type {
    pub use arbitrary_int::*;
}

/// Automates boilerplate for implementing ZoruaField
/// and BackingField on built-in int types
macro_rules! impl_backing {
    ($($ty:ty),*) => {
        $(
            impl BackingField for $ty {
                fn get_bits<T: BackingBitField, const INDEX: u32>(self) -> T
                where Self: From<T::UnderlyingType> + TryInto<T::UnderlyingType> {
                    T::new(
                        ((self & (Into::<$ty>::into(T::MASK) << INDEX)) >> INDEX).try_into().unwrap_or_else(
                            |_| {
                                panic!("Zorua Error: The BackedBitField::MASK of type {} must be wrong",
                                    std::any::type_name::<$ty>())
                            }
                        )
                    )
                }
                fn set_bits<T: BackingBitField, const INDEX: u32>(&mut self, value: T)
                where Self: From<T::UnderlyingType> + TryInto<T::UnderlyingType> {
                    *self &= !(Into::<$ty>::into(T::MASK) << INDEX);
                    *self |= (Into::<$ty>::into(value.get_backed())) << INDEX;
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
                const MASK: Self::UnderlyingType = <$ty>::MASK;
                fn get_backed(self) -> Self::UnderlyingType {
                    self.value()
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
                const MASK: Self::UnderlyingType = <$ty>::MAX;
                fn get_backed(self) -> Self::UnderlyingType { self }
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
    pub trait BackingField: ZoruaField {
        fn get_bits<T: BackingBitField, const INDEX: u32>(self) -> T
        where
            Self: From<T::UnderlyingType> + TryInto<T::UnderlyingType>;

        fn set_bits<T: BackingBitField, const INDEX: u32>(&mut self, value: T)
        where
            Self: From<T::UnderlyingType> + TryInto<T::UnderlyingType>;
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
    pub trait BackingBitField: ZoruaBitField + Number {
        const MASK: Self::UnderlyingType;
        fn get_backed(self) -> Self::UnderlyingType;
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
        //Regular struct macro
        //Generic support courtesy of: https://stackoverflow.com/a/61189128/10910105
        (
            $(#[$struct_meta:meta])*
            $sv:vis struct $s:ident$(<$($g:tt$(:$gt:tt$(+$gtx:tt)*)?),+>)? {
                $($fv:vis $f:ident : $ft:ty,
                    $($(|$sfv:vis $sf:ident : $sft:tt@$sfi:literal,)+)?
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
                        paste! {
                            $sfv fn $sf(&self) -> $sft {
                                let bit_repr = self.$f.get_bits::<<$sft as ZoruaBitField>::BitRepr, $sfi>();
                                <$sft as ZoruaBitField>::from_repr(bit_repr)
                            }
                            $sfv fn [<set_ $sf>](&mut self, val: $sft) {
                                let bit_repr = val.to_repr();
                                self.$f.set_bits::<<$sft as ZoruaBitField>::BitRepr, $sfi>(bit_repr);
                            }
                        }
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
    }
    pub use zorua;
}
