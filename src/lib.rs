#![cfg_attr(not(feature = "std"), no_std)]

pub mod data_type;
pub mod traits;

#[macro_use]
pub mod prelude {
    pub use crate::data_type::*;
    pub use crate::traits::*;
    pub use paste::paste;

    #[macro_export]
    macro_rules! bitfields {
        //array bitfields
        (impl "subfield_impl", $f:ident, [$(#[$sfm:meta])*], $sfv:vis, $sf:ident, $sfi:literal, [$sft:tt;$sfl:literal],) => {
            paste! {
                $(#[$sfm])*
                $sfv fn $sf(&self, index: usize) -> $sft {
                    let bit_repr = self.$f.get_bits_at::<<$sft as ZoruaBitField>::BitRepr>($sfi+<$sft as ZoruaBitField>::BitRepr::BITS as usize*index);
                    <$sft as ZoruaBitField>::from_bit_repr(bit_repr)
                }
                $(#[$sfm])*
                $sfv fn [<set_ $sf>](&mut self, val: $sft, index: usize) {
                    let bit_repr = val.to_bit_repr();
                    self.$f.set_bits_at::<<$sft as ZoruaBitField>::BitRepr>(bit_repr, $sfi+<$sft as ZoruaBitField>::BitRepr::BITS as usize*index);
                }
            }
        };
        //single-generic bitfields (e.g., SpeciesId<u9>)
        (impl "subfield_impl", $f:ident, [$(#[$sfm:meta])*], $sfv:vis, $sf:ident, $sfi:literal, $sft:tt, $sftg:tt,) => {
            paste! {
                $(#[$sfm])*
                $sfv fn $sf(&self) -> $sft<$sftg> {
                    let bit_repr = self.$f.get_bits_at::<<$sft<$sftg> as ZoruaBitField>::BitRepr>($sfi);
                    <$sft<$sftg> as ZoruaBitField>::from_bit_repr(bit_repr)
                }
                $(#[$sfm])*
                $sfv fn [<set_ $sf>](&mut self, val: $sft<$sftg>) {
                    let bit_repr = val.to_bit_repr();
                    self.$f.set_bits_at::<<$sft<$sftg> as ZoruaBitField>::BitRepr>(bit_repr, $sfi);
                }
            }
        };
        //two-generic bitfields (e.g., Fallible<Ball, u4>)
        (impl "subfield_impl", $f:ident, [$(#[$sfm:meta])*], $sfv:vis, $sf:ident, $sfi:literal, $sft:tt, $sftg1:tt, $sftg2:tt,) => {
            paste! {
                $(#[$sfm])*
                $sfv fn $sf(&self) -> $sft<$sftg1, $sftg2> {
                    let bit_repr = self.$f.get_bits_at::<<$sft<$sftg1, $sftg2> as ZoruaBitField>::BitRepr>($sfi);
                    <$sft<$sftg1, $sftg2> as ZoruaBitField>::from_bit_repr(bit_repr)
                }
                $(#[$sfm])*
                $sfv fn [<set_ $sf>](&mut self, val: $sft<$sftg1, $sftg2>) {
                    let bit_repr = val.to_bit_repr();
                    self.$f.set_bits_at::<<$sft<$sftg1, $sftg2> as ZoruaBitField>::BitRepr>(bit_repr, $sfi);
                }
            }
        };
        //non-generic bitfields (e.g., u9, bool)
        (impl "subfield_impl", $f:ident, [$(#[$sfm:meta])*], $sfv:vis, $sf:ident, $sfi:literal, $sft:tt,) => {
            paste! {
                $(#[$sfm])*
                $sfv fn $sf(&self) -> $sft {
                    let bit_repr = self.$f.get_bits_at::<<$sft as ZoruaBitField>::BitRepr>($sfi);
                    <$sft as ZoruaBitField>::from_bit_repr(bit_repr)
                }
                $(#[$sfm])*
                $sfv fn [<set_ $sf>](&mut self, val: $sft) {
                    let bit_repr = val.to_bit_repr();
                    self.$f.set_bits_at::<<$sft as ZoruaBitField>::BitRepr>(bit_repr, $sfi);
                }
            }
        };

        //Regular struct macro (supports doc comments on fields and bitfields)
        //Generic support courtesy of: https://stackoverflow.com/a/61189128/10910105
        (
            $(#[$struct_meta:meta])*
            $sv:vis struct $s:ident$(<$($g:tt$(:$gt:tt$(+$gtx:tt)*)?),+>)? {
                $(
                    $(#[$field_meta:meta])*
                    $fv:vis $f:ident : $ft:ty
                    $({
                        $(
                            $(#[$subfield_meta:meta])*
                            $sfv:vis $sf:ident : $sft:tt$(<$sftg1:tt $(, $sftg2:tt)?>)?@$sfi:literal,
                        )+
                    })?,
                )*
            }
        ) => {
                // Define the struct
                $(#[$struct_meta])*
                $sv struct $s$(<$($g $(:$gt$(+$gtx)*)?),+>)? {
                    $(
                        $(#[$field_meta])*
                        $fv $f: $ft
                    ),*
                }
                // Generate the impl block
                impl$(<$($g$(:$gt$(+$gtx)*)?),+>)? $s$(<$($g),+>)? {
                    $($($(
                        bitfields!(impl "subfield_impl", $f, [$(#[$subfield_meta])*], $sfv, $sf, $sfi, $sft, $($sftg1, $($sftg2,)?)?);
                    )+)?)*
                }
        };
    }
    pub use bitfields;

    /// Shorthand for the [`AlignedBytes`] type with the same size and
    /// alignment as the input type. For example:
    ///
    /// ```
    /// use std::any::TypeId;
    /// use zorua::prelude::*;
    ///
    /// //ALIGN = 2, SIZE = 4
    /// struct MyStruct {
    ///     a: u16,
    ///     b: u16,
    /// }
    /// assert_eq!(
    ///     TypeId::of::<AlignedBytes<2, 4>>(),
    ///     TypeId::of::<aligned_bytes_of!(MyStruct)>()
    /// ); //passes
    /// ```
    #[macro_export]
    macro_rules! aligned_bytes_of {
        ($ty:ty) => {
            AlignedBytes<
                { core::mem::align_of::<$ty>() },
                { core::mem::size_of::<$ty>() }
            >
        }
    }
    pub use aligned_bytes_of;
}
