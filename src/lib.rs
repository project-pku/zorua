#![feature(generic_const_exprs)]
#![allow(incomplete_features)]

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
        (impl "subfield_impl", $f:ident, $sfv:vis, $sf:ident, $sfi:literal, [$sft:tt;$sfl:literal],) => {
            paste! {
                $sfv fn $sf(&self, index: usize) -> $sft {
                    let bit_repr = self.$f.get_bits_at::<<$sft as ZoruaBitField>::BitRepr>($sfi+<$sft as ZoruaBitField>::BitRepr::BITS as usize*index);
                    <$sft as ZoruaBitField>::from_bit_repr(bit_repr)
                }
                $sfv fn [<set_ $sf>](&mut self, val: $sft, index: usize) {
                    let bit_repr = val.to_bit_repr();
                    self.$f.set_bits_at::<<$sft as ZoruaBitField>::BitRepr>(bit_repr, $sfi+<$sft as ZoruaBitField>::BitRepr::BITS as usize*index);
                }
            }
        };
        //all other bitfields
        (impl "subfield_impl", $f:ident, $sfv:vis, $sf:ident, $sfi:literal, $sft:tt, $($sftg1:tt, $sftg2:tt)?) => {
            paste! {
                $sfv fn $sf(&self) -> $sft$(<$sftg1, $sftg2>)? {
                    let bit_repr = self.$f.get_bits_at::<<$sft$(<$sftg1, $sftg2>)? as ZoruaBitField>::BitRepr>($sfi);
                    <$sft$(<$sftg1, $sftg2>)? as ZoruaBitField>::from_bit_repr(bit_repr)
                }
                $sfv fn [<set_ $sf>](&mut self, val: $sft$(<$sftg1, $sftg2>)?) {
                    let bit_repr = val.to_bit_repr();
                    self.$f.set_bits_at::<<$sft$(<$sftg1, $sftg2>)? as ZoruaBitField>::BitRepr>(bit_repr, $sfi);
                }
            }
        };

        //Regular struct macro
        //Generic support courtesy of: https://stackoverflow.com/a/61189128/10910105
        (
            $(#[$struct_meta:meta])*
            $sv:vis struct $s:ident$(<$($g:tt$(:$gt:tt$(+$gtx:tt)*)?),+>)? {
                $($fv:vis $f:ident : $ft:ty,
                    $($(|$sfv:vis $sf:ident : $sft:tt$(<$sftg1:tt, $sftg2:tt>)?@$sfi:literal,)+)?
                )*
            }
        ) => {
                // Define the struct
                $(#[$struct_meta])*
                $sv struct $s$(<$($g $(:$gt$(+$gtx)*)?),+>)? {
                    $($fv $f: $ft),*
                }
                // Generate the impl block
                impl$(<$($g$(:$gt$(+$gtx)*)?),+>)? $s$(<$($g),+>)? {
                    $($($(
                        bitfields!(impl "subfield_impl", $f, $sfv, $sf, $sfi, $sft, $($sftg1, $sftg2)?);
                    )+)?)*
                }
        };
    }
    pub use bitfields;

    #[macro_export]
    macro_rules! zorua_enum {
        //Zorua Enum field common impl
        (impl "common", $($struct_meta:meta)*, $ev:vis, $e:ident, $byterepr:ty, $($v:ident $(=$vv:literal)?),*) => {
            $(#[$struct_meta])*
            #[derive(Debug, Clone, Copy, PartialEq)]
            #[repr($byterepr)]
            $ev enum $e {
                $($v $(=$vv)?),*
            }
        };
        (impl "fallible", $e:ident, $repr:ty, $($v:ident),*) => {
            unsafe impl ZoruaFallible<$repr> for $e {
                fn is_valid(value: $repr) -> bool {
                    $(value == unsafe {std::mem::transmute($e::$v)})||*
                }
            }
            impl TryInto<$e> for Fallible<$e, $repr> {
                type Error = $repr;
                fn try_into(self) -> Result<$e, $repr> {
                    if $e::is_valid(self.value) {
                        Ok(unsafe { std::mem::transmute_copy(&self) })
                    } else {
                        Err(self.value)
                    }
                }
            }
        };
        (impl "bitfield", $e:ident, $bitrepr:ty) => {
            impl ZoruaBitField for $e {
                type BitRepr = $bitrepr;
                fn to_bit_repr(self) -> Self::BitRepr {
                    Self::BitRepr::from_backed(self as <Self::BitRepr as BackingBitField>::ByteRepr)
                }
                fn from_bit_repr(value: Self::BitRepr) -> Self {
                    unsafe { std::mem::transmute(value.to_backed() as <Self::BitRepr as BackingBitField>::ByteRepr) }
                }
            }
        };

        // c-like byte exhaustive enum
        {
            $(#[$struct_meta:meta])*
            $ev:vis enum $e:ident: =$byterepr:ty {
                $($v:ident $(=$vv:literal)?),*$(,)?
            }
        } => {
            zorua_enum!(impl "common", $($struct_meta)*, $ev, $e, $byterepr, $($v $(=$vv)?),*);
            zorua_enum!(impl "bitfield", $e, $byterepr);
            impl ZoruaField for $e {
                type Alignment = A1; //assumption, no way they fill up a u16+
                fn swap_bytes_mut(&mut self) {
                    //must be safe because enum is exhaustive over repr
                    <$byterepr as ZoruaField>::swap_bytes_mut(unsafe {std::mem::transmute(self)});
                }
            }
        };

        // c-like bit exhaustive enum
        {
            $(#[$struct_meta:meta])*
            $ev:vis enum $e:ident: =$bitrepr:ty:$byterepr:ty {
                $($v:ident $(= $vv:literal)?),*$(,)?
            }
        } => {
            zorua_enum!(impl "common", $($struct_meta)*, $ev, $e, $byterepr, $($v $(=$vv)?),*);
            zorua_enum!(impl "fallible", $e, $bitrepr, $($v),*);
            zorua_enum!(impl "bitfield", $e, $bitrepr);
        };

        // c-like byte non-exhaustive enum
        {
            $(#[$struct_meta:meta])*
            $ev:vis enum $e:ident: $byterepr:ty {
                $($v:ident $(= $vv:literal)?),*$(,)?
            }
        } => {
            zorua_enum!(impl "common", $($struct_meta)*, $ev, $e, $byterepr, $($v $(=$vv)?),*);
            zorua_enum!(impl "fallible", $e, $byterepr, $($v),*);
        };

        // c-like bit non-exhaustive enum
        {
            $(#[$struct_meta:meta])*
            $ev:vis enum $e:ident: $bitrepr:ty:$byterepr:ty {
                $($v:ident $(= $vv:literal)?),*$(,)?
            }
        } => {
            zorua_enum!(impl "common", $($struct_meta)*, $ev, $e, $byterepr, $($v $(=$vv)?),*);
            zorua_enum!(impl "fallible", $e, $bitrepr, $($v),*);
        };
    }
    pub use zorua_enum;
}
