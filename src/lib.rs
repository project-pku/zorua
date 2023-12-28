pub mod data_type;
pub mod traits;

#[macro_use]
pub mod prelude {
    pub use crate::data_type::*;
    pub use crate::traits::*;
    pub use paste::paste;

    #[macro_export]
    macro_rules! zorua {
        //array bitfields
        (impl "subfield_impl", $f:ident, $sfv:vis, $sf:ident, $sfi:literal, [$sft:tt;$sfl:literal],) => {
            paste! {
                $sfv fn $sf(&self, index: usize) -> $sft {
                    let bit_repr = self.$f.get_bits::<<$sft as ZoruaBitField>::BitRepr>($sfi+<$sft as ZoruaBitField>::BitRepr::BITS as usize*index);
                    <$sft as ZoruaBitField>::from_repr(bit_repr)
                }
                $sfv fn [<set_ $sf>](&mut self, val: $sft, index: usize) {
                    let bit_repr = val.to_repr();
                    self.$f.set_bits::<<$sft as ZoruaBitField>::BitRepr>(bit_repr, $sfi+<$sft as ZoruaBitField>::BitRepr::BITS as usize*index);
                }
            }
        };
        //all other bitfields
        (impl "subfield_impl", $f:ident, $sfv:vis, $sf:ident, $sfi:literal, $sft:tt, $($sftg:tt)?) => {
            paste! {
                $sfv fn $sf(&self) -> $sft$(<$sftg>)? {
                    let bit_repr = self.$f.get_bits::<<$sft$(<$sftg>)? as ZoruaBitField>::BitRepr>($sfi);
                    <$sft$(<$sftg>)? as ZoruaBitField>::from_repr(bit_repr)
                }
                $sfv fn [<set_ $sf>](&mut self, val: $sft$(<$sftg>)?) {
                    let bit_repr = val.to_repr();
                    self.$f.set_bits::<<$sft$(<$sftg>)? as ZoruaBitField>::BitRepr>(bit_repr, $sfi);
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
            impl ZoruaFallible for $e {
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
