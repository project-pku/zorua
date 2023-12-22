pub mod data_type {
    pub use arbitrary_int::*;
}

//Automates boilerplate for implementing Zorua on int types
macro_rules! impl_zorua_int {
    ($($ty:ty),*) => {
        $(
            impl Zorua for $ty {
                #[inline]
                fn swap_bytes_mut(&mut self) {
                    *self = (*self).swap_bytes();
                }
            }
        )*
    };
}

#[macro_use]
pub mod prelude {
    pub use crate::data_type::*;
    pub use paste::paste;

    pub trait Zorua {
        fn swap_bytes_mut(&mut self);

        fn to_le_mut(&mut self) {
            //Assumption: All targets are either little or big endian.
            #[cfg(target_endian = "big")] {
                self.swap_bytes_mut();
            }
        }

        fn to_be_mut(&mut self) {
            //Assumption: All targets are either little or big endian.
            #[cfg(target_endian = "little")] {
                self.swap_bytes_mut();
            }
        }
    }
    impl Zorua for () {
        fn swap_bytes_mut(&mut self) {}
    }
    impl_zorua_int!(u8, u16, u32, u64, u128, i8, i16, i32, i64, i128);
    impl<const N: usize, T: Zorua> Zorua for [T; N] {
        fn swap_bytes_mut(&mut self) {
            self.iter_mut().for_each(|value| {
                value.swap_bytes_mut();
            });
        }
    }

    #[macro_export]
    macro_rules! zorua {
        //Subfield get/set implementations
        (impl "$sf_impl", $f:ident, $ft:ty, $sfv:vis, $sf:ident, $sfs:expr, bool) => {
            paste! {
                $sfv fn $sf(&self) -> bool {
                    let bitmask = 1 << $sfs;
                    (self.$f & bitmask) != 0
                }
                $sfv fn [<set_ $sf>](&mut self, val: bool) {
                    let bitmask: $ft = 1 << $sfs;
                    if val {
                        self.$f |= Into::<$ft>::into(bitmask);
                    } else {
                        self.$f &= Into::<$ft>::into(!bitmask);
                    }
                }
            }
        };
        (impl "$sf_impl", $f:ident, $ft:ty, $sfv:vis, $sf:ident, $sfs:expr, $sft:ty) => {
            paste! {
                $sfv fn $sf(&self) -> $sft {
                    $sft::new(
                        ((self.$f & (($sft::MASK as $ft) << $sfs)) >> $sfs)
                            as <$sft as Number>::UnderlyingType,
                    )
                }
                $sfv fn [<set_ $sf>](&mut self, val: $sft) {
                    self.$f &= Into::<$ft>::into(!(($sft::MASK as $ft) << $sfs));
                    self.$f |= Into::<$ft>::into((val.value() << $sfs) as $ft);
                }
            }
        };

        //Regular struct macro
        //Generic support courtesy of: https://stackoverflow.com/a/61189128/10910105
        (
            $(#[$struct_meta:meta])*
            $sv:vis struct $struct_name:ident$(<$($lt:tt$(:$clt:tt$(+$dlt:tt)*)?),+>)? {
                $($fv:vis $f:ident : $ft:ty,
                    $($(|$sfv:vis $sf:ident : $sfs:expr;$sft:tt,)+)?
                )*
            };
        ) => {
                // Define the struct
                $(#[$struct_meta])*
                #[derive(Debug, PartialEq, Clone)]
                $sv struct $struct_name$(<$($lt $(:$clt$(+$dlt)*)?),+>)? {
                    $($fv $f: $ft),*
                }
                // Generate the impl block
                impl$(<$($lt$(:$clt$(+$dlt)*)?),+>)? $struct_name$(<$($lt),+>)? {
                    $($($(
                        zorua!(impl "$sf_impl", $f, $ft, $sfv, $sf, $sfs, $sft);
                    )+)?)*
                }
                impl$(<$($lt$(:$clt$(+$dlt)*)?),+>)? Zorua for $struct_name$(<$($lt),+>)? {
                    fn swap_bytes_mut(&mut self) {
                        $(self.$f.swap_bytes_mut();)*
                    }
                }
        };

        // single tuple struct w/ optional non-const generics
        {
            $(#[$struct_meta:meta])*
            $sv:vis struct $struct_name:ident$(<$($lt:tt$(:$clt:tt$(+$dlt:tt)*)?),+>)? (
                $fv:vis $ft:ty
            );
        }=> {
            $(#[$struct_meta])*
            #[derive(Debug, PartialEq, Clone)]
            $sv struct $struct_name$(<$($lt:tt$(:$clt:tt$(+$dlt:tt)*)?),+>)? ($fv $ft);

            impl$(<$($lt$(:$clt$(+$dlt)*)?),+>)? Zorua for $struct_name$(<$($lt),+>)? {
                #[inline]
                fn swap_bytes_mut(&mut self) {
                    self.0.swap_bytes_mut();
                }
            }
        };

        // single tuple struct w/ single const generic
        {
            $(#[$struct_meta:meta])*
            $sv:vis struct $struct_name:ident<const $N:ident : $Nt:ty> (
                $fv:vis $ft:ty
            );
        }=> {
            $(#[$struct_meta])*
            #[derive(Debug, PartialEq, Clone)]
            $sv struct $struct_name<const $N : $Nt> ($fv $ft);

            impl<const $N: $Nt> Zorua for $struct_name<$N> {
                #[inline]
                fn swap_bytes_mut(&mut self) {
                    self.0.swap_bytes_mut();
                }
            }
        };
    }
    pub use zorua;
}
