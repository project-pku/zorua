#![cfg_attr(not(feature = "std"), no_std)]

pub mod bits;
pub mod data_type;
pub mod traits;

#[macro_use]
pub mod prelude {
    pub use crate::bits;
    pub use crate::data_type::*;
    pub use crate::traits::*;
    pub use paste::paste;

    pub use zorua_macro::Zorua;
    pub use zorua_macro::ZoruaStruct;
    pub use zorua_macro::bitfields;
}
