#![cfg_attr(not(feature = "std"), no_std)]

pub mod data_type;
pub mod native;
pub mod traits;

#[macro_use]
pub mod prelude {
    pub use crate::data_type::*;
    pub use crate::native::ZoruaNative;
    pub use crate::traits::*;
    pub use paste::paste;

    pub use zorua_macro::ZoruaNative;
    pub use zorua_macro::bitfields;
}
