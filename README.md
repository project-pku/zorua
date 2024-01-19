# zorua

Zorua enables the creation of **zero-copy**, **endian-aware**, **bitfield-equipped** structs in Rust.

## Installation

To use `zorua` add the following dependencies to your `Cargo.toml`:

```toml
[dependencies]
zorua = { git = "https://github.com/project-pku/zorua.git", tag="v0.1" }
zerocopy = { version = "~0.7.30", features = ["derive"] }
```

<sub>Until [this issue](https://github.com/google/zerocopy/issues/11) is resolved, `zerocopy` must be imported directly, as shown above.</sub>

## Usage

### Defining a struct
To define a zorua struct, simply import the `prelude` module and use the `zorua` macro:
```rust
use zorua::prelude::*;

zorua! {
    #[repr(C)]
    pub struct MyStruct {
        pub field_a: u32_le, //little-endian u32
        pub field_b: u8,
        pub field_c: MyWrapperStruct, //can compose `zorua` structs
        field_d: u16_le, //parent field (no `pub` so only bitfields are exposed)
        |pub bitfield_a: 0;u5, //5 bit uint @ index 0
        |pub bitfield_b: 5;bool, //1 bit bool @ index 5
        |bitfield_c: 6;i7, //bitfields can be private too
        |pub padding: 13;u4, //The rest of the bits
    };
}

zorua! {
  #[repr(transparent)]
  pub struct MyWrapperStruct(u8)
}
```

## Setting and getting fields
Referencing the same `MyStruct` defined above, we can access its fields like so:

```rust
use zorua::transmute::FromZeroes;
fn main() {
  //define a MyStruct populated with zeros
  let mut myStruct: MyStruct = FromZeroes::new_zeroed();

  myStruct.field_a = 23.into(); //`into` needed to convert u32 -> u32_le
  myStruct.field_b = 7; //no need for `into` since u8 is a primitive

  myStruct.set_bitfield_a(u5::new(3)); //set bitfield_a to 3
  if (!myStruct.bitfield_b()) { //get bitfield_b (which is a bool)
    println!("Bitfield c is: {}", myStruct.bitfield_c); //print bitfield_c
  }
}
```

*We use the [`FromZeroes` trait](https://docs.rs/zerocopy/latest/zerocopy/trait.FromZeroes.html) here, which is implemented on all zorua structs. It provides functions for initializing & resetting your struct with zeros.*

### Converting to/from bytes
Converting from and back to an array of bytes can be done by importing the relevant traits from `zorua::transmute` (all of which are included in the `prelude`) and calling the following functions:
```rust
use zorua::transmute::{AsBytes, FromBytes};

let bytes = [0x00, 0x01, 0x02, 0x03, 0x04].as_slice();
let myStruct = MyFiveByteStruct::ref_from(bytes).unwrap(); //uses FromBytes trait
let bytes = myStruct.as_bytes(); //uses AsBytes trait
```

There are other ways to convert between bytes and a `zorua` struct, which are detailed in the documentation of the [`AsBytes`](https://docs.rs/zerocopy/latest/zerocopy/trait.AsBytes.html) and [`FromBytes`](https://docs.rs/zerocopy/latest/zerocopy/trait.FromBytes.html) traits.

### Field type constraints
In order to ensure a struct is zerocopy-able, all zorua structs:

- Must include a [non-aligned repr](https://doc.rust-lang.org/nomicon/other-reprs.html) annotation, e.g. `#[repr(C)]` or `#[repr(transparent)]`.
- Must only have fields of the following types[^1][^2]:
  - Single byte values, e.g. `u8`, `i8`.
  - One of the endian-aware multi-byte integers, e.g. `u16_le`, `i64_be`, `u128_le`.
    - These are provided in both the `zorua::prelude` and `zorua::data_type` modules for convenience.
  - Another zorua struct.
  - A fixed size array `[T; N]` of any of the above types.

[^1]: More types are available, including user-defined ones, as long as they are [sized](https://doc.rust-lang.org/std/marker/trait.Sized.html) and implement `zerocopy`'s [`Unaligned` trait](https://docs.rs/zerocopy/latest/zerocopy/trait.Unaligned.html).

[^2]: it might be possible to use types that are not `Unaligned`, e.g. `u16`, if they are arranged within the struct in such a manner as to not create any padding. This is not recommended though, especially since this could create arch dependent behavior in how they are serialized. For such use cases, the `zerocopy` crate proper would be better suited.

### Bitfield types
You'll notice that some fields in a zorua struct have a `|` prepended to them. Any block of fields beginning with this character are ***bitfields*** and they correspond to the first non-bitfield above them, we call this field their *parent*.

They are formatted like so:

```rust
|pub bitfield_a: 0;u5
 ^   ^           ^ ^
 |   |           | |
 |   |           | +- Type: u5
 |   |           +--- Bit Index: 0
 |   +--------------- Name: bitfield_a
 +------------------- (Optional) Vis. Modifier: pub
```

Some things to note about bitfields:

- Their parent field must be either a `u8` or one of the endian-aware multi-byte uints.
- Their type must be either a `bool` or a `uX` where X is any number from 1-128.
  - The `u1` to `u127` types (not including the built-ins `u8`,`u16`,`u32`,`u64`,`u128`) are provided in both the `zorua::prelude` and `zorua::data_type` modules for convenience. They are not endian-aware, because they don't need to be.
- The `zorua` macro, currently, does not enforce that a bitfield block accounts for all bits in the parent, nor that some fields do not overlap:

```rust
pub parent: u8,
|pub child_1: 0;u4 //Takes up bits 0-3
|pub child_2: 1;u5 //Takes up bits 1-5
```