# zorua

Zorua enables the creation of **zero-copy**, **endian-aware**, **bitfield-equipped** structs in Rust.

## Installation

To use `zorua` add the following dependency to your `Cargo.toml`:

```toml
[dependencies]
zorua = { git = "https://github.com/project-pku/zorua.git", tag="v0.3" }
```

> [!WARNING]  
> This crate currently uses **nightly**!

## Types
### Zorua structs
Zorua structs are the main feature of the crate. To create one simply import the `prelude` module and use the `zorua!` macro:

```rust
use zorua::prelude::*;

zorua! {
    #[repr(C)] //important*
    pub struct MyStruct {
        pub field_a: u32,
        pub field_b: u8,
        pub field_c: MyWrapperStruct, //can compose `zorua` structs
        field_d: u16, //backing field (no `pub` so only bitfields are exposed)
        |pub bitfield_a: u5@0, //5 bit uint @ index 0
        |pub bitfield_b: bool@5, //1 bit bool @ index 5
        |bitfield_c: u7@6, //bitfields can be private too
        |pub padding: u4@13, //The rest of the bits
    };
}

// You can also define wrapper structs w/ the macro
// The type being  wrapped must impl `ZoruaField`
zorua! {
  #[repr(transparent)]
  pub struct MyWrapperStruct(u8)
}
```
A zorua struct is composed of **fields** and **bitfields**. They are expanded upon below.

**To ensure stability and field ordering, zorua structs should include the `#[repr(C)]` attr and wrapper structs should include the `#[repr(transparent)]`.*

### Fields 
All fields in a zorua struct must implement the `ZoruaField` trait. This trait is already implemented on the following types:
- `u8`, `u16`, `u32`, `u64`, `u128`
- `()`
- `[T: ZoruaTrait; const N: usize]`
- Structs/enums created with `zorua!` macro

### Bitfields
You'll notice that some fields in a zorua struct have a `|` prepended to them. Any block of fields beginning with this character are ***bitfields*** and they correspond to the first non-bitfield above them, we call this field their *backing field*.

They are formatted like so:

```rust
pub my_backing_val: u8, <------------ Backing Field: my_backing_val
|pub my_bitfield_a: u2@0,
|pub my_bitfield_b: u5@2,
 ^   ^           ^ ^
 |   |           | |
 |   |           | +- Type: u5
 |   |           +--- Bit Index: 2
 |   +--------------- Name: my_bitfield_a
 +------------------- (Optional) Vis Modifier: pub
```

The following types already implement `ZoruaBitField`:
- The `u1` to `u128` types
- `bool` (takes up 1 bit)
- Enums created with the `zorua!` macro

Adding a bitfield to a zorua struct doesn't change anything about the data it stores or its layout. Instead, it adds a getter & setter method onto the struct for that particular bitfield. These functions get/set as many bits from the backing field as needed for bitfield's type, starting at the given index.

Some things to note about bitfields:

- Backing fields must be one of: `u8`, `u16`, `u32`, `u64`, `u128`.
- The `zorua!` macro does not currently enforce that a bitfield block accounts for all bits in the parent, nor that fields do not overlap:

```rust
pub parent: u8,
|pub child_1: u4@0 //Takes up bits 0-3
|pub child_2: u5@1 //Takes up bits 1-5
```

### Enums
The `zorua!` macro can be used to automatically implement the `ZoruaField` & `ZoruaBitField` traits on [c-like enums](https://doc.rust-lang.org/rust-by-example/custom_types/enum/c_like.html). We simply wrap an enum with the `zorua!` macro, and preface it with its bit repr (i.e. how many bits it takes up in the backing field) and its byte repr (i.e. the smallest built-in uint that can hold the bit repr):

```rust
zorua! {
  u2, u8, //the bit repr, then the bit repr
  pub enum Enum1 {
      Variant1,
      Variant2,
      Variant3,
      //Not enough variants to fill 2 bits
  };
}

zorua! {
  =u1, u8, //`=` before bit repr means exhaustive
  pub enum Enum2 {
      Variant1,
      Variant2,
      //Exactly enough variants to fill 1 bit
  };
}

zorua! {
  =u8, //if bit repr & byte repr both exhaustive, only need 1
  pub enum Enum3 {
      Variant1,
      ...
      Variant256
      //Exactly enough variants to fill 8 bits
  };
}
```

As you can see, there are 3 types of enums that the macro supports:
- **Exhaustive byte enums**: These are enums whose byte and bit repr are equal and that have a variant for each possible value of their the repr. (Considering how many values that is, you'd probably only use it for `u8`). These implement both `ZoruaField` and `ZoruaBitField`.
- **Exhaustive bit enums**: These are enums that have a variant for each possible value of their bit repr but not their byte repr. These implement `ZoruaBitField`, and the `Fallible` versions implement `ZoruaField`.
- **Non-Exhaustive enums**: These are enums that do not have enough variants for either their bit or byte repr. The `Fallible` versions implement both `ZoruaField` and `ZoruaBitField`.

The `Fallible` version of an enum is simply a wrapper that allows it to be used as a field or bitfield despite it not having enough variants to fill it:
```rust
zorua! {
  #[repr(C)]
  pub struct Foo {
    pub field_1: Fallible<Enum1>, //need to wrap w/ Fallible
    pub field_2: Fallible<Enum2>, //^^
    pub field_3: Enum3, //no need for fallible
    pub backing: u16,
    |pub bitfield_1: Fallible<Enum1>@0 //Takes up 2 bits
    |pub bitfield_2: Enum2@2 //Takes up 1 bit
    |pub bitfield_3: Enum3@3 //8 bits
  }
}
```

## Usage
### Setting and getting fields
Consider the following zorua struct:
```rust
zorua! {
  #[repr(C)]
  pub struct MyStruct {
    pub field_1: u16,
    pub field_2: Fallible<Enum1>, //1 byte
    pub field_3: Enum3, //1 byte
    backing: u32,
    |pub bitfield_1: u3@0
    |pub bitfield_2: Fallible<Enum1>@3 //2 bits
    |pub bitfield_3: Enum3@5 //8 bits
    |pub bitfield_4: u8@12
    |pub bitfield_5: bool@20
  }
}
```

We can access its fields as you might expect:
```rust
//Just a regular field
myStruct.field_1 = 23;

//Since field_2 is a fallible, need to cast variant
myStruct.field_2 = Enum1::Variant1.into();

//Since field_2 might not correspond to a valid variant
//Need to use try_into. Error type is just the value.
let x: Enum1 = myStruct.field_2.try_into().unwrap();

//no need to do any casting
myStruct.field_3 = Enum3::Variant2;

let x: u3 = myStruct.bitfield_1();
myStruct.set_bitfield_1(4u8.into()); //u8 -> u3
myStruct.set_bitfield_2(Enum1::Variant1.into());
myStruct.set_bitfield_5(false);
```

### Converting to/from bytes
Currently you can simply call `std::mem::transmute` to zero-copy convert your zorua struct to/from a byte array. To deal with potential alignment issues, will implement a alignment safe conversion in the next release.