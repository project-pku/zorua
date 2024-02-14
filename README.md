# zorua
Zorua enables the creation of **zero-copy**, **endian-aware**, **bitfield-equipped** structs in Rust.

## Installation
To use `zorua` add the following dependency to your `Cargo.toml`:

```toml
[dependencies]
zorua = { git = "https://github.com/project-pku/zorua.git", tag="v0.4" }
```

> [!WARNING]  
> This crate currently uses **nightly**!

## Example
To use the crate simply import the `prelude` module and use make use of the derive and `bitfields` macros:

```rust
use zorua::prelude::*;

bitfields! {
    #[repr(C)]
    #[derive(ZoruaField)]
    #[alignment(A4)]
    pub struct MyStruct {
        pub field_a: u32,
        pub field_b: u8,
        pub field_c: MyWrapperStruct, // All fields must also implement ZoruaField
        field_d: u16, // Backing field (no `pub` so only bitfields are exposed)
        |pub bitfield_a: u5@0, // 5 bit uint @ index 0
        |pub bitfield_b: bool@5, // 1 bit bool @ index 5
        |bitfield_c: u7@6, // Bitfields can be private too
        |pub padding: u2@13, // The rest of the bits
    };
}

#[derive(ZoruaField)]
pub struct MyWrapperStruct(u8)
```

## Traits
There are two main traits this crate provides: `ZoruaField` and `ZoruaBitField`.

### ZoruaField
`ZoruaField` represents a type that is valid for any bitpattern. It can be derived on structs and exhaustive [c-like enums](https://doc.rust-lang.org/rust-by-example/custom_types/enum/c_like.html) (i.e. having a variant for every bitpattern).

```rust
use zorua::prelude::*;

// Wrapper struct
#[derive(ZoruaField)]
struct MyWrapperStruct(u8)

// Composite struct
#[derive(ZoruaField)]
#[alignment(A4)] //composite structs must include alignment annotation
struct MyStruct {
  field_a: u32,
  field_b: u8,
  field_c: MyWrapperStruct
}

// u8 Exhaustive enum
#[derive(ZoruaField)]
#[repr(u8)]
enum MyExhaustiveEnum {
  Variant1,
  Variant2,
  //...,
  Variant256, //2^8 = 256
}
```

The `ZoruaField` trait enables the use of:
- `to_le_mut` and `to_be_mut` which swaps the byte order, in-place, of the type to the target architecture's endianness.
- `as_bytes` and `as_bytes_mut` which safely transmutes the type to an aligned byte array, up to mutability.
- `from_bytes` and `from_bytes_mut` which safely transmutes an aligned byte array to an instance of the type, up to mutability.

### ZoruaBitField
`ZoruaBitField` represents a type that can be represented as an *n*bit value. It can be derived on exhaustive c-like enums, or implemented manually for structs.

```rust
// Struct
// - Can be represented by a u4
struct MyStruct {
  fourbitval: u8
}
impl trait ZoruaBitField for MyStruct {
    type BitRepr = u4;

    fn to_bit_repr(self) -> Self::BitRepr {
      //truncate u8 -> u4
      (self.fourbitval % 16).try_into().unwrap()
    }

    fn from_bit_repr(value: Self::BitRepr) -> Self {
      value.into() //cast u4 -> u8
    }
}

// Exhaustive enum
// - Total number of variants is a power of 2
#[derive(ZoruaField)]
#[repr(u8)]
enum MyExhaustive2BitEnum {
  Variant1,
  Variant2,
  Variant3,
  Variant4 //2^2 = 4
}
```

## Bitfields
You may be asking yourself, "`ZoruaField`s made sense, they are required for the safe transmutation of data types to byte arrays, and structured byte order swapping. But what is the point of `ZoruaBitField`s?" The answer is found in the `bitfield` macro. Consider the struct defined in the [example](#example) section:

```rust
bitfields! {
    #[repr(C)]
    #[derive(ZoruaField)]
    #[alignment(A4)]
    pub struct MyStruct {
        pub field_a: u32,
        pub field_b: u8,
        pub field_c: MyWrapperStruct, // All fields must also implement ZoruaField
        field_d: u16, // Backing field (no `pub` so only bitfields are exposed)
        |pub bitfield_a: u5@0, // 5 bit uint @ index 0
        |pub bitfield_b: bool@5, // 1 bit bool @ index 5
        |bitfield_c: u7@6, // Bitfields can be private too
        |pub padding: u2@13, // The rest of the bits
    };
}
```

You'll notice that some "fields" in the struct have a `|` prepended to them. Any block of fields beginning with this character are ***bitfields*** and they correspond to the first non-bitfield above them, we call this field their ***backing field***.

In the example above:

```rust
field_d: u16, <------------ Backing Field: my_backing_val
|pub bitfield_a: bool@0,
|pub bitfield_b: u5@1,
 ^   ^           ^  ^
 |   |           |  |
 |   |           |  +- Type: u5
 |   |           +---- Bit Index: 1
 |   +---------------- Name: bitfield_b
 +-------------------- (Optional) Vis Modifier: pub
```

The bitfields must be of a type that implements `ZoruaBitField`. On top user defined types, `ZoruaBitField` is implemented on bool (as a 1 bit value), and the provided `u1`-`u127` types.

### Implementation
Adding a bitfield to a struct doesn't change anything about the data it stores or its layout. Instead, it adds a getter & setter method onto the struct for that particular bitfield. These functions get/set as many bits from the backing field as needed for bitfield's type, starting at the given index.

Using the example above, we could call the following:

```rust
let mystruct: MyStruct = //...init struct

mystruct.field_d = 0b100000; //bit index 5 = 1 (i.e. true)

println!("{}", mystruct.bitfield_a()); //prints "true"
mystruct.set_bitfield_b(false); //sets value of bitfield_a
println!("{}", mystruct.bitfield_a()); //prints "false"
```

Some things to note about bitfields:
- Backing fields must be one of: `u8`, `u16`, `u32`, `u64`, `u128`.
- The `bitfields` macro does not currently enforce that a bitfield block accounts for all bits in the parent, nor that fields do not overlap:

```rust
pub parent: u8,
|pub child_1: u4@0 //Takes up bits 0-3
|pub child_2: u5@1 //Takes up bits 1-5
```

## Fallible
While the `ZoruaField` and `ZoruaBitField` traits deal with cover the case of exhaustive enums, what about non-exhaustive ones. After all, most enums don't happen to have a number of variants equal to a power of 2.

Enter the `ZoruaFallible` trait. Deriving this trait on a c-like enum, with an arbitrary number of variants, allows the use of the `Fallible` type in in fields and bitfields. For example:

```rust
#[derive(ZoruaFallible)]
#[targets(u3, u8)] //can be represented by both u8 and u3
enum MyEnum {
    Variant1,
    Variant2,
    Variant3,
    Variant4,
    Variant5
}

struct MyTestStruct {
  field_a: u16,
  field_b: Fallible<MyEnum, u8>, //1 byte
  field_c: u8,
  |subfield_a: u2@0,
  |subfield_b: Fallible<MyEnum, u3>@2, //3 bits
  |subfield_c: bool@5,
}
```
