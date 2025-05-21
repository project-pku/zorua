# zorua
Zorua enables the creation of **zero-copy**, **endian-aware**, **bitfield-equipped** structs in Rust.

## Installation
To use `zorua` add the following dependency to your `Cargo.toml`:

```toml
[dependencies]
zorua = { git = "https://github.com/project-pku/zorua.git", tag="v0.7" }
```

> [!WARNING]  
> This crate currently uses **nightly**!

> [!NOTE]
> zorua supports `no_std` environments by disabling the default `std` feature.

## Features
To use the crate simply import the `prelude` module.

The `zorua` crate was created to enable the following features safely, and within a [zero-copy](https://en.wikipedia.org/wiki/Zero-copy) context:

- [Transmutation](#transmutation)
- [Bitfields](#bitfields)
- [Enums](#enums)

### Transmutation
Deriving the `ZoruaField` trait places conditions on the implementing type to ensure safety. All these conditions are checked at compile time:

- Must have a compatible repr (`C` for structs, `u8` for enums)
- Must have no padding
- All it's fields must be types that implement `ZoruaField`

The `ZoruaField` trait provides multiple transmutation methods:
- `transmute<T>()` - Convert to another ZoruaField type (owned)
- `transmute_ref<T>()` - Get a reference to the data as another type
- `transmute_mut<T>()` - Get a mutable reference to the data as another type
- `transmute_split_ref<A, B>()` - Split a reference into two adjacent parts
- `transmute_split_mut<A, B>()` - Split a mutable reference into two adjacent parts
- `box_transmute<T>()` - Convert a boxed value to another type (requires `std` feature)

> [!NOTE]
> These methods can only be used if the target type has the same size and an equal or lower alignment as the source type. We call this condition having a **compatible layout**, and these methods validate it at compile time.

The `ZoruaField` trait also provides convinience methods for transmuting structs into aligned bytes specifically:
- `into_aligned_bytes()` - Convert to AlignedBytes
- `as_aligned_bytes()` - Get a reference to the data as AlignedBytes
- `as_aligned_bytes_mut()` - Get a mutable reference to the data as AlignedBytes

> [!NOTE]
> The `AlignedBytes<ALIGN, SIZE>` type is an array of bytes of size `SIZE` with an alignment `ALIGN` known at compile time.
The trait also provides raw byte access:
- `as_bytes()` - Get a byte slice view of the data
- `as_bytes_mut()` - Get a mutable byte slice view of the data


```rust
use zorua::prelude::*;

#[repr(C)]
#[derive(ZoruaField)]
struct MyStruct {
    field_a: u8,
    field_b: u8,
    field_c: u16,
}

// Same size and alignment as MyStruct
// i.e. compatible layout
#[repr(C)]
#[derive(ZoruaField, Clone)]
struct MyStruct2 {
    field_1: u16,
    field_2: u16,
}

fn main() {
    let myStruct = MyStruct {
        field_a: 0,
        field_b: 2,
        field_c: 5,
    };

    // Transmute to another ZoruaField of w/ compatible layout
    let myStruct2: MyStruct2 = myStruct.transmute();

    // Convert to aligned bytes
    let bytes = myStruct2.clone().into_aligned_bytes();
    
    // Or use the transmutation fn instead
    let bytes: aligned_bytes_of!(MyStruct) = myStruct2.transmute();
}
```

> [!NOTE]
> The `aligned_bytes_of!(T)` macro simply returns `AlignedBytes<{mem::align_of::<T>()}, {mem::size_of::<T>()}>`. It is useful for type annotations.

### Bitfields
The `zorua` crate also supports adding virtual *bitfields* to your structs:

```rust
bitfields! {
    #[repr(C)]
    #[derive(ZoruaField)]
    pub struct MyStruct {
        pub field_a: u16,
        field_b: u16, // Backing field (no `pub` so only bitfields are exposed)
        |pub bitfield_a: u5@0, // 5 bit uint @ index 0
        |pub bitfield_b: bool@5, // 1 bit bool @ index 5
        |bitfield_c: u7@6, // Bitfields can be private too
        |pub padding: u3@13, // The rest of the bits
    };
}

fn main() {
    let myStruct = MyStruct {
        field_a: 12,
        field_b: 367, //0b000-0000101-1-01111
    }

    println!("{:#b}", myStruct.bitfield_a()); // "0b01111"
    myStruct.set_bitfield_a(2u8.try_into().unwrap()); //casts u8->u5
    println!("{:#b}", myStruct.bitfield_a()); // "0b00010"
}

```

#### Declaring a bitfield
```
field_b: u16, <------------ Backing field: field_b
|pub bitfield_a: bool@0,
|pub bitfield_b: u5@1,
 ^   ^           ^  ^
 |   |           |  |
 |   |           |  +- Type: u5
 |   |           +---- Index: 1
 |   +---------------- Name: bitfield_b
 +-------------------- (Optional) Vis keyword: pub
```

#### Bitfield types
Only certain types can be used as bitfield types in the `bitfield!` macro.

**`uX` Types**

Importing `zorua::prelude::*` makes the types `u1`-`u15` available. These types are exactly what they seem, they are unsigned integers of varying bitsizes. Just like with the built-in `uX`'s, if a type is smaller than the data trying to be fit into it (e.g. `u16` into a `u12`), a `try_into()` is required instead of an `into()`.

**Custom Bitfields**

Just like the `ZoruaField` trait allows one to create structs capable of being transmuted, implementing the `ZoruaBitField` trait allows a struct to be used as a bitfield type in the `bitfield!` macro.

### Enums
While we've covered implementing `ZoruaField` and `ZoruaBitField` w.r.t to structs, the `zorua` crate *also* supports implementing these traits on [c-like enums](https://doc.rust-lang.org/rust-by-example/custom_types/enum/c_like.html).

#### Exhaustive enums
Since every possible bit-pattern must be valid for a `ZoruaField`/`ZoruaBitField`, implementing them on enums directly requires that type has to have a variant for for each possible value it could hold.

**ZoruaField** enums must have exactly 256 (2^8) variants:
```rust
#[repr(u8)] //requires some `uX` repr
#[derive(ZoruaField)]
enum MyExhaustiveEnum {
    Variant0,
    Variant1,
    //...,
    Variant255,
}
```

**ZoruaBitField** enums must have some power of 2 (2^n) variants:
```rust
#[repr(u8)] //requires some `uX` repr
#[derive(ZoruaBitField)]
enum MyExhaustive2BitEnum {
    Variant1,
    Variant2,
    Variant3,
    Variant4, //2^2 = 4
}
```

#### Fallible enums
This is great, but its unlikely that all your enums will have *exactly* 2^n variants. What do you do in those cases? This is where `ZoruaFallible` comes in. It allows you to define valid and invalid bitpatterns that are checked before every access.

```rust
#[repr(u8)]
#[derive(ZoruaFallible)]
#[targets(u8, u2)]
enum MyEnum {
    Variant0,
    Variant1,
    Variant2, //2^1 < 3 < 2^2
}

struct MyStruct {
    field_a: Fallible<MyEnum, u8>,
    field_b: u8,
    |pub test: Fallible<MyEnum, u2>@0
}

fn main() {
    let myStruct = MyStruct {
      field_b: 0,
    }
    
    //valid value
    myStruct.set_test(MyEnum::Variant1.into())
    let val: Fallible<MyEnum, u2> = myStruct.test();
    let res: Result<MyEnum, u2> = val.try_into();
    assert!(res.is_ok());

    //invalid value
    myStruct.set_test(Fallible::from_raw(u2::new(3)))
    let val: Fallible<MyEnum, u2> = myStruct.test();
    let res: Result<MyEnum, u2> = val.try_into();
    assert!(res.is_err()); //if value is invalid, raw value is error
}

```

Note that a `Fallible` type can be used either as a `ZoruaField` or `ZoruaBitField` depending on the given target (e.g. targets of `u8`, `u16`, etc. can be used as both fields and bitfields, while `u2`, `u12`, etc. can only be used as bitfields).

Also note that while `ZoruaFallible` can be derived for enums, you can manually implement it for structs as well.
