# zorua
Zorua enables the creation of **zero-copy**, **endian-aware**, **bitfield-equipped** structs in Rust.

## Installation
To use `zorua` add the following dependency to your `Cargo.toml`:

```toml
[dependencies]
zorua = { git = "https://github.com/project-pku/zorua.git", tag="v0.8" }
```

> [!NOTE]
> zorua supports `no_std` environments by disabling the default `std` feature.

## Features
To use the crate simply import the `prelude` module.

The `zorua` crate was created to enable the following features safely, and within a [zero-copy](https://en.wikipedia.org/wiki/Zero-copy) and endian-aware context:

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

The trait also provides raw byte access:
- `as_bytes()` - Get a byte slice view of the data
- `as_bytes_mut()` - Get a mutable byte slice view of the data

For aligned byte buffers, you can transmute to `AlignedBytes<ALIGN, SIZE>` - an array of bytes with size `SIZE` and alignment `ALIGN` known at compile time. The `aligned_bytes_of!(T)` macro provides a convenient way to get an `AlignedBytes` type matching any struct's layout.


```rust
use zorua::prelude::*;

#[repr(C)]
#[derive(ZoruaField)]
struct MyStruct {
    field_a: u8,
    field_b: u8,
    field_c: u16_le,
}

// Same size and alignment as MyStruct
// i.e. compatible layout
#[repr(C)]
#[derive(ZoruaField, Clone)]
struct MyStruct2 {
    field_1: u16_le,
    field_2: u16_le,
}

fn main() {
    let myStruct = MyStruct {
        field_a: 0,
        field_b: 2,
        field_c: 5u16.into(),
    };

    // Transmute to another ZoruaField of w/ compatible layout
    let myStruct2: MyStruct2 = myStruct.transmute();

    // Convert to aligned bytes
    let bytes: aligned_bytes_of!(MyStruct) = myStruct2.clone().transmute();

    // Get a reference as aligned bytes
    let bytes_ref: &aligned_bytes_of!(MyStruct) = myStruct2.transmute_ref();
    
    // Or access raw bytes directly
    let raw_bytes: &[u8] = myStruct2.as_bytes();
}
```

> [!NOTE]
> Built-in primitives like `u16` do not implement `ZoruaField` because their internal representation is different on different architectures. Use `u16_le` instead, which is always stored in little-endian format. There are equivalents for `u32` and `u64`, for both little-endian and big-endian.

### Bitfields
Bitfields let you define sub-byte fields packed within a larger integer. They're useful for binary formats where multiple values are crammed into a single byte or word.

The `bitfields!` macro wraps struct definitions, allowing you to attach bitfield accessors to a compatible field (the *backing field*):

```rust
bitfields! {
    #[repr(C)]
    #[derive(ZoruaField)]
    pub struct Pokemon {
        pub species: u16_le,
        // Backing field stores the raw bits; bitfields provide typed access
        iv_data: u32_le {
            pub hp_iv: u5@0,       // bits 0-4
            pub atk_iv: u5@5,      // bits 5-9
            pub def_iv: u5@10,     // bits 10-14
            pub speed_iv: u5@15,   // bits 15-19
            pub spatk_iv: u5@20,   // bits 20-24
            pub spdef_iv: u5@25,   // bits 25-29
            pub is_egg: bool@30,   // bit 30
            pub is_nicknamed: bool@31,
        },
    }
}

fn main() {
    let pokemon = Pokemon {
        species: 25u16.into(), // Pikachu
        iv_data: 0x7FFF_FFFFu32.into(), // all IVs maxed
    };

    println!("{}", pokemon.hp_iv());  // 31
    pokemon.set_is_egg(true);
}
```

#### Syntax
```
backing_field: u32_le {
    pub flag: bool@0,
        lower: u7@1,
    pub upper: u8@8,
    ─┬─ ──┬── ─┬ ┬
     │    │    │ └─ bit index
     │    │    └─── type
     │    └──────── name
     └───────────── visibility
},
```

Each bitfield generates a getter (`name()`) and setter (`set_name()`). Doc comments on bitfields are supported.

#### Bitfield types

The prelude exports `u1` through `u15` for sub-byte integers. These work like standard Rust integers—use `.into()` for infallible conversions and `.try_into()` when overflow is possible.

You can also use `bool` (1 bit), or any type implementing `ZoruaBitField`.

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
#[target_byte(u8)] //For use in fields. Can be u8, u16_le, u64_be, etc. 
#[target_bit(u2)] //For use in bitfields. Can be u1, u2, u12, etc.
enum MyEnum {
    Variant0,
    Variant1,
    Variant2, //2^1 < 3 < 2^2
}

bitfields! {
    #[repr(C)]
    #[derive(ZoruaField)]
    struct MyStruct {
        field_a: Fallible<MyEnum, u8>,
        field_b: u8 {
            pub test: Fallible<MyEnum, u2>@0,
        },
    }
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

Note that a `Fallible` type can be used either as a `ZoruaField` or `ZoruaBitField` depending on the given target.

Also note that while `ZoruaFallible` can be derived for enums, you can manually implement it for structs as well (although you must ensure the safety conditions are met).
