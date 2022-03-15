//! Derive macro for encoding and decoding instructions and operands as bytecode.
//!
//! Note that only big-endian byte ordering is supported by this crate.
//!
//! # Derive macro
//!
//! ## Enum attributes
//!
//! Attributes that can be used on enums.
//!
//! - `type = ...` (required)
//!
//!     Sets which type is used for the instruction code. Possible values are `u8`, `u16`, `u32`,
//!     and `u64`.
//!
//! ## Variant attributes
//!
//! Attributes that can be used on enum variants.
//!
//! - `code = ...`
//!
//!     Sets the instruction codes for this enum variant. A code has to be specified for every
//!     instruction this variant generates. If the variant only generates one instruction, then a
//!     single number can be used for the `code` attribute (instead of a list of numbers).
//!
//! ## Variant field attributes
//!
//! Attributes that can be used on fields of enum variants.
//!
//! - `flatten = [...]`
//!
//!     Flattens the field into instruction codes for every specified value. This attribute is
//!     intended to be used for optimizing some values and falling back to using an operand for any
//!     non-specified value. If you want to flatten all values, use the `flatten_all` attribute
//!     instead.
//!
//!     See the [example](#example) below for more information on how to use this attribute.
//!
//! - `flatten_all = [...]`
//!
//!     Flattens the enum variant into instruction codes for every specified value. Note that every
//!     possible value has to be specified. If you only want to flatten some values, use the
//!     `flatten` attribute instead.
//!
//!     See the [example](#example) below for more information on how to use this attribute.
//!
//! ## Field attributes
//!
//! Attributes that can be used on struct fields or fields of enum variants.
//!
//! - `skip`
//!
//!     Skips encoding and decoding of the field. For decoding, the value from
//!     `std::default::Default` is used.
//!
//! # Example
//!
//! ```rust
//! use bytecoding::Bytecode;
//!
//! #[derive(Debug, PartialEq, Eq, Bytecode)]
//! struct Operand {
//!     value1: u8,
//!     value2: u16,
//! }
//!
//! #[derive(Debug, PartialEq, Eq, Bytecode)]
//! #[bytecode(type = u8)]
//! enum Instruction {
//!     Add,
//!     Sub,
//!     Jump(u8),
//!     Foo(Operand),
//!
//!     // This generates four instruction codes without an operand for the values 0 to 3, and one
//!     // instruction code with an operand for all remaining values
//!     Const {
//!         #[bytecode(flatten = [0, 1, 2, 3])]
//!         index: u16,
//!     },
//!
//!     // This generates two instruction codes (without an operand):
//!     // - The first code is for the value `true`
//!     // - The second code is for the value `false`
//!     Bool(#[bytecode(flatten_all = [true, false])] bool),
//! }
//!
//! # fn main() -> Result<(), bytecoding::DecodeError> {
//! let instructions = vec![
//!     Instruction::Sub,
//!     Instruction::Add,
//!     Instruction::Foo(Operand { value1: 20, value2: 30 }),
//!     Instruction::Jump(42),
//!     Instruction::Const { index: 0 },
//!     Instruction::Const { index: 4 },
//!     Instruction::Bool(true),
//!     Instruction::Bool(false),
//! ];
//!
//! // Encoding
//! let mut buf = Vec::new();
//! for instruction in &instructions {
//!     instruction.encode(&mut buf);
//! }
//! assert_eq!(buf, vec![
//!     1, // Sub
//!     0, // Add
//!     3, 20, 0, 30, // Foo(Operand { value1: 20, value2: 30 })
//!     2, 42, // Jump(42)
//!     4, // Const { index: 0 }
//!     8, 0, 4, // Const { index: 4 }
//!     9, // Bool(true)
//!     10, // Bool(false)
//! ]);
//!
//! // Decoding
//! let mut buf: &[u8] = &buf;
//! let mut decoded_instructions = Vec::new();
//! while !buf.is_empty() {
//!     decoded_instructions.push(Instruction::decode(&mut buf)?);
//! }
//! assert_eq!(decoded_instructions, instructions);
//! # Ok(())
//! # }

use bytes::{Buf, BufMut};
use core::fmt;
use paste::paste;
use std::error::Error;

/// Derive macro for [`Bytecode`].
///
/// See the [module-level documentation](crate) for more information.
pub use bytecoding_derive::Bytecode;

#[doc(hidden)]
pub use bytes;

/// Error that can occur while decoding the bytecode.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum DecodeError {
    InvalidValue(Box<[u8]>),
    UnexpectedEnd,
}

impl Error for DecodeError {}

impl fmt::Display for DecodeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::InvalidValue(v) => write!(f, "Invalid value `{:?}`", v),
            Self::UnexpectedEnd => f.write_str("Unexpected end"),
        }
    }
}

/// Encode and decode instructions and operands.
pub trait Bytecode: Sized {
    /// Encodes the instruction or the operand.
    ///
    /// # Example
    ///
    /// ```rust
    /// use bytecoding::Bytecode;
    ///
    /// #[derive(Bytecode)]
    /// #[bytecode(type = u8)]
    /// enum Instruction {
    ///     Add,
    ///     Sub,
    ///     Jump(u8),
    /// }
    ///
    /// let mut buf = Vec::new();
    /// Instruction::Sub.encode(&mut buf);
    /// Instruction::Add.encode(&mut buf);
    /// Instruction::Jump(42).encode(&mut buf);
    ///
    /// assert_eq!(buf, vec![1, 0, 2, 42]);
    /// ```
    fn encode<B: BufMut>(&self, buf: &mut B);

    /// Decodes a instruction or an operand from the given buffer.
    ///
    /// Advances the buffer to the next instruction.
    ///
    /// # Example
    ///
    /// ```rust
    /// use bytecoding::Bytecode;
    ///
    /// #[derive(Debug, PartialEq, Eq, Bytecode)]
    /// #[bytecode(type = u8)]
    /// enum Instruction {
    ///     Add,
    ///     Sub,
    ///     Jump(u8),
    /// }
    ///
    /// # fn main() -> Result<(), bytecoding::DecodeError> {
    /// let mut buf: &[u8] = &[1, 0, 2, 42];
    /// let instructions = [
    ///     Instruction::decode(&mut buf)?,
    ///     Instruction::decode(&mut buf)?,
    ///     Instruction::decode(&mut buf)?,
    /// ];
    ///
    /// assert_eq!(instructions, [Instruction::Sub, Instruction::Add, Instruction::Jump(42)]);
    /// # Ok(())
    /// # }
    /// ```
    fn decode<B: Buf>(buf: &mut B) -> Result<Self, DecodeError>;
}

macro_rules! impl_integer_bytecode {
    ($($type_ident:ident),*) => {
        $(impl Bytecode for $type_ident {
            fn encode<B: BufMut>(&self, buf: &mut B) {
                paste! { buf.[<put_ $type_ident>](*self); }
            }

            fn decode<B: Buf>(buf: &mut B) -> Result<Self, DecodeError> {
                const NUM_BYTES: usize = ($type_ident::BITS / 8) as usize;
                if buf.remaining() < NUM_BYTES {
                    return Err(DecodeError::UnexpectedEnd);
                }
                Ok(paste! { buf.[<get_ $type_ident>]() })
            }
        })*
    };
}

impl_integer_bytecode!(u8, u16, u32, u64, u128, i8, i16, i32, i64, i128);
