//! Simple AArch64 Disassembler.
//!
//! ## Overview
//!
//! `sad64` is a minimalist AArch64 disassembly library featuring full ARMv8-A architecture
//! profile's AArch64 instruction set support, with base and FP/SIMD instructions.
//! This crate is `#![no_std]`-compatible.
//!
//! ## Usage
//!
//! ```no_run
//! use sad64::{Formatter, SimpleFormatter, SymbolResolver, NoopResolver, StdoutOutput};
//!
//! fn main() {
//!     let instructions = [
//!         0xd10203ff,
//!         0xf9000be2,
//!         0xf9400fe8,
//!         0x9b0a7d29,
//!     ];
//!
//!     let mut f = SimpleFormatter;
//!     let resolver = NoopResolver;
//!
//!     for word in instructions {
//!         let insn = sad64::decode(word).unwrap();
//!         f.write_insn(&mut StdoutOutput, &resolver, &insn).unwrap();
//!         println!();
//!     }
//! }
//! ```
//!
//! Will output:
//!
//! ```no_run,custom
//! sub sp, sp, #0x80
//! str x2, [sp, #0x10]
//! ldr x8, [sp, #0x18]
//! mul x9, x9, x10
//! ```
#![allow(non_snake_case, non_camel_case_types)]
#![feature(if_let_guard)]
#![cfg_attr(docsrs, feature(doc_cfg))]
#![cfg_attr(not(feature = "std"), no_std)]
#![warn(missing_docs)]

mod decode;
mod mnemonic;
mod msr;
mod operand;

#[cfg(feature = "format")]
#[cfg_attr(docsrs, doc(cfg(feature = "format")))]
mod format;

#[cfg_attr(docsrs, doc(cfg(feature = "std")))]
#[cfg(all(feature = "format", feature = "std"))]
pub use format::StdoutOutput;
#[cfg(feature = "format")]
pub use format::{Formatter, NoopResolver, SimpleFormatter, SymbolResolver};

pub use mnemonic::Mnemonic;

#[cfg(feature = "simd")]
pub use mnemonic::SimdMnemonic;

pub use msr::SystemReg;
pub use operand::*;

/// Struct representing a single instruction.
#[derive(Debug)]
pub struct Instruction {
    /// Instruction mnemonic, as it appears in the assembly file.
    pub mnemonic: Mnemonic,
    /// List of instruction operands. Always contiguous, i.e., doesn't have "None" holes between
    /// two "Some" operands.
    pub operands: [Option<Operand>; 4],
}

/// Decodes an AArch64 instruction from its 32-bit word representation.
pub fn decode(insn: u32) -> Option<Instruction> {
    decode::decode_inner(insn)
}
