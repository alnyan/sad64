#![allow(non_snake_case, non_camel_case_types)]
#![feature(if_let_guard)]
#![cfg_attr(not(feature = "std"), no_std)]

mod decode;
mod mnemonic;
mod msr;
mod operand;

#[cfg(feature = "std")]
mod format;

#[cfg(feature = "std")]
pub use format::{Formatter, NoopResolver, SimpleFormatter, SymbolResolver};

pub use mnemonic::{Mnemonic, SimdMnemonic};
pub use msr::SystemReg;
pub use operand::{
    Barrier, BarrierDomain, BranchCondition, IndexMode, Operand, RegExtend, RegExtendWord, SysOp,
    Tlbi,
};

#[derive(Debug)]
pub struct Instruction {
    pub mnemonic: Mnemonic,
    pub operands: [Option<Operand>; 4],
}

pub fn decode(insn: u32) -> Option<Instruction> {
    decode::decode_inner(insn)
}
