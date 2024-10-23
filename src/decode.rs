use bitmatch::bitmatch;

use crate::{Instruction, Mnemonic, Operand};

mod branch;
mod data_reg;
mod imm;
mod ldst;

#[bitmatch]
pub(super) fn decode_inner(insn: u32) -> Option<Instruction> {
    if insn == 0 {
        return Some(Instruction {
            mnemonic: Mnemonic::udf,
            operands: [Some(Operand::Imm(0)), None, None, None],
        });
    }

    let op0 = (insn >> 25) & 0xF;
    #[bitmatch]
    match op0 {
        // Unallocated
        "00??" => None,
        // Data processing - immediate
        "100x" => imm::decode_data_imm(x as _, insn),
        // Branches, exception generating and system instructions
        "101?" => branch::decode_branch(insn),
        // Loads and stores
        "?1?0" => ldst::decode_load_store(insn),
        // Data processing - register
        "?101" => data_reg::decode_data_reg(insn),
        // Data processing - SIMD and floating point
        "0111" => todo!(),
        // Data processing - SIMD and floating point
        "1111" => todo!(),
        _ => None,
    }
}

fn sext(value: u64, sign: usize) -> i64 {
    if sign == 64 {
        return value as i64;
    }

    if value & (1 << sign) != 0 {
        let sext = u64::MAX << sign;
        (value | sext) as i64
    } else {
        value as _
    }
}
