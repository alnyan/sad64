use bitmatch::bitmatch;

use crate::{
    msr::{DecodeDirection, SystemReg},
    Barrier, BarrierDomain, BranchCondition, Instruction, Mnemonic, Operand, SysOp, Tlbi,
};

use super::sext;

#[bitmatch]
pub fn decode_branch(insn: u32) -> Option<Instruction> {
    let op0 = insn >> 29;
    let op1 = (insn >> 22) & 0xF;
    let op01 = (op0 << 4) | op1;

    #[bitmatch]
    match op01 {
        // Conditional branch (imm)
        "010_0???" => decode_cond_br_imm(insn),
        // Exception generation
        "110_00??" => decode_exception(insn),
        // System
        "110_0100" => decode_system(insn),
        // Unconditional branch (reg)
        "110_1???" => decode_br_reg(insn),
        // Unconditional branch (imm)
        "?00_????" => decode_br_imm(insn),
        // Compare & branch (imm)
        "?01_0???" => decode_cmp_br_imm(insn),
        // Test & branch (imm)
        "?01_1???" => decode_test_br_imm(insn),
        _ => todo!(),
    }
}

#[bitmatch]
fn decode_cond_br_imm(insn: u32) -> Option<Instruction> {
    #[bitmatch]
    let "??????? a iiiiiiiiiiiiiiiiiii a cccc" = insn;
    if a != 0 {
        todo!("Unallocated");
    }
    // B.cond
    let cond = BranchCondition::try_from(c as u8).unwrap();

    let mnemonic = Mnemonic::CondB(cond);
    let imm = sext((i as u64) << 2, 20);

    Some(Instruction {
        mnemonic,
        operands: [Some(Operand::PcRelImm(imm)), None, None, None],
    })
}

#[bitmatch]
fn decode_exception(insn: u32) -> Option<Instruction> {
    #[bitmatch]
    let "???????? aaa iiiiiiiiiiiiiiii bbb LL" = insn;

    match (a, b, L) {
        // brk #imm
        (0b001, 0b000, 0b00) => Some(Instruction {
            mnemonic: Mnemonic::brk,
            operands: [Some(Operand::Imm(i as _)), None, None, None],
        }),
        // svc #imm
        (0b000, 0b000, 0b01) => Some(Instruction {
            mnemonic: Mnemonic::svc,
            operands: [Some(Operand::Imm(i as _)), None, None, None],
        }),
        // hvc #imm
        (0b000, 0b000, 0b10) => Some(Instruction {
            mnemonic: Mnemonic::hvc,
            operands: [Some(Operand::Imm(i as _)), None, None, None],
        }),
        // smc #imm
        (0b000, 0b000, 0b11) => Some(Instruction {
            mnemonic: Mnemonic::smc,
            operands: [Some(Operand::Imm(i as _)), None, None, None],
        }),
        _ => todo!(),
    }
}

#[bitmatch]
fn decode_system(insn: u32) -> Option<Instruction> {
    #[bitmatch]
    let "?????????? L aa bbb NNNN MMMM ccc TTTTT" = insn;
    // a - op0
    // b - op1
    // c - op2

    match (L, a, b, N, M, c, T) {
        // hint - hints 8 to 127
        (0, 0b00, 0b011, 0b0010, _, _, 0b11111) if M != 0 => {
            todo!();
        }
        // nop
        (0, 0b00, 0b011, 0b0010, 0b0000, 0b000, 0b11111) => Some(Instruction {
            mnemonic: Mnemonic::nop,
            operands: [None, None, None, None],
        }),
        // yield
        (0, 0b00, 0b011, 0b0010, 0b0000, 0b001, 0b11111) => {
            todo!();
        }
        // wfe
        (0, 0b00, 0b011, 0b0010, 0b0000, 0b010, 0b11111) => Some(Instruction {
            mnemonic: Mnemonic::wfe,
            operands: [None, None, None, None],
        }),
        // wfi
        (0, 0b00, 0b011, 0b0010, 0b0000, 0b011, 0b11111) => Some(Instruction {
            mnemonic: Mnemonic::wfi,
            operands: [None, None, None, None],
        }),
        // sev
        (0, 0b00, 0b011, 0b0010, 0b0000, 0b100, 0b11111) => Some(Instruction {
            mnemonic: Mnemonic::sev,
            operands: [None, None, None, None],
        }),
        // sevl
        (0, 0b00, 0b011, 0b0010, 0b0000, 0b101, 0b1111) => {
            todo!();
        }
        // hint - hints 6/7
        (0, 0b00, 0b011, 0b0010, 0b0000, 0b110 | 0b111, 0b11111) => {
            todo!();
        }
        // clrex
        (0, 0b00, 0b011, 0b0011, _, 0b010, 0b11111) => {
            let imm = match M {
                0b1111 => None,
                _ => Some(Operand::Imm(M as _)),
            };
            Some(Instruction {
                mnemonic: Mnemonic::clrex,
                operands: [imm, None, None, None],
            })
        }
        // dsb/dmb
        (0, 0b00, 0b011, 0b0011, _, 0b100 | 0b101, 0b11111) => {
            let mnemonic = match c & 1 != 0 {
                false => Mnemonic::dsb,
                true => Mnemonic::dmb,
            };
            let domain = match M >> 2 {
                0b00 => BarrierDomain::OuterShareable,
                0b01 => BarrierDomain::Nonshareable,
                0b10 => BarrierDomain::InnerShareable,
                0b11 => BarrierDomain::FullSystem,
                _ => unreachable!(),
            };
            let barrier = match M & 0x3 {
                0b01 => Barrier::Reads(domain),
                0b10 => Barrier::Writes(domain),
                0b11 => Barrier::All(domain),
                _ => Barrier::All(BarrierDomain::FullSystem),
            };

            Some(Instruction {
                mnemonic,
                operands: [Some(Operand::Barrier(barrier)), None, None, None],
            })
        }
        // isb
        (0, 0b00, 0b011, 0b0011, _, 0b110, 0b11111) => Some(Instruction {
            mnemonic: Mnemonic::isb,
            operands: [None, None, None, None],
        }),
        // sys
        (0, 0b01, _, _, _, _, _) => {
            // SYS #<op1>, <Cn>, <Cm>, #<op2>{, <Xt>}

            match (N, M) {
                // sys -> at
                (0b0111, 0b1000)
                    if let Some(SysOp::At) = sys_op(b as u8, 0b0111, 0b1000, c as u8) =>
                {
                    todo!()
                }
                // sys -> dc
                (0b0111, _) if let Some(SysOp::Dc) = sys_op(b as u8, 0b0111, M as u8, c as u8) => {
                    todo!()
                }
                // sys -> ic
                (0b0111, _) if let Some(SysOp::Ic) = sys_op(b as u8, 0b0111, M as u8, c as u8) => {
                    todo!()
                }
                // sys -> tlbi
                (0b1000, _)
                    if let Some(SysOp::Tlbi(tlbi)) = sys_op(b as u8, 0b1000, M as u8, c as u8) =>
                {
                    Some(Instruction {
                        mnemonic: Mnemonic::tlbi,
                        operands: [
                            Some(Operand::Tlbi(tlbi)),
                            Some(Operand::X(T as u8)),
                            None,
                            None,
                        ],
                    })
                }
                _ => todo!(),
            }
        }
        // sysl
        (1, 0b01, _, _, _, _, _) => {
            // SYSL <Xt>, #<op1>, <Cn>, <Cm>, #<op2>
            todo!();
        }
        // msr/mrs
        (_, 0b10 | 0b11, _, _, _, _, _) => {
            let (mnemonic, direction) = match L != 0 {
                false => (Mnemonic::msr, DecodeDirection::Msr),
                true => (Mnemonic::mrs, DecodeDirection::Mrs),
            };
            let system_reg = SystemReg::decode(direction, a as _, b as _, N as _, M as _, c as _);
            let Rt = Operand::X(T as u8);

            let operands = match direction {
                DecodeDirection::Msr => [Some(Operand::Sys(system_reg)), Some(Rt), None, None],
                DecodeDirection::Mrs => [Some(Rt), Some(Operand::Sys(system_reg)), None, None],
            };

            Some(Instruction { mnemonic, operands })
        }

        _ => todo!(),
    }
}

#[bitmatch]
fn decode_br_reg(insn: u32) -> Option<Instruction> {
    #[bitmatch]
    let "??????? oooo aaaaa bbbbbb NNNNN ccccc" = insn;
    let N = N as u8;

    match (o, a, b, N, c) {
        // br/blr reg
        (0b0000 | 0b0001, 0b11111, 0b000000, _, 0b00000) => {
            let mnemonic = match o & 1 != 0 {
                false => Mnemonic::br,
                true => Mnemonic::blr,
            };

            Some(Instruction {
                mnemonic,
                operands: [Some(Operand::X(N)), None, None, None],
            })
        }
        // ret
        (0b0010, 0b11111, 0b000000, _, 0b00000) => {
            let reg = if N == 30 {
                None // Defaults to x30/lr
            } else {
                Some(Operand::X(N))
            };

            Some(Instruction {
                mnemonic: Mnemonic::ret,
                operands: [reg, None, None, None],
            })
        }

        // eret
        (0b0100, 0b11111, 0b000000, 0b11111, 0b00000) => Some(Instruction {
            mnemonic: Mnemonic::eret,
            operands: [None, None, None, None],
        }),
        // drps
        (0b0101, 0b11111, 0b000000, 0b11111, 0b00000) => todo!(),

        _ => unimplemented!(),
    }
}

fn decode_br_imm(insn: u32) -> Option<Instruction> {
    let mnemonic = match insn & (1 << 31) != 0 {
        false => Mnemonic::b,
        true => Mnemonic::bl,
    };
    let imm26 = insn & 0x3FFFFFF;

    Some(Instruction {
        mnemonic,
        operands: [
            Some(Operand::PcRelImm(sext(imm26 as u64, 25) * 4)),
            None,
            None,
            None,
        ],
    })
}

#[bitmatch]
fn decode_cmp_br_imm(insn: u32) -> Option<Instruction> {
    #[bitmatch]
    let "s ?????? o iiiiiiiiiiiiiiiiiii TTTTT" = insn;
    let T = T as u8;

    let mnemonic = match o != 0 {
        false => Mnemonic::cbz,
        true => Mnemonic::cbnz,
    };

    let op = match s != 0 {
        false => Operand::W,
        true => Operand::X,
    };

    let Rt = op(T);
    let imm = sext((i << 2) as u64, 20);

    Some(Instruction {
        mnemonic,
        operands: [Some(Rt), Some(Operand::PcRelImm(imm)), None, None],
    })
}

#[bitmatch]
fn decode_test_br_imm(insn: u32) -> Option<Instruction> {
    #[bitmatch]
    let "a ?????? o bbbbb iiiiiiiiiiiiii TTTTT" = insn;
    let mnemonic = match o != 0 {
        false => Mnemonic::tbz,
        true => Mnemonic::tbnz,
    };
    let op = match a != 0 {
        false => Operand::W,
        true => Operand::X,
    };
    let bit_pos = b | (a << 5);
    let imm = sext((i as u64) << 2, 15);

    let Rt = op(T as u8);

    Some(Instruction {
        mnemonic,
        operands: [
            Some(Rt),
            Some(Operand::Imm(bit_pos as _)),
            Some(Operand::PcRelImm(imm)),
            None,
        ],
    })
}

fn sys_op(op1: u8, crn: u8, crm: u8, op2: u8) -> Option<SysOp> {
    match (crn, crm) {
        // at
        (0b0111, 0b1000) => {
            todo!();
        }
        // ic/dc
        (0b0111, _) => todo!(),
        // tlbi
        (0b1000, _) => {
            let variant = match (op1, crm, op2) {
                (0b100, 0b0000, 0b001) => Tlbi::ipas2e1is,
                (0b100, 0b0000, 0b101) => Tlbi::ipas2le1is,
                (0b000, 0b0011, 0b000) => Tlbi::vmalle1is,
                (0b100, 0b0011, 0b000) => Tlbi::alle2is,
                (0b110, 0b0011, 0b000) => Tlbi::alle3is,
                (0b000, 0b0011, 0b010) => Tlbi::aside1is,
                (0b000, 0b0011, 0b011) => Tlbi::vaae1is,
                (0b100, 0b0011, 0b100) => Tlbi::alle1is,
                (0b000, 0b0011, 0b101) => Tlbi::vale1is,
                (0b100, 0b0011, 0b101) => Tlbi::vale2is,
                (0b110, 0b0011, 0b101) => Tlbi::vale3is,
                (0b100, 0b0011, 0b110) => Tlbi::vmalls12e1is,
                (0b000, 0b0011, 0b111) => Tlbi::vaale1is,
                (0b100, 0b0100, 0b001) => Tlbi::ipas2e1,
                (0b100, 0b0100, 0b101) => Tlbi::ipas2le1,
                (0b000, 0b0111, 0b000) => Tlbi::vmalle1,
                (0b100, 0b0111, 0b000) => Tlbi::alle2,
                (0b110, 0b0111, 0b000) => Tlbi::alle3,
                (0b000, 0b0111, 0b001) => Tlbi::vae1,
                (0b100, 0b0111, 0b001) => Tlbi::vae2,
                (0b110, 0b0111, 0b001) => Tlbi::vae3,
                (0b000, 0b0111, 0b010) => Tlbi::aside1,
                (0b000, 0b0111, 0b011) => Tlbi::vaae1,
                (0b100, 0b0111, 0b100) => Tlbi::alle1,
                (0b000, 0b0111, 0b101) => Tlbi::vale1,
                (0b100, 0b0111, 0b101) => Tlbi::vale2,
                (0b110, 0b0111, 0b101) => Tlbi::vale3,
                (0b100, 0b0111, 0b110) => Tlbi::vmalls12e1,
                (0b000, 0b0111, 0b111) => Tlbi::vaale1,
                _ => return None,
            };

            Some(SysOp::Tlbi(variant))
        }
        _ => todo!(),
    }
}