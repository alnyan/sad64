use bitmatch::bitmatch;

use crate::{BranchCondition, Instruction, Mnemonic, Operand, RegExtend};

#[bitmatch]
pub fn decode_data_reg(insn: u32) -> Option<Instruction> {
    let op0 = (insn >> 30) & 1;
    let op1 = (insn >> 28) & 1;
    let op2 = (insn >> 21) & 0xF;
    let op3 = (insn >> 11) & 1;

    let op = op3 | (op2 << 1) | (op1 << 5) | (op0 << 6);

    #[bitmatch]
    match op {
        // Data-processing (2 source)
        "0 1 0110 ?" => decode_data_2src(insn),
        // Data-processing (1 source)
        "1 1 0110 ?" => decode_data_1src(insn),
        // Logical (shifted register)
        "? 0 0??? ?" => decode_logical_shreg(insn),
        // Add/sub (shifted/extended register)
        "? 0 1??? ?" => decode_addsub_reg(insn),
        // Add/sub (with carry)
        "? 1 0000 ?" => decode_addsub_carry(insn),
        // Conditional compare (reg)
        "? 1 0010 0" => decode_cond_cmp_reg(insn),
        // Conditional compare (imm)
        "? 1 0010 1" => decode_cond_cmp_imm(insn),
        // Conditional select
        "? 1 0100 ?" => decode_cond_select(insn),
        // Data-processing (3 source)
        "? 1 1??? ?" => decode_data_3src(insn),
        _ => None,
    }
}

#[bitmatch]
fn decode_data_2src(insn: u32) -> Option<Instruction> {
    #[bitmatch]
    let "s ? S ???????? MMMMM oooooo NNNNN DDDDD" = insn;
    assert!(S == 0, "Unallocated");
    let M = M as u8;
    let N = N as u8;
    let D = D as u8;

    let op = match s != 0 {
        false => Operand::W,
        true => Operand::X,
    };

    let Rm = op(M);
    let Rn = op(N);
    let Rd = op(D);

    #[bitmatch]
    match o {
        // udiv/sdiv
        "00001x" => {
            let mnemonic = match x != 0 {
                false => Mnemonic::udiv,
                true => Mnemonic::sdiv,
            };

            Some(Instruction {
                mnemonic,
                operands: [Some(Rd), Some(Rn), Some(Rm), None],
            })
        }
        // lslv/lsrv/asrv/rorv
        "0010xx" => {
            let mnemonic = match x {
                0b00 => Mnemonic::lslv,
                0b01 => Mnemonic::lsr,
                0b10 => Mnemonic::asrv,
                0b11 => Mnemonic::rorv,
                _ => unreachable!(),
            };

            Some(Instruction {
                mnemonic,
                operands: [Some(Rd), Some(Rn), Some(Rm), None],
            })
        }
        "010?11" => unimplemented!(),
        // crc...
        "010xxx" => todo!(),
        _ => unimplemented!(),
    }
}

#[bitmatch]
fn decode_data_1src(insn: u32) -> Option<Instruction> {
    #[bitmatch]
    let "s ? S ???????? AAAAA BBBBBB NNNNN DDDDD" = insn;

    if S != 0 || A != 0 {
        return None;
    }

    let op = match s != 0 {
        false => Operand::W,
        true => Operand::X,
    };

    let Rd = op(D as u8);
    let Rn = op(N as u8);

    let mnemonic = match (s, B) {
        // rbit
        (_, 0b000000) => Mnemonic::rbit,
        // rev16
        (_, 0b000001) => Mnemonic::rev16,
        // rev - 32-bit
        (0, 0b000010) => Mnemonic::rev,
        // rev32
        (1, 0b000010) => todo!(),
        // rev - 64-bit
        (1, 0b000011) => Mnemonic::rev,
        // clz
        (_, 0b000100) => Mnemonic::clz,
        // cls
        (_, 0b000101) => todo!(),

        _ => todo!(),
    };

    Some(Instruction {
        mnemonic,
        operands: [Some(Rd), Some(Rn), None, None],
    })
}

#[bitmatch]
fn decode_logical_shreg(insn: u32) -> Option<Instruction> {
    #[bitmatch]
    let "s oo ????? SS n MMMMM iiiiii NNNNN DDDDD" = insn;
    let M = M as u8;
    let N = N as u8;
    let D = D as u8;

    let op = match s != 0 {
        false => Operand::W,
        true => Operand::X,
    };

    let Rm = op(M);
    let Rn = op(N);
    let Rd = op(D);

    if s == 0 && i & (1 << 5) != 0 {
        todo!()
    }

    let shift = match S {
        0b00 => Operand::Lsl,
        0b01 => Operand::Lsr,
        0b10 => Operand::Asr,
        0b11 => Operand::Ror,
        _ => unreachable!(),
    };

    let shift = match i {
        0 => None,
        _ => Some(shift(i as u8)),
    };

    // Alias checks
    match (o, n, S, i, N) {
        // orr -> mov alias
        (0b01, 0, 0b00, 0, 0b11111) => Some(Instruction {
            mnemonic: Mnemonic::mov,
            operands: [Some(Rd), Some(Rm), None, None],
        }),
        // orn -> mvn alias
        (0b01, 1, 0b00, 0, 0b11111) => Some(Instruction {
            mnemonic: Mnemonic::mvn,
            operands: [Some(Rd), Some(Rm), None, None],
        }),

        _ => {
            let mnemonic = match (o, n) {
                // and
                (0b00, 0) => Mnemonic::and,
                // orr
                (0b01, 0) => Mnemonic::orr,
                // eor
                (0b10, 0) => Mnemonic::eor,
                // ands
                (0b11, 0) => Mnemonic::ands,
                // bic
                (0b00, 1) => Mnemonic::bic,
                // orn
                (0b01, 1) => Mnemonic::orn,
                // eon
                (0b10, 1) => todo!(),
                // bics
                (0b11, 1) => Mnemonic::bics,
                _ => unreachable!(),
            };

            Some(Instruction {
                mnemonic,
                operands: [Some(Rd), Some(Rn), Some(Rm), shift],
            })
        }
    }
}

#[bitmatch]
fn decode_addsub_reg(insn: u32) -> Option<Instruction> {
    // Shifted
    //     #[bitmatch]
    //     let "s o S ????? xx ? MMMMM iiiiii NNNNN DDDDD" = insn;
    //
    // Extended
    //     #[bitmatch]
    //     let "s o S ????? aa ? MMMMM bbb iii NNNNN DDDDD" = insn;

    enum Ext {
        Shift(fn(u8) -> Operand, u8),
        Extend(RegExtend),
    }

    let (o, S, N, D, op01, op2, ext) = #[bitmatch]
    match insn {
        // Shifted
        "s o S 01011 qq 0 MMMMM iiiiii NNNNN DDDDD" => {
            // ADD <Wd>, <Wn>, <Wm>{, <shift> #<amount>}
            // ADD <Xd>, <Xn>, <Xm>{, <shift> #<amount>}
            let shift = match q {
                0b00 => Operand::Lsl,
                0b01 => Operand::Lsr,
                0b10 => Operand::Asr,
                _ => unimplemented!(),
            };

            let op = match s != 0 {
                false => Operand::W,
                true => Operand::X,
            };

            (o, S, N, D, op, op(M as u8), Ext::Shift(shift, i as u8))
        }
        // Extended
        "s o S 01011 aa 1 MMMMM bbb iii NNNNN DDDDD" => {
            // ADD <Wd|WSP>, <Wn|WSP>, <Wm>{, <extend> #<amount>}
            // ADD <Xd|WSP>, <Xn|WSP>, <R><m>{, <extend> #<amount>}

            if a != 0 {
                // Unallocated
                todo!();
            }

            if i & 0b101 == 0b101 || i & 0b110 == 0b110 {
                // Unallocated
                todo!();
            }

            let extend = if (b == 0b010 || b == 0b011) && (N == 0b11111 || D == 0b11111) {
                RegExtend::lsl(i as u8)
            } else {
                RegExtend::decode(b as u8, i as u8)
            };

            let op01 = match s != 0 {
                false => Operand::WSp,
                true => Operand::XSp,
            };

            let op2 = match (s, b) {
                (0, 0b111 | 0b011) => Operand::X(M as u8),
                _ => Operand::W(M as u8),
            };

            (o, S, N, D, op01, op2, Ext::Extend(extend))
        }
        _ => unreachable!(),
    };

    let mnemonic = match (o, S) {
        (0, 0) => Mnemonic::add,
        (0, 1) => Mnemonic::adds,
        (1, 0) => Mnemonic::sub,
        (1, 1) => Mnemonic::subs,
        _ => unreachable!(),
    };

    let Rn = op01(N as u8);
    let Rd = op01(D as u8);

    let op3 = match ext {
        Ext::Shift(_, 0) => None,
        Ext::Shift(shift, amount) => Some(shift(amount)),
        Ext::Extend(extend) => Some(Operand::RegExtend(extend)),
    };

    match (mnemonic, ext) {
        // adds -> cmn (extended/shifted)
        (Mnemonic::adds, _) if D == 0b11111 => todo!(),
        // sub (shifted) -> neg (shifted)
        (Mnemonic::sub, Ext::Shift(..)) if N == 0b11111 => todo!(),
        // subs -> cmp (extended/shifted)
        (Mnemonic::subs, _) if D == 0b11111 => Some(Instruction {
            mnemonic: Mnemonic::cmp,
            operands: [Some(Rn), Some(op2), op3, None],
        }),
        // subs (shifted) -> negs
        (Mnemonic::subs, Ext::Shift(..)) if N == 0b11111 => todo!(),

        _ => Some(Instruction {
            mnemonic,
            operands: [Some(Rd), Some(Rn), Some(op2), op3],
        }),
    }
}

#[bitmatch]
fn decode_addsub_carry(insn: u32) -> Option<Instruction> {
    #[bitmatch]
    let "s A S ???????? MMMMM BBBBBB NNNNN DDDDD" = insn;

    if B != 0 {
        return None;
    }

    let mnemonic = match (A, S) {
        (0, 0) => Mnemonic::adc,
        (0, 1) => Mnemonic::adcs,
        (1, 0) => Mnemonic::sbc,
        (1, 1) => Mnemonic::sbcs,
        _ => unreachable!(),
    };
    let op = match s != 0 {
        false => Operand::W,
        true => Operand::X,
    };
    let Rd = op(D as u8);
    let Rn = op(N as u8);
    let Rm = op(M as u8);

    Some(Instruction {
        mnemonic,
        operands: [Some(Rd), Some(Rn), Some(Rm), None],
    })
}

#[bitmatch]
fn decode_cond_cmp_reg(insn: u32) -> Option<Instruction> {
    #[bitmatch]
    let "s A S ???????? MMMMM cccc ? B NNNNN C ffff" = insn;

    if B != 0 || C != 0 || S != 1 {
        return None;
    }

    let mnemonic = match A != 0 {
        false => Mnemonic::ccmn,
        true => Mnemonic::ccmp,
    };
    let op = match s != 0 {
        false => Operand::W,
        true => Operand::X,
    };

    let Rn = op(N as u8);
    let Rm = op(M as u8);
    let cond = BranchCondition::try_from(c as u8).unwrap();

    Some(Instruction {
        mnemonic,
        operands: [
            Some(Rn),
            Some(Rm),
            Some(Operand::Imm(f as _)),
            Some(Operand::Cond(cond)),
        ],
    })
}

#[bitmatch]
fn decode_cond_cmp_imm(insn: u32) -> Option<Instruction> {
    #[bitmatch]
    let "s A S ???????? iiiii cccc ? B NNNNN C ffff" = insn;

    if B != 0 || C != 0 || S != 1 {
        return None;
    }

    let mnemonic = match A != 0 {
        false => Mnemonic::ccmn,
        true => Mnemonic::ccmp,
    };
    let op = match s != 0 {
        false => Operand::W,
        true => Operand::X,
    };

    let Rn = op(N as u8);
    let cond = BranchCondition::try_from(c as u8).unwrap();

    Some(Instruction {
        mnemonic,
        operands: [
            Some(Rn),
            Some(Operand::Imm(i as _)),
            Some(Operand::Imm(f as _)),
            Some(Operand::Cond(cond)),
        ],
    })
}

#[bitmatch]
fn decode_cond_select(insn: u32) -> Option<Instruction> {
    #[bitmatch]
    match insn {
        "s o 0 11010100 MMMMM cccc oo NNNNN DDDDD" => {
            let mnemonic = match o {
                0b000 => Mnemonic::csel,
                0b001 => Mnemonic::csinc,
                0b100 => Mnemonic::csinv,
                0b101 => Mnemonic::csneg,
                _ => unimplemented!(),
            };
            let op = match s != 0 {
                false => Operand::W,
                true => Operand::X,
            };
            let Rd = op(D as u8);
            let Rn = op(N as u8);
            let Rm = op(M as u8);

            match (mnemonic, N, M, c) {
                // csinc -> cinc
                (Mnemonic::csinc, _, _, _)
                    if c & 0b1110 != 0b1110 && N != 31 && M != 31 && N == M =>
                {
                    let cond = BranchCondition::try_from((c ^ 1) as u8).unwrap();
                    Some(Instruction {
                        mnemonic: Mnemonic::cinc,
                        operands: [Some(Rd), Some(Rn), Some(Operand::Cond(cond)), None],
                    })
                }
                // csinc -> cset
                (Mnemonic::csinc, _, _, _) if N == 31 && M == 31 && c & 0b1110 != 0b1110 => {
                    let cond = BranchCondition::try_from((c ^ 1) as u8).unwrap();
                    Some(Instruction {
                        mnemonic: Mnemonic::cset,
                        operands: [Some(Rd), Some(Operand::Cond(cond)), None, None],
                    })
                }
                _ => {
                    let cond = BranchCondition::try_from(c as u8).unwrap();

                    Some(Instruction {
                        mnemonic,
                        operands: [Some(Rd), Some(Rn), Some(Rm), Some(Operand::Cond(cond))],
                    })
                }
            }
        }
        _ => unimplemented!(),
    }
}

#[bitmatch]
fn decode_data_3src(insn: u32) -> Option<Instruction> {
    #[bitmatch]
    let "s aa ????? bbb MMMMM c AAAAA NNNNN DDDDD" = insn;
    let A = A as u8;
    let N = N as u8;
    let D = D as u8;
    let M = M as u8;

    let op = match s != 0 {
        false => Operand::W,
        true => Operand::X,
    };

    let Rd = op(D);

    match (s, a, b, c) {
        // madd/msub
        (_, 0b00, 0b000, _) => {
            let Rn = op(N);
            let Rm = op(M);
            let Ra = op(A);

            match (A, c) {
                // mul
                (0b11111, 0) => Some(Instruction {
                    mnemonic: Mnemonic::mul,
                    operands: [Some(Rd), Some(Rn), Some(Rm), None],
                }),
                // mneg
                (0b11111, 1) => Some(Instruction {
                    mnemonic: Mnemonic::mneg,
                    operands: [Some(Rd), Some(Rn), Some(Rm), None],
                }),
                _ => {
                    let mnemonic = match c != 0 {
                        false => Mnemonic::madd,
                        true => Mnemonic::msub,
                    };
                    Some(Instruction {
                        mnemonic,
                        operands: [Some(Rd), Some(Rn), Some(Rm), Some(Ra)],
                    })
                }
            }
        }
        // smaddl/smsubl
        (1, 0b00, 0b001, _) => {
            let Rn = Operand::W(N);
            let Rm = Operand::W(M);
            let Ra = Operand::X(A);

            match (A, c) {
                // smaddl -> smull
                (0b11111, 0) => Some(Instruction {
                    mnemonic: Mnemonic::smull,
                    operands: [Some(Rd), Some(Rn), Some(Rm), None],
                }),
                // smsubl -> smnegl
                (0b11111, 1) => todo!(),
                _ => {
                    let mnemonic = match c != 0 {
                        false => Mnemonic::smaddl,
                        true => Mnemonic::smsubl,
                    };
                    Some(Instruction {
                        mnemonic,
                        operands: [Some(Rd), Some(Rn), Some(Rm), Some(Ra)],
                    })
                }
            }
        }
        // umaddl/umsubl
        (1, 0b00, 0b101, _) => {
            let Rn = Operand::W(N);
            let Rm = Operand::W(M);
            let Ra = Operand::X(A);

            match (A, c) {
                // umaddl -> umull
                (0b11111, 0) => Some(Instruction {
                    mnemonic: Mnemonic::umul,
                    operands: [Some(Rd), Some(Rn), Some(Rm), None],
                }),
                // umsubl -> umnegl
                (0b11111, 1) => todo!(),
                _ => {
                    let mnemonic = match c != 0 {
                        false => Mnemonic::umaddl,
                        true => Mnemonic::umsubl,
                    };
                    Some(Instruction {
                        mnemonic,
                        operands: [Some(Rd), Some(Rn), Some(Rm), Some(Ra)],
                    })
                }
            }
        }
        // smulh
        (1, 0b00, 0b010, 0) => {
            let Rn = Operand::X(N);
            let Rm = Operand::X(M);

            Some(Instruction {
                mnemonic: Mnemonic::smulh,
                operands: [Some(Rd), Some(Rn), Some(Rm), None],
            })
        }
        // umulh
        (1, 0b00, 0b110, 0) => {
            let Rn = Operand::X(N);
            let Rm = Operand::X(M);

            Some(Instruction {
                mnemonic: Mnemonic::umulh,
                operands: [Some(Rd), Some(Rn), Some(Rm), None],
            })
        }
        _ => unimplemented!(),
    }
}
