use bitmatch::bitmatch;

use crate::{Instruction, Mnemonic, Operand};

use super::sext;

#[bitmatch]
pub fn decode_data_imm(_x: u8, insn: u32) -> Option<Instruction> {
    #[bitmatch]
    let "??? ??? ooo ???????????????????????" = insn;

    match o {
        // PC-rel addressing
        0b000 | 0b001 => decode_pcrel_addr(insn),
        // add/sub immediate
        0b010 | 0b011 => decode_add_sub_imm(insn),
        // logical immediate
        0b100 => decode_logical_imm(insn),
        // move wide immediate
        0b101 => decode_wmov_imm(insn),
        // bit field
        0b110 => decode_bit_field(insn),
        // extract
        0b111 => decode_extract(insn),
        _ => None,
    }
}

#[bitmatch]
fn decode_pcrel_addr(insn: u32) -> Option<Instruction> {
    #[bitmatch]
    let "o ii ????? IIIIIIIIIIIIIIIIIII DDDDD" = insn;
    let imm = ((I << 2) | i) as u64;
    let (mnemonic, imm) = match o != 0 {
        false => (Mnemonic::adr, Operand::PcRelImm(sext(imm, 20))),
        true => (Mnemonic::adrp, Operand::Adrp(sext(imm << 12, 32))),
    };
    let Rd = Operand::X(D as u8);

    Some(Instruction {
        mnemonic,
        operands: [Some(Rd), Some(imm), None, None],
    })
}

#[bitmatch]
fn decode_add_sub_imm(insn: u32) -> Option<Instruction> {
    #[bitmatch]
    let "s o S ????? qq iiiiiiiiiiii NNNNN DDDDD" = insn;

    let shift = match q {
        0b00 => None,
        0b01 => Some(Operand::Lsl(12)),
        _ => return None,
    };

    let mnemonic = match (o, S) {
        (0, 0) => Mnemonic::add,
        (0, 1) => Mnemonic::adds,
        (1, 0) => Mnemonic::sub,
        (1, 1) => Mnemonic::subs,
        _ => unreachable!(),
    };
    let op0 = match (S != 0, s != 0) {
        (false, false) => Operand::WSp,
        (false, true) => Operand::XSp,
        (true, false) => Operand::W,
        (true, true) => Operand::X,
    };
    let op1 = match s != 0 {
        false => Operand::WSp,
        true => Operand::XSp,
    };

    let Rd = op0(D as u8);
    let Rn = op1(N as u8);
    let imm = i as u64;

    match (mnemonic, q, i) {
        // add -> mov (to/from SP) alias
        (Mnemonic::add, 0, 0) if N == 0b11111 || D == 0b11111 => Some(Instruction {
            mnemonic: Mnemonic::mov,
            operands: [Some(Rd), Some(Rn), None, None],
        }),
        // adds -> cmn (immediate)
        (Mnemonic::adds, _, _) if D == 0b11111 => Some(Instruction {
            mnemonic: Mnemonic::cmn,
            operands: [Some(Rn), Some(Operand::Imm(imm)), shift, None],
        }),
        // subs -> cmp (immediate)
        (Mnemonic::subs, _, _) if D == 0b11111 => Some(Instruction {
            mnemonic: Mnemonic::cmp,
            operands: [Some(Rn), Some(Operand::Imm(imm)), shift, None],
        }),

        _ => Some(Instruction {
            mnemonic,
            operands: [Some(Rd), Some(Rn), Some(Operand::Imm(imm)), shift],
        }),
    }
}

#[bitmatch]
fn decode_wmov_imm(insn: u32) -> Option<Instruction> {
    #[bitmatch]
    let "s oo ?????? ww iiiiiiiiiiiiiiii DDDDD" = insn;

    let mnemonic = match o {
        0b00 => Mnemonic::movn,
        0b10 => Mnemonic::movz,
        0b11 => Mnemonic::movk,
        _ => return None,
    };
    let op = match s != 0 {
        false => Operand::W,
        true => Operand::X,
    };
    let shift = match w {
        0 => None,
        _ => Some(Operand::Lsl(w as u8 * 16)),
    };
    let Rd = op(D as u8);

    match (s, mnemonic) {
        // movn -> mov (inverted wide imm)
        (0, Mnemonic::movn) if !(i == 0 && w != 0) && (i as u16 != u16::MAX) => {
            let val = !((i as u64) << (w * 16));
            let val = sext(val, (w as usize + 1) * 16);
            Some(Instruction {
                mnemonic: Mnemonic::mov,
                operands: [Some(Rd), Some(Operand::Simm(val)), None, None],
            })
        }
        (1, Mnemonic::movn) if !(i == 0 && w != 0) => {
            let val = !((i as u64) << (w * 16));
            let val = sext(val, (w as usize + 1) * 16);
            Some(Instruction {
                mnemonic: Mnemonic::mov,
                operands: [Some(Rd), Some(Operand::Simm(val)), None, None],
            })
        }
        // movz -> mov (wide imm)
        (_, Mnemonic::movz) if !(i == 0 && w != 0) => {
            let val = (i as u64) << (w * 16);
            let val = sext(val, (w as usize + 1) * 16);
            Some(Instruction {
                mnemonic: Mnemonic::mov,
                operands: [Some(Rd), Some(Operand::Simm(val)), None, None],
            })
        }
        _ => Some(Instruction {
            mnemonic,
            operands: [Some(Rd), Some(Operand::Imm(i as u64)), shift, None],
        }),
    }
}

#[bitmatch]
fn decode_logical_imm(insn: u32) -> Option<Instruction> {
    #[bitmatch]
    let "s oo ?????? n RRRRRR SSSSSS NNNNN DDDDD" = insn;

    if s == 0 && n == 1 {
        // Unallocated
        return None;
    }

    let mnemonic = match o {
        0b00 => Mnemonic::and,
        0b01 => Mnemonic::orr,
        0b10 => Mnemonic::eor,
        0b11 => Mnemonic::ands,
        _ => unreachable!(),
    };
    let op = match s != 0 {
        false => Operand::WSp,
        true => Operand::XSp,
    };

    let Rn = op(N as u8);
    let Rd = op(D as u8);

    let mask = decode_logical_imm_bitmask(n != 0, S as _, R as _, if s != 0 { 8 } else { 4 })?;

    match mnemonic {
        Mnemonic::orr if N == 0b11111 && !wmov_preferred(s != 0, n != 0, S, R) => {
            Some(Instruction {
                mnemonic: Mnemonic::mov,
                operands: [Some(Rd), Some(Operand::Imm(mask)), None, None],
            })
        }

        // ands -> tst
        Mnemonic::ands if D == 0b11111 => Some(Instruction {
            mnemonic: Mnemonic::tst,
            operands: [Some(Rn), Some(Operand::Imm(mask)), None, None],
        }),

        _ => Some(Instruction {
            mnemonic,
            operands: [Some(Rd), Some(Rn), Some(Operand::Imm(mask)), None],
        }),
    }
}

#[bitmatch]
fn decode_bit_field(insn: u32) -> Option<Instruction> {
    #[bitmatch]
    let "s oo ?????? n RRRRRR SSSSSS NNNNN DDDDD" = insn;

    if s != n || o == 0b11 {
        return None;
    }

    let mnemonic = match o {
        0b00 => Mnemonic::sbfm,
        0b01 => Mnemonic::bfm,
        0b10 => Mnemonic::ubfm,
        _ => unreachable!(),
    };
    let (scale, op): (_, fn(_) -> _) = match s != 0 {
        false => (32, Operand::W),
        true => (64, Operand::X),
    };

    let Rn = op(N as u8);
    let Rd = op(D as u8);

    match (mnemonic, s, R, S) {
        // BFM aliases
        // bfi
        (Mnemonic::bfm, _, _, _) if N != 0b11111 && S < R => {
            let lsb = (!R + 1) % scale;
            let width = S + 1;

            Some(Instruction {
                mnemonic: Mnemonic::bfi,
                operands: [
                    Some(Rd),
                    Some(Rn),
                    Some(Operand::Imm(lsb as _)),
                    Some(Operand::Imm(width as _)),
                ],
            })
        }
        // bfxil
        (Mnemonic::bfm, _, _, _) if S >= R => {
            let lsb = R;
            let width = S - lsb + 1;

            Some(Instruction {
                mnemonic: Mnemonic::bfxil,
                operands: [
                    Some(Rd),
                    Some(Rn),
                    Some(Operand::Imm(lsb as _)),
                    Some(Operand::Imm(width as _)),
                ],
            })
        }

        // SBFM aliases
        // asr (imm)
        (Mnemonic::sbfm, 0, _, 0b011111) | (Mnemonic::sbfm, 1, _, 0b111111) => Some(Instruction {
            mnemonic: Mnemonic::asr,
            operands: [Some(Rd), Some(Rn), Some(Operand::Imm(R as _)), None],
        }),
        // sxtb
        (Mnemonic::sbfm, _, 0b000000, 0b000111) => Some(Instruction {
            mnemonic: Mnemonic::sxtb,
            operands: [Some(Rd), Some(Operand::W(N as u8)), None, None],
        }),
        // sxth
        (Mnemonic::sbfm, _, 0b000000, 0b001111) => Some(Instruction {
            mnemonic: Mnemonic::sxth,
            operands: [Some(Rd), Some(Operand::W(N as u8)), None, None],
        }),
        // sxtw
        (Mnemonic::sbfm, _, 0b000000, 0b011111) => Some(Instruction {
            mnemonic: Mnemonic::sxtw,
            operands: [
                Some(Operand::X(D as u8)),
                Some(Operand::W(N as u8)),
                None,
                None,
            ],
        }),
        // sbfx
        (Mnemonic::sbfm, _, _, _) if bfx_preferred(s != 0, o >> 1 != 0, S as _, R as _) => {
            let lsb = R;
            let width = S - R + 1;

            Some(Instruction {
                mnemonic: Mnemonic::sbfx,
                operands: [
                    Some(Rd),
                    Some(Rn),
                    Some(Operand::Imm(lsb as _)),
                    Some(Operand::Imm(width as _)),
                ],
            })
        }
        // sbfiz
        (Mnemonic::sbfm, _, _, _) if S < R => {
            let lsb = (!R + 1) % scale;
            let width = S + 1;

            Some(Instruction {
                mnemonic: Mnemonic::sbfiz,
                operands: [
                    Some(Rd),
                    Some(Rn),
                    Some(Operand::Imm(lsb as _)),
                    Some(Operand::Imm(width as _)),
                ],
            })
        }

        // UBFM aliases
        // lsl (imm)
        (Mnemonic::ubfm, _, _, _) if S != (s << 5) | 0b11111 && S + 1 == R => {
            let shift = scale - (S + 1);
            Some(Instruction {
                mnemonic: Mnemonic::lsl,
                operands: [Some(Rd), Some(Rn), Some(Operand::Imm(shift as _)), None],
            })
        }
        // lsr (imm)
        (Mnemonic::ubfm, 0, _, 0b011111) | (Mnemonic::ubfm, 1, _, 0b111111) => Some(Instruction {
            mnemonic: Mnemonic::lsr,
            operands: [Some(Rd), Some(Rn), Some(Operand::Imm(R as _)), None],
        }),
        // ubfiz
        (Mnemonic::ubfm, _, _, _) if S < R => {
            let lsb = (!R + 1) % scale;
            let width = S + 1;

            Some(Instruction {
                mnemonic: Mnemonic::ubfiz,
                operands: [
                    Some(Rd),
                    Some(Rn),
                    Some(Operand::Imm(lsb as _)),
                    Some(Operand::Imm(width as _)),
                ],
            })
        }
        // ubfx
        (Mnemonic::ubfm, _, _, _) if bfx_preferred(s != 0, o >> 1 != 0, S as _, R as _) => {
            let lsb = R;
            let width = S - R + 1;

            Some(Instruction {
                mnemonic: Mnemonic::ubfx,
                operands: [
                    Some(Rd),
                    Some(Rn),
                    Some(Operand::Imm(lsb as _)),
                    Some(Operand::Imm(width as _)),
                ],
            })
        }
        // uxtb
        (Mnemonic::ubfm, _, 0b000000, 0b000111) => Some(Instruction {
            mnemonic: Mnemonic::uxtb,
            operands: [Some(Rd), Some(Operand::W(N as u8)), None, None],
        }),
        // uxth
        (Mnemonic::ubfm, _, 0b000000, 0b001111) => Some(Instruction {
            mnemonic: Mnemonic::uxth,
            operands: [Some(Rd), Some(Operand::W(N as u8)), None, None],
        }),

        _ => Some(Instruction {
            mnemonic,
            operands: [
                Some(Rd),
                Some(Rn),
                Some(Operand::Imm(R as _)),
                Some(Operand::Imm(S as _)),
            ],
        }),
    }
}

#[bitmatch]
fn decode_extract(insn: u32) -> Option<Instruction> {
    #[bitmatch]
    let "s AA ?????? n B MMMMM SSSSSS NNNNN DDDDD" = insn;

    if A != 0 || B != 0 || s != n {
        return None;
    }
    if s == 0 && S & 0b100000 != 0 {
        return None;
    }
    let op = match s != 0 {
        false => Operand::W,
        true => Operand::X,
    };

    let Rd = op(D as u8);
    let Rn = op(N as u8);
    let Rm = op(M as u8);

    if N == M {
        Some(Instruction {
            mnemonic: Mnemonic::ror,
            operands: [Some(Rd), Some(Rn), Some(Operand::Imm(S as _)), None],
        })
    } else {
        Some(Instruction {
            mnemonic: Mnemonic::extr,
            operands: [Some(Rd), Some(Rn), Some(Rm), Some(Operand::Imm(S as _))],
        })
    }
}

// TODO I don't understand how this crap works
fn decode_logical_imm_bitmask(n: bool, mut imms: u32, mut immr: u32, reg_size: u32) -> Option<u64> {
    let n = n as u32;
    let mut imm;
    let mask;
    let bit_count: u32;

    if n != 0 {
        bit_count = 64;
        mask = !0;
    } else {
        bit_count = match imms {
            0x00..=0x1f => 32,
            0x20..=0x2f => {
                imms &= 0xf;
                16
            }
            0x30..=0x37 => {
                imms &= 0x7;
                8
            }
            0x38..=0x3b => {
                imms &= 0x3;
                4
            }
            0x3c..=0x3d => {
                imms &= 0x1;
                2
            }
            _ => return None,
        };
        mask = (1u64 << bit_count) - 1;
        immr &= bit_count - 1;
    }

    if bit_count > reg_size * 8 {
        return None;
    }

    if imms == bit_count - 1 {
        return None;
    }

    imm = (1u64 << (imms + 1)) - 1;
    if immr != 0 {
        imm = ((imm << (bit_count - immr)) & mask) | (imm >> immr);
    }

    let replicate: &[u64] = match bit_count {
        2 => &[2, 4, 8, 16, 32],
        4 => &[4, 8, 16, 32],
        8 => &[8, 16, 32],
        16 => &[16, 32],
        32 => &[32],
        64 => &[],
        _ => return None,
    };
    for &r in replicate {
        imm |= imm << r;
    }

    let limm = !0 << (reg_size * 4) << (reg_size * 4);
    let limm = imm & !limm;
    Some(limm)
}

fn wmov_preferred(sf: bool, n: bool, imms: u32, immr: u32) -> bool {
    let width = match sf {
        false => 32,
        true => 64,
    };

    if sf && !n {
        return false;
    }
    if !sf && (n || imms >> 5 != 0) {
        return false;
    }

    if imms < 16 {
        return (-(immr as i32) % 16) <= (15 - (imms as i32));
    }

    if imms >= width - 15 {
        return (immr % 16) <= (imms - (width - 15));
    }

    false
}

fn bfx_preferred(sf: bool, uns: bool, imms: u8, immr: u8) -> bool {
    let imms = imms as u32;
    let immr = immr as u32;

    // Handled in UBFIZ/SBFIX
    if imms < immr {
        return false;
    }
    // Handled in LSR/ASR/LSL
    if imms == ((sf as u32) << 5) | 0b11111 {
        return false;
    }
    // Handled in UXTx/SXTx
    if immr == 0 {
        // 32-bit UXT[BH], SXT[BH]
        if !sf && (imms == 0b000111 || imms == 0b001111) {
            return false;
        }
        // 64-bit SXT[BHW]
        if sf && !uns && (imms == 0b000111 || imms == 0b001111 || imms == 0b011111) {
            return false;
        }
    }

    true
}
