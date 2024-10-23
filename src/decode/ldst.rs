use bitmatch::bitmatch;

use crate::{decode::sext, IndexMode, Instruction, Mnemonic, Operand, RegExtend};

#[bitmatch]
pub fn decode_load_store(insn: u32) -> Option<Instruction> {
    #[bitmatch]
    match insn {
        "a?bb_1c0d_d?ee_eeee ????_ff??_????_????" => {
            let op = f | (e << 2) | (d << 8) | (c << 10) | (b << 11) | (a << 13);
            decode_load_store_inner(op, insn)
        }
        _ => todo!(),
    }
}

#[bitmatch]
fn decode_load_store_inner(op: u32, insn: u32) -> Option<Instruction> {
    #[bitmatch]
    match op {
        // Load/store exclusive
        "? 00 0 0? ?????? ??" => decode_ldst_exc(insn),
        // Load register (literal)
        "? 01 ? 0? ?????? ??" => decode_ldst_reg_lit(insn),
        // Load/store register pair
        "? 10 ? ?? ?????? ??" => decode_ldst_reg_pair(insn),
        // Load/store register
        "? 11 ? 0? 0????? ??" => decode_ldst_reg(insn),
        // Load/store register (register offset)
        "? 11 ? 0? 1????? 10" => decode_ldst_reg_reg(insn),
        // Load/store register (unsigned imm)
        "? 11 ? 1? ?????? ??" => decode_ldst_reg_uimm(insn),
        _ => todo!(),
    }
}

fn pre_index_simm(xsp: u8, imm: u32, scale: u32, sign: u32) -> (Option<Operand>, Option<Operand>) {
    let offset = sext(imm.into(), sign as usize) << scale;
    (
        Some(Operand::MemXSpOff(xsp, IndexMode::PreIndex(offset))),
        None,
    )
}

fn offset_index_simm(
    xsp: u8,
    imm: u32,
    scale: u32,
    sign: u32,
) -> (Option<Operand>, Option<Operand>) {
    let offset = sext(imm.into(), sign as usize) << scale;
    (
        Some(Operand::MemXSpOff(xsp, IndexMode::Signed(offset))),
        None,
    )
}

fn post_index_simm(xsp: u8, imm: u32, scale: u32, sign: u32) -> (Option<Operand>, Option<Operand>) {
    let offset = sext(imm.into(), sign as usize) << scale;
    (
        Some(Operand::MemXSpOff(xsp, IndexMode::Unsigned(0))),
        Some(Operand::Simm(offset)),
    )
}

#[bitmatch]
fn decode_ldst_exc(insn: u32) -> Option<Instruction> {
    #[bitmatch]
    let "sQ ?????? A L B SSSSS C ttttt NNNNN TTTTT" = insn;
    // stlxr - Ws, Wt, [Xn|SP]
    //         Ws, Xt, [Xn|SP]
    // ldaxr - Wt, [Xn|SP]
    //         Xt, [Xn|SP]
    // stxr - Ws, Wt, [Xn|SP]
    //        Ws, Xt, [Xn|SP]
    let op1 = match Q != 0 {
        false => Operand::W,
        true => Operand::X,
    };
    let Rs = Operand::W(S as u8);
    let Rt = op1(T as u8);
    let Rn = Operand::MemXSpOff(N as u8, IndexMode::Unsigned(0));
    // TODO Rt2
    let _ = t;

    if A == 1 && C == 0 {
        unimplemented!()
    }

    let (mnemonic, operands) = match (s, Q, B) {
        // stxrb/stlxrb/stlrb + ldxrb/ldaxrb/ldarb
        (0, 0, 0) => match (L, A, C) {
            (0, 0, 0) => (Mnemonic::stxrb, [Some(Rs), Some(Rt), Some(Rn), None]),
            (0, 0, 1) => (Mnemonic::stlxrb, [Some(Rs), Some(Rt), Some(Rn), None]),
            (0, 1, 1) => (Mnemonic::stlrb, [Some(Rt), Some(Rn), None, None]),
            (1, 0, 0) => (Mnemonic::ldxrb, [Some(Rt), Some(Rn), None, None]),
            (1, 0, 1) => (Mnemonic::ldaxrb, [Some(Rt), Some(Rn), None, None]),
            (1, 1, 1) => (Mnemonic::ldarb, [Some(Rt), Some(Rn), None, None]),
            _ => unreachable!(),
        },

        // stxrh/stlxrh/stlrh + ldxrh/ldaxrh/ldarh
        (0, 1, 0) => match (L, A, C) {
            (0, 0, 0) => (Mnemonic::stxrh, [Some(Rs), Some(Rt), Some(Rn), None]),
            (0, 0, 1) => (Mnemonic::stlxrh, [Some(Rs), Some(Rt), Some(Rn), None]),
            (0, 1, 1) => (Mnemonic::stlrh, [Some(Rt), Some(Rn), None, None]),
            (1, 0, 0) => (Mnemonic::ldxrh, [Some(Rt), Some(Rn), None, None]),
            (1, 0, 1) => (Mnemonic::ldaxrh, [Some(Rt), Some(Rn), None, None]),
            (1, 1, 1) => (Mnemonic::ldarh, [Some(Rt), Some(Rn), None, None]),
            _ => unreachable!(),
        },

        // stxr/stlxr/stlr + ldxr/ldaxr/ldar
        (1, _, 0) => match (L, A, C) {
            (0, 0, 0) => (Mnemonic::stxr, [Some(Rs), Some(Rt), Some(Rn), None]),
            (0, 0, 1) => (Mnemonic::stlxr, [Some(Rs), Some(Rt), Some(Rn), None]),
            (0, 1, 1) => (Mnemonic::stlr, [Some(Rt), Some(Rn), None, None]),
            (1, 0, 0) => (Mnemonic::ldxr, [Some(Rt), Some(Rn), None, None]),
            (1, 0, 1) => (Mnemonic::ldaxr, [Some(Rt), Some(Rn), None, None]),
            (1, 1, 1) => (Mnemonic::ldar, [Some(Rt), Some(Rn), None, None]),
            _ => unreachable!(),
        },

        _ => todo!(),
    };

    Some(Instruction { mnemonic, operands })
}

#[bitmatch]
fn decode_ldst_reg_lit(insn: u32) -> Option<Instruction> {
    #[bitmatch]
    let "oo ??? V ?? iiiiiiiiiiiiiiiiiii TTTTT" = insn;

    match (o, V) {
        (_, 1) => todo!("vector load literal"),
        // ldr
        (_, 0) => {
            let (mnemonic, op): (_, fn(_) -> _) = match o {
                0b00 => (Mnemonic::ldr, Operand::W),
                0b01 => (Mnemonic::ldr, Operand::X),
                _ => todo!(),
            };
            let Rt = op(T as u8);
            let off = sext((i as u64) << 2, 20);

            Some(Instruction {
                mnemonic,
                operands: [Some(Rt), Some(Operand::PcRelImm(off)), None, None],
            })
        }
        _ => todo!(),
    }
}

#[bitmatch]
fn decode_ldst_reg_pair(insn: u32) -> Option<Instruction> {
    #[bitmatch]
    match insn {
        "s0 101 0 0mm L iiiiiii ttttt NNNNN TTTTT" => {
            let mode = match m {
                // No-allocate pair
                0b00 => todo!(),
                // Post-index
                0b01 => post_index_simm,
                // Offset
                0b10 => offset_index_simm,
                // Pre-index
                0b11 => pre_index_simm,
                _ => unreachable!(),
            };
            let mnemonic = match L != 0 {
                false => Mnemonic::stp,
                true => Mnemonic::ldp,
            };
            let op = match s != 0 {
                false => Operand::W,
                true => Operand::X,
            };
            let Rt1 = op(T as u8);
            let Rt2 = op(t as u8);
            let scale = 2 + s;
            let (mem1, mem2) = mode(N as u8, i, scale, 6);

            Some(Instruction {
                mnemonic,
                operands: [Some(Rt1), Some(Rt2), mem1, mem2],
            })
        }
        _ => todo!(),
    }
}

#[bitmatch]
fn decode_ldst_reg(insn: u32) -> Option<Instruction> {
    #[bitmatch]
    let "ss ??? V ?? oL ? iiiiiiiii mm NNNNN TTTTT" = insn;

    let (scale, mode): (_, fn(_, _, _, _) -> _) = match m {
        // Unscaled immediate
        0b00 => (0, offset_index_simm),
        // Immediate post-indexed
        0b01 => (1, post_index_simm),
        // Unprivileged
        0b10 => todo!(),
        // Immediate pre-indexed
        0b11 => (1, pre_index_simm),
        _ => unreachable!(),
    };

    match (s, V, o, L) {
        // strb/ldrb
        (0b00, 0, 0, _) => {
            let mnemonic = match (scale, L != 0) {
                (0, false) => Mnemonic::sturb,
                (0, true) => Mnemonic::ldurb,
                (1, false) => Mnemonic::strb,
                (1, true) => Mnemonic::ldrb,
                _ => unreachable!(),
            };
            let (mem1, mem2) = mode(N as u8, i, 0, 8);
            let Rt = Operand::W(T as u8);

            Some(Instruction {
                mnemonic,
                operands: [Some(Rt), mem1, mem2, None],
            })
        }
        // ldrsb
        (0b00, 0, 1, _) => {
            let op = match L != 0 {
                false => Operand::X,
                true => Operand::W,
            };
            let mnemonic = match scale != 0 {
                false => Mnemonic::ldursb,
                true => Mnemonic::ldrsb,
            };
            let (mem1, mem2) = mode(N as u8, i, 0, 8);
            let Rt = op(T as u8);

            Some(Instruction {
                mnemonic,
                operands: [Some(Rt), mem1, mem2, None],
            })
        }
        // strh/ldrh
        (0b01, 0, 0, _) => {
            let mnemonic = match (scale, L != 0) {
                (0, false) => Mnemonic::sturh,
                (0, true) => Mnemonic::ldurh,
                (1, false) => Mnemonic::strh,
                (1, true) => Mnemonic::ldrh,
                _ => unreachable!(),
            };
            let (mem1, mem2) = mode(N as u8, i, 0, 8);
            let Rt = Operand::W(T as u8);

            Some(Instruction {
                mnemonic,
                operands: [Some(Rt), mem1, mem2, None],
            })
        }
        // str/ldr
        (0b10 | 0b11, 0, 0, _) => {
            let mnemonic = match (scale, L != 0) {
                (0, false) => Mnemonic::stur,
                (0, true) => Mnemonic::ldur,
                (1, false) => Mnemonic::str,
                (1, true) => Mnemonic::ldr,
                _ => unreachable!(),
            };
            let op = match s & 1 != 0 {
                false => Operand::W,
                true => Operand::X,
            };
            let (mem1, mem2) = mode(N as u8, i, 0, 8);
            let Rt = op(T as u8);

            Some(Instruction {
                mnemonic,
                operands: [Some(Rt), mem1, mem2, None],
            })
        }
        _ => todo!(),
    }
}

#[bitmatch]
fn decode_ldst_reg_reg(insn: u32) -> Option<Instruction> {
    #[bitmatch]
    let "ss ??? V ?? oL ? MMMMM OOO S ?? NNNNN TTTTT" = insn;

    match (s, V, o, O) {
        // strb/ldrb - shifted
        (0b00, 0, 0, 0b011) => {
            let mnemonic = match L != 0 {
                false => Mnemonic::strb,
                true => Mnemonic::ldrb,
            };
            let Rt = Operand::W(T as u8);
            // TODO vague description of <amount> field

            Some(Instruction {
                mnemonic,
                operands: [
                    Some(Rt),
                    Some(Operand::MemXSpOff(N as u8, IndexMode::X(M as u8))),
                    None,
                    None,
                ],
            })
        }
        // strb/ldrb - extended
        (0b00, 0, 0, _) => {
            let mnemonic = match L != 0 {
                false => Mnemonic::strb,
                true => Mnemonic::ldrb,
            };
            let extend = match O {
                0b010 => RegExtend::uxtw,
                0b110 => RegExtend::sxtw,
                0b111 => RegExtend::sxtx,
                _ => unimplemented!(),
            };
            let index = match O & 1 != 0 {
                false => IndexMode::WExt,
                true => IndexMode::XExt,
            };
            let Rt = Operand::W(T as u8);
            let index = index(M as u8, extend(0));

            Some(Instruction {
                mnemonic,
                operands: [
                    Some(Rt),
                    Some(Operand::MemXSpOff(N as u8, index)),
                    None,
                    None,
                ],
            })
        }

        // ldrsb shifted
        (0b00, 0, 1, 0b011) => {
            assert!(S == 0, "TODO");
            let op = match L != 0 {
                false => Operand::X,
                true => Operand::W,
            };
            let index = match O & 1 != 0 {
                false => IndexMode::WExt,
                true => IndexMode::XExt,
            };
            let Rt = op(T as u8);
            let index = index(M as u8, RegExtend::lsl(0));

            Some(Instruction {
                mnemonic: Mnemonic::ldrsb,
                operands: [
                    Some(Rt),
                    Some(Operand::MemXSpOff(N as u8, index)),
                    None,
                    None,
                ],
            })
        }
        // ldrsb extended
        (0b00, 0, 1, _) => todo!(),

        // strh/ldrh
        (0b01, 0, 0, _) => {
            let mnemonic = match L != 0 {
                false => Mnemonic::strh,
                true => Mnemonic::ldrh,
            };
            let shift = S;
            let extend = match O {
                0b010 => RegExtend::uxtw,
                0b011 => RegExtend::lsl,
                0b110 => RegExtend::sxtw,
                0b111 => RegExtend::sxtx,
                _ => todo!(),
            };
            let Rt = Operand::W(T as u8);
            let index = match O & 1 != 0 {
                false => IndexMode::WExt,
                true => IndexMode::XExt,
            };
            let index = index(M as u8, extend(shift as u8));

            Some(Instruction {
                mnemonic,
                operands: [
                    Some(Rt),
                    Some(Operand::MemXSpOff(N as u8, index)),
                    None,
                    None,
                ],
            })
        }

        // ldrsw
        (0b10, 0, 1, _) if L == 0 => {
            let shift = S * 2;
            let extend = match O {
                0b010 => RegExtend::uxtw,
                0b011 => RegExtend::lsl,
                0b110 => RegExtend::sxtw,
                0b111 => RegExtend::sxtx,
                _ => todo!(),
            };
            let Rt = Operand::X(T as u8);
            let index = match O & 1 != 0 {
                false => IndexMode::WExt,
                true => IndexMode::XExt,
            };
            let index = index(M as u8, extend(shift as u8));

            Some(Instruction {
                mnemonic: Mnemonic::ldrsw,
                operands: [
                    Some(Rt),
                    Some(Operand::MemXSpOff(N as u8, index)),
                    None,
                    None,
                ],
            })
        }

        // ldr/str
        (0b10 | 0b11, 0, 0, _) => {
            let mnemonic = match L != 0 {
                false => Mnemonic::str,
                true => Mnemonic::ldr,
            };
            let scale = match (s & 1, S & 1) {
                (_, 0) => 0,
                (0, 1) => 2,
                (1, 1) => 3,
                _ => unreachable!(),
            };
            let op = match s & 1 != 0 {
                false => Operand::W,
                true => Operand::X,
            };
            let extend = match O {
                0b010 => RegExtend::uxtw,
                0b011 => RegExtend::lsl,
                0b110 => RegExtend::sxtw,
                0b111 => RegExtend::sxtx,
                _ => unimplemented!(),
            };
            let index = match O & 1 != 0 {
                false => IndexMode::WExt,
                true => IndexMode::XExt,
            };
            let Rt = op(T as u8);
            let index = index(M as u8, extend(scale));
            let Rn = Operand::MemXSpOff(N as u8, index);

            Some(Instruction {
                mnemonic,
                operands: [Some(Rt), Some(Rn), None, None],
            })
        }

        _ => todo!(),
    }
}

// Load/store register (unsigned immediate)
#[bitmatch]
fn decode_ldst_reg_uimm(insn: u32) -> Option<Instruction> {
    #[bitmatch]
    let "oo ??? o ?? oo iiiiiiiiiiii NNNNN TTTTT" = insn;
    let T = T as u8;
    let N = N as u8;

    #[bitmatch]
    match o {
        // str/ldr (imm)
        "1s 0 0L" => {
            let mnemonic = match L != 0 {
                false => Mnemonic::str,
                true => Mnemonic::ldr,
            };
            let op = match s != 0 {
                false => Operand::W,
                true => Operand::X,
            };

            let scale = 2 + s;
            let imm = (i as u64) << scale;
            let Rt = op(T);

            Some(Instruction {
                mnemonic,
                operands: [
                    Some(Rt),
                    Some(Operand::MemXSpOff(N, IndexMode::Unsigned(imm))),
                    None,
                    None,
                ],
            })
        }

        // strh/ldrh (imm)
        "01 0 0L" => {
            let mnemonic = match L != 0 {
                false => Mnemonic::strh,
                true => Mnemonic::ldrh,
            };
            let Rt = Operand::W(T);

            Some(Instruction {
                mnemonic,
                operands: [
                    Some(Rt),
                    Some(Operand::MemXSpOff(N, IndexMode::Unsigned(i as u64 * 2))),
                    None,
                    None,
                ],
            })
        }

        // ldrsh
        "01 0 1L" => {
            let op = match L != 0 {
                false => Operand::X,
                true => Operand::W,
            };
            let Rt = op(T);

            Some(Instruction {
                mnemonic: Mnemonic::ldrsh,
                operands: [
                    Some(Rt),
                    Some(Operand::MemXSpOff(N, IndexMode::Unsigned(i as u64 * 2))),
                    None,
                    None,
                ],
            })
        }

        // strb/ldrb (imm)
        "00 0 0L" => {
            let mnemonic = match L != 0 {
                false => Mnemonic::strb,
                true => Mnemonic::ldrb,
            };
            let Rt = Operand::W(T);

            Some(Instruction {
                mnemonic,
                operands: [
                    Some(Rt),
                    Some(Operand::MemXSpOff(N as u8, IndexMode::Unsigned(i as u64))),
                    None,
                    None,
                ],
            })
        }

        // ldrsb
        "00 0 1L" => {
            let op = match L != 0 {
                false => Operand::X,
                true => Operand::W,
            };
            let Rt = op(T);

            Some(Instruction {
                mnemonic: Mnemonic::ldrsb,
                operands: [
                    Some(Rt),
                    Some(Operand::MemXSpOff(N, IndexMode::Unsigned(i as u64))),
                    None,
                    None,
                ],
            })
        }

        // ldrsw
        "10 0 10" => Some(Instruction {
            mnemonic: Mnemonic::ldrsw,
            operands: [
                Some(Operand::X(T)),
                Some(Operand::MemXSpOff(N, IndexMode::Unsigned((i << 2).into()))),
                None,
                None,
            ],
        }),

        _ => todo!("opc = {:b}", o),
    }
}
