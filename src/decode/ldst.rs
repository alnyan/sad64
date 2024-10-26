use bitmatch::bitmatch;

use crate::{
    decode::sext,
    operand::{Prefetch, VectorMulti, VectorMultiGroup, VectorSingle, VectorSingleGroup},
    IndexMode, Instruction, Mnemonic, Operand, RegExtend,
};

#[bitmatch]
pub fn decode_load_store(insn: u32) -> Option<Instruction> {
    #[bitmatch]
    match insn {
        "a?bb_1c0d_d?ee_eeee ????_ff??_????_????" => {
            let op = f | (e << 2) | (d << 8) | (c << 10) | (b << 11) | (a << 13);
            decode_load_store_inner(op, insn)
        }
        _ => None,
    }
}

#[bitmatch]
fn decode_load_store_inner(op: u32, insn: u32) -> Option<Instruction> {
    #[bitmatch]
    match op {
        // Load/store SIMD multiple structures
        "0 00 1 0? ?????? ??" => decode_ldst_simd_multi(insn),
        // Load/store SIMD single structure
        "0 00 1 1? ?????? ??" => decode_ldst_simd_single(insn),
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
        _ => None,
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
fn decode_ldst_simd_multi(insn: u32) -> Option<Instruction> {
    #[bitmatch]
    let "? Q ??????q L ? MMMMM oooo ss NNNNN TTTTT" = insn;
    // q == 0 -> multi struct
    // q == 1 -> multi struct post indexed

    let T = T as u8;

    let (mnemonic, len) = match (L, o) {
        // ld4/st4 multiple
        (0, 0b0000) => (Mnemonic::st4, 4),
        (1, 0b0000) => (Mnemonic::ld4, 4),
        // ld1/st1 four regs
        (0, 0b0010) => (Mnemonic::st1, 4),
        (1, 0b0010) => (Mnemonic::ld1, 4),
        // ld3/st3
        (0, 0b0100) => (Mnemonic::st3, 3),
        (1, 0b0100) => (Mnemonic::ld3, 3),
        // ld1/st1 three regs
        (0, 0b0110) => (Mnemonic::st1, 3),
        (1, 0b0110) => (Mnemonic::ld1, 3),
        // ld1/st1 one reg
        (0, 0b0111) => (Mnemonic::st1, 1),
        (1, 0b0111) => (Mnemonic::ld1, 1),
        // ld2/st2
        (0, 0b1000) => (Mnemonic::st2, 2),
        (1, 0b1000) => (Mnemonic::ld2, 2),
        // ld1/st1 two regs
        (0, 0b1010) => (Mnemonic::st1, 2),
        (1, 0b1010) => (Mnemonic::ld1, 2),
        _ => return None,
    };

    let (op1, op2) = match (q, M) {
        (1, 0b11111) => (
            Some(Operand::MemXSpOff(N as u8, IndexMode::Unsigned(0))),
            Some(Operand::Imm((Q as u64 + 1) * (8 * len as u64))),
        ),
        (1, _) => (
            Some(Operand::MemXSpOff(N as u8, IndexMode::Unsigned(0))),
            Some(Operand::X(M as u8)),
        ),
        (0, _) => (
            Some(Operand::MemXSpOff(N as u8, IndexMode::Unsigned(0))),
            None,
        ),
        _ => unreachable!(),
    };

    let base = match (mnemonic, s, Q) {
        (_, 0b00, 0) => VectorMulti::v8b(T, None),
        (_, 0b00, 1) => VectorMulti::v16b(T, None),
        (_, 0b01, 0) => VectorMulti::v4h(T, None),
        (_, 0b01, 1) => VectorMulti::v8h(T, None),
        (_, 0b10, 0) => VectorMulti::v2s(T, None),
        (_, 0b10, 1) => VectorMulti::v4s(T, None),
        // .1d is only for st1/ld1
        (Mnemonic::ld1 | Mnemonic::st1, 0b11, 0) => VectorMulti::v1d(T, None),
        (_, 0b11, 1) => VectorMulti::v2d(T, None),
        _ => return None,
    };

    let group = VectorMultiGroup { base, size: len };

    Some(Instruction {
        mnemonic,
        operands: [Some(Operand::VMultiGroup(group)), op1, op2, None],
    })
}

#[bitmatch]
fn decode_ldst_simd_single(insn: u32) -> Option<Instruction> {
    #[bitmatch]
    let "? Q ??????q L R MMMMM ooo S ss NNNNN TTTTT" = insn;
    let T = T as u8;

    let (bits, len) = match (L, R, o, S, s) {
        // No replicate
        (_, 0, 0b000, _, _) => (8, 1),
        (_, 0, 0b001, _, _) => (8, 3),
        (_, 0, 0b010, _, 0b10 | 0b00) => (16, 1),
        (_, 0, 0b011, _, 0b10 | 0b00) => (16, 3),
        (_, 0, 0b100, _, 0b00) => (32, 1),
        (_, 0, 0b100, 0, 0b01) => (64, 1),
        (_, 0, 0b101, _, 0b00) => (32, 3),
        (_, 0, 0b101, 0, 0b01) => (64, 3),
        (_, 1, 0b000, _, _) => (8, 2),
        (_, 1, 0b001, _, _) => (8, 4),
        (_, 1, 0b010, _, 0b10 | 0b00) => (16, 2),
        (_, 1, 0b011, _, 0b10 | 0b00) => (16, 4),
        (_, 1, 0b100, _, 0b00) => (32, 2),
        (_, 1, 0b100, 0, 0b01) => (64, 2),
        (_, 1, 0b101, _, 0b00) => (32, 4),
        (_, 1, 0b101, 0, 0b01) => (64, 4),
        // Replicate
        (1, 0, 0b110, 0, _) => (0, 1),
        (1, 0, 0b111, 0, _) => (0, 3),
        (1, 1, 0b110, 0, _) => (0, 2),
        (1, 1, 0b111, 0, _) => (0, 4),
        _ => return None,
    };

    match bits != 0 {
        // No replicate
        true => {
            let imm = (len as u64) * (bits / 8);

            let (op1, op2) = match (q, M) {
                (1, 0b11111) => (
                    Some(Operand::MemXSpOff(N as u8, IndexMode::Unsigned(0))),
                    Some(Operand::Imm(imm)),
                ),
                (1, _) => (
                    Some(Operand::MemXSpOff(N as u8, IndexMode::Unsigned(0))),
                    Some(Operand::X(M as u8)),
                ),
                (0, _) => (
                    Some(Operand::MemXSpOff(N as u8, IndexMode::Unsigned(0))),
                    None,
                ),
                _ => unreachable!(),
            };

            let mnemonic = match (L, len) {
                (0, 1) => Mnemonic::st1,
                (1, 1) => Mnemonic::ld1,
                (0, 2) => Mnemonic::st2,
                (1, 2) => Mnemonic::ld2,
                (0, 3) => Mnemonic::st3,
                (1, 3) => Mnemonic::ld3,
                (0, 4) => Mnemonic::st4,
                (1, 4) => Mnemonic::ld4,
                _ => unreachable!(),
            };

            let (base, index) = match bits {
                8 => (VectorSingle::b(T), s | (S << 2) | (Q << 3)),
                16 => (VectorSingle::h(T), (s >> 1) | (S << 1) | (Q << 2)),
                32 => (VectorSingle::s(T), S | (Q << 1)),
                64 => (VectorSingle::d(T), Q),
                _ => unreachable!(),
            };

            let group = VectorSingleGroup {
                base,
                size: len,
                index: index as u8,
            };

            Some(Instruction {
                mnemonic,
                operands: [Some(Operand::VSingleGroup(group)), op1, op2, None],
            })
        }
        // Replicate
        false => {
            let base = match (s, Q) {
                (0b00, 0) => VectorMulti::v8b(T, None),
                (0b00, 1) => VectorMulti::v16b(T, None),
                (0b01, 0) => VectorMulti::v4h(T, None),
                (0b01, 1) => VectorMulti::v8h(T, None),
                (0b10, 0) => VectorMulti::v2s(T, None),
                (0b10, 1) => VectorMulti::v4s(T, None),
                (0b11, 0) => VectorMulti::v1d(T, None),
                (0b11, 1) => VectorMulti::v2d(T, None),
                _ => unreachable!(),
            };
            let imm = (len as u64) * (1 << s);

            let (op1, op2) = match (q, M) {
                (1, 0b11111) => (
                    Some(Operand::MemXSpOff(N as u8, IndexMode::Unsigned(0))),
                    Some(Operand::Imm(imm)),
                ),
                (1, _) => (
                    Some(Operand::MemXSpOff(N as u8, IndexMode::Unsigned(0))),
                    Some(Operand::X(M as u8)),
                ),
                (0, _) => (
                    Some(Operand::MemXSpOff(N as u8, IndexMode::Unsigned(0))),
                    None,
                ),
                _ => unreachable!(),
            };

            let mnemonic = match len {
                1 => Mnemonic::ld1r,
                2 => Mnemonic::ld2r,
                3 => Mnemonic::ld3r,
                4 => Mnemonic::ld4r,
                _ => unreachable!(),
            };

            let group = VectorMultiGroup { base, size: len };

            Some(Instruction {
                mnemonic,
                operands: [Some(Operand::VMultiGroup(group)), op1, op2, None],
            })
        }
    }
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
    let Rt2 = op1(t as u8);
    let Rn = Operand::MemXSpOff(N as u8, IndexMode::Unsigned(0));

    if A == 1 && C == 0 {
        return None;
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
            _ => return None,
        },

        // stxrh/stlxrh/stlrh + ldxrh/ldaxrh/ldarh
        (0, 1, 0) => match (L, A, C) {
            (0, 0, 0) => (Mnemonic::stxrh, [Some(Rs), Some(Rt), Some(Rn), None]),
            (0, 0, 1) => (Mnemonic::stlxrh, [Some(Rs), Some(Rt), Some(Rn), None]),
            (0, 1, 1) => (Mnemonic::stlrh, [Some(Rt), Some(Rn), None, None]),
            (1, 0, 0) => (Mnemonic::ldxrh, [Some(Rt), Some(Rn), None, None]),
            (1, 0, 1) => (Mnemonic::ldaxrh, [Some(Rt), Some(Rn), None, None]),
            (1, 1, 1) => (Mnemonic::ldarh, [Some(Rt), Some(Rn), None, None]),
            _ => return None,
        },

        // stxr/stlxr/stlr + ldxr/ldaxr/ldar
        (1, _, 0) => match (L, A, C) {
            (0, 0, 0) => (Mnemonic::stxr, [Some(Rs), Some(Rt), Some(Rn), None]),
            (0, 0, 1) => (Mnemonic::stlxr, [Some(Rs), Some(Rt), Some(Rn), None]),
            (0, 1, 1) => (Mnemonic::stlr, [Some(Rt), Some(Rn), None, None]),
            (1, 0, 0) => (Mnemonic::ldxr, [Some(Rt), Some(Rn), None, None]),
            (1, 0, 1) => (Mnemonic::ldaxr, [Some(Rt), Some(Rn), None, None]),
            (1, 1, 1) => (Mnemonic::ldar, [Some(Rt), Some(Rn), None, None]),
            _ => return None,
        },

        // stxp/stlxp + ldxp/ldaxp
        (1, _, 1) => match (L, A, C) {
            (0, 0, 0) => (Mnemonic::stxp, [Some(Rs), Some(Rt), Some(Rt2), Some(Rn)]),
            (0, 0, 1) => (Mnemonic::stlxp, [Some(Rs), Some(Rt), Some(Rt2), Some(Rn)]),
            (1, 0, 0) => (Mnemonic::ldxp, [Some(Rt), Some(Rt2), Some(Rn), None]),
            (1, 0, 1) => (Mnemonic::ldaxp, [Some(Rt), Some(Rt2), Some(Rn), None]),
            _ => return None,
        },

        _ => return None,
    };

    Some(Instruction { mnemonic, operands })
}

#[bitmatch]
fn decode_ldst_reg_lit(insn: u32) -> Option<Instruction> {
    #[bitmatch]
    let "oo ??? V ?? iiiiiiiiiiiiiiiiiii TTTTT" = insn;

    match (o, V) {
        (0b11, 1) => None,
        (_, 1) => {
            let op0 = match o {
                0b00 => VectorMulti::s,
                0b01 => VectorMulti::d,
                0b10 => VectorMulti::q,
                _ => unreachable!(),
            };
            let Rt = Operand::VMulti(op0(T as u8));
            let off = sext((i as u64) << 2, 20);

            Some(Instruction {
                mnemonic: Mnemonic::ldr,
                operands: [Some(Rt), Some(Operand::PcRelImm(off)), None, None],
            })
        }
        // prfm
        (0b11, 0) => {
            let op0 = match Prefetch::decode(T as u8) {
                Some(prefetch) => Operand::Prefetch(prefetch),
                None => Operand::Imm(T as _),
            };
            let off = sext((i as u64) << 2, 20);
            Some(Instruction {
                mnemonic: Mnemonic::prfm,
                operands: [Some(op0), Some(Operand::PcRelImm(off)), None, None],
            })
        }
        // ldr
        (_, 0) => {
            let (mnemonic, op): (_, fn(_) -> _) = match o {
                0b00 => (Mnemonic::ldr, Operand::W),
                0b01 => (Mnemonic::ldr, Operand::X),
                0b10 => (Mnemonic::ldrsw, Operand::X),
                _ => return None,
            };
            let Rt = op(T as u8);
            let off = sext((i as u64) << 2, 20);

            Some(Instruction {
                mnemonic,
                operands: [Some(Rt), Some(Operand::PcRelImm(off)), None, None],
            })
        }
        _ => unreachable!(),
    }
}

#[bitmatch]
fn decode_ldst_reg_pair(insn: u32) -> Option<Instruction> {
    #[bitmatch]
    match insn {
        // Vector load/store pair
        "ss 101 1 0mm L iiiiiii ttttt NNNNN TTTTT" => {
            let mode = match m {
                // No-allocate pair
                0b00 => offset_index_simm,
                // Post-index
                0b01 => post_index_simm,
                // Offset
                0b10 => offset_index_simm,
                // Pre-index
                0b11 => pre_index_simm,
                _ => unreachable!(),
            };

            let op01 = match s {
                0b00 => VectorMulti::s,
                0b01 => VectorMulti::d,
                0b10 => VectorMulti::q,
                _ => return None,
            };

            let mnemonic = match (m, L != 0) {
                (0b00, false) => Mnemonic::stnp,
                (0b00, true) => Mnemonic::ldnp,
                (_, false) => Mnemonic::stp,
                (_, true) => Mnemonic::ldp,
            };

            let Rt1 = Operand::VMulti(op01(T as u8));
            let Rt2 = Operand::VMulti(op01(t as u8));
            let scale = s + 2;
            let (mem1, mem2) = mode(N as u8, i, scale, 6);

            Some(Instruction {
                mnemonic,
                operands: [Some(Rt1), Some(Rt2), mem1, mem2],
            })
        }
        "s0 101 0 0mm L iiiiiii ttttt NNNNN TTTTT" => {
            let mode = match m {
                // No-allocate pair
                0b00 => offset_index_simm,
                // Post-index
                0b01 => post_index_simm,
                // Offset
                0b10 => offset_index_simm,
                // Pre-index
                0b11 => pre_index_simm,
                _ => unreachable!(),
            };
            let mnemonic = match (m, L != 0) {
                (0b00, false) => Mnemonic::stnp,
                (0b00, true) => Mnemonic::ldnp,
                (_, false) => Mnemonic::stp,
                (_, true) => Mnemonic::ldp,
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
        _ => None,
    }
}

#[bitmatch]
fn decode_ldst_reg(insn: u32) -> Option<Instruction> {
    #[bitmatch]
    let "ss ??? V ?? oL ? iiiiiiiii mm NNNNN TTTTT" = insn;

    let (pri, scale, mode): (_, _, fn(_, _, _, _) -> _) = match m {
        // Unscaled immediate
        0b00 => (1, 0, offset_index_simm),
        // Immediate post-indexed
        0b01 => (1, 1, post_index_simm),
        // Unprivileged
        0b10 => (0, 1, offset_index_simm),
        // Immediate pre-indexed
        0b11 => (1, 1, pre_index_simm),
        _ => unreachable!(),
    };

    match (pri, s, V, o, L) {
        (1, _, 1, _, _) => {
            let (size, op): (_, fn(_) -> _) = match (s, o) {
                (0b00, 0) => (1, VectorMulti::b),
                (0b00, 1) => (16, VectorMulti::q),
                (0b01, 0) => (2, VectorMulti::h),
                (0b10, 0) => (4, VectorMulti::s),
                (0b11, 0) => (8, VectorMulti::d),
                _ => return None,
            };
            // No immediate pre-index for <32bit
            if size < 4 && m == 0b11 {
                return None;
            }
            let mnemonic = match (scale, L != 0) {
                (0, false) => Mnemonic::stur,
                (0, true) => Mnemonic::ldur,
                (1, false) => Mnemonic::str,
                (1, true) => Mnemonic::ldr,
                _ => unreachable!(),
            };
            let (mem1, mem2) = mode(N as u8, i, 0, 8);
            let Rt = Operand::VMulti(op(T as u8));

            Some(Instruction {
                mnemonic,
                operands: [Some(Rt), mem1, mem2, None],
            })
        }

        // pfrm
        (1, 0b11, 0, 1, 0) => {
            let mnemonic = match scale != 0 {
                false => Mnemonic::prfum,
                true => Mnemonic::prfm,
            };
            let (mem1, mem2) = mode(N as u8, i, 0, 8);
            let op0 = match Prefetch::decode(T as u8) {
                Some(prefetch) => Operand::Prefetch(prefetch),
                None => Operand::Imm(T as _),
            };
            Some(Instruction {
                mnemonic,
                operands: [Some(op0), mem1, mem2, None],
            })
        }

        (_, _, 0, _, _) => {
            let (mnemonic, force_x) = load_store_mnemonic(pri, scale, s, o, L)?;
            let op = load_store_operand(force_x, s)?;

            let (mem1, mem2) = mode(N as u8, i, 0, 8);
            let Rt = op(T as u8);

            Some(Instruction {
                mnemonic,
                operands: [Some(Rt), mem1, mem2, None],
            })
        }
        _ => None,
    }
}

#[bitmatch]
fn decode_ldst_reg_reg(insn: u32) -> Option<Instruction> {
    #[bitmatch]
    let "ss ??? V ?? oL ? MMMMM OOO S ?? NNNNN TTTTT" = insn;

    match (s, V, o, O) {
        // simd load/store
        (_, 1, _, _) => {
            let (scale, op): (_, fn(_) -> _) = match (s, o) {
                (0b00, 0) => (0, VectorMulti::b),
                (0b01, 0) => (1, VectorMulti::h),
                (0b10, 0) => (2, VectorMulti::s),
                (0b11, 0) => (3, VectorMulti::d),
                (0b00, 1) => (4, VectorMulti::q),
                _ => return None,
            };
            let mnemonic = match L != 0 {
                false => Mnemonic::str,
                true => Mnemonic::ldr,
            };
            let Rt = Operand::VMulti(op(T as u8));
            let amount = (S != 0).then_some(scale * S as u8);
            let index = RegExtend::decode_index(M as _, O as _, amount)?;

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

        // prfm
        (0b11, 0, 1, _) if L == 0 => {
            let scale = S * 3;
            let amount = (scale != 0).then_some(scale as u8);
            let index = RegExtend::decode_index(M as _, O as _, amount)?;
            let Rn = Operand::MemXSpOff(N as u8, index);
            let op0 = match Prefetch::decode(T as u8) {
                Some(prefetch) => Operand::Prefetch(prefetch),
                None => Operand::Imm(T as _),
            };

            Some(Instruction {
                mnemonic: Mnemonic::prfm,
                operands: [Some(op0), Some(Rn), None, None],
            })
        }

        // strb/strh/str + ldrb/ldrh/ldr/ldrsb/ldrsh/ldrsw
        (_, 0, _, _) => {
            let (mnemonic, force_x) = load_store_mnemonic(1, 1, s, o, L)?;
            let op = load_store_operand(force_x, s)?;

            let Rt = op(T as u8);
            let amount = (S != 0).then_some((s * S) as u8);
            let index = RegExtend::decode_index(M as _, O as _, amount)?;

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

        _ => None,
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
        "ss 1 sL" => {
            let op = match s {
                0b000 => VectorMulti::b,
                0b001 => VectorMulti::q,
                0b010 => VectorMulti::h,
                0b100 => VectorMulti::s,
                0b110 => VectorMulti::d,
                _ => return None,
            };
            let Rt = Operand::VMulti(op(T));
            let mnemonic = match L != 0 {
                false => Mnemonic::str,
                true => Mnemonic::ldr,
            };

            Some(Instruction {
                mnemonic,
                operands: [
                    Some(Rt),
                    Some(Operand::MemXSpOff(N, IndexMode::Unsigned(i as u64))),
                    None,
                    None,
                ],
            })
        }

        // prfm
        "11 0 10" => {
            let op0 = match Prefetch::decode(T) {
                Some(prefetch) => Operand::Prefetch(prefetch),
                None => Operand::Imm(T as _),
            };
            let imm = (i as u64) << 3;

            Some(Instruction {
                mnemonic: Mnemonic::prfm,
                operands: [
                    Some(op0),
                    Some(Operand::MemXSpOff(N, IndexMode::Unsigned(imm))),
                    None,
                    None,
                ],
            })
        }

        "ss 0 SL" => {
            let (mnemonic, force_x) = load_store_mnemonic(1, 1, s, S, L)?;
            let op = load_store_operand(force_x, s)?;

            let imm = (i as u64) << s;
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

        _ => None,
    }
}

fn load_store_operand(force_x: bool, size: u32) -> Option<fn(u8) -> Operand> {
    match (force_x, size) {
        (true, _) => Some(Operand::X),
        (_, 0..=2) => Some(Operand::W),
        (_, 3) => Some(Operand::X),
        _ => None,
    }
}

fn load_store_mnemonic(
    privf: u32,
    scalef: u32,
    scale: u32,
    signed: u32,
    load: u32,
) -> Option<(Mnemonic, bool)> {
    let (mnemonic, force_x) = match (privf, scalef, scale, signed, load) {
        // Unprivileged
        (0, 1, 0, 1, _) => (Mnemonic::ldtrsb, load == 0),
        (0, 1, 1, 1, _) => (Mnemonic::ldtrsh, load == 0),
        (0, 1, 2, 1, _) => (Mnemonic::ldtrsw, load == 0),
        (0, 1, 0, 0, 0) => (Mnemonic::sttrb, false),
        (0, 1, 0, 0, 1) => (Mnemonic::ldtrb, false),
        (0, 1, 1, 0, 0) => (Mnemonic::sttrh, false),
        (0, 1, 1, 0, 1) => (Mnemonic::ldtrh, false),
        (0, 1, _, 0, 0) => (Mnemonic::sttr, false),
        (0, 1, _, 0, 1) => (Mnemonic::ldtr, false),

        // Unscaled
        (1, 0, 0, 1, _) => (Mnemonic::ldursb, load == 0),
        (1, 0, 1, 1, _) => (Mnemonic::ldursh, load == 0),
        (1, 0, 2, 1, _) => (Mnemonic::ldursw, load == 0),
        (1, 0, 0, 0, 0) => (Mnemonic::sturb, false),
        (1, 0, 0, 0, 1) => (Mnemonic::ldurb, false),
        (1, 0, 1, 0, 0) => (Mnemonic::sturh, false),
        (1, 0, 1, 0, 1) => (Mnemonic::ldurh, false),
        (1, 0, _, 0, 0) => (Mnemonic::stur, false),
        (1, 0, _, 0, 1) => (Mnemonic::ldur, false),

        // Other
        (1, 1, 0, 1, _) => (Mnemonic::ldrsb, load == 0),
        (1, 1, 1, 1, _) => (Mnemonic::ldrsh, load == 0),
        (1, 1, 2, 1, _) => (Mnemonic::ldrsw, load == 0),
        (1, 1, 0, 0, 0) => (Mnemonic::strb, false),
        (1, 1, 0, 0, 1) => (Mnemonic::ldrb, false),
        (1, 1, 1, 0, 0) => (Mnemonic::strh, false),
        (1, 1, 1, 0, 1) => (Mnemonic::ldrh, false),
        (1, 1, _, 0, 0) => (Mnemonic::str, false),
        (1, 1, _, 0, 1) => (Mnemonic::ldr, false),
        _ => return None,
    };

    Some((mnemonic, force_x))
}
