use bitmatch::bitmatch;

use crate::{
    operand::{VectorMulti, VectorMultiGroup, VectorSingle},
    Instruction, Mnemonic, Operand, SimdMnemonic,
};

#[bitmatch]
pub fn decode_simd(insn: u32) -> Option<Instruction> {
    // let "aaaa ??? bb cccc dd ? eeeeee ??????????" = insn;
    #[bitmatch]
    match insn {
        "0100 111 0? ?101 00 ? ????10 ??????????" => decode_crypto_aes(insn),
        "0101 111 0? ?0?? ?? ? 0???00 ??????????" => decode_crypto_3reg_sha(insn),
        "0101 111 0? ?101 00 ? ????10 ??????????" => decode_crypto_2reg_sha(insn),
        "01?1 111 00 00?? ?? ? 0????1 ??????????" => decode_simd_scalar_copy(insn),
        "01?1 111 0? ?100 00 ? ????10 ??????????" => decode_simd_scalar_2reg_misc(insn),
        "01?1 111 0? ?110 00 ? ????10 ??????????" => decode_simd_scalar_pairwise(insn),
        "01?1 111 0? ?1?? ?? ? ????10 ??????????" => todo!(),
        "01?1 111 0? ?1?? ?? ? ?????? ??????????" => decode_simd_scalar_3diff_same(insn),
        "01?1 111 10 ???? ?? ? ?????1 ??????????" => decode_simd_scalar_shift_imm(insn),
        "01?1 111 1? ???? ?? ? ?????0 ??????????" => decode_simd_scalar_x_index(insn),
        "0?00 111 0? ?0?? ?? ? 0???00 ??????????" => decode_simd_table_lookup(insn),
        "0?00 111 0? ?0?? ?? ? 0???10 ??????????" => decode_simd_permute(insn),
        "0?10 111 0? ?0?? ?? ? 0????0 ??????????" => decode_simd_extract(insn),
        "0??0 111 00 00?? ?? ? 0????1 ??????????" => decode_simd_copy(insn),

        // Advanced SIMD 2-reg misc
        "0??0 111 0? ?100 00 ? ????10 ??????????" => {
            todo!()
        }
        // Advanced SIMD across lanes
        "0??0 111 0? ?100 00 ? ????10 ??????????" => {
            todo!()
        }
        // Advanced SIMD three different
        "0??0 111 0? ?1?? ?? ? ????00 ??????????" => {
            todo!()
        }
        // Advanced SIMD three same
        "0??0 111 0? ?1?? ?? ? ?????1 ??????????" => {
            todo!()
        }
        // Advanced SIMD modified immediate
        "0??0 111 10 0000 ?? ? ?????1 ??????????" => {
            todo!()
        }
        // Advanced SIMD shifted by immediate
        "0??0 111 10 ???? ?? ? ?????1 ??????????" => {
            todo!()
        }
        // Advanced SIMD vector x indexed element
        "0??0 111 1? ???? ?? ? ?????0 ??????????" => {
            todo!()
        }
        // Conversion between float and fixed
        "?0?1 111 0? ?0?? ?? ? 000000 ??????????" => {
            todo!()
        }
        // Conversion between float and int
        "?0?1 111 0? ?1?? ?? ? 100000 ??????????" => {
            todo!()
        }
        // Float data-processing 1src
        "?0?1 111 0? ?1?? ?? ? ?10000 ??????????" => {
            todo!()
        }
        // Float compare
        "?0?1 111 0? ?1?? ?? ? ??1000 ??????????" => {
            todo!()
        }
        // Float immediate
        "?0?1 111 0? ?1?? ?? ? ???100 ??????????" => {
            todo!()
        }
        // Float conditional
        "?0?1 111 0? ?1?? ?? ? ????01 ??????????" => {
            todo!()
        }
        // Float data-processing 2src
        "?0?1 111 0? ?1?? ?? ? ????10 ??????????" => {
            todo!()
        }
        // Float conditional select
        "?0?1 111 0? ?1?? ?? ? ????11 ??????????" => {
            todo!()
        }
        // Float data-processing 3src
        "?0?1 111 1? ???? ?? ? ?????? ??????????" => {
            todo!()
        }
        _ => unimplemented!(),
    }
}

#[bitmatch]
fn decode_crypto_aes(insn: u32) -> Option<Instruction> {
    #[bitmatch]
    let "???????? ss ????? ooooo ?? NNNNN DDDDD" = insn;

    if s != 0 {
        return None;
    }

    let mnemonic = Mnemonic::Simd(match o {
        0b00100 => SimdMnemonic::aese,
        0b00101 => SimdMnemonic::aesd,
        0b00110 => SimdMnemonic::aesmc,
        0b00111 => SimdMnemonic::aesimc,
        _ => return None,
    });
    let Rd = Operand::VMulti(VectorMulti::v16b(D as u8, None));
    let Rn = Operand::VMulti(VectorMulti::v16b(N as u8, None));

    Some(Instruction {
        mnemonic,
        operands: [Some(Rd), Some(Rn), None, None],
    })
}

#[bitmatch]
fn decode_crypto_3reg_sha(insn: u32) -> Option<Instruction> {
    #[bitmatch]
    let "???????? ss ? MMMMM ? ooo ?? NNNNN DDDDD" = insn;

    if s != 0 {
        return None;
    }

    let (op0, op1, op2, mnemonic) = match o {
        0b000 => (
            VectorMulti::q(D as _),
            VectorMulti::s(N as _),
            VectorMulti::v4s(M as _, None),
            SimdMnemonic::sha1c,
        ),
        0b001 => (
            VectorMulti::q(D as _),
            VectorMulti::s(N as _),
            VectorMulti::v4s(M as _, None),
            SimdMnemonic::sha1p,
        ),
        0b010 => (
            VectorMulti::q(D as _),
            VectorMulti::s(N as _),
            VectorMulti::v4s(M as _, None),
            SimdMnemonic::sha1m,
        ),
        0b011 => (
            VectorMulti::v4s(D as _, None),
            VectorMulti::v4s(N as _, None),
            VectorMulti::v4s(M as _, None),
            SimdMnemonic::sha1su0,
        ),
        0b100 => (
            VectorMulti::q(D as _),
            VectorMulti::q(N as _),
            VectorMulti::v4s(M as _, None),
            SimdMnemonic::sha256h,
        ),
        0b101 => (
            VectorMulti::q(D as _),
            VectorMulti::q(N as _),
            VectorMulti::v4s(M as _, None),
            SimdMnemonic::sha256h2,
        ),
        0b110 => (
            VectorMulti::v4s(D as _, None),
            VectorMulti::v4s(N as _, None),
            VectorMulti::v4s(M as _, None),
            SimdMnemonic::sha256su1,
        ),
        _ => todo!(),
    };
    let mnemonic = Mnemonic::Simd(mnemonic);
    let op0 = Operand::VMulti(op0);
    let op1 = Operand::VMulti(op1);
    let op2 = Operand::VMulti(op2);

    Some(Instruction {
        mnemonic,
        operands: [Some(op0), Some(op1), Some(op2), None],
    })
}

#[bitmatch]
fn decode_crypto_2reg_sha(insn: u32) -> Option<Instruction> {
    #[bitmatch]
    let "???????? ss ????? ooooo ?? NNNNN DDDDD" = insn;

    if s != 0 {
        return None;
    }

    let (op0, op1, mnemonic) = match o {
        0b00000 => (
            VectorMulti::s(D as _),
            VectorMulti::s(N as _),
            SimdMnemonic::sha1h,
        ),
        0b00001 => (
            VectorMulti::v4s(D as _, None),
            VectorMulti::v4s(N as _, None),
            SimdMnemonic::sha1su1,
        ),
        0b00010 => (
            VectorMulti::v4s(D as _, None),
            VectorMulti::v4s(N as _, None),
            SimdMnemonic::sha256su0,
        ),
        _ => todo!(),
    };
    let mnemonic = Mnemonic::Simd(mnemonic);
    let op0 = Operand::VMulti(op0);
    let op1 = Operand::VMulti(op1);

    Some(Instruction {
        mnemonic,
        operands: [Some(op0), Some(op1), None, None],
    })
}

#[bitmatch]
fn decode_simd_scalar_copy(insn: u32) -> Option<Instruction> {
    #[bitmatch]
    let "?? o ???????? iiiii ? IIII ? NNNNN DDDDD" = insn;

    if o != 0 || I != 0 {
        return None;
    }
    let size = i.trailing_zeros();
    let index = i >> (size + 1);
    let (op0, op1): (fn(_) -> _, fn(_) -> _) = match size {
        0 => (VectorMulti::b, VectorSingle::b),
        1 => (VectorMulti::h, VectorSingle::h),
        2 => (VectorMulti::s, VectorSingle::s),
        3 => (VectorMulti::d, VectorSingle::d),
        _ => return None,
    };
    let Rd = Operand::VMulti(op0(D as _));
    let Rn = Operand::VSingleIndex(op1(N as _), index as _);

    Some(Instruction {
        mnemonic: Mnemonic::Simd(SimdMnemonic::dup),
        operands: [Some(Rd), Some(Rn), None, None],
    })
}

#[bitmatch]
fn decode_simd_scalar_2reg_misc(insn: u32) -> Option<Instruction> {
    #[bitmatch]
    let "?? U ????? ss ????? ooooo ?? NNNNN DDDDD" = insn;
    let D = D as u8;
    let N = N as u8;

    let (imm0, s0, s1, mnemonic) = match (U, s, o) {
        (0, _, 0b00011) => (0, s, s, SimdMnemonic::suqadd),
        (0, _, 0b00111) => (0, s, s, SimdMnemonic::sqabs),
        (0, _, 0b01000) => (1, s, s, SimdMnemonic::cmgt),
        (0, _, 0b01001) => (1, s, s, SimdMnemonic::cmeq),
        (0, _, 0b01010) => (1, s, s, SimdMnemonic::cmlt),
        (0, _, 0b01011) => (0, s, s, SimdMnemonic::abs),
        (0, _, 0b10100) => (0, s, s + 1, SimdMnemonic::sqxtn),
        (0, 0 | 1, 0b11010) => (0, s + 2, s + 2, SimdMnemonic::fcvtns),
        (0, 0 | 1, 0b11011) => (0, s + 2, s + 2, SimdMnemonic::fcvtms),
        (0, 0 | 1, 0b11100) => (0, s + 2, s + 2, SimdMnemonic::fcvtas),
        (0, 0 | 1, 0b11101) => (0, s + 2, s + 2, SimdMnemonic::scvtf),
        (0, 2 | 3, 0b01100) => (2, s, s, SimdMnemonic::fcmgt),
        (0, 2 | 3, 0b01101) => (2, s, s, SimdMnemonic::fcmeq),
        (0, 2 | 3, 0b01110) => (2, s, s, SimdMnemonic::fcmlt),
        (0, 2 | 3, 0b11010) => (0, s, s, SimdMnemonic::fcvtps),
        (0, 2 | 3, 0b11011) => (0, s, s, SimdMnemonic::fcvtzs),
        (0, 2 | 3, 0b11101) => (0, s, s, SimdMnemonic::frecpe),
        (0, 2 | 3, 0b11111) => (0, s, s, SimdMnemonic::frecpx),
        (1, _, 0b00011) => (0, s, s, SimdMnemonic::usqadd),
        (1, _, 0b00111) => (0, s, s, SimdMnemonic::sqneg),
        (1, _, 0b01000) => (1, s, s, SimdMnemonic::cmge),
        (1, _, 0b01001) => (1, s, s, SimdMnemonic::cmle),
        (1, _, 0b01011) => (0, s, s, SimdMnemonic::neg),
        (1, _, 0b10010) => (0, s, s + 1, SimdMnemonic::sqxtun),
        (1, _, 0b10100) => (0, s, s + 1, SimdMnemonic::uqxtn),
        (1, 0 | 1, 0b10110) => (0, s + 1, s + 2, SimdMnemonic::fcvtxn),
        (1, 0 | 1, 0b11010) => (0, s + 2, s + 2, SimdMnemonic::fcvtnu),
        (1, 0 | 1, 0b11011) => (0, s + 2, s + 2, SimdMnemonic::fcvtmu),
        (1, 0 | 1, 0b11100) => (0, s + 2, s + 2, SimdMnemonic::fcvtau),
        (1, 0 | 1, 0b11101) => (0, s + 2, s + 2, SimdMnemonic::ucvtf),
        (1, 2 | 3, 0b01100) => (2, s, s, SimdMnemonic::fcmge),
        (1, 2 | 3, 0b01101) => (2, s, s, SimdMnemonic::fcmle),
        (1, 2 | 3, 0b11010) => (0, s, s, SimdMnemonic::fcvtpu),
        (1, 2 | 3, 0b11011) => (0, s, s, SimdMnemonic::fcvtzu),
        (1, 2 | 3, 0b11101) => (0, s, s, SimdMnemonic::frsqrte),

        _ => return None,
    };
    let Rd = make_operand(0, s0, D)?;
    let Rn = make_operand(0, s1, N)?;

    let op2 = match imm0 {
        0 => None,
        1 => Some(Operand::DecImm(0)),
        2 => Some(Operand::FpImm(0.0)),
        _ => todo!(),
    };

    Some(Instruction {
        mnemonic: Mnemonic::Simd(mnemonic),
        operands: [Some(Rd), Some(Rn), op2, None],
    })
}

#[bitmatch]
fn decode_simd_scalar_pairwise(insn: u32) -> Option<Instruction> {
    #[bitmatch]
    let "?? U ????? ss ????? ooooo ?? NNNNN DDDDD" = insn;

    let (s0, s1, mnemonic) = match (U, s, o) {
        (0, _, 0b11011) => (3, 3, SimdMnemonic::addp),
        (1, 0 | 1, 0b01100) => (s + 2, s + 2, SimdMnemonic::fmaxnmp),
        (1, 0 | 1, 0b01101) => (s + 2, s + 2, SimdMnemonic::faddp),
        (1, 0 | 1, 0b01111) => (s + 2, s + 2, SimdMnemonic::fmaxp),
        (1, 2 | 3, 0b01100) => (s, s, SimdMnemonic::fminnmp),
        (1, 2 | 3, 0b01111) => (s, s, SimdMnemonic::fminp),
        _ => return None,
    };
    let Rd = make_operand(0, s0, D as u8)?;
    let Rn = match s1 {
        2 => Operand::VMulti(VectorMulti::v2s(N as _, None)),
        3 => Operand::VMulti(VectorMulti::v2d(N as _, None)),
        _ => unreachable!(),
    };

    Some(Instruction {
        mnemonic: Mnemonic::Simd(mnemonic),
        operands: [Some(Rd), Some(Rn), None, None],
    })
}

#[bitmatch]
fn decode_simd_scalar_3diff_same(insn: u32) -> Option<Instruction> {
    #[bitmatch]
    let "?? U ????? ss ? MMMMM oooooo NNNNN DDDDD" = insn;

    let (add0, add, min, max, mnemonic) = match (U, s, o) {
        // 3 diff
        (0, _, 0b100100) => (1, 0, 1, 2, SimdMnemonic::sqdmlal),
        (0, _, 0b101100) => (1, 0, 1, 2, SimdMnemonic::sqdmlsl),
        (0, _, 0b110100) => (1, 0, 1, 2, SimdMnemonic::sqdmull),
        // 3 same
        (0, _, 0b000011) => (0, 0, 0, 3, SimdMnemonic::sqadd),
        (0, _, 0b001011) => (0, 0, 0, 3, SimdMnemonic::sqsub),
        (0, _, 0b001101) => (0, 0, 3, 3, SimdMnemonic::cmgt),
        (0, _, 0b001111) => (0, 0, 3, 3, SimdMnemonic::cmge),
        (0, _, 0b010001) => (0, 0, 3, 3, SimdMnemonic::sshl),
        (0, _, 0b010011) => (0, 0, 0, 3, SimdMnemonic::sqshl),
        (0, _, 0b010101) => (0, 0, 3, 3, SimdMnemonic::srshl),
        (0, _, 0b010111) => (0, 0, 0, 3, SimdMnemonic::sqrshl),
        (0, _, 0b100001) => (0, 0, 3, 3, SimdMnemonic::add),
        (0, _, 0b100011) => (0, 0, 3, 3, SimdMnemonic::cmtst),
        (0, _, 0b101101) => (0, 0, 1, 2, SimdMnemonic::sqdmulh),
        (0, _, 0b110111) => (0, 2, 0, 1, SimdMnemonic::fmulx),
        (0, _, 0b111001) => (0, 2, 0, 1, SimdMnemonic::fcmeq),
        (0, 0 | 1, 0b111111) => (0, 2, 0, 1, SimdMnemonic::frecps),
        (0, 2 | 3, 0b111111) => (0, 0, 2, 3, SimdMnemonic::frsqrts),
        (1, _, 0b000011) => (0, 0, 0, 3, SimdMnemonic::uqadd),
        (1, _, 0b001011) => (0, 0, 0, 3, SimdMnemonic::uqsub),
        (1, _, 0b001101) => (0, 0, 3, 3, SimdMnemonic::cmhi),
        (1, _, 0b001111) => (0, 0, 3, 3, SimdMnemonic::cmhs),
        (1, _, 0b010001) => (0, 0, 3, 3, SimdMnemonic::ushl),
        (1, _, 0b010011) => (0, 0, 0, 3, SimdMnemonic::uqshl),
        (1, _, 0b010101) => (0, 0, 3, 3, SimdMnemonic::urshl),
        (1, _, 0b010111) => (0, 0, 0, 3, SimdMnemonic::uqrshl),
        (1, _, 0b100001) => (0, 0, 3, 3, SimdMnemonic::sub),
        (1, _, 0b100011) => (0, 0, 3, 3, SimdMnemonic::cmeq),
        (1, _, 0b101101) => (0, 0, 1, 2, SimdMnemonic::sqrdmulh),
        (1, 0 | 1, 0b111001) => (0, 2, 0, 1, SimdMnemonic::fcmge),
        (1, 0 | 1, 0b111011) => (0, 2, 0, 1, SimdMnemonic::facge),
        (1, 2 | 3, 0b110101) => (0, 0, 2, 3, SimdMnemonic::fabd),
        (1, 2 | 3, 0b111001) => (0, 0, 2, 3, SimdMnemonic::fcmgt),
        (1, 2 | 3, 0b111011) => (0, 0, 2, 3, SimdMnemonic::facgt),
        _ => return None,
    };

    if s < min || s > max {
        return None;
    }

    let Rd = make_operand(0, s + add0 + add, D as _)?;
    let Rn = make_operand(0, s + add, N as _)?;
    let Rm = make_operand(0, s + add, M as _)?;

    Some(Instruction {
        mnemonic: Mnemonic::Simd(mnemonic),
        operands: [Some(Rd), Some(Rn), Some(Rm), None],
    })
}

#[bitmatch]
fn decode_simd_scalar_shift_imm(insn: u32) -> Option<Instruction> {
    #[bitmatch]
    let "?? U ?????? hhhh bbb ooooo ? NNNNN DDDDD" = insn;

    enum Ty {
        // s0, s1 = d
        D(u32),
        // s0, s1 = leading_zeros(immh), shift based on s0, s1
        A,
        // s1 = leading_zeros(immg), s0 = s1 - 1, shift based on s1
        B,
        // s0, s1 = leading_zeros(immh), shift based on s0, s1
        C,
    }

    if h == 0 {
        return None;
    }

    let (ty, mnemonic) = match (U, o) {
        (0, 0b00000) if h & 0b1000 != 0 => (Ty::D(128 - ((h << 3) | b)), SimdMnemonic::sshr),
        (0, 0b00010) if h & 0b1000 != 0 => (Ty::D(128 - ((h << 3) | b)), SimdMnemonic::ssra),
        (0, 0b00100) if h & 0b1000 != 0 => (Ty::D(128 - ((h << 3) | b)), SimdMnemonic::srshr),
        (0, 0b00110) if h & 0b1000 != 0 => (Ty::D(128 - ((h << 3) | b)), SimdMnemonic::srsra),
        (0, 0b01010) if h & 0b1000 != 0 => (Ty::D(((h << 3) | b) - 64), SimdMnemonic::shl),
        (0, 0b01110) => (Ty::A, SimdMnemonic::sqshl),
        (0, 0b10010) => (Ty::B, SimdMnemonic::sqshrn),
        (0, 0b10011) => (Ty::B, SimdMnemonic::sqrshrn),
        (0, 0b11100) => (Ty::C, SimdMnemonic::scvtf),
        (0, 0b11111) => (Ty::C, SimdMnemonic::fcvtzs),
        (1, 0b00000) if h & 0b1000 != 0 => (Ty::D(128 - ((h << 3) | b)), SimdMnemonic::ushr),
        (1, 0b00010) if h & 0b1000 != 0 => (Ty::D(128 - ((h << 3) | b)), SimdMnemonic::usra),
        (1, 0b00100) if h & 0b1000 != 0 => (Ty::D(128 - ((h << 3) | b)), SimdMnemonic::urshr),
        (1, 0b00110) if h & 0b1000 != 0 => (Ty::D(128 - ((h << 3) | b)), SimdMnemonic::ursra),
        (1, 0b01000) if h & 0b1000 != 0 => (Ty::D(128 - ((h << 3) | b)), SimdMnemonic::sri),
        (1, 0b01010) if h & 0b1000 != 0 => (Ty::D(((h << 3) | b) - 64), SimdMnemonic::sli),
        (1, 0b01100) => (Ty::A, SimdMnemonic::sqshlu),
        (1, 0b01110) => (Ty::A, SimdMnemonic::uqshl),
        (1, 0b10000) => (Ty::B, SimdMnemonic::sqshrun),
        (1, 0b10001) => (Ty::B, SimdMnemonic::sqrshrun),
        (1, 0b10010) => (Ty::B, SimdMnemonic::uqshrn),
        (1, 0b10011) => (Ty::B, SimdMnemonic::uqrshrn),
        (1, 0b11100) => (Ty::C, SimdMnemonic::ucvtf),
        (1, 0b11111) => (Ty::C, SimdMnemonic::fcvtzu),
        _ => return None,
    };

    let (s0, s1, shift) = match ty {
        Ty::D(shift) => (3, 3, shift),
        Ty::A => {
            let s = 31 - h.leading_zeros();
            let i = ((h << 3) | b) - (8 << s);
            (s, s, i)
        }
        Ty::B => {
            let s = 32 - h.leading_zeros();
            if s == 0 {
                return None;
            }
            let i = (8 << s) - ((h << 3) | b);
            (s - 1, s, i)
        }
        Ty::C => {
            let s = 31 - h.leading_zeros();
            if s == 0 {
                return None;
            }
            let i = (16 << s) - ((h << 3) | b);
            (s, s, i)
        }
    };

    let Rd = make_operand(0, s0, D as _)?;
    let Rn = make_operand(0, s1, N as _)?;
    let shift = Operand::Imm(shift as _);

    Some(Instruction {
        mnemonic: Mnemonic::Simd(mnemonic),
        operands: [Some(Rd), Some(Rn), Some(shift), None],
    })
}

#[bitmatch]
fn decode_simd_scalar_x_index(insn: u32) -> Option<Instruction> {
    #[bitmatch]
    let "?? U ????? ss L m MMMM oooo H ? NNNNN DDDDD" = insn;

    let (s0, s1, s2, mnemonic) = match (U, s, o) {
        (0, _, 0b0011) => (s + 1, s, s, SimdMnemonic::sqdmlal),
        (0, _, 0b0111) => (s + 1, s, s, SimdMnemonic::sqdmlsl),
        (0, _, 0b1011) => (s + 1, s, s, SimdMnemonic::sqdmull),
        (0, _, 0b1100) => (s, s, s, SimdMnemonic::sqdmulh),
        (0, _, 0b1101) => (s, s, s, SimdMnemonic::sqrdmulh),
        (0, 0, 0b0001) => (1, 1, 1, SimdMnemonic::fmla),
        (0, 2 | 3, 0b0001) => (s, s, s, SimdMnemonic::fmla),
        (0, 0, 0b0101) => (1, 1, 1, SimdMnemonic::fmls),
        (0, 2 | 3, 0b0101) => (s, s, s, SimdMnemonic::fmls),
        (0, 0, 0b1001) => (1, 1, 1, SimdMnemonic::fmul),
        (0, 2 | 3, 0b1001) => (s, s, s, SimdMnemonic::fmul),
        (1, 0, 0b1001) => (1, 1, 1, SimdMnemonic::fmulx),
        (1, 2 | 3, 0b1001) => (s, s, s, SimdMnemonic::fmulx),
        _ => return None,
    };

    let Rd = make_operand(0, s0, D as _)?;
    let Rn = make_operand(0, s1, N as _)?;
    let (Rm, index) = match s2 {
        0b01 => (VectorSingle::h(M as _), m | (L << 1) | (H << 2)),
        0b10 => (VectorSingle::s(M as _), L | (H << 1)),
        0b11 => (VectorSingle::d(M as _), H),
        _ => return None,
    };
    let Rm = Operand::VSingleIndex(Rm, index as _);

    Some(Instruction {
        mnemonic: Mnemonic::Simd(mnemonic),
        operands: [Some(Rd), Some(Rn), Some(Rm), None],
    })
}

#[bitmatch]
fn decode_simd_table_lookup(insn: u32) -> Option<Instruction> {
    #[bitmatch]
    let "? Q ?????? OO ? MMMMM ? ll o ?? NNNNN DDDDD" = insn;

    if O != 0 {
        return None;
    }

    let mnemonic = match o != 0 {
        false => SimdMnemonic::tbl,
        true => SimdMnemonic::tbx,
    };
    let op = match Q != 0 {
        false => VectorMulti::v8b,
        true => VectorMulti::v16b,
    };
    let Rd = Operand::VMulti(op(D as _, None));
    let Rm = Operand::VMulti(op(M as _, None));
    let base = VectorMulti::v16b(N as _, None);
    let Rn = Operand::VMultiGroup(VectorMultiGroup {
        base,
        size: l as usize + 1,
    });

    Some(Instruction {
        mnemonic: Mnemonic::Simd(mnemonic),
        operands: [Some(Rd), Some(Rn), Some(Rm), None],
    })
}

#[bitmatch]
fn decode_simd_permute(insn: u32) -> Option<Instruction> {
    #[bitmatch]
    let "? Q ?????? ss ? MMMMM ? ooo ?? NNNNN DDDDD" = insn;

    let mnemonic = match o {
        0b001 => SimdMnemonic::uzp1,
        0b010 => SimdMnemonic::trn1,
        0b011 => SimdMnemonic::zip1,
        0b101 => SimdMnemonic::uzp2,
        0b110 => SimdMnemonic::trn2,
        0b111 => SimdMnemonic::zip2,
        _ => return None,
    };

    let Rd = decode_vt(s, Q, D as _)?;
    let Rn = decode_vt(s, Q, N as _)?;
    let Rm = decode_vt(s, Q, M as _)?;

    Some(Instruction {
        mnemonic: Mnemonic::Simd(mnemonic),
        operands: [Some(Rd), Some(Rn), Some(Rm), None],
    })
}

#[bitmatch]
fn decode_simd_extract(insn: u32) -> Option<Instruction> {
    #[bitmatch]
    let "? Q ?????? oo ? MMMMM ? iiii ? NNNNN DDDDD" = insn;
    if o != 0 {
        return None;
    }
    if Q == 0 && i & (1 << 3) != 0 {
        return None;
    }
    let op = match Q != 0 {
        false => VectorMulti::v8b,
        true => VectorMulti::v16b,
    };
    let Rd = Operand::VMulti(op(D as _, None));
    let Rn = Operand::VMulti(op(N as _, None));
    let Rm = Operand::VMulti(op(M as _, None));

    Some(Instruction {
        mnemonic: Mnemonic::Simd(SimdMnemonic::ext),
        operands: [Some(Rd), Some(Rn), Some(Rm), Some(Operand::Imm(i as _))],
    })
}

#[bitmatch]
fn decode_simd_copy(insn: u32) -> Option<Instruction> {
    #[bitmatch]
    let "? Q o ???????? IIIII ? iiii ? NNNNN DDDDD" = insn;
    let D = D as u8;
    let N = N as u8;

    enum Ty {
        // <V><d>, <Vn>.<Ts>[<index>]
        A,
        // <Vd>.<T>, <R><n>
        B,
        // <R><d>, <Vn>.<Ts>[<index>]
        C,
        // <Vn>.<Ts>[<index>], <R><d>
        D,
        // <Vd>.<Ts>[<index1>], <Vn>.<Ts>[<index2>]
        E,
    }

    let (ty, mnemonic) = match (Q, o, I, i) {
        (_, 0, _, 0b0000) => (Ty::A, SimdMnemonic::dup),
        (_, 0, _, 0b0001) => (Ty::B, SimdMnemonic::dup),
        (_, 0, _, 0b0101) => (Ty::C, SimdMnemonic::smov),
        (_, 0, _, 0b0111) => (Ty::C, SimdMnemonic::umov),
        (1, 0, _, 0b0011) => (Ty::D, SimdMnemonic::mov),
        (1, 1, _, _) => (Ty::E, SimdMnemonic::mov),
        _ => todo!(),
    };

    let size = I.trailing_zeros();
    let (op0, op1) = match ty {
        Ty::A => (decode_vt(size, Q, D)?, decode_imm5_vtsi(size, I >> 1, N)?),
        Ty::B => (
            decode_vt(size, Q, D)?,
            if size == 3 {
                Operand::X(N)
            } else {
                Operand::W(N)
            },
        ),
        Ty::C => (
            if Q != 0 { Operand::X(D) } else { Operand::W(D) },
            decode_imm5_vtsi(size, I >> 1, N)?,
        ),
        Ty::D => (
            decode_imm5_vtsi(size, I >> 1, D)?,
            if Q != 0 { Operand::X(N) } else { Operand::W(N) },
        ),
        Ty::E => {
            let Rd = decode_imm5_vtsi(size, I >> 1, D)?;
            let Rn = decode_imm5_vtsi(size, i, D)?;
            (Rd, Rn)
        }
    };

    let mnemonic = match (mnemonic, size) {
        (SimdMnemonic::umov, 2 | 3) => SimdMnemonic::mov,
        _ => mnemonic,
    };

    Some(Instruction {
        mnemonic: Mnemonic::Simd(mnemonic),
        operands: [Some(op0), Some(op1), None, None],
    })
}

fn decode_imm5_vtsi(size: u32, i: u32, R: u8) -> Option<Operand> {
    let (op, index): (fn(_) -> _, _) = match size {
        0 => (VectorSingle::b, i >> 0),
        1 => (VectorSingle::h, i >> 1),
        2 => (VectorSingle::s, i >> 2),
        3 => (VectorSingle::d, i >> 3),
        _ => return None,
    };
    Some(Operand::VSingleIndex(op(R), index as _))
}

fn decode_vt(size: u32, Q: u32, R: u8) -> Option<Operand> {
    let op = match (size, Q) {
        (0, 0) => VectorMulti::v8b,
        (0, 1) => VectorMulti::v16b,
        (1, 0) => VectorMulti::v4h,
        (1, 1) => VectorMulti::v8h,
        (2, 0) => VectorMulti::v2s,
        (2, 1) => VectorMulti::v4s,
        (3, 1) => VectorMulti::v2d,
        _ => return None,
    };
    Some(Operand::VMulti(op(R, None)))
}

fn make_operand(vector: u32, size: u32, R: u8) -> Option<Operand> {
    match (vector, size) {
        (1, _) => todo!(),
        (0, 0) => Some(Operand::VMulti(VectorMulti::b(R))),
        (0, 1) => Some(Operand::VMulti(VectorMulti::h(R))),
        (0, 2) => Some(Operand::VMulti(VectorMulti::s(R))),
        (0, 3) => Some(Operand::VMulti(VectorMulti::d(R))),
        _ => None,
    }
}
