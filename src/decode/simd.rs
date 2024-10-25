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
        "0??0 111 0? ?100 00 ? ????10 ??????????" => decode_2reg_misc(insn),
        "0??0 111 0? ?110 00 ? ????10 ??????????" => decode_across_lanes(insn),
        "0??0 111 0? ?1?? ?? ? ????10 ??????????" => todo!(),
        "0??0 111 0? ?1?? ?? ? ?????? ??????????" => decode_3diff_same(insn),
        "0??0 111 10 0000 ?? ? ?????1 ??????????" => decode_simd_modified_imm(insn),
        "0??0 111 10 ???? ?? ? ?????1 ??????????" => decode_simd_shifted_imm(insn),
        "0??0 111 1? ???? ?? ? ?????0 ??????????" => decode_simd_vector_x_index(insn),
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

#[bitmatch]
fn decode_2reg_misc(insn: u32) -> Option<Instruction> {
    #[bitmatch]
    let "? Q U ????? ss ????? ooooo ?? NNNNN DDDDD" = insn;
    let D = D as u8;
    let N = N as u8;

    enum Ty {
        A, // <Vd>.<T>, <Vn>.<T>
        B, // <Vd>.<Ta>, <Vn>.<Tb> (where Tb = Ta * 2 with lower size)
        C, // <Vd>.<Ta>, <Vn>.<Tb> (where Tb = Ta with lower size)
        D, // <Vd>.<Ta>, <Vn>.<Tb> (where Ta = Tb * 2 with lower size)
        E, // <Vd>.<Ta>, <Vn>.<Tb> (where Ta = Tb with lower size)
    }

    let (ty, imm, min, max, sz, mnemonic) = match (U, s, o) {
        (0, _, 0b00000) => (Ty::A, 0, 0, 2, s, SimdMnemonic::rev64),
        (0, _, 0b00001) => (Ty::A, 0, 0, 0, s, SimdMnemonic::rev16),
        (0, _, 0b00010) => (Ty::B, 0, 0, 2, s, SimdMnemonic::saddlp),
        (0, _, 0b00011) => (Ty::A, 0, 0, 3, s, SimdMnemonic::suqadd),
        (0, _, 0b00100) => (Ty::A, 0, 0, 2, s, SimdMnemonic::cls),
        (0, _, 0b00101) => (Ty::A, 0, 0, 0, s, SimdMnemonic::cnt),
        (0, _, 0b00110) => (Ty::B, 0, 0, 2, s, SimdMnemonic::sadalp),
        (0, _, 0b00111) => (Ty::A, 0, 0, 3, s, SimdMnemonic::sqabs),
        (0, _, 0b01000) => (Ty::A, 1, 0, 3, s, SimdMnemonic::cmgt),
        (0, _, 0b01001) => (Ty::A, 1, 0, 3, s, SimdMnemonic::cmeq),
        (0, _, 0b01010) => (Ty::A, 1, 0, 3, s, SimdMnemonic::cmlt),
        (0, _, 0b01011) => (Ty::A, 0, 0, 3, s, SimdMnemonic::abs),
        (0, _, 0b10010) if Q == 0 => (Ty::C, 0, 0, 2, s, SimdMnemonic::xtn),
        (0, _, 0b10010) if Q == 1 => (Ty::D, 0, 0, 2, s, SimdMnemonic::xtn2),
        (0, _, 0b10100) if Q == 0 => (Ty::C, 0, 0, 2, s, SimdMnemonic::sqxtn),
        (0, _, 0b10100) if Q == 1 => (Ty::D, 0, 0, 2, s, SimdMnemonic::sqxtn2),
        (0, 0 | 1, 0b10110) if Q == 0 => (Ty::C, 0, 0, 1, s + 1, SimdMnemonic::fcvtn),
        (0, 0 | 1, 0b10110) if Q == 1 => (Ty::D, 0, 0, 1, s + 1, SimdMnemonic::fcvtn2),
        (0, 0 | 1, 0b10111) if Q == 0 => (Ty::E, 0, 0, 1, s + 1, SimdMnemonic::fcvtl),
        (0, 0 | 1, 0b10111) if Q == 1 => (Ty::B, 0, 0, 1, s + 1, SimdMnemonic::fcvtl2),
        (0, 0 | 1, 0b11000) => (Ty::A, 0, 0, 1, s + 2, SimdMnemonic::frintn),
        (0, 0 | 1, 0b11001) => (Ty::A, 0, 0, 1, s + 2, SimdMnemonic::frintm),
        (0, 0 | 1, 0b11010) => (Ty::A, 0, 0, 1, s, SimdMnemonic::fcvtns),
        (0, 0 | 1, 0b11011) => (Ty::A, 0, 0, 1, s, SimdMnemonic::fcvtms),
        (0, 0 | 1, 0b11100) => (Ty::A, 0, 0, 1, s, SimdMnemonic::fcvtas),
        (0, 0 | 1, 0b11101) => (Ty::A, 0, 0, 1, s, SimdMnemonic::scvtf),
        (0, 2 | 3, 0b01100) => (Ty::A, 2, 2, 3, s, SimdMnemonic::fcmgt),
        (0, 2 | 3, 0b01101) => (Ty::A, 2, 2, 3, s, SimdMnemonic::fcmeq),
        (0, 2 | 3, 0b01110) => (Ty::A, 2, 2, 3, s, SimdMnemonic::fcmlt),
        (0, 2 | 3, 0b01111) => (Ty::A, 0, 2, 3, s, SimdMnemonic::fabs),
        (0, 2 | 3, 0b11000) => (Ty::A, 0, 2, 3, s, SimdMnemonic::frintp),
        (0, 2 | 3, 0b11001) => (Ty::A, 0, 2, 3, s, SimdMnemonic::frintz),
        (0, 2 | 3, 0b11010) => (Ty::A, 0, 2, 3, s, SimdMnemonic::fcvtps),
        (0, 2 | 3, 0b11011) => (Ty::A, 0, 2, 3, s, SimdMnemonic::fcvtzs),
        (0, 2 | 3, 0b11100) => (Ty::A, 0, 2, 3, s, SimdMnemonic::urecpe),
        (0, 2 | 3, 0b11101) => (Ty::A, 0, 2, 3, s, SimdMnemonic::frecpe),
        (1, _, 0b00000) => (Ty::A, 0, 0, 3, s, SimdMnemonic::rev32),
        (1, _, 0b00010) => (Ty::B, 0, 0, 2, s, SimdMnemonic::uaddlp),
        (1, _, 0b00011) => (Ty::A, 0, 0, 3, s, SimdMnemonic::usqadd),
        (1, _, 0b00100) => (Ty::A, 0, 0, 2, s, SimdMnemonic::clz),
        (1, _, 0b00110) => (Ty::B, 0, 0, 2, s, SimdMnemonic::uadalp),
        (1, _, 0b00111) => (Ty::A, 0, 0, 3, s, SimdMnemonic::sqneg),
        (1, _, 0b01000) => (Ty::A, 1, 0, 3, s, SimdMnemonic::cmge),
        (1, _, 0b01001) => (Ty::A, 1, 0, 3, s, SimdMnemonic::cmle),
        (1, _, 0b01011) => (Ty::A, 0, 0, 3, s, SimdMnemonic::neg),
        (1, _, 0b10010) if Q == 0 => (Ty::C, 0, 0, 2, s, SimdMnemonic::sqxtun),
        (1, _, 0b10010) if Q == 1 => (Ty::D, 0, 0, 2, s, SimdMnemonic::sqxtun2),
        (1, 0 | 1, 0b10110) if Q == 0 => (Ty::C, 0, 0, 2, s + 1, SimdMnemonic::fcvtxn),
        (1, 0 | 1, 0b10110) if Q == 1 => (Ty::D, 0, 0, 2, s + 1, SimdMnemonic::fcvtxn2),
        (1, 0 | 1, 0b11000) => (Ty::A, 0, 0, 1, s + 2, SimdMnemonic::frinta),
        (1, 0 | 1, 0b11001) => (Ty::A, 0, 0, 1, s + 2, SimdMnemonic::frintx),
        (1, 0 | 1, 0b11010) => (Ty::A, 0, 0, 1, s + 2, SimdMnemonic::fcvtnu),
        (1, 0 | 1, 0b11011) => (Ty::A, 0, 0, 1, s + 2, SimdMnemonic::fcvtmu),
        (1, 0 | 1, 0b11100) => (Ty::A, 0, 0, 1, s + 2, SimdMnemonic::fcvtau),
        (1, 0 | 1, 0b11101) => (Ty::A, 0, 0, 1, s + 2, SimdMnemonic::ucvtf),
        (1, 0, 0b00101) => (Ty::A, 0, 0, 0, 0, SimdMnemonic::mvn),
        (1, 1, 0b00101) => (Ty::A, 0, 1, 1, 0, SimdMnemonic::rbit),
        (1, 2 | 3, 0b01100) => (Ty::A, 2, 2, 3, s, SimdMnemonic::fcmge),
        (1, 2 | 3, 0b01101) => (Ty::A, 2, 2, 3, s, SimdMnemonic::fcmle),
        (1, 2 | 3, 0b01111) => (Ty::A, 0, 2, 3, s, SimdMnemonic::fneg),
        (1, 2 | 3, 0b11001) => (Ty::A, 0, 2, 3, s, SimdMnemonic::frinti),
        (1, 2 | 3, 0b11010) => (Ty::A, 0, 2, 3, s, SimdMnemonic::fcvtpu),
        (1, 2 | 3, 0b11011) => (Ty::A, 0, 2, 3, s, SimdMnemonic::fcvtzu),
        (1, 2 | 3, 0b11100) => (Ty::A, 0, 2, 3, s, SimdMnemonic::ursqrte),
        (1, 2 | 3, 0b11101) => (Ty::A, 0, 2, 3, s, SimdMnemonic::frsqrte),
        (1, 2 | 3, 0b11111) => (Ty::A, 0, 2, 3, s, SimdMnemonic::fsqrt),
        _ => todo!(),
    };

    if s < min || s > max {
        return None;
    }

    let (op0, op1) = match ty {
        Ty::A => (decode_vt(sz, Q, D)?, decode_vt(sz, Q, N)?),
        Ty::B => (decode_vt(sz + 1, Q, D)?, decode_vt(sz, Q, N)?),
        Ty::C => (decode_vt(sz, 0, D)?, decode_vt(sz + 1, 1, N)?),
        Ty::D => (decode_vt(sz, Q, D)?, decode_vt(sz + 1, Q, N)?),
        Ty::E => (decode_vt(sz + 1, 1, D)?, decode_vt(sz, 0, N)?),
    };
    let op2 = match imm {
        0 => None,
        1 => Some(Operand::DecImm(0)),
        2 => Some(Operand::FpImm(0.0)),
        _ => unreachable!(),
    };

    Some(Instruction {
        mnemonic: Mnemonic::Simd(mnemonic),
        operands: [Some(op0), Some(op1), op2, None],
    })
}

#[bitmatch]
fn decode_across_lanes(insn: u32) -> Option<Instruction> {
    #[bitmatch]
    let "? Q U ????? ss ????? ooooo ?? NNNNN DDDDD" = insn;

    let (s0, s1, mnemonic) = match (U, s, o) {
        (0, _, 0b00011) => (s + 1, s, SimdMnemonic::saddlv),
        (0, _, 0b01010) => (s, s, SimdMnemonic::smaxv),
        (0, _, 0b11010) => (s, s, SimdMnemonic::sminv),
        (0, _, 0b11011) => (s, s, SimdMnemonic::addv),
        (1, _, 0b00011) => (s + 1, s, SimdMnemonic::uaddlv),
        (1, _, 0b01010) => (s, s, SimdMnemonic::umaxv),
        (1, _, 0b11010) => (s, s, SimdMnemonic::uminv),
        (_, 0 | 1, 0b01100) => (U + 1, U + 1, SimdMnemonic::fmaxnmv),
        (_, 0 | 1, 0b01111) => (U + 1, U + 1, SimdMnemonic::fmaxv),
        (_, 2 | 3, 0b01100) => (U + 1, U + 1, SimdMnemonic::fminnmv),
        (_, 2 | 3, 0b01111) => (U + 1, U + 1, SimdMnemonic::fminv),
        _ => return None,
    };

    let Rd = make_operand(0, s0, D as _)?;
    let Rn = decode_vt(s1, Q, N as _)?;

    Some(Instruction {
        mnemonic: Mnemonic::Simd(mnemonic),
        operands: [Some(Rd), Some(Rn), None, None],
    })
}

#[bitmatch]
fn decode_3diff_same(insn: u32) -> Option<Instruction> {
    // 0 Q U 01110 ss 1 MMMMM oooo 00 NNNNN DDDDD
    // 0 Q U 01110 ss 1 MMMMM oooo o1 NNNNN DDDDD
    #[bitmatch]
    let "? Q U ????? ss ? MMMMM oooooq NNNNN DDDDD" = insn;

    let (s0, s1, s2, q0, q1, q2, mnemonic) = match (U, s, o, q) {
        // !U, !q
        (0, _, 0b00000, 0) if Q == 0 => (s + 1, s, s, 1, Q, Q, SimdMnemonic::saddl),
        (0, _, 0b00000, 0) if Q == 1 => (s + 1, s, s, 1, Q, Q, SimdMnemonic::saddl2),
        (0, _, 0b00010, 0) if Q == 0 => (s + 1, s + 1, s, 1, 1, Q, SimdMnemonic::saddw),
        (0, _, 0b00010, 0) if Q == 1 => (s + 1, s + 1, s, 1, 1, Q, SimdMnemonic::saddw2),
        (0, _, 0b00100, 0) if Q == 0 => (s + 1, s, s, 1, Q, Q, SimdMnemonic::ssubl),
        (0, _, 0b00100, 0) if Q == 1 => (s + 1, s, s, 1, Q, Q, SimdMnemonic::ssubl2),
        (0, _, 0b00110, 0) if Q == 0 => (s + 1, s + 1, s, 1, 1, Q, SimdMnemonic::ssubw),
        (0, _, 0b00110, 0) if Q == 1 => (s + 1, s + 1, s, 1, 1, Q, SimdMnemonic::ssubw2),
        (0, _, 0b01000, 0) if Q == 0 => (s, s + 1, s + 1, Q, 1, 1, SimdMnemonic::addhn),
        (0, _, 0b01000, 0) if Q == 1 => (s, s + 1, s + 1, Q, 1, 1, SimdMnemonic::addhn2),
        (0, _, 0b01010, 0) if Q == 0 => (s + 1, s, s, 1, Q, Q, SimdMnemonic::sabal),
        (0, _, 0b01010, 0) if Q == 1 => (s + 1, s, s, 1, Q, Q, SimdMnemonic::sabal2),
        (0, _, 0b01100, 0) if Q == 0 => (s, s + 1, s + 1, Q, 1, 1, SimdMnemonic::subhn),
        (0, _, 0b01100, 0) if Q == 1 => (s, s + 1, s + 1, Q, 1, 1, SimdMnemonic::subhn2),
        (0, _, 0b01110, 0) if Q == 0 => (s + 1, s, s, 1, Q, Q, SimdMnemonic::sabdl),
        (0, _, 0b01110, 0) if Q == 1 => (s + 1, s, s, 1, Q, Q, SimdMnemonic::sabdl2),
        (0, _, 0b10000, 0) if Q == 0 => (s + 1, s, s, 1, Q, Q, SimdMnemonic::smlal),
        (0, _, 0b10000, 0) if Q == 1 => (s + 1, s, s, 1, Q, Q, SimdMnemonic::smlal2),
        (0, _, 0b10010, 0) if Q == 0 => (s + 1, s, s, 1, Q, Q, SimdMnemonic::sqdmlal),
        (0, _, 0b10010, 0) if Q == 1 => (s + 1, s, s, 1, Q, Q, SimdMnemonic::sqdmlal2),
        (0, _, 0b10100, 0) if Q == 0 => (s + 1, s, s, 1, Q, Q, SimdMnemonic::smlsl),
        (0, _, 0b10100, 0) if Q == 1 => (s + 1, s, s, 1, Q, Q, SimdMnemonic::smlsl2),
        (0, _, 0b10110, 0) if Q == 0 => (s + 1, s, s, 1, Q, Q, SimdMnemonic::sqdmlsl),
        (0, _, 0b10110, 0) if Q == 1 => (s + 1, s, s, 1, Q, Q, SimdMnemonic::sqdmlsl2),
        (0, _, 0b11000, 0) if Q == 0 => (s + 1, s, s, Q, Q, Q, SimdMnemonic::smull),
        (0, _, 0b11000, 0) if Q == 1 => (s + 1, s, s, Q, Q, Q, SimdMnemonic::smull2),
        (0, _, 0b11010, 0) if Q == 0 => (s + 1, s, s, 1, Q, Q, SimdMnemonic::sqdmull),
        (0, _, 0b11010, 0) if Q == 1 => (s + 1, s, s, 1, Q, Q, SimdMnemonic::sqdmull2),
        (0, _, 0b11100, 0) if Q == 0 => (s + 1, s, s, 1, Q, Q, SimdMnemonic::pmull),
        (0, _, 0b11100, 0) if Q == 1 => (s + 1, s, s, 1, Q, Q, SimdMnemonic::pmull2),
        // U, !q
        (1, _, 0b00000, 0) if Q == 0 => (s, s, s, Q, Q, Q, SimdMnemonic::uaddl),
        (1, _, 0b00000, 0) if Q == 1 => (s, s, s, Q, Q, Q, SimdMnemonic::uaddl2),
        (1, _, 0b00010, 0) if Q == 0 => (s + 1, s + 1, s, 1, 1, Q, SimdMnemonic::uaddw),
        (1, _, 0b00010, 0) if Q == 1 => (s + 1, s + 1, s, 1, 1, Q, SimdMnemonic::uaddw2),
        (1, _, 0b00100, 0) if Q == 0 => (s + 1, s, s, 1, Q, Q, SimdMnemonic::usubl),
        (1, _, 0b00100, 0) if Q == 1 => (s + 1, s, s, 1, Q, Q, SimdMnemonic::usubl2),
        (1, _, 0b00110, 0) if Q == 0 => (s + 1, s + 1, s, 1, 1, Q, SimdMnemonic::usubw),
        (1, _, 0b00110, 0) if Q == 1 => (s + 1, s + 1, s, 1, 1, Q, SimdMnemonic::usubw2),
        (1, _, 0b01000, 0) if Q == 0 => (s, s + 1, s + 1, Q, 1, 1, SimdMnemonic::raddhn),
        (1, _, 0b01000, 0) if Q == 1 => (s, s + 1, s + 1, Q, 1, 1, SimdMnemonic::raddhn2),
        (1, _, 0b01010, 0) if Q == 0 => (s + 1, s, s, 1, Q, Q, SimdMnemonic::uabal),
        (1, _, 0b01010, 0) if Q == 1 => (s + 1, s, s, 1, Q, Q, SimdMnemonic::uabal2),
        (1, _, 0b01100, 0) if Q == 0 => (s, s + 1, s + 1, Q, 1, 1, SimdMnemonic::rsubhn),
        (1, _, 0b01100, 0) if Q == 1 => (s, s + 1, s + 1, Q, 1, 1, SimdMnemonic::rsubhn2),
        (1, _, 0b01110, 0) if Q == 0 => (s + 1, s, s, 1, Q, Q, SimdMnemonic::uabdl),
        (1, _, 0b01110, 0) if Q == 1 => (s + 1, s, s, 1, Q, Q, SimdMnemonic::uabdl2),
        (1, _, 0b10000, 0) if Q == 0 => (s + 1, s, s, 1, Q, Q, SimdMnemonic::umlal),
        (1, _, 0b10000, 0) if Q == 1 => (s + 1, s, s, 1, Q, Q, SimdMnemonic::umlal2),
        (1, _, 0b10100, 0) if Q == 0 => (s + 1, s, s, 1, Q, Q, SimdMnemonic::umlsl),
        (1, _, 0b10100, 0) if Q == 1 => (s + 1, s, s, 1, Q, Q, SimdMnemonic::umlsl2),
        (1, _, 0b11000, 0) if Q == 0 => (s + 1, s, s, Q, Q, Q, SimdMnemonic::umull),
        (1, _, 0b11000, 0) if Q == 1 => (s + 1, s, s, Q, Q, Q, SimdMnemonic::umull2),
        // !U, q
        (0, _, 0b00000, 1) => (s, s, s, Q, Q, Q, SimdMnemonic::shadd),
        (0, _, 0b00001, 1) => (s, s, s, Q, Q, Q, SimdMnemonic::sqadd),
        (0, _, 0b00010, 1) => (s, s, s, Q, Q, Q, SimdMnemonic::srhadd),
        (0, _, 0b00100, 1) => (s, s, s, Q, Q, Q, SimdMnemonic::shsub),
        (0, _, 0b00101, 1) => (s, s, s, Q, Q, Q, SimdMnemonic::sqsub),
        (0, _, 0b00110, 1) => (s, s, s, Q, Q, Q, SimdMnemonic::cmgt),
        (0, _, 0b00111, 1) => (s, s, s, Q, Q, Q, SimdMnemonic::cmge),
        (0, _, 0b01000, 1) => (s, s, s, Q, Q, Q, SimdMnemonic::sshl),
        (0, _, 0b01001, 1) => (s, s, s, Q, Q, Q, SimdMnemonic::sqshl),
        (0, _, 0b01010, 1) => (s, s, s, Q, Q, Q, SimdMnemonic::srshl),
        (0, _, 0b01011, 1) => (s, s, s, Q, Q, Q, SimdMnemonic::sqrshl),
        (0, _, 0b01100, 1) => (s, s, s, Q, Q, Q, SimdMnemonic::smax),
        (0, _, 0b01101, 1) => (s, s, s, Q, Q, Q, SimdMnemonic::smin),
        (0, _, 0b01110, 1) => (s, s, s, Q, Q, Q, SimdMnemonic::sabd),
        (0, _, 0b01111, 1) => (s, s, s, Q, Q, Q, SimdMnemonic::saba),
        (0, _, 0b10000, 1) => (s, s, s, Q, Q, Q, SimdMnemonic::add),
        (0, _, 0b10001, 1) => (s, s, s, Q, Q, Q, SimdMnemonic::cmtst),
        (0, _, 0b10010, 1) => (s, s, s, Q, Q, Q, SimdMnemonic::mla),
        (0, _, 0b10011, 1) => (s, s, s, Q, Q, Q, SimdMnemonic::mul),
        (0, _, 0b10100, 1) => (s, s, s, Q, Q, Q, SimdMnemonic::smaxp),
        (0, _, 0b10101, 1) => (s, s, s, Q, Q, Q, SimdMnemonic::sminp),
        (0, _, 0b10110, 1) => (s, s, s, Q, Q, Q, SimdMnemonic::sqdmulh),
        (0, _, 0b10111, 1) => (s, s, s, Q, Q, Q, SimdMnemonic::addp),
        (0, 0 | 1, 0b11000, 1) => (s + 2, s + 2, s + 2, Q, Q, Q, SimdMnemonic::fmaxnm),
        (0, 0 | 1, 0b11001, 1) => (s + 2, s + 2, s + 2, Q, Q, Q, SimdMnemonic::fmla),
        (0, 0 | 1, 0b11010, 1) => (s + 2, s + 2, s + 2, Q, Q, Q, SimdMnemonic::fadd),
        (0, 0 | 1, 0b11011, 1) => (s + 2, s + 2, s + 2, Q, Q, Q, SimdMnemonic::fmulx),
        (0, 0 | 1, 0b11100, 1) => (s + 2, s + 2, s + 2, Q, Q, Q, SimdMnemonic::fcmeq),
        (0, 0 | 1, 0b11110, 1) => (s + 2, s + 2, s + 2, Q, Q, Q, SimdMnemonic::fmax),
        (0, 0 | 1, 0b11111, 1) => (s + 2, s + 2, s + 2, Q, Q, Q, SimdMnemonic::frecps),
        (0, 0, 0b00011, 1) => (0, 0, 0, Q, Q, Q, SimdMnemonic::and),
        (0, 1, 0b00011, 1) => (0, 0, 0, Q, Q, Q, SimdMnemonic::bic),
        (0, 2 | 3, 0b11000, 1) => (s, s, s, Q, Q, Q, SimdMnemonic::fminnm),
        (0, 2 | 3, 0b11001, 1) => (s, s, s, Q, Q, Q, SimdMnemonic::fmls),
        (0, 2 | 3, 0b11010, 1) => (s, s, s, Q, Q, Q, SimdMnemonic::fsub),
        (0, 2 | 3, 0b11110, 1) => (s, s, s, Q, Q, Q, SimdMnemonic::fmin),
        (0, 2 | 3, 0b11111, 1) => (s, s, s, Q, Q, Q, SimdMnemonic::frsqrts),
        (0, 2, 0b00011, 1) => (0, 0, 0, Q, Q, Q, SimdMnemonic::orr),
        (0, 3, 0b00011, 1) => (0, 0, 0, Q, Q, Q, SimdMnemonic::orn),
        // U, q
        (1, _, 0b00000, 1) => (s, s, s, Q, Q, Q, SimdMnemonic::uhadd),
        (1, _, 0b00001, 1) => (s, s, s, Q, Q, Q, SimdMnemonic::uqadd),
        (1, _, 0b00010, 1) => (s, s, s, Q, Q, Q, SimdMnemonic::urhadd),
        (1, _, 0b00100, 1) => (s, s, s, Q, Q, Q, SimdMnemonic::uhsub),
        (1, _, 0b00101, 1) => (s, s, s, Q, Q, Q, SimdMnemonic::uqsub),
        (1, _, 0b00110, 1) => (s, s, s, Q, Q, Q, SimdMnemonic::cmhi),
        (1, _, 0b00111, 1) => (s, s, s, Q, Q, Q, SimdMnemonic::cmhs),
        (1, _, 0b01000, 1) => (s, s, s, Q, Q, Q, SimdMnemonic::ushl),
        (1, _, 0b01001, 1) => (s, s, s, Q, Q, Q, SimdMnemonic::uqshl),
        (1, _, 0b01010, 1) => (s, s, s, Q, Q, Q, SimdMnemonic::urshl),
        (1, _, 0b01011, 1) => (s, s, s, Q, Q, Q, SimdMnemonic::uqrshl),
        (1, _, 0b01100, 1) => (s, s, s, Q, Q, Q, SimdMnemonic::umax),
        (1, _, 0b01101, 1) => (s, s, s, Q, Q, Q, SimdMnemonic::umin),
        (1, _, 0b01110, 1) => (s, s, s, Q, Q, Q, SimdMnemonic::uabd),
        (1, _, 0b01111, 1) => (s, s, s, Q, Q, Q, SimdMnemonic::uaba),
        (1, _, 0b10000, 1) => (s, s, s, Q, Q, Q, SimdMnemonic::sub),
        (1, _, 0b10001, 1) => (s, s, s, Q, Q, Q, SimdMnemonic::cmeq),
        (1, _, 0b10010, 1) => (s, s, s, Q, Q, Q, SimdMnemonic::mls),
        (1, _, 0b10011, 1) => (s, s, s, Q, Q, Q, SimdMnemonic::pmul),
        (1, _, 0b10100, 1) => (s, s, s, Q, Q, Q, SimdMnemonic::umaxp),
        (1, _, 0b10101, 1) => (s, s, s, Q, Q, Q, SimdMnemonic::uminp),
        (1, _, 0b10110, 1) => (s, s, s, Q, Q, Q, SimdMnemonic::sqrdmulh),
        (1, 0 | 1, 0b11000, 1) => (s + 2, s + 2, s + 2, Q, Q, Q, SimdMnemonic::fmaxnmp),
        (1, 0 | 1, 0b11010, 1) => (s + 2, s + 2, s + 2, Q, Q, Q, SimdMnemonic::faddp),
        (1, 0 | 1, 0b11011, 1) => (s + 2, s + 2, s + 2, Q, Q, Q, SimdMnemonic::fmul),
        (1, 0 | 1, 0b11100, 1) => (s + 2, s + 2, s + 2, Q, Q, Q, SimdMnemonic::fcmge),
        (1, 0 | 1, 0b11101, 1) => (s + 2, s + 2, s + 2, Q, Q, Q, SimdMnemonic::facge),
        (1, 0 | 1, 0b11110, 1) => (s + 2, s + 2, s + 2, Q, Q, Q, SimdMnemonic::fmaxp),
        (1, 0 | 1, 0b11111, 1) => (s + 2, s + 2, s + 2, Q, Q, Q, SimdMnemonic::fdiv),
        (1, 0, 0b00011, 1) => (0, 0, 0, Q, Q, Q, SimdMnemonic::eor),
        (1, 1, 0b00011, 1) => (0, 0, 0, Q, Q, Q, SimdMnemonic::bsl),
        (1, 2 | 3, 0b11000, 1) => (s, s, s, Q, Q, Q, SimdMnemonic::fminnmp),
        (1, 2 | 3, 0b11010, 1) => (s, s, s, Q, Q, Q, SimdMnemonic::fabd),
        (1, 2 | 3, 0b11100, 1) => (s, s, s, Q, Q, Q, SimdMnemonic::fcmgt),
        (1, 2 | 3, 0b11101, 1) => (s, s, s, Q, Q, Q, SimdMnemonic::facgt),
        (1, 2 | 3, 0b11110, 1) => (s, s, s, Q, Q, Q, SimdMnemonic::fminp),
        (1, 2, 0b00011, 1) => (0, 0, 0, Q, Q, Q, SimdMnemonic::bit),
        (1, 3, 0b00011, 1) => (0, 0, 0, Q, Q, Q, SimdMnemonic::bif),

        _ => return None,
    };

    let Rd = decode_vt(s0, q0, D as _)?;
    let Rn = decode_vt(s1, q1, N as _)?;
    let Rm = decode_vt(s2, q2, M as _)?;

    Some(Instruction {
        mnemonic: Mnemonic::Simd(mnemonic),
        operands: [Some(Rd), Some(Rn), Some(Rm), None],
    })
}

#[bitmatch]
fn decode_simd_modified_imm(insn: u32) -> Option<Instruction> {
    #[bitmatch]
    let "? Q o ?????????? a b c CCCC O ? d e f g h DDDDD" = insn;

    if C == 0b1111 && O == 0 {
        if o == 1 && Q != 1 {
            return None;
        }

        let B = (!b) & 1;
        let exp = ((B << 2) | (c << 1) | d) - 3;
        let mantissa = (16 + ((e << 3) | (f << 2) | (g << 1) | h)) / 16;
        let S = match a != 0 {
            false => 1.0,
            true => -1.0,
        };
        let value = S * (exp as f64).exp2() * (mantissa as f64);
        let Rd = decode_vt(o + 2, Q, D as _)?;

        return Some(Instruction {
            mnemonic: Mnemonic::Simd(SimdMnemonic::fmov),
            operands: [Some(Rd), Some(Operand::FpImm(value)), None, None],
        });
    }

    let (scalar, size, imm8, msl, mnemonic) = match (Q, o, C, O) {
        // movi 32
        (_, 0, _, 0) if C & 0b1001 == 0b0000 => (0, 2, 1, 0, SimdMnemonic::movi),
        // orr 32
        (_, 0, _, 0) if C & 0b1001 == 0b0001 => (0, 2, 1, 0, SimdMnemonic::orr),
        // movi 16
        (_, 0, _, 0) if C & 0b1101 == 0b1000 => (0, 1, 1, 0, SimdMnemonic::movi),
        // orr 16
        (_, 0, _, 0) if C & 0b1101 == 0b1001 => (0, 1, 1, 0, SimdMnemonic::orr),
        // movi 32 shifting
        (_, 0, _, 0) if C & 0b1110 == 0b1100 => (0, 2, 1, 1, SimdMnemonic::movi),
        // movi 8
        (_, 0, 0b1110, 0) => (0, 0, 1, 0, SimdMnemonic::movi),
        // mvni 32
        (_, 1, _, 0) if C & 0b1001 == 0b0000 => (0, 2, 1, 0, SimdMnemonic::mvni),
        // bic 32
        (_, 1, _, 0) if C & 0b1001 == 0b0001 => (0, 2, 1, 0, SimdMnemonic::bic),
        // mvni 16
        (_, 1, _, 0) if C & 0b1101 == 0b1000 => (0, 1, 1, 0, SimdMnemonic::mvni),
        // bic 16
        (_, 1, _, 0) if C & 0b1101 == 0b1001 => (0, 1, 1, 0, SimdMnemonic::bic),
        // mvni 32 shifting
        (_, 1, _, 0) if C & 0b1110 == 0b1100 => (0, 2, 1, 1, SimdMnemonic::mvni),
        // movi 64 scalar
        (0, 1, 0b1110, 0) => (1, 3, 0, 0, SimdMnemonic::movi),
        // movi 64 vector
        (1, 1, 0b1110, 0) => (0, 3, 0, 0, SimdMnemonic::movi),

        _ => return None,
    };

    let Rd = match scalar != 0 {
        false => decode_vt(size, Q, D as _)?,
        true => make_operand(0, size, D as _)?,
    };
    let imm = match imm8 != 0 {
        false => {
            let a = replicate_bit(a as _, 8) as u64;
            let b = replicate_bit(a as _, 8) as u64;
            let c = replicate_bit(a as _, 8) as u64;
            let d = replicate_bit(a as _, 8) as u64;
            let e = replicate_bit(a as _, 8) as u64;
            let f = replicate_bit(a as _, 8) as u64;
            let g = replicate_bit(a as _, 8) as u64;
            let h = replicate_bit(a as _, 8) as u64;

            h | (g << 8) | (f << 16) | (e << 24) | (d << 32) | (c << 40) | (b << 48) | (a << 56)
        }
        true => {
            (h | (g << 1) | (f << 2) | (e << 3) | (d << 4) | (c << 5) | (b << 6) | (a << 7)) as u64
        }
    };

    let shift = match size {
        0 => 0,
        1 => ((C >> 1) & 1) * 8,
        2 => ((C >> 1) & 3) * 8,
        3 => 0,
        _ => unreachable!(),
    };

    let shift = if msl != 0 {
        (shift != 0).then_some(Operand::Msl(shift as _))
    } else {
        (shift != 0).then_some(Operand::Lsl(shift as _))
    };

    Some(Instruction {
        mnemonic: Mnemonic::Simd(mnemonic),
        operands: [Some(Rd), Some(Operand::Imm(imm)), shift, None],
    })
}

#[bitmatch]
fn decode_simd_shifted_imm(insn: u32) -> Option<Instruction> {
    #[bitmatch]
    let "? Q U ?????? hhhh bbb ooooo ? NNNNN DDDDD" = insn;

    let (s, smode, mnemonic) = match (U, o) {
        (0, 0b00000) => (0, 0, SimdMnemonic::sshr),
        (0, 0b00010) => (0, 0, SimdMnemonic::ssra),
        (0, 0b00100) => (0, 0, SimdMnemonic::srshr),
        (0, 0b00110) => (0, 0, SimdMnemonic::srsra),
        (0, 0b01010) => (0, 1, SimdMnemonic::shl),
        (0, 0b01110) => (0, 1, SimdMnemonic::sqshl),
        (0, 0b10000) if Q == 0 => (1, 0, SimdMnemonic::shrn),
        (0, 0b10000) if Q == 1 => (1, 0, SimdMnemonic::shrn2),
        (0, 0b10001) if Q == 0 => (1, 0, SimdMnemonic::rshrn),
        (0, 0b10001) if Q == 1 => (1, 0, SimdMnemonic::rshrn2),
        (0, 0b10010) if Q == 0 => (1, 0, SimdMnemonic::sqshrn),
        (0, 0b10010) if Q == 1 => (1, 0, SimdMnemonic::sqshrn2),
        (0, 0b10011) if Q == 0 => (2, 0, SimdMnemonic::sqrshrn),
        (0, 0b10011) if Q == 1 => (2, 0, SimdMnemonic::sqrshrn2),
        (0, 0b10100) if Q == 0 => (2, 1, SimdMnemonic::sshll),
        (0, 0b10100) if Q == 1 => (2, 1, SimdMnemonic::sshll2),
        (0, 0b11100) => (0, 0, SimdMnemonic::scvtf),
        (0, 0b11111) => (0, 0, SimdMnemonic::fcvtzs),

        (1, 0b00000) => (0, 0, SimdMnemonic::ushr),
        (1, 0b00010) => (0, 0, SimdMnemonic::usra),
        (1, 0b00100) => (0, 0, SimdMnemonic::urshr),
        (1, 0b00110) => (0, 0, SimdMnemonic::ursra),
        (1, 0b01000) => (0, 0, SimdMnemonic::sri),
        (1, 0b01010) => (0, 1, SimdMnemonic::sli),
        (1, 0b01100) => (0, 1, SimdMnemonic::sqshlu),
        (1, 0b01110) => (0, 1, SimdMnemonic::uqshl),
        (1, 0b10000) if Q == 0 => (1, 0, SimdMnemonic::sqshrun),
        (1, 0b10000) if Q == 1 => (1, 0, SimdMnemonic::sqshrun2),
        (1, 0b10001) if Q == 0 => (1, 0, SimdMnemonic::sqrshrun),
        (1, 0b10001) if Q == 1 => (1, 0, SimdMnemonic::sqrshrun2),
        (1, 0b10010) if Q == 0 => (1, 0, SimdMnemonic::uqshrn),
        (1, 0b10010) if Q == 1 => (1, 0, SimdMnemonic::uqshrn2),
        (1, 0b10011) if Q == 0 => (1, 0, SimdMnemonic::uqrshrn),
        (1, 0b10011) if Q == 1 => (1, 0, SimdMnemonic::uqrshrn2),
        (1, 0b10100) if Q == 0 => (2, 1, SimdMnemonic::ushll),
        (1, 0b10100) if Q == 1 => (2, 1, SimdMnemonic::ushll2),
        (1, 0b11100) => (0, 0, SimdMnemonic::ucvtf),
        (1, 0b11111) => (0, 0, SimdMnemonic::fcvtzu),

        _ => todo!(),
    };

    let sz = 31 - h.leading_zeros();
    let (s0, s1, q0, q1) = match s {
        0 => (sz, sz, Q, Q),
        1 => (sz, sz + 1, Q, 1),
        2 => (sz + 1, sz, 1, Q),
        _ => unreachable!(),
    };
    let imm = (h << 3) | b;
    let imm = match (sz, smode) {
        (0, 0) => 16 - imm,
        (1, 0) => 32 - imm,
        (2, 0) => 64 - imm,
        (3, 0) => 128 - imm,
        (0, 1) => imm - 8,
        (1, 1) => imm - 16,
        (2, 1) => imm - 32,
        (3, 1) => imm - 64,
        _ => todo!(),
    };
    let Rd = decode_vt(s0, q0, D as _)?;
    let Rn = decode_vt(s1, q1, N as _)?;

    Some(Instruction {
        mnemonic: Mnemonic::Simd(mnemonic),
        operands: [Some(Rd), Some(Rn), Some(Operand::Imm(imm as _)), None],
    })
}

#[bitmatch]
fn decode_simd_vector_x_index(insn: u32) -> Option<Instruction> {
    #[bitmatch]
    let "? Q U ????? ss L m MMMM oooo H ? NNNNN DDDDD" = insn;

    enum Ty {
        A,
        B,
    }

    let (ty, mnemonic) = match (U, s, o) {
        (0, _, 0b0010) if Q == 0 => (Ty::A, SimdMnemonic::smlal),
        (0, _, 0b0010) if Q == 1 => (Ty::A, SimdMnemonic::smlal2),
        (0, _, 0b0011) if Q == 0 => (Ty::A, SimdMnemonic::sqdmlal),
        (0, _, 0b0011) if Q == 1 => (Ty::A, SimdMnemonic::sqdmlal2),
        (0, _, 0b0110) if Q == 0 => (Ty::A, SimdMnemonic::smlsl),
        (0, _, 0b0110) if Q == 1 => (Ty::A, SimdMnemonic::smlsl2),
        (0, _, 0b0111) if Q == 0 => (Ty::A, SimdMnemonic::sqdmlsl),
        (0, _, 0b0111) if Q == 1 => (Ty::A, SimdMnemonic::sqdmlsl2),
        (0, _, 0b1000) => (Ty::B, SimdMnemonic::mul),
        (0, _, 0b1010) if Q == 0 => (Ty::A, SimdMnemonic::smull),
        (0, _, 0b1010) if Q == 1 => (Ty::A, SimdMnemonic::smull2),
        (0, _, 0b1011) if Q == 0 => (Ty::A, SimdMnemonic::sqdmull),
        (0, _, 0b1011) if Q == 1 => (Ty::A, SimdMnemonic::sqdmull2),
        (0, _, 0b1100) => (Ty::B, SimdMnemonic::sqdmulh),
        (0, _, 0b1101) => (Ty::B, SimdMnemonic::sqrdmulh),

        (0, 2 | 3, 0b0001) => (Ty::B, SimdMnemonic::fmla),
        (0, 2 | 3, 0b0101) => (Ty::B, SimdMnemonic::fmls),
        (0, 2 | 3, 0b1001) => (Ty::B, SimdMnemonic::fmul),

        (1, _, 0b0000) => (Ty::B, SimdMnemonic::mla),
        (1, _, 0b0010) if Q == 0 => (Ty::A, SimdMnemonic::umlal),
        (1, _, 0b0010) if Q == 1 => (Ty::A, SimdMnemonic::umlal2),
        (1, _, 0b0100) => (Ty::B, SimdMnemonic::mls),
        (1, _, 0b0110) if Q == 0 => (Ty::A, SimdMnemonic::umlsl),
        (1, _, 0b0110) if Q == 1 => (Ty::A, SimdMnemonic::umlsl2),
        (1, _, 0b1010) if Q == 0 => (Ty::A, SimdMnemonic::umull),
        (1, _, 0b1010) if Q == 1 => (Ty::A, SimdMnemonic::umull2),
        (1, _, 0b1001) => (Ty::B, SimdMnemonic::fmulx),

        _ => return None,
    };

    let index = (H << 3) | (L << 2) | (m << 1);
    let (s0, s1, s2, q0, q1) = match ty {
        Ty::A => (s + 1, s, s, 1, Q),
        Ty::B => (s, s, s, Q, Q),
    };

    let Rd = decode_vt(s0, q0, D as _)?;
    let Rn = decode_vt(s1, q1, N as _)?;
    let Rm = decode_imm5_vtsi(s2, index, M as _)?;

    Some(Instruction {
        mnemonic: Mnemonic::Simd(mnemonic),
        operands: [Some(Rd), Some(Rn), Some(Rm), None],
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
        (3, 0) => VectorMulti::v1d,
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

fn replicate_bit(bit: u8, amount: usize) -> u32 {
    if bit == 0 {
        0
    } else {
        u32::MAX >> (32 - amount)
    }
}
