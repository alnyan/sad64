use bitmatch::bitmatch;

use crate::Instruction;

#[bitmatch]
pub fn decode_simd(insn: u32) -> Option<Instruction> {
    // let "aaaa ??? bb cccc dd ? eeeeee ??????????" = insn;
    #[bitmatch]
    match insn {
        // Cryptographic AES
        "0100 111 0? ?101 00 ? ????10 ??????????" => {
            todo!()
        }
        // Cryptographic 3-reg SHA
        "0101 111 0? ?0?? ?? ? 0???00 ??????????" => {
            todo!()
        }
        // Cryptographic 2-reg SHA
        "0101 111 0? ?101 00 ? ????10 ??????????" => {
            todo!()
        }
        // Advanced SIMD scalar copy
        "01?1 111 00 00?? ?? ? 0????1 ??????????" => {
            todo!()
        }
        // Advanced SIMD scalar 2-reg misc
        "01?1 111 0? ?100 00 ? ????10 ??????????" => {
            todo!()
        }
        // Advanced SIMD scalar pairwise
        "01?1 111 0? ?110 00 ? ????10 ??????????" => {
            todo!()
        }
        // Advanced SIMD scalar three different
        "01?1 111 0? ?1?? ?? ? ????00 ??????????" => {
            todo!()
        }
        // Advanced SIMD scalar three same
        "01?1 111 0? ?1?? ?? ? ?????1 ??????????" => {
            todo!()
        }
        // Advanced SIMD scalar shift by immediate
        "01?1 111 10 ???? ?? ? ?????1 ??????????" => {
            todo!()
        }
        // Advanced SIMD scalar x indexed element
        "01?1 111 1? ???? ?? ? ?????0 ??????????" => {
            todo!()
        }
        // Advanced SIMD table lookup
        "0?00 111 0? ?0?? ?? ? 0???00 ??????????" => {
            todo!()
        }
        // Advanced SIMD permute
        "0?00 111 0? ?0?? ?? ? 0???10 ??????????" => {
            todo!()
        }
        // Advanced SIMD extract
        "0?10 111 0? ?0?? ?? ? 0????0 ??????????" => {
            todo!()
        }
        // Advanced SIMD copy
        "0??0 111 00 00?? ?? ? 0????1 ??????????" => {
            todo!()
        }
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
