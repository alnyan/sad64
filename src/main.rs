#![feature(os_str_display)]
use std::{fs, path::Path};

use elf::{
    abi::{PF_X, PT_LOAD},
    endian::AnyEndian,
    segment::ProgramHeader,
    ElfBytes,
};
use sad64::{decode, Formatter, SimpleFormatter};

const SKIP: &[u64] = &[
    0x400800a0,
    0x400800a4,
    0xFFFFFF80400c58a8,
    0xFFFFFF80400c58ac,
    0xFFFFFF80400c5a50,
    0xFFFFFF80400c5a54,
];

fn dump(name: &str, elf: &mut ElfBytes<AnyEndian>, phdr: &ProgramHeader) -> Result<(), String> {
    let mut f = SimpleFormatter;
    let data = elf.segment_data(phdr).unwrap();
    let len = data.len() / size_of::<u32>();

    println!("Decode file: {:?}", name);

    for i in 0..len {
        let addr = phdr.p_vaddr + i as u64 * 4;
        let mut bytes = [0; 4];
        bytes.copy_from_slice(&data[i * 4..i * 4 + 4]);
        let word = u32::from_ne_bytes(bytes);
        print!("{:08x}: ", addr);
        if SKIP.contains(&addr) {
            println!(".word {:#010x}", word);
        } else {
            let insn = decode(word)
                .ok_or_else(|| format!("Error decoding instruction in file: {:?}", name))?;

            f.write_insn(&insn);
        }
    }

    Ok(())
}

fn main() {
    for entry in fs::read_dir("bins").unwrap() {
        let entry = entry.unwrap();
        if entry.file_type().unwrap().is_file() {
            let name = entry.file_name().to_str().unwrap().to_owned();

            let file = std::fs::read(entry.path()).unwrap();
            let mut elf = ElfBytes::<AnyEndian>::minimal_parse(&file).unwrap();

            for phdr in elf.segments().unwrap() {
                if phdr.p_type == PT_LOAD && phdr.p_flags & PF_X == PF_X {
                    dump(&name, &mut elf, &phdr).unwrap();
                }
            }
        }
    }

    // let code = &[
    //     0x9100347f, 0x910037e3, 0x9100007f, 0x910003e3, 0x700003c1, 0x30fffc21, 0x92400c20,
    //     0xb27f0841, 0xd2400462, 0xf27e0483, 0x92824681, 0xf2824681, 0xd2824681, 0x92a24681,
    //     0xf2a24681, 0xd2a24681, 0x92a00001, 0xf2a00001, 0xd2a00001,
    // ];
    // let mut f = SimpleFormatter;
    // for &word in code {
    //     let insn = decode(word).unwrap();
    //     f.write_insn(&insn);
    // }
}
