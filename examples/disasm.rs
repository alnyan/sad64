#![feature(os_str_display)]

use std::{env::args, fmt};

use elf::{
    abi::{SHF_EXECINSTR, SHN_UNDEF, SHT_PROGBITS},
    endian::AnyEndian,
    ElfBytes,
};
use rangemap::RangeMap;
use sad64::{decode, Formatter, SimpleFormatter, SymbolResolver};

pub struct StdoutOutput;

impl fmt::Write for StdoutOutput {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        print!("{}", s);
        Ok(())
    }
}

pub struct Resolver<'a> {
    symbol_table: &'a RangeMap<u64, String>,
}

impl<'a> SymbolResolver for Resolver<'a> {
    fn resolve(&self, address: u64) -> Option<(&str, u64)> {
        let (range, name) = self.symbol_table.get_key_value(&address)?;
        assert!(address >= range.start);
        Some((name.as_str(), address - range.start))
    }
}

fn dump(name: &str, resolver: &Resolver, vaddr: u64, data: &[u8]) -> Result<(), String> {
    let mut f = SimpleFormatter;
    let len = data.len() / size_of::<u32>();

    println!("Section {}:", name);

    for i in 0..len {
        let addr = vaddr + i as u64 * 4;
        if let Some((location, offset)) = resolver.resolve(addr) {
            if offset == 0 {
                println!("{:#08x} <{}>:", addr, location);
            }
        }

        let mut bytes = [0; 4];
        bytes.copy_from_slice(&data[i * 4..i * 4 + 4]);
        let word = u32::from_ne_bytes(bytes);
        print!("\t{:08x}: ", addr);

        if let Some(insn) = decode(word) {
            f.write_insn(&mut StdoutOutput, addr, resolver, &insn).ok();
            println!();
        } else {
            println!(".word {:#010x}", word);
        }
    }

    Ok(())
}

fn main() {
    let args = args().collect::<Vec<_>>();
    if args.len() != 2 {
        todo!();
    }

    let file = std::fs::read(&args[1]).unwrap();
    let elf = ElfBytes::<AnyEndian>::minimal_parse(&file).unwrap();
    let (shdrs, strtab) = elf.section_headers_with_strtab().unwrap();
    let (symtab, symstrtab) = elf.symbol_table().unwrap().unwrap();
    let shdrs = shdrs.unwrap();
    let strtab = strtab.unwrap();
    let mut symbol_table = RangeMap::new();

    println!("Disassembling file: {:?}", args[1]);

    for symbol in symtab {
        if symbol.st_shndx == SHN_UNDEF || symbol.st_size == 0 {
            continue;
        }
        let name = symstrtab.get(symbol.st_name as _).unwrap();
        let start = symbol.st_value;
        let end = symbol.st_value + symbol.st_size;
        println!("{}: {:#x?}", name, start..end);
        symbol_table.insert(start..end, name.to_owned());
    }

    let resolver = Resolver {
        symbol_table: &symbol_table,
    };

    for section in shdrs {
        if section.sh_type == SHT_PROGBITS && section.sh_flags & (SHF_EXECINSTR as u64) != 0 {
            let name = strtab.get(section.sh_name as _).unwrap();
            let (data, _) = elf.section_data(&section).unwrap();
            dump(name, &resolver, section.sh_addr, data).unwrap();
        }
    }
}
