use std::env;
use asm_simulator::processor::Processor;
use asm_simulator::parser::{ parse, assemble};
use std::fs::File;
use std::io::Read;

const GREETER: &str = r"
asm-simulator v0.1.0
Created by K.Takahashi
";

fn main() {
    println!("{}", GREETER);

    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        eprintln!("No input file specified!");
        return;
    }
    let in_file_path = args.get(1).unwrap();
    let mut in_file = File::open(in_file_path).expect("File not found!");

    let mut program = String::new();
    in_file
        .read_to_string(&mut program)
        .expect("Could not read the file!");

    println!("===== INPUT PROGRAM =====\n{}", program);
    println!("=========================");

    let program_list = parse(program.as_str());
    println!("===== SYMBOLS TABLE =====");
    for (label, val) in program_list.get_symbol_table() {
        print!("{}\t", label);

        match val {
            Some(v) => {
                println!("{:04X}", v);
            },
            None => {
                println!(" N/A");
            }
        }
    }
    println!("=========================");

    let bytes = assemble(&program_list);
    let prog_len_in_bytes = bytes.len();
    println!("======== OBJECTS ========");
    for (i, byte) in bytes.iter().enumerate() {
        println!("{0:04X}\t{1:08b}\t{1:02X}", i, byte);
    }
    println!("=========================");

    let mut processor = Processor::new();
    println!("===== INITIAL STATE =====\n{}", processor.registers.show());
    println!("=========================");

    processor.extract_into_memory(bytes);
    processor.execute();

    println!("===== CURRENT STATE =====\n{}", processor.registers.show());
    println!("=========================");

    println!();
    println!("{}", processor.memory.show(0, prog_len_in_bytes as u16));
}
