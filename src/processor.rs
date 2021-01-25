use super::register::{GeneralRegisters, RegisterPairs};
use crate::register::{RegisterSet, SpecialRegisters, FlagRegister};
use crate::memory::Memory;
use crate::parity::Parity;

pub struct Processor {
    pub registers: RegisterSet,
    pub memory: Memory
}

impl Processor {
    pub fn new() -> Processor {
        Processor {
            registers: RegisterSet::new(),
            memory: Memory::new()
        }
    }

    pub fn extract_into_memory(&mut self, program: Vec<u8>) {
        for (i, byte) in program.iter().enumerate() {
            self.memory.put(i as u16, *byte);
        }
    }

    pub fn execute(&mut self) {
        self.registers.put_sp(SpecialRegisters::SP, 20);
        loop {
            let instruction = self.fetch();

            // HLT - Halt
            if instruction == 0b01110110 {
                break;
            }
            // MOV - Move
            else if (instruction & 0b11000000) == 0b01000000 {
                let dest = (instruction & 0b00111000) >> 3;
                let src = instruction & 0b00000111;
                let addr = self.registers.readx(RegisterPairs::H);

                match (Processor::decode_reg(dest), Processor::decode_reg(src)) {
                    (Some(d), Some(s)) => {
                        // MOV r1,r2
                        // r1 <- r2
                        let v = self.registers.read(s);
                        self.registers.put(d, v);
                    },
                    (Some(d), None) => {
                        // MOV r,M
                        // r <- ((H) (L))
                        let v = self.memory.read(addr);
                        self.registers.put(d, v);
                    },
                    (None, Some(s)) => {
                        // MOV M,r
                        // ((H) (L)) <- r
                        let v = self.registers.read(s);
                        self.memory.put(addr, v);
                    },
                    _ => {
                        unimplemented!();
                    }
                }
            }
            // MVI - Move immediate
            else if (instruction & 0b11000111) == 0b00000110 {
                let operand = self.fetch();
                let dest = (instruction & 0b00111000) >> 3;

                match Processor::decode_reg(dest) {
                    Some(d) => {
                        self.registers.put(d, operand);
                    },
                    None => {
                        let addr = self.registers.readx(RegisterPairs::H);
                        self.memory.put(addr, operand);
                    }
                }
            }
            // INR - Increment
            else if (instruction & 0b11000111) == 0b00000100 {
                let tgt = (instruction & 0b00111000) >> 3;

                match Processor::decode_reg(tgt) {
                    Some(t) => {
                        let v = self.registers.read(t);
                        let nv = (v as u16) + 1;

                        self.registers.put(t, nv as u8);
                        self.set_flags(v, nv);
                    },
                    None => {
                        let addr = self.registers.readx(RegisterPairs::H);
                        let v = self.memory.read(addr);
                        let nv = (v as u16) + 1;

                        self.memory.put(addr, nv as u8);
                        self.set_flags(v, nv);
                    }
                }
            }
            // DCR - Decrement
            else if (instruction & 0b11000111) == 0b00000101 {
                let tgt = (instruction & 0b00111000) >> 3;

                match Processor::decode_reg(tgt) {
                    Some(t) => {
                        let v = self.registers.read(t);
                        let nv = (v as i8) - 1;

                        self.registers.put(t, nv as u8);
                        self.set_flags(v, nv as u16);
                    },
                    None => {
                        let addr = self.registers.readx(RegisterPairs::H);
                        let v = self.memory.read(addr);
                        let nv = (v as i8) - 1;

                        self.memory.put(addr, nv as u8);
                        self.set_flags(v, nv as u16);
                    }
                }
            }
            // ADD - Add to A
            else if (instruction & 0b11111000) == 0b10000000 {
                let tgt = instruction & 0b00000111;

                let v = self.read_reg_or_mem(tgt) as u16;

                let a = self.registers.read(GeneralRegisters::A);
                let nv = (a as u16) + v;

                self.registers.put(GeneralRegisters::A, nv as u8);
                self.set_flags(a, nv);
            }
            // ADC - Add to A with carry
            else if (instruction & 0b11111000) == 0b10001000 {
                let tgt = instruction & 0b00000111;
                let c_flag = self.registers.read_flag(FlagRegister::C);
                let carry = if c_flag { 1 } else { 0 };

                let v = self.read_reg_or_mem(tgt) as u16;

                let a = self.registers.read(GeneralRegisters::A);
                let nv = (a as u16) + v + carry;

                self.registers.put(GeneralRegisters::A, nv as u8);
                self.set_flags(a, nv);
            }
            // SUB - Subtract from A
            else if (instruction & 0b11111000) == 0b10011000 {
                let tgt = instruction & 0b00000111;
                let v = self.read_reg_or_mem(tgt);
                let a = self.registers.read(GeneralRegisters::A);

                let nv = if v <= a {
                    a - v
                }
                else {
                    !(v - a) + 1
                };

                self.registers.put(GeneralRegisters::A, nv as u8);
                self.set_flags(a, nv as u16);
            }
            // SBB - Subtract from A with borrow
            else if (instruction & 0b11111000) == 0b10011000 {
                let tgt = instruction & 0b00000111;
                let v = self.read_reg_or_mem(tgt);
                let a = self.registers.read(GeneralRegisters::A);
                let c = a < v;

                // let nv = a - v - (if c { 1 } else { 0 });
                // let nv = if c { !(v - a) + 1 } else { a - v } - (if c { 1 } else { 0 });
                let nv = if c { !(v - a) } else { a - v };

                self.registers.put(GeneralRegisters::A, nv as u8);
                self.set_flags(a as u8, nv as u16);
            }
            // ANA - AND with A
            else if (instruction & 0b11111000) == 0b10100000 {
                let tgt = instruction & 0b00000111;
                let v = self.read_reg_or_mem(tgt);
                let a = self.registers.read(GeneralRegisters::A);

                let nv = a & v;

                self.registers.put(GeneralRegisters::A, nv);
                self.set_flags(a, nv as u16);
            }
            // XRA - XOR with A
            else if (instruction & 0b11111000) == 0b10101000 {
                let tgt = instruction & 0b00000111;
                let v = self.read_reg_or_mem(tgt);
                let a = self.registers.read(GeneralRegisters::A);

                let nv = a ^ v;

                self.registers.put(GeneralRegisters::A, nv);
                self.set_flags(a, nv as u16);
            }
            // ORA - OR with A
            else if (instruction & 0b11111000) == 0b10110000 {
                let tgt = instruction & 0b00000111;
                let v = self.read_reg_or_mem(tgt);
                let a = self.registers.read(GeneralRegisters::A);

                let nv = a | v;

                self.registers.put(GeneralRegisters::A, nv);
                self.set_flags(a, nv as u16);
            }
            // CMP - Compare with A
            else if (instruction & 0b11111000) == 0b10111000 {
                let tgt = instruction & 0b00000111;
                let v = self.read_reg_or_mem(tgt);
                let a = self.registers.read(GeneralRegisters::A);

                let nv = if v <= a {
                    a - v
                }
                else {
                    !(v - a) + 1
                };

                self.set_flags(a, nv as u16);
            }
            // ADI - Add immediate to A
            else if instruction == 0b11000110 {
                let imm = self.fetch() as u16;

                let a = self.registers.read(GeneralRegisters::A);
                let nv = (a as u16) + imm;

                self.registers.put(GeneralRegisters::A, nv as u8);
                self.set_flags(a, nv);
            }
            // ACI - Add immediate to A with carry
            else if instruction == 0b11001110 {
                let imm = self.fetch() as u16;

                let c_flag = self.registers.read_flag(FlagRegister::C);
                let carry = if c_flag { 1 } else { 0 };

                let a = self.registers.read(GeneralRegisters::A);
                let nv = (a as u16) + imm + carry;

                self.registers.put(GeneralRegisters::A, nv as u8);
                self.set_flags(a, nv);
            }
            // SUI - Subtract immediate from A
            else if instruction == 0b11010110 {
                let imm = self.fetch();

                let a = self.registers.read(GeneralRegisters::A);
                let nv = a - imm;

                self.registers.put(GeneralRegisters::A, nv);
                self.set_flags(a, nv as u16);
            }
            // SBI - Subtract immediate from A with borrow
            else if instruction == 0b11011110 {
                let imm = self.fetch();

                let a = self.registers.read(GeneralRegisters::A);
                let c = a < imm;

                let nv = a - imm - (if c { 1 } else { 0 });

                self.registers.put(GeneralRegisters::A, nv);
                self.set_flags(a, nv as u16);
            }
            // ANI - AND immediate with A
            else if instruction == 0b11100110 {
                let imm = self.fetch();

                let a = self.registers.read(GeneralRegisters::A);
                let nv = a & imm;

                self.registers.put(GeneralRegisters::A, nv);
                self.set_flags(a, nv as u16);
            }
            // XRI - XOR immediate with A
            else if instruction == 0b11101110 {
                let imm = self.fetch();

                let a = self.registers.read(GeneralRegisters::A);
                let nv = a ^ imm;

                self.registers.put(GeneralRegisters::A, nv);
                self.set_flags(a, nv as u16);
            }
            // ORI - OR immediate with A
            else if instruction == 0b11110110 {
                let imm = self.fetch();

                let a = self.registers.read(GeneralRegisters::A);
                let nv = a | imm;

                self.registers.put(GeneralRegisters::A, nv);
                self.set_flags(a, nv as u16);
            }
            // CPI - Compare immediate with A
            else if instruction == 0b11111110 {
                let imm = self.fetch();

                let a = self.registers.read(GeneralRegisters::A);
                let nv = if imm <= a {
                    a - imm
                }
                else {
                    !(imm - a) + 1
                };

                self.set_flags(a, nv as u16);
            }
            // RLC - Rotate A left
            else if instruction == 0b00000111 {
                let a = self.registers.read(GeneralRegisters::A);
                let msb = if (a & 0x80) == 0x80 { 1 } else { 0 };

                let nv = (a << 1) | msb;

                self.registers.put(GeneralRegisters::A, nv);
                self.set_flags(a, nv as u16);
            }
            // RRC - Rotate A right
            else if instruction == 0b00001111 {
                let a = self.registers.read(GeneralRegisters::A);
                let lsb = a & 0x01;

                let nv = (lsb << 7) | (a >> 1);

                self.registers.put(GeneralRegisters::A, nv);
                self.set_flags(a, nv as u16);
            }
            // RAL - Rotate A left through carry
            else if instruction == 0b00010111 {
                let a = self.registers.read(GeneralRegisters::A) as u16;
                let msb = if (a & 0x80) == 0x80 { 1u16 } else { 0u16 };
                let carry = if self.registers.read_flag(FlagRegister::C) { 1 } else { 0 };

                let nv = (msb << 8) | (a << 1) | carry;

                self.registers.put(GeneralRegisters::A, nv as u8);
                self.set_flags(a as u8, nv);
            }
            // RAR - Rotate A right through carry
            else if instruction == 0b00011111 {
                let a = self.registers.read(GeneralRegisters::A) as u16;
                let lsb = (a & 0x01) as u16;
                let carry = if self.registers.read_flag(FlagRegister::C) { 1 } else { 0 };

                let nv = (lsb << 8) | (carry << 7) | (a >> 1);

                self.registers.put(GeneralRegisters::A, nv as u8);
                self.set_flags(a as u8, nv);
            }
            // JMP - Jump
            else if instruction == 0b11000011 {
                let addr_h = self.fetch() as u16;
                let addr_l = self.fetch() as u16;

                self.registers.put_sp(SpecialRegisters::PC, (addr_h << 8) | addr_l);
            }
            // J[cond] - Jump conditional
            else if (instruction & 0b11000111) == 0b11000010 {
                let addr_h = self.fetch() as u16;
                let addr_l = self.fetch() as u16;

                let condition = match (instruction & 0b00111000) >> 3 {
                    // JC
                    0b011 => self.registers.read_flag(FlagRegister::C),
                    // JNC
                    0b010 => !self.registers.read_flag(FlagRegister::C),
                    // JZ
                    0b001 => self.registers.read_flag(FlagRegister::Z),
                    // JNZ
                    0b000 => !self.registers.read_flag(FlagRegister::Z),
                    // JP
                    0b110 => !self.registers.read_flag(FlagRegister::S),
                    // JM
                    0b111 => self.registers.read_flag(FlagRegister::S),
                    // JPE
                    0b101 => self.registers.read_flag(FlagRegister::P),
                    // JPO
                    0b100 => !self.registers.read_flag(FlagRegister::P),
                    // No way to happen!
                    _ => false
                };

                if condition {
                    self.registers.put_sp(SpecialRegisters::PC, (addr_h << 8) | addr_l);
                }
            }
            // CALL - Call
            else if instruction == 0b11001101 {
                let dest_h = self.fetch() as u16;
                let dest_l = self.fetch() as u16;
                let dest = (dest_h << 8) | dest_l;

                let pc = self.registers.read_sp(SpecialRegisters::PC);
                self.push_stack(pc);

                self.registers.put_sp(SpecialRegisters::PC, dest);
            }
            // C[cond] - Call conditional
            else if (instruction & 0b11000111) == 0b11000100 {
                let addr_h = self.fetch() as u16;
                let addr_l = self.fetch() as u16;
                let addr = (addr_h << 8) | addr_l;

                let condition = match (instruction & 0b00111000) >> 3 {
                    // CC
                    0b011 => self.registers.read_flag(FlagRegister::C),
                    // CNC
                    0b010 => !self.registers.read_flag(FlagRegister::C),
                    // CZ
                    0b001 => self.registers.read_flag(FlagRegister::Z),
                    // CNZ
                    0b000 => !self.registers.read_flag(FlagRegister::Z),
                    // CP
                    0b110 => !self.registers.read_flag(FlagRegister::S),
                    // CM
                    0b111 => self.registers.read_flag(FlagRegister::S),
                    // CPE
                    0b101 => self.registers.read_flag(FlagRegister::P),
                    // CPO
                    0b100 => !self.registers.read_flag(FlagRegister::P),
                    // No way to happen!
                    _ => false
                };

                if condition {
                    let pc = self.registers.read_sp(SpecialRegisters::PC);
                    self.push_stack(pc);
                    self.registers.put_sp(SpecialRegisters::PC, addr);
                }
            }
            // RET - Return
            else if instruction == 0b11001001 {
                let dest = self.pop_stack();
                self.registers.put_sp(SpecialRegisters::PC, dest);
            }
            // R[cond] - Return conditional
            else if (instruction & 0b11000111) == 0b11000000 {
                let condition = match (instruction & 0b00111000) >> 3 {
                    // CC
                    0b011 => self.registers.read_flag(FlagRegister::C),
                    // CNC
                    0b010 => !self.registers.read_flag(FlagRegister::C),
                    // CZ
                    0b001 => self.registers.read_flag(FlagRegister::Z),
                    // CNZ
                    0b000 => !self.registers.read_flag(FlagRegister::Z),
                    // CP
                    0b110 => !self.registers.read_flag(FlagRegister::S),
                    // CM
                    0b111 => self.registers.read_flag(FlagRegister::S),
                    // CPE
                    0b101 => self.registers.read_flag(FlagRegister::P),
                    // CPO
                    0b100 => !self.registers.read_flag(FlagRegister::P),
                    // No way to happen!
                    _ => false
                };

                if condition {
                    let pc = self.pop_stack();
                    self.registers.put_sp(SpecialRegisters::PC, pc);
                }
            }
            // RST - Restart
            else if (instruction & 0b11000111) == 0b11000111 {
                unimplemented!();
            }
            // IN - Input
            else if instruction == 0b11011011 {
                unimplemented!();
            }
            // OUT - Output
            else if instruction == 0b11010011 {
                unimplemented!();
            }
            // LXI - Load immediate into register pair
            else if (instruction & 0b11001111) == 0b00000001 {
                let imm16_h = self.fetch() as u16;
                let imm16_l = self.fetch() as u16;
                let imm16 = (imm16_h << 8) | imm16_l;
                let dest = (instruction & 0b00110000) >> 4;

                match Processor::decode_reg_pair(dest) {
                    Some(d) => {
                        self.registers.putx(d, imm16);
                    },
                    None => {
                        // code = 3
                        // ここではSPを指す
                        self.registers.put_sp(SpecialRegisters::SP, imm16);
                    }
                }
            }
            // PUSH - Push register pair on stack
            else if (instruction & 0b11001111) == 0b11000101 {
                let dest = (instruction & 0b00110000) >> 4;

                let v = match Processor::decode_reg_pair(dest) {
                    Some(d) => {
                        self.registers.readx(d)
                    },
                    None => {
                        // code = 3
                        // ここではPSW(AF)を指す
                        let a = self.registers.read(GeneralRegisters::A) as u16;
                        let f =
                            if self.registers.read_flag(FlagRegister::C) { 0x01 } else { 0 }
                            | if self.registers.read_flag(FlagRegister::P) { 0x04 } else { 0 }
                            | if self.registers.read_flag(FlagRegister::H) { 0x10 } else { 0 }
                            | if self.registers.read_flag(FlagRegister::Z) { 0x40 } else { 0 }
                            | if self.registers.read_flag(FlagRegister::S) { 0x80 } else { 0 };
                        (a << 8) | f
                    }
                };

                self.push_stack(v);
            }
            // POP - Pop register pair off stack
            else if (instruction & 0b11001111) == 0b11000001 {
                let v = self.pop_stack();

                let dest = (instruction & 0b00110000) >> 4;
                match Processor::decode_reg_pair(dest) {
                    Some(d) => {
                        self.registers.putx(d, v);
                    },
                    None => {
                        // code = 3
                        // ここではPSW(AF)を指す
                        let h = (v >> 8) as u8;
                        let l = v as u8;

                        self.registers.put(GeneralRegisters::A, h);
                        self.registers.put_flag(FlagRegister::C, (l & 0x01) == 0x01);
                        self.registers.put_flag(FlagRegister::P, (l & 0x04) == 0x04);
                        self.registers.put_flag(FlagRegister::H, (l & 0x10) == 0x10);
                        self.registers.put_flag(FlagRegister::Z, (l & 0x40) == 0x40);
                        self.registers.put_flag(FlagRegister::S, (l & 0x80) == 0x80);
                    }
                }
            }
            // STA - Store A direct
            else if instruction == 0b00110010 {
                let addr_h = self.fetch() as u16;
                let addr_l = self.fetch() as u16;
                let addr = (addr_h << 8) | addr_l;

                let a = self.registers.read(GeneralRegisters::A);
                self.memory.put(addr, a);
            }
            // LDA - Load A direct
            else if instruction == 0b00111010 {
                let addr_h = self.fetch() as u16;
                let addr_l = self.fetch() as u16;
                let addr = (addr_h << 8) | addr_l;

                let v = self.memory.read(addr);
                self.registers.put(GeneralRegisters::A, v);
            }
            // XCHG - Exchange DE & HL
            else if instruction == 0b11101011 {
                let de = self.registers.readx(RegisterPairs::D);
                let hl = self.registers.readx(RegisterPairs::H);
                self.registers.putx(RegisterPairs::D, hl);
                self.registers.putx(RegisterPairs::H, de);
            }
            // XTHL - Exchange HL & top of stack
            else if instruction == 0b11100011 {
                let hl = self.registers.readx(RegisterPairs::H);
                let st = self.pop_stack();

                self.registers.putx(RegisterPairs::H, st);
                self.push_stack(hl);
            }
            // SPHL - HL to SP
            else if instruction == 0b11111001 {
                let hl = self.registers.readx(RegisterPairs::H);
                self.registers.put_sp(SpecialRegisters::SP, hl);
            }
            // PCHL - HL to PC
            else if instruction == 0b11101001 {
                let hl = self.registers.readx(RegisterPairs::H);
                self.registers.put_sp(SpecialRegisters::PC, hl);
            }
            // DAD - Add rp to HL
            else if (instruction & 0b11001111) == 0b00001001 {
                let tgt = (instruction & 0b00110000) >> 4;

                let v = match Processor::decode_reg_pair(tgt) {
                    Some(s) => {
                        self.registers.readx(s)
                    },
                    None => {
                        // tgt = 3
                        // ここではSPを指す
                        self.registers.read_sp(SpecialRegisters::SP)
                    }
                };

                let hl = self.registers.readx(RegisterPairs::H);
                let nv = (hl as u32) + (v as u32);
                self.registers.putx(RegisterPairs::H, nv as u16);
                self.set_flags_x(hl, nv);
            }
            // STAX - Store A indirect
            else if (instruction & 0b11001111) == 0b00000010 {
                let a = self.registers.read(GeneralRegisters::A);
                let tgt = (instruction & 0b00110000) >> 4;

                match Processor::decode_reg_pair(tgt) {
                    Some(d) => {
                        // 本来 d は B, D のいずれかだが HL でも動作は変わらんので無視
                        let dest_addr = self.registers.readx(d);
                        self.memory.put(dest_addr, a);
                    },
                    None => {
                        // tgt = 3
                        // ここでは指定できない
                        unimplemented!();
                    }
                }
            }
            // LDAX - Load A indirect
            else if (instruction & 0b11001111) == 0b00001010 {
                let tgt = (instruction & 0b00110000) >> 4;

                let v = match Processor::decode_reg_pair(tgt) {
                    Some(s) => {
                        // 本来 s は B, D のいずれかだが HL でも変わらんので無視
                        let src_addr = self.registers.readx(s);
                        self.memory.read(src_addr)
                    },
                    None => {
                        // tgt = 3
                        // ここでは指定できない
                        unimplemented!();
                    }
                };

                self.registers.put(GeneralRegisters::A, v);
            }
            // INX - Increment rp
            else if (instruction & 0b11001111) == 0b00000011 {
                let tgt = (instruction & 0b00110000) >> 4;

                match Processor::decode_reg_pair(tgt) {
                    Some(s) => {
                        let v = self.registers.readx(s);
                        let nv = (v as u32) + 1;

                        self.registers.putx(s, nv as u16);
                        self.set_flags_x(v, nv);
                    },
                    None => {
                        // tgt = 3
                        // ここではSPを指す
                        let v = self.registers.read_sp(SpecialRegisters::SP);
                        let nv = (v as u32) + 1;

                        self.registers.put_sp(SpecialRegisters::SP, nv as u16);
                        self.set_flags_x(v, nv);
                    }
                }
            }
            // DCX - Decrement rp
            else if (instruction & 0b11001111) == 0b00001011 {
                let tgt = (instruction & 0b00110000) >> 4;

                match Processor::decode_reg_pair(tgt) {
                    Some(s) => {
                        let v = self.registers.readx(s);
                        let nv = v - 1;

                        self.registers.putx(s, nv as u16);
                        self.set_flags_x(v, nv as u32);
                    },
                    None => {
                        // tgt = 3
                        // ここではSPを指す
                        let v = self.registers.read_sp(SpecialRegisters::SP);
                        let nv = v - 1;

                        self.registers.put_sp(SpecialRegisters::SP, nv as u16);
                        self.set_flags_x(v, nv as u32);
                    }
                }
            }
            // CMA - Complement A
            else if instruction == 0b00101111 {
                let a = self.registers.read(GeneralRegisters::A);
                let nv = !a;

                self.registers.put(GeneralRegisters::A, nv);
                self.set_flags(a, nv as u16);
            }
            // STC - Set carry
            else if instruction == 0b00110111 {
                self.registers.put_flag(FlagRegister::C, true);
            }
            // CMC - Complement carry
            else if instruction == 0b00111111 {
                let c = self.registers.read_flag(FlagRegister::C);
                self.registers.put_flag(FlagRegister::C, !c);
            }
            // DAA - Decimal adjust A
            else if instruction == 0b00100111 {
                unimplemented!();
            }
            // SHLD - Store HL direct
            else if instruction == 0b00100010 {
                let addr_h = self.fetch() as u16;
                let addr_l = self.fetch() as u16;
                let addr = (addr_h << 8) | addr_l;

                let hl = self.registers.readx(RegisterPairs::H);

                self.memory.put(addr, (hl >> 8) as u8);
                self.memory.put(addr + 1, hl as u8);
            }
            // LHLD - Load HL direct
            else if instruction == 0b00101010 {
                let addr_h = self.fetch() as u16;
                let addr_l = self.fetch() as u16;
                let addr = (addr_h << 8) | addr_l;

                let vh = self.memory.read(addr) as u16;
                let vl = self.memory.read(addr + 1) as u16;
                let v = (vh << 8) | vl;

                self.registers.putx(RegisterPairs::H, v);
            }
            // EI - Enable interrupts
            else if instruction == 0b11111011 {
                unimplemented!();
            }
            // DI - Disable interrupts
            else if instruction == 0b11110011 {
                unimplemented!();
            }
            // NOP - No operation
            else if instruction == 0b00000000 {
                // No operation
            }
        }
    }

    fn fetch(&mut self) -> u8 {
        let pc = self.registers.read_sp(SpecialRegisters::PC);
        let v = self.memory.read(pc);
        self.registers.put_sp(SpecialRegisters::PC, pc + 1);
        return v;
    }

    fn set_flags(&mut self, old_value: u8, op_result: u16) {
        self.registers.put_flag(FlagRegister::C, 0xFF < op_result);
        self.registers.put_flag(FlagRegister::Z, (op_result & 0xFF) == 0);
        self.registers.put_flag(FlagRegister::S, (op_result & 0x80) == 0x80);
        self.registers.put_flag(FlagRegister::P, (op_result as u8).get_parity());
        self.registers.put_flag(FlagRegister::H, (old_value <= 0x0F) && (0x0F < op_result));
    }

    fn set_flags_x(&mut self, old_value: u16, op_result: u32) {
        self.registers.put_flag(FlagRegister::C, 0xFFFF < op_result);
        self.registers.put_flag(FlagRegister::Z, (op_result & 0xFFFF) == 0);
        self.registers.put_flag(FlagRegister::S, (op_result & 0x8000) == 0x8000);
        self.registers.put_flag(FlagRegister::P, (op_result as u16).get_parity());
        self.registers.put_flag(FlagRegister::H, (old_value <= 0x0F) && (0x0F < op_result));
    }

    fn read_reg_or_mem(&self, code: u8) -> u8 {
        match Processor::decode_reg(code) {
            Some(t) => {
                self.registers.read(t)
            },
            None => {
                let addr = self.registers.readx(RegisterPairs::H);
                self.memory.read(addr)
            }
        }
    }

    fn push_stack(&mut self, val: u16) {
        let sp = self.registers.read_sp(SpecialRegisters::SP);
        let nsp = sp + 2;

        self.memory.put(nsp, ((val & 0xFF00) >> 8) as u8);
        self.memory.put(nsp + 1, (val & 0x00FF) as u8);

        self.registers.put_sp(SpecialRegisters::SP, sp + 2);
    }

    fn pop_stack(&mut self) -> u16 {
        let sp = self.registers.read_sp(SpecialRegisters::SP);

        let h = self.memory.read(sp) as u16;
        let l = self.memory.read(sp + 1) as u16;

        self.registers.put_sp(SpecialRegisters::SP, sp - 2);

        return (h << 8) | l;
    }

    fn decode_reg(code: u8) -> Option<GeneralRegisters> {
        match code {
            0 => Some(GeneralRegisters::B),
            1 => Some(GeneralRegisters::C),
            2 => Some(GeneralRegisters::D),
            3 => Some(GeneralRegisters::E),
            4 => Some(GeneralRegisters::H),
            5 => Some(GeneralRegisters::L),
            7 => Some(GeneralRegisters::A),
            _ => None
        }
    }

    fn decode_reg_pair(code: u8) -> Option<RegisterPairs> {
        match code {
            0 => Some(RegisterPairs::B),
            1 => Some(RegisterPairs::D),
            2 => Some(RegisterPairs::H),
            _ => None
        }
    }
}
