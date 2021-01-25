use crate::into_binary::IntoBinary;

#[derive(Eq, PartialEq, Copy, Clone, Debug)]
pub enum GeneralRegisters {
    A = 0,
    B = 2,
    C = 3,
    D = 4,
    E = 5,
    H = 6,
    L = 7
}

impl IntoBinary for GeneralRegisters {
    fn get_code(&self) -> Vec<u8> {
        let code = match self {
            GeneralRegisters::A => 7,
            GeneralRegisters::B => 0,
            GeneralRegisters::C => 1,
            GeneralRegisters::D => 2,
            GeneralRegisters::E => 3,
            GeneralRegisters::H => 4,
            GeneralRegisters::L => 5
        };
        vec![code]
    }
}

#[derive(Eq, PartialEq, Copy, Clone, Debug)]
pub enum RegisterPairs {
    B = 2,
    D = 4,
    H = 6
}

impl IntoBinary for RegisterPairs {
    fn get_code(&self) -> Vec<u8> {
        let code = match self {
            RegisterPairs::B => 0,
            RegisterPairs::D => 1,
            RegisterPairs::H => 2
        };
        vec![code]
    }
}

pub enum SpecialRegisters {
    PC = 0,
    SP = 1
}

pub enum FlagRegister {
    C = 0,
    P = 2,
    H = 4,
    Z = 6,
    S = 7
}

pub struct RegisterSet {
    registers: [u8; 8],
    special_regs: [u16; 2]
}

impl RegisterSet {
    pub fn new() -> RegisterSet {
        return RegisterSet {
            registers: [0, 0, 0, 0, 0, 0, 0, 0],
            special_regs: [0, 0]
        };
    }

    pub fn put(&mut self, reg: GeneralRegisters, val: u8) {
        self.registers[reg as usize] = val;
    }

    pub fn putx(&mut self, reg_pair: RegisterPairs, val: u16) {
        let ptr = reg_pair as usize;
        let h = ((val & 0xFF00) >> 8) as u8;
        let l = (val & 0x00FF) as u8;

        self.registers[ptr] = h;
        self.registers[ptr + 1] = l;
    }

    pub fn put_sp(&mut self, sp_reg: SpecialRegisters, val: u16) {
        let ptr = sp_reg as usize;

        self.special_regs[ptr] = val;
    }

    pub fn put_flag(&mut self, flag: FlagRegister, val: bool) {
        let current = self.registers[1];
        let v = (1 << (flag as u8)) as u8;

        self.registers[1] = if val { current | v } else { current & (!v) };
    }

    pub fn read(&self, reg: GeneralRegisters) -> u8 {
        return self.registers[reg as usize];
    }

    pub fn readx(&self, reg_pair: RegisterPairs) -> u16 {
        let ptr = reg_pair as usize;
        let h = self.registers[ptr] as u16;
        let l = self.registers[ptr + 1] as u16;

        return (h << 8) | l;
    }

    pub fn read_sp(&self, sp_reg: SpecialRegisters) -> u16 {
        let ptr = sp_reg as usize;
        self.special_regs[ptr]
    }

    pub fn read_flag(&self, bit: FlagRegister) -> bool {
        (self.registers[1] >> (bit as u8)) & 0x01 == 1
    }

    pub fn show(&self) -> String {
        let flag =
            if self.read_flag(FlagRegister::C) { 1 } else { 0 }
            | (1 << 1)
            | ((if self.read_flag(FlagRegister::P) { 1 } else { 0 }) << 2)
            // | (0 << 3)
            | ((if self.read_flag(FlagRegister::H) { 1 } else { 0 }) << 4)
            // | (0 << 5)
            | ((if self.read_flag(FlagRegister::Z) { 1 } else { 0 }) << 6)
            | ((if self.read_flag(FlagRegister::S) { 1 } else { 0 }) << 7);
        let mut s = String::from("registers:\n\t");
        s += format!("A: {:02X}\tF: {:02X}\n\t", self.read(GeneralRegisters::A), flag).as_str();
        s += format!("B: {:02X}\tC: {:02X}\n\t", self.read(GeneralRegisters::B), self.read(GeneralRegisters::C)).as_str();
        s += format!("D: {:02X}\tE: {:02X}\n\t", self.read(GeneralRegisters::D), self.read(GeneralRegisters::E)).as_str();
        s += format!("H: {:02X}\tL: {:02X}\n\t", self.read(GeneralRegisters::H), self.read(GeneralRegisters::L)).as_str();
        s += format!("PC:{:02X}\tSP:{:02X}", self.read_sp(SpecialRegisters::PC), self.read_sp(SpecialRegisters::SP)).as_str();

        return s;
    }
}