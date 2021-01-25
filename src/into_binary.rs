use crate::parser::{RegisterOrMemory, Conditions, Label};

pub trait IntoBinary {
    fn get_code(&self) -> Vec<u8>;
}

impl IntoBinary for RegisterOrMemory {
    fn get_code(&self) -> Vec<u8> {
        match self {
            RegisterOrMemory::Register(reg) => reg.get_code(),
            RegisterOrMemory::Memory => vec![6]
        }
    }
}

impl IntoBinary for Conditions {
    fn get_code(&self) -> Vec<u8> {
        vec![*self as u8]
    }
}

impl IntoBinary for Label {
    fn get_code(&self) -> Vec<u8> {
        unimplemented!();
    }
}

impl IntoBinary for u8 {
    fn get_code(&self) -> Vec<u8> {
        vec![*self]
    }
}

impl IntoBinary for u16 {
    fn get_code(&self) -> Vec<u8> {
        vec![((*self & 0xFF00) >> 8) as u8, (*self & 0x00FF) as u8]
    }
}
