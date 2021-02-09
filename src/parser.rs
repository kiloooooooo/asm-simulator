use std::collections::HashMap;
use crate::register::{GeneralRegisters, RegisterPairs};
use std::slice::Iter;
use num_traits::{PrimInt, Num};
use regex::Regex;
use crate::into_binary::IntoBinary;

#[derive(Eq, PartialEq, Debug)]
pub struct ProgramList {
    lines: Vec<Operation>,
    symbol_table: HashMap<String, Option<u16>>
}

impl ProgramList {
    pub fn new() -> ProgramList {
        return ProgramList {
            lines: vec![],
            symbol_table: HashMap::new()
        };
    }

    pub fn from(lines: Vec<Operation>, symbol_table: HashMap<String, Option<u16>>) -> ProgramList {
        return ProgramList {
            lines,
            symbol_table
        };
    }

    pub fn iter(&self) -> Iter<Operation> {
        self.lines.iter()
    }

    pub fn get_symbol_table(&self) -> &HashMap<String, Option<u16>> {
        &self.symbol_table
    }
}

#[derive(Eq, PartialEq, Debug)]
pub enum RegisterOrMemory {
    Register(GeneralRegisters),
    Memory
}

#[derive(Eq, PartialEq, Copy, Clone, Debug)]
pub enum Conditions {
    NZ = 0,
    Z  = 1,
    NC = 2,
    C  = 3,
    PO = 4,
    PE = 5,
    P  = 6,
    M  = 7
}

#[derive(Eq, PartialEq, Clone, Debug)]
pub struct Label {
    pub name: String
}

impl Label {
    fn new(name: String) -> Label {
        Label {
            name
        }
    }
}

#[derive(Eq, PartialEq, Clone, Debug)]
pub enum Operand<T: IntoBinary> {
    Label(Label),
    Value(T)
}

impl<T: IntoBinary> Operand<T> {
    pub fn get_code(&self, symbol_table: &HashMap<String, Option<u16>>) -> Result<Vec<u8>, AssembleError> {
        match self {
            Operand::Value(v) => Ok(v.get_code()),
            Operand::Label(l) => {
                if let Some(val) = symbol_table.get(l.name.as_str()) {
                    if let Some(v) = val {
                        Ok(vec![((*v & 0xFF00) >> 8) as u8, (*v & 0x00FF) as u8])
                    }
                    else {
                        Err(AssembleError::LabelUndefined(l.name.clone()))
                    }
                }
                else {
                    Err(AssembleError::LabelUndefined(l.name.clone()))
                }
            }
        }
    }
}

#[derive(Eq, PartialEq, Debug)]
enum Symbols {
    LabelDeclaration(String),
    Label(String),
    Mnemonic(String),
    Immediate(String),
    RegLabel(String)
}

impl Symbols {
    fn unwrap(&self) -> &String {
        match self {
            Symbols::LabelDeclaration(label) => label,
            Symbols::Label(label) => label,
            Symbols::Mnemonic(mnemonic) => mnemonic,
            Symbols::Immediate(imm) => imm,
            Symbols::RegLabel(rl) => rl
        }
    }
}

#[derive(Eq, PartialEq, Debug)]
struct Symbol {
    sym: Symbols,
    at: usize
}

impl Symbol {
    fn new(sym: Symbols, at: usize) -> Symbol {
        Symbol {
            sym,
            at
        }
    }
}

#[derive(Eq, PartialEq, Debug)]
pub enum Operation {
    ORG(Operand<u16>),
    SET(Operand<u8>),
    EQU(Operand<u8>),
    DB(Vec<Operand<u8>>),
    DW(Vec<Operand<u16>>),
    DS(Operand<u8>),
    START,
    END,
    MOV(Operand<RegisterOrMemory>, Operand<RegisterOrMemory>),
    MVI(Operand<RegisterOrMemory>, Operand<u8>),
    LXI(Operand<RegisterPairs>, Operand<u16>),
    LDA(Operand<u16>),
    STA(Operand<u16>),
    LHLD(Operand<u16>),
    SHLD(Operand<u16>),
    LDAX(Operand<RegisterPairs>),
    STAX(Operand<RegisterPairs>),
    XCHG,
    INR(Operand<GeneralRegisters>),
    DCR(Operand<GeneralRegisters>),
    INX(Operand<RegisterPairs>),
    DCX(Operand<RegisterPairs>),
    ADD(Operand<RegisterOrMemory>),
    ADI(Operand<u8>),
    ADC(Operand<RegisterOrMemory>),
    ACI(Operand<u8>),
    SUB(Operand<RegisterOrMemory>),
    SUI(Operand<u8>),
    SBB(Operand<RegisterOrMemory>),
    SBI(Operand<u8>),
    DAD(Operand<RegisterPairs>),
    CMP(Operand<RegisterOrMemory>),
    CPI(Operand<u8>),
    JMP(Operand<u16>),
    J(Operand<Conditions>, Operand<u16>),
    ANA(Operand<RegisterOrMemory>),
    ANI(Operand<u8>),
    XRA(Operand<GeneralRegisters>),
    XRI(Operand<u8>),
    ORA(Operand<RegisterOrMemory>),
    ORI(Operand<u8>),
    RLC,
    RRC,
    RAL,
    RAR,
    CMA,
    CMC,
    STC,
    DAA,
    PUSH(Operand<RegisterPairs>),
    POP(Operand<RegisterPairs>),
    XTHL,
    SPHL,
    PCHL,
    CALL(Operand<u16>),
    C(Operand<Conditions>, Operand<u16>),
    RET,
    R(Operand<Conditions>),
    RST(Operand<u8>),
    EI,
    DI,
    IN(Operand<u8>),
    OUT(Operand<u8>),
    HLT,
    NOP
}

impl Operation {
    pub fn length(&self) -> u16 {
        match self {
            Operation::MVI(_, _)
            | Operation::ADI(_)
            | Operation::SUI(_) => 2,

            Operation::LXI(_, _)
            | Operation::LDA(_)
            | Operation::STA(_)
            | Operation::LHLD(_)
            | Operation::SHLD(_)
            | Operation::JMP(_)
            | Operation::J(_, _)
            | Operation::CALL(_)
            | Operation::C(_, _) => 3,

            Operation::ORG(_)
            | Operation::EQU(_)
            | Operation::SET(_) => 0,

            Operation::DB(vs) => {
                vs.len() as u16
            },

            Operation::DW(vs) => {
                (vs.len() * 2) as u16
            },

            Operation::DS(n) => match n {
                Operand::Value(m) => *m as u16,
                Operand::Label(_) => {
                    unimplemented!();
                }
            },

            _ => 1
        }
    }

    pub fn get_op_code(&self, symbol_table: &HashMap<String, Option<u16>>) -> (Option<Operand<u16>>, Result<Vec<u8>, AssembleError>) {
        let mut jump_to: Option<Operand<u16>> = None;
        let result = match self {
            Operation::MOV(dst, src) => {
                let dst_code = dst.get_code(symbol_table);
                let src_code = src.get_code(symbol_table);

                match (&src_code, &dst_code) {
                    (Ok(_src), Ok(_dst)) => {
                        let code = 0b01000000 | ((_dst[0]) << 3) | _src[0];
                        Ok(vec![code])
                    },
                    (Err(_), _) => src_code,
                    (_, Err(_)) => dst_code
                }
            },
            Operation::HLT => {
                Ok(vec![0b01110110])
            },
            Operation::MVI(dst, imm) => {
                let dst_code = dst.get_code(symbol_table);
                let imm_code =
                    imm.get_code(symbol_table)
                       .map(|v| {
                           let last = v.last();
                           match last {
                               Some(_last) => vec![*_last],
                               None => vec![]
                           }
                       });

                match (&dst_code, &imm_code) {
                    (Ok(_dst), Ok(_imm)) => {
                        let code = 0b00000000 | (_dst[0] << 3) | 0b110;
                        Ok([vec![code].as_slice(), _imm.as_slice()].concat())
                    },
                    (Err(_), _) => dst_code,
                    (_, Err(_)) => imm_code
                }
            },
            Operation::INR(tgt) => {
                let tgt_code = tgt.get_code(symbol_table);

                match &tgt_code {
                    Ok(_tgt) => {
                        let code = 0b00000000 | (_tgt[0] << 3) | 0b100;
                        Ok(vec![code])
                    },
                    Err(_) => tgt_code
                }
            },
            Operation::DCR(tgt) => {
                let tgt_code = tgt.get_code(symbol_table);

                match &tgt_code {
                    Ok(_tgt) => {
                        let code = 0b00000000 | (_tgt[0] << 3) | 0b101;
                        Ok(vec![code])
                    },
                    Err(_) => tgt_code
                }
            },
            Operation::ADD(src) => {
                let src_code = src.get_code(symbol_table);

                match &src_code {
                    Ok(_src) => {
                        let code = 0b10000000 | _src[0];
                        Ok(vec![code])
                    },
                    Err(_) => src_code
                }
            },
            Operation::ADC(src) => {
                let src_code = src.get_code(symbol_table);

                match &src_code {
                    Ok(_src) => {
                        let code = 0b10001000 | _src[0];
                        Ok(vec![code])
                    },
                    Err(_) => src_code
                }
            },
            Operation::SUB(src) => {
                let src_code = src.get_code(symbol_table);

                match &src_code {
                    Ok(_src) => {
                        let code = 0b10010000 | _src[0];
                        Ok(vec![code])
                    },
                    Err(_) => src_code
                }
            },
            Operation::SBB(src) => {
                let src_code = src.get_code(symbol_table);

                match &src_code {
                    Ok(_src) => {
                        let code = 0b10011000 | _src[0];
                        Ok(vec![code])
                    },
                    Err(_) => src_code
                }
            },
            Operation::ANA(src) => {
                let src_code = src.get_code(symbol_table);

                match &src_code {
                    Ok(_src) => {
                        let code = 0b10100000 | _src[0];
                        Ok(vec![code])
                    },
                    Err(_) => src_code
                }
            },
            Operation::XRA(src) => {
                let src_code = src.get_code(symbol_table);

                match &src_code {
                    Ok(_src) => {
                        let code = 0b10101000 | _src[0];
                        Ok(vec![code])
                    },
                    Err(_) => src_code
                }
            },
            Operation::ORA(src) => {
                let src_code = src.get_code(symbol_table);

                match &src_code {
                    Ok(_src) => {
                        let code = 0b10110000 | _src[0];
                        Ok(vec![code])
                    },
                    Err(_) => src_code
                }
            },
            Operation::CMP(src) => {
                let src_code = src.get_code(symbol_table);

                match &src_code {
                    Ok(_src) => {
                        let code = 0b10111000 | _src[0];
                        Ok(vec![code])
                    },
                    Err(_) => src_code
                }
            },
            Operation::ADI(imm) => {
                let imm_code =
                    imm.get_code(symbol_table)
                        .map(|v| {
                            let last = v.last();
                            match last {
                                Some(_last) => vec![*_last],
                                None => vec![]
                            }
                        });

                match &imm_code {
                    Ok(_imm) => {
                        let code = 0b11000110;
                        Ok([vec![code].as_slice(), _imm.as_slice()].concat())
                    },
                    Err(_) => imm_code
                }
            },
            Operation::ACI(imm) => {
                let imm_code =
                    imm.get_code(symbol_table)
                        .map(|v| {
                            let last = v.last();
                            match last {
                                Some(_last) => vec![*_last],
                                None => vec![]
                            }
                        });

                match &imm_code {
                    Ok(_imm) => {
                        let code = 0b11001110;
                        Ok([vec![code].as_slice(), _imm.as_slice()].concat())
                    },
                    Err(_) => imm_code
                }
            },
            Operation::SUI(imm) => {
                let imm_code =
                    imm.get_code(symbol_table)
                        .map(|v| {
                            let last = v.last();
                            match last {
                                Some(_last) => vec![*_last],
                                None => vec![]
                            }
                        });

                match &imm_code {
                    Ok(_imm) => {
                        let code = 0b11010110;
                        Ok([vec![code].as_slice(), _imm.as_slice()].concat())
                    },
                    Err(_) => imm_code
                }
            },
            Operation::SBI(imm) => {
                let imm_code =
                    imm.get_code(symbol_table)
                        .map(|v| {
                            let last = v.last();
                            match last {
                                Some(_last) => vec![*_last],
                                None => vec![]
                            }
                        });

                match &imm_code {
                    Ok(_imm) => {
                        let code = 0b11011110;
                        Ok([vec![code].as_slice(), _imm.as_slice()].concat())
                    },
                    Err(_) => imm_code
                }
            },
            Operation::ANI(imm) => {
                let imm_code =
                    imm.get_code(symbol_table)
                        .map(|v| {
                            let last = v.last();
                            match last {
                                Some(_last) => vec![*_last],
                                None => vec![]
                            }
                        });

                match &imm_code {
                    Ok(_imm) => {
                        let code = 0b11100110;
                        Ok([vec![code].as_slice(), _imm.as_slice()].concat())
                    },
                    Err(_) => imm_code
                }
            },
            Operation::XRI(imm) => {
                let imm_code =
                    imm.get_code(symbol_table)
                        .map(|v| {
                            let last = v.last();
                            match last {
                                Some(_last) => vec![*_last],
                                None => vec![]
                            }
                        });

                match &imm_code {
                    Ok(_imm) => {
                        let code = 0b11101110;
                        Ok([vec![code].as_slice(), _imm.as_slice()].concat())
                    },
                    Err(_) => imm_code
                }
            },
            Operation::ORI(imm) => {
                let imm_code =
                    imm.get_code(symbol_table)
                        .map(|v| {
                            let last = v.last();
                            match last {
                                Some(_last) => vec![*_last],
                                None => vec![]
                            }
                        });

                match &imm_code {
                    Ok(_imm) => {
                        let code = 0b11110110;
                        Ok([vec![code].as_slice(), _imm.as_slice()].concat())
                    },
                    Err(_) => imm_code
                }
            },
            Operation::CPI(imm) => {
                let imm_code =
                    imm.get_code(symbol_table)
                        .map(|v| {
                            let last = v.last();
                            match last {
                                Some(_last) => vec![*_last],
                                None => vec![]
                            }
                        });

                match &imm_code {
                    Ok(_imm) => {
                        let code = 0b11111110;
                        Ok([vec![code].as_slice(), _imm.as_slice()].concat())
                    },
                    Err(_) => imm_code
                }
            },
            Operation::RLC => {
                let code = 0b00000111;
                Ok(vec![code])
            },
            Operation::RRC => {
                let code = 0b00001111;
                Ok(vec![code])
            },
            Operation::RAL => {
                let code = 0b00010111;
                Ok(vec![code])
            },
            Operation::RAR => {
                let code = 0b00011111;
                Ok(vec![code])
            },
            Operation::JMP(addr) => {
                let addr_code = addr.get_code(symbol_table);

                match &addr_code {
                    Ok(_imm) => {
                        let code = 0b11000011;
                        Ok([vec![code].as_slice(), _imm.as_slice()].concat())
                    },
                    Err(_) => addr_code
                }
            },
            Operation::J(cond, addr) => {
                let cond_code = cond.get_code(symbol_table);
                let addr_code = addr.get_code(symbol_table);

                match (&cond_code, &addr_code) {
                    (Ok(_cond), Ok(_addr)) => {
                        let code = 0b11000010 | (_cond[0] << 3);
                        Ok([vec![code].as_slice(), _addr.as_slice()].concat())
                    },
                    (Err(_), _) => cond_code,
                    (_, Err(_)) => addr_code
                }
            },
            Operation::CALL(addr) => {
                let addr_code = addr.get_code(symbol_table);

                match &addr_code {
                    Ok(_addr) => {
                        let code = 0b11001101;
                        Ok([vec![code].as_slice(), _addr.as_slice()].concat())
                    },
                    Err(_) => addr_code
                }
            },
            Operation::C(cond, addr) => {
                let cond_code = cond.get_code(symbol_table);
                let addr_code = addr.get_code(symbol_table);

                match (&cond_code, &addr_code) {
                    (Ok(_cond), Ok(_addr)) => {
                        let code = 0b11000100 | (_cond[0] << 3);
                        Ok([vec![code].as_slice(), _addr.as_slice()].concat())
                    },
                    (Err(_), _) => cond_code,
                    (_, Err(_)) => addr_code
                }
            },
            Operation::RET => {
                let code = 0b11001001;
                Ok(vec![code])
            },
            Operation::R(cond) => {
                let cond_code = cond.get_code(symbol_table);

                match &cond_code {
                    Ok(_cond) => {
                        let code = 0b11000000 | (_cond[0] << 3);
                        Ok(vec![code])
                    },
                    Err(_) => cond_code
                }
            },
            Operation::RST(vec) => {
                let vec_code = vec.get_code(symbol_table);

                match &vec_code {
                    Ok(_vec) => {
                        let code = 0b11000111 | (_vec[0] << 3);
                        Ok(vec![code])
                    },
                    Err(_) => vec_code
                }
            },
            Operation::IN(port) => {
                let port_code = port.get_code(symbol_table);

                match &port_code {
                    Ok(_port) => {
                        let code = 0b11011011;
                        Ok([vec![code].as_slice(), _port.as_slice()].concat())
                    },
                    Err(_) => port_code
                }
            },
            Operation::OUT(port) => {
                let port_code = port.get_code(symbol_table);

                match &port_code {
                    Ok(_port) => {
                        let code = 0b11010011;
                        Ok([vec![code].as_slice(), _port.as_slice()].concat())
                    },
                    Err(_) => port_code
                }
            },
            Operation::LXI(rp, imm) => {
                let rp_code = rp.get_code(symbol_table);
                let imm_code = imm.get_code(symbol_table);

                match (&rp_code, &imm_code) {
                    (Ok(_rp), Ok(_imm)) => {
                        let code = 0b00000001 | (_rp[0] << 4);
                        Ok([vec![code].as_slice(), _imm.as_slice()].concat())
                    },
                    (Err(_), _) => rp_code,
                    (_, Err(_)) => imm_code
                }
            },
            Operation::PUSH(rp) => {
                let rp_code = rp.get_code(symbol_table);

                match &rp_code {
                    Ok(_rp) => {
                        let code = 0b11000101 | (_rp[0] << 4);
                        Ok(vec![code])
                    },
                    Err(_) => rp_code
                }
            },
            Operation::POP(rp) => {
                let rp_code = rp.get_code(symbol_table);

                match &rp_code {
                    Ok(_rp) => {
                        let code = 0b11000001 | (_rp[0] << 4);
                        Ok(vec![code])
                    },
                    Err(_) => rp_code
                }
            },
            Operation::STA(addr) => {
                let addr_code = addr.get_code(symbol_table);

                match &addr_code {
                    Ok(_addr) => {
                        let code = 0b00110010;
                        Ok([vec![code].as_slice(), _addr.as_slice()].concat())
                    },
                    Err(_) => addr_code
                }
            },
            Operation::LDA(addr) => {
                let addr_code = addr.get_code(symbol_table);

                match &addr_code {
                    Ok(_addr) => {
                        let code = 0b00111010;
                        Ok([vec![code].as_slice(), _addr.as_slice()].concat())
                    },
                    Err(_) => addr_code
                }
            },
            Operation::XCHG => {
                let code = 0b11101011;
                Ok(vec![code])
            },
            Operation::XTHL => {
                let code = 0b11100011;
                Ok(vec![code])
            },
            Operation::SPHL => {
                let code = 0b11111001;
                Ok(vec![code])
            },
            Operation::PCHL => {
                let code = 0b11101001;
                Ok(vec![code])
            },
            Operation::DAD(rp) => {
                let rp_code = rp.get_code(symbol_table);

                match &rp_code {
                    Ok(_rp) => {
                        let code = 0b00001001 | (_rp[0] << 4);
                        Ok(vec![code])
                    },
                    Err(_) => rp_code
                }
            },
            Operation::STAX(rx) => {
                let rx_code = rx.get_code(symbol_table);

                match &rx_code {
                    Ok(_rx) => {
                        let code = 0b00000010 | (_rx[0] << 4);
                        Ok(vec![code])
                    },
                    Err(_) => rx_code
                }
            },
            Operation::LDAX(rx) => {
                let rx_code = rx.get_code(symbol_table);

                match &rx_code {
                    Ok(_rx) => {
                        let code = 0b00001010 | (_rx[0] << 4);
                        Ok(vec![code])
                    },
                    Err(_) => rx_code
                }
            },
            Operation::INX(rp) => {
                let rp_code = rp.get_code(symbol_table);

                match &rp_code {
                    Ok(_rp) => {
                        let code = 0b00000011 | (_rp[0] << 4);
                        Ok(vec![code])
                    },
                    Err(_) => rp_code
                }
            },
            Operation::DCX(rp) => {
                let rp_code = rp.get_code(symbol_table);

                match &rp_code {
                    Ok(_rp) => {
                        let code = 0b00001011 | (_rp[0] << 4);
                        Ok(vec![code])
                    },
                    Err(_) => rp_code
                }
            },
            Operation::CMA => {
                let code = 0b00101111;
                Ok(vec![code])
            },
            Operation::STC => {
                let code = 0b00110111;
                Ok(vec![code])
            },
            Operation::CMC => {
                let code = 0b00111111;
                Ok(vec![code])
            },
            Operation::DAA => {
                let code = 0b00100111;
                Ok(vec![code])
            },
            Operation::SHLD(rp) => {
                let rp_code = rp.get_code(symbol_table);

                match &rp_code {
                    Ok(_rp) => {
                        let code = 0b00100010;
                        Ok([vec![code].as_slice(), _rp.as_slice()].concat())
                    },
                    Err(_) => rp_code
                }
            },
            Operation::LHLD(rp) => {
                let rp_code = rp.get_code(symbol_table);

                match &rp_code {
                    Ok(_rp) => {
                        let code = 0b00101010;
                        Ok([vec![code].as_slice(), _rp.as_slice()].concat())
                    },
                    Err(_) => rp_code
                }
            },
            Operation::EI => {
                let code = 0b11111011;
                Ok(vec![code])
            },
            Operation::DI => {
                let code = 0b11110011;
                Ok(vec![code])
            },
            Operation::NOP => {
                let code = 0b00000000;
                Ok(vec![code])
            },
            Operation::ORG(to) => {
                jump_to = Some(to.clone());
                Ok(vec![])
            },
            Operation::SET(_) => {
                Ok(vec![])
            },
            Operation::EQU(_) => {
                Ok(vec![])
            },
            Operation::DB(v) => {
                v.iter().map(|_v| _v.get_code(symbol_table).map(|r| r[0])).collect()
                // v.get_code(symbol_table)
            },
            Operation::DW(v) => {
                v.iter().map(|_v| _v.get_code(symbol_table).map(|r| r[0])).collect()
                // v.get_code(symbol_table)
            },
            Operation::DS(n) => {
                let mut spacer = vec![];
                match n {
                    Operand::Value(_n) => {
                        for _ in 0..(*_n) {
                            spacer.push(0u8)
                        }
                        Ok(spacer)
                    },
                    Operand::Label(l) => {
                        match symbol_table.get(l.name.as_str()) {
                            Some(_n) => {
                                match _n {
                                    Some(__n) => {
                                        for _ in 0..(*__n) {
                                            spacer.push(0u8);
                                        }
                                        Ok(spacer)
                                    },
                                    None => {
                                        Err(AssembleError::LabelUndefined(l.name.clone()))
                                    }
                                }
                            },
                            None => {
                                Err(AssembleError::LabelUndefined(l.name.clone()))
                            }
                        }
                    }
                }
            },
            Operation::START => {
                Ok(vec![])
            },
            Operation::END => {
                Ok(vec![])
            }
        };

        (jump_to, result)
    }
}

#[derive(Eq, PartialEq, Debug)]
enum ParseError {
    UnexpectedSymbol(usize, String),
    UnexpectedEol
}

#[derive(Eq, PartialEq, Debug)]
pub enum AssembleError {
    LabelUndefined(String)
}

pub fn parse(str: &str) -> ProgramList {
    let mut list = ProgramList::new();
    let mut symbol_table: HashMap<String, Option<u16>> = HashMap::new();

    let mut cursor: usize = 0;
    let mut abort_flag = false;

    for (line_idx, line) in str.split('\n').enumerate() {
        // 空行は無視
        if line == "" {
            continue;
        }

        // ; から始まる行は無視
        if line.starts_with(";") {
            continue;
        }

        let symbols = extract_symbols(line);
        for (idx, sym) in symbols.iter().enumerate() {
            match &sym.sym {
                Symbols::LabelDeclaration(label) => {
                    let ops = symbols.split_at(idx + 1).1;
                    let operation = parse_operation(ops);

                    match operation {
                        Ok(op) => {
                            let op_len = op.length() as usize;
                            match op {
                                Operation::SET(operand)
                                | Operation::EQU(operand) => {
                                    match operand {
                                        Operand::Value(v) => {
                                            symbol_table.insert(label.clone(), Some(v as u16));
                                        },
                                        Operand::Label(_) => {
                                            println!("SET/EQU/DB/DWのオペランドにラベルを使用することはできません");
                                            symbol_table.insert(label.clone(), None);
                                        }
                                    }
                                    cursor += op_len;
                                },
                                _ => {
                                    symbol_table.insert(label.clone(), Some(cursor as u16));
                                }
                            }
                        },
                        Err(e) => {
                            match e {
                                ParseError::UnexpectedSymbol(at, sym) => {
                                    println!("予期しないシンボル({}:{}): {}", line_idx + 1, at, sym);
                                },
                                ParseError::UnexpectedEol => {
                                    println!("予期しない改行({}:{})", line_idx + 1, line.len());
                                }
                            }
                        }
                    }

                    // symbol_table.insert(label.clone(), None);
                },
                Symbols::Mnemonic(_) => {
                    let ops = symbols.split_at(idx).1;
                    let operation = parse_operation(ops);

                    match operation {
                        Ok(op) => {
                            cursor += op.length() as usize;
                            list.lines.push(op);
                        },
                        Err(e) => {
                            match e {
                                ParseError::UnexpectedSymbol(at, sym) => {
                                    println!("予期しないシンボル({}:{}): {}", line_idx + 1, at, sym);
                                },
                                ParseError::UnexpectedEol => {
                                    println!("予期しない改行({}:{})", line_idx + 1, line.len());
                                }
                            }
                            abort_flag = true;
                        }
                    }

                    break;
                },
                _ => {}
            }
        }

        if abort_flag {
            println!("中止");
            list.lines.clear();
            list.symbol_table.clear();
            list.lines.push(Operation::HLT);
            break;
        }
    }

    // println!("{:?}", symbol_table);
    // println!("{:?}", list.lines);

    list.symbol_table = symbol_table;

    return list;
}

pub fn assemble(list: &ProgramList) -> Vec<u8> {
    let mut result: Vec<u8> = vec![];

    for op in list.iter() {
        match op.get_op_code(&list.symbol_table) {
            (jump_to, Ok(bytes)) => {
                match jump_to {
                    Some(operand) => {
                        let spacer_len = match operand {
                            Operand::Value(to) => {
                                to
                            },
                            Operand::Label(label) => {
                                match list.symbol_table.get(label.name.as_str()) {
                                    Some(to) => {
                                        match to {
                                            Some(v) => {
                                                *v
                                            },
                                            None => {
                                                unimplemented!();
                                            }
                                        }
                                    },
                                    None => {
                                        unimplemented!();
                                    }
                                }
                            }
                        };

                        let mut spacer: Vec<u8> = vec![];
                        let start = result.len() as u16;

                        for _ in start..spacer_len {
                            spacer.push(0u8);
                        }
                        result.extend(spacer);
                        result.extend(bytes);
                    },
                    None => {
                        result.extend(bytes);
                    }
                }
            },
            (_, Err(asm_err)) => {
                match asm_err {
                    AssembleError::LabelUndefined(label) => {
                        println!("未定義のシンボル: {}", label);
                        println!("中止");
                        result.clear();
                        result.push(0x76); // HLT
                        break;
                    }
                }
            }
        }
    }

    return result;
}

fn extract_symbols(line: &str) -> Vec<Symbol> {
    let imm_regex: Regex = Regex::new(r"^[0-9]").unwrap();
    let reg_regex: Regex = Regex::new(r"^[A-EHLM]$").unwrap();
    let splitter_regex: Regex = Regex::new(r"[\t,\x20]").unwrap();

    let mut result: Vec<Symbol> = vec![];
    let mut indent = 0;
    let mut col = 1;
    let mut labeled = false;

    for (idx, word) in splitter_regex.split(line).enumerate() {
        if word.starts_with(';') {
            break;
        }

        if word == "" {
            col += 1;
            indent += 1;
            continue;
        }

        let sym = if word.ends_with(':') {
            labeled = true;
            let v = word.replace(':', "");
            Symbols::LabelDeclaration(v)
        }
        else {
            let mnemonic_idx = if labeled { indent + 1 } else { indent };
            if idx == mnemonic_idx {
                Symbols::Mnemonic(String::from(word))
            }
            else {
                if imm_regex.is_match(word) {
                    Symbols::Immediate(String::from(word))
                }
                else if reg_regex.is_match(word) {
                    Symbols::RegLabel(String::from(word))
                }
                else {
                    Symbols::Label(String::from(word))
                }
            }
        };

        let s = Symbol::new(sym, col);
        result.push(s);

        col += word.len() + 1;
    }

    return result;
}

fn parse_immediate<T: PrimInt>(s: &str) -> Result<T, <T as Num>::FromStrRadixErr> {
    T::from_str_radix(
        s.replace('H', "").as_str(),
        if s.ends_with('H') { 16 } else { 10 })
}

fn parse_operation(syms: &[Symbol]) -> Result<Operation, ParseError> {
    if let Some(inst) = syms.get(0) {
        let operands = if 1 < syms.len() {
            syms.split_at(1).1
        }
        else {
            &[]
        };

        if let Symbols::Mnemonic(mnemonic) = &inst.sym {
            match mnemonic.as_str() {
                "ORG" => {
                    let operand = parse_imm16(operands);
                    operand.map(|o| Operation::ORG(o))
                },
                "SET" => {
                    let operand = parse_imm(operands);
                    operand.map(|o| Operation::SET(o))
                },
                "EQU" => {
                    let operand = parse_imm(operands);
                    operand.map(|o| Operation::EQU(o))
                },
                "DB" => {
                    let mut _ops: Vec<Result<Operand<u8>, ParseError>> = vec![];
                    for i in 0..operands.len() {
                        let split_ops = operands.split_at(i).1;
                        _ops.push(parse_imm(split_ops));
                    }

                    let mut ops: Vec<Operand<u8>> = vec![];
                    for op in _ops {
                        match op {
                            Ok(o) => {
                                ops.push(o);
                            },
                            Err(e) => {
                                return Err(e)
                            }
                        }
                    }

                    Ok(Operation::DB(ops))

                    // let operand = parse_imm(operands);
                    // operand.map(|o| Operation::DB(o))
                },
                "DW" => {
                    // let operand = parse_imm16(operands);
                    // operand.map(|o| Operation::DW(o))
                    let mut _ops: Vec<Result<Operand<u16>, ParseError>> = vec![];
                    for i in 0..operands.len() {
                        let split_ops = operands.split_at(i).1;
                        _ops.push(parse_imm16(split_ops));
                    }

                    let mut ops: Vec<Operand<u16>> = vec![];
                    for op in _ops {
                        match op {
                            Ok(o) => {
                                ops.push(o);
                            },
                            Err(e) => {
                                return Err(e)
                            }
                        }
                    }

                    Ok(Operation::DW(ops))
                },
                "DS" => {
                    let operand = parse_imm(operands);
                    operand.map(|o| Operation::DS(o))
                },
                "START" => {
                    Ok(Operation::START)
                },
                "END" => {
                    Ok(Operation::END)
                },
                "MOV" => {
                    // let ops = parse_2_operands(operands);
                    let op1 = parse_register_or_mem(operands);
                    let op2 = parse_register_or_mem(operands.split_at(1).1);
                    let ops = pack_2_ops(op1, op2);
                    ops.map(|os| Operation::MOV(os.0, os.1))
                },
                "MVI" => {
                    // let ops = parse_2_operands(operands);
                    let op1 = parse_register_or_mem(operands);
                    let op2 = parse_imm(operands.split_at(1).1);
                    let ops = pack_2_ops(op1, op2);
                    ops.map(|os| Operation::MVI(os.0, os.1))
                },
                "LXI" => {
                    let op1 = parse_register_pair(operands);
                    let op2 = parse_imm16(operands.split_at(1).1);
                    let ops = pack_2_ops(op1, op2);
                    ops.map(|os| Operation::LXI(os.0, os.1))
                },
                "LDA" => {
                    let operand = parse_imm16(operands);
                    operand.map(|o| Operation::LDA(o))
                },
                "STA" => {
                    let operand = parse_imm16(operands);
                    operand.map(|o| Operation::STA(o))
                },
                "LHLD" => {
                    let operand = parse_imm16(operands);
                    operand.map(|o| Operation::LHLD(o))
                },
                "SHLD" => {
                    let operand = parse_imm16(operands);
                    operand.map(|o| Operation::SHLD(o))
                },
                "LDAX" => {
                    let operand = parse_register_pair(operands);
                    operand.map(|o| Operation::LDAX(o))
                },
                "STAX" => {
                    let operand = parse_register_pair(operands);
                    operand.map(|o| Operation::STAX(o))
                },
                "XCHG" => {
                    Ok(Operation::XCHG)
                },
                "INR" => {
                    let operand = parse_register(operands);
                    operand.map(|o| Operation::INR(o))
                },
                "DCR" => {
                    let operand = parse_register(operands);
                    operand.map(|o| Operation::DCR(o))
                },
                "INX" => {
                    let operand = parse_register_pair(operands);
                    operand.map(|o| Operation::INX(o))
                },
                "DCX" => {
                    let operand = parse_register_pair(operands);
                    operand.map(|o| Operation::DCX(o))
                },
                "ADD" => {
                    let operand = parse_register_or_mem(operands);
                    operand.map(|o| Operation::ADD(o))
                },
                "ADI" => {
                    let operand = parse_imm(operands);
                    operand.map(|o| Operation::ADI(o))
                },
                "ADC" => {
                    let operand = parse_register_or_mem(operands);
                    operand.map(|o| Operation::ADC(o))
                },
                "ACI" => {
                    let operand = parse_imm(operands);
                    operand.map(|o| Operation::ACI(o))
                },
                "SUB" => {
                    let operand = parse_register_or_mem(operands);
                    operand.map(|o| Operation::SUB(o))
                },
                "SUI" => {
                    let operand = parse_imm(operands);
                    operand.map(|o| Operation::SUI(o))
                },
                "SBB" => {
                    let operand = parse_register_or_mem(operands);
                    operand.map(|o| Operation::SBB(o))
                },
                "SBI" => {
                    let operand = parse_imm(operands);
                    operand.map(|o| Operation::SBI(o))
                },
                "DAD" => {
                    let operand = parse_register_pair(operands);
                    operand.map(|o| Operation::DAD(o))
                },
                "CMP" => {
                    let operand = parse_register_or_mem(operands);
                    operand.map(|o| Operation::CMP(o))
                },
                "CPI" => {
                    let operand = parse_imm(operands);
                    operand.map(|o| Operation::CPI(o))
                },
                "JMP" => {
                    let operand = parse_imm16(operands);
                    operand.map(|o| Operation::JMP(o))
                },
                "JC" => {
                    let cond = Operand::Value(Conditions::C);
                    let operand = parse_imm16(operands);
                    operand.map(|o| Operation::J(cond, o))
                },
                "JNC" => {
                    let cond = Operand::Value(Conditions::NC);
                    let operand = parse_imm16(operands);
                    operand.map(|o| Operation::J(cond, o))
                },
                "JZ" => {
                    let cond = Operand::Value(Conditions::Z);
                    let operand = parse_imm16(operands);
                    operand.map(|o| Operation::J(cond, o))
                },
                "JNZ" => {
                    let cond = Operand::Value(Conditions::NZ);
                    let operand = parse_imm16(operands);
                    operand.map(|o| Operation::J(cond, o))
                },
                "JP" => {
                    let cond = Operand::Value(Conditions::P);
                    let operand = parse_imm16(operands);
                    operand.map(|o| Operation::J(cond, o))
                },
                "JM" => {
                    let cond = Operand::Value(Conditions::M);
                    let operand = parse_imm16(operands);
                    operand.map(|o| Operation::J(cond, o))
                },
                "JPE" => {
                    let cond = Operand::Value(Conditions::PE);
                    let operand = parse_imm16(operands);
                    operand.map(|o| Operation::J(cond, o))
                },
                "JPO" => {
                    let cond = Operand::Value(Conditions::PO);
                    let operand = parse_imm16(operands);
                    operand.map(|o| Operation::J(cond, o))
                },
                "ANA" => {
                    let operand = parse_register_or_mem(operands);
                    operand.map(|o| Operation::ANA(o))
                },
                "ANI" => {
                    let operand = parse_imm(operands);
                    operand.map(|o| Operation::ANI(o))
                },
                "XRA" => {
                    let operand = parse_register(operands);
                    operand.map(|o| Operation::XRA(o))
                },
                "XRI" => {
                    let operand = parse_imm(operands);
                    operand.map(|o| Operation::XRI(o))
                },
                "ORA" => {
                    let operand = parse_register_or_mem(operands);
                    operand.map(|o| Operation::ORA(o))
                },
                "ORI" => {
                    let operand = parse_imm(operands);
                    operand.map(|o| Operation::ORI(o))
                },
                "RLC" => {
                    Ok(Operation::RLC)
                },
                "RRC" => {
                    Ok(Operation::RRC)
                },
                "RAR" => {
                    Ok(Operation::RAR)
                },
                "CMA" => {
                    Ok(Operation::CMA)
                },
                "CMC" => {
                    Ok(Operation::CMC)
                },
                "STC" => {
                    Ok(Operation::STC)
                },
                "DAA" => {
                    Ok(Operation::DAA)
                },
                "PUSH" => {
                    let operand = parse_register_pair(operands);
                    operand.map(|o| Operation::PUSH(o))
                },
                "POP" => {
                    let operand = parse_register_pair(operands);
                    operand.map(|o| Operation::POP(o))
                },
                "XTHL" => {
                    Ok(Operation::XTHL)
                },
                "SPHL" => {
                    Ok(Operation::SPHL)
                },
                "PCHL" => {
                    Ok(Operation::PCHL)
                },
                "CALL" => {
                    let operand = parse_imm16(operands);
                    operand.map(|o| Operation::CALL(o))
                },
                "CC" => {
                    let cond = Operand::Value(Conditions::C);
                    let operand = parse_imm16(operands);
                    operand.map(|o| Operation::C(cond, o))
                },
                "CNC" => {
                    let cond = Operand::Value(Conditions::NC);
                    let operand = parse_imm16(operands);
                    operand.map(|o| Operation::C(cond, o))
                },
                "CZ" => {
                    let cond = Operand::Value(Conditions::Z);
                    let operand = parse_imm16(operands);
                    operand.map(|o| Operation::C(cond, o))
                },
                "CNZ" => {
                    let cond = Operand::Value(Conditions::NZ);
                    let operand = parse_imm16(operands);
                    operand.map(|o| Operation::C(cond, o))
                },
                "CP" => {
                    let cond = Operand::Value(Conditions::P);
                    let operand = parse_imm16(operands);
                    operand.map(|o| Operation::C(cond, o))
                },
                "CM" => {
                    let cond = Operand::Value(Conditions::M);
                    let operand = parse_imm16(operands);
                    operand.map(|o| Operation::C(cond, o))
                },
                "CPE" => {
                    let cond = Operand::Value(Conditions::PE);
                    let operand = parse_imm16(operands);
                    operand.map(|o| Operation::C(cond, o))
                },
                "CPO" => {
                    let cond = Operand::Value(Conditions::PO);
                    let operand = parse_imm16(operands);
                    operand.map(|o| Operation::C(cond, o))
                },
                "RET" => {
                    Ok(Operation::RET)
                },
                "RC" => {
                    let cond = Operand::Value(Conditions::C);
                    Ok(Operation::R(cond))
                },
                "RNC" => {
                    let cond = Operand::Value(Conditions::NC);
                    Ok(Operation::R(cond))
                },
                "RZ" => {
                    let cond = Operand::Value(Conditions::Z);
                    Ok(Operation::R(cond))
                },
                "RNZ" => {
                    let cond = Operand::Value(Conditions::NZ);
                    Ok(Operation::R(cond))
                },
                "RP" => {
                    let cond = Operand::Value(Conditions::P);
                    Ok(Operation::R(cond))
                },
                "RM" => {
                    let cond = Operand::Value(Conditions::M);
                    Ok(Operation::R(cond))
                },
                "RPE" => {
                    let cond = Operand::Value(Conditions::PE);
                    Ok(Operation::R(cond))
                },
                "RPO" => {
                    let cond = Operand::Value(Conditions::PO);
                    Ok(Operation::R(cond))
                },
                "EI" => {
                    Ok(Operation::EI)
                },
                "DI" => {
                    Ok(Operation::DI)
                },
                "IN" => {
                    let operand = parse_imm(operands);
                    operand.map(|o| Operation::IN(o))
                },
                "OUT" => {
                    let operand = parse_imm(operands);
                    operand.map(|o| Operation::OUT(o))
                },
                "HLT" => {
                    Ok(Operation::HLT)
                },
                "NOP" => {
                    Ok(Operation::NOP)
                }
                _ => {
                    Err(ParseError::UnexpectedSymbol(inst.at, mnemonic.clone()))
                }
            }
        }
        else {
            Err(ParseError::UnexpectedSymbol(inst.at, inst.sym.unwrap().clone()))
        }
    }
    else {
        Err(ParseError::UnexpectedEol)
    }
}

fn parse_imm(syms: &[Symbol]) -> Result<Operand<u8>, ParseError> {
    if let Some(s) = syms.get(0) {
        if let Symbols::Immediate(imm) = &s.sym {
            match parse_immediate::<u8>(imm.as_str()) {
                Ok(v) => Ok(Operand::Value(v)),
                // Err(_) => Ok(Operand::Label(Label::new(imm.clone())))
                Err(_) => Err(ParseError::UnexpectedSymbol(s.at, String::from(s.sym.unwrap())))
            }
        }
        else if let Symbols::Label(label) = &s.sym {
            Ok(Operand::Label(Label::new(label.clone())))
        }
        else {
            Err(ParseError::UnexpectedSymbol(s.at, String::from(s.sym.unwrap())))
        }
    }
    else {
        Err(ParseError::UnexpectedEol)
    }
}

fn parse_imm16(syms: &[Symbol]) -> Result<Operand<u16>, ParseError> {
    if let Some(s) = syms.get(0) {
        if let Symbols::Immediate(imm) = &s.sym {
            match parse_immediate::<u16>(imm.as_str()) {
                Ok(v) => Ok(Operand::Value(v)),
                // Err(_) => Ok(Operand::Label(Label::new(imm.clone())))
                Err(_) => Err(ParseError::UnexpectedSymbol(s.at, String::from(s.sym.unwrap())))
            }
        }
        else if let Symbols::Label(label) = &s.sym {
            Ok(Operand::Label(Label::new(label.clone())))
        }
        else {
            Err(ParseError::UnexpectedSymbol(s.at, String::from(s.sym.unwrap())))
        }
    }
    else {
        Err(ParseError::UnexpectedEol)
    }
}

fn parse_register(syms: &[Symbol]) -> Result<Operand<GeneralRegisters>, ParseError> {
    if let Some(s) = syms.get(0) {
        if let Symbols::RegLabel(rl) = &s.sym {
            match rl.as_str() {
                "A" => Ok(Operand::Value(GeneralRegisters::A)),
                "B" => Ok(Operand::Value(GeneralRegisters::B)),
                "C" => Ok(Operand::Value(GeneralRegisters::C)),
                "D" => Ok(Operand::Value(GeneralRegisters::D)),
                "E" => Ok(Operand::Value(GeneralRegisters::E)),
                "H" => Ok(Operand::Value(GeneralRegisters::H)),
                "L" => Ok(Operand::Value(GeneralRegisters::L)),
                _ => Err(ParseError::UnexpectedSymbol(s.at, rl.clone()))
            }
        }
        else {
            Err(ParseError::UnexpectedSymbol(s.at, String::from(s.sym.unwrap())))
        }
    }
    else {
        Err(ParseError::UnexpectedEol)
    }
}

fn parse_register_or_mem(syms: &[Symbol]) -> Result<Operand<RegisterOrMemory>, ParseError> {
    if let Some(s) = syms.get(0) {
        if let Symbols::RegLabel(rl) = &s.sym {
            match rl.as_str() {
                "A" => Ok(Operand::Value(RegisterOrMemory::Register(GeneralRegisters::A))),
                "B" => Ok(Operand::Value(RegisterOrMemory::Register(GeneralRegisters::B))),
                "C" => Ok(Operand::Value(RegisterOrMemory::Register(GeneralRegisters::C))),
                "D" => Ok(Operand::Value(RegisterOrMemory::Register(GeneralRegisters::D))),
                "E" => Ok(Operand::Value(RegisterOrMemory::Register(GeneralRegisters::E))),
                "H" => Ok(Operand::Value(RegisterOrMemory::Register(GeneralRegisters::H))),
                "L" => Ok(Operand::Value(RegisterOrMemory::Register(GeneralRegisters::L))),
                "M" => Ok(Operand::Value(RegisterOrMemory::Memory)),
                _ => Err(ParseError::UnexpectedSymbol(s.at, rl.clone()))
            }
        }
        else {
            Err(ParseError::UnexpectedSymbol(s.at, String::from(s.sym.unwrap())))
        }
    }
    else {
        Err(ParseError::UnexpectedEol)
    }
}

fn parse_register_pair(syms: &[Symbol]) -> Result<Operand<RegisterPairs>, ParseError> {
    if let Some(s) = syms.get(0) {
        if let Symbols::RegLabel(rl) = &s.sym {
            match rl.as_str() {
                "B" => Ok(Operand::Value(RegisterPairs::B)),
                "D" => Ok(Operand::Value(RegisterPairs::D)),
                "H" => Ok(Operand::Value(RegisterPairs::H)),
                _ => Err(ParseError::UnexpectedSymbol(s.at, rl.clone()))
            }
        }
        else {
            Err(ParseError::UnexpectedSymbol(s.at, String::from(s.sym.unwrap())))
        }
    }
    else {
        Err(ParseError::UnexpectedEol)
    }
}

fn pack_2_ops<T: IntoBinary, U: IntoBinary>(op1: Result<Operand<T>, ParseError>, op2: Result<Operand<U>, ParseError>) -> Result<(Operand<T>, Operand<U>), ParseError> {
    match (op1, op2) {
        (Ok(o1), Ok(o2)) => {
            Ok((o1, o2))
        },
        (Err(e1), _) => {
            Err(e1)
        },
        (_, Err(e2)) => {
            Err(e2)
        }
    }
}
