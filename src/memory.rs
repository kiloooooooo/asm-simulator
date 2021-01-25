pub type AddrType = u16;
pub type ValType = u8;
pub const MEMORY_SIZE: usize = AddrType::max_value() as usize;

pub struct Memory {
    pub content: [ValType; MEMORY_SIZE]
}

impl Memory {
    pub fn new() -> Memory {
        return Memory {
            content: [0; MEMORY_SIZE]
        };
    }

    pub fn put(&mut self, addr: AddrType, val: ValType) {
        self.content[addr as usize] = val;
    }

    pub fn read(&self, addr: AddrType) -> ValType {
        self.content[addr as usize]
    }

    pub fn show(&self, from: AddrType, to: AddrType) -> String {
        const LINE_LENGTH: usize = 16;

        if to < from {
            panic!("`to` must be greater than `from`");
        }
        else if MEMORY_SIZE <= to as usize {
            panic!("`to` must be less than MEMORY_SIZE");
        }

        let mut s = String::from("Memory:\n");
        for (i, addr) in (from..to).enumerate() {
            if (i % LINE_LENGTH) == 0 {
                s += format!("\t{:04X}:\t", addr).as_str();
            }

            s += format!("{:02X}", self.content[addr as usize]).as_str();

            s += if ((i + 1) % LINE_LENGTH) == 0 {
                "\n"
            }
            else {
                " "
            }
        }

        return s;
    }
}
