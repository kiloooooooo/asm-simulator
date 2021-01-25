pub trait Parity {
    fn get_parity(&self) -> bool;
}

impl Parity for u8 {
    fn get_parity(&self) -> bool {
        let mut flag = true;

        for bit_cursor in 0..7 {
            let bit = ((self >> bit_cursor) & 0x01) == 1;
            flag ^= bit;
        }

        return flag;
    }
}

impl Parity for u16 {
    fn get_parity(&self) -> bool {
        let mut flag = true;

        for bit_cursor in 0..15 {
            let bit = ((self >> bit_cursor) & 0x01) == 1;
            flag ^= bit;
        }

        return flag;
    }
}

#[test]
fn get_parity_works() {
    let v1: u8 = 0b00001101;
    assert_eq!(v1.get_parity(), false);

    let v2: u8 = 0b00001100;
    assert_eq!(v2.get_parity(), true);
}
