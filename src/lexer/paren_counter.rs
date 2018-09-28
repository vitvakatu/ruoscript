#[derive(Default)]
pub struct ParenCounter {
    round: u8,
    curly: u8,
    square: u8,
}

impl ParenCounter {
    pub fn round_left(&mut self) -> bool {
        self.round += 1;
        true
    }

    pub fn round_right(&mut self) -> bool {
        if self.round == 0 {
            false
        } else {
            self.round -= 1;
            true
        }
    }

    pub fn curly_left(&mut self) -> bool {
        self.curly += 1;
        true
    }

    pub fn curly_right(&mut self) -> bool {
        if self.curly == 0 {
            false
        } else {
            self.curly -= 1;
            true
        }
    }

    pub fn square_left(&mut self) -> bool {
        self.square += 1;
        true
    }

    pub fn square_right(&mut self) -> bool {
        if self.square == 0 {
            false
        } else {
            self.square -= 1;
            true
        }
    }
}
