use core::fmt;
use std::collections::HashSet;

pub type CardId = i32;
pub type WinningNumbers = HashSet<i32>;
pub type MyNumbers = HashSet<i32>;
pub type Intersection = Vec<i32>;

#[derive(Debug)]
pub struct Card {
    pub card_id: CardId,
    pub winning_numbers: WinningNumbers,
    pub my_numbers: MyNumbers,
}

impl fmt::Display for Card {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let _ = writeln!(f, "{}", self.card_id);
        for num in &self.winning_numbers {
            println!("{num}");
        }
        for num in &self.my_numbers {
            println!("{num}");
        }
        Ok(())
    }
}
