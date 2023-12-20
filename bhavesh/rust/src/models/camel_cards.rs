use core::fmt;

use strum_macros::EnumIter;

pub type Bid = i32;
pub type Hand = Vec<Card>;
pub type Hands = Vec<HandRow>;

pub fn to_string(hand: Hand) -> String {
    hand.iter()
        .map(|card| format!("{}", card))
        .collect::<Vec<String>>()
        .join(" ")
}

// -----------------------------------------------------------------------------------------------------------------------------
#[derive(Debug, PartialEq, Clone, Copy, PartialOrd, Eq, Ord, EnumIter)]
pub enum Card {
    Ace = 14,
    King = 13,
    Queen = 12,
    Jack = 11,
    Ten = 10,
    Nine = 9,
    Eight = 8,
    Seven = 7,
    Six = 6,
    Five = 5,
    Four = 4,
    Three = 3,
    Two = 2,
    None = 1,
}

impl fmt::Display for Card {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Card::Ace => write!(f, "A"),
            Card::King => write!(f, "K"),
            Card::Queen => write!(f, "Q"),
            Card::Jack => write!(f, "J"),
            Card::Ten => write!(f, "T"),
            Card::Nine => write!(f, "9"),
            Card::Eight => write!(f, "8"),
            Card::Seven => write!(f, "7"),
            Card::Six => write!(f, "6"),
            Card::Five => write!(f, "5"),
            Card::Four => write!(f, "4"),
            Card::Three => write!(f, "3"),
            Card::Two => write!(f, "2"),
            Card::None => write!(f, "None"),
        }
    }
}

// -----------------------------------------------------------------------------------------------------------------------------
#[derive(Debug, Clone)]
pub struct HandType {
    pub hand: Hand,
    pub kind: HandTypeKind,
}

impl PartialOrd for HandType {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match other.kind.cmp(&self.kind) {
            std::cmp::Ordering::Equal => Some(other.hand.cmp(&self.hand)),
            other => Some(other),
        }
    }
}

impl PartialEq for HandType {
    fn eq(&self, other: &Self) -> bool {
        self.hand == other.hand && self.kind == other.kind
    }
}

impl Eq for HandType {}

impl Ord for HandType {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        other.partial_cmp(&self).unwrap()
    }
}

impl fmt::Display for HandType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let _ = write!(f, "{}: {}", to_string(self.hand.clone()), self.kind);
        Ok(())
    }
}

// -----------------------------------------------------------------------------------------------------------------------------
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
#[repr(u8)]
pub enum HandTypeKind {
    FiveOfAKind = 7,
    FourOfAKind = 6,
    FullHouse = 5,
    ThreeOfAKind = 4,
    TwoPair = 3,
    OnePair = 2,
    HighCard = 1,
}

impl fmt::Display for HandTypeKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            HandTypeKind::FiveOfAKind => write!(f, "Five of a kind"),
            HandTypeKind::FourOfAKind => write!(f, "Four of a kind"),
            HandTypeKind::FullHouse => write!(f, "Full house"),
            HandTypeKind::ThreeOfAKind => write!(f, "Three of a kind"),
            HandTypeKind::TwoPair => write!(f, "Two pair"),
            HandTypeKind::OnePair => write!(f, "One pair"),
            HandTypeKind::HighCard => write!(f, "High card"),
        }
    }
}

// -----------------------------------------------------------------------------------------------------------------------------
#[derive(Debug, Clone)]
pub struct HandRow {
    pub hand_type: HandType ,
    pub bid: Bid,
}

impl PartialOrd for HandRow {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        // self.hand_type.partial_cmp(&other.hand_type)
        other.hand_type.partial_cmp(&self.hand_type)
    }
}

impl PartialEq for HandRow {
    fn eq(&self, other: &Self) -> bool {
        self.hand_type == other.hand_type
    }
}

impl Eq for HandRow {}

impl Ord for HandRow {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        other.hand_type.cmp(&self.hand_type)
    }
}

impl fmt::Display for HandRow {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let _ = write!(f, "{} ", self.hand_type);
        let _ = write!(f, "{}", self.bid);
        Ok(())
    }
}
