pub type Bid = i32;
pub type Hand = Vec<Card>;
pub type Hands = Vec<Hand>;

#[derive(PartialEq, Clone, Copy)]
pub enum Card {
    Ace = 13,
    King = 12,
    Queen = 11,
    Jack = 10,
    Ten = 9,
    Nine = 8,
    Eight = 7,
    Seven = 6,
    Six = 5,
    Five = 4,
    Four = 3,
    Three = 2,
    Two = 1,
}

#[derive(PartialEq)]
pub enum HandType {
    FiveOfAKind = 7,
    FourOfAKind = 6,
    FullHouse = 5,
    ThreeOfAKind = 4,
    TwoPair = 3,
    OnePair = 2,
    HighCard = 1,
}
