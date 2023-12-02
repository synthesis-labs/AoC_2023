use std::fmt::{self};

pub type GameId = i32;
pub type Rounds = Vec<Round>;
pub type Round = Vec<RoundSet>;

pub struct Game {
    pub game_id: GameId,
    pub game_rounds: Rounds,
}

pub struct RoundSet {
    pub cube_colour: CubeColour,
    pub num_cubes: i32,
}

#[derive(PartialEq)]
pub enum CubeColour {
    Red,
    Green,
    Blue,
}

impl fmt::Display for CubeColour {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CubeColour::Red => writeln!(f, "red"),
            CubeColour::Green => writeln!(f, "green"),
            CubeColour::Blue => writeln!(f, "blue"),
        }
    }
}

impl fmt::Display for RoundSet {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "{} {}", self.cube_colour, self.num_cubes)
    }
}

impl fmt::Display for Game {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "{}", self.game_id)
    }
}
