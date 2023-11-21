use std::fmt;

pub struct AocAnswer {
    pub(crate) day: i32,
    pub(crate) part1: String,
    pub(crate) part2: String,
}

impl fmt::Display for AocAnswer {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Day: {}", self.day)?;
        writeln!(f, "Part 1: {}", self.part1)?;
        writeln!(f, "Part 2: {}", self.part2)?;
        Ok(())
    }
}
