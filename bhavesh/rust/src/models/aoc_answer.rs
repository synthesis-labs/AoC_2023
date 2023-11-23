use std::fmt::{self, Display};

pub struct AocAnswer<T> {
    pub day: i32,
    pub part1: T,
    pub part2: T,
}

impl<T> fmt::Display for AocAnswer<T>
where
    T: Display,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Day: {}", self.day)?;
        writeln!(f, "Part 1: {}", &self.part1)?;
        writeln!(f, "Part 2: {}", &self.part2)?;
        Ok(())
    }
}
