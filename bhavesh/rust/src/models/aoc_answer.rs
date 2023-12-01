use std::fmt::{self, Display};

pub struct AocAnswer<T>
where
    T: Display,
{
    pub day: i32,
    pub sample_solution: T,
    pub part1: T,
    pub part2: T,
}

impl<T> fmt::Display for AocAnswer<T>
where
    T: Display,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Day: {}", self.day)?;
        writeln!(f, "Sample Solution: {}", &self.sample_solution)?;
        writeln!(f, "Part 1: {}", &self.part1)?;
        writeln!(f, "Part 2: {}", &self.part2)?;
        Ok(())
    }
}
