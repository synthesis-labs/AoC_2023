use std::fmt::{self};

pub struct AocAnswer {
    pub day: i32,
    pub sample_solution_part1: String,
    pub sample_solution_part2: String,
    pub part1: String,
    pub part2: String,
}

impl fmt::Display for AocAnswer {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Day: {}", self.day)?;
        writeln!(f, "Sample Solution Part 1: {}", &self.sample_solution_part1)?;
        writeln!(f, "Sample Solution Part 2: {}", &self.sample_solution_part2)?;
        writeln!(f, "Part 1: {}", &self.part1)?;
        writeln!(f, "Part 2: {}", &self.part2)?;
        Ok(())
    }
}
