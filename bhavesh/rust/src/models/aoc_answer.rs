use std::fmt;

pub struct AocAnswer<T> {
    pub(crate) day: i32,
    pub(crate) part1: T,
    pub(crate) part2: T,
}

impl fmt::Display for AocAnswer<String> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Day: {}", self.day)?;
        writeln!(f, "Part 1: {}", &self.part1)?;
        writeln!(f, "Part 2: {}", &self.part2)?;
        Ok(())
    }
}

impl fmt::Display for AocAnswer<Vec<i32>> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Day: {}", self.day)?;

        let _ = write!(f, "Part 1: ");
        for x in &self.part1 {
            print!("{}, ", x);
        }
        let _ = writeln!(f, "");
        
        let _ = write!(f, "Part 2: ");
        for x in &self.part2 {
            print!("{}, ", x);
        }
        let _ = writeln!(f, "");

        Ok(())
    }
}
