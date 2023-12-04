use std::fs::File;
use std::io::{self, BufRead};
use std::ops::Index;
use std::path::Path;
use std::str::Chars;

pub fn day03p1(){
    if let Ok(lines) = read_lines("./src/input/testing.txt") {
        let mut line_vec: Vec<Chars<'_>> = Vec::new();

        for line in lines {
            if let Ok(line) = line {

                let line_copy: &str = &line as &str;
                //change the line into an array chars
                let copy = line_copy;
                let line_chars = copy.chars().clone();

                //add the char array into a vector (Vec[1] = 2nd line of the input)
                 line_vec.push(line_chars);

                //note that the first time through the 
            }
        }
        //println!("Day02Part1: {}", game_num_sum)
    }
}

pub fn day03p2(){
    
}

fn read_lines<P>(filename: P) -> io::Result<io::Lines<io::BufReader<File>>>
where P: AsRef<Path>, {
    let file = File::open(filename)?;
    Ok(io::BufReader::new(file).lines())
}