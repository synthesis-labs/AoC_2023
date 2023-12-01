use std::fs::File;
use std::io::{self, BufRead};
use std::path::Path;
use std::collections::BTreeMap;

pub fn day01p1(){
    let mut calibation_val:i32 = 0;
    
    if let Ok(lines) = read_lines("./src/input/aoc2023d01.txt") {
        for line in lines {
            let mut first_num: char = '0';
            let mut last_num: char = '0';
            if let Ok(line) = line {
                let cvec: Vec<char> = line.chars().collect();
                for character in cvec {
                    if character.is_numeric() {
                        if first_num == '0'{
                            first_num = character;
                            last_num = character;
                        }
                        else {
                            last_num = character;
                        }
                    }
                }
                let temp = format!("{}{}", first_num, last_num);
                calibation_val +=  temp.parse::<i32>().unwrap();
            }
        }
    }
    println!("Day01Par1: {}", calibation_val);
}

pub fn day01p2(){
    
}

fn read_lines<P>(filename: P) -> io::Result<io::Lines<io::BufReader<File>>>
where P: AsRef<Path>, {
    let file = File::open(filename)?;
    Ok(io::BufReader::new(file).lines())
}