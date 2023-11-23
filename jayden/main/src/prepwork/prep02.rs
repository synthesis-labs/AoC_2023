use std::fs::File;
use std::io::{self, BufRead};
use std::path::Path;
use std::collections::BTreeMap;

pub fn part1(){
    let mut score: i32 = 0;
    let mut map: BTreeMap<&str, i32> = BTreeMap::new();

    map.insert("A X", 3 + 1);
    map.insert("A Y", 6 + 2);
    map.insert("A Z", 0 + 3);

    map.insert("B X", 0 + 1);
    map.insert("B Y", 3 + 2);
    map.insert("B Z", 6 + 3);

    map.insert("C X", 6 + 1);
    map.insert("C Y", 0 + 2);
    map.insert("C Z", 3 + 3);

    if let Ok(lines) = read_lines("./src/input/aoc2022d02.txt") {
        for line in lines {
            if let Ok(line) = line {

                //split the string into 2 chars
                let combo: &str = &line as &str;

                //find combo
                score += map.get(combo).unwrap();

            }
        }
    }
    println!("total for 22-d02 part1: {}", score);
}

pub fn part2(){
    let mut score: i32 = 0;
    let mut map: BTreeMap<&str, i32> = BTreeMap::new();

    map.insert("A X", 0 + 3);
    map.insert("A Y", 3 + 1);
    map.insert("A Z", 6 + 2);

    map.insert("B X", 0 + 1);
    map.insert("B Y", 3 + 2);
    map.insert("B Z", 6 + 3);

    map.insert("C X", 0 + 2);
    map.insert("C Y", 3 + 3);
    map.insert("C Z", 6 + 1);


    if let Ok(lines) = read_lines("./src/input/aoc2022d02.txt") {
        for line in lines {
            if let Ok(line) = line {
                //split the string into 2 chars
                let combo: &str = &line as &str;

                //find combo
                score += map.get(combo).unwrap();
                
            }
        }
    }
    println!("total for 22-d02 part2: {}", score);
}

fn read_lines<P>(filename: P) -> io::Result<io::Lines<io::BufReader<File>>>
where P: AsRef<Path>, {
    let file = File::open(filename)?;
    Ok(io::BufReader::new(file).lines())
}