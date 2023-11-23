use std::fs::File;
use std::io::{self, BufRead};
use std::path::Path;
use std::collections::BTreeMap;

pub fn calorie_counting_part1(){
    let mut highest_cal: i32 = 0;
    let mut current_cals: i32 = 0;

    if let Ok(lines) = read_lines("./src/input/aoc2022d01.txt") {
        for line in lines {
            if let Ok(cal_line) = line {
                if cal_line == "" {
                    if current_cals > highest_cal {
                        highest_cal = current_cals;
                    }
                    current_cals = 0;
                }
                else{
                    current_cals += cal_line.parse::<i32>().unwrap();
                }
            }
        }
    }
    println!("fattest elf cal count: {}", highest_cal)
}

pub fn calorie_counting_part2(){
    let mut map: BTreeMap<i32, &str> = BTreeMap::new();
    map.insert(1, "");
    map.insert(2, "");
    map.insert(3, "");

    let mut current_cals: i32 = 0;

    if let Ok(lines) = read_lines("./src/input/aoc2022d01.txt") {
        for line in lines {
            if let Ok(cal_line) = line {
                if cal_line == "" {
                    map.insert(current_cals, "1");
                    map.pop_first();
                    current_cals = 0;
                }
                else{
                    current_cals += cal_line.parse::<i32>().unwrap();
                }
            }
        }
    }
    let values = map.keys().cloned();
    current_cals = 0;
    for val in values {
        current_cals += val;
    }
    println!("top 3 elf cal count: {}", current_cals);
}

fn read_lines<P>(filename: P) -> io::Result<io::Lines<io::BufReader<File>>>
where P: AsRef<Path>, {
    let file = File::open(filename)?;
    Ok(io::BufReader::new(file).lines())
}