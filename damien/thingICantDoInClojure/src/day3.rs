use std::{error::Error, collections::HashSet};

use regex::Regex;


type Res<T> = Result<T, Box<dyn Error>>;

#[derive(Eq, Clone)]
#[derive(PartialEq)]
#[derive(Hash, Debug)]
struct Num {
    val: i32,
    y: usize,
    startx: usize,
    endx: usize,
}

pub fn day3() -> Res<()> {
    let input = "467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..";
    // let input = include_str!("input.txt");

    let input_split: Vec<&str> = input.split("\n").collect();
    let mut numbers = Vec::new();

    let re = Regex::new(r"\d+")?;
    for (y, i) in input_split.iter().enumerate() {
        for cap in re.find_iter(i) {
            numbers.push(Num {
                val: cap.as_str().parse().unwrap(),
                y: y as usize,
                startx: cap.start() as usize,
                endx: cap.end() as usize - 1
            });
    
        }
    }
    
    part_one(numbers.clone(), input_split.clone());

    part_two(numbers.clone(), input_split.clone());

    Ok(())
}

fn part_two(numbers: Vec<Num>, input_split: Vec<&str>) {
    let mut total = 0;
    for y in 0..input_split.len() {
        for x in 0..input_split[y].len() {
            let current_char = input_split[y].as_bytes()[x];
            if !is_symbol(current_char) {
                continue;
            }

            let mut surrounding_nums = HashSet::new();
            for new_y in y - 1..y + 2 {
                for new_x in x - 1..x + 2 {                       
                    if let Some(num) = get_number_at(&numbers, new_x, new_y) {
                        println!("Number at x: {} y: {} val: {}", new_x, new_y, num.val);
                        surrounding_nums.insert(num);
                    }
                }
            }

            if surrounding_nums.len() == 2 {
                total += surrounding_nums.iter().fold(1, |acc: i32, val: &&Num| acc * val.val);
            }
        }
    }

    println!("Total {total}");
}

fn part_one(numbers: Vec<Num>, input_split: Vec<&str>) {

    let mut surr_nums = HashSet::new();
    for y in 0..input_split.len() {
        for x in 0..input_split[y].len() {
            let current_char = input_split[y].as_bytes()[x];
            if !is_symbol(current_char) {
                continue;
            }

            for new_y in y - 1..y + 2 {
                for new_x in x - 1..x + 2 {                       
                    println!("Number at x: {} y: {}", new_x, new_y);
                    if let Some(num) = get_number_at(&numbers, new_x, new_y) {
                        println!("Number at x: {} y: {} val: {}", new_x, new_y, num.val);
                        surr_nums.insert(num);
                    }
                }
            }
        }
    }


    let mut total = 0;
    for i in surr_nums.iter() {
        total += i.val;
    }

    println!("Total: {}", total);
}

fn is_symbol(b: u8) -> bool {
    b != b'.' && (b < b'0' || b > b'9')
}

fn get_number_at(numbers: &Vec<Num>, x: usize, y: usize) -> Option<&Num> {
    if x < 0 || y < 0  {
        return None;
    }

    for num in numbers {
        if num.y == y && x >= num.startx && x <= num.endx {
            return Some(num)
        }
    }

    return None
}
