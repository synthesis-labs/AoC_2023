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
    let mut calibation_val:i32 = 0;
    let mut i_vec: Vec<&str> = Vec::new();
    i_vec.push("1");
    i_vec.push("2");
    i_vec.push("3");
    i_vec.push("4");
    i_vec.push("5");
    i_vec.push("6");
    i_vec.push("7");
    i_vec.push("8");
    i_vec.push("9");

    let mut s_vec: Vec<&str> = Vec::new();
    s_vec.push("zero");
    s_vec.push("one");
    s_vec.push("two");
    s_vec.push("three");
    s_vec.push("four");
    s_vec.push("five");
    s_vec.push("six");
    s_vec.push("seven");
    s_vec.push("eight");
    s_vec.push("nine");
    
    if let Ok(lines) = read_lines("./src/input/aoc2023d01.txt") {
        for line in lines {
            if let Ok(line) = line {
                let mut current_first = -1;
                let mut current_last = -1;
                let mut first_value= "";
                let mut last_value = "";
                let mut is_first_num = true;
                let mut is_last_num = true;

                //numbers
                for num in i_vec.iter() {
                    if line.find(num).is_some(){
                        let index = line.find(num).unwrap() as i32;
                        let lastIndex = line.rfind(num).unwrap() as i32;
                        //we know it exists, so now we iterate over the line to find the index of the sub-string.
                        if current_first == -1 {
                            first_value = num;
                            last_value= num;
                            current_first = index;
                            current_last = lastIndex;
                        }
                        if index < current_first {
                            first_value = num;
                            current_first = index;
                        }
                        if lastIndex > current_last {
                            last_value = num;
                            current_last = lastIndex;
                        }
                    }
                }
                //words
                for num in s_vec.iter() {
                    if line.find(num).is_some(){
                        let index = line.find(num).unwrap() as i32;
                        let last_index = line.rfind(num).unwrap() as i32;
                        //we know it exists, so now we iterate over the line to find the index of the sub-string.
                        if current_first == -1 {
                            if first_value == ""{
                                first_value = num;
                                is_first_num = false;
                            }
                            if last_value == ""{
                                last_value = num;
                                is_last_num = false;
                            }
                            current_first = index;
                            current_last = last_index;
                        }
                        if index < current_first {
                            is_first_num = false;
                            first_value = num;
                            current_first = index;
                        }
                        if last_index > current_last {
                            is_last_num = false;
                            last_value = num;
                            current_last = last_index;
                        }
                    }
                }
                //add normal nums to int nums
                let mut first_digit: String = "".to_string();
                let mut last_digit: String = "".to_string();
                
                if is_first_num {
                    first_digit = first_value.to_string();
                }
                else {
                    for num in 0..10 {
                        if s_vec[num] == first_value{
                            first_digit = num.to_string();
                        }
                    }
                }

                if is_last_num {
                    last_digit = last_value.to_string();
                }
                else{
                    for num in 0..10 {
                        if s_vec[num] == last_value{
                            last_digit = num.to_string(); 
                        }
                    }
                }
                let temp = format!("{}{}", first_digit, last_digit);
                calibation_val += temp.parse::<i32>().unwrap();
                //calibation_val +=  temp.parse::<i32>().unwrap();
            }
        }
    }
    println!("Day01Par2: {}", calibation_val);
}

fn read_lines<P>(filename: P) -> io::Result<io::Lines<io::BufReader<File>>>
where P: AsRef<Path>, {
    let file = File::open(filename)?;
    Ok(io::BufReader::new(file).lines())
}