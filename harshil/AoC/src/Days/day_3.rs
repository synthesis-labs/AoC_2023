use std::fs::File;
use std::io::Read;

pub fn day_2_pt1() {
    let file = File::open("C:/Users/harsh/RustroverProjects/AoC_2023/harshil/AoC/src/Data/Day_2.txt");
    let mut contents = String::new();
    file.unwrap().read_to_string(&mut contents).expect("Error reading file");

    let mut vcontent: Vec<String>= contents.split("\n").collect();
    
}

pub  fn check_for_symbol(line:String)->bool{
    
}

pub fn get_Index_number(line:String,indicies: Vec<i32>){
    
}

pub fn get_number(line:String) -> Vec<i32>{
    let mut numbers:Vec<i32> = vec![];
    let mut tnum:String=String::new();
    for (index, char) in line.char_indices() {
        if line.chars().nth(index).unwrap().is_digit(10) {
            tnum.push(
                line.chars()
                    .nth(index)
                    .and_then(|c| c.to_digit(10))
                    .and_then(|d| std::char::from_digit(d, 10))
                    .unwrap_or('0'),
            );
        }else {
            numbers.push(tnum.parse().unwrap());
            tnum=String::new();
        }
    }
    println!("{:?}", numbers);
    return numbers
}