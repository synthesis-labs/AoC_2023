use std::fs::File;
use std::io::Read;
use num_traits::pow;


pub fn day_4_pt1() {
    let file = File::open("C:/Users/harsh/RustroverProjects/AoC_2023/harshil/AoC/src/Data/Day_4.txt");
    let mut contents = String::new();
    file.unwrap().read_to_string(&mut contents).expect("Error reading file");
    let mut sum=0;

    let mut winning: Vec<i32>=Vec::new();
    let mut yours: Vec<i32>=Vec::new();
    for line in contents.lines() {
        let game = &line[0..line.find(":").unwrap()];
        let mut line = line.replace(game, "");

        let winning: Vec<i32> = line[0..line.find("|").unwrap()].split_whitespace().map(|s| s.parse().unwrap()).collect();
        let yours: Vec<i32> = line[line.find("|").unwrap()..].split_whitespace().map(|s| s.parse().unwrap()).collect();

        let won = find_intersection(winning.clone(), yours.clone());
        sum += pow(2, won.len());
    }
    print!("{}", sum)

}

pub fn find_intersection(winning:Vec<i32>, yours:Vec<i32>) -> Vec<i32>{
    return winning.into_iter().filter(|x| yours.contains(x)).collect();
}

