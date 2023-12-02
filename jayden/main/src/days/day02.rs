use std::fs::File;
use std::io::{self, BufRead};
use std::path::Path;


pub fn day02p1(){
    if let Ok(lines) = read_lines("./src/input/aoc2023d02.txt") {
        let mut game_num = 0;
        let mut game_num_sum = 0;
        for line in lines {
            game_num += 1;
            if let Ok(line) = line {
                let game: &str = &line;
                let mut handful: Vec<&str> = Vec::new();//total of hand
                let mut in_hand: Vec<&str> = Vec::new();//content of hand (number of each block type)
                let mut colour_split: Vec<&str> = Vec::new();//used to split the number away from the colour
                let mut is_game_valid = true;
                
                //need to take out the Game 1: from the game string
                let mut fix_input :Vec<&str> = Vec::new();
                fix_input = game.rsplit(':').collect();

                //split the line into sub strings, based on ;
                handful = fix_input[0].rsplit(';').collect();
                
                //for each in the vec, split into another vec based on ,
                for hand in  handful{
                    //eg 3 blue, 4 red;
                    in_hand = hand.rsplit(',').collect();
                    
                    //check if its red, green or blue (take out the word when found)
                    for colour in in_hand {
                        //println!("checking sanity: {}", colour);
                        //eg. 10 red
                        if colour.contains("red"){
                            //println!("the colour is red. count of blocks: {}", colour);
                            colour_split = colour.rsplit(' ').collect();//the remainder should be just the number
                            if colour_split[1].parse::<i32>().unwrap() > 12{
                                //println!("the game: {} is invalid", game_num);
                                is_game_valid = false;
                            }
                        }
                        else if colour.contains("green"){
                            //println!("the colour is green. count of blocks: {}", colour);
                            colour_split = colour.rsplit(' ').collect();
                            if colour_split[1].parse::<i32>().unwrap() > 13{
                                //println!("the game: {} is invalid", game_num);
                                is_game_valid = false;
                            }
                        }
                        else if colour.contains("blue"){
                            //println!("the colour is blue. count of blocks: {}", colour);
                            colour_split = colour.rsplit(' ').collect();
                            if colour_split[1].parse::<i32>().unwrap() > 14{
                                //println!("the game: {} is invalid", game_num);
                                is_game_valid = false;
                            }
                        }
                    }
                }
                //if the game is possible, add it to the game_sum_total
                if is_game_valid{
                    //println!("the game number: {} was deemed valid", game_num);
                    game_num_sum += game_num;
                }
            }
        }
        println!("Day02Part1: {}", game_num_sum)
    }
}

pub fn day02p2(){
    if let Ok(lines) = read_lines("./src/input/aoc2023d02.txt") {
        let mut game_num = 0;
        let mut total_power_sum = 0;
        for line in lines {
            game_num += 1;
            let mut game_power_sum = 0;
            if let Ok(line) = line {
                let game: &str = &line;
                let mut handful: Vec<&str> = Vec::new();//total of hand
                let mut in_hand: Vec<&str> = Vec::new();//content of hand (number of each block type)
                let mut colour_split: Vec<&str> = Vec::new();//used to split the number away from the colour
                let mut is_game_valid = true;

                //always 1, since there is always at least 1
                let mut red_min = 0;
                let mut green_min = 0;
                let mut blue_min = 0;
                
                //need to take out the Game 1: from the game string
                let mut fix_input :Vec<&str> = Vec::new();
                fix_input = game.rsplit(':').collect();

                //split the line into sub strings, based on ;
                handful = fix_input[0].rsplit(';').collect();
                
                //for each in the vec, split into another vec based on ,
                for hand in  handful{
                    //eg 3 blue, 4 red;
                    in_hand = hand.rsplit(',').collect();
                    
                    //check if its red, green or blue (take out the word when found)
                    for colour in in_hand {
                        //println!("checking sanity: {}", colour);
                        //eg. 10 red
                        if colour.contains("red"){
                            //println!("the colour is red. count of blocks: {}", colour);
                            colour_split = colour.rsplit(' ').collect();//the remainder should be just the number
                            if colour_split[1].parse::<i32>().unwrap() > red_min{
                                //println!("the game: {} is invalid", game_num);
                                red_min = colour_split[1].parse::<i32>().unwrap();
                            }
                        }
                        else if colour.contains("green"){
                            //println!("the colour is green. count of blocks: {}", colour);
                            colour_split = colour.rsplit(' ').collect();
                            if colour_split[1].parse::<i32>().unwrap() > green_min{
                                //println!("the game: {} is invalid", game_num);
                                green_min = colour_split[1].parse::<i32>().unwrap();
                            }
                        }
                        else if colour.contains("blue"){
                            //println!("the colour is blue. count of blocks: {}", colour);
                            colour_split = colour.rsplit(' ').collect();
                            if colour_split[1].parse::<i32>().unwrap() > blue_min{
                                //println!("the game: {} is invalid", game_num);
                                blue_min = colour_split[1].parse::<i32>().unwrap();
                            }
                        }
                    }
                }
                //println!("total power for game: {} is: {}", game_num, red_min*blue_min*green_min);

                total_power_sum += red_min*blue_min*green_min;
            }
        }
        println!("Day02Part2: {}", total_power_sum)
    }
}

fn read_lines<P>(filename: P) -> io::Result<io::Lines<io::BufReader<File>>>
where P: AsRef<Path>, {
    let file = File::open(filename)?;
    Ok(io::BufReader::new(file).lines())
}