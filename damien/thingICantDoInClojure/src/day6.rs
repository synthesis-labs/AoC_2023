use std::iter::zip;

use crate::common::Res;

use regex::Regex;

fn part1(times: &Vec<i64>, distances: &Vec<i64>) {
    let mut total_possible_wins = 1;
    for (time, dist) in zip(times, distances) {

        let mut possible_wins = 0;
        for i in 0..*time {
            let speed = i;
            let time_left = time - i;
            let distance = speed * time_left;
            if distance > *dist {
                possible_wins += 1;
            }
        }

        total_possible_wins *= possible_wins;
    }

    println!("total possible wins {total_possible_wins}");
}

fn part2(input: &str) {
    let input_without_spaces = input.replace(" ", "");
    let inputs_split_no_space: Vec<_> = input_without_spaces.split("\n").collect();
    
    let re = Regex::new(r"\d+").unwrap();
    let time:  i64 = re.find_iter(inputs_split_no_space[0])
        .map(|s| s.as_str().parse().unwrap())
        .collect::<Vec<i64>>()[0];
    let distance:  i64 = re.find_iter(inputs_split_no_space[1])
        .map(|s| s.as_str().parse().unwrap())
        .collect::<Vec<i64>>()[0];
    
    let mut possible_wins = 0;
    for i in 0..time {
        let speed = i;
        let time_left = time - i;
        let dist = speed * time_left;
        if dist > distance {
            possible_wins += 1;
        }
    }

    println!("possible wins {possible_wins}");
}

pub fn day6() -> Res<()> {

    let input = "Time:      7  15   30
Distance:  9  40  200";
    
    // let input = include_str!("input.txt");
    
    let inputs_split: Vec<_> = input.split("\n").collect();
    let re = Regex::new(r"\d+")?;
    let times:  Vec<i64> = re.find_iter(inputs_split[0])
        .map(|s| s.as_str().parse().unwrap())
        .collect();
    let distances:  Vec<i64> = re.find_iter(inputs_split[1])
        .map(|s| s.as_str().parse().unwrap())
        .collect();

    part1(&times, &distances);

    part2(input);

    Ok(())
}
