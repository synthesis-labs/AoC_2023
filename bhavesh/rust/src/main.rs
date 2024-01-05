use std::io::stdin;

use apply::Apply;
use dotenv::dotenv;
use models::aoc_answer::AocAnswer;

mod days;
pub mod models;
pub mod utils;

#[tokio::main]
async fn main() {
    dotenv().ok();

    println!("Advent of Code 2023");
    println!("Which day would you like to solve?");

    let mut day = String::new();
    let _ = stdin().read_line(&mut day);
    let day: i32 = day.trim().parse().expect("Please type a number!");
    let answer: AocAnswer = day.apply(parse_day).await;

    println!("answer: {}", answer);
}

async fn parse_day(day: i32) -> AocAnswer {
    match day {
        1 => days::day01::solve().await,
        2 => days::day02::solve().await,
        3 => days::day03::solve().await,
        4 => days::day04::solve().await,
        5 => days::day05::solve().await,
        6 => days::day06::solve().await,
        7 => days::day07::solve().await,
        8 => days::day08::solve().await,
        9 => days::day09::solve().await,
        10 => days::day10::solve().await,
        11 => days::day11::solve().await,
        12 => days::day12::solve().await,
        15 => days::day15::solve().await,
        19 => days::day19::solve().await,
        20 => days::day20::solve().await,
        _ => {
            if day > 0 && day < 50 {
                panic!("This day is not yet implemented")
            } else {
                panic!("Unknown day")
            }
        }
    }
}
