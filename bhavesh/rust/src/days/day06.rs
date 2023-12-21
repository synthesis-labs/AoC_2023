extern crate apply;

use crate::{models::aoc_answer::AocAnswer, utils::get_question_data::get_question_data};
use apply::Apply;

// --- Day 6: Wait For It ---

// --------------------------------------------------------------------------------------
// Boilerplate
// --------------------------------------------------------------------------------------
pub async fn solve() -> AocAnswer {
    let input_data = get_question_data(2023, 6)
        .await
        .expect("Could not get Day 6 data");

    let answer: AocAnswer = AocAnswer {
        day: 6,
        sample_solution_part1: sample_solution_part1(),
        sample_solution_part2: sample_solution_part2(),
        part1: part1(&input_data),
        part2: part2(&input_data),
    };
    return answer;
}

fn part1(input_data: &String) -> String {
    let (time_data, distance_data) = parse_data(input_data.clone());

    check_games(&time_data, &distance_data).to_string()
}

fn part2(input_data: &String) -> String {
    let (time_data, distance_data) = parse_data_with_kerning(input_data.clone());

    check_games(&vec![time_data], &vec![distance_data]).to_string()
}

fn sample_solution_part1() -> String {
    let input_data = String::from("Time:      7  15   30\nDistance:  9  40  200\n");

    part1(&input_data)
}

fn sample_solution_part2() -> String {
    let input_data = String::from("Time:      7  15   30\nDistance:  9  40  200\n");

    part2(&input_data)
}

// --------------------------------------------------------------------------------------
// Actual solution
// --------------------------------------------------------------------------------------
fn check_games(time_data: &Vec<i64>, distance_data: &Vec<i64>) -> i64 {
    let mut result = 1;
    for game in 0..time_data.len() {
        let time = time_data[game];
        let distance = distance_data[game];

        let wins = count_wins(time, distance);

        result *= wins;
    }

    result
}

fn count_wins(time: i64, distance: i64) -> i64 {
    let mut wins = 0;
    for hold in 0..time + 1 {
        let remaining = time - hold;
        let possible_distance = hold * remaining;

        if distance < possible_distance {
            wins += 1;
        }
    }
    wins
}

fn parse_data_with_kerning(input_data: String) -> (i64, i64) {
    let data = input_data
        .split("\n")
        .filter(|x| !x.is_empty())
        .map(|x| x.to_string());

    let time_data: i64 = data
        .clone()
        .nth(0)
        .unwrap()
        .apply(parse_time_data_with_kerning);
    let distance_data: i64 = data
        .clone()
        .nth(1)
        .unwrap()
        .apply(parse_distance_data_with_kerning);

    (time_data, distance_data)
}

fn parse_time_data_with_kerning(line: String) -> i64 {
    line.replace("Time:", "")
        .trim()
        .split(' ')
        .filter(|s| !s.is_empty())
        .fold(String::new(), |a, b| a + b + "")
        .parse::<i64>()
        .unwrap()
}

fn parse_distance_data_with_kerning(line: String) -> i64 {
    line.replace("Distance:", "")
        .trim()
        .split(' ')
        .filter(|s| !s.is_empty())
        .fold(String::new(), |a, b| a + b + "")
        .parse::<i64>()
        .unwrap()
}

fn parse_data(input_data: String) -> (Vec<i64>, Vec<i64>) {
    let data = input_data
        .split("\n")
        .filter(|x| !x.is_empty())
        .map(|x| x.to_string());

    let time_data: Vec<i64> = data.clone().nth(0).unwrap().apply(parse_time_data);
    let distance_data: Vec<i64> = data.clone().nth(1).unwrap().apply(parse_distance_data);

    (time_data, distance_data)
}

fn parse_time_data(line: String) -> Vec<i64> {
    line.replace("Time:", "")
        .trim()
        .split(' ')
        .filter(|s| !s.is_empty())
        .map(|s| s.parse::<i64>().unwrap())
        .collect()
}

fn parse_distance_data(line: String) -> Vec<i64> {
    line.replace("Distance:", "")
        .trim()
        .split(' ')
        .filter(|s| !s.is_empty())
        .map(|s| s.parse::<i64>().unwrap())
        .collect()
}
