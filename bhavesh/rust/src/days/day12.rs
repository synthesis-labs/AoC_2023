use itertools::Itertools;
use rayon::{
    iter::{IntoParallelRefIterator, ParallelIterator},
    str::ParallelString,
};

use crate::{models::aoc_answer::AocAnswer, utils::get_question_data::get_question_data};

// --- Day 12: Hot Springs ---

// --------------------------------------------------------------------------------------
// Boilerplate
// --------------------------------------------------------------------------------------
pub async fn solve() -> AocAnswer {
    let input_data = get_question_data(2023, 12)
        .await
        .expect("Could not get Day 12 data");

    let answer: AocAnswer = AocAnswer {
        day: 12,
        sample_solution_part1: sample_solution_part1(),
        sample_solution_part2: sample_solution_part2(),
        part1: part1(&input_data),
        part2: part2(&input_data),
    };
    return answer;
}

fn part1(input_data: &String) -> String {
    input_data
        .par_lines()
        .map(parse_input)
        .map(|(springs_str, damaged_springs)| {
            get_combinations(&get_question_mark_indexes(&springs_str))
                .par_iter()
                .filter(|x| is_valid_combination(&springs_str, x, &damaged_springs))
                .count() as i64
        })
        .sum::<i64>()
        .to_string()
}

fn part2(input_data: &String) -> String {
    input_data.len().to_string()
}

fn sample_solution_part1() -> String {
    let input_data = String::from("???.### 1,1,3\n.??..??...?##. 1,1,3\n?#?#?#?#?#?#?#? 1,3,1,6\n????.#...#... 4,1,1\n????.######..#####. 1,6,5\n?###???????? 3,2,1\n");

    part1(&input_data)
}

fn sample_solution_part2() -> String {
    let input_data = String::from("???.### 1,1,3\n.??..??...?##. 1,1,3\n?#?#?#?#?#?#?#? 1,3,1,6\n????.#...#... 4,1,1\n????.######..#####. 1,6,5\n?###???????? 3,2,1\n");

    // let result = input_data
    //     .par_lines()
    //     .map(parse_input)
    //     .map(|(springs_str, damaged_springs)| {
    //         (
    //             unfold_springs(&springs_str),
    //             unfold_damaged_springs(damaged_springs),
    //         )
    //     })
    //     .map(|(springs_str, damaged_springs)| {
    //         get_combinations(&get_question_mark_indexes(&springs_str))
    //             .par_iter()
    //             .filter(|x| is_valid_combination(&springs_str, x, &damaged_springs))
    //             .count() as i64
    //     })
    //     .sum::<i64>()
    //     .to_string();

    // println!("result: {}", result);

    part2(&input_data)
}

// --------------------------------------------------------------------------------------
// Actual solution
// --------------------------------------------------------------------------------------
// fn unfold_springs(str: &String) -> String {
//     repeat_n(str, 5).join("?").to_string()
// }

// fn unfold_damaged_springs(damaged_springs: Vec<i64>) -> Vec<i64> {
//     repeat_n(&damaged_springs, 5)
//         .flatten()
//         .map(|x| *x)
//         .collect()
// }

fn get_question_mark_indexes(str: &String) -> Vec<i64> {
    str.chars()
        .enumerate()
        .filter(|(_, c)| *c == '?')
        .map(|(i, _)| i as i64)
        .collect()
}

fn is_valid_combination(
    springs_str: &String,
    combination: &Vec<i64>,
    damaged_springs: &Vec<i64>,
) -> bool {
    let replaced_springs_str = replace_question_marks(springs_str, combination);

    let contiguous_damaged_springs = count_contiguous_damaged_springs(&replaced_springs_str);

    *damaged_springs == contiguous_damaged_springs
}

fn count_contiguous_damaged_springs(springs_str: &String) -> Vec<i64> {
    springs_str
        .split(".")
        .filter(|x| x.len() > 0)
        .map(|x| x.len() as i64)
        .collect()
}

fn replace_question_marks(springs_str: &String, combination: &Vec<i64>) -> String {
    let mut replaced_springs_str = "".to_string();
    for (i, c) in springs_str.chars().enumerate() {
        if c == '?' {
            let index = i as i64;
            if combination.contains(&index) {
                replaced_springs_str += "#";
            } else {
                replaced_springs_str += ".";
            }
        } else {
            replaced_springs_str += &c.to_string();
        }
    }

    replaced_springs_str
}

fn get_combinations(indexes_array: &Vec<i64>) -> Vec<Vec<i64>> {
    let num_replacements: i64 = indexes_array.len() as i64;
    let mut combinations: Vec<Vec<i64>> = Vec::new();
    combinations.push(vec![]); // for when there are no damaged springs (all replacements will be '.')

    for i in 1..num_replacements + 1 {
        combinations.extend(
            indexes_array
                .iter()
                .combinations(i as usize)
                .unique()
                .map(|x| x.iter().map(|y| **y).collect())
                .collect::<Vec<_>>(),
        )
    }

    combinations
}

fn parse_input(line: &str) -> (String, Vec<i64>) {
    (parse_springs(line), parse_damaged_springs(line))
}

fn parse_springs(line: &str) -> String {
    line.split_whitespace().nth(0).unwrap().to_string()
}

fn parse_damaged_springs(line: &str) -> Vec<i64> {
    line.split_whitespace()
        .nth(1)
        .unwrap()
        .split(',')
        .map(|x| x.parse::<i64>().unwrap())
        .collect()
}
