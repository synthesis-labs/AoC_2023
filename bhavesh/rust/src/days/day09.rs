use crate::{models::aoc_answer::AocAnswer, utils::get_question_data::get_question_data};

// --- Day 9: Mirage Maintenance ---

// --------------------------------------------------------------------------------------
// Boilerplate
// --------------------------------------------------------------------------------------
pub async fn solve() -> AocAnswer {
    let input_data = get_question_data(2023, 9)
        .await
        .expect("Could not get Day 9 data");

    let answer: AocAnswer = AocAnswer {
        day: 9,
        sample_solution_part1: sample_solution_part1(),
        sample_solution_part2: sample_solution_part2(),
        part1: part1(&input_data),
        part2: part2(&input_data),
    };
    return answer;
}

fn part1(input_data: &String) -> String {
    input_data
        .split("\n")
        .filter(|s| !s.is_empty())
        .map(|s| s.to_string())
        .map(parse_sequence)
        .map(|sequence| get_differences(&sequence, vec![sequence.clone()]))
        .map(|differences| get_next_value_in_sequence(differences))
        .sum::<i32>()
        .to_string()
}

fn part2(input_data: &String) -> String {
    input_data
        .split("\n")
        .filter(|s| !s.is_empty())
        .map(|s| s.to_string())
        .map(parse_sequence)
        .map(|sequence| get_differences(&sequence, vec![sequence.clone()]))
        .map(|differences| get_previous_value_in_sequence(differences))
        .sum::<i32>()
        .to_string()
}

fn sample_solution_part1() -> String {
    let input_data = String::from("0 3 6 9 12 15\n1 3 6 10 15 21\n10 13 16 21 30 45\n");

    part1(&input_data)
}

fn sample_solution_part2() -> String {
    let input_data = String::from("0 3 6 9 12 15\n1 3 6 10 15 21\n10 13 16 21 30 45\n");

    part2(&input_data)
}
// --------------------------------------------------------------------------------------
// Actual solution
// --------------------------------------------------------------------------------------
fn parse_sequence(line: String) -> Vec<i32> {
    line.split(" ").map(|s| s.parse::<i32>().unwrap()).collect()
}

fn get_previous_value_in_sequence(mut differences: Vec<Vec<i32>>) -> i32 {
    let mut next_value = differences.remove(0)[0];

    let mut negative: bool = true;

    for difference in differences {
        if negative {
            next_value -= difference[0];
        } else {
            next_value += difference[0];
        }
        negative = !negative;
    }

    next_value
}

fn get_next_value_in_sequence(differences: Vec<Vec<i32>>) -> i32 {
    let mut next_value = 0;

    for difference in differences {
        next_value += difference[difference.len() - 1];
    }

    next_value
}

fn get_differences(sequence: &Vec<i32>, mut differences: Vec<Vec<i32>>) -> Vec<Vec<i32>> {
    if sequence.iter().max() == Some(&0) && sequence.iter().min() == Some(&0) {
        return differences;
    }

    let difference = sequence
        .windows(2)
        .map(|window| window[1] - window[0])
        .collect::<Vec<i32>>();

    differences.push(difference.clone());

    return get_differences(&difference, differences);
}
