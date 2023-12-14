use crate::{models::aoc_answer::AocAnswer, utils::get_question_data::get_question_data};

// --- Day 1: Trebuchet?! ---

// --------------------------------------------------------------------------------------
// Boilerplate
// --------------------------------------------------------------------------------------
pub async fn solve() -> AocAnswer {
    let input_data = get_question_data(2023, 1)
        .await
        .expect("Could not get Day 1 data");

    let answer: AocAnswer = AocAnswer {
        day: 1,
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
        .filter(|x| !x.is_empty())
        .map(|x| x.to_string())
        .map(match_digits)
        .map(calc_calibration_number)
        .sum::<i32>()
        .to_string()
}

fn part2(input_data: &String) -> String {
    input_data
        .split("\n")
        .filter(|x| !x.is_empty())
        .map(|x| x.to_string())
        .map(replace_words_with_digits)
        .map(match_digits)
        .map(calc_calibration_number)
        .sum::<i32>()
        .to_string()
}

fn sample_solution_part1() -> String {
    let input_data = String::from("1abc2\npqr3stu8vwx\na1b2c3d4e5f\ntreb7uchet\n");
    part1(&input_data)
}

fn sample_solution_part2() -> String {
    let input_data = String::from("two1nine\neightwothree\nabcone2threexyz\nxtwone3four\n4nineeightseven2\nzoneight234\n7pqrstsixteen\n");

    part2(&input_data)
}

// --------------------------------------------------------------------------------------
// Actual solution
// --------------------------------------------------------------------------------------
fn match_digits(string: String) -> Vec<i32> {
    string
        .chars()
        .filter(|c| c.is_digit(10))
        .map(|c| c.to_string())
        .map(|c| c.parse::<i32>().unwrap())
        .collect()
}

fn replace_words_with_digits(string: String) -> String {
    string
        .replace("one", "o1e")
        .replace("two", "t2o")
        .replace("three", "t3e")
        .replace("four", "f4r")
        .replace("five", "f5e")
        .replace("six", "s6x")
        .replace("seven", "s7n")
        .replace("eight", "e8t")
        .replace("nine", "n9e")
}

fn calc_calibration_number(calibration_numbers: Vec<i32>) -> i32 {
    let first: String = calibration_numbers[0].to_string();
    let last: String = calibration_numbers[calibration_numbers.len() - 1].to_string();

    (first + &last).parse().unwrap()
}
