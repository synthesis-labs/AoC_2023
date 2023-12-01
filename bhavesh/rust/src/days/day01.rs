use crate::{models::aoc_answer::AocAnswer, utils::get_question_data::get_question_data};

// --- Day 1: Trebuchet?! ---

pub async fn solve() -> AocAnswer<i32> {
    let input_data = get_question_data(2023, 1)
        .await
        .expect("Could not get Day 1 data");

    let answer: AocAnswer<i32> = AocAnswer {
        day: 1,
        sample_solution: sample_solution(),
        part1: part1(&input_data),
        part2: part2(&input_data),
    };
    return answer;
}

fn part1(input_data: &String) -> i32 {
    return input_data
        .split("\n")
        .map(|x| find_first_and_last_number_in_string(x.to_string()))
        .sum();
}

fn part2(input_data: &String) -> i32 {
    String::len(input_data).try_into().unwrap()
}

fn sample_solution() -> i32 {
    let input_data = String::from("1abc2\npqr3stu8vwx\na1b2c3d4e5f\ntreb7uchet\n");

    input_data
        .split("\n")
        .map(|x| find_first_and_last_number_in_string(x.to_string()))
        .sum()
}

fn find_first_and_last_number_in_string(string: String) -> i32 {
    if string.len() == 0 {
        return 0;
    }

    let s = string.chars().filter(|c| is_integer(*c));

    let first: String = s.clone().next().unwrap().to_string();
    let binding = s.last().unwrap().to_string();
    let last: &str = binding.as_str();

    let combined = first + last;

    if combined.len() == 0 {
        return 0;
    }

    return combined.parse::<i32>().unwrap();
}

fn is_integer(c: char) -> bool {
    return c.is_digit(10);
}
