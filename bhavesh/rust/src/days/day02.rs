use crate::{models::aoc_answer::AocAnswer, utils::get_question_data::get_question_data};

pub async fn solve() -> AocAnswer {
    let response = get_question_data(2022, 2).await;

    let result = AocAnswer {
        day: 2,
        part1: part1().to_string(),
        part2: response.len().to_string(),
    };

    result
}

fn part1() -> i32 {
    0
}

