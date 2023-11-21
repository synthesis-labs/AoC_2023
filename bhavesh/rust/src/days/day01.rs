use crate::{models::aoc_answer::AocAnswer, utils::get_question_data::get_question_data};

pub async fn solve() -> AocAnswer<String> {
    let response = get_question_data(2022, 1)
        .await
        .expect("Could not get Day 1 data");

    let answer: AocAnswer<String> = AocAnswer {
        day: 1,
        part1: String::len(&response).to_string(),
        part2: String::from("Hello part 2"),
    };
    return answer;
}
