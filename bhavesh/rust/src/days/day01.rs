use crate::{models::aoc_answer::AocAnswer, utils::get_question_data::get_question_data};

pub async fn solve() -> AocAnswer<String> {
    let input_data = get_question_data(2022, 1)
        .await
        .expect("Could not get Day 1 data");

    let answer: AocAnswer<String> = AocAnswer {
        day: 1,
        part1: part1(&input_data),
        part2: part2(&input_data),
    };
    return answer;
}

fn part1(input_data: &String) -> String {
    String::len(&input_data).to_string()
}

fn part2(_input_data: &String) -> String {
    String::from("Hello part 2")
}
