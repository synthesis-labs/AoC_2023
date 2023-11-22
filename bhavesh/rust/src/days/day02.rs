use crate::{models::aoc_answer::AocAnswer, utils::get_question_data::get_question_data};

pub async fn solve() -> AocAnswer<i32> {
    let input_data = get_question_data(2022, 2)
        .await
        .expect("Could not get Day 2 data");

    let result: AocAnswer<i32> = AocAnswer {
        day: 2,
        part1: part1(&input_data),
        part2: part2(&input_data),
    };

    result
}

fn part1(_input_data: &String) -> i32 {
    77
}

fn part2(input_data: &String) -> i32 {
    String::len(input_data).try_into().unwrap()
}
