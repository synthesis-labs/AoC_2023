use crate::{models::aoc_answer::AocAnswer, utils::get_question_data::get_question_data};

pub async fn solve() -> AocAnswer<Vec<i32>> {
    let response = get_question_data(2022, 2)
        .await
        .expect("Could not get Day 2 data");

    let result: AocAnswer<Vec<i32>> = AocAnswer {
        day: 2,
        part1: part1(),
        part2: part2(response.len() as i32),
    };

    result
}

fn part1() -> Vec<i32> {
    let a: i32 = 77;
    let mut b: Vec<i32> = Vec::new();
    b.insert(0, a);
    b.insert(0, a);
    b
}


fn part2(param: i32) -> Vec<i32> {
    let mut b: Vec<i32> = Vec::new();
    b.insert(0, param);
    b
}