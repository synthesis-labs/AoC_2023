use std::{collections::HashMap, ops::Sub};

use apply::Apply;

use crate::{
    models::{
        aoc_answer::AocAnswer,
        scratchcards::{Card, CardId, Intersection, MyNumbers, WinningNumbers},
    },
    utils::get_question_data::get_question_data,
};

// --- Day 4: Scratchcards ---

// --------------------------------------------------------------------------------------
// Boilerplate
// --------------------------------------------------------------------------------------
pub async fn solve() -> AocAnswer {
    let input_data = get_question_data(2023, 4)
        .await
        .expect("Could not get Day 4 data");

    let answer: AocAnswer = AocAnswer {
        day: 4,
        sample_solution_part1: sample_solution_part1(),
        sample_solution_part2: sample_solution_part2(),
        part1: part1(&input_data),
        part2: part2(&input_data),
    };
    return answer;
}

fn part1(input_data: &String) -> String {
    return input_data
        .split("\n")
        .filter(|x| !x.is_empty())
        .map(|x| x.to_string())
        .map(parse_card)
        .map(|x| intersect(x.winning_numbers, x.my_numbers))
        .map(calculate_card_points)
        .sum::<i32>()
        .to_string();
}

fn part2(input_data: &String) -> String {
    input_data
        .split("\n")
        .filter(|x| !x.is_empty())
        .map(|x| x.to_string())
        .map(parse_card)
        .collect::<Vec<Card>>()
        .apply(calculate_total_scratchcards)
        .values()
        .sum::<i32>()
        .to_string()
}

fn sample_solution_part1() -> String {
    let input_data = String::from("Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53\nCard 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19\nCard 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1\nCard 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83\nCard 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36\nCard 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11\n");

    part1(&input_data)
}

fn sample_solution_part2() -> String {
    let input_data = String::from("Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53\nCard 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19\nCard 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1\nCard 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83\nCard 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36\nCard 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11\n");

    part2(&input_data)
}

// --------------------------------------------------------------------------------------
// Actual solution
// --------------------------------------------------------------------------------------

fn calculate_total_scratchcards(cards: Vec<Card>) -> HashMap<CardId, i32> {
    let mut result: HashMap<CardId, i32> = cards.iter().map(|x| (x.card_id, 1)).collect();

    for card in cards {
        let intersection = intersect(card.winning_numbers.clone(), card.my_numbers.clone());
        for _ in 0..result[&card.card_id] {
            for j in 1..(intersection.len() + 1) {
                let card_id = card.card_id + j as CardId;
                result.entry(card_id).and_modify(|x| *x = *x + 1);
            }
        }
    }
    result
}

fn calculate_card_points(list: Intersection) -> i32 {
    if list.len() == 0 {
        return 0;
    }
    2_i32.pow(list.len().sub(1) as u32)
}

fn intersect(winning_numbers: WinningNumbers, my_numbers: MyNumbers) -> Intersection {
    winning_numbers
        .intersection(&my_numbers)
        .map(|x| *x)
        .collect()
}

fn parse_card(card: String) -> Card {
    let split: Vec<String> = card.split(": ").map(|x| x.to_string()).collect();

    let card_id_string = split[0].clone();
    let card_data: Vec<String> = split[1].clone().split("|").map(|x| x.to_string()).collect();

    let card_id: CardId = parse_card_id(card_id_string);
    let winning_numbers: WinningNumbers = parse_winning_numbers(card_data.get(0).unwrap().clone());
    let my_numbers: MyNumbers = parse_my_numbers(card_data.get(1).unwrap().clone());

    Card {
        card_id,
        winning_numbers,
        my_numbers,
    }
}

fn parse_card_id(card_id_str: String) -> i32 {
    card_id_str.replace("Card", "").trim().parse().unwrap()
}

fn parse_winning_numbers(winning_numbers_str: String) -> WinningNumbers {
    winning_numbers_str
        .split(" ")
        .filter(|x| !x.is_empty())
        .map(|x| x.parse().unwrap())
        .collect()
}

fn parse_my_numbers(my_numbers_str: String) -> MyNumbers {
    my_numbers_str
        .split(" ")
        .filter(|x| !x.is_empty())
        .map(|x| x.parse().unwrap())
        .collect()
}
