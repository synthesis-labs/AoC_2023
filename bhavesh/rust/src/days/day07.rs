extern crate apply;

use crate::{
    models::{
        aoc_answer::AocAnswer,
        camel_cards::{Card, Hand, HandType},
    },
    utils::get_question_data::get_question_data,
};

// --- Day 7: Camel Cards ---

// --------------------------------------------------------------------------------------
// Boilerplate
// --------------------------------------------------------------------------------------
pub async fn solve() -> AocAnswer {
    let input_data = get_question_data(2023, 7)
        .await
        .expect("Could not get Day 7 data");

    let answer: AocAnswer = AocAnswer {
        day: 7,
        sample_solution_part1: sample_solution_part1(),
        sample_solution_part2: sample_solution_part2(),
        part1: part1(&input_data),
        part2: part2(&input_data),
    };
    return answer;
}

fn part1(input_data: &String) -> String {
    input_data.lines().next().unwrap().to_string()
}

fn part2(input_data: &String) -> String {
    input_data.lines().next().unwrap().to_string()
}

fn sample_solution_part1() -> String {
    let input_data = String::from("32T3K 765\nT55J5 684\nKK677 28\nKTJJT 220\nQQQJA 483\n");

    determine_hand_type(parse_hand("T55J5".to_string()));

    part1(&input_data)
}

fn sample_solution_part2() -> String {
    let input_data = String::from("Time:      7  15   30\nDistance:  9  40  200\n");

    part2(&input_data)
}

// --------------------------------------------------------------------------------------
// Actual solution
// --------------------------------------------------------------------------------------
fn parse_hand(hand: String) -> Hand {
    hand.chars()
        .map(|c| match c {
            'A' => Card::Ace,
            'K' => Card::King,
            'Q' => Card::Queen,
            'J' => Card::Jack,
            'T' => Card::Ten,
            '9' => Card::Nine,
            '8' => Card::Eight,
            '7' => Card::Seven,
            '6' => Card::Six,
            '5' => Card::Five,
            '4' => Card::Four,
            '3' => Card::Three,
            '2' => Card::Two,
            _ => panic!("lol"),
        })
        .collect()
}

fn determine_hand_type(hand: Hand) -> HandType {
    let mut counts: Vec<i32> = vec![0; 13];

    for card in hand {
        counts[card as usize - 1] += 1
    }

    println!("{:?}", counts);

    HandType::FiveOfAKind
}
