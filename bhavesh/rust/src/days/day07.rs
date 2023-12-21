extern crate apply;

use strum::IntoEnumIterator;

use crate::{
    models::{
        aoc_answer::AocAnswer,
        camel_cards::{Bid, Card, Hand, HandRow, HandType, HandTypeKind, Hands},
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
    let mut hand_rows: Hands = input_data
        .split("\n")
        .filter(|s| !s.is_empty())
        .map(|s| parse_hand_row(s.to_string(), false))
        .collect();

    hand_rows.sort();

    let mut sum = 0;
    for i in 0..hand_rows.len() {
        sum += hand_rows[i].bid * (i as i32 + 1);
    }

    sum.to_string()
}

fn part2(input_data: &String) -> String {
    let mut hand_rows: Hands = input_data
        .split("\n")
        .filter(|s| !s.is_empty())
        .map(|s| parse_hand_row(s.to_string(), true))
        .collect();

    hand_rows.sort();

    let mut sum = 0;
    for i in 0..hand_rows.len() {
        sum += hand_rows[i].bid * (i as i32 + 1);
    }

    sum.to_string()
}

fn sample_solution_part1() -> String {
    let input_data = String::from("32T3K 765\nT55J5 684\nKK677 28\nKTJJT 220\nQQQJA 483\n");

    part1(&input_data)
}

fn sample_solution_part2() -> String {
    let input_data = String::from("32T3K 765\nT55J5 684\nKK677 28\nKTJJT 220\nQQQJA 483\n");

    part2(&input_data)
}

// --------------------------------------------------------------------------------------
// Actual solution
// --------------------------------------------------------------------------------------

fn parse_joker_hand_type(hand: &Hand) -> HandType {
    let mut best_hand: HandType = parse_hand_type(&hand);

    for card in Card::iter() {
        let new_hand = replace_jokers(hand.clone(), card);
        let new_hand_type = parse_hand_type(&new_hand);

        if new_hand_type.kind > best_hand.kind {
            best_hand = new_hand_type
        }
    }
    HandType {
        hand: replace_jokers(hand.clone(), Card::None),
        kind: best_hand.kind,
    }
}

fn replace_jokers(hand: Hand, replacement: Card) -> Hand {
    hand.iter()
        .map(|c| if *c == Card::Jack { replacement } else { *c })
        .collect()
}

fn parse_hand_row(row: String, joker: bool) -> HandRow {
    let mut split = row.split(" ");
    let hand = parse_hand(split.next().unwrap().to_string());
    let hand_type = if joker {
        parse_joker_hand_type(&hand)
    } else {
        parse_hand_type(&hand)
    };
    let bid = split.next().unwrap().parse::<Bid>().unwrap();

    HandRow { hand_type, bid }
}

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

fn parse_hand_type(hand: &Hand) -> HandType {
    let mut counts: Vec<i32> = vec![0; 14];

    for card in hand {
        counts[*card as usize - 1] += 1
    }

    if counts.contains(&5) {
        return HandType {
            hand: hand.clone(),
            kind: HandTypeKind::FiveOfAKind,
        };
    }
    if counts.contains(&4) {
        return HandType {
            hand: hand.clone(),
            kind: HandTypeKind::FourOfAKind,
        };
    }
    if counts.contains(&3) {
        counts.remove(counts.iter().position(|&x| x == 3).unwrap());
        if counts.contains(&2) {
            return HandType {
                hand: hand.clone(),
                kind: HandTypeKind::FullHouse,
            };
        }
        return HandType {
            hand: hand.clone(),
            kind: HandTypeKind::ThreeOfAKind,
        };
    }
    if counts.contains(&2) {
        counts.remove(counts.iter().position(|&x| x == 2).unwrap());
        if counts.contains(&2) {
            return HandType {
                hand: hand.clone(),
                kind: HandTypeKind::TwoPair,
            };
        }
        return HandType {
            hand: hand.clone(),
            kind: HandTypeKind::OnePair,
        };
    }

    HandType {
        hand: hand.clone(),
        kind: HandTypeKind::HighCard,
    }
}
