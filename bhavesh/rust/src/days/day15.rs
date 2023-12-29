use std::collections::HashMap;

use apply::Apply;
use itertools::Itertools;

use crate::{
    models::{
        aoc_answer::AocAnswer,
        lens_library::{Box, Boxes, FocalLength, Lens, LensLabel, Lenses},
    },
    utils::get_question_data::get_question_data,
};

// --- Day 15: Lens Library ---

// --------------------------------------------------------------------------------------
// Boilerplate
// --------------------------------------------------------------------------------------
pub async fn solve() -> AocAnswer {
    let input_data = get_question_data(2023, 15)
        .await
        .expect("Could not get Day 15 data");

    let answer: AocAnswer = AocAnswer {
        day: 15,
        sample_solution_part1: sample_solution_part1(),
        sample_solution_part2: sample_solution_part2(),
        part1: part1(&input_data),
        part2: part2(&input_data),
    };
    return answer;
}

fn part1(input_data: &String) -> String {
    input_data
        .split(",")
        .filter(|s| !s.is_empty())
        .map(|s| s.trim().to_string())
        .map(|s| hash(&s))
        .sum::<i32>()
        .to_string()
}

fn part2(input_data: &String) -> String {
    let instructions: Vec<String> = input_data
        .split(",")
        .filter(|s| !s.is_empty())
        .map(|s| s.trim().to_string())
        .collect();

    create_boxes(instructions)
        .apply(calc_focusing_power)
        .to_string()
}

fn sample_solution_part1() -> String {
    let input_data = String::from("rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7\n");

    part1(&input_data)
}

fn sample_solution_part2() -> String {
    let input_data = String::from("rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7\n");

    part2(&input_data)
}

// --------------------------------------------------------------------------------------
// Actual solution
// --------------------------------------------------------------------------------------
fn calc_focusing_power(boxes: Boxes) -> i64 {
    let mut sum = 0;
    for key in boxes.keys().sorted() {
        let lenses = &boxes[key];
        for (index, lens) in lenses.iter().enumerate() {
            let box_val = (key + 1) as i64;
            let slot_val = (index + 1) as i64;
            let focal_length_val = lens.focal_length as i64;

            sum += box_val * slot_val * focal_length_val;
        }
    }

    sum
}

fn create_boxes(instructions: Vec<String>) -> Boxes {
    let mut boxes: Boxes = HashMap::new();
    for instruction in instructions {
        if (&instruction).contains("=") {
            let x_split: Vec<&str> = instruction.split("=").collect();
            let lens: LensLabel = x_split[0].to_string();
            let focal_length: FocalLength = x_split[1].parse().unwrap();
            add_lens(lens, focal_length, &mut boxes);
        } else {
            let x_split: Vec<&str> = instruction.split("-").collect();
            let lens: LensLabel = x_split[0].to_string();
            remove_lens(lens, &mut boxes);
        }
    }
    boxes
}

fn add_lens(lens: LensLabel, focal_length: FocalLength, boxes: &mut Boxes) {
    let hash = hash(&lens);
    let new_lens = Lens {
        label: lens.clone(),
        focal_length,
    };

    if !boxes.contains_key(&hash) {
        let lenses: Lenses = vec![new_lens].into();
        boxes.insert(hash, lenses);
        return;
    }

    // modify in place if the lens already exists
    let index = boxes
        .get(&hash)
        .unwrap()
        .iter()
        .position(|x| x.label == lens);

    if index.is_none() {
        boxes.get_mut(&hash).unwrap().push_back(new_lens);
        return;
    }

    boxes.get_mut(&hash).unwrap()[index.unwrap()] = new_lens;
}

fn remove_lens(lens: LensLabel, boxes: &mut Boxes) {
    let hash = hash(&lens);

    if !boxes.contains_key(&hash) {
        return;
    }

    let index = boxes
        .get(&hash)
        .unwrap()
        .iter()
        .position(|x| x.label == lens);

    if index.is_none() {
        return;
    }

    boxes.get_mut(&hash).unwrap().remove(index.unwrap());
}

fn hash(value: &String) -> Box {
    let ascii = value.chars().map(|c| c as i32).collect::<Vec<i32>>();
    let mut hash = 0;
    for i in ascii {
        hash = hash + i;
        hash = hash * 17;
        hash = hash % 256;
    }
    hash
}
