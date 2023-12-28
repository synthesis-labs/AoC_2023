use std::collections::HashMap;

use apply::Apply;

use crate::{
    models::{
        aoc_answer::AocAnswer,
        haunted_wasteland::{Branches, Instructions, Map, Node, Nodes},
    },
    utils::get_question_data::get_question_data,
};

// --- Day 8: Haunted Wasteland ---

// --------------------------------------------------------------------------------------
// Boilerplate
// --------------------------------------------------------------------------------------
pub async fn solve() -> AocAnswer {
    let input_data = get_question_data(2023, 8)
        .await
        .expect("Could not get Day 8 data");

    let answer: AocAnswer = AocAnswer {
        day: 8,
        sample_solution_part1: sample_solution_part1(),
        sample_solution_part2: sample_solution_part2(),
        part1: part1(&input_data),
        part2: part2(&input_data),
    };
    return answer;
}

fn part1(input_data: &String) -> String {
    let (instructions, map) = parse_input(input_data);

    find_graph_length_simple(&instructions, &map).to_string()
}

fn part2(input_data: &String) -> String {
    let (instructions, map) = parse_input(input_data);

    let nodes: Nodes = map
        .keys()
        .filter(|x| x.chars().last().unwrap() == 'A')
        .map(|x| x.to_string())
        .collect();

    let counts = nodes
        .iter()
        .map(|x| find_graph_length_complex(x.to_string(), &instructions, &map))
        .collect::<Vec<i64>>();

    let mut lcm = 1;
    for count in counts {
        lcm = num::integer::lcm(count, lcm);
    }

    lcm.to_string()
}

fn sample_solution_part1() -> String {
    let input_data_1 = String::from("RL\n\nAAA = (BBB, CCC)\nBBB = (DDD, EEE)\nCCC = (ZZZ, GGG)\nDDD = (DDD, DDD)\nEEE = (EEE, EEE)\nGGG = (GGG, GGG)\nZZZ = (ZZZ, ZZZ)\n");
    let input_data_2 =
        String::from("LLR\n\nAAA = (BBB, BBB)\nBBB = (AAA, ZZZ)\nZZZ = (ZZZ, ZZZ)\n");

    format!(
        "sample_1: {} sample_2: {}",
        part1(&input_data_1),
        part1(&input_data_2)
    )
}

fn sample_solution_part2() -> String {
    let input_data = String::from("LR\n\n11A = (11B, XXX)\n11B = (XXX, 11Z)\n11Z = (11B, XXX)\n22A = (22B, XXX)\n22B = (22C, 22C)\n22C = (22Z, 22Z)\n22Z = (22B, 22B)\nXXX = (XXX, XXX)\n");

    part2(&input_data)
}

// --------------------------------------------------------------------------------------
// Actual solution
// --------------------------------------------------------------------------------------
fn find_graph_length_simple(instructions: &Instructions, map: &Map) -> i32 {
    let mut count = 0;
    let mut instructions_iter = instructions.iter().cycle();

    let mut node: &Node = &"AAA".to_string();
    while node != "ZZZ" {
        let branch = instructions_iter.next().unwrap();
        count += 1;
        node = map.get(node).unwrap().get(branch).unwrap();
    }
    count
}

fn find_graph_length_complex(node: Node, instructions: &Instructions, map: &Map) -> i64 {
    let mut instructions_iter = instructions.iter().cycle();
    let mut count = 0;
    let mut n: &Node = &node;
    while n.chars().last().unwrap() != 'Z' {
        let branch = instructions_iter.next().unwrap();
        count += 1;
        n = map.get(n).unwrap().get(branch).unwrap();
    }
    count
}

fn parse_input(input_data: &String) -> (Instructions, Map) {
    let split: Vec<String> = input_data
        .split("\n\n")
        .filter(|x| !x.is_empty())
        .map(|x| x.to_string())
        .collect();

    let instructions: Instructions = split[0].chars().filter(|x| !x.is_whitespace()).collect();

    let mut map: Map = HashMap::new();

    for line in split[1].lines() {
        let split: Vec<String> = line
            .split(" = ")
            .filter(|x| !x.is_empty())
            .map(|x| x.to_string())
            .collect();

        let branches: Branches = split[1]
            .split(", ")
            .map(|x| x.to_string())
            .map(|x| x.replace("(", "").replace(")", ""))
            .apply(|mut x| {
                let left = ('L', x.next().unwrap());
                let right = ('R', x.next().unwrap());

                HashMap::from([left, right])
            });

        map.insert(split[0].clone(), branches);
    }

    (instructions, map)
}
