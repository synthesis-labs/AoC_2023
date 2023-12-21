use crate::common::Res;

use regex::Regex;
use rayon::prelude::*;

#[derive(Debug)]
struct Mapping {
    start: i64,
    end: i64,
    change: i64
}

fn get_mappings(source: &str) -> Vec<Mapping> {
    let source_split: Vec<_> = source.split("\n").collect();

    source_split
        .iter()
        .skip(1)
        .filter(|s| !s.trim().is_empty())
        .map(|s| {
            let numbers = s.split(" ")
                .map(|ss| {
                    ss.parse().unwrap()
                }).collect::<Vec<i64>>();
            Mapping {
                start: numbers[1],
                end: numbers[1] + numbers[2],
                change: numbers[0] - numbers[1]
            }
        }).collect()
}

fn map_val(mappings: &Vec<Mapping>, current_val: i64) -> i64 {
    for i in mappings {
        if current_val >= i.start && current_val < i.end {
            return current_val + i.change;
        }
    }
    
    return current_val;
}

fn part1(input: &str) -> Res<()> {
    let inputs_split: Vec<_> = input.split("\n\n").collect();

    let re = Regex::new(r"\d+")?;
    let seeds:  Vec<i64> = re.find_iter(inputs_split[0])
        .map(|s| s.as_str().parse().unwrap())
        .collect();

    let mappings: Vec<_> = inputs_split.iter().skip(1).map(|i| get_mappings(i)).collect();

    let mut lowest = i64::MAX;
    for seed in seeds {
        let mut current_val = seed;
        println!("seed {seed}");
        for i in &mappings {
            current_val = map_val(&i, current_val);
            println!("newval {current_val}");
        }
        if current_val < lowest {
            lowest = current_val
        }
    }

    println!("Lowest {lowest}");
    Ok(())
}

fn part2(input: &str) -> Res<()> {
    
    let inputs_split: Vec<_> = input.split("\n\n").collect();

    let re = Regex::new(r"\d+")?;
    let seed_ranges:  Vec<i64> = re.find_iter(inputs_split[0])
        .map(|s| s.as_str().parse().unwrap())
        .collect();

    let mappings: Vec<_> = inputs_split.iter().skip(1).map(|i| get_mappings(i)).collect();

    let lowestest = (0..seed_ranges.len())
        .into_par_iter()
        .step_by(2)
        .map(|i| {
        let mut lowest = i64::MAX;
        // let mut count = 0;
        for j in seed_ranges[i]..(seed_ranges[i] + seed_ranges[i + 1]) {
            // count += 1;
            // if count % 16384 == 0 {
            //     println!("count {count}");
            // }
            let mut current_val = j;
            // println!("seed {j}");
            for i in &mappings {
                current_val = map_val(&i, current_val);
                // println!("newval {current_val}");
            }
            if current_val < lowest {
                lowest = current_val
            }
        }
        lowest
    }).min().unwrap();

    println!("Lowest {lowestest}");

    Ok(())
}

pub fn day5() -> Res<()> {
    
//     let input = "seeds: 79 14 55 13
//
// seed-to-soil map:
// 50 98 2
// 52 50 48
//
// soil-to-fertilizer map:
// 0 15 37
// 37 52 2
// 39 0 15
//
// fertilizer-to-water map:
// 49 53 8
// 0 11 42
// 42 0 7
// 57 7 4
//
// water-to-light map:
// 88 18 7
// 18 25 70
//
// light-to-temperature map:
// 45 77 23
// 81 45 19
// 68 64 13
//
// temperature-to-humidity map:
// 0 69 1
// 1 0 69
//
// humidity-to-location map:
// 60 56 37
// 56 93 4";
    let input = include_str!("input.txt");
    
    // part1(input)?;

    part2(input)?;

    Ok(())
}
