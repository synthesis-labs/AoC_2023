use std::collections::VecDeque;

use apply::Apply;
use rayon::iter::{IntoParallelRefIterator, ParallelIterator, IntoParallelIterator};

use crate::{
    models::{
        aoc_answer::AocAnswer,
        seeds::{Almanac, AlmanacItems, AlmanacMap, AlmanacMaps, Location, Seed, Seeds},
    },
    utils::get_question_data::get_question_data,
};

// --- Day 5: If You Give A Seed A Fertilizer ---

// --------------------------------------------------------------------------------------
// Boilerplate
// --------------------------------------------------------------------------------------
pub async fn solve() -> AocAnswer {
    let input_data = get_question_data(2023, 5)
        .await
        .expect("Could not get Day 5 data");

    let result: AocAnswer = AocAnswer {
        day: 5,
        sample_solution_part1: sample_solution_part1(),
        sample_solution_part2: sample_solution_part2(),
        part1: part1(&input_data),
        part2: part2(&input_data),
    };

    result
}

fn part1(input_data: &String) -> String {
    let almanac: Almanac = parse_data(input_data.clone());

    let min_location: i64 = almanac
        .0
        .par_iter()
        .map(|seed| find_location_for_seed(*seed, almanac.1.clone()))
        .min()
        .unwrap();

    min_location.to_string()
}

fn part2(input_data: &String) -> String {
    let almanac: Almanac = parse_data(input_data.clone());
    let min_location = almanac
        .0
        .par_iter()
        .map(|x| *x)
        .collect::<Vec<i64>>()
        .chunks(2)
        .map(|x| (x[0], x[1]))
        .map(|(seed_start, range)| {
            (seed_start..seed_start + range)
                .into_par_iter()
                .map(|seed| find_location_for_seed(seed, almanac.1.clone()))
                .min()
        })
        .min()
        .unwrap()
        .unwrap();

    min_location.to_string()
}

fn sample_solution_part1() -> String {
    let input_data = String::from(
    "seeds: 79 14 55 13\n\nseed-to-soil map:\n50 98 2\n52 50 48\n\nsoil-to-fertilizer map:\n0 15 37\n37 52 2\n39 0 15\n\nfertilizer-to-water map:\n49 53 8\n0 11 42\n42 0 7\n57 7 4\n\nwater-to-light map:\n88 18 7\n18 25 70\n\nlight-to-temperature map:\n45 77 23\n81 45 19\n68 64 13\n\ntemperature-to-humidity map:\n0 69 1\n1 0 69\n\nhumidity-to-location map:\n60 56 37\n56 93 4\n");

    part1(&input_data)
}

fn sample_solution_part2() -> String {
    let input_data = String::from(
        "seeds: 79 14 55 13\n\nseed-to-soil map:\n50 98 2\n52 50 48\n\nsoil-to-fertilizer map:\n0 15 37\n37 52 2\n39 0 15\n\nfertilizer-to-water map:\n49 53 8\n0 11 42\n42 0 7\n57 7 4\n\nwater-to-light map:\n88 18 7\n18 25 70\n\nlight-to-temperature map:\n45 77 23\n81 45 19\n68 64 13\n\ntemperature-to-humidity map:\n0 69 1\n1 0 69\n\nhumidity-to-location map:\n60 56 37\n56 93 4\n");

    part2(&input_data)
}

// --------------------------------------------------------------------------------------
// Actual solution
// --------------------------------------------------------------------------------------

fn find_location_for_seed(seed: Seed, almanac_items: AlmanacItems) -> Location {
    let mut value: i64 = seed.clone();

    let mut j = 0;
    while j < almanac_items.len() {
        let almanac_maps = &almanac_items[j];
        value = lookup_destination(value, &almanac_maps);
        j += 1;
    }

    value
}

fn lookup_destination(value: i64, almanac_maps: &AlmanacMaps) -> i64 {
    for almanac_map in almanac_maps {
        let source_range_end = almanac_map.source_start + almanac_map.length;

        if value >= almanac_map.source_start && value < source_range_end {
            let offset_value = value - almanac_map.source_start;
            let new_destination = almanac_map.destination_start + offset_value;

            return new_destination;
        }
    }

    value
}

fn parse_data(input_data: String) -> Almanac {
    let mut data: VecDeque<String> = input_data
        .split("\n\n")
        .filter(|x| !x.is_empty())
        .map(|x| x.to_string())
        .collect();

    let seeds_line = data.pop_front().unwrap();

    let seeds: Seeds = parse_seeds(seeds_line);
    let almanac_items: AlmanacItems = parse_almanac_items(data);

    (seeds, almanac_items)
}

fn parse_almanac_items(data: VecDeque<String>) -> AlmanacItems {
    let mut almanac_items: AlmanacItems = VecDeque::new();

    data.into_iter().for_each(|map| {
        let mut data_map: VecDeque<String> = map
            .split("\n")
            .filter(|x| !x.is_empty())
            .map(|x| x.to_string())
            .collect();

        let _ = data_map.pop_front().unwrap();
        almanac_items.push_back(parse_almanac_maps(data_map));
    });

    almanac_items
}

fn parse_seeds(line: String) -> Seeds {
    line.replace("seeds: ", "")
        .split(" ")
        .filter(|x| !x.is_empty())
        .map(|x| x.parse().unwrap())
        .collect()
}

fn parse_almanac_maps(maps_vec: VecDeque<String>) -> AlmanacMaps {
    maps_vec
        .iter()
        .map(|x| x.to_string())
        .map(parse_almanac_map)
        .collect()
}

fn parse_almanac_map(row: String) -> AlmanacMap {
    row.split(" ")
        .map(|x| x.to_string().parse::<i64>().unwrap())
        .collect::<Vec<i64>>()
        .apply(|x| AlmanacMap {
            destination_start: x[0],
            source_start: x[1],
            length: x[2],
        })
}
