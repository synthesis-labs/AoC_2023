use crate::{models::aoc_answer::AocAnswer, utils::get_question_data::get_question_data};

// --- Day 3: Gear Ratios ---
// part 1:              81721933
// latest idiot answer: 518219

// --------------------------------------------------------------------------------------
// Boilerplate
// --------------------------------------------------------------------------------------
pub async fn solve() -> AocAnswer<i32> {
    let input_data = get_question_data(2023, 3)
        .await
        .expect("Could not get Day 3 data");

    let result: AocAnswer<i32> = AocAnswer {
        day: 3,
        sample_solution_part1: sample_solution_part1(),
        sample_solution_part2: sample_solution_part2(),
        part1: part1(&input_data),
        part2: part2(&input_data),
    };

    result
}

fn part1(input_data: &String) -> i32 {
    let engine_schematic = parse_engine_schematic(&input_data);

    let engine_parts_sum: i32 = extract_engine_parts(engine_schematic)
        .iter()
        // .map(|x| {
        //     println!("{}", x);
        //     *x
        // })
        .sum();

    engine_parts_sum
}

fn part2(input_data: &String) -> i32 {
    input_data.len() as i32
}

fn sample_solution_part1() -> i32 {
    let input_data = String::from(
    "467..114..\n...*......\n..35..633.\n......#...\n617*......\n.....+.58.\n..592.....\n......755.\n...$.*....\n.664.598..\n");

    part1(&input_data)
}

fn sample_solution_part2() -> i32 {
    let input_data = String::from("Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green\nGame 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue\nGame 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red\nGame 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red\nGame 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green\n");

    part2(&input_data)
}

// --------------------------------------------------------------------------------------
// Actual solution
// --------------------------------------------------------------------------------------

type EngineSchematic = Vec<String>;
type Point = (i32, i32);

fn parse_engine_schematic(input_data: &String) -> EngineSchematic {
    input_data
        .split("\n")
        .filter(|line| !line.is_empty())
        .map(|line| line.to_string())
        .collect()
}

fn extract_engine_parts(engine_schematic: EngineSchematic) -> Vec<i32> {
    let mut result: Vec<i32> = Vec::new();

    for i in 0..engine_schematic.len() {
        let engine_line = engine_schematic[i].clone();

        let numbers = extract_numbers(engine_line.clone());
        for number in numbers {
            let col_val: i32 = number.1;

            let point: Point = (i as i32, col_val as i32);
            let number_length: i32 = number.0.to_string().len() as i32;
            let row_boundary = engine_schematic.len() as i32 - 1;
            let col_boundary = engine_schematic[0].len() as i32 - 1;
            let neighbours = create_neighbours(point, number_length, row_boundary, col_boundary);

            // println!("Neighbours: {:?} for number {:?}", neighbours, number);

            if is_engine_part(engine_schematic.clone(), neighbours) {
                result.push(number.0);
                // println!("Found engine part: {}", number.0);
            }
        }
    }

    result
}

// fn find_index_of_number(line: String, number: i32) -> i32 {}

fn create_neighbours(
    index: Point,
    number_length: i32,
    row_boundary: i32,
    col_boundary: i32,
) -> Vec<Point> {
    let mut result: Vec<Point> = Vec::new();

    let start = index.1 - 1;
    let end = index.1 + number_length + 1;

    for i in start..end {
        if i < 0 || i > col_boundary {
            continue;
        }

        if index.0 - 1 >= 0 {
            result.push((index.0 - 1, i));
        }

        if index.0 + 1 <= row_boundary {
            result.push((index.0 + 1, i));
        }

        if i == index.1 - 1 {
            result.push((index.0, i));
        }

        if i == index.1 + number_length {
            result.push((index.0, i));
        }
    }

    result
}

fn extract_numbers(line: String) -> Vec<(i32, i32)> {
    let char_array: Vec<char> = line.chars().collect();

    let mut numbers: Vec<(i32, i32)> = Vec::new();
    let mut temp_num: String = String::new();
    for i in 0..char_array.len() {
        if char_array[i].is_digit(10) {
            temp_num.push(char_array[i]);
        } else {
            if temp_num == "" {
                continue;
            }

            let index: i32 = i as i32 - temp_num.len() as i32;

            let i_temp_num: i32 = temp_num.parse().unwrap();

            numbers.push((i_temp_num, index));
            temp_num = String::new();
        }
    }
    numbers
}

fn is_engine_part(engine_schematic: EngineSchematic, neighbours: Vec<Point>) -> bool {
    for neighbour in neighbours {
        let neighbour_row = engine_schematic[neighbour.0 as usize].clone();

        let neighbour_char = neighbour_row.chars().nth(neighbour.1 as usize).unwrap();

        if is_valid_symbol(neighbour_char) {
            println!(
                "neighbour row: {:?}, neighbour char: {}",
                neighbour_row, neighbour_char
            );

            return true;
        }
    }
    false
}

fn is_valid_symbol(char: char) -> bool {
    !char.is_digit(10) && char != '.'
}
