use std::collections::HashMap;
use std::fs::File;
use std::io::Read;

pub fn day_3_pt1() {
    let file = File::open("C:/Users/harsh/RustroverProjects/AoC_2023/harshil/AoC/src/Data/Day_3.txt");
    let mut contents = String::new();
    file.unwrap().read_to_string(&mut contents).expect("Error reading file");

    let engine_schematic = parse_engine_schematic(&contents);

    let engine_parts_sum: i32 = extract_engine_parts(engine_schematic).iter().sum();

    print!("{}", engine_parts_sum);
}

pub fn day_3_pt2()  {
    let file = File::open("C:/Users/harsh/RustroverProjects/AoC_2023/harshil/AoC/src/Data/Day_3.txt");
    let mut contents = String::new();
    file.unwrap().read_to_string(&mut contents).expect("Error reading file");
    let engine_schematic = parse_engine_schematic(&contents);

    let gear_ratio = extract_gear_ratio(engine_schematic);

    //println!("{:?}", gear_ratio.iter().map(|x| x.1[0] * x.1[1]).sum())
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


fn extract_gear_ratio(engine_schematic: EngineSchematic) -> HashMap<Point, Vec<i32>>{

    let mut star_hash_map: HashMap<Point, Vec<i32>> = HashMap::new();

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

            for neighbour in neighbours {
                let neighbour_row = engine_schematic[neighbour.0 as usize].clone();

                let neighbour_char = neighbour_row.chars().nth(neighbour.1 as usize).unwrap();
                if neighbour_char == '*' {
                    if star_hash_map.contains_key(&neighbour) {
                        star_hash_map.get_mut(&neighbour).unwrap().push(number.0);
                    } else {
                        star_hash_map.insert(neighbour, vec![number.0]);
                    }
                }
            }

        }
    }

    let a: HashMap<Point, Vec<i32>> = star_hash_map
        .iter()
        .filter(|x| x.1.len() == 2)
        .map(|x| (*x.0, x.1.clone()))
        .collect();
    // .map(|x| x.1)
    // .collect();
    println!("{:?}", a);

    a
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

            if is_engine_part(engine_schematic.clone(), neighbours) {
                result.push(number.0);
            }
        }
    }

    result
}

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

    // if !temp_num.is_empty() {
    //     let i_temp_num: i32 = temp_num.parse().unwrap();
    //     numbers.push((i_temp_num, char_array.len() as i32 - temp_num.len() as i32));
    // }

    numbers
}

fn is_engine_part(engine_schematic: EngineSchematic, neighbours: Vec<Point>) -> bool {
    for neighbour in neighbours {
        let neighbour_row = engine_schematic[neighbour.0 as usize].clone();
        println!("{:?}", neighbour_row.chars().nth(neighbour.1 as usize));
        let neighbour_char = neighbour_row.chars().nth(neighbour.1 as usize).unwrap();

        if is_valid_symbol(neighbour_char) {
            return true;
        }
    }
    false
}

fn is_valid_symbol(char: char) -> bool {
    !char.is_digit(10) && char != '.'
}
