use itertools::Itertools;

use crate::{models::aoc_answer::AocAnswer, utils::get_question_data::get_question_data};

// --- Day 11: Cosmic Expansion ---

// --------------------------------------------------------------------------------------
// Boilerplate
// --------------------------------------------------------------------------------------
pub async fn solve() -> AocAnswer {
    let input_data = get_question_data(2023, 11)
        .await
        .expect("Could not get Day 11 data");

    let answer: AocAnswer = AocAnswer {
        day: 11,
        sample_solution_part1: sample_solution_part1(),
        sample_solution_part2: sample_solution_part2(),
        part1: part1(&input_data),
        part2: part2(&input_data),
    };
    return answer;
}

fn part1(input_data: &String) -> String {
    let image = parse_input(&input_data);

    let empty_rows = get_empty_rows(&image);
    let empty_cols = get_empty_cols(&image);

    get_galaxy_pairs(&image)
        .iter()
        .map(|pair| manhattan_distance(&pair, empty_rows.clone(), empty_cols.clone(), 2))
        .sum::<i64>()
        .to_string()
}

fn part2(input_data: &String) -> String {
    let image = parse_input(&input_data);

    let empty_rows = get_empty_rows(&image);
    let empty_cols = get_empty_cols(&image);

    get_galaxy_pairs(&image)
        .iter()
        .map(|pair| manhattan_distance(&pair, empty_rows.clone(), empty_cols.clone(), 1_000_000))
        .sum::<i64>()
        .to_string()
}

fn sample_solution_part1() -> String {
    let input_data = String::from("...#......\n.......#..\n#.........\n..........\n......#...\n.#........\n.........#\n..........\n.......#..\n#...#.....\n");

    // let image = parse_input(&input_data);

    // let empty_rows = get_empty_rows(&image);
    // let empty_cols = get_empty_cols(&image);

    // println!("empty_rows: {:?}", empty_rows);
    // println!("empty_cols: {:?}", empty_cols);

    // let pairs: Pairs = get_galaxy_pairs(&image);
    // let distances: Vec<i64> = pairs
    //     .iter()
    //     .map(|pair| {
    //         let distance = manhattan_distance(&pair, empty_rows.clone(), empty_cols.clone(), 2);

    //         println!(
    //             "id1: {}:{:?}, id2: {}:{:?}, distance: {}",
    //             pair.0 .0, pair.0 .1, pair.1 .0, pair.1 .1, distance
    //         );

    //         distance
    //     })
    //     .collect();

    // println!("total: {:?}", distances.iter().sum::<i64>());

    part1(&input_data)
}

fn sample_solution_part2() -> String {
    let input_data = String::from("...#......\n.......#..\n#.........\n..........\n......#...\n.#........\n.........#\n..........\n.......#..\n#...#.....\n");

    part2(&input_data)
}

// --------------------------------------------------------------------------------------
// Actual solution
// --------------------------------------------------------------------------------------
type SpaceImage = Vec<Vec<char>>;
type Point = (i32, (i64, i64));
type Pair = (Point, Point);
type Pairs = Vec<Pair>;

fn parse_input(input_data: &String) -> SpaceImage {
    input_data
        .lines()
        .filter(|line| !line.is_empty())
        .map(|line| line.chars().collect())
        .collect()
}

fn get_empty_rows(image: &SpaceImage) -> Vec<i64> {
    let mut empty_rows: Vec<i64> = Vec::new();
    for (x, row) in image.iter().enumerate() {
        if row.iter().all(|&space| space == '.') {
            empty_rows.push(x as i64);
        }
    }

    empty_rows
}

fn get_empty_cols(image: &SpaceImage) -> Vec<i64> {
    let mut empty_cols: Vec<i64> = Vec::new();
    for (y, _) in image[0].iter().enumerate() {
        if image.iter().all(|row| row[y] == '.') {
            empty_cols.push(y as i64);
        }
    }
    empty_cols
}

fn get_galaxy_pairs(image: &SpaceImage) -> Pairs {
    let mut galaxies: Vec<Point> = Vec::new();
    let mut id = 0;
    for (row, line) in image.iter().enumerate() {
        for (col, &space) in line.iter().enumerate() {
            if space == '#' {
                id += 1;

                galaxies.push((id, (row as i64, col as i64)));
            }
        }
    }

    galaxies
        .iter()
        .tuple_combinations::<(_, _)>()
        .map(|(p1, p2)| (*p1, *p2))
        .collect::<Pairs>()
}

fn manhattan_distance(
    pair: &Pair,
    empty_rows: Vec<i64>,
    empty_cols: Vec<i64>,
    expansion_factor: i64,
) -> i64 {
    let (row1, col1) = pair.0 .1.clone();
    let (row2, col2) = pair.1 .1.clone();

    let max_row = row1.max(row2);
    let min_row = row1.min(row2);
    let max_col = col1.max(col2);
    let min_col = col1.min(col2);

    let mut extra_rows = 0;
    for row in empty_rows {
        if row > min_row && row < max_row {
            extra_rows += 1;
        }
    }

    let mut extra_cols = 0;
    for col in empty_cols {
        if col > min_col && col < max_col {
            extra_cols += 1;
        }
    }

    let mut distance = (row1.abs_diff(row2) as i64) + (col1.abs_diff(col2) as i64);
    distance += (extra_rows + extra_cols) * (expansion_factor - 1);
    distance
}

fn _print_image(image: &SpaceImage) {
    for line in image {
        println!("{}", line.iter().collect::<String>());
    }
}
