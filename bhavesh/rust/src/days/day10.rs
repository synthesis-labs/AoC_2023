use apply::Apply;

use crate::{models::aoc_answer::AocAnswer, utils::get_question_data::get_question_data};

// --- Day 10: Pipe Maze ---

// --------------------------------------------------------------------------------------
// Boilerplate
// --------------------------------------------------------------------------------------
pub async fn solve() -> AocAnswer {
    let input_data = get_question_data(2023, 10)
        .await
        .expect("Could not get Day 10 data");

    let answer: AocAnswer = AocAnswer {
        day: 10,
        sample_solution_part1: sample_solution_part1(),
        sample_solution_part2: sample_solution_part2(),
        part1: part1(&input_data),
        part2: part2(&input_data),
    };
    return answer;
}

fn part1(input_data: &String) -> String {
    let (start, grid) = input_data.apply(parse_grid);

    let pipe_loop = find_loop(start, &grid);

    (pipe_loop.len() / 2).to_string()
}

fn part2(input_data: &String) -> String {
    let (start, grid) = (input_data).apply(parse_grid);
    let pipe_loop = find_loop(start, &grid);

    let actual_path: Path = pipe_loop.clone()[0..pipe_loop.len() - 1].to_vec();

    let direction_grid = get_direction_grid(&grid, &actual_path);

    count_enclosed(&direction_grid).to_string()
}

fn sample_solution_part1() -> String {
    let input_data_1 = String::from("..F7.\n.FJ|.\nSJ.L7\n|F--J\nLJ...\n");
    let input_data_2 = String::from(".....\n.S-7.\n.|.|.\n.L-J.\n.....\n");

    format!(
        "sample_1: {} sample_2: {}",
        part1(&input_data_1),
        part1(&input_data_2)
    )
}

fn sample_solution_part2() -> String {
    let input_data = String::from("..........\n.S------7.\n.|F----7|.\n.||....||.\n.||....||.\n.|L-7F-J|.\n.|..||..|.\n.L--JL--J.\n..........\n");

    part2(&input_data)
}
// --------------------------------------------------------------------------------------
// Actual solution
// --------------------------------------------------------------------------------------
type Grid = Vec<Vec<char>>;
type Point = (usize, usize);
type Path = Vec<Point>;

fn step(current: Point, previous: Point, grid: &Grid) -> Point {
    let pipes_that_go_north = vec!['|', '7', 'F', 'S'];
    let pipes_that_go_south = vec!['|', 'L', 'J', 'S'];
    let pipes_that_go_east = vec!['-', '7', 'J', 'S'];
    let pipes_that_go_west = vec!['-', 'L', 'F', 'S'];

    let current_pipe = grid[current.0][current.1];

    // # can I go north?
    if pipes_that_go_south.contains(&current_pipe) && current.0 > 0 {
        let north = (current.0 - 1, current.1);
        let north_pipe = grid[north.0][north.1];
        if pipes_that_go_north.contains(&north_pipe) && north != previous {
            return north;
        }
    }

    // # can I go south?
    if pipes_that_go_north.contains(&current_pipe) && current.0 < grid.len() - 1 {
        let south = (current.0 + 1, current.1);
        let south_pipe = grid[south.0][south.1];
        if pipes_that_go_south.contains(&south_pipe) && south != previous {
            return south;
        }
    }

    // # can I go east?
    if pipes_that_go_west.contains(&current_pipe) && current.1 < grid[0].len() - 1 {
        let east = (current.0, current.1 + 1);
        let east_pipe = grid[east.0][east.1];
        if pipes_that_go_east.contains(&east_pipe) && east != previous {
            return east;
        }
    }

    // # can I go west?
    if pipes_that_go_east.contains(&current_pipe) && current.1 > 0 {
        let west = (current.0, current.1 - 1);
        let west_pipe = grid[west.0][west.1];
        if pipes_that_go_west.contains(&west_pipe) && west != previous {
            return west;
        }
    }
    panic!("Can't go any further");
}

fn get_direction_grid(grid: &Grid, path: &Path) -> Grid {
    let mut result: Grid = Vec::new();

    let mut deltas: Vec<(i32, i32)> = Vec::new();
    for i in 0..path.len() {
        let current = path[i];
        let mut _delta = (0, 0);
        if i == 0 {
            let previous = path[path.len() - 1];
            _delta = (
                current.0 as i32 - previous.0 as i32,
                current.1 as i32 - previous.1 as i32,
            );
        } else {
            let previous = path[i - 1];
            _delta = (
                current.0 as i32 - previous.0 as i32,
                current.1 as i32 - previous.1 as i32,
            );
        }
        deltas.push(_delta);
    }

    for i in 0..deltas.len() {
        if deltas[i].0 == 1 || deltas[i].0 == -1 {
            continue;
        }

        let mut k = 0;
        while deltas[i].0 == 0 {
            k += 1;
            if i + k == deltas.len() {
                deltas[i] = deltas[0];
            } else {
                deltas[i] = deltas[i + k];
            }
        }
    }

    for (row, line) in grid.iter().enumerate() {
        let mut new_line = Vec::new();
        for (col, c) in line.iter().enumerate() {
            if !path.contains(&(row, col)) {
                new_line.push(' ');
            } else {
                new_line.push(*c);
            }
        }

        result.push(new_line);
    }

    for (i, p) in path.iter().enumerate() {
        let delta = deltas[i];

        let mut c = ' ';
        if delta == (1, 0) {
            c = 'V';
        } else if delta == (-1, 0) {
            c = 'Λ';
        }
        result[p.0][p.1] = c;
    }

    result
}

fn count_enclosed(direction_grid: &Grid) -> u32 {

    let mut count: u32 = 0;

    for line in direction_grid {
        let mut inside = false;
        let mut direction = ' ';
        for point in line {
            if *point == ' ' && inside {
                count += 1;
            } else {
                if (*point == 'V' || *point == 'Λ') && *point != direction {
                    inside = !inside;
                    direction = *point;
                }
            }
        }
    }
    count
}

fn find_loop(start: Point, grid: &Grid) -> Vec<Point> {
    let mut current = start;
    let mut previous = start;
    let mut next = step(current, previous, grid);
    let mut result: Vec<Point> = vec![current, next];

    while grid[next.0][next.1] != 'S' {
        previous = current;
        current = next;
        next = step(current, previous, grid);
        result.push(next);
    }

    result
}

fn parse_grid(raw_data: &String) -> (Point, Grid) {
    let mut starting_point: Point = (0, 0);

    let mut grid: Grid = Vec::new();

    for (row, line) in raw_data.lines().enumerate() {
        for (col, c) in line.chars().enumerate() {
            if c == 'S' {
                starting_point = (row, col);
            }
        }
        grid.push(line.chars().collect());
    }

    (starting_point, grid)
}

fn _print_grid(grid: &Grid) {
    for line in grid {
        for c in line {
            print!("{}", c);
        }
        println!();
    }
}
