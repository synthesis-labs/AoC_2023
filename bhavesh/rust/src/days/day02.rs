use crate::{
    models::{
        aoc_answer::AocAnswer,
        cube_conundrum::{CubeColour, Game, GameId, Round, RoundSet, Rounds},
    },
    utils::get_question_data::get_question_data,
};

// --- Day 2: Cube Conundrum ---

// --------------------------------------------------------------------------------------
// Boilerplate
// --------------------------------------------------------------------------------------
pub async fn solve() -> AocAnswer<i32> {
    let input_data = get_question_data(2023, 2)
        .await
        .expect("Could not get Day 2 data");

    let result: AocAnswer<i32> = AocAnswer {
        day: 2,
        sample_solution_part1: sample_solution_part1(),
        sample_solution_part2: sample_solution_part2(),
        part1: part1(&input_data),
        part2: part2(&input_data),
    };

    result
}

fn part1(input_data: &String) -> i32 {
    input_data
        .split("\n")
        .filter(|x| !x.is_empty())
        .map(|x| x.to_string())
        .map(parse_game)
        .map(is_game_possible)
        .filter(|x| x.1)
        .map(|x| x.0)
        .sum()
}

fn part2(input_data: &String) -> i32 {
    input_data
        .split("\n")
        .filter(|x| !x.is_empty())
        .map(|x| x.to_string())
        .map(parse_game)
        .map(find_max_num_cubes)
        .map(|x| x.1 .0 * x.1 .1 * x.1 .2)
        .sum()
}

fn sample_solution_part1() -> i32 {
    let input_data = String::from("Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green\nGame 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue\nGame 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red\nGame 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red\nGame 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green\n");

    part1(&input_data)
}

fn sample_solution_part2() -> i32 {
    let input_data = String::from("Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green\nGame 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue\nGame 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red\nGame 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red\nGame 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green\n");

    part2(&input_data)
}

// --------------------------------------------------------------------------------------
// Actual solution
// --------------------------------------------------------------------------------------
const RED_CUBES: i32 = 12;
const GREEN_CUBES: i32 = 13;
const BLUE_CUBES: i32 = 14;

fn is_game_possible(game: Game) -> (GameId, bool) {
    let game_result: bool = game
        .game_rounds
        .iter()
        .map(|round| {
            round
                .iter()
                .map(|round_set| match round_set.cube_colour {
                    CubeColour::Red => round_set.num_cubes <= RED_CUBES,
                    CubeColour::Green => round_set.num_cubes <= GREEN_CUBES,
                    CubeColour::Blue => round_set.num_cubes <= BLUE_CUBES,
                })
                .collect::<Vec<bool>>()
        })
        .flatten()
        .all(|x| x);

    (game.game_id, game_result)
}

fn find_max_num_cubes(game: Game) -> (GameId, (i32, i32, i32)) {
    let mut max_red = 0;
    let mut max_green = 0;
    let mut max_blue = 0;

    for round in game.game_rounds {
        for round_set in round {
            if round_set.cube_colour == CubeColour::Red {
                max_red = max_red.max(round_set.num_cubes);
            } else if round_set.cube_colour == CubeColour::Green {
                max_green = max_green.max(round_set.num_cubes);
            } else if round_set.cube_colour == CubeColour::Blue {
                max_blue = max_blue.max(round_set.num_cubes);
            }
        }
    }

    (game.game_id, (max_red, max_green, max_blue))
}

fn parse_game(game: String) -> Game {
    let split: Vec<String> = game.split(": ").map(|x| x.to_string()).collect();

    let game_id: GameId = parse_game_id(split[0].clone());
    let game_rounds: Rounds = parse_game_rounds(split[1].clone());

    Game {
        game_id,
        game_rounds,
    }
}

fn parse_game_id(game_id_str: String) -> i32 {
    game_id_str.replace("Game ", "").parse().unwrap()
}

fn parse_game_rounds(rounds: String) -> Rounds {
    rounds
        .split("; ")
        .map(|x| x.to_string())
        .map(parse_game_round)
        .collect()
}

fn parse_game_round(round: String) -> Round {
    round
        .split(", ")
        .map(|x| x.to_string())
        .map(parse_game_round_set)
        .collect()
}

fn parse_game_round_set(round_set: String) -> RoundSet {
    let split = round_set.split(" ").collect::<Vec<&str>>();

    let num_cubes: i32 = split[0].parse().unwrap();
    let cube_colour: CubeColour = parse_colour(split[1].to_string());

    RoundSet {
        cube_colour,
        num_cubes,
    }
}

fn parse_colour(colour: String) -> CubeColour {
    match colour.as_str() {
        "red" => CubeColour::Red,
        "green" => CubeColour::Green,
        "blue" => CubeColour::Blue,
        _ => panic!("Unknown colour: {}", colour),
    }
}
