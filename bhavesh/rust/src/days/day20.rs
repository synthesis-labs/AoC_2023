use std::collections::{HashMap, VecDeque};

use crate::{
    models::{
        aoc_answer::AocAnswer,
        pulse_propagation::{Module, ModuleType, Modules, Pulse, Pulses},
    },
    utils::get_question_data::get_question_data,
};

// --- Day 20: Pulse Propagation ---

// --------------------------------------------------------------------------------------
// Boilerplate
// --------------------------------------------------------------------------------------
pub async fn solve() -> AocAnswer {
    let input_data = get_question_data(2023, 20)
        .await
        .expect("Could not get Day 20 data");

    let answer: AocAnswer = AocAnswer {
        day: 20,
        sample_solution_part1: sample_solution_part1(),
        sample_solution_part2: sample_solution_part2(),
        part1: part1(&input_data),
        part2: part2(&input_data),
    };
    return answer;
}

fn part1(input_data: &String) -> String {
    let modules = parse_modules(input_data);
    let input_pulses = create_input_pulses();
    let r = count_pulses(&input_pulses, &modules);

    (r.0 * r.1).to_string()
}

fn part2(input_data: &String) -> String {
    input_data.len().to_string()
}

fn sample_solution_part1() -> String {
    let input_data_1 =
        String::from("broadcaster -> a, b, c\n%a -> b\n%b -> c\n%c -> inv\n&inv -> a\n");
    let input_data_2 =
        String::from("broadcaster -> a\n%a -> inv, con\n&inv -> b\n%b -> con\n&con -> output\n");

    format!(
        "sample_1: {} sample_2: {}",
        part1(&input_data_1),
        part1(&input_data_2)
    )
}

fn sample_solution_part2() -> String {
    let input_data =
        String::from("broadcaster -> a, b, c\n%a -> b\n%b -> c\n%c -> inv\n&inv -> a\n");

    part2(&input_data)
}

// --------------------------------------------------------------------------------------
// Actual solution
// --------------------------------------------------------------------------------------
fn count_pulses(pulses: &Pulses, modules: &Modules) -> (i32, i32) {
    let mut _count_high = 0;
    let mut _count_low = 0;

    let mut mutable_modules = modules.clone();
    for pulse in pulses {
        let mut current = ("broadcaster".to_string(), pulse.clone());
        let mut previous = current.0.clone();

        let mut queue: VecDeque<((String, Pulse), String)> = VecDeque::new();
        queue.push_back((current.clone(), previous.clone()));

        while !queue.is_empty() {
            let element = queue.pop_front().unwrap();
            current = element.0;
            previous = element.1;
            let key = current.0.clone();
            let current_pulse = current.1.clone();

            if current_pulse == Pulse::High {
                _count_high += 1;
            } else if current_pulse == Pulse::Low {
                _count_low += 1;
            }

            if mutable_modules.get(&key).is_none() {
                continue;
            }
            let current_module = mutable_modules.get_mut(&key).unwrap().clone();

            if key == "broadcaster" {
                queue.extend(
                    current_module
                        .destinations
                        .clone()
                        .iter()
                        .map(|d| ((d.clone(), current_pulse.clone()), key.clone())),
                );
                continue;
            }

            let (new_module, new_pulse) =
                get_output_for_module(&current_module, &current_pulse, &previous);

            if new_module.module_type != ModuleType::Terminal {
                *mutable_modules.get_mut(&key).unwrap() = new_module.clone();
            }

            if new_pulse != Pulse::None {
                queue.extend(
                    current_module
                        .destinations
                        .clone()
                        .iter()
                        .map(|d| ((d.clone(), new_pulse.clone()), key.clone())),
                );
            }

            // println!("{} -{}-> {:?}", key, new_pulse, new_module.destinations);
        }
        // println!("")
    }
    (_count_low, _count_high)
}

fn get_output_for_module(module: &Module, pulse: &Pulse, emitter: &String) -> (Module, Pulse) {
    let mut output_module: Module;
    let output_pulse: Pulse;

    match &module.module_type {
        ModuleType::FlipFlop(on) => {
            let o = get_output_for_flip_flop(pulse, *on);
            output_module = module.clone();
            output_module.module_type = o.0;
            output_pulse = o.1;
        }
        ModuleType::Conjunction(previous_pulses) => {
            let o = get_output_for_conjunction(&pulse, &previous_pulses, &emitter);
            output_module = module.clone();
            output_module.module_type = o.0;
            output_pulse = o.1;
        }
        _ => {
            output_module = Module {
                module_type: ModuleType::Terminal,
                destinations: module.destinations.clone(),
            };
            output_pulse = Pulse::None;
        }
    }

    (output_module, output_pulse)
}

fn get_output_for_flip_flop(pulse: &Pulse, on: bool) -> (ModuleType, Pulse) {
    // Flip-flop modules (prefix %) are either on or off; they are initially off.
    // If a flip-flop module receives a high pulse, it is ignored and nothing happens.
    // However, if a flip-flop module receives a low pulse,
    //    it flips between on and off. If it was off, it turns on and sends a high pulse.
    //    If it was on, it turns off and sends a low pulse.
    if pulse == &Pulse::Low {
        if on {
            (ModuleType::FlipFlop(false), Pulse::Low)
        } else {
            (ModuleType::FlipFlop(true), Pulse::High)
        }
    } else {
        (ModuleType::Terminal, Pulse::None)
    }
}

fn get_output_for_conjunction(
    pulse: &Pulse,
    previous_pulses: &HashMap<String, Pulse>,
    emitter: &String,
) -> (ModuleType, Pulse) {
    // Conjunction modules (prefix &) remember the type of the most recent pulse received from each of their connected input modules;
    // they initially default to remembering a low pulse for each input.
    // When a pulse is received, the conjunction module first updates its memory for that input.
    // Then, if it remembers high pulses for all inputs,
    //    it sends a low pulse;
    //    otherwise, it sends a high pulse.

    let mut updated_pulses = previous_pulses.clone();
    if previous_pulses.contains_key(emitter) {
        *updated_pulses.get_mut(emitter).unwrap() = pulse.clone();
    } else {
        updated_pulses.insert(emitter.clone(), pulse.clone());
    }

    let mut all_high = true;
    for (_, v) in updated_pulses.iter() {
        if v == &Pulse::Low {
            all_high = false;
        }
    }

    if all_high {
        (ModuleType::Conjunction(updated_pulses), Pulse::Low)
    } else {
        (ModuleType::Conjunction(updated_pulses), Pulse::High)
    }
}

fn create_input_pulses() -> Pulses {
    let mut input_pulses = Vec::new();

    for _ in 0..1000 {
        input_pulses.push(Pulse::Low);
    }

    input_pulses
}

fn parse_modules(input_data: &String) -> Modules {
    let mut modules = Modules::new();
    for line in input_data.lines() {
        let parts = line.split(" -> ").collect::<Vec<&str>>();

        let module_name = parse_module_name(parts[0]);
        let module_value = Module {
            module_type: parse_module_type(parts[0]),
            destinations: parse_module_destinations(parts[1]),
        };
        modules.insert(module_name, module_value);
    }

    let conjunction_modules = modules
        .iter()
        .filter(|m| m.1.module_type == ModuleType::Conjunction(HashMap::new()))
        .map(|m| m.0.clone())
        .collect::<Vec<String>>();

    for m in modules.clone() {
        let destinations = m.1.destinations.clone();
        for d in destinations {
            if conjunction_modules.contains(&d) {
                modules
                    .get_mut(&d)
                    .unwrap()
                    .module_type
                    .as_conjunction()
                    .unwrap()
                    .insert(m.0.clone(), Pulse::Low);
            }
        }
    }

    modules
}

fn parse_module_type(str: &str) -> ModuleType {
    match str {
        "broadcaster" => ModuleType::Broadcaster,
        _ if str.starts_with("%") => ModuleType::FlipFlop(false),
        _ if str.starts_with("&") => ModuleType::Conjunction(HashMap::new()),
        _ => ModuleType::Terminal,
    }
}

fn parse_module_name(str: &str) -> String {
    if str.starts_with("%") || str.starts_with("&") {
        str[1..].to_string()
    } else {
        str.to_string()
    }
}

fn parse_module_destinations(str: &str) -> Vec<String> {
    str.split(", ").map(|s| s.to_string()).collect()
}
