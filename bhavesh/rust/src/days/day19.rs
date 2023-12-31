use crate::{
    models::{
        aoc_answer::AocAnswer,
        aplenty::{Key, PartRating, Parts, Rule, WorkFlow, WorkFlows},
    },
    utils::get_question_data::get_question_data,
};

// --- Day 19: Aplenty ---

// --------------------------------------------------------------------------------------
// Boilerplate
// --------------------------------------------------------------------------------------
pub async fn solve() -> AocAnswer {
    let input_data = get_question_data(2023, 19)
        .await
        .expect("Could not get Day 19 data");

    let answer: AocAnswer = AocAnswer {
        day: 19,
        sample_solution_part1: sample_solution_part1(),
        sample_solution_part2: sample_solution_part2(),
        part1: part1(&input_data),
        part2: part2(&input_data),
    };
    return answer;
}

fn part1(input_data: &String) -> String {
    let split_str = input_data
        .split("\n\n")
        .filter(|x| !x.is_empty())
        .map(|x| x.to_string())
        .collect::<Vec<String>>();

    let workflows = parse_workflows(&split_str[0]);
    let parts = parse_parts(&split_str[1]);

    sum_valid_parts(&parts, &workflows).to_string()
}

fn part2(input_data: &String) -> String {
    input_data.len().to_string()
}

fn sample_solution_part1() -> String {
    let input_data = String::from("px{a<2006:qkq,m>2090:A,rfg}\npv{a>1716:R,A}\nlnx{m>1548:A,A}\nrfg{s<537:gd,x>2440:R,A}\nqs{s>3448:A,lnx}\nqkq{x<1416:A,crn}\ncrn{x>2662:A,R}\nin{s<1351:px,qqz}\nqqz{s>2770:qs,m<1801:hdj,R}\ngd{a>3333:R,R}\nhdj{m>838:A,pv}\n\n{x=787,m=2655,a=1222,s=2876}\n{x=1679,m=44,a=2067,s=496}\n{x=2036,m=264,a=79,s=2244}\n{x=2461,m=1339,a=466,s=291}\n{x=2127,m=1623,a=2188,s=1013}\n");

    part1(&input_data)
}

fn sample_solution_part2() -> String {
    let input_data = String::from("px{a<2006:qkq,m>2090:A,rfg}\npv{a>1716:R,A}\nlnx{m>1548:A,A}\nrfg{s<537:gd,x>2440:R,A}\nqs{s>3448:A,lnx}\nqkq{x<1416:A,crn}\ncrn{x>2662:A,R}\nin{s<1351:px,qqz}\nqqz{s>2770:qs,m<1801:hdj,R}\ngd{a>3333:R,R}\nhdj{m>838:A,pv}\n\n{x=787,m=2655,a=1222,s=2876}\n{x=1679,m=44,a=2067,s=496}\n{x=2036,m=264,a=79,s=2244}\n{x=2461,m=1339,a=466,s=291}\n{x=2127,m=1623,a=2188,s=1013}\n");

    let split_str = input_data
        .split("\n\n")
        .filter(|x| !x.is_empty())
        .map(|x| x.to_string())
        .collect::<Vec<String>>();

    let workflows = parse_workflows(&split_str[0]);

    let paths: Vec<Vec<String>> = traverse_workflows(&workflows, &String::from("in"));

    println!("{:?}", paths);

    part2(&input_data)
}

// --------------------------------------------------------------------------------------
// Actual solution
// --------------------------------------------------------------------------------------
fn traverse_workflows(workflows: &WorkFlows, key: &Key) -> Vec<Vec<String>> {
    if key == "R" || key == "A" {
        return vec![vec![key.clone()]];
    }

    let mut new_paths: Vec<Vec<String>> = Vec::new();

    for rule in workflows.get(key).unwrap() {
        println!("rule {:?}", rule);

        let next_paths = traverse_workflows(workflows, &rule.result);
        for next_path in next_paths {
            let mut sub_path: Vec<String> = vec![key.clone()];
            sub_path.extend(next_path);
            new_paths.push(sub_path.clone());
        }
    }
    new_paths
        .iter()
        .filter(|x| x.to_vec().contains(&"A".to_string()))
        .map(|x| x.to_vec())
        .collect::<Vec<Vec<String>>>()
}

fn sum_valid_parts(parts: &Parts, workflows: &WorkFlows) -> i64 {
    parts
        .iter()
        .map(|x| (x, process_part(x, &workflows)))
        .filter(|x| x.1 == "A")
        .map(|x| x.0)
        .map(|x| x.x + x.m + x.a + x.s)
        .sum::<i64>()
}

fn process_part(part: &PartRating, workflows: &WorkFlows) -> Key {
    let mut key = "in".to_string();
    loop {
        if key == "R" || key == "A" {
            break;
        }
        // println!(
        //     "  going from key {} to key {}",
        //     key,
        //     process_part_through_workflow(part, workflows.get(&key).unwrap())
        // );
        key = process_part_through_workflow(part, workflows.get(&key).unwrap());
    }
    key
}

fn process_part_through_workflow(part: &PartRating, workflow: &WorkFlow) -> Key {
    for rule in workflow {
        if rule.condition.is_empty() {
            return rule.result.clone();
        }

        if rule.condition.contains("<") {
            let condition_parts = rule.condition.split("<").collect::<Vec<&str>>();
            let var = condition_parts[0];
            let val = condition_parts[1].parse::<i64>().unwrap();

            if var == "x" {
                if part.x < val {
                    return rule.result.clone();
                }
                continue;
            } else if var == "m" {
                if part.m < val {
                    return rule.result.clone();
                }
                continue;
            } else if var == "a" {
                if part.a < val {
                    return rule.result.clone();
                }
                continue;
            } else if var == "s" {
                if part.s < val {
                    return rule.result.clone();
                }
                continue;
            }
        } else if rule.condition.contains(">") {
            let condition_parts = rule.condition.split(">").collect::<Vec<&str>>();
            let var = condition_parts[0];
            let val = condition_parts[1].parse::<i64>().unwrap();

            if var == "x" {
                if part.x > val {
                    return rule.result.clone();
                }
                continue;
            } else if var == "m" {
                if part.m > val {
                    return rule.result.clone();
                }
                continue;
            } else if var == "a" {
                if part.a > val {
                    return rule.result.clone();
                }
                continue;
            } else if var == "s" {
                if part.s > val {
                    return rule.result.clone();
                }
                continue;
            }
        }
    }

    panic!("rule not found");
}

fn parse_workflows(workflows: &str) -> WorkFlows {
    let mut flows = WorkFlows::new();

    for line in workflows.lines() {
        let open_curly_pos = line.find('{').unwrap();
        let close_curly_pos = line.find('}').unwrap();

        let key: Key = line[0..open_curly_pos].to_string();
        let rules_str = &line[open_curly_pos + 1..close_curly_pos];

        let mut rules: WorkFlow = WorkFlow::new();

        for rule in rules_str.split(",") {
            let colon_pos = rule.find(':');

            if colon_pos.is_none() {
                rules.push(Rule {
                    condition: "".to_string(),
                    result: rule.to_string(),
                });
            } else {
                let colon_pos = colon_pos.unwrap();
                let condition = rule[0..colon_pos].to_string();
                let result = rule[colon_pos + 1..].to_string();
                rules.push(Rule { condition, result });
            }
        }

        flows.insert(key, rules);
    }

    return flows;
}

fn parse_parts(parts_str: &str) -> Parts {
    let mut parts: Parts = Parts::new();

    for line in parts_str.lines() {
        let open_curly_pos = line.find('{').unwrap();
        let close_curly_pos = line.find('}').unwrap();

        let parts_data = &line[open_curly_pos + 1..close_curly_pos]
            .split(",")
            .map(|x| x.split("=").collect::<Vec<&str>>())
            .collect::<Vec<Vec<&str>>>();

        let part_rating: PartRating = PartRating {
            x: parts_data[0][1].parse::<i64>().unwrap(),
            m: parts_data[1][1].parse::<i64>().unwrap(),
            a: parts_data[2][1].parse::<i64>().unwrap(),
            s: parts_data[3][1].parse::<i64>().unwrap(),
        };

        parts.push(part_rating);
    }

    parts
}
