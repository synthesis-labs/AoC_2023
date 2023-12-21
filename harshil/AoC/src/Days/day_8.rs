use std::collections::HashMap;
use std::fs::File;
use std::io::Read;
use crate::Days::Node;

pub fn day_8_pt1() {
    let file = File::open("C:/Users/harsh/RustroverProjects/AoC_2023/harshil/AoC/src/Data/Day_8.txt");
    let mut contents = String::new();
    file.unwrap().read_to_string(&mut contents).expect("Error reading file");

    let mut vcontent: Vec<&str>=contents.lines().collect();
    let mut directions:Vec<&str>=vcontent[0].split("").collect();
    let mut graph =HashMap::new();
    for line in vcontent[2..].iter() {
        let node_name: Vec<&str> = line.split('=').map(|s| s.trim()).collect();
        if node_name.len() == 2 {
            let snode_name = node_name[0].trim();
            let connections: Vec<&str> = node_name[1]
                .trim_matches(|c| c == '(' || c == ')')
                .split(',')
                .map(|s| s.trim())
                .collect();
            graph.insert(snode_name, connections);
        }
    }
    directions.remove(0);
    directions.remove(directions.len()-1);
    println!("{:?}", graph);
    println!("{:?}", directions);
    let mut current_name="AAA";
    let mut current=graph.get(current_name).unwrap();
    let mut i =0;
    let mut steps=0;
    while !current_name.eq("ZZZ"){
        println!("{}", current_name);
        println!("{}", directions[i]);
        if directions[i].eq("L") {
            current_name=current[0];
            current=graph.get(current_name).unwrap();
        }else {
            current_name=current[1];
            current=graph.get(current_name).unwrap();
        }
        i+=1;
        steps+=1;
        if i==directions.len() {
            i=0;
        }
    }
    println!("{}", steps);
}

