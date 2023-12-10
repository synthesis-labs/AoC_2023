use std::fs::File;
use std::io::{BufRead, Read};

pub(crate) fn day_6_pt1(){
    let file = File::open("C:/Users/harsh/RustroverProjects/AoC_2023/harshil/AoC/src/Data/Day_6.txt");
    let mut contents = String::new();
    file.unwrap().read_to_string(&mut contents).expect("Error reading file");
    let mut vcontent: Vec<&str>=contents.lines().collect();
    let mut time_a_values: Vec<&str> = Vec::new();
    let mut dist_a_values: Vec<&str> = Vec::new();
    let mut timeA = Vec::new();
    let mut distA = Vec::new();
    for line in vcontent  {
        let values: Vec<&str> = line.split_whitespace().collect();
        if let Some (first)= values.get(0) {
            match *first {
                "Time:" =>timeA=values[1..].to_vec(),
                "Distance:"=>distA=values[1..].to_vec(),
                _ => {}
            }
        }
    }

    let mut waystowin=0;
    let mut mul=1;
    for i in 0..timeA.len() {
        waystowin=0;
        for j in 0..timeA[i].parse().unwrap() {
            let mut chargedDistance=j*(timeA[i].parse::<i32>().unwrap() -j);
            if chargedDistance> distA[i].parse().unwrap() {
                waystowin+=1;
            }
            println!("cd: {}, ways:{}", chargedDistance,waystowin);
        }
        mul*=waystowin;
    }

    println!("{}", mul);
}

pub(crate) fn day_6_pt2(){
    let file = File::open("C:/Users/harsh/RustroverProjects/AoC_2023/harshil/AoC/src/Data/Day_6.txt");
    let mut contents = String::new();
    file.unwrap().read_to_string(&mut contents).expect("Error reading file");
    let mut vcontent: Vec<&str>=contents.lines().collect();
    let mut time_a_values: Vec<&str> = Vec::new();
    let mut dist_a_values: Vec<&str> = Vec::new();
    let mut timeA:Vec<i64> = vec![0;2];
    let mut distA:Vec<i64> = vec![0;2];
    for line in vcontent  {
        let values: Vec<&str> = line.split_whitespace().collect();
        if let Some (first)= values.get(0) {
            match *first {
                "Time:" =>timeA[1]= values[1..].to_vec().join("").parse().unwrap(),
                "Distance:"=>distA[1]= values[1..].to_vec().join("").parse().unwrap(),
                _ => {}
            }
        }
    }

    println!("{:?}", timeA);
    println!("{:?}", distA);

    let mut waystowin=0;
    let mut mul:i64=1;
    for i in 0..timeA.len() {
        for j in 0..timeA[i] {
            let mut chargedDistance=j*(timeA[i] -j);
            if chargedDistance> distA[i] {
                waystowin+=1;
            }
            //println!("cd: {}, ways:{}", chargedDistance,waystowin);
        }
        mul*=waystowin;
    }

    println!("{}", waystowin);
}

