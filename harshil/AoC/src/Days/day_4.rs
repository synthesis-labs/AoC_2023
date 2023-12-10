use std::collections::{HashMap, HashSet, VecDeque};
use std::fs::File;
use std::io::Read;
use std::iter::Map;
use num_traits::pow;


pub fn day_4_pt1() {
    let file = File::open("C:/Users/harsh/RustroverProjects/AoC_2023/harshil/AoC/src/Data/Day_4.txt");
    let mut contents = String::new();
    file.unwrap().read_to_string(&mut contents).expect("Error reading file");
    let mut sum=0;

    for line in contents.lines() {
        let game = &line[0..line.find(":").unwrap()+1];
        let mut line = line.replace(game, "");

        let winning: Vec<i32> = line[0..line.find("|").unwrap()-1]
            .split_whitespace()
            .map(|s|{
                println!("{}", s);
                s.parse().unwrap()})
            .collect();
        let yours: Vec<i32> = line[line.find("|").unwrap()+1..]
            .split_whitespace()
            .map(|s| s.parse().unwrap())
            .collect();

        let won = find_intersection(winning.clone(), yours.clone());
        if won.len()>0 {
            sum += pow(2, (won.len()-1));
        }
        println!("Sum:{}", sum);
    }
    print!("{}", sum)

}

pub fn day_4_pt2() {
    let file = File::open("C:/Users/harsh/RustroverProjects/AoC_2023/harshil/AoC/src/Data/Day_4.txt");
    let mut contents = String::new();
    file.unwrap().read_to_string(&mut contents).expect("Error reading file");
    let mut mapcard= HashMap::new();
    let mut q=VecDeque::new();
    let mut sum=0;

    for line in contents.lines() {
        let parts: Vec<&str> = line.split(':').collect();
        println!("{:?}", parts);

        let cid: i32 = parts[0][4..].trim().parse().unwrap();

        let numbers = parts[1];
        let separator_idx = numbers.find('|').unwrap();
        let (winner, ours) = numbers.split_at(separator_idx);

        let winning: HashSet<&str> = winner.split_whitespace().collect();
        let yours: HashSet<&str> = ours.split_whitespace().collect();

        let matches: HashSet<_> = winning.intersection(&yours).cloned().collect();

        mapcard.insert(cid,matches);
        q.push_back(cid);

    }

    while let Some(k) = q.pop_front() {
        sum += 1;
        for i in k + 1..=k + mapcard.get(&k).unwrap().len() as i32{
            q.push_back(i);
        }
    }


    println!("{}", sum)
}

// pub fn rec(mapcards: &HashMap<String, i32>, matches:i32, pos: String)->(HashMap<String,i32>,i32){
//     let mut rmap =HashMap::new();
//     for i in 0..matches {
//         if  rmap.contains_key(&pos) {
//             let cur=rmap.get(&pos);
//             rmap.entry(&pos).or_insert(cur+1);
//         }else {
//             rmap.insert(&pos,1 );
//         }
//     }
//
// }

pub fn find_intersection(winning:Vec<i32>, yours:Vec<i32>) -> Vec<i32>{
    return winning.into_iter()
        .filter(|x| yours.contains(x))
        .collect();
}

