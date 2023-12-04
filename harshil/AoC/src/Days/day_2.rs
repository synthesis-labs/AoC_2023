use std::fs::File;
use std::io::Read;

pub fn day_2_pt1(){
    let file = File::open("C:/Users/harsh/RustroverProjects/AoC_2023/harshil/AoC/src/Data/Day_2.txt");
    let mut contents = String::new();
    file.unwrap().read_to_string(&mut contents).expect("Error reading file");

    let mut poss_game_sum =0;
    let rload=12;
    let gload=13;
    let bload=14;
    let mut gameload=0;
    
    for mut line in contents.lines() {

        let mut rposs=true;
        let mut bposs=true;
        let mut gposs=true;

        gameload+=1;
        //gameload=line.chars().nth(5).and_then(|c| c.to_digit(10).map(|d| d as i32));
        let _ = line.replace("Game ", "");
        let sep_game:Vec<_>= line.split(|c| c==',' || c==';').collect();
        let dred: Vec<_> = sep_game.iter().filter(|red| red.contains("red")).collect();
        let dblue: Vec<_> = sep_game.iter().filter(|blue| blue.contains("blue")).collect();
        let dgreen: Vec<_> = sep_game.iter().filter(|green| green.contains("green")).collect();
        println!("{:?}", gameload);
        println!("{:?}", dred);

        for  mut red in dred {
            //println!("{:?}", red.chars().nth(1).and_then(|c| c.to_digit(10)));
            println!("this is the sub before:{}", red);
            let tred= red.trim();
            let space:usize= tred.find(" ").unwrap() as usize;
            println!("this is the sub after:{}", tred);
//            if red[1..space].and_then(|c| c.to_digit(10))> Some(rload) {
            let mut sub= &red[1..(space)];
            println!("this is the sub{}", &red[1..space]);
            if let Ok(isub) = sub.parse::<i32>() {
                if red[1..space].parse::<i32>().unwrap() > rload {
                    rposs = false;
                    break;
                }
            }
        }

        for  blue in dblue {
            if blue.chars().nth(1).and_then(|c| c.to_digit(10))> Some(bload) {
                bposs=false;
                break;
            }
        }

        for  green in dgreen {
            if green.chars().nth(1).and_then(|c| c.to_digit(10))> Some(gload) {
                gposs=false;
                break;
            }
        }

        if rposs && bposs && gposs {
           poss_game_sum+=gameload;
        }
    }

    println!("{}", poss_game_sum);
}