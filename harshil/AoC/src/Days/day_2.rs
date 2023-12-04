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
        let sep_game:Vec<_>= line.split(|c| c==',' || c==';' || c==':').collect();
        let dred: Vec<_> = sep_game.iter().filter(|red| red.contains("red")).collect();
        let dblue: Vec<_> = sep_game.iter().filter(|blue| blue.contains("blue")).collect();
        let dgreen: Vec<_> = sep_game.iter().filter(|green| green.contains("green")).collect();
        println!("{:?}", gameload);
        println!("{:?}", dred);

        for mut red in dred {
            let rred = red.replace(" red", "").trim().to_string();
            match rred.parse::<i32>() {
                Ok(ired) => {
                    if ired > rload {
                        rposs = false;
                        break;
                    }
                }
                Err(err) => {
                    println!("Failed to parse integer: {:?}: :{}:", err,rred);
                }
            }
        }

        for mut blue in dblue {
            let rblue = blue.replace(" blue", "").trim().to_string();
            match rblue.parse::<i32>() {
                Ok(iblue) => {
                    if iblue > bload {
                        rposs = false;
                        break;
                    }
                }
                Err(err) => {
                    println!("Failed to parse integer: {:?}: :{}:", err,rblue);
                }
            }
        }

        for mut green in dgreen {
            let rgreen = green.replace(" green", "").trim().to_string();
            match rgreen.parse::<i32>() {
                Ok(igreen) => {
                    if igreen > gload {
                        rposs = false;
                        break;
                    }
                }
                Err(err) => {
                    println!("Failed to parse integer: {:?}: :{}:", err,rgreen);
                }
            }
        }

        if rposs && bposs && gposs {
           poss_game_sum+=gameload;
        }
    }

    println!("{}", poss_game_sum);
}

pub fn day_2_pt2(){
    let file = File::open("C:/Users/harsh/RustroverProjects/AoC_2023/harshil/AoC/src/Data/Day_2.txt");
    let mut contents = String::new();
    file.unwrap().read_to_string(&mut contents).expect("Error reading file");

    let mut poss_game_mul =0;
    let mut poss_game_sum=0;

    let mut gameload=0;

    for mut line in contents.lines() {
        let mut rload =0;
        let mut gload =0;
        let mut bload =0;

        gameload+=1;
        //gameload=line.chars().nth(5).and_then(|c| c.to_digit(10).map(|d| d as i32));
        let _ = line.replace("Game ", "");
        let sep_game:Vec<_>= line.split(|c| c==',' || c==';' || c==':').collect();
        let dred: Vec<_> = sep_game.iter().filter(|red| red.contains("red")).collect();
        let dblue: Vec<_> = sep_game.iter().filter(|blue| blue.contains("blue")).collect();
        let dgreen: Vec<_> = sep_game.iter().filter(|green| green.contains("green")).collect();
        println!("{:?}", dred);
        println!("{:?}", dblue);
        println!("{:?}", dgreen);

        for mut red in dred {
            let rred = red.replace(" red", "").trim().to_string();
            match rred.parse::<i32>() {
                Ok(ired) => {
                    if ired > rload {
                        //println!("reddd {} <{}", ired,rload);
                        rload=ired;
                    }
                }
                Err(err) => {
                    println!("Failed to parse integer: {:?}: :{}:", err,rred);
                }
            }
        }

        for mut blue in dblue {
            let rblue = blue.replace(" blue", "").trim().to_string();
            match rblue.parse::<i32>() {
                Ok(iblue) => {
                    if iblue > bload {
                        bload=iblue;
                        //println!("blue {} <{}", iblue,bload);

                    }
                }
                Err(err) => {
                    println!("Failed to parse integer: {:?}: :{}:", err,rblue);
                }
            }
        }

        for mut green in dgreen {
            let rgreen = green.replace(" green", "").trim().to_string();
            match rgreen.parse::<i32>() {
                Ok(igreen) => {
                    if igreen > gload {
                        gload=igreen;
                        //println!("green {} <{}", igreen,gload);
                    }
                }
                Err(err) => {
                    println!("Failed to parse integer: {:?}: :{}:", err,rgreen);
                }
            }
        }

        println!("red:{}, blue {}, green:{}",rload,bload,gload);
        poss_game_mul= rload*bload*gload;
        poss_game_sum+= poss_game_mul;
    }

    println!("{}", poss_game_sum);
}