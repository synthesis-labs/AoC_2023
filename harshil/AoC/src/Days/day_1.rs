use std::fs::File;
use std::io::Read;

pub fn day1pt1(){
    let file= File::open("C:/Users/harsh/RustroverProjects/AoC_2023/harshil/AoC/src/Data/Day_1.txt");
    let mut contents=String::new();
    file.unwrap().read_to_string(&mut contents).expect("TODO: panic message");
    //println!("{}", contents);
    let mut sum:i32=0;
    for line in contents.lines(){
        let mut calval:String=String::new();
        for c in line.chars() {
            if c.is_numeric() {
                calval=c.to_string();
                break;
            }
        }
        for c in line.chars().rev() {
            if c.is_numeric() {
                calval=calval+&c.to_string();
                break;
            }
        }
        let icalval:i32=calval.parse().unwrap();
        sum=sum+icalval;
        println!("{}", sum);
    }
}

pub fn day1pt2(){
    let mut words: Vec<&str> = Vec::new();
    words.push("one");
    words.push("two");
    words.push("three");
    words.push("four");
    words.push("five");
    words.push("six");
    words.push("seven");
    words.push("eight");
    words.push("nine");
    let file= File::open("C:/Users/harsh/RustroverProjects/AoC_2023/harshil/AoC/src/Data/Day_1.txt");
    let mut contents=String::new();
    file.unwrap().read_to_string(&mut contents).expect("TODO: panic message");
    //println!("{}", contents);
    let mut sum:i32=0;
    for line in contents.lines(){
        println!("{}", line);
        let mut calval:String=String::new();
        let mut i=0;
        for c in line.chars() {
            if c.is_numeric() {
                calval=c.to_string();
                println!("{}", &line[0..i]);
                break;
            }else if words.iter().any(|&s| s.contains(&line[0..i]))   {
                let index:i32= words.iter().position(|&s| s.contains(&line[0..i])).unwrap() as i32;
                calval=calval+ &*(index + 1).to_string();
                println!("{}", &line[0..i]);
                break;
            }
            i += 1;
        }
        i=0;
        for c in line.chars().rev() {
            if c.is_numeric() {
                calval=calval+&c.to_string();
                println!("{}", &line[0..i]);
                break;
            }else if words.iter().any(|&s| s.contains(&line[0..i].chars().rev().collect::<String>()))   {
                let index:i32= words.iter().position(|&s| s.contains(&line[0..i])).unwrap() as i32;
                calval=(index+1).to_string();
                println!("{}", &line[0..i]);
                calval=calval+ &*(index + 1).to_string();
            }
            i+=1;
        }
        let icalval:i32=calval.parse().unwrap();
        sum=sum+icalval;
        println!("{}", sum);
    }
}