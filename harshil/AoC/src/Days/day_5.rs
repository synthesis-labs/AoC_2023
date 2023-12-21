use std::collections::HashMap;
use std::fs::File;
use std::io::Read;
use rayon::iter::{IntoParallelRefIterator, ParallelIterator, IntoParallelIterator};

use std::collections::VecDeque;

pub fn day_5_pt1() {
    let file = File::open("C:/Users/harsh/RustroverProjects/AoC_2023/harshil/AoC/src/Data/Day_5.txt");
    let mut contents = String::new();
    file.unwrap().read_to_string(&mut contents).expect("Error reading file");

    let mut Vmaps:Vec<HashMap<String,(i64,i64,i64)>>=Vec::new();

    let mut vcontent: Vec<&str>=contents.lines().collect();

    let first_line_seeds:Vec<&str>=vcontent[0].split_whitespace().collect();
    let almanac:Vec<(HashMap<String,(i64,i64,i64)>,String)>=get_all_maps(vcontent);
    let mut min:i64 =1000000000;
    for  i in 1..first_line_seeds.len() {
        let loc:i64=get_location_for_seed(almanac.clone(), first_line_seeds[i].parse().unwrap());
        if loc<min {
            min=loc
        }
    }
    println!("min {}", min)

}

pub fn day_5_pt2() {
    let file = File::open("C:/Users/harsh/RustroverProjects/AoC_2023/harshil/AoC/src/Data/Day_5.txt");
    let mut contents = String::new();
    file.unwrap().read_to_string(&mut contents).expect("Error reading file");

    let mut Vmaps:Vec<HashMap<String,(i64,i64,i64)>>=Vec::new();

    let mut vcontent: Vec<&str>=contents.lines().collect();

    let first_line_seeds:Vec<&str>=vcontent[0].split_whitespace().collect();
    let almanac:Vec<(HashMap<String,(i64,i64,i64)>,String)>=get_all_maps(vcontent);
    let mut min:i64 =1000000000;
    let mut start =0;
    let mut range =0;
    for i in 1..first_line_seeds.len()  {
        if i%2==1 {
            start=first_line_seeds[i].parse().unwrap();
            range=first_line_seeds[i+1].parse().unwrap();
        }
        for current in start..start+range  {
            let loc:i64=get_location_for_seed(almanac.clone(), current);
            if loc<min {
                min=loc
            }
        }
    }
    // for  i in 1..first_line_seeds.len() {
    //     let loc:i64=get_location_for_seed(almanac.clone(), first_line_seeds[i].parse().unwrap());
    //     if loc<min {
    //         min=loc
    //     }
    // }
    println!("min {}", min)

}
fn get_relevant_map(almanac: Vec<(HashMap<String, (i64, i64, i64)>, String)>, name_of_map: &str) -> Option<(HashMap<String, (i64, i64, i64)>, String)> {
    for tuple in almanac.iter() {
        if tuple.1.contains( name_of_map) {
            let tmp = tuple.clone();
            return Some(tmp);
        }
    }
    return None;
}
fn get_location_for_seed(almanac: Vec<(HashMap<String, (i64, i64, i64)>,String)>,seed:i64)-> i64 {
    let soil=get_soil_for_seed(almanac.clone(),seed);
    let fertilizer=get_fertilizer_for_soil(almanac.clone(), soil);
    let water=get_water_for_fertilizer(almanac.clone(),fertilizer);
    let light=get_light_for_water(almanac.clone(),water);
    let temperature=get_temperature_for_light(almanac.clone(),light);
    let humidity=get_humidity_for_temperature(almanac.clone(),temperature);
    let location=get_location_for_humidity(almanac.clone(),humidity);
    // println!("location {}",location);

    return location;

}

fn get_soil_for_seed(almanac: Vec<(HashMap<String, (i64, i64, i64)>,String)>, seed:i64) -> i64 {
    let map:HashMap<String,(i64,i64,i64)> = get_relevant_map(almanac, "seed-to-soil map").unwrap().0;
    let Vranges=map.values();
    for range in Vranges {
        let mut maxseed=range.1+range.2;
        let mut minseed=range.1;
        if seed<=maxseed && seed>=minseed {
            let diff=seed-minseed;
            return range.0+diff;
        }
    }
    return seed;
}

fn get_fertilizer_for_soil(almanac: Vec<(HashMap<String, (i64, i64, i64)>,String)>, soil:i64) -> i64 {
    let map:HashMap<String,(i64,i64,i64)> = get_relevant_map(almanac, "soil-to-fertilizer map").unwrap().0;
    let Vranges=map.values();
    for range in Vranges {
        let mut maxsoil=range.1+range.2;
        let mut minsoil=range.1;
        if soil<=maxsoil && soil>=minsoil {
            let diff=soil-minsoil;
            return range.0+diff;
        }
    }
    return soil;
}
fn get_water_for_fertilizer(almanac: Vec<(HashMap<String, (i64, i64, i64)>,String)>, fertilizer:i64) -> i64 {
    let map:HashMap<String,(i64,i64,i64)> = get_relevant_map(almanac, "fertilizer-to-water map").unwrap().0;
    let Vranges=map.values();
    for range in Vranges {
        let mut maxfertilizer=range.1+range.2;
        let mut minfertilizer=range.1;
        if fertilizer<=maxfertilizer && fertilizer>=minfertilizer {
            let diff=fertilizer-minfertilizer;
            return range.0+diff;
        }
    }
    return fertilizer;
}
fn get_light_for_water(almanac: Vec<(HashMap<String, (i64, i64, i64)>,String)>, water:i64) -> i64 {
    let map:HashMap<String,(i64,i64,i64)> = get_relevant_map(almanac, "water-to-light map").unwrap().0;
    let Vranges=map.values();
    for range in Vranges {
        let mut maxwater=range.1+range.2;
        let mut minwater=range.1;
        if water<=maxwater && water>=minwater {
            let diff=water-minwater;
            return range.0+diff;
        }
    }
    return water;
}
fn get_temperature_for_light(almanac: Vec<(HashMap<String, (i64, i64, i64)>,String)>, light:i64) -> i64 {
    let map:HashMap<String,(i64,i64,i64)> = get_relevant_map(almanac, "light-to-temperature map").unwrap().0;
    let Vranges=map.values();
    for range in Vranges {
        let mut maxlight=range.1+range.2;
        let mut minlight=range.1;
        if light<=maxlight && light>=minlight {
            let diff=light-minlight;
            return range.0+diff;
        }
    }
    return light;
}
fn get_humidity_for_temperature(almanac:Vec<(HashMap<String, (i64, i64, i64)>,String)>, temperature:i64) -> i64 {
    let map:HashMap<String,(i64,i64,i64)> = get_relevant_map(almanac, "temperature-to-humidity map").unwrap().0;
    let Vranges=map.values();
    for range in Vranges {
        let mut maxtemperature=range.1+range.2;
        let mut mintemperature=range.1;
        if temperature<=maxtemperature && temperature>=mintemperature {
            let diff=temperature-mintemperature;
            return range.0+diff;
        }
    }
    return temperature;

}
fn get_location_for_humidity(almanac: Vec<(HashMap<String, (i64, i64, i64)>,String)>, humidity:i64) -> i64 {

    let map:HashMap<String,(i64,i64,i64)> = get_relevant_map(almanac, "humidity-to-location map").unwrap().0;
    let Vranges=map.values();
    for range in Vranges {
        let mut maxhumidity=range.1+range.2;
        let mut minhumidity=range.1;
        if humidity<=maxhumidity && humidity>=minhumidity {
            let diff=humidity-minhumidity;
            return range.0+diff;

        }
    }
    return humidity;
}

fn get_all_maps(vcontent: Vec<&str>) -> Vec<(HashMap<String, (i64, i64, i64)>,String)> {
    let mut almanac: Vec<(HashMap<String, (i64, i64, i64)>,String)> = Vec::new();
    let mut i = 2;

    while i < vcontent.len() {
        let mut name_of_map = String::new();
        let mut range: (i64, i64, i64) = (0, 0, 0);
        let mut mini_map = HashMap::new();

        // Use match in a let statement to bind the result
        let matched = match vcontent[i] {
            "seed-to-soil map:" => "seed-to-soil map".parse().unwrap(),
            "soil-to-fertilizer map:" => "soil-to-fertilizer map".parse().unwrap(),
            "fertilizer-to-water map:" => "fertilizer-to-water map".parse().unwrap(),
            "water-to-light map:" => "water-to-light map".parse().unwrap(),
            "light-to-temperature map:" => "light-to-temperature map".parse().unwrap(),
            "temperature-to-humidity map:" => "temperature-to-humidity map".parse().unwrap(),
            "humidity-to-location map:" => "humidity-to-location map".parse().unwrap(),
            _ => String::new(),
        };

        // Check if the result matches a non-empty string and the current line is not empty

        if i==vcontent.len() {
         break;
        }else {
            i+=1;
        }
        for line in vcontent[i..].iter() {
            if matched.is_empty() || line.is_empty() || !line.contains(name_of_map.as_str()) {
                break;
            }
            let rangeline: Vec<&str> = line.split_whitespace().collect();
            range = (
                rangeline[0].parse().unwrap(),
                rangeline[1].parse().unwrap(),
                rangeline[2].parse().unwrap(),
            );
            mini_map.insert(matched.clone() + &*i.to_string(), range);
            i += 1;
        }
        let tmp: (HashMap<String, (i64, i64, i64)>,String) = (mini_map,matched);
        almanac.push(tmp);
        i += 1; // Increment outside the while loop
    }

    almanac
}