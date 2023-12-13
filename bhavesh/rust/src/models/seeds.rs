use std::collections::VecDeque;

pub type Seeds = VecDeque<Seed>;
pub type Seed = i64;
pub type Location = i64;
pub type AlmanacMaps = VecDeque<AlmanacMap>;
pub type AlmanacItems = VecDeque<AlmanacMaps>;

pub type Almanac = (Seeds, AlmanacItems);

#[derive(Clone, Debug)]
pub struct AlmanacMap {
    pub destination_start: i64,
    pub source_start: i64,
    pub length: i64,
}
