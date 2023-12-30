use core::fmt;
use std::collections::HashMap;

#[derive(Debug)]
pub struct PartRating {
    pub x: i64,
    pub m: i64,
    pub a: i64,
    pub s: i64,
}

impl fmt::Display for PartRating {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "x:{} m:{} a:{} s:{}", self.x, self.m, self.a, self.s)
    }
}

#[derive(Debug, Clone)]
pub struct Rule {
    pub condition: String,
    pub result: Key,
}

impl fmt::Display for Rule {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}", self.condition, self.result)
    }
}

pub type Parts = Vec<PartRating>;
pub type Key = String;
pub type WorkFlow = Vec<Rule>;
pub type WorkFlows = HashMap<Key, WorkFlow>;
