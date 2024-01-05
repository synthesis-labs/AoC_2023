use core::fmt;
use std::collections::HashMap;

#[derive(Debug, PartialEq, Clone)]
pub enum ModuleType {
    FlipFlop(bool), // true = on, false = off
    Conjunction(HashMap<String, Pulse>),
    Broadcaster,
    Terminal,
}

impl ModuleType {
    pub fn as_conjunction(&mut self) -> Option<&mut HashMap<String, Pulse>> {
        match self {
            ModuleType::Conjunction(pulses) => Some(pulses),
            _ => None,
        }
    }
}

impl fmt::Display for ModuleType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ModuleType::FlipFlop(state) => write!(f, "FlipFlop({})", state),
            ModuleType::Conjunction(pulses) => write!(f, "Conjunction({:?})", pulses),
            ModuleType::Broadcaster => write!(f, "Broadcaster"),
            ModuleType::Terminal => write!(f, "Terminal"),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Pulse {
    Low,
    High,
    None,
}

impl fmt::Display for Pulse {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Pulse::Low => write!(f, "Low"),
            Pulse::High => write!(f, "High"),
            Pulse::None => write!(f, "None"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Module {
    pub module_type: ModuleType,
    pub destinations: Vec<String>,
}

impl fmt::Display for Module {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} -> {:?}", self.module_type, self.destinations)
    }
}

pub type Modules = HashMap<String, Module>;
pub type Pulses = Vec<Pulse>;
