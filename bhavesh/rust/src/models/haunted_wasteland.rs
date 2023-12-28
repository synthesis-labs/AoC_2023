use std::collections::HashMap;

pub type Instruction = char;
pub type Node = String;
pub type Nodes = Vec<Node>;
pub type Instructions = Vec<Instruction>;
pub type Branches = HashMap<Instruction, Node>;
pub type Map = HashMap<Node, Branches>;
