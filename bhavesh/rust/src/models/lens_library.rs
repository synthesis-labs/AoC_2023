use std::collections::{HashMap, VecDeque};

pub type LensLabel = String;
pub type FocalLength = i32;
pub type Box = i32;
pub type Lenses = VecDeque<Lens>;
pub type Boxes = HashMap<Box, Lenses>;

#[derive(Debug, Clone, PartialEq)]
pub struct Lens {
    pub label: LensLabel,
    pub focal_length: FocalLength,
}
