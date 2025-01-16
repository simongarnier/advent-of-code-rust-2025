advent_of_code::solution!(8);

use grid::*;
use itertools::Itertools;
use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{anychar, line_ending},
    combinator::verify,
    multi::{many1, separated_list1},
    sequence::terminated,
    IResult, Parser,
};
use std::{
    char,
    collections::{HashMap, HashSet},
    ops::Add,
};

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
enum Marker {
    Empty,
    Antenna(char),
}
use Marker::*;

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
struct Vec2d {
    x: i32,
    y: i32,
}

impl Add for Vec2d {
    type Output = Vec2d;
    fn add(self, rhs: Self) -> Self::Output {
        Vec2d {
            x: self.x + rhs.x,
            y: self.y + rhs.y,
        }
    }
}

impl Vec2d {
    fn get_vector_to(&self, other: &Self) -> Self {
        Vec2d {
            x: other.x - self.x,
            y: other.y - self.y,
        }
    }

    fn in_grid(self, grid: &Grid<Marker>) -> Option<Self> {
        grid.get(self.y, self.x).map(|_| self)
    }
}

fn parse_input(input: &str) -> IResult<&str, Grid<Marker>> {
    terminated(
        separated_list1(
            line_ending,
            many1(alt((
                tag(".").map(|_| Empty),
                verify(anychar, |c| c.is_alphanumeric()).map(Antenna),
            ))),
        ),
        line_ending,
    )(input)
    .map(|(r, g)| (r, Grid::from(g)))
}

pub fn part_one(input: &str) -> Option<u64> {
    let (rest, grid) = parse_input(input).expect("parsing failed");

    if !rest.is_empty() {
        panic!("rest of input: {rest:?}");
    }

    let antennas: HashMap<char, HashSet<Vec2d>> = grid
        .indexed_iter()
        .filter_map(|((y, x), marker)| match marker {
            Empty => None,
            Antenna(char) => Some((
                Vec2d {
                    x: x as i32,
                    y: y as i32,
                },
                char,
            )),
        })
        .fold(HashMap::new(), |mut acc, (vec2d, c)| {
            match acc.get_mut(c) {
                Some(set) => {
                    set.insert(vec2d);
                }
                None => {
                    acc.insert(*c, HashSet::from([vec2d]));
                }
            }
            acc
        });

    let antinode_sets: Vec<HashSet<Vec2d>> = antennas
        .values()
        .map(|vectors| {
            vectors
                .iter()
                .permutations(2)
                .filter_map(|pair| {
                    let first = pair[0];
                    let second = pair[1];

                    first.get_vector_to(second).add(*second).in_grid(&grid)
                })
                .collect()
        })
        .collect();

    let antinodes: HashSet<&Vec2d> = antinode_sets.iter().flatten().collect();
    Some(antinodes.len() as u64)
}

pub fn part_two(input: &str) -> Option<u64> {
    let (rest, grid) = parse_input(input).expect("parsing failed");

    if !rest.is_empty() {
        panic!("rest of input: {rest:?}");
    }

    let antennas: HashMap<char, HashSet<Vec2d>> = grid
        .indexed_iter()
        .filter_map(|((y, x), marker)| match marker {
            Empty => None,
            Antenna(char) => Some((
                Vec2d {
                    x: x as i32,
                    y: y as i32,
                },
                char,
            )),
        })
        .fold(HashMap::new(), |mut acc, (vec2d, c)| {
            match acc.get_mut(c) {
                Some(set) => {
                    set.insert(vec2d);
                }
                None => {
                    acc.insert(*c, HashSet::from([vec2d]));
                }
            }
            acc
        });

    let antinode_sets: Vec<Vec<Vec<Vec2d>>> = antennas
        .values()
        .map(|vectors| {
            vectors
                .iter()
                .permutations(2)
                .map(|pair| {
                    let first = pair[0];
                    let second = pair[1];

                    let mut antinodes: Vec<Vec2d> = Vec::new();

                    let vector = first.get_vector_to(second);

                    let mut current = first.add(vector);

                    while current.in_grid(&grid).is_some() {
                        antinodes.push(current);
                        current = current.add(vector);
                    }

                    antinodes
                })
                .collect()
        })
        .collect();

    let antinodes: HashSet<&Vec2d> =
        antinode_sets.iter().flatten().flatten().collect();

    Some(antinodes.len() as u64)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_input() {
        assert!(parse_input(
            advent_of_code::template::read_file("examples", DAY).as_str(),
        )
        .is_ok(),)
    }

    #[test]
    fn test_part_one() {
        let result =
            part_one(&advent_of_code::template::read_file("examples", DAY));
        assert_eq!(result, Some(14));
    }

    #[test]
    fn test_part_two() {
        let result =
            part_two(&advent_of_code::template::read_file("examples", DAY));
        assert_eq!(result, Some(34));
    }
}
