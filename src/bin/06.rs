advent_of_code::solution!(6);

use grid::*;
use std::{collections::HashSet, fmt::Debug};

use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::line_ending,
    multi::{many1, separated_list1},
    IResult, Parser,
};

use rayon::prelude::*;

#[derive(Debug)]
struct Map<T>(Grid<T>);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct Position {
    x: usize,
    y: usize,
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum Marker {
    Empty,
    Obstacle,
    Start,
}
use Marker::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum Direction {
    N,
    S,
    E,
    W,
}
use Direction::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum StepOutcome {
    Exited,
    CycleDetected,
    InProgres,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Guard {
    position: Position,
    direction: Direction,
    visited: HashSet<(Position, Direction)>,
}

impl From<(usize, usize)> for Position {
    fn from((row, col): (usize, usize)) -> Self {
        Position { x: col, y: row }
    }
}

impl Debug for Marker {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Empty => f.write_str(".")?,
            Obstacle => f.write_str("#")?,
            Start => f.write_str("^")?,
        }
        Ok(())
    }
}

impl Direction {
    fn next(&self, Position { x, y }: &Position) -> Option<Position> {
        match self {
            N => y.checked_sub(1).map(|y| Position { x: *x, y }),
            S => y.checked_add(1).map(|y| Position { x: *x, y }),
            E => x.checked_sub(1).map(|x| Position { x, y: *y }),
            W => x.checked_add(1).map(|x| Position { x, y: *y }),
        }
    }

    fn rotated(&self) -> Self {
        match self {
            N => W,
            W => S,
            S => E,
            E => N,
        }
    }
}

impl Guard {
    fn new(position: Position) -> Self {
        Self {
            position,
            direction: N,
            visited: HashSet::from([(position, N)]),
        }
    }

    fn from(map: &Map<Marker>) -> Self {
        map.0
            .indexed_iter()
            .find_map(|(index, m)| matches!(m, Start).then(|| index.into()))
            .map(Guard::new)
            .expect("starting position not found")
    }

    fn step(
        &mut self,
        map: &Map<Marker>,
        extra_obstacle: Option<Position>,
    ) -> StepOutcome {
        if let Some((position, marker)) = self
            .direction
            .next(&self.position)
            .and_then(|p| map.0.get(p.y, p.x).map(|m| (p, m)))
        {
            match marker {
                Obstacle => {
                    self.direction = self.direction.rotated();
                }
                _ if Some(position) == extra_obstacle => {
                    self.direction = self.direction.rotated();
                }
                _ => {
                    self.position = position;
                }
            }
            if self.visited.contains(&(self.position, self.direction)) {
                StepOutcome::CycleDetected
            } else {
                self.visited.insert((self.position, self.direction));
                StepOutcome::InProgres
            }
        } else {
            StepOutcome::Exited
        }
    }

    fn walk(
        &mut self,
        map: &Map<Marker>,
        extra_obstacle: Option<Position>,
    ) -> StepOutcome {
        let mut outcome = StepOutcome::InProgres;
        while matches!(outcome, StepOutcome::InProgres) {
            outcome = self.step(map, extra_obstacle)
        }
        outcome
    }
}

fn parse_input(input: &str) -> IResult<&str, Map<Marker>> {
    separated_list1(
        line_ending,
        many1(alt((
            tag(".").map(|_| Empty),
            tag("#").map(|_| Obstacle),
            tag("^").map(|_| Start),
        ))),
    )(input)
    .map(|(r, g)| (r, Map(Grid::from(g))))
}

pub fn part_one(input: &str) -> Option<u64> {
    let (_, map) = parse_input(input).expect("parsing failed");

    let mut guard = Guard::from(&map);

    guard.walk(&map, None);

    let visited: HashSet<Position> =
        guard.visited.iter().map(|(p, _)| *p).collect();

    Some(visited.len() as u64)
}

pub fn part_two(input: &str) -> Option<u64> {
    let (_, map) = parse_input(input).expect("parsing failed");

    let guard = Guard::from(&map);
    let mut first_guard = guard.clone();

    first_guard.walk(&map, None);

    let visited: HashSet<Position> =
        first_guard.visited.iter().map(|(p, _)| *p).collect();

    let potential_obstacles: Vec<Position> = visited
        .into_par_iter()
        .filter(|position| {
            let mut guard = guard.clone();
            let outcome = guard.walk(&map, Some(*position));
            matches!(outcome, StepOutcome::CycleDetected)
        })
        .collect();

    Some(potential_obstacles.len() as u64)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_input() {
        assert!(parse_input(
            advent_of_code::template::read_file("examples", DAY).as_str(),
        )
        .is_ok());
    }

    #[test]
    fn test_next() {
        assert_eq!(
            N.next(&Position { x: 5, y: 5 }),
            Some(Position { x: 5, y: 4 })
        );
        assert_eq!(
            S.next(&Position { x: 5, y: 5 }),
            Some(Position { x: 5, y: 6 })
        );
        assert_eq!(
            E.next(&Position { x: 5, y: 5 }),
            Some(Position { x: 4, y: 5 })
        );
        assert_eq!(
            W.next(&Position { x: 5, y: 5 }),
            Some(Position { x: 6, y: 5 })
        );
    }

    #[test]
    fn test_part_one() {
        let result =
            part_one(&advent_of_code::template::read_file("examples", DAY));
        assert_eq!(result, Some(41));
    }

    #[test]
    fn test_part_two() {
        let result =
            part_two(&advent_of_code::template::read_file("examples", DAY));
        assert_eq!(result, Some(6));
    }
}
