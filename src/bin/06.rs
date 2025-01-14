advent_of_code::solution!(6);

use std::{collections::HashSet, fmt::Debug};

use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::line_ending,
    multi::{many1, separated_list1},
    IResult, Parser,
};

use rayon::prelude::*;

struct Grid<T>(Vec<Vec<T>>);

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
    Visited,
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

impl Debug for Marker {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Empty => f.write_str(".")?,
            Obstacle => f.write_str("#")?,
            Start => f.write_str("^")?,
            Visited => f.write_str("X")?,
        }
        Ok(())
    }
}

impl<T: Debug> Debug for Grid<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for row in &self.0 {
            for m in row {
                f.write_fmt(format_args!("{:?}", m))?;
            }
            f.write_str("\n")?;
        }
        Ok(())
    }
}

impl Grid<Marker> {
    fn mark_visited(&self, visited: &HashSet<Position>) -> Self {
        let visited_grid: Vec<Vec<Marker>> = self
            .0
            .iter()
            .enumerate()
            .map(|(y, row)| {
                row.iter()
                    .enumerate()
                    .map(|(x, m)| {
                        if visited.contains(&Position { y, x })
                            && !matches!(m, Start)
                        {
                            Visited
                        } else {
                            *m
                        }
                    })
                    .collect()
            })
            .collect();

        Grid(visited_grid)
    }
}

impl<T> Grid<T> {
    fn get(&self, Position { x, y }: Position) -> Option<&T> {
        self.0.get(y).and_then(|r| r.get(x))
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

    fn from(grid: &Grid<Marker>) -> Self {
        grid.0
            .iter()
            .enumerate()
            .find_map(|(y, row)| {
                row.iter().enumerate().find_map(|(x, m)| {
                    matches!(m, Start).then(|| Position { x, y })
                })
            })
            .map(Guard::new)
            .expect("starting position not found")
    }

    fn step(
        &mut self,
        grid: &Grid<Marker>,
        extra_obstacle: Option<Position>,
    ) -> StepOutcome {
        if let Some((position, marker)) = self
            .direction
            .next(&self.position)
            .and_then(|p| grid.get(p).map(|m| (p, m)))
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
        grid: &Grid<Marker>,
        extra_obstacle: Option<Position>,
    ) -> StepOutcome {
        let mut outcome = StepOutcome::InProgres;
        while matches!(outcome, StepOutcome::InProgres) {
            outcome = self.step(grid, extra_obstacle)
        }
        outcome
    }
}

fn parse_input(input: &str) -> IResult<&str, Grid<Marker>> {
    separated_list1(
        line_ending,
        many1(alt((
            tag(".").map(|_| Empty),
            tag("#").map(|_| Obstacle),
            tag("^").map(|_| Start),
        ))),
    )(input)
    .map(|(r, g)| (r, Grid(g)))
}

pub fn part_one(input: &str) -> Option<u64> {
    let (_, grid) = parse_input(input).expect("parsing failed");

    let mut guard = Guard::from(&grid);

    guard.walk(&grid, None);

    let visited: HashSet<Position> =
        guard.visited.iter().map(|(p, _)| *p).collect();

    Some(visited.len() as u64)
}

pub fn part_two(input: &str) -> Option<u64> {
    let (_, grid) = parse_input(input).expect("parsing failed");

    let potential_obstacles: Vec<Position> = grid
        .0
        .par_iter()
        .enumerate()
        .flat_map(|(y, row)| {
            row.iter()
                .enumerate()
                .filter_map(|(x, m)| {
                    let position = Position { x, y };
                    if !matches!(m, Obstacle) {
                        let mut guard = Guard::from(&grid);
                        let outcome = guard.walk(&grid, Some(position));
                        matches!(outcome, StepOutcome::CycleDetected)
                            .then(|| position)
                    } else {
                        None
                    }
                })
                .collect::<Vec<Position>>()
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
