use colored::{ColoredString, Colorize};
use fake::{faker::number::raw::Digit, locales::EN, Dummy, Fake};
use grid::Grid;
use itertools::Itertools;
use nom::{
    character::complete::{anychar, line_ending},
    combinator::map_res,
    multi::{many1, separated_list1},
    sequence::terminated,
    IResult,
};
use rand::{seq::IteratorRandom, Rng};
use std::string::String;
use std::{cell::RefCell, collections::HashMap, fmt::Display};
use strum::IntoEnumIterator;
use strum_macros::EnumIter;

advent_of_code::solution!(10);

#[derive(Debug, PartialEq, Eq, Clone)]
enum MountainArea {
    Trailhead {
        access_to: RefCell<HashMap<(usize, usize), usize>>,
    },
    Segment {
        height: u8,
        access_to: RefCell<HashMap<(usize, usize), usize>>,
    },
    Peak,
}

#[derive(Debug, PartialEq, Eq, Clone)]
struct Mountain(Grid<MountainArea>);

impl MountainArea {
    fn new(input: u8) -> Self {
        match input {
            0 => MountainArea::Trailhead {
                access_to: RefCell::new(HashMap::new()),
            },
            9 => MountainArea::Peak,
            height => MountainArea::Segment {
                height,
                access_to: RefCell::new(HashMap::new()),
            },
        }
    }
}

impl MountainArea {
    fn topographic_color(
        &self,
        peak_pos: Option<(usize, usize)>,
    ) -> ColoredString {
        match self {
            MountainArea::Trailhead { access_to: _ } => {
                format!("{}", self).white().on_green()
            }
            MountainArea::Peak => format!("{}", self).white().on_red(),
            MountainArea::Segment { height, access_to }
                if peak_pos
                    .map(|p| access_to.borrow().keys().contains(&p))
                    .unwrap_or(true) =>
            {
                format!("{}", height).white().on_truecolor(
                    255 / height,
                    255 / height,
                    255 / height,
                )
            }
            _ => ColoredString::from(" "),
        }
    }
}

impl Display for MountainArea {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                MountainArea::Trailhead { access_to: _ } => 0u8,
                MountainArea::Segment {
                    height,
                    access_to: _,
                } => *height,
                MountainArea::Peak => 9u8,
            }
        )
    }
}

struct Input;
impl Dummy<Input> for Mountain {
    fn dummy_with_rng<R: Rng + ?Sized>(_: &Input, rng: &mut R) -> Self {
        let w = (8usize..=16usize).choose(rng).unwrap();
        let h = (8usize..=16usize).choose(rng).unwrap();
        Mountain(Grid::from_vec(
            (0..w * h)
                .map(|_| {
                    MountainArea::new(
                        Digit(EN)
                            .fake_with_rng::<String, R>(rng)
                            .parse::<u8>()
                            .unwrap(),
                    )
                })
                .collect(),
            w,
        ))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, EnumIter)]
enum Direction {
    Up,
    Down,
    Left,
    Right,
}

struct Neighbours<'a> {
    mountain: &'a Mountain,
    pos: (usize, usize),
    dir_iter: DirectionIter,
}

impl<'a> Iterator for Neighbours<'a> {
    type Item = ((usize, usize), &'a MountainArea);

    fn next(&mut self) -> Option<Self::Item> {
        let (row, col) = self.pos;

        while let Some(dir) = self.dir_iter.next() {
            if let Some((next_pos, next_cell)) = match dir {
                Direction::Up => row.checked_sub(1).map(|r| (r, col)),
                Direction::Down => row.checked_add(1).map(|r| (r, col)),
                Direction::Left => col.checked_sub(1).map(|c| (row, c)),
                Direction::Right => col.checked_add(1).map(|c| (row, c)),
            }
            .and_then(|(r, c)| {
                self.mountain.0.get(r, c).map(|cell| ((r, c), cell))
            }) {
                return Some((next_pos, next_cell));
            } else {
                continue;
            }
        }
        None
    }
}

impl Mountain {
    fn neighbours(&self, pos: (usize, usize)) -> Neighbours {
        Neighbours {
            mountain: self,
            pos,
            dir_iter: Direction::iter(),
        }
    }

    fn upsert_path_count(
        access_to: &RefCell<HashMap<(usize, usize), usize>>,
        peak_pos: (usize, usize),
        path_count: usize,
    ) {
        let current_count = access_to.borrow().get(&peak_pos).copied();
        match current_count {
            None => {
                access_to.borrow_mut().insert(peak_pos, path_count);
            }
            Some(current_count) => {
                access_to
                    .borrow_mut()
                    .insert(peak_pos, path_count + current_count);
            }
        }
    }

    fn peaks(&self) -> Vec<((usize, usize), &MountainArea)> {
        self.0
            .indexed_iter()
            .filter(|(_, area)| matches!(area, MountainArea::Peak))
            .collect()
    }

    fn computes_paths(&self) {
        for (peak_pos, peak_area) in self.peaks() {
            self.backtrack((peak_pos, peak_area), peak_pos, 1);
        }
    }

    fn backtrack(
        &self,
        (current_pos, current_area): ((usize, usize), &MountainArea),
        peak_pos: (usize, usize),
        path_count: usize,
    ) {
        let neighbours = self.neighbours(current_pos);
        match current_area {
            MountainArea::Trailhead { access_to } => {
                Self::upsert_path_count(access_to, peak_pos, path_count);
            }
            MountainArea::Segment { height, access_to } => {
                Self::upsert_path_count(access_to, peak_pos, path_count);
                // recurse
                neighbours.for_each(|(n_pos, n_area)| match n_area {
                    MountainArea::Trailhead { access_to: _ }
                        if *height == 1 =>
                    {
                        self.backtrack((n_pos, n_area), peak_pos, path_count);
                    }
                    MountainArea::Segment {
                        height: next_height,
                        access_to: _,
                    } if *next_height == height - 1 => {
                        self.backtrack((n_pos, n_area), peak_pos, path_count);
                    }
                    _ => {}
                });
            }
            MountainArea::Peak => {
                neighbours.for_each(|(n_pos, n_area)| match n_area {
                    MountainArea::Segment {
                        height: next_height,
                        access_to: _,
                    } if *next_height == 8 => {
                        self.backtrack((n_pos, n_area), peak_pos, path_count);
                    }
                    _ => {}
                });
            }
        }
    }

    #[allow(dead_code)]
    fn topographic_color(
        &self,
        peak_pos: Option<(usize, usize)>,
    ) -> ColoredString {
        let mut buffer = String::from("");
        for row in self.0.iter_rows().map(|row| {
            let mut buffer = String::from("");
            let colored_cells = row.map(|c| c.topographic_color(peak_pos));
            for cell in colored_cells {
                buffer = format!("{buffer}{cell}");
            }
            buffer
        }) {
            buffer = format!("{buffer}\n{row}");
        }
        ColoredString::from(buffer)
    }
}

impl From<Mountain> for String {
    fn from(value: Mountain) -> Self {
        value
            .0
            .iter_rows()
            .map(|row| row.map(|c| c.to_string()).join(""))
            .join("\n")
            + "\n"
    }
}

impl From<String> for Mountain {
    fn from(value: String) -> Self {
        parse_input(value.as_str()).expect("parsing failed").1
    }
}

fn parse_single_digit(input: &str) -> IResult<&str, u8> {
    map_res(anychar, |c| {
        c.to_digit(10)
            .and_then(|d| u8::try_from(d).ok())
            .ok_or("could not parse digit")
    })(input)
}

fn parse_mountain_cell(input: &str) -> IResult<&str, MountainArea> {
    parse_single_digit(input).map(|(r, h)| (r, MountainArea::new(h)))
}

fn parse_input(input: &str) -> IResult<&str, Mountain> {
    terminated(
        separated_list1(line_ending, many1(parse_mountain_cell)),
        line_ending,
    )(input)
    .map(|(r, g)| (r, Mountain(Grid::from(g))))
}

pub fn part_one(input: &str) -> Option<u64> {
    let mountain: Mountain = Mountain::from(input.to_string());

    mountain.computes_paths();

    let score_sum: u64 = mountain
        .0
        .indexed_iter()
        .filter_map(|(_, area)| match area {
            MountainArea::Trailhead { access_to } => {
                Some(access_to.borrow().keys().len() as u64)
            }
            _ => None,
        })
        .sum();

    Some(score_sum)
}

pub fn part_two(input: &str) -> Option<u64> {
    let mountain: Mountain = Mountain::from(input.to_string());

    mountain.computes_paths();

    let score_sum: usize = mountain
        .0
        .indexed_iter()
        .filter_map(|(_, area)| match area {
            MountainArea::Trailhead { access_to } => {
                Some(access_to.borrow().values().copied().sum::<usize>())
            }
            _ => None,
        })
        .sum();

    Some(score_sum as u64)
}

#[cfg(test)]
mod tests {
    use std::collections::HashSet;

    use super::*;
    use fake::Fake;

    #[test]
    fn test_parse_single_digit() {
        let input: &str = Digit(EN).fake();
        let result = parse_single_digit(input);
        assert_eq!(result, Ok(("", input.parse().unwrap())));
    }

    #[test]
    fn test_parse_input() {
        let mountain: Mountain = Input.fake();
        let input: String = mountain.clone().into();
        let result: Mountain = input.into();
        assert_eq!(result, mountain);
    }

    #[test]
    fn test_neighbours_iter_middle() {
        (0..10).for_each(|_| {
            let mountain: Mountain = Input.fake();

            let row = mountain.0.rows() / 2;
            let col = mountain.0.cols() / 2;

            assert_eq!(
                mountain
                    .neighbours((row, col))
                    .map(|(p, _)| p)
                    .collect::<HashSet<_>>(),
                HashSet::from([
                    (row + 1, col),
                    (row - 1, col),
                    (row, col + 1),
                    (row, col - 1)
                ])
            );
        })
    }

    #[test]
    fn test_neighbours_iter_zero() {
        let mountain: Mountain = Input.fake();

        let col = 0;
        let row = 0;

        assert_eq!(
            mountain
                .neighbours((row, col))
                .map(|(p, _)| p)
                .collect::<HashSet<_>>(),
            HashSet::from([(row + 1, col), (row, col + 1)])
        );
    }

    #[test]
    fn test_neighbours_iter_last() {
        let mountain: Mountain = Input.fake();

        let row = mountain.0.rows() - 1;
        let col = mountain.0.cols() - 1;

        assert_eq!(
            mountain
                .neighbours((row, col))
                .map(|(p, _)| p)
                .collect::<HashSet<_>>(),
            HashSet::from([(row - 1, col), (row, col - 1)])
        );
    }

    #[test]
    fn test_part_one() {
        let result =
            part_one(&advent_of_code::template::read_file("examples", DAY));
        assert_eq!(result, Some(36));
    }

    #[test]
    fn test_part_two() {
        let result =
            part_two(&advent_of_code::template::read_file("examples", DAY));
        assert_eq!(result, Some(81));
    }
}
