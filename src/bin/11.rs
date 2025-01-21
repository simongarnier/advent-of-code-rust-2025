use std::collections::HashMap;

use nom::{
    bytes::complete::tag,
    character::complete::{i64, line_ending},
    multi::separated_list1,
    sequence::terminated,
    IResult, Parser,
};
use rayon::prelude::*;

advent_of_code::solution!(11);

#[derive(Debug, PartialEq)]
enum Operation {
    ZeroToOne,
    Times2024,
}

#[derive(Debug, PartialEq)]
enum StoneTree {
    Leaf(u64),
    Split {
        left: Box<StoneTree>,
        right: Box<StoneTree>,
    },
    Mutate {
        operation: Operation,
        next: Box<StoneTree>,
    },
}

impl StoneTree {
    fn new(value: u64) -> Self {
        StoneTree::Leaf(value)
    }
}

fn split(value: u64) -> Option<(u64, u64)> {
    let digit_len = value.ilog10() + 1;
    if digit_len % 2 == 0 {
        let zero_len = 10u64.pow(digit_len / 2);
        let left = value / zero_len;
        Some((left, value - left * zero_len))
    } else {
        None
    }
}

fn blink(stone_tree: &mut StoneTree) -> u64 {
    match stone_tree {
        StoneTree::Leaf(0) => {
            *stone_tree = StoneTree::Mutate {
                operation: Operation::ZeroToOne,
                next: Box::new(StoneTree::Leaf(1)),
            };

            1
        }
        StoneTree::Leaf(val) => {
            if let Some((left, right)) = split(*val) {
                *stone_tree = StoneTree::Split {
                    left: Box::new(StoneTree::Leaf(left)),
                    right: Box::new(StoneTree::Leaf(right)),
                };
                2
            } else {
                *stone_tree = StoneTree::Mutate {
                    operation: Operation::Times2024,
                    next: Box::new(StoneTree::Leaf(*val * 2024)),
                };
                1
            }
        }
        StoneTree::Split { left, right } => blink(right) + blink(left),
        StoneTree::Mutate { operation: _, next } => blink(next),
    }
}

pub fn part_one(input: &str) -> Option<u64> {
    let (_, mut trees) = parse_input(input).expect("parsing failed");

    Some(trees.par_iter_mut().map(|tree| blink_times(tree, 25)).sum())
}

pub fn part_two(input: &str) -> Option<u64> {
    let (_, result_occurences) =
        parse_input_to_hash_map(input).expect("parsing_failed");

    let updated_occurences = (0..75).fold(result_occurences, |mut acc, _| {
        let keys: Vec<_> = acc.clone().into_iter().collect();
        acc.clear();

        for (key, occurences) in keys {
            match key {
                0 => {
                    acc.entry(1)
                        .and_modify(|v| *v += occurences)
                        .or_insert(occurences);
                }
                key => {
                    if let Some((left, right)) = split(key) {
                        acc.entry(left)
                            .and_modify(|v| *v += occurences)
                            .or_insert(occurences);
                        acc.entry(right)
                            .and_modify(|v| *v += occurences)
                            .or_insert(occurences);
                    } else {
                        acc.entry(key * 2024)
                            .and_modify(|v| *v += occurences)
                            .or_insert(occurences);
                    }
                }
            }
        }

        acc
    });

    Some(updated_occurences.values().sum())
}

fn blink_times(stone_tree: &mut StoneTree, n: u8) -> u64 {
    let (_, final_count) = (0..n).fold((stone_tree, 1u64), |(t, _), n| {
        let count = blink(t);
        println!("blinked {n} times count is {count}");
        (t, count)
    });

    final_count
}

fn parse_input(input: &str) -> IResult<&str, Vec<StoneTree>> {
    terminated(
        separated_list1(tag(" "), i64.map(|v| StoneTree::new(v as u64))),
        line_ending,
    )(input)
}

fn parse_input_to_hash_map(input: &str) -> IResult<&str, HashMap<u64, u64>> {
    terminated(separated_list1(tag(" "), i64), line_ending)(input).map(
        |(r, keys)| {
            (
                r,
                keys.iter()
                    .map(|k| (*k as u64, 1u64))
                    .collect::<HashMap<_, _>>(),
            )
        },
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_split() {
        assert_eq!(split(8123), Some((81u64, 23u64)));
        assert_eq!(split(81234), None);
        assert_eq!(split(9959_9796), Some((9959, 9796)));
    }

    #[test]
    fn test_parse_input() {
        assert_eq!(
            parse_input(&advent_of_code::template::read_file("examples", DAY)),
            Ok(("", vec![StoneTree::Leaf(125), StoneTree::Leaf(17)])),
        )
    }

    #[test]
    fn test_parse_input_to_hash_map() {
        assert_eq!(
            parse_input_to_hash_map(&advent_of_code::template::read_file(
                "examples", DAY
            )),
            Ok(("", HashMap::from([(125, 1), (17, 1)]))),
        )
    }

    #[test]
    fn test_6_blink() {
        let result_125 = blink_times(&mut StoneTree::new(125), 6);
        let result_17 = blink_times(&mut StoneTree::new(17), 6);
        assert_eq!(result_17 + result_125, 22);
    }

    #[test]
    fn test_part_one() {
        let result =
            part_one(&advent_of_code::template::read_file("examples", DAY));
        assert_eq!(result, Some(55312));
    }

    #[test]
    fn test_part_two() {
        let result =
            part_two(&advent_of_code::template::read_file("examples", DAY));
        assert_eq!(result, Some(65601038650482));
    }
}
