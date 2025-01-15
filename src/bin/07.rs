use std::sync::Arc;

use itertools::Itertools;
use moka::sync::Cache;
use nom::{
    bytes::complete::tag,
    character::complete::{line_ending, u64},
    multi::separated_list1,
    sequence::{separated_pair, terminated},
    IResult, Parser,
};
use rayon::prelude::*;

advent_of_code::solution!(7);

#[derive(Debug, PartialEq, Eq)]
struct Equation {
    total: u64,
    operands: Vec<u64>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
enum Operation {
    Mul,
    Add,
    Concat,
}

impl Operation {
    fn apply(&self, lhs: u64, rhs: u64) -> u64 {
        match self {
            Operation::Mul => lhs.saturating_mul(rhs),
            Operation::Add => lhs.saturating_add(rhs),
            Operation::Concat => lhs * 10u64.pow(rhs.ilog10() + 1) + rhs,
        }
    }
}

fn parse_input(input: &str) -> IResult<&str, Vec<Equation>> {
    terminated(
        separated_list1(
            line_ending,
            separated_pair(u64, tag(": "), separated_list1(tag(" "), u64))
                .map(|(total, operands)| Equation { total, operands }),
        ),
        tag("\n"),
    )(input)
}

fn get_operation_cartesian_product(
    operations: Vec<Operation>,
    len: usize,
) -> Vec<Vec<Operation>> {
    std::iter::repeat(operations)
        .take(len)
        .multi_cartesian_product()
        .collect()
}

fn compute_with(equations: Vec<Equation>, operations: Vec<Operation>) -> u64 {
    let operation_cartesian_product_cache: Cache<
        usize,
        Arc<Vec<Vec<Operation>>>,
    > = Cache::new(30);
    equations
        .par_iter()
        .filter(|Equation { total, operands }| {
            let operator_len = operands.len() - 1;
            operation_cartesian_product_cache
                .get_with(operator_len, || {
                    Arc::new(get_operation_cartesian_product(
                        operations.clone(),
                        operator_len,
                    ))
                })
                .clone()
                .iter()
                .any(|ops| {
                    let mut ops_iter = ops.iter();
                    operands
                        .iter()
                        .copied()
                        .reduce(|acc, operand| {
                            ops_iter
                                .next()
                                .expect("missing operator")
                                .apply(acc, operand)
                        })
                        .map(|r| r == *total)
                        .expect("list of operands was empty")
                })
        })
        .map(|Equation { total, operands: _ }| total)
        .sum()
}

pub fn part_one(input: &str) -> Option<u64> {
    let (_, equations) = parse_input(input).expect("parsing failed");

    Some(compute_with(
        equations,
        vec![Operation::Add, Operation::Mul],
    ))
}

pub fn part_two(input: &str) -> Option<u64> {
    let (_, equations) = parse_input(input).expect("parsing failed");

    Some(compute_with(
        equations,
        vec![Operation::Add, Operation::Mul, Operation::Concat],
    ))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_input() {
        assert_eq!(
            parse_input(
                advent_of_code::template::read_file("examples", DAY).as_str(),
            ),
            Ok((
                "",
                vec![
                    Equation {
                        total: 190,
                        operands: vec![10, 19]
                    },
                    Equation {
                        total: 3267,
                        operands: vec![81, 40, 27]
                    },
                    Equation {
                        total: 83,
                        operands: vec![17, 5]
                    },
                    Equation {
                        total: 156,
                        operands: vec![15, 6]
                    },
                    Equation {
                        total: 7290,
                        operands: vec![6, 8, 6, 15]
                    },
                    Equation {
                        total: 161011,
                        operands: vec![16, 10, 13]
                    },
                    Equation {
                        total: 192,
                        operands: vec![17, 8, 14]
                    },
                    Equation {
                        total: 21037,
                        operands: vec![9, 7, 18, 13]
                    },
                    Equation {
                        total: 292,
                        operands: vec![11, 6, 16, 20]
                    }
                ]
            ))
        );
    }

    #[test]
    fn test_concat() {
        assert_eq!(Operation::Concat.apply(12, 345), 12345)
    }

    #[test]
    fn test_part_one() {
        let result =
            part_one(&advent_of_code::template::read_file("examples", DAY));
        assert_eq!(result, Some(3749));
    }

    #[test]
    fn test_part_two() {
        let result =
            part_two(&advent_of_code::template::read_file("examples", DAY));
        assert_eq!(result, Some(11387));
    }
}
