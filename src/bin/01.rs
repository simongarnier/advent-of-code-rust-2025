advent_of_code::solution!(1);

use std::iter::zip;

use itertools::Itertools;
use nom::{
    bytes::complete::tag, character::complete::i32, multi::separated_list0,
    sequence::separated_pair, IResult,
};

fn parse_number_pair(input: &str) -> IResult<&str, (i32, i32)> {
    separated_pair(i32, tag("   "), i32)(input)
}

fn parse_input(input: &str) -> (Vec<i32>, Vec<i32>) {
    let (_, pair) = separated_list0(tag("\n"), parse_number_pair)(input)
        .map(|(rest, pair)| (rest, pair.into_iter().unzip()))
        .expect("failed to parse input");

    pair
}

pub fn part_one(input: &str) -> Option<u64> {
    let (first, second) = parse_input(input);
    let result: i32 = zip(first.iter().sorted(), second.iter().sorted())
        .map(|(f, s)| (f - s).abs())
        .sum();

    Some(result as u64)
}

pub fn part_two(input: &str) -> Option<u64> {
    let (first, second) = parse_input(input);
    let result: i32 = first
        .iter()
        .map(|f| f * second.iter().filter(|s| f == *s).count() as i32)
        .sum();

    Some(result as u64)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part_one() {
        let result = part_one(&advent_of_code::template::read_file("examples", DAY));
        assert_eq!(result, Some(11));
    }

    #[test]
    fn test_part_two() {
        let result = part_two(&advent_of_code::template::read_file("examples", DAY));
        assert_eq!(result, Some(31));
    }

    #[test]
    fn test_basic_parser() {
        assert_eq!(
            parse_input(advent_of_code::template::read_file("examples", DAY).as_str()),
            (vec![3, 4, 2, 1, 3, 3], vec![4, 3, 5, 3, 9, 3])
        )
    }
}
