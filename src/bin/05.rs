use std::cmp::Ordering;

use itertools::Itertools;
use nom::{
    bytes::complete::tag,
    character::complete::{i32, line_ending},
    multi::separated_list1,
    sequence::{separated_pair, terminated},
    IResult,
};

advent_of_code::solution!(5);

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
struct Rule {
    before: i32,
    after: i32,
}

impl Rule {
    fn is_valid(&self, before: i32, after: i32) -> bool {
        self.before != after || self.after != before
    }
}

fn parse_rule(input: &str) -> IResult<&str, Rule> {
    separated_pair(i32, tag("|"), i32)(input)
        .map(|(r, (before, after))| (r, Rule { before, after }))
}

fn parse_rules(input: &str) -> IResult<&str, Vec<Rule>> {
    separated_list1(line_ending, parse_rule)(input)
}

fn parse_update(input: &str) -> IResult<&str, Vec<i32>> {
    separated_list1(tag(","), i32)(input)
}

fn parse_updates(input: &str) -> IResult<&str, Vec<Vec<i32>>> {
    separated_list1(line_ending, parse_update)(input)
}

fn parse_input(input: &str) -> IResult<&str, (Vec<Rule>, Vec<Vec<i32>>)> {
    terminated(
        separated_pair(parse_rules, tag("\n\n"), parse_updates),
        tag("\n"),
    )(input)
}

fn partition_updates(
    rules: &[Rule],
    updates: Vec<Vec<i32>>,
) -> (Vec<Vec<i32>>, Vec<Vec<i32>>) {
    updates.into_iter().partition(|update| {
        update.iter().enumerate().all(|(i, page)| {
            let preceding = &update[0..i];
            let follwing = &update[i + 1..update.len()];

            rules.iter().all(|rule| {
                preceding.iter().all(|p| rule.is_valid(*p, *page))
                    && follwing.iter().all(|f| rule.is_valid(*page, *f))
            })
        })
    })
}

fn sum_middle(updates: Vec<Vec<i32>>) -> i32 {
    updates.iter().map(|update| update[update.len() / 2]).sum()
}

pub fn part_one(input: &str) -> Option<u64> {
    let (_, (rules, updates)) = parse_input(input).expect("parsing failed");
    let (sorted, _) = partition_updates(&rules, updates);
    let result = sum_middle(sorted);
    Some(result as u64)
}

pub fn part_two(input: &str) -> Option<u64> {
    let (_, (rules, updates)) = parse_input(input).expect("parsing failed");
    let (_, unsorted) = partition_updates(&rules, updates);
    let result = sum_middle(
        unsorted
            .into_iter()
            .map(|update| {
                update
                    .into_iter()
                    .sorted_by(|before, after| {
                        if rules
                            .iter()
                            .all(|rule| rule.is_valid(*before, *after))
                        {
                            Ordering::Equal
                        } else {
                            Ordering::Less
                        }
                    })
                    .collect()
            })
            .collect(),
    );

    Some(result as u64)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_rule() {
        assert_eq!(
            parse_rule("47|53"),
            Ok((
                "",
                Rule {
                    before: 47,
                    after: 53
                }
            ))
        )
    }

    #[test]
    fn test_parse_update() {
        assert_eq!(
            parse_update("75,47,61,53,29"),
            Ok(("", vec![75, 47, 61, 53, 29]))
        );
        assert!(parse_update("").is_err(),)
    }

    #[test]
    fn test_parse_updates() {
        assert_eq!(
            parse_updates("75,47,61,53,29\n97,61,53,29,13"),
            Ok(("", vec![vec![75, 47, 61, 53, 29], vec![97, 61, 53, 29, 13]]))
        );
        assert!(parse_update("").is_err(),)
    }

    #[test]
    fn test_parse_rules() {
        assert_eq!(
            parse_rules("47|53"),
            Ok((
                "",
                vec![Rule {
                    before: 47,
                    after: 53
                }]
            ))
        );
        assert!(parse_rules("").is_err());
        assert_eq!(
            parse_rules("47|53\n97|13"),
            Ok((
                "",
                vec![
                    Rule {
                        before: 47,
                        after: 53
                    },
                    Rule {
                        before: 97,
                        after: 13
                    }
                ]
            ))
        );
    }

    #[test]
    fn test_parse_input() {
        assert!(matches!(
            parse_input(
                advent_of_code::template::read_file("examples", DAY).as_str()
            ),
            Ok(("", _))
        ))
    }

    #[test]
    fn test_part_one() {
        let result =
            part_one(&advent_of_code::template::read_file("examples", DAY));
        assert_eq!(result, Some(143));
    }

    #[test]
    fn test_part_two() {
        let result =
            part_two(&advent_of_code::template::read_file("examples", DAY));
        assert_eq!(result, Some(123));
    }
}
