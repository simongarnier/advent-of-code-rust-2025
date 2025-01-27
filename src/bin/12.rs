use grid::Grid;
use nom::{
    branch::alt,
    character::complete::{anychar, line_ending},
    combinator::verify,
    multi::{many1, separated_list1},
    sequence::terminated,
    AsChar, IResult,
};
use std::{array::IntoIter, collections::HashSet, iter::Flatten};

advent_of_code::solution!(12);

fn neighbours(
    (row, col): &(usize, usize),
) -> Flatten<IntoIter<Option<(usize, usize)>, 4>> {
    [
        Some((row + 1, *col)),
        Some((*row, col + 1)),
        row.checked_sub(1).map(|r| (r, *col)),
        col.checked_sub(1).map(|c| (*row, c)),
    ]
    .into_iter()
    .flatten()
}

fn neighbours_in_plot(
    pos: &(usize, usize),
    plot: &Grid<char>,
) -> HashSet<(usize, usize)> {
    neighbours(pos)
        .filter(|pos| plot.get(pos.0, pos.1).is_some())
        .collect()
}

fn neighbours_in_group(
    pos: &(usize, usize),
    group: &HashSet<(usize, usize)>,
) -> HashSet<(usize, usize)> {
    neighbours(pos).filter(|pos| group.contains(pos)).collect()
}

fn group_by_crop(
    plot: &Grid<char>,
    pos: (usize, usize),
    target: char,
    visited: &mut HashSet<(usize, usize)>,
    group: &mut HashSet<(usize, usize)>,
) {
    let (row, col) = pos;
    if visited.contains(&pos) {
        return;
    }

    if plot.get(row, col) != Some(&target) {
        return;
    }

    visited.insert(pos);
    group.insert(pos);

    neighbours_in_plot(&pos, plot)
        .into_iter()
        .for_each(|new_pos| {
            group_by_crop(plot, new_pos, target, visited, group);
        });
}

fn get_area(group: &HashSet<(usize, usize)>) -> u64 {
    group.len() as u64
}

fn get_perimeter(group: &HashSet<(usize, usize)>) -> u64 {
    group.iter().fold(0usize, |acc, (row, col)| {
        acc + 4 - neighbours_in_group(&(*row, *col), group).len()
    }) as u64
}

pub fn part_one(input: &str) -> Option<u64> {
    let (_, plot) = parse_input(input).expect("parsing failed");
    let mut visited: HashSet<(usize, usize)> = HashSet::new();

    Some(
        plot.indexed_iter()
            .map(|(pos, area)| {
                if !visited.contains(&pos) {
                    let mut group: HashSet<(usize, usize)> = HashSet::new();
                    group_by_crop(&plot, pos, *area, &mut visited, &mut group);
                    get_area(&group) * get_perimeter(&group)
                } else {
                    0
                }
            })
            .sum(),
    )
}

pub fn part_two(input: &str) -> Option<u64> {
    None
}

fn parse_input(input: &str) -> IResult<&str, Grid<char>> {
    terminated(
        separated_list1(
            line_ending,
            many1(alt((verify(anychar, |c| c.is_alpha()),))),
        ),
        line_ending,
    )(input)
    .map(|(r, g)| (r, Grid::from(g)))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_input() {
        let str_input = &advent_of_code::template::read_file("examples", DAY);
        let result = parse_input(str_input);
        assert_eq!(
            result,
            Ok((
                "",
                Grid::from(vec![
                    vec!['R', 'R', 'R', 'R', 'I', 'I', 'C', 'C', 'F', 'F'],
                    vec!['R', 'R', 'R', 'R', 'I', 'I', 'C', 'C', 'C', 'F'],
                    vec!['V', 'V', 'R', 'R', 'R', 'C', 'C', 'F', 'F', 'F'],
                    vec!['V', 'V', 'R', 'C', 'C', 'C', 'J', 'F', 'F', 'F'],
                    vec!['V', 'V', 'V', 'V', 'C', 'J', 'J', 'C', 'F', 'E'],
                    vec!['V', 'V', 'I', 'V', 'C', 'C', 'J', 'J', 'E', 'E'],
                    vec!['V', 'V', 'I', 'I', 'I', 'C', 'J', 'J', 'E', 'E'],
                    vec!['M', 'I', 'I', 'I', 'I', 'I', 'J', 'J', 'E', 'E'],
                    vec!['M', 'I', 'I', 'I', 'S', 'I', 'J', 'E', 'E', 'E'],
                    vec!['M', 'M', 'M', 'I', 'S', 'S', 'J', 'E', 'E', 'E']
                ])
            ))
        );
    }

    #[test]
    fn test_part_one() {
        let result =
            part_one(&advent_of_code::template::read_file("examples", DAY));
        assert_eq!(result, Some(1930));
    }

    #[test]
    fn test_part_two() {
        let result =
            part_two(&advent_of_code::template::read_file("examples", DAY));
        assert_eq!(result, None);
    }
}
