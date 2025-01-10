use nom::{
    branch::alt,
    character::complete::{char, line_ending},
    multi::{many1, separated_list1},
    IResult, Parser,
};
use strum::IntoEnumIterator;
use strum_macros::EnumIter;

advent_of_code::solution!(4);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Xmas {
    X,
    M,
    A,
    S,
}
use Xmas::*;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum XmasError {
    Invalid,
    AlreadyCompleted,
}
use XmasError::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum XmasNext {
    Current(Xmas),
    Completed,
}
use XmasNext::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq, EnumIter)]
enum Direction {
    N,
    S,
    E,
    W,
    NE,
    NW,
    SE,
    SW,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Grid(Vec<Vec<Xmas>>);

struct GridDirectionIterator<'a> {
    grid: &'a Grid,
    direction: Direction,
    coordinate: (usize, usize),
    step_count: usize,
}

impl XmasNext {
    fn next(&self, next: Xmas) -> Result<XmasNext, XmasError> {
        match (self, next) {
            (Current(X), M) => Ok(Current(M)),
            (Current(M), A) => Ok(Current(A)),
            (Current(A), S) => Ok(Completed),
            (Completed, _) => Err(AlreadyCompleted),
            _ => Err(Invalid),
        }
    }
}

impl Xmas {
    fn start(&self) -> Result<XmasNext, XmasError> {
        match self {
            X => Ok(Current(X)),
            _ => Err(Invalid),
        }
    }
}

impl Direction {
    fn shift(&self, (x, y): (usize, usize)) -> Option<(usize, usize)> {
        match self {
            Direction::N => y.checked_sub(1).map(|y| (x, y)),
            Direction::S => Some((x, y + 1)),
            Direction::E => x.checked_sub(1).map(|x| (x, y)),
            Direction::W => Some((x + 1, y)),
            Direction::NE => Direction::E
                .shift((x, y))
                .and_then(|c| Direction::N.shift(c)),
            Direction::NW => Direction::W
                .shift((x, y))
                .and_then(|c| Direction::N.shift(c)),
            Direction::SW => Direction::W
                .shift((x, y))
                .and_then(|c| Direction::S.shift(c)),
            Direction::SE => Direction::E
                .shift((x, y))
                .and_then(|c| Direction::S.shift(c)),
        }
    }
}

impl Grid {
    fn get(&self, (x, y): (usize, usize)) -> Option<&Xmas> {
        self.0.get(y)?.get(x)
    }

    fn get_adjacent(
        &self,
        coordinate: (usize, usize),
        direction: Direction,
    ) -> Option<&Xmas> {
        direction.shift(coordinate).and_then(|c| self.get(c))
    }

    fn iter_in_direction(
        &self,
        coordinate: (usize, usize),
        direction: Direction,
    ) -> GridDirectionIterator {
        GridDirectionIterator {
            grid: self,
            coordinate,
            direction,
            step_count: 0,
        }
    }

    fn is_valid_x_dash_mas(&self, coordinate: (usize, usize)) -> bool {
        if !matches!(self.get(coordinate), Some(A)) {
            return false;
        }

        matches!(
            (
                self.get_adjacent(coordinate, Direction::NE),
                self.get_adjacent(coordinate, Direction::SW),
            ),
            (Some(S), Some(M)) | (Some(M), Some(S))
        ) && matches!(
            (
                self.get_adjacent(coordinate, Direction::NW),
                self.get_adjacent(coordinate, Direction::SE),
            ),
            (Some(S), Some(M)) | (Some(M), Some(S))
        )
    }
}

impl<'a> Iterator for GridDirectionIterator<'a> {
    type Item = &'a Xmas;

    fn next(&mut self) -> Option<Self::Item> {
        match (self.step_count, self.direction.shift(self.coordinate)) {
            (0..3, Some(new_coordinate)) => {
                self.step_count += 1;
                self.coordinate = new_coordinate;
                self.grid.get(new_coordinate)
            }
            _ => None,
        }
    }
}

fn parse_row(input: &str) -> IResult<&str, Vec<Xmas>> {
    many1(alt((
        char('X').map(|_| X),
        char('M').map(|_| M),
        char('A').map(|_| A),
        char('S').map(|_| S),
    )))(input)
}

fn parse_grid(input: &str) -> IResult<&str, Grid> {
    separated_list1(line_ending, parse_row)(input)
        .map(|(r, grid)| (r, Grid(grid)))
}

pub fn part_one(input: &str) -> Option<u64> {
    let (_, grid) = parse_grid(input).expect("parsing failed");

    let result = grid.0.iter().enumerate().fold(0, |row_acc, (y, row)| {
        row.iter().enumerate().fold(row_acc, |cell_acc, (x, cell)| {
            let start = cell.start();
            match start {
                Err(_) => cell_acc,
                Ok(_) => {
                    cell_acc
                        + Direction::iter()
                            .filter(|direction| {
                                grid.iter_in_direction((x, y), *direction)
                                    .fold(start, |result, xmas| match result {
                                        Ok(xmas_next) => xmas_next.next(*xmas),
                                        _ => result,
                                    })
                                    .map(|xmas_next| {
                                        matches!(xmas_next, Completed)
                                    })
                                    .unwrap_or(false)
                            })
                            .count()
                }
            }
        })
    });
    Some(result as u64)
}

pub fn part_two(input: &str) -> Option<u64> {
    let (_, grid) = parse_grid(input).expect("parsing failed");
    let result = grid.0.iter().enumerate().fold(0, |row_acc, (y, row)| {
        (0..row.len()).fold(row_acc, |cell_acc, x| {
            if grid.is_valid_x_dash_mas((x, y)) {
                cell_acc + 1
            } else {
                cell_acc
            }
        })
    });
    Some(result as u64)
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_parse_row() {
        assert_eq!(
            parse_row("MMMSXXMASM"),
            Ok(("", vec![M, M, M, S, X, X, M, A, S, M]))
        )
    }

    #[test]
    fn test_parse_grid() {
        assert_eq!(
            parse_grid(
                advent_of_code::template::read_file("examples", DAY).as_str()
            ),
            Ok((
                "",
                Grid(vec![
                    vec![M, M, M, S, X, X, M, A, S, M],
                    vec![M, S, A, M, X, M, S, M, S, A],
                    vec![A, M, X, S, X, M, A, A, M, M],
                    vec![M, S, A, M, A, S, M, S, M, X],
                    vec![X, M, A, S, A, M, X, A, M, M],
                    vec![X, X, A, M, M, X, X, A, M, A],
                    vec![S, M, S, M, S, A, S, X, S, S],
                    vec![S, A, X, A, M, A, S, A, A, A],
                    vec![M, A, M, M, M, X, M, M, M, M],
                    vec![M, X, M, X, A, X, M, A, S, X]
                ])
            ))
        )
    }

    #[test]
    fn test_xmas_next() {
        let first = X.start();
        let others = [M, A, S];

        assert_eq!(first, Ok(Current(X)));

        assert_eq!(
            others.into_iter().fold(first, |acc, xmas| match acc {
                Ok(nextable) => nextable.next(xmas),
                _ => acc,
            }),
            Ok(Completed)
        )
    }

    #[test]
    fn test_part_one() {
        let result =
            part_one(&advent_of_code::template::read_file("examples", DAY));
        assert_eq!(result, Some(18));
    }

    #[test]
    fn test_part_two() {
        let result =
            part_two(&advent_of_code::template::read_file("examples", DAY));
        assert_eq!(result, Some(9));
    }
}
