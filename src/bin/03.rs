use nom::{
    branch::alt,
    bytes::complete::{tag, take},
    character::complete::i32,
    combinator::map,
    multi::{many1, many_till},
    sequence::{delimited, separated_pair},
    IResult,
};

advent_of_code::solution!(3);

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
struct Mul {
    lhs: i32,
    rhs: i32,
}

#[derive(Debug, PartialEq, Eq)]
enum Instruction {
    Mult { lhs: i32, rhs: i32 },
    Do,
    DoNot,
}
use Instruction::*;

impl From<(i32, i32)> for Mul {
    fn from(value: (i32, i32)) -> Self {
        Mul {
            lhs: value.0,
            rhs: value.1,
        }
    }
}

impl From<(i32, i32)> for Instruction {
    fn from(value: (i32, i32)) -> Self {
        Mult {
            lhs: value.0,
            rhs: value.1,
        }
    }
}

fn parse_mul<T>(input: &str) -> IResult<&str, T>
where
    T: From<(i32, i32)>,
{
    delimited(tag("mul("), separated_pair(i32, tag(","), i32), tag(")"))(input)
        .map(|(rest, result)| (rest, result.into()))
}

fn parse_do(input: &str) -> IResult<&str, Instruction> {
    tag("do()")(input).map(|(rest, _)| (rest, Do))
}

fn parse_do_not(input: &str) -> IResult<&str, Instruction> {
    tag("don't()")(input).map(|(rest, _)| (rest, DoNot))
}

fn parse_instruction(input: &str) -> IResult<&str, Instruction> {
    alt((parse_do, parse_do_not, parse_mul))(input)
}

trait Computable<T> {
    fn compute(self) -> T;
}

impl Computable<i32> for Mul {
    fn compute(self) -> i32 {
        self.lhs * self.rhs
    }
}

impl Computable<i32> for Vec<Mul> {
    fn compute(self) -> i32 {
        self.iter().map(|mul| mul.compute()).sum()
    }
}

#[derive(Debug, PartialEq, Eq)]
enum Stack {
    EmptyOpen,
    EmptyClosed,
    Open(i32),
    Closed(i32),
}
use Stack::*;

impl Stack {
    fn apply(&self, inst: Instruction) -> Self {
        match (self, inst) {
            (EmptyOpen, Mult { lhs, rhs }) => Open(lhs * rhs),
            (EmptyOpen, DoNot) => EmptyClosed,
            (EmptyOpen, _) => EmptyOpen,
            (EmptyClosed, Do) => EmptyOpen,
            (EmptyClosed, _) => EmptyClosed,
            (Open(acc), DoNot) => Closed(*acc),
            (Open(acc), Mult { lhs, rhs }) => Open(*acc + (lhs * rhs)),
            (Open(acc), _) => Open(*acc),
            (Closed(acc), Do) => Open(*acc),
            (Closed(acc), _) => Closed(*acc),
        }
    }
}

impl From<Stack> for Option<u64> {
    fn from(value: Stack) -> Self {
        match value {
            Open(acc) => Some(acc),
            Closed(acc) => Some(acc),
            _ => None,
        }
        .map(|acc| acc as u64)
    }
}

impl Computable<Stack> for Vec<Instruction> {
    fn compute(self) -> Stack {
        self.into_iter()
            .fold(EmptyOpen, |stack, inst| stack.apply(inst))
    }
}

fn parse_input1(input: &str) -> IResult<&str, Vec<Mul>> {
    many1(map(many_till(take(1_usize), parse_mul), |(_, result)| {
        result
    }))(input)
}

fn parse_input2(input: &str) -> IResult<&str, Vec<Instruction>> {
    many1(map(
        many_till(take(1_usize), parse_instruction),
        |(_, result)| result,
    ))(input)
}

pub fn part_one(input: &str) -> Option<u64> {
    parse_input1(input)
        .map(|(_, muls)| muls.compute() as u64)
        .ok()
}

pub fn part_two(input: &str) -> Option<u64> {
    parse_input2(input)
        .ok()
        .and_then(|(_, insts)| insts.compute().into())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_mul() {
        assert_eq!(parse_mul("mul(1,2)"), Ok(("", Mul { lhs: 1, rhs: 2 })))
    }

    #[test]
    fn test_parse_input1_simple() {
        assert_eq!(
            parse_input1("mul(1,2)"),
            Ok(("", vec![Mul { lhs: 1, rhs: 2 }]))
        )
    }

    #[test]
    fn test_parse_input1_delimited() {
        assert_eq!(
            parse_input1("mul(1,2)simonmul(3,4)"),
            Ok(("", vec![Mul { lhs: 1, rhs: 2 }, Mul { lhs: 3, rhs: 4 }]))
        )
    }

    #[test]
    fn test_parse_input1_example() {
        assert_eq!(
            parse_input1(
                advent_of_code::template::read_file("examples", DAY).as_str()
            ),
            Ok((
                ")\n",
                vec![
                    Mul { lhs: 2, rhs: 4 },
                    Mul { lhs: 5, rhs: 5 },
                    Mul { lhs: 11, rhs: 8 },
                    Mul { lhs: 8, rhs: 5 }
                ]
            ))
        )
    }

    #[test]
    fn test_parse_input2_example() {
        assert_eq!(
            parse_input2(
                advent_of_code::template::read_file("examples", DAY).as_str()
            ),
            Ok((
                ")\n",
                vec![
                    Mult { lhs: 2, rhs: 4 },
                    Mult { lhs: 5, rhs: 5 },
                    Mult { lhs: 11, rhs: 8 },
                    Mult { lhs: 8, rhs: 5 }
                ]
            ))
        );
        assert_eq!(
            parse_input2(
                "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"
            ),
            Ok((
                ")",
                vec![
                    Mult { lhs: 2, rhs: 4 },
                    DoNot,
                    Mult { lhs: 5, rhs: 5 },
                    Mult { lhs: 11, rhs: 8 },
                    Do,
                    Mult { lhs: 8, rhs: 5 }
                ]
            ))
        )
    }

    #[test]
    fn test_compute_part_two() {
        assert_eq!(
            parse_input2(
                "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"
            ).map(|(_, insts)| insts.compute()),
            Ok(
                Open(48)
            )
        );
    }
    #[test]
    fn test_part_one() {
        let result =
            part_one(&advent_of_code::template::read_file("examples", DAY));
        assert_eq!(result, Some(161));
    }

    #[test]
    fn test_part_two() {
        let result =
            part_two(&advent_of_code::template::read_file("examples", DAY));
        assert_eq!(result, Some(161));
    }
}
