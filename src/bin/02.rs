use nom::{
    bytes::complete::tag, character::complete::i32, multi::separated_list1,
    IResult,
};

advent_of_code::solution!(2);

#[derive(Debug, PartialEq, Eq)]
struct Report {
    elements: Vec<i32>,
}

impl From<Vec<i32>> for Report {
    fn from(value: Vec<i32>) -> Self {
        Report { elements: value }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum Order {
    Desc,
    Asc,
}

impl From<(&i32, &i32)> for Order {
    fn from(value: (&i32, &i32)) -> Self {
        let (first, second) = value;
        if first > second {
            Order::Desc
        } else {
            Order::Asc
        }
    }
}

impl Report {
    fn is_safe(&self) -> bool {
        let mut previous: Option<&i32> = None;
        let mut order: Option<Order> = None;

        for current in self.elements.iter() {
            if let Some(previous) = previous {
                let new_order: Order = (previous, current).into();
                if order.map_or(false, |o| o != new_order)
                    || !(1..=3).contains(&(previous - current).abs())
                {
                    return false;
                }
                order = Some(new_order);
            }
            previous = Some(current);
        }

        true
    }

    fn is_safe_dampened(&self) -> bool {
        (0..self.elements.len()).any(|i| {
            let mut minus_one = self.elements.clone();
            minus_one.remove(i);
            Report {
                elements: minus_one,
            }
            .is_safe()
        })
    }
}

fn parse_report(input: &str) -> IResult<&str, Report> {
    separated_list1(tag(" "), i32)(input)
        .map(|(r, element)| (r, element.into()))
}

fn parse_reports(input: &str) -> IResult<&str, Vec<Report>> {
    separated_list1(tag("\n"), parse_report)(input)
}

pub fn part_one(input: &str) -> Option<u64> {
    Some(
        parse_reports(input)
            .map(|(_, reports)| {
                reports.iter().filter(|report| report.is_safe()).count()
            })
            .expect("parsing failed") as u64,
    )
}

pub fn part_two(input: &str) -> Option<u64> {
    Some(
        parse_reports(input)
            .map(|(_, reports)| {
                reports
                    .iter()
                    .filter(|report| {
                        report.is_safe() || report.is_safe_dampened()
                    })
                    .count()
            })
            .expect("parsing failed") as u64,
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_reports() {
        assert_eq!(
            parse_reports(
                advent_of_code::template::read_file("examples", DAY).as_str()
            ),
            Ok((
                "",
                vec![
                    Report {
                        elements: vec![7, 6, 4, 2, 1]
                    },
                    Report {
                        elements: vec![1, 2, 7, 8, 9]
                    },
                    Report {
                        elements: vec![9, 7, 6, 2, 1]
                    },
                    Report {
                        elements: vec![1, 3, 2, 4, 5]
                    },
                    Report {
                        elements: vec![8, 6, 4, 4, 1]
                    },
                    Report {
                        elements: vec![1, 3, 6, 7, 9]
                    },
                ]
            ))
        )
    }

    #[test]
    fn test_part_one() {
        let result =
            part_one(&advent_of_code::template::read_file("examples", DAY));
        assert_eq!(result, Some(2));
    }

    #[test]
    fn test_part_two() {
        let result =
            part_two(&advent_of_code::template::read_file("examples", DAY));
        assert_eq!(result, Some(4));
    }
}
