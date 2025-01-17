use itertools::Itertools;
use nom::{
    character::complete::{anychar, line_ending},
    combinator::{map_res, verify},
    multi::many1,
    sequence::terminated,
    IResult,
};

advent_of_code::solution!(9);

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
struct SingleDigit(u8);

#[derive(Debug, PartialEq, Eq)]
struct DiskMap(Vec<SingleDigit>);

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum Block {
    Empty,
    Populated(u64),
}

#[derive(Debug, PartialEq, Eq)]
struct Disk(Vec<Block>);

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
struct File {
    id: u64,
    len: usize,
    start: usize,
}

impl From<DiskMap> for Disk {
    fn from(value: DiskMap) -> Self {
        let (block_file_vec, _): (Vec<Block>, u64) = value
            .0
            .iter()
            .enumerate()
            .fold((Vec::new(), 0), |(mut acc, id), (i, digit)| {
                if i % 2 == 0 {
                    (0..digit.0).for_each(|_| acc.push(Block::Populated(id)));
                    (acc, id + 1)
                } else {
                    (0..digit.0).for_each(|_| acc.push(Block::Empty));
                    (acc, id)
                }
            });

        Disk(block_file_vec)
    }
}

impl Disk {
    fn defrag_disk(&mut self) {
        for i in 0..self.0.len() {
            if matches!(self.0[i], Block::Empty) {
                if let Some(last_populated_block_position) = self
                    .0
                    .iter()
                    .rposition(|b| matches!(b, Block::Populated(_)))
                {
                    if last_populated_block_position > i {
                        self.0.swap(i, last_populated_block_position);
                    }
                }
            }
        }
    }

    fn get_files_sorted(&self) -> Vec<File> {
        let files: Vec<File> = self.0.iter().enumerate().fold(
            Vec::new(),
            |mut acc, (start, block)| {
                if let Block::Populated(id) = block {
                    match acc.last_mut() {
                        Some(last_file) if last_file.id == *id => {
                            last_file.len += 1;
                        }
                        _ => {
                            acc.push(File {
                                id: *id,
                                len: 1,
                                start,
                            });
                        }
                    }
                }
                acc
            },
        );

        files.into_iter().sorted_by_key(|f| f.id).rev().collect()
    }

    fn defrag_disk_whole_file(&mut self) {
        self.get_files_sorted().iter().for_each(|file| {
            for i in 0..self.0.len() {
                if i < file.start
                    && (i..(i + file.len))
                        .all(|i| matches!(self.0.get(i), Some(Block::Empty)))
                {
                    (i..(i + file.len))
                        .for_each(|ii| self.0[ii] = Block::Populated(file.id));
                    (file.start..(file.start + file.len))
                        .for_each(|ii| self.0[ii] = Block::Empty);

                    break;
                }
            }
        });
    }

    fn compute_checksum(&self) -> u64 {
        self.0
            .iter()
            .enumerate()
            .map(|(i, b)| match b {
                Block::Empty => 0,
                Block::Populated(id) => *id * i as u64,
            })
            .sum()
    }
}

impl SingleDigit {
    fn new(value: char) -> Result<Self, &'static str> {
        if let Some(valid) = value
            .to_digit(10)
            .filter(|d| *d <= 9)
            .and_then(|d| u8::try_from(d).ok())
        {
            Ok(SingleDigit(valid))
        } else {
            Err("invalid single digit")
        }
    }
}

fn parse_input(input: &str) -> IResult<&str, DiskMap> {
    terminated(
        many1(map_res(
            verify(anychar, |c| c.is_numeric()),
            SingleDigit::new,
        )),
        line_ending,
    )(input)
    .map(|(r, vec)| (r, DiskMap(vec)))
}

pub fn part_one(input: &str) -> Option<u64> {
    let (_, disk_map) = parse_input(input).expect("parsing failed");

    let mut block_file: Disk = disk_map.into();

    block_file.defrag_disk();

    Some(block_file.compute_checksum())
}

pub fn part_two(input: &str) -> Option<u64> {
    let (_, disk_map) = parse_input(input).expect("parsing failed");

    let mut block_file: Disk = disk_map.into();

    block_file.defrag_disk_whole_file();

    Some(block_file.compute_checksum())
}

#[cfg(test)]
mod tests {
    use super::*;
    use Block::*;

    #[test]
    fn test_parse_input() {
        assert_eq!(
            parse_input(&advent_of_code::template::read_file("examples", DAY)),
            Ok((
                "",
                DiskMap(vec![
                    SingleDigit(2),
                    SingleDigit(3),
                    SingleDigit(3),
                    SingleDigit(3),
                    SingleDigit(1),
                    SingleDigit(3),
                    SingleDigit(3),
                    SingleDigit(1),
                    SingleDigit(2),
                    SingleDigit(1),
                    SingleDigit(4),
                    SingleDigit(1),
                    SingleDigit(4),
                    SingleDigit(1),
                    SingleDigit(3),
                    SingleDigit(1),
                    SingleDigit(4),
                    SingleDigit(0),
                    SingleDigit(2)
                ])
            ))
        )
    }

    #[test]
    fn test_from_disk_map() {
        assert_eq!(
            parse_input(&advent_of_code::template::read_file("examples", DAY))
                .map(|(_, dm)| dm.into()),
            Ok(Disk(vec![
                Populated(0),
                Populated(0),
                Empty,
                Empty,
                Empty,
                Populated(1),
                Populated(1),
                Populated(1),
                Empty,
                Empty,
                Empty,
                Populated(2),
                Empty,
                Empty,
                Empty,
                Populated(3),
                Populated(3),
                Populated(3),
                Empty,
                Populated(4),
                Populated(4),
                Empty,
                Populated(5),
                Populated(5),
                Populated(5),
                Populated(5),
                Empty,
                Populated(6),
                Populated(6),
                Populated(6),
                Populated(6),
                Empty,
                Populated(7),
                Populated(7),
                Populated(7),
                Empty,
                Populated(8),
                Populated(8),
                Populated(8),
                Populated(8),
                Populated(9),
                Populated(9)
            ]))
        )
    }

    #[test]
    fn test_defrag_disk() {
        let mut disk = Disk(vec![
            Populated(0),
            Populated(0),
            Empty,
            Empty,
            Empty,
            Populated(1),
            Populated(1),
            Populated(1),
            Empty,
            Empty,
            Empty,
            Populated(2),
            Empty,
            Empty,
            Empty,
            Populated(3),
            Populated(3),
            Populated(3),
            Empty,
            Populated(4),
            Populated(4),
            Empty,
            Populated(5),
            Populated(5),
            Populated(5),
            Populated(5),
            Empty,
            Populated(6),
            Populated(6),
            Populated(6),
            Populated(6),
            Empty,
            Populated(7),
            Populated(7),
            Populated(7),
            Empty,
            Populated(8),
            Populated(8),
            Populated(8),
            Populated(8),
            Populated(9),
            Populated(9),
        ]);

        disk.defrag_disk();

        assert_eq!(
            disk,
            Disk(vec![
                Populated(0),
                Populated(0),
                Populated(9),
                Populated(9),
                Populated(8),
                Populated(1),
                Populated(1),
                Populated(1),
                Populated(8),
                Populated(8),
                Populated(8),
                Populated(2),
                Populated(7),
                Populated(7),
                Populated(7),
                Populated(3),
                Populated(3),
                Populated(3),
                Populated(6),
                Populated(4),
                Populated(4),
                Populated(6),
                Populated(5),
                Populated(5),
                Populated(5),
                Populated(5),
                Populated(6),
                Populated(6),
                Empty,
                Empty,
                Empty,
                Empty,
                Empty,
                Empty,
                Empty,
                Empty,
                Empty,
                Empty,
                Empty,
                Empty,
                Empty,
                Empty,
            ])
        )
    }

    #[test]
    fn test_get_files_sorted() {
        let disk = Disk(vec![
            Populated(0),
            Populated(0),
            Empty,
            Empty,
            Empty,
            Populated(1),
            Populated(1),
            Populated(1),
            Empty,
            Empty,
            Empty,
            Populated(2),
            Empty,
            Empty,
            Empty,
            Populated(3),
            Populated(3),
            Populated(3),
            Empty,
            Populated(4),
            Populated(4),
            Empty,
            Populated(5),
            Populated(5),
            Populated(5),
            Populated(5),
            Empty,
            Populated(6),
            Populated(6),
            Populated(6),
            Populated(6),
            Empty,
            Populated(7),
            Populated(7),
            Populated(7),
            Empty,
            Populated(8),
            Populated(8),
            Populated(8),
            Populated(8),
            Populated(9),
            Populated(9),
        ]);

        assert_eq!(
            disk.get_files_sorted(),
            vec![
                File {
                    id: 9,
                    len: 2,
                    start: 40
                },
                File {
                    id: 8,
                    len: 4,
                    start: 36
                },
                File {
                    id: 7,
                    len: 3,
                    start: 32
                },
                File {
                    id: 6,
                    len: 4,
                    start: 27
                },
                File {
                    id: 5,
                    len: 4,
                    start: 22
                },
                File {
                    id: 4,
                    len: 2,
                    start: 19
                },
                File {
                    id: 3,
                    len: 3,
                    start: 15
                },
                File {
                    id: 2,
                    len: 1,
                    start: 11
                },
                File {
                    id: 1,
                    len: 3,
                    start: 5
                },
                File {
                    id: 0,
                    len: 2,
                    start: 0
                },
            ]
        );
    }

    #[test]
    fn test_defrag_disk_whole_file() {
        let mut disk = Disk(vec![
            Populated(0),
            Populated(0),
            Empty,
            Empty,
            Empty,
            Populated(1),
            Populated(1),
            Populated(1),
            Empty,
            Empty,
            Empty,
            Populated(2),
            Empty,
            Empty,
            Empty,
            Populated(3),
            Populated(3),
            Populated(3),
            Empty,
            Populated(4),
            Populated(4),
            Empty,
            Populated(5),
            Populated(5),
            Populated(5),
            Populated(5),
            Empty,
            Populated(6),
            Populated(6),
            Populated(6),
            Populated(6),
            Empty,
            Populated(7),
            Populated(7),
            Populated(7),
            Empty,
            Populated(8),
            Populated(8),
            Populated(8),
            Populated(8),
            Populated(9),
            Populated(9),
        ]);

        disk.defrag_disk_whole_file();

        assert_eq!(
            disk,
            Disk(vec![
                Populated(0),
                Populated(0),
                Populated(9),
                Populated(9),
                Populated(2),
                Populated(1),
                Populated(1),
                Populated(1),
                Populated(7),
                Populated(7),
                Populated(7),
                Empty,
                Populated(4),
                Populated(4),
                Empty,
                Populated(3),
                Populated(3),
                Populated(3),
                Empty,
                Empty,
                Empty,
                Empty,
                Populated(5),
                Populated(5),
                Populated(5),
                Populated(5),
                Empty,
                Populated(6),
                Populated(6),
                Populated(6),
                Populated(6),
                Empty,
                Empty,
                Empty,
                Empty,
                Empty,
                Populated(8),
                Populated(8),
                Populated(8),
                Populated(8),
                Empty,
                Empty,
            ])
        )
    }

    #[test]
    fn test_part_one() {
        let result =
            part_one(&advent_of_code::template::read_file("examples", DAY));
        assert_eq!(result, Some(1928));
    }

    #[test]
    fn test_part_two() {
        let result =
            part_two(&advent_of_code::template::read_file("examples", DAY));
        assert_eq!(result, Some(2858));
    }
}
