use cached::proc_macro::cached;
use Condition::*;

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
enum Condition {
    Operational,
    Damaged,
    Unknown,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
struct Record {
    conditions: Vec<Condition>,
    groups: Vec<usize>,
}

impl Record {
    fn extend(self) -> Self {
        Self {
            conditions: {
                let mut cs = self.conditions;
                cs.push(Operational);
                cs
            },
            groups: self.groups,
        }
    }

    fn unfold(self) -> Self {
        let mut conditions = self.conditions.clone();
        let mut groups = self.groups.clone();

        for _ in 0..4 {
            conditions.push(Unknown);
            conditions.append(&mut self.conditions.clone());

            groups.append(&mut self.groups.clone());
        }

        Self { conditions, groups }
    }
}

fn solve_record(input: &'static Record) -> usize {
    enum Status {
        Final,
        Valid,
        Invalid,
    }

    #[derive(Hash, Clone, PartialEq, Eq)]
    struct State {
        record: &'static Record,
        c_idx: usize,
        g_idx: usize,
        size: usize,
    }

    impl State {
        pub fn status(&self) -> Status {
            if self.g_idx == self.record.groups.len()
                && self.c_idx == self.record.conditions.len()
                && self.size == 0
            {
                Status::Final
            } else if self.c_idx < self.record.conditions.len()
                && (self.g_idx < self.record.groups.len()
                    || (self.g_idx == self.record.groups.len() && self.size == 0))
            {
                Status::Valid
            } else {
                Status::Invalid
            }
        }
    }

    #[cached]
    pub fn count(state: State) -> usize {
        let mut total = 0;

        match state.status() {
            Status::Final => return 1,
            Status::Invalid => return 0,
            _ => (),
        }

        let cs = if state.record.conditions[state.c_idx] == Unknown {
            vec![Operational, Damaged]
        } else {
            vec![state.record.conditions[state.c_idx]]
        };

        for c in cs {
            match c {
                Operational => {
                    if state.size == 0 {
                        total += count(State {
                            record: state.record,
                            c_idx: state.c_idx + 1,
                            g_idx: state.g_idx,
                            size: 0,
                        });
                    } else if state.size == state.record.groups[state.g_idx] {
                        total += count(State {
                            record: state.record,
                            c_idx: state.c_idx + 1,
                            g_idx: state.g_idx + 1,
                            size: 0,
                        });
                    }
                }
                Damaged => {
                    total += count(State {
                        record: state.record,
                        c_idx: state.c_idx + 1,
                        g_idx: state.g_idx,
                        size: state.size + 1,
                    });
                }
                _ => panic!(),
            }
        }

        total
    }

    count(State {
        record: input,
        c_idx: 0,
        g_idx: 0,
        size: 0,
    })
}

fn part1(input: Vec<Record>) -> usize {
    let input = input
        .into_iter()
        .map(|r| r.extend())
        .collect::<Vec<_>>()
        .leak();

    input.iter().map(solve_record).sum()
}

fn part2(input: Vec<Record>) -> usize {
    let input = input
        .iter()
        .map(|r| r.clone().unfold().extend())
        .collect::<Vec<_>>()
        .leak();

    input.iter().map(solve_record).sum()
}

fn solve(input: &str) -> (usize, usize) {
    let lines = input.lines();

    let records = lines
        .map(|line| {
            let (cs, gs) = line.split_once(' ').unwrap();
            let conditions = cs
                .chars()
                .map(|char| match char {
                    '.' => Operational,
                    '#' => Damaged,
                    '?' => Unknown,
                    _ => panic!("Invalid char: {}", char),
                })
                .collect::<Vec<Condition>>();
            let groups = gs.split(',').map(|char| char.parse().unwrap()).collect();

            Record { conditions, groups }
        })
        .collect::<Vec<_>>();

    let p1 = part1(records.clone());
    println!("Part 1: {}", p1);

    let p2 = part2(records);
    println!("Part 2: {}", p2);

    (p1, p2)
}

fn main() {
    let input = include_str!("../in.txt");
    solve(input);
}

#[test]
fn example1() {
    let input = include_str!("../ex1.txt");
    assert_eq!(solve(input), (21, 525152));
}
