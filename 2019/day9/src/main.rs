use std::collections::HashMap;
use Mode::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Mode {
    Position(i128),
    Immediate(i128),
    Relative(i128),
}

struct Computer {
    heap: HashMap<i128, i128>,
    pc: i128,
    rb: i128,
}

impl Computer {
    fn parse(input: &str) -> Self {
        Self {
            heap: input
                .trim()
                .split(',')
                .map(|s| s.parse().unwrap())
                .enumerate()
                .map(|(i, s)| (i as i128, s))
                .collect::<HashMap<_, _>>(),
            pc: 0,
            rb: 0,
        }
    }

    fn execute(self, input: &[i128]) -> Vec<i128> {
        let mut output = vec![];

        let mut input = input.to_vec();
        input.reverse();

        let Self {
            mut heap,
            mut pc,
            mut rb,
        } = self;

        macro_rules! heap {
            ($p: expr) => {
                match $p {
                    Position(i) => *heap.get(&i).unwrap_or(&0),
                    Immediate(i) => i,
                    Relative(i) => *heap.get(&(i + rb)).unwrap_or(&0),
                }
            };
        }
        macro_rules! heap_mut {
            ($p: expr) => {
                match $p {
                    Position(i) => heap.entry(i).or_insert(0),
                    Relative(i) => heap.entry(i + rb).or_insert(0),
                    Immediate(_) => panic!(),
                }
            };
        }

        loop {
            let opcode = heap[&pc];

            if opcode == 99 {
                return output;
            }

            let mut ps = [Mode::Position(0); 3];
            for i in 0..3 {
                let v = heap![Position(pc + i as i128 + 1)];
                ps[i] = match ((opcode - (opcode % 10i128.pow(2 + i as u32)))
                    / 10i128.pow(2 + i as u32))
                    % 10
                {
                    0 => Position(v),
                    1 => Immediate(v),
                    2 => Relative(v),
                    _ => panic!(),
                }
            }

            match opcode % 10 {
                1 => {
                    *heap_mut![ps[2]] = heap![ps[0]] + heap![ps[1]];
                    pc += 4;
                }
                2 => {
                    *heap_mut![ps[2]] = heap![ps[0]] * heap![ps[1]];
                    pc += 4;
                }
                3 => {
                    *heap_mut![ps[0]] = input.pop().unwrap();
                    pc += 2
                }
                4 => {
                    output.push(heap![ps[0]]);
                    pc += 2
                }
                5 => {
                    if heap![ps[0]] != 0 {
                        pc = heap![ps[1]];
                    } else {
                        pc += 3;
                    }
                }
                6 => {
                    if heap![ps[0]] == 0 {
                        pc = heap![ps[1]];
                    } else {
                        pc += 3;
                    }
                }
                7 => {
                    *heap_mut![ps[2]] = (heap![ps[0]] < heap![ps[1]]) as i128;
                    pc += 4;
                }
                8 => {
                    *heap_mut![ps[2]] = (heap![ps[0]] == heap![ps[1]]) as i128;
                    pc += 4;
                }
                9 => {
                    rb += heap![ps[0]];
                    pc += 2;
                }
                _ => panic!("Incorrect opcode"),
            }
        }
    }
}

fn solve(input: &str, instr: i128) -> usize {
    let computer = Computer::parse(input);
    computer.execute(&[instr]).pop().unwrap() as usize
}

fn part1(input: &str) -> usize {
    solve(input, 1)
}

fn part2(input: &str) -> usize {
    solve(input, 2)
}

fn main() {
    let input = include_str!("../in.txt");
    println!("Part 1: {}", part1(input));
    println!("Part 2: {}", part2(input));
}

#[test]
fn example1() {
    let input = include_str!("../ex1.txt");
    assert_eq!(part1(input), 1125899906842624);
}

#[test]
fn example2() {
    let input = include_str!("../ex2.txt");
    assert_eq!(part1(input), 99);
}
