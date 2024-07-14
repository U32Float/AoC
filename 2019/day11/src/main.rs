use std::collections::{HashMap, VecDeque};
use Color::*;
use Direction::*;
use Mode::*;
use Rotation::*;

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
    input: VecDeque<i128>,
}

impl Computer {
    fn parse(input_str: &str, mut input: Vec<i128>) -> Self {
        input.reverse();
        Self {
            input: input.into(),
            heap: input_str
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

    fn write(&mut self, input: i128) {
        self.input.push_back(input);
    }
}

impl Iterator for Computer {
    type Item = i128;

    fn next(&mut self) -> Option<Self::Item> {
        let Self {
            heap,
            pc,
            rb,
            input,
        } = self;

        macro_rules! heap {
            ($p: expr) => {
                match $p {
                    Position(i) => *heap.get(&i).unwrap_or(&0),
                    Immediate(i) => i,
                    Relative(i) => *heap.get(&(i + *rb)).unwrap_or(&0),
                }
            };
        }
        macro_rules! heap_mut {
            ($p: expr) => {
                match $p {
                    Position(i) => heap.entry(i).or_insert(0),
                    Relative(i) => heap.entry(i + *rb).or_insert(0),
                    Immediate(_) => panic!(),
                }
            };
        }

        loop {
            let opcode = heap[&pc];

            if opcode == 99 {
                return None;
            }

            let mut ps = [Mode::Position(0); 3];
            for i in 0..3 {
                let v = heap![Position(*pc + i as i128 + 1)];
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
                    *pc += 4;
                }
                2 => {
                    *heap_mut![ps[2]] = heap![ps[0]] * heap![ps[1]];
                    *pc += 4;
                }
                3 => {
                    *heap_mut![ps[0]] = input.pop_front().unwrap();
                    *pc += 2
                }
                4 => {
                    *pc += 2;
                    return Some(heap![ps[0]]);
                }
                5 => {
                    if heap![ps[0]] != 0 {
                        *pc = heap![ps[1]];
                    } else {
                        *pc += 3;
                    }
                }
                6 => {
                    if heap![ps[0]] == 0 {
                        *pc = heap![ps[1]];
                    } else {
                        *pc += 3;
                    }
                }
                7 => {
                    *heap_mut![ps[2]] = (heap![ps[0]] < heap![ps[1]]) as i128;
                    *pc += 4;
                }
                8 => {
                    *heap_mut![ps[2]] = (heap![ps[0]] == heap![ps[1]]) as i128;
                    *pc += 4;
                }
                9 => {
                    *rb += heap![ps[0]];
                    *pc += 2;
                }
                _ => panic!("Incorrect opcode"),
            }
        }
    }
}

#[derive(Clone, Copy)]
enum Direction {
    North,
    East,
    South,
    West,
}

impl Direction {
    fn rotate(self, rotation: Rotation) -> Self {
        match (self, rotation) {
            (North, Left) => West,
            (North, Right) => East,
            (East, Left) => North,
            (East, Right) => South,
            (South, Left) => East,
            (South, Right) => West,
            (West, Left) => South,
            (West, Right) => North,
        }
    }
}

enum Rotation {
    Left,
    Right,
}

#[derive(Clone, Copy)]
enum Color {
    Black,
    White,
}

struct Instruction {
    color: Color,
    rotation: Rotation,
}

fn get_instruction(computer: &mut Computer, color: Color) -> Option<Instruction> {
    let input = match color {
        Black => 0,
        White => 1,
    };
    computer.write(input);
    let color = match computer.next()? {
        0 => Black,
        1 => White,
        _ => panic!(),
    };
    let rotation = match computer.next()? {
        0 => Left,
        1 => Right,
        _ => panic!(),
    };
    Some(Instruction { color, rotation })
}

fn part1(input: &str) -> usize {
    let mut computer = Computer::parse(input, vec![]);

    let mut panels = HashMap::new();
    let mut dir = North;
    let (mut y, mut x) = (0, 0);
    let mut color = Black;

    while let Some(instr) = get_instruction(&mut computer, color) {
        let Instruction { color: c, rotation } = instr;

        panels.insert((y, x), c);

        dir = dir.rotate(rotation);
        match dir {
            North => y -= 1,
            East => x += 1,
            South => y += 1,
            West => x -= 1,
        }

        color = *panels.get(&(y, x)).unwrap_or(&Black);
    }

    panels.len()
}

fn part2(input: &str) -> usize {
    let mut computer = Computer::parse(input, vec![]);

    let mut panels = HashMap::new();
    let mut dir = North;
    let (mut y, mut x) = (0, 0);
    let mut color = White;

    while let Some(instr) = get_instruction(&mut computer, color) {
        let Instruction { color: c, rotation } = instr;

        panels.insert((y, x), c);

        dir = dir.rotate(rotation);
        match dir {
            North => y -= 1,
            East => x += 1,
            South => y += 1,
            West => x -= 1,
        }

        color = *panels.get(&(y, x)).unwrap_or(&Black);
    }

    let (min_y, min_x, max_y, max_x) = panels
        .keys()
        .fold((i32::MAX, i32::MAX, 0, 0), |acc, (y, x)| {
            (acc.0.min(*y), acc.1.min(*x), acc.2.max(*y), acc.3.max(*x))
        });

    let (h, w) = (max_y - min_y + 1, max_x - min_x + 1);

    let mut grid = vec![vec![Black; w as usize]; h as usize];

    for y in 0..h {
        for x in 0..w {
            grid[y as usize][x as usize] = *panels.get(&(y + min_y, x + min_x)).unwrap_or(&Black);
        }
    }

    grid.into_iter().for_each(|row| {
        println!(
            "{}",
            row.into_iter()
                .map(|c| match c {
                    Black => '.',
                    White => '#',
                })
                .collect::<String>()
        )
    });

    0
}

fn main() {
    let input = include_str!("../in.txt");
    println!("Part 1: {}", part1(input));
    println!("Part 2: {}", part2(input));
}
