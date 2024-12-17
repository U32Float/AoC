#![feature(int_roundings)]

use itertools::Itertools;

// -----------------------------------------------------------------------------

#[derive(Debug, Clone)]
struct Computer {
    reg_a: u64,
    reg_b: u64,
    reg_c: u64,

    pc: usize,
    program: Vec<u8>,
}

impl Computer {
    fn parse(input: &str) -> Self {
        let mut ls = input.lines();
        let reg_a = ls.next().unwrap()[12..].parse().unwrap();
        let reg_b = ls.next().unwrap()[12..].parse().unwrap();
        let reg_c = ls.next().unwrap()[12..].parse().unwrap();
        ls.next();
        let program = ls.next().unwrap()[9..]
            .split(",")
            .map(|x| x.parse().unwrap())
            .collect();
        Self {
            reg_a,
            reg_b,
            reg_c,
            pc: 0,
            program,
        }
    }

    fn combo(&self, op: u8) -> u64 {
        match op {
            x @ (0..=3) => x as u64,
            4 => self.reg_a,
            5 => self.reg_b,
            6 => self.reg_c,
            _ => panic!(),
        }
    }

    fn run(mut self) -> Vec<u8> {
        let mut output = vec![];
        while self.pc < self.program.len() {
            let op = self.program[self.pc + 1];
            match self.program[self.pc] {
                0 => self.reg_a = self.reg_a.div_floor(2u64.pow(self.combo(op) as u32)),
                1 => self.reg_b ^= op as u64,
                2 => self.reg_b = self.combo(op) % 8,
                3 => {
                    if self.reg_a != 0 {
                        self.pc = op as usize;
                        continue;
                    }
                }
                4 => self.reg_b ^= self.reg_c,
                5 => {
                    output.push((self.combo(op) % 8) as u8);
                }
                6 => self.reg_b = self.reg_a.div_floor(2u64.pow(self.combo(op) as u32)),
                7 => self.reg_c = self.reg_a.div_floor(2u64.pow(self.combo(op) as u32)),
                _ => panic!(),
            }
            self.pc += 2;
        }
        output
    }
}

fn part1(input: &str) -> String {
    Computer::parse(input)
        .run()
        .into_iter()
        .map(|x| x.to_string())
        .collect_vec()
        .join(",")
}

#[inline(always)]
fn concat(nums: &[u8]) -> u64 {
    nums.iter().fold(0, |acc, x| (acc << 3) + *x as u64)
}

fn part2(input: &str) -> u64 {
    let computer = Computer::parse(input);
    let n = computer.program.len();

    let mut best = u64::MAX;
    let mut stack = vec![vec![]];
    while let Some(nums) = stack.pop() {
        if nums.len() == n {
            best = best.min(concat(&nums));
            continue;
        }

        for x in 0..8 {
            let mut new_nums = nums.clone();
            new_nums.push(x);

            let a = concat(&new_nums);
            let b = (a % 8) ^ 2;
            let c = a >> b;
            let b = (b ^ 7) ^ c;
            let out = (b % 8) as u8;

            if out == computer.program[n - 1 - nums.len()] {
                stack.push(new_nums);
            }
        }
    }

    best
}

fn main() {
    let input = include_str!("../in.txt");
    println!("Part 1: {}", part1(input));
    println!("Part 2: {}", part2(input));
}

#[test]
fn example1() {
    let input = include_str!("../ex1.txt");
    assert_eq!(part1(input), "4,6,3,5,6,3,5,2,1,0");
}
