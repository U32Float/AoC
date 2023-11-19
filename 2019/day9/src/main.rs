#![allow(dead_code)]

use std::{
    collections::{HashMap, VecDeque},
    i128,
};

use tap::TapOptional;

#[derive(Clone, Copy)]
enum Mode {
    Immediate,
    Position,
    Relative(u128),
}

impl Mode {
    fn get(self, value: i128, code: &HashMap<u128, i128>) -> i128 {
        match self {
            Mode::Immediate => value,
            Mode::Position => *code.get(&(value as u128)).unwrap_or(&0),
            Mode::Relative(base) => code[&((base as i128 + value) as u128)],
        }
    }
}

struct Amplifier {
    pc: u128,
    base: u128,
    stack: VecDeque<i128>,
    code: HashMap<u128, i128>,
}

impl Amplifier {
    fn new(code: Vec<i128>) -> Self {
        let mut map = HashMap::new();
        for (i, c) in code.into_iter().enumerate() {
            map.insert(i as u128, c);
        }
        Self {
            pc: 0,
            base: 0,
            stack: VecDeque::new(),
            code: map,
        }
    }

    fn apply(&mut self, input: Option<i128>) -> Option<i128> {
        input.tap_some(|i| self.stack.push_back(*i));

        loop {
            let code = &mut self.code;
            let pc = &mut self.pc;
            let opcode: i128 = code[pc];

            println!("{:?}", opcode);

            if opcode == 99 {
                return None;
            }

            let mut modes = [Mode::Position; 3];
            for i in 0..3i128 {
                let m = ((opcode - (opcode % 10i128.pow(2 + i as u32))) / 10i128.pow(2 + i as u32))
                    % 10;
                modes[i as usize] = match m {
                    0 => Mode::Position,
                    1 => Mode::Immediate,
                    2 => Mode::Relative(self.base),
                    _ => panic!(),
                }
            }

            match opcode % 10 {
                1 => {
                    #[rustfmt::skip] let x = modes[0].get(code[&(*pc + 1)], &code);
                    #[rustfmt::skip] let y = modes[1].get(code[&(*pc + 2)], &code);
                    #[rustfmt::skip] let addr = modes[2].get(code[&(*pc + 3)], &code);
                    code.insert(addr as u128, x + y);
                    *pc += 4
                }
                2 => {
                    #[rustfmt::skip] let x = modes[0].get(code[&(*pc + 1)], &code);
                    #[rustfmt::skip] let y = modes[1].get(code[&(*pc + 2)], &code);
                    #[rustfmt::skip] let addr = modes[2].get(code[&(*pc + 3)], &code);
                    code.insert(addr as u128, x * y);
                    *pc += 4
                }
                3 => {
                    #[rustfmt::skip] let addr = modes[0].get(code[&(*pc + 1)], &code);
                    let val = self.stack.pop_front().unwrap();
                    code.insert(addr as u128, val);
                    *pc += 2
                }
                4 => {
                    #[rustfmt::skip] let addr = modes[0].get(code[&(*pc + 1)], &code);
                    let ret = Some(*code.get(&(addr as u128)).unwrap_or(&0));
                    *pc += 2;
                    return ret;
                }
                5 => {
                    #[rustfmt::skip] let x = modes[0].get(code[&(*pc + 1)], &code);
                    #[rustfmt::skip] let y = modes[1].get(code[&(*pc + 2)], &code);
                    if x != 0 {
                        *pc = y as u128;
                    } else {
                        *pc += 3;
                    }
                }
                6 => {
                    #[rustfmt::skip] let x = modes[0].get(code[&(*pc + 1)], &code);
                    #[rustfmt::skip] let y = modes[1].get(code[&(*pc + 2)], &code);
                    if x == 0 {
                        *pc = y as u128;
                    } else {
                        *pc += 3;
                    }
                }
                7 => {
                    #[rustfmt::skip] let x = modes[0].get(code[&(*pc + 1)], &code);
                    #[rustfmt::skip] let y = modes[1].get(code[&(*pc + 2)], &code);
                    #[rustfmt::skip] let addr = code[&(*pc + 3)];
                    code.insert(addr as u128, (x < y) as i128);
                    *pc += 4;
                }
                8 => {
                    #[rustfmt::skip] let x = modes[0].get(code[&(*pc + 1)], &code);
                    #[rustfmt::skip] let y = modes[1].get(code[&(*pc + 2)], &code);
                    #[rustfmt::skip] let addr = code[&(*pc + 3)];
                    code.insert(addr as u128, (x == y) as i128);
                    *pc += 4;
                }
                9 => {
                    #[rustfmt::skip] let x = code[&(*pc + 1)];
                    self.base = (self.base as i128 + x) as u128;
                    *pc += 2;
                }
                _ => panic!("Incorrect opcode"),
            }
        }
    }
}

fn main() {
    #[cfg(debug_assertions)]
    let input = include_str!("../ex1.txt");
    #[cfg(debug_assertions)]
    let id = None;
    #[cfg(not(debug_assertions))]
    let input = include_str!("../in.txt");
    #[cfg(not(debug_assertions))]
    let id = Some(1);

    let code = input
        .split(",")
        .flat_map(|s| s.trim().parse::<i128>())
        .collect();

    let mut amplifier = Amplifier::new(code);

    let prev = id;
    let mut output = 0;
    while let Some(out) = amplifier.apply(prev) {
        println!("{:?}", out);
        output = out;
    }
    println!("Part 1: {:?}", output);
}
