use std::collections::VecDeque;

use itertools::Itertools;

// ----------------------------------------------------------------------------

struct Amplifier {
    pc: usize,
    stack: VecDeque<isize>,
    code: Vec<isize>,
}

impl Amplifier {
    fn new(phase: isize, code: Vec<isize>) -> Self {
        Self {
            pc: 0,
            stack: vec![phase].into(),
            code,
        }
    }

    fn apply(&mut self, input: isize) -> Option<isize> {
        self.stack.push_back(input);

        loop {
            let code = &mut self.code;
            let pc = &mut self.pc;
            let opcode = code[*pc];

            if opcode == 99 {
                return None;
            }

            let mut modes = [false; 2];
            for i in 0..2 {
                modes[i] = (((opcode - (opcode % 10isize.pow(2 + i as u32)))
                    / 10isize.pow(2 + i as u32))
                    % 10)
                    == 1;
            }

            match opcode % 10 {
                1 => {
                    #[rustfmt::skip] let x = if modes[0] { code[*pc + 1] } else { code[code[*pc + 1] as usize] };
                    #[rustfmt::skip] let y = if modes[1] { code[*pc + 2] } else { code[code[*pc + 2] as usize] };
                    #[rustfmt::skip] let addr = code[*pc + 3];
                    code[addr as usize] = x + y;
                    *pc += 4
                }
                2 => {
                    #[rustfmt::skip] let x = if modes[0] { code[*pc + 1] } else { code[code[*pc + 1] as usize] };
                    #[rustfmt::skip] let y = if modes[1] { code[*pc + 2] } else { code[code[*pc + 2] as usize] };
                    #[rustfmt::skip] let addr = code[*pc + 3];
                    code[addr as usize] = x * y;
                    *pc += 4
                }
                3 => {
                    #[rustfmt::skip] let addr = code[*pc + 1];
                    code[addr as usize] = self.stack.pop_front().unwrap();
                    *pc += 2
                }
                4 => {
                    #[rustfmt::skip] let addr = code[*pc + 1];
                    let ret = Some(code[addr as usize]);
                    *pc += 2;
                    return ret;
                }
                5 => {
                    #[rustfmt::skip] let x = if modes[0] { code[*pc + 1] } else { code[code[*pc + 1] as usize] };
                    #[rustfmt::skip] let y = if modes[1] { code[*pc + 2] } else { code[code[*pc + 2] as usize] };
                    if x != 0 {
                        *pc = y as usize;
                    } else {
                        *pc += 3;
                    }
                }
                6 => {
                    #[rustfmt::skip] let x = if modes[0] { code[*pc + 1] } else { code[code[*pc + 1] as usize] };
                    #[rustfmt::skip] let y = if modes[1] { code[*pc + 2] } else { code[code[*pc + 2] as usize] };
                    if x == 0 {
                        *pc = y as usize;
                    } else {
                        *pc += 3;
                    }
                }
                7 => {
                    #[rustfmt::skip] let x = if modes[0] { code[*pc + 1] } else { code[code[*pc + 1] as usize] };
                    #[rustfmt::skip] let y = if modes[1] { code[*pc + 2] } else { code[code[*pc + 2] as usize] };
                    #[rustfmt::skip] let addr = code[*pc + 3];
                    code[addr as usize] = (x < y) as isize;
                    *pc += 4;
                }
                8 => {
                    #[rustfmt::skip] let x = if modes[0] { code[*pc + 1] } else { code[code[*pc + 1] as usize] };
                    #[rustfmt::skip] let y = if modes[1] { code[*pc + 2] } else { code[code[*pc + 2] as usize] };
                    #[rustfmt::skip] let addr = code[*pc + 3];
                    code[addr as usize] = (x == y) as isize;
                    *pc += 4;
                }
                _ => panic!("Incorrect opcode"),
            }
        }
    }
}

fn main() {
    #[cfg(debug_assertions)]
    let input = include_str!("../ex1.txt");
    #[cfg(not(debug_assertions))]
    let input = include_str!("../in.txt");
    let program: Vec<isize> = input
        .split(",")
        .map(|s| s.trim().parse::<isize>().unwrap())
        .collect();

    let mut max = 0;
    for p in (0..5isize).permutations(5).unique() {
        let mut prev = 0;
        for i in 0..5 {
            let mut ampl = Amplifier::new(p[i], program.clone());
            prev = ampl.apply(prev).unwrap();
        }
        if prev > max {
            max = prev;
        }
    }
    println!("Part 1: {:?}", max);

    let mut max = 0;
    for p in (5..10isize).permutations(5).unique() {
        let mut amplifiers = vec![];
        for i in 0..5 {
            amplifiers.push(Amplifier::new(p[i], program.clone()));
        }

        let mut prev = 0;
        let mut i = 0;
        loop {
            let out = amplifiers[i % 5].apply(prev);
            if out.is_none() {
                break;
            }
            prev = out.unwrap();
            i += 1;
        }
        if prev > max {
            max = prev;
        }
    }
    println!("Part 2: {:?}", max);
}
