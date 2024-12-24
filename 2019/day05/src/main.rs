fn solve(id: isize, mut code: Vec<isize>) -> isize {
    let mut pc = 0;
    let mut stack = vec![id];

    loop {
        let opcode = code[pc];

        if opcode == 99 {
            return stack.pop().unwrap();
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
                #[rustfmt::skip] let x = if modes[0] { code[pc + 1] } else { code[code[pc + 1] as usize] };
                #[rustfmt::skip] let y = if modes[1] { code[pc + 2] } else { code[code[pc + 2] as usize] };
                #[rustfmt::skip] let addr = code[pc + 3];
                code[addr as usize] = x + y;
                pc += 4
            }
            2 => {
                #[rustfmt::skip] let x = if modes[0] { code[pc + 1] } else { code[code[pc + 1] as usize] };
                #[rustfmt::skip] let y = if modes[1] { code[pc + 2] } else { code[code[pc + 2] as usize] };
                #[rustfmt::skip] let addr = code[pc + 3];
                code[addr as usize] = x * y;
                pc += 4
            }
            3 => {
                #[rustfmt::skip] let addr = code[pc + 1];
                code[addr as usize] = stack.pop().unwrap();
                pc += 2
            }
            4 => {
                #[rustfmt::skip] let addr = code[pc + 1];
                stack.push(code[addr as usize]);
                pc += 2
            }
            5 => {
                #[rustfmt::skip] let x = if modes[0] { code[pc + 1] } else { code[code[pc + 1] as usize] };
                #[rustfmt::skip] let y = if modes[1] { code[pc + 2] } else { code[code[pc + 2] as usize] };
                if x != 0 {
                    pc = y as usize;
                } else {
                    pc += 3;
                }
            }
            6 => {
                #[rustfmt::skip] let x = if modes[0] { code[pc + 1] } else { code[code[pc + 1] as usize] };
                #[rustfmt::skip] let y = if modes[1] { code[pc + 2] } else { code[code[pc + 2] as usize] };
                if x == 0 {
                    pc = y as usize;
                } else {
                    pc += 3;
                }
            }
            7 => {
                #[rustfmt::skip] let x = if modes[0] { code[pc + 1] } else { code[code[pc + 1] as usize] };
                #[rustfmt::skip] let y = if modes[1] { code[pc + 2] } else { code[code[pc + 2] as usize] };
                #[rustfmt::skip] let addr = code[pc + 3];
                code[addr as usize] = (x < y) as isize;
                pc += 4;
            }
            8 => {
                #[rustfmt::skip] let x = if modes[0] { code[pc + 1] } else { code[code[pc + 1] as usize] };
                #[rustfmt::skip] let y = if modes[1] { code[pc + 2] } else { code[code[pc + 2] as usize] };
                #[rustfmt::skip] let addr = code[pc + 3];
                code[addr as usize] = (x == y) as isize;
                pc += 4;
            }
            _ => panic!("Incorrect opcode"),
        }
    }
}

fn main() {
    let input = include_str!("../in.txt");

    let code: Vec<isize> = input
        .split(",")
        .map(|s| s.trim().parse().unwrap())
        .collect();

    let p1 = solve(1, code.clone());
    println!("Part 1: {}", p1);
    let p2 = solve(5, code.clone());
    println!("Part 2: {}", p2);
}
