use std::cell::RefCell;
use std::collections::HashMap;
use std::sync::LazyLock;

use itertools::Itertools;

// -----------------------------------------------------------------------------

fn parse(input: &'static str) -> Vec<&'static str> {
    input.lines().collect()
}

type ButtonList = Vec<char>;

fn find_moves<const W: usize, const H: usize>(
    keypad: [[char; W]; H],
) -> HashMap<(char, char), Vec<ButtonList>> {
    let mut moves = HashMap::new();
    for (y, x) in (0..H).cartesian_product(0..W) {
        for (v, u) in (0..H).cartesian_product(0..W) {
            let btn1 = keypad[y][x];
            let btn2 = keypad[v][u];
            if (y, x) == (v, u) {
                moves.insert((btn1, btn2), vec![]);
                continue;
            }
            if keypad[y][x] == ' ' || keypad[v][u] == ' ' {
                continue;
            }

            let mut tmp = Vec::new();
            let mut stack = vec![(y, x, vec![])];
            while let Some((y, x, moves)) = stack.pop() {
                if keypad[y][x] == ' ' {
                    continue;
                }
                if (y, x) == (v, u) {
                    let mut new_moves = moves.clone();
                    new_moves.push('A');
                    tmp.push(new_moves);
                    continue;
                }
                if y < v {
                    let mut new_moves = moves.clone();
                    new_moves.push('v');
                    stack.push((y + 1, x, new_moves));
                }
                if y > v {
                    let mut new_moves = moves.clone();
                    new_moves.push('^');
                    stack.push((y - 1, x, new_moves));
                }
                if x < u {
                    let mut new_moves = moves.clone();
                    new_moves.push('>');
                    stack.push((y, x + 1, new_moves));
                }
                if x > u {
                    let mut new_moves = moves.clone();
                    new_moves.push('<');
                    stack.push((y, x - 1, new_moves));
                }
            }
            moves.insert((btn1, btn2), tmp);
        }
    }
    moves
}

fn numerical_moves() -> HashMap<(char, char), Vec<ButtonList>> {
    let keypad = [
        ['7', '8', '9'],
        ['4', '5', '6'],
        ['1', '2', '3'],
        [' ', '0', 'A'],
    ];
    find_moves(keypad)
}

fn directional_moves() -> HashMap<(char, char), Vec<ButtonList>> {
    let keypad = [[' ', '^', 'A'], ['<', 'v', '>']];
    find_moves(keypad)
}

fn get_moves(button: char, dir_ptr: char, n: usize) -> u128 {
    static DIR_MOVES: LazyLock<HashMap<(char, char), Vec<ButtonList>>> =
        LazyLock::new(directional_moves);

    thread_local! {
        pub static CACHE: RefCell<HashMap<(char, char, usize), u128>> = RefCell::new(HashMap::new());
    }

    #[derive(Debug)]
    enum Cmd {
        Solve(char, char, usize),
        Fold(usize),
        Store(char, char, usize, usize),
    }

    let mut cmd_stack = vec![Cmd::Solve(button, dir_ptr, n)];
    let mut data_stack = vec![];

    while let Some(cmd) = cmd_stack.pop() {
        match cmd {
            Cmd::Solve(btn, ptr, n) => {
                if let Some(x) = CACHE.with(|c| c.borrow().get(&(btn, ptr, n)).cloned()) {
                    data_stack.push(x);
                    continue;
                }

                let moves = DIR_MOVES.get(&(ptr, btn)).unwrap();
                if moves.is_empty() {
                    data_stack.push(1);
                    continue;
                }

                cmd_stack.push(Cmd::Store(btn, ptr, n, moves.len()));

                for ms in moves {
                    if n == 1 {
                        data_stack.push(ms.len() as u128);
                        continue;
                    }

                    cmd_stack.push(Cmd::Fold(ms.len()));

                    let mut ptr_next = 'A';
                    for m in ms {
                        cmd_stack.push(Cmd::Solve(*m, ptr_next, n - 1));
                        ptr_next = *m;
                    }
                }
            }
            Cmd::Fold(n) => {
                let mut sum = 0;
                for _ in 0..n {
                    sum += data_stack.pop().unwrap();
                }
                data_stack.push(sum);
            }
            Cmd::Store(btn, ptr, n, m) => {
                let mut x = data_stack.pop().unwrap();
                for _ in 1..m {
                    x = x.min(data_stack.pop().unwrap());
                }
                data_stack.push(x);
                CACHE.with(|c| c.borrow_mut().insert((btn, ptr, n), x));
            }
        }
    }

    data_stack.pop().unwrap()
}

fn solve(code: &str, n: usize) -> u128 {
    let num_moves = numerical_moves();

    let mut best = u128::MAX;
    let mut stack = vec![(code.chars().collect_vec(), 'A', vec![])];

    while let Some((code, num_ptr, moves)) = stack.pop() {
        if code.is_empty() {
            let mut total = 0;
            let mut dir_ptr = 'A';
            for m in moves {
                total += get_moves(m, dir_ptr, n);
                dir_ptr = m;
            }
            best = best.min(total);
            continue;
        }

        for m in &num_moves[&(num_ptr, code[0])] {
            let mut new_moves = moves.clone();
            new_moves.extend(m);
            stack.push((code[1..].to_vec(), code[0], new_moves));
        }
    }

    best
}

fn part1(input: &'static str) -> u128 {
    parse(input)
        .iter()
        .map(|c| solve(c, 2) * c[..c.len() - 1].parse::<u128>().unwrap())
        .sum()
}

fn part2(input: &'static str) -> u128 {
    parse(input)
        .iter()
        .map(|c| solve(c, 25) * c[..c.len() - 1].parse::<u128>().unwrap())
        .sum()
}

fn main() {
    let input = include_str!("../in.txt");
    println!("Part 1: {}", part1(input));
    println!("Part 2: {}", part2(input));
}

#[test]
fn example1() {
    let input = include_str!("../ex1.txt");
    assert_eq!(part1(input), 126384);
}
