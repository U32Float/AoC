use std::cell::RefCell;
use std::collections::HashMap;

use itertools::Itertools;

// -----------------------------------------------------------------------------

fn parse(input: &'static str) -> (Vec<&str>, Vec<&str>) {
    let patterns = input.lines().next().unwrap().split(", ").collect_vec();
    let designs = input.lines().skip(2).collect();
    (patterns, designs)
}

fn solve(patterns: &[&'static str], design: &'static str) -> usize {
    thread_local! {
        static CACHE: RefCell<HashMap<&'static str, usize>> = RefCell::new(HashMap::new());
    }

    enum Cmd {
        Solve(&'static str),
        Add(&'static str, usize),
    }

    let mut cmd_stack = vec![Cmd::Solve(design)];
    let mut data_stack = vec![];

    while let Some(cmd) = cmd_stack.pop() {
        match cmd {
            Cmd::Solve(d) => {
                if let Some(n) = CACHE.with(|cache| cache.borrow().get(d).copied()) {
                    data_stack.push(n);
                    continue;
                }

                let mut n = 0;
                let mut cmds = vec![];
                for p in patterns {
                    if let Some(rest) = d.strip_prefix(p) {
                        n += 1;
                        if rest.is_empty() {
                            data_stack.push(1);
                        } else {
                            cmds.push(Cmd::Solve(rest));
                        }
                    }
                }

                if n == 0 {
                    data_stack.push(0);
                } else {
                    cmd_stack.push(Cmd::Add(d, n));
                    cmd_stack.extend(cmds);
                }
            }
            Cmd::Add(d, n) => {
                let mut x = 0;
                for _ in 0..n {
                    x += data_stack.pop().unwrap();
                }
                data_stack.push(x);
                CACHE.with(|cache| cache.borrow_mut().insert(d, x));
            }
        }
    }

    data_stack.pop().unwrap()
}

fn part1(input: &'static str) -> usize {
    let (patterns, designs) = parse(input);

    designs.iter().filter(|d| solve(&patterns, d) > 0).count()
}

fn part2(input: &'static str) -> usize {
    let (patterns, designs) = parse(input);

    designs.iter().map(|d| solve(&patterns, d)).sum()
}

fn main() {
    let input = include_str!("../in.txt");
    println!("Part 1: {}", part1(input));
    println!("Part 2: {}", part2(input));
}

#[test]
fn example1() {
    let input = include_str!("../ex1.txt");
    assert_eq!(part1(input), 6);
    assert_eq!(part2(input), 16);
}
