use core::f64;
use std::sync::LazyLock;

use minilp::{ComparisonOp, LinearExpr, OptimizationDirection, Problem};
use regex::Regex;

// -----------------------------------------------------------------------------

#[derive(Debug, Clone, Copy)]
struct Vec2 {
    x: usize,
    y: usize,
}

fn parse_btn(input: &str) -> Vec2 {
    static PAT: LazyLock<Regex> =
        LazyLock::new(|| Regex::new(r"Button \w: X\+(\d+), Y\+(\d+)").unwrap());

    let cs = PAT.captures(input).unwrap();
    Vec2 {
        x: cs.get(1).unwrap().as_str().parse().unwrap(),
        y: cs.get(2).unwrap().as_str().parse().unwrap(),
    }
}

fn parse_prize(input: &str) -> Vec2 {
    static PAT: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"Prize: X=(\d+), Y=(\d+)").unwrap());

    let cs = PAT.captures(input).unwrap();
    Vec2 {
        x: cs.get(1).unwrap().as_str().parse().unwrap(),
        y: cs.get(2).unwrap().as_str().parse().unwrap(),
    }
}

fn solve(a: Vec2, b: Vec2, prize: Vec2) -> usize {
    let mut problem = Problem::new(OptimizationDirection::Minimize);

    let x = problem.add_var(3., (0.0, f64::INFINITY));
    let y = problem.add_var(1., (0.0, f64::INFINITY));

    let expr = {
        let mut e = LinearExpr::empty();
        e.add(x, a.x as f64);
        e.add(y, b.x as f64);
        e
    };
    problem.add_constraint(expr, ComparisonOp::Eq, prize.x as f64);

    let expr = {
        let mut e = LinearExpr::empty();
        e.add(x, a.y as f64);
        e.add(y, b.y as f64);
        e
    };
    problem.add_constraint(expr, ComparisonOp::Eq, prize.y as f64);

    problem
        .solve()
        .map(|s| {
            let x = s[x].round() as usize;
            let y = s[y].round() as usize;
            let f1 = a.x * x + b.x * y == prize.x;
            let f2 = a.y * x + b.y * y == prize.y;
            if !(f1 && f2) {
                return 0;
            }
            3 * x + y
        })
        .unwrap_or(0)
}

fn solve_all(input: &str, offset: usize) -> usize {
    let mut lines = input.lines().peekable();

    let mut tokens = 0;
    while lines.peek().is_some() {
        let a = parse_btn(lines.next().unwrap());
        let b = parse_btn(lines.next().unwrap());
        let prize = parse_prize(lines.next().unwrap());
        tokens += solve(
            a,
            b,
            Vec2 {
                x: prize.x + offset,
                y: prize.y + offset,
            },
        );
        lines.next();
    }

    tokens
}

fn part1(input: &str) -> usize {
    solve_all(input, 0)
}

fn part2(input: &str) -> usize {
    solve_all(input, 10000000000000)
}

fn main() {
    let input = include_str!("../in.txt");
    println!("Part 1: {}", part1(input));
    println!("Part 2: {}", part2(input));
}

#[test]
fn example1() {
    let input = include_str!("../ex1.txt");
    assert_eq!(part1(input), 480);
}
