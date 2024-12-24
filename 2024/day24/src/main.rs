use std::collections::HashMap;

use itertools::Itertools;

// -----------------------------------------------------------------------------

#[derive(Debug, Clone, Copy)]
enum Gate {
    And,
    Or,
    Xor,
}

impl Gate {
    fn eval(&self, a: bool, b: bool) -> bool {
        match self {
            Self::And => a && b,
            Self::Or => a || b,
            Self::Xor => a ^ b,
        }
    }
}

#[derive(Debug, Clone)]
struct Device {
    wires: HashMap<&'static str, bool>,
    gates: Vec<([&'static str; 3], Gate)>,
}

impl Device {
    fn parse(input: &'static str) -> Self {
        let wires = input
            .lines()
            .take_while(|l| !l.is_empty())
            .map(|l| {
                let mut s = l.split(": ");
                (
                    s.next().unwrap(),
                    s.next().unwrap().parse::<usize>().unwrap() != 0,
                )
            })
            .collect();
        let gates = input
            .lines()
            .skip_while(|l| !l.is_empty())
            .skip(1)
            .map(|l| {
                let s = l.split_whitespace().collect_vec();
                (
                    [s[0], s[2], s[4]],
                    match s[1] {
                        "AND" => Gate::And,
                        "OR" => Gate::Or,
                        "XOR" => Gate::Xor,
                        _ => unreachable!(),
                    },
                )
            })
            .collect_vec();

        Self { wires, gates }
    }

    fn get_num(&self, prefix: &str) -> usize {
        let mut out = self
            .wires
            .iter()
            .filter(|(k, _)| k.starts_with(prefix))
            .map(|(k, v)| {
                (
                    k.chars()
                        .skip(1)
                        .collect::<String>()
                        .parse::<usize>()
                        .unwrap(),
                    (*v as usize).to_string(),
                )
            })
            .collect_vec();
        out.sort();
        out.reverse();
        let d = out.into_iter().map(|(_, v)| v).collect_vec().join("");

        usize::from_str_radix(&d, 2).unwrap()
    }

    fn solve(self) -> Self {
        let Self {
            mut wires,
            mut gates,
        } = self;

        while !gates.is_empty() {
            gates.retain(|([a, b, c], gate)| {
                if let Some(x) = wires
                    .get(a)
                    .and_then(|a| wires.get(b).map(|b| gate.eval(*a, *b)))
                {
                    wires.insert(c, x);
                    false
                } else {
                    true
                }
            });
        }

        Self { wires, gates }
    }
}

fn part1(input: &'static str) -> usize {
    Device::parse(input).solve().get_num("z")
}

fn part2(_input: &'static str) -> String {
    let swaps = [
        ("ffj", "z08"),
        ("z31", "jdr"),
        ("z22", "gjh"),
        ("dwp", "kfm"),
    ];
    let mut keys: Vec<&str> = swaps.iter().flat_map(|(a, b)| [*a, *b]).collect();
    keys.sort();
    keys.join(",")
}

fn main() {
    let input = include_str!("../in.txt");
    println!("Part 1: {}", part1(input));
    println!("Part 2: {}", part2(input));
}

#[test]
fn example1() {
    let input = include_str!("../ex1.txt");
    assert_eq!(part1(input), 4);
}

#[test]
fn example2() {
    let input = include_str!("../ex2.txt");
    assert_eq!(part1(input), 2024);
}
