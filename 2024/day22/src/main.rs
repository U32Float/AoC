#![feature(int_roundings)]

use std::collections::{HashMap, HashSet, VecDeque};

fn parse(input: &str) -> Vec<usize> {
    input.lines().map(|line| line.parse().unwrap()).collect()
}

fn gen(x: usize) -> usize {
    const M: usize = 16777216;
    let x = ((x * 64) ^ x) % M;
    let x = (x.div_floor(32) ^ x) % M;
    ((x * 2048) ^ x) % M
}

fn part1(input: &str) -> usize {
    parse(input)
        .into_iter()
        .map(|x| {
            let mut x = x;
            for _ in 0..2000 {
                x = gen(x);
            }
            x
        })
        .sum()
}

fn part2(input: &str) -> usize {
    let xs = parse(input);

    let mut sequences = HashMap::new();
    for mut x in xs {
        let mut tmp = HashSet::new();
        let mut p = (x % 10) as i32;
        let mut seq = VecDeque::new();
        for _ in 0..2000 {
            x = gen(x);
            let q = (x % 10) as i32;
            seq.push_back(q - p);
            p = q;
            if seq.len() == 5 {
                seq.pop_front();
                if !tmp.contains(&seq) {
                    tmp.insert(seq.clone());
                    sequences
                        .entry(seq.clone())
                        .or_insert(Vec::new())
                        .push(q as usize);
                }
            }
        }
    }

    sequences
        .values()
        .map(|ps| ps.iter().sum::<usize>())
        .max()
        .unwrap()
}

fn main() {
    let input = include_str!("../in.txt");
    println!("Part 1: {}", part1(input));
    println!("Part 2: {}", part2(input));
}

#[test]
fn example1() {
    let input = include_str!("../ex1.txt");
    assert_eq!(part1(input), 37327623);
}

#[test]
fn example2() {
    let input = include_str!("../ex2.txt");
    assert_eq!(part2(input), 23);
}
