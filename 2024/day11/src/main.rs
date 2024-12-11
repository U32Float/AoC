use std::collections::HashMap;

use smallvec::SmallVec;

// -----------------------------------------------------------------------------

fn parse(input: &str) -> Vec<usize> {
    input
        .split_whitespace()
        .map(|x| x.parse().unwrap())
        .collect()
}

fn blink(x: usize) -> SmallVec<[usize; 2]> {
    if x == 0 {
        SmallVec::from_slice(&[1])
    } else {
        let s = x.to_string();
        if s.len() % 2 == 0 {
            SmallVec::from_slice(&[
                s[0..s.len() / 2].parse().unwrap(),
                s[s.len() / 2..].parse().unwrap(),
            ])
        } else {
            SmallVec::from_slice(&[x * 2024])
        }
    }
}

fn blink_many(x: usize, n: usize) -> Vec<(usize, usize)> {
    let mut stones = vec![x];
    for _ in 0..n {
        stones = stones.into_iter().flat_map(blink).collect();
    }

    let mut counter = HashMap::new();
    for x in stones {
        *counter.entry(x).or_insert(0) += 1;
    }
    counter.into_iter().collect()
}

const I: usize = 25;

fn solve(input: &str, steps: usize) -> usize {
    let mut stones: HashMap<usize, usize> =
        parse(input).into_iter().zip(std::iter::repeat(1)).collect();

    let mut cache = HashMap::new();
    let mut i = 0;
    while i < steps {
        let mut new = HashMap::new();
        for (x, n) in &stones {
            for (y, m) in cache.entry(*x).or_insert_with(|| blink_many(*x, I)).clone() {
                *new.entry(y).or_insert(0) += n * m;
            }
        }

        stones = new;

        i += I;
    }

    stones.values().sum()
}

fn part1(input: &str) -> usize {
    solve(input, 25)
}

fn part2(input: &str) -> usize {
    solve(input, 75)
}

fn main() {
    let input = include_str!("../in.txt");
    println!("Part 1: {}", part1(input));
    println!("Part 2: {}", part2(input));
}

#[test]
fn example1() {
    let input = include_str!("../ex1.txt");
    assert_eq!(part1(input), 55312);
}
