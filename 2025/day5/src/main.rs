use itertools::Itertools;
use std::{collections::HashSet, ops::RangeInclusive};

fn parse(input: &str) -> (Vec<RangeInclusive<usize>>, Vec<usize>) {
    let fresh = input
        .lines()
        .take_while(|l| !l.is_empty())
        .map(|l| {
            let nums = l
                .split("-")
                .map(|s| s.parse::<usize>().unwrap())
                .collect_vec();
            nums[0]..=nums[1]
        })
        .collect_vec();
    let available = input
        .lines()
        .skip_while(|l| !l.is_empty())
        .skip(1)
        .map(|l| l.parse::<usize>().unwrap())
        .collect_vec();
    (fresh, available)
}

fn part1(input: &str) -> usize {
    let (fresh, available) = parse(input);
    available
        .iter()
        .filter(|ingr| fresh.iter().any(|range| range.contains(ingr)))
        .count()
}

fn join(a: RangeInclusive<usize>, b: RangeInclusive<usize>) -> Option<RangeInclusive<usize>> {
    if a.end() < b.start() || b.end() < a.start() {
        return None;
    }
    Some(*a.start().min(b.start())..=*a.end().max(b.end()))
}

fn part2(input: &str) -> usize {
    let (mut fresh, _) = parse(input);
    loop {
        let mut joined = HashSet::new();
        let mut tmp = Vec::new();
        'main: for i in 0..fresh.len() {
            if joined.contains(&i) {
                continue;
            }
            for j in i + 1..fresh.len() {
                if joined.contains(&j) {
                    continue;
                }
                if let Some(new) = join(fresh[i].clone(), fresh[j].clone()) {
                    joined.insert(j);
                    tmp.push(new);
                    continue 'main;
                }
            }
            tmp.push(fresh[i].clone());
        }
        if tmp.len() == fresh.len() {
            break;
        }
        fresh = std::mem::take(&mut tmp);
    }
    fresh.iter().map(|r| r.end() - r.start() + 1).sum()
}

fn main() {
    let input = include_str!("../in.txt");
    println!("Part 1: {}", part1(input));
    println!("Part 2: {}", part2(input));
}

#[test]
fn example1() {
    let input = include_str!("../ex1.txt");
    assert_eq!(part1(input), 3);
    assert_eq!(part2(input), 14);
}
