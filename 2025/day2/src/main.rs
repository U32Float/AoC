use itertools::Itertools;
use std::{collections::HashSet, ops::RangeInclusive};

fn find_partial_range(range: RangeInclusive<usize>, rep: usize) -> RangeInclusive<usize> {
    let mut start = *range.start();
    let mut n = start.ilog10() + 1;
    if n as usize % rep != 0 {
        start = 10usize.pow(((n / rep as u32) + 1) * rep as u32 - 1);
        n = start.ilog10() + 1;
    }
    let partial_start = start / 10usize.pow(n - n / rep as u32);

    let mut end = *range.end();
    let mut m = end.ilog10() + 1;
    if m as usize % rep != 0 {
        if m < rep as u32 {
            return 1..=0;
        }
        end = "9".repeat((m as usize / rep) * rep).parse().unwrap();
        m = end.ilog10() + 1;
    }
    let partial_end = end / 10usize.pow(m - m / rep as u32);

    partial_start..=partial_end
}

fn part1(input: &str) -> usize {
    fn invalid_ids(range: RangeInclusive<usize>) -> usize {
        let partial_range = find_partial_range(range.clone(), 2);

        let mut total = 0;
        for x in partial_range {
            let f = 10usize.pow(x.ilog10() + 1);
            let y = x * f + x;
            if range.contains(&y) {
                total += y;
            }
        }
        total
    }

    input
        .split(",")
        .map(|s| {
            let range = s
                .split("-")
                .map(|x| x.parse::<usize>().unwrap())
                .collect_vec();
            range[0]..=range[1]
        })
        .map(|range| invalid_ids(range))
        .sum()
}

fn part2(input: &str) -> usize {
    fn invalid_ids(range: RangeInclusive<usize>) -> usize {
        let mut invalid_ids = HashSet::new();
        for rep in 2..=range.end().ilog10() + 1 {
            let partial_range = find_partial_range(range.clone(), rep as usize);
            if partial_range.end() < partial_range.start() {
                continue;
            }
            for x in partial_range {
                let y = (0..rep)
                    .map(|i| x * 10usize.pow(i * (x.ilog10() + 1)))
                    .sum::<usize>();
                if range.contains(&y) {
                    invalid_ids.insert(y);
                }
            }
        }
        invalid_ids.iter().sum()
    }

    input
        .split(",")
        .map(|s| {
            let range = s
                .split("-")
                .map(|x| x.parse::<usize>().unwrap())
                .collect_vec();
            range[0]..=range[1]
        })
        .map(|range| invalid_ids(range))
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
    assert_eq!(part1(input), 1227775554);
    assert_eq!(part2(input), 4174379265);
}
