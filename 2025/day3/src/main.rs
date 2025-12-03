#![feature(slice_as_array)]

use itertools::Itertools;

fn parse(input: &str) -> impl Iterator<Item = Vec<usize>> {
    input.lines().map(|l| {
        l.chars()
            .map(|c| c.to_digit(10).unwrap() as usize)
            .collect_vec()
    })
}

fn find_max<const N: usize>(bank: Vec<usize>) -> usize {
    let mut enabled = bank[..N].to_vec();

    let mut monotonic = vec![true; N];
    for ((i, x), (_, y)) in enabled.iter().enumerate().tuple_windows() {
        monotonic[i] = x >= y;
    }

    'main: for i in N..bank.len() {
        for j in 0..N - 1 {
            if !monotonic[j] {
                enabled.remove(j);
                enabled.push(bank[i]);
                monotonic.remove(j);
                monotonic.push(true);
                monotonic[N - 2] = enabled[N - 2] >= enabled[N - 1];
                if j > 0 {
                    monotonic[j - 1] = enabled[j - 1] >= enabled[j];
                }
                continue 'main;
            }
        }

        if bank[i] > *enabled.last().unwrap() {
            enabled[N - 1] = bank[i];
            monotonic[N - 2] = enabled[N - 2] >= enabled[N - 1];
        }
    }

    enabled
        .iter()
        .rev()
        .enumerate()
        .map(|(i, x)| x * 10usize.pow(i as u32))
        .sum()
}

fn part1(input: &str) -> usize {
    parse(input).map(find_max::<2>).sum()
}

fn part2(input: &str) -> usize {
    parse(input).map(find_max::<12>).sum()
}

fn main() {
    let input = include_str!("../in.txt");
    println!("Part 1: {}", part1(input));
    println!("Part 2: {}", part2(input));
}

#[test]
fn example1() {
    let input = include_str!("../ex1.txt");
    assert_eq!(part1(input), 357);
    assert_eq!(part2(input), 3121910778619);
}
