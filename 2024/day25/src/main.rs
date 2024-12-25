#![feature(iter_array_chunks)]

use itertools::Itertools;

// -----------------------------------------------------------------------------

fn parse(input: &str) -> (Vec<[usize; 5]>, Vec<[usize; 5]>) {
    let mut locks = vec![];
    let mut keys = vec![];

    input
        .lines()
        .filter(|l| !l.is_empty())
        .array_chunks()
        .map(|c: [&str; 7]| {
            c.iter()
                .map(|l| l.chars().map(|c| c == '#').collect_vec())
                .collect_vec()
        })
        .for_each(|g| {
            let l = (0..5)
                .map(|x| (0..7).filter(|y| g[*y][x]).count() - 1)
                .collect_vec()
                .try_into()
                .unwrap();
            if g[0][0] {
                locks.push(l);
            } else {
                keys.push(l);
            }
        });

    (locks, keys)
}

fn part1(input: &str) -> usize {
    let (locks, keys) = parse(input);

    let mut t = 0;
    for lock in locks {
        for key in &keys {
            if !lock.iter().zip(key).any(|(l, k)| l + k > 5) {
                t += 1;
            }
        }
    }
    t
}

fn main() {
    let input = include_str!("../in.txt");
    println!("Part 1: {}", part1(input));
}

#[test]
fn example1() {
    let input = include_str!("../ex1.txt");
    assert_eq!(part1(input), 3);
}
