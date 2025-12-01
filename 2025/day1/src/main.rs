#![feature(int_roundings)]

fn part1(input: &str) -> usize {
    let (count, _) = input.lines().fold((0, 50), |(count, dial), rot| {
        let s = if rot.chars().next().unwrap() == 'L' {
            -1
        } else {
            1
        };
        let x: i32 = rot[1..].parse().unwrap();

        let dial = (((dial + (x * s)) % 100) + 100) % 100;
        (count + (dial == 0) as usize, dial)
    });

    count
}

fn part2(input: &str) -> usize {
    let (count, _) = input.lines().fold((0, 50), |(count, dial), rot| {
        let s = if rot.chars().next().unwrap() == 'L' {
            -1
        } else {
            1
        };
        let x: i32 = rot[1..].parse().unwrap();

        let new = (((dial + (x * s)) % 100) + 100) % 100;

        let r = (dial != 0 && ((s > 0 && new < dial) || (s < 0 && new > dial)) || new == 0)
            as usize
            + x.div_floor(100) as usize;
        (count + r, new)
    });

    count
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
    assert_eq!(part2(input), 6);
}
