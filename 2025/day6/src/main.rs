use itertools::Itertools;

fn parse(input: &str) -> impl Iterator<Item = (Vec<usize>, String)> {
    let nums = input
        .lines()
        .tuple_windows()
        .map(|(l, _)| {
            l.split_whitespace()
                .map(|s| s.parse().unwrap())
                .collect_vec()
        })
        .collect_vec();
    let ops = input.lines().last().unwrap().split_whitespace().map_into();
    (0..nums[0].len())
        .map(move |i| nums.iter().map(|row| row[i]).collect_vec())
        .zip(ops)
}

fn part1(input: &str) -> usize {
    parse(input)
        .map(|(nums, op)| {
            if op == "*" {
                nums.iter().product::<usize>()
            } else {
                nums.iter().sum()
            }
        })
        .sum()
}

fn part2(input: &str) -> usize {
    let lines = input.lines().collect_vec();
    let n = lines.len();
    let mut col_widths = lines
        .last()
        .unwrap()
        .chars()
        .chunk_by(|c| *c == ' ')
        .into_iter()
        .filter(|(key, _)| *key)
        .map(|(_, items)| items.count())
        .collect_vec();
    *col_widths.last_mut().unwrap() += 1;

    let mut col = 0;
    let mut total = 0;
    while col < col_widths.len() {
        let offset = col_widths[..col].iter().map(|o| o + 1).sum();
        let product = lines[n - 1].chars().nth(offset).unwrap() == '*';
        let nums = (0..col_widths[col]).map(|i| {
            let x = (0..n - 1)
                .map(|j| lines[j].chars().nth(offset + i).unwrap())
                .join("");
            if x.trim().is_empty() {
                if product { 1 } else { 0 }
            } else {
                x.trim().parse::<usize>().unwrap()
            }
        });

        if product {
            total += nums.product::<usize>();
        } else {
            total += nums.sum::<usize>();
        }
        col += 1;
    }
    total
}

fn main() {
    let input = include_str!("../in.txt");
    println!("Part 1: {}", part1(input));
    println!("Part 2: {}", part2(input));
}

#[test]
fn example1() {
    let input = include_str!("../ex1.txt");
    assert_eq!(part1(input), 4277556);
    assert_eq!(part2(input), 3263827);
}
