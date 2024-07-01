use Direction::*;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
enum Direction {
    Right,
    Down,
    Left,
    Up,
}

#[derive(Clone, Copy, Debug)]
struct Instruction {
    dir: Direction,
    n: i128,
}

impl Instruction {
    fn parse_naive(input: &str) -> Self {
        let split = input.split_whitespace().collect::<Vec<_>>();
        let dir = match split[0].chars().nth(0).unwrap() {
            'R' => Right,
            'D' => Down,
            'L' => Left,
            'U' => Up,
            _ => panic!(),
        };
        let n = split[1].parse::<u128>().unwrap();

        Self { dir, n: n as i128 }
    }

    fn parse(input: &str) -> Self {
        let split = input.split_whitespace().collect::<Vec<_>>();

        let n = u128::from_str_radix(&split[2][2..7], 16).unwrap() as i128;
        let dir = match u128::from_str_radix(&split[2][7..8], 16).unwrap() {
            0 => Right,
            1 => Down,
            2 => Left,
            3 => Up,
            _ => panic!(),
        };

        Self { dir, n }
    }
}

fn parse_instructions_naive(input: &str) -> &'static [Instruction] {
    input
        .lines()
        .map(|line| Instruction::parse_naive(line))
        .collect::<Vec<_>>()
        .leak()
}

fn parse_instructions(input: &str) -> &'static [Instruction] {
    input
        .lines()
        .map(|line| Instruction::parse(line))
        .collect::<Vec<_>>()
        .leak()
}

fn solve(dig_plan: &[Instruction]) -> i128 {
    let mut pos = 0;
    let mut area = 1f64;
    for instr in dig_plan {
        let Instruction { dir, n } = instr;

        let (x, y) = match dir {
            Right => (*n, 0),
            Down => (0, *n),
            Left => (-*n, 0),
            Up => (0, -*n),
        };

        pos += x;
        area += y as f64 * pos as f64 + *n as f64 / 2.;
    }
    area as i128
}

fn part1(input: &str) -> i128 {
    let dig_plan = parse_instructions_naive(input);

    solve(dig_plan)
}

fn part2(input: &str) -> i128 {
    let dig_plan = parse_instructions(input);

    solve(dig_plan)
}

fn main() {
    let input = include_str!("../in.txt");

    let p1 = part1(input);
    println!("Part 1: {}", p1);

    let p2 = part2(input);
    println!("Part 2: {}", p2);
}

#[test]
fn example1() {
    let input = include_str!("../ex1.txt");
    assert_eq!(part1(input), 62);
    assert_eq!(part2(input), 952408144115);
}
