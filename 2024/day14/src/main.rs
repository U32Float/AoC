#![feature(int_roundings)]

use regex::Regex;

// -----------------------------------------------------------------------------

#[derive(Debug)]
struct Robot {
    x: i32,
    y: i32,
    vx: i32,
    vy: i32,
}

impl Robot {
    fn predict(&self, t: i32, (w, h): (i32, i32)) -> (i32, i32) {
        let (u, v) = (self.x + self.vx * t, self.y + self.vy * t);
        let u = if u < 0 {
            u.abs().div_ceil(w) * w + u
        } else {
            u % w
        };
        let v = if v < 0 {
            v.abs().div_ceil(h) * h + v
        } else {
            v % h
        };
        (u, v)
    }
}

fn parse(input: &str) -> Vec<Robot> {
    let mut robots = Vec::new();
    let pat = Regex::new(r"p=(\d+),(\d+) v=(-?\d+),(-?\d+)").unwrap();
    for line in input.lines() {
        let cs = pat.captures(line).unwrap();
        robots.push(Robot {
            x: cs.get(1).unwrap().as_str().parse().unwrap(),
            y: cs.get(2).unwrap().as_str().parse().unwrap(),
            vx: cs.get(3).unwrap().as_str().parse().unwrap(),
            vy: cs.get(4).unwrap().as_str().parse().unwrap(),
        });
    }
    robots
}

fn part1(input: &str, (w, h): (i32, i32)) -> usize {
    let robots = parse(input);
    robots
        .iter()
        .map(|r| r.predict(100, (w, h)))
        .fold([0; 4], |mut acc, (x, y)| {
            match (x.cmp(&(w / 2)), y.cmp(&(h / 2))) {
                (std::cmp::Ordering::Less, std::cmp::Ordering::Less) => acc[0] += 1,
                (std::cmp::Ordering::Greater, std::cmp::Ordering::Less) => acc[1] += 1,
                (std::cmp::Ordering::Less, std::cmp::Ordering::Greater) => acc[2] += 1,
                (std::cmp::Ordering::Greater, std::cmp::Ordering::Greater) => acc[3] += 1,
                _ => (),
            }
            acc
        })
        .iter()
        .product()
}

fn print(robots: &[(i32, i32)], (w, h): (i32, i32)) {
    let mut grid = vec![vec!['.'; w as usize]; h as usize];
    for (x, y) in robots {
        grid[*y as usize][*x as usize] = '#';
    }
    for row in grid {
        println!("{}", row.iter().collect::<String>());
    }
}

fn count(robots: &[(i32, i32)]) -> usize {
    robots.iter().filter(|(x, _)| *x == 40 || *x == 70).count()
}

fn part2(input: &str, bathroom: (i32, i32)) -> usize {
    let robots = parse(input);

    let mut t = 0;
    loop {
        let robots = robots
            .iter()
            .map(|r| r.predict(t, bathroom))
            .collect::<Vec<_>>();
        t += 1;
        let c = count(&robots);
        if c < 70 {
            continue;
        }
        println!("t: {}", t - 1);
        println!("{}", c);
        print(&robots, bathroom);
        std::io::stdin().read_line(&mut String::new()).unwrap();
    }
}

fn main() {
    let input = include_str!("../in.txt");
    let bathroom = (101, 103);
    println!("Part 1: {}", part1(input, bathroom));
    println!("Part 2: {}", part2(input, bathroom));
}

#[test]
fn example1() {
    let input = include_str!("../ex1.txt");
    let bathroom = (11, 7);
    assert_eq!(part1(input, bathroom), 12);
}
