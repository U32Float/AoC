use std::cmp::Ordering::*;
use std::{collections::HashMap, f32::consts::PI};

use gcd::Gcd;
use itertools::Itertools;

#[derive(Debug)]
struct Grid {
    grid: Vec<Vec<bool>>,
}

impl Grid {
    fn parse(input: &str) -> Self {
        Self {
            grid: input
                .lines()
                .map(|l| l.chars().map(|c| c == '#').collect())
                .collect(),
        }
    }

    fn width(&self) -> usize {
        self.grid[0].len()
    }

    fn height(&self) -> usize {
        self.grid.len()
    }
}

fn vector_map(grid: &Grid) -> HashMap<(usize, usize), HashMap<(i32, i32), Vec<(usize, usize)>>> {
    let mut vectors = HashMap::new();
    for r1 in 0..grid.height() {
        for c1 in 0..grid.width() {
            for r2 in 0..grid.height() {
                for c2 in 0..grid.width() {
                    if !grid.grid[r1][c1] || !grid.grid[r2][c2] || (r1 == r2 && c1 == c2) {
                        continue;
                    }
                    let (dy, dx) = (r2 as i32 - r1 as i32, c2 as i32 - c1 as i32);
                    let gcd = (dy.abs() as usize).gcd(dx.abs() as usize) as i32;
                    let normal = (dy / gcd.max(1), dx / gcd.max(1));
                    vectors
                        .entry((r1, c1))
                        .or_insert(HashMap::new())
                        .entry(normal)
                        .or_insert(vec![])
                        .push((r2, c2));
                }
            }
        }
    }

    vectors
}

fn part1(input: &'static str) -> usize {
    let grid = Grid::parse(input);

    vector_map(&grid)
        .values()
        .max_by(|a, b| a.len().cmp(&b.len()))
        .unwrap()
        .len()
}

fn angle((x1, y1): (f32, f32), (x2, y2): (f32, f32)) -> f32 {
    let dot = x1 * x2 + y1 * y2;
    let det = x1 * y2 - y1 * x2;
    let a = det.atan2(dot);

    if a > 0.0 {
        a
    } else {
        2. * PI - a
    }
}

fn manhatten((y1, x1): (usize, usize), (y2, x2): (usize, usize)) -> i32 {
    (y2 as i32 - y1 as i32).abs() + (x2 as i32 - x1 as i32).abs()
}

fn part2(input: &'static str) -> usize {
    let grid = Grid::parse(input);
    let map = vector_map(&grid);

    let ((y, x), mut map) = map
        .into_iter()
        .max_by(|a, b| a.1.len().cmp(&b.1.len()))
        .unwrap();

    let sorted = map
        .clone()
        .into_keys()
        .sorted_by(|(y1, x1), (y2, x2)| {
            let a1 = angle((*x1 as f32, *y1 as f32), (0., 1.));
            let a2 = angle((*x2 as f32, *y2 as f32), (0., 1.));
            a1.total_cmp(&a2)
        })
        .collect_vec();

    map.values_mut().for_each(|ps| {
        ps.sort_by(
            |p1, p2| match manhatten(*p1, (y, x)).cmp(&manhatten(*p2, (y, x))) {
                Less => Greater,
                Equal => Equal,
                Greater => Less,
            },
        );
    });

    let mut i = 1;
    loop {
        for v in &sorted {
            let ps = map.get_mut(v).unwrap();
            if ps.len() > 0 {
                let asteroid = ps.pop().unwrap();
                if i == 200 {
                    return asteroid.0 + 100 * asteroid.1;
                }
                i += 1;
            }
        }
    }
}

fn main() {
    let input = include_str!("../in.txt");
    println!("Part 1: {}", part1(input));
    println!("Part 2: {}", part2(input));
}

#[test]
fn example1() {
    let input = include_str!("../ex1.txt");
    assert_eq!(part1(input), 210);
    assert_eq!(part2(input), 802);
}
