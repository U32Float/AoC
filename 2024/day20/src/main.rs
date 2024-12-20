use std::{
    collections::{BinaryHeap, HashMap, HashSet},
    ops::Index,
};

use itertools::Itertools;

// -----------------------------------------------------------------------------

struct Map {
    grid: Vec<Vec<bool>>,
    start: (i32, i32),
    end: (i32, i32),
}

impl Index<(i32, i32)> for Map {
    type Output = bool;

    fn index(&self, (y, x): (i32, i32)) -> &Self::Output {
        &self.grid[y as usize][x as usize]
    }
}

fn manhatten(a: (i32, i32), b: (i32, i32)) -> i32 {
    (a.0 - b.0).abs() + (a.1 - b.1).abs()
}

impl Map {
    fn parse(input: &str) -> Self {
        let grid = input
            .lines()
            .map(|l| l.chars().map(|c| c == '#').collect())
            .collect();
        let start = input.lines().enumerate().find_map(|(y, l)| {
            l.chars().enumerate().find_map(|(x, c)| {
                if c == 'S' {
                    Some((y as i32, x as i32))
                } else {
                    None
                }
            })
        });
        let end = input.lines().enumerate().find_map(|(y, l)| {
            l.chars().enumerate().find_map(|(x, c)| {
                if c == 'E' {
                    Some((y as i32, x as i32))
                } else {
                    None
                }
            })
        });

        Self {
            grid,
            start: start.unwrap(),
            end: end.unwrap(),
        }
    }

    fn path_finding(&self) -> HashMap<(i32, i32), i32> {
        let mut queue = BinaryHeap::new();
        queue.push((0i32, self.end));
        let mut vs = HashMap::new();

        while let Some((time, (y, x))) = queue.pop() {
            for (dy, dx) in [(-1, 0), (1, 0), (0, -1), (0, 1)] {
                let v = y + dy;
                let u = x + dx;
                if v < 0 || u < 0 || v >= self.grid.len() as i32 || u >= self.grid[0].len() as i32 {
                    continue;
                }

                if vs.contains_key(&(v, u)) {
                    continue;
                }
                vs.insert((v, u), (time - 1).abs());

                if self[(v, u)] {
                    continue;
                }

                queue.push((time - 1, (v, u)));
            }
        }
        vs
    }

    fn possible_moves(&self, cheat_time: i32) -> HashMap<(i32, i32), Vec<(i32, (i32, i32))>> {
        let mut moves = HashMap::new();
        for start in (0..self.grid.len() as i32).cartesian_product(0..self.grid[0].len() as i32) {
            if self[start] {
                continue;
            }
            moves.insert(start, vec![]);
            for end in (0..self.grid.len() as i32).cartesian_product(0..self.grid[0].len() as i32) {
                if start == end {
                    continue;
                }
                if self[end] {
                    continue;
                }
                let manhatten = manhatten(start, end);
                if manhatten <= cheat_time {
                    moves.get_mut(&start).unwrap().push((manhatten, end));
                }
            }
        }
        moves
    }

    fn find_cheats(&self, threshold: usize, cheat_time: usize) -> usize {
        let mut cheats = HashSet::new();
        let paths = self.path_finding();
        let best = paths[&self.start];
        let moves = self.possible_moves(cheat_time as i32);

        let mut queue = BinaryHeap::new();
        queue.push((0i32, self.start));
        let mut vs = HashSet::new();

        while let Some((time, (y, x))) = queue.pop() {
            if vs.contains(&(y, x)) {
                continue;
            } else {
                vs.insert((y, x));
            }
            for (dt, (v, u)) in &moves[&(y, x)] {
                if *dt > 1 {
                    let rem = paths[&(*v, *u)];
                    let saving = best - (time - dt - rem).abs();
                    if saving >= threshold as i32 {
                        cheats.insert(((y, x), (*v, *u)));
                    }
                    continue;
                }

                queue.push((time - 1, (*v, *u)));
            }
        }
        cheats.len()
    }
}

fn part1(input: &str, threshold: usize) -> usize {
    Map::parse(input).find_cheats(threshold, 2)
}

fn part2(input: &str, threshold: usize) -> usize {
    Map::parse(input).find_cheats(threshold, 20)
}

fn main() {
    let input = include_str!("../in.txt");
    println!("Part 1: {}", part1(input, 100));
    println!("Part 2: {}", part2(input, 100));
}

#[test]
fn example1() {
    let input = include_str!("../ex1.txt");
    assert_eq!(part1(input, 10), 10);
    assert_eq!(part2(input, 70), 40);
}
