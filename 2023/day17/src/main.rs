use std::collections::{HashMap, VecDeque};

use Direction::*;

struct Grid {
    grid: Vec<Vec<usize>>,
}

impl Grid {
    fn width(&self) -> usize {
        self.grid[0].len()
    }

    fn height(&self) -> usize {
        self.grid.len()
    }

    fn parse(input: &str) -> Self {
        let grid = input
            .lines()
            .map(|line| {
                line.chars()
                    .map(|c| c.to_digit(10).unwrap() as usize)
                    .collect()
            })
            .collect();

        Self { grid }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
enum Direction {
    North,
    East,
    South,
    West,
}

impl Direction {
    fn opposite(self) -> Self {
        match self {
            North => South,
            East => West,
            South => North,
            West => East,
        }
    }
}

fn insort<T: Ord>(vec: &mut VecDeque<T>, val: T) {
    match vec.binary_search(&val) {
        Ok(idx) => vec.insert(idx, val),
        Err(idx) => vec.insert(idx, val),
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct State {
    x: i32,
    y: i32,
    dir: Direction,
    steps: usize,
    heat_loss: usize,
}

impl PartialOrd for State {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.heat_loss.partial_cmp(&other.heat_loss)
    }
}

impl Ord for State {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.heat_loss.cmp(&other.heat_loss)
    }
}

fn solve(grid: &Grid, min: usize, max: usize) -> usize {
    let south = State {
        x: 0,
        y: 1,
        dir: South,
        steps: 1,
        heat_loss: grid.grid[1][0],
    };
    let east = State {
        x: 1,
        y: 0,
        dir: East,
        steps: 1,
        heat_loss: grid.grid[0][1],
    };

    let mut visited = HashMap::new();

    let mut queue = VecDeque::new();
    insort(&mut queue, south);
    insort(&mut queue, east);

    while let Some(state) = queue.pop_front() {
        if state.x == grid.width() as i32 - 1 && state.y == grid.height() as i32 - 1 {
            if state.steps >= min {
                return state.heat_loss;
            } else {
                continue;
            }
        }

        match visited.entry((state.x, state.y, state.steps, state.dir)) {
            std::collections::hash_map::Entry::Occupied(mut entry) => {
                if state.heat_loss < *entry.get() {
                    entry.insert(state.heat_loss);
                } else {
                    continue;
                }
            }
            std::collections::hash_map::Entry::Vacant(entry) => {
                entry.insert(state.heat_loss);
            }
        }

        for dir in [North, East, South, West] {
            if (dir == state.dir && state.steps >= max)
                || (dir != state.dir && state.steps < min)
                || dir == state.dir.opposite()
            {
                continue;
            }
            let (x, y) = (state.x, state.y);
            let (u, v) = match dir {
                North => (x, y - 1),
                East => (x + 1, y),
                South => (x, y + 1),
                West => (x - 1, y),
            };

            if u < 0 || u >= grid.width() as i32 || v < 0 || v >= grid.height() as i32 {
                continue;
            }

            insort(
                &mut queue,
                State {
                    x: u,
                    y: v,
                    dir,
                    steps: if dir == state.dir { state.steps + 1 } else { 1 },
                    heat_loss: state.heat_loss + grid.grid[v as usize][u as usize],
                },
            );
        }
    }

    panic!("No path found");
}

fn part1(input: &str) -> usize {
    let grid = Grid::parse(input);

    solve(&grid, 1, 3)
}

fn part2(input: &str) -> usize {
    let grid = Grid::parse(input);

    solve(&grid, 4, 10)
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
    assert_eq!(part1(input), 102);
    assert_eq!(part2(input), 94);
}

#[test]
fn example2() {
    let input = include_str!("../ex2.txt");
    assert_eq!(part2(input), 71);
}
