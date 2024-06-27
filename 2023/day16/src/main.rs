use std::collections::HashSet;

use Direction::*;

#[derive(Hash, PartialEq, Eq, Clone)]
struct Grid {
    grid: Vec<Vec<char>>,
}

impl Grid {
    fn width(&self) -> usize {
        self.grid[0].len()
    }

    fn height(&self) -> usize {
        self.grid.len()
    }

    fn parse(input: &str) -> Self {
        let grid = input.lines().map(|line| line.chars().collect()).collect();

        Self { grid }
    }
}

#[derive(Hash, PartialEq, Eq, Clone, Copy)]
enum Direction {
    North,
    East,
    South,
    West,
}

fn energize(grid: &Grid, x: i32, y: i32, dir: Direction) -> usize {
    let mut energy = {
        let mut tmp = vec![];
        for _ in 0..grid.height() {
            tmp.push(vec![0; grid.width()]);
        }
        tmp
    };

    fn traverse(
        visited: &mut HashSet<(i32, i32, Direction)>,
        grid: &Grid,
        energy: &mut Vec<Vec<usize>>,
        x: i32,
        y: i32,
        dir: Direction,
    ) {
        if visited.contains(&(x, y, dir)) {
            return;
        } else {
            visited.insert((x, y, dir));
        }

        let (u, v) = match dir {
            North => (x, y - 1),
            East => (x + 1, y),
            South => (x, y + 1),
            West => (x - 1, y),
        };

        if !(u >= 0 && u < grid.width() as i32 && v >= 0 && v < grid.height() as i32) {
            return;
        }

        energy[v as usize][u as usize] += 1;
        match grid.grid[v as usize][u as usize] {
            '.' => traverse(visited, grid, energy, u, v, dir),
            '|' => match dir {
                North | South => traverse(visited, grid, energy, u, v, dir),
                East | West => {
                    traverse(visited, grid, energy, u, v, North);
                    traverse(visited, grid, energy, u, v, South);
                }
            },
            '-' => match dir {
                East | West => traverse(visited, grid, energy, u, v, dir),
                North | South => {
                    traverse(visited, grid, energy, u, v, East);
                    traverse(visited, grid, energy, u, v, West);
                }
            },
            '/' => match dir {
                North => traverse(visited, grid, energy, u, v, East),
                East => traverse(visited, grid, energy, u, v, North),
                South => traverse(visited, grid, energy, u, v, West),
                West => traverse(visited, grid, energy, u, v, South),
            },
            '\\' => match dir {
                North => traverse(visited, grid, energy, u, v, West),
                East => traverse(visited, grid, energy, u, v, South),
                South => traverse(visited, grid, energy, u, v, East),
                West => traverse(visited, grid, energy, u, v, North),
            },
            _ => panic!(),
        }
    }

    traverse(&mut HashSet::new(), &grid, &mut energy, x, y, dir);

    energy
        .iter()
        .map(|row| row.iter().filter(|c| **c > 0).count())
        .sum()
}

fn part1(input: &str) -> usize {
    let grid = Grid::parse(input);

    energize(&grid, -1, 0, East)
}

fn part2(input: &str) -> usize {
    let grid = Grid::parse(input);

    let mut max = 0;

    for x in 0..grid.width() {
        max = max.max(energize(&grid, x as i32, -1, South));
        max = max.max(energize(&grid, x as i32, grid.height() as i32, North));
    }

    for y in 0..grid.height() {
        max = max.max(energize(&grid, -1, y as i32, East));
        max = max.max(energize(&grid, grid.width() as i32, y as i32, West));
    }

    max
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
    assert_eq!(part1(input), 46);
    assert_eq!(part2(input), 51);
}
