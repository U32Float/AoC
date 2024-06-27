use Cell::*;
use Direction::*;

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
enum Cell {
    RoundRock,
    CubeRock,
    Empty,
}

#[derive(Clone, Copy)]
enum Direction {
    North,
    East,
    South,
    West,
}

#[derive(Clone, Hash, PartialEq, Eq)]
struct Grid {
    grid: Vec<Vec<Cell>>,
}

impl Grid {
    fn height(&self) -> usize {
        self.grid.len()
    }

    fn width(&self) -> usize {
        self.grid[0].len()
    }

    fn parse(input: &str) -> Self {
        Self {
            grid: input
                .lines()
                .map(|line| {
                    line.chars()
                        .map(|char| match char {
                            'O' => RoundRock,
                            '#' => CubeRock,
                            '.' => Empty,
                            _ => panic!(),
                        })
                        .collect()
                })
                .collect(),
        }
    }

    fn tilt(mut self, dir: Direction) -> Self {
        fn iter(grid: &mut Grid, dir: Direction, col: usize, row: usize) {
            if grid.grid[row][col] != RoundRock {
                return;
            }

            let mut i = 1;
            let mut placement = None;
            loop {
                match dir {
                    North => {
                        if (row as i32 - i as i32) < 0 {
                            break;
                        }
                    }
                    East => {
                        if col + i >= grid.width() {
                            break;
                        }
                    }
                    South => {
                        if row + i >= grid.height() {
                            break;
                        }
                    }
                    West => {
                        if (col as i32 - i as i32) < 0 {
                            break;
                        }
                    }
                };
                let next = match dir {
                    North => grid.grid[row - i][col],
                    East => grid.grid[row][col + i],
                    South => grid.grid[row + i][col],
                    West => grid.grid[row][col - i],
                };
                if next == CubeRock {
                    break;
                }

                if next == Empty {
                    placement = Some(match dir {
                        North => (row - i, col),
                        East => (row, col + i),
                        South => (row + i, col),
                        West => (row, col - i),
                    })
                }
                i += 1;
            }

            if let Some(p) = placement {
                grid.grid[row][col] = Empty;
                grid.grid[p.0][p.1] = RoundRock;
            }
        }

        match dir {
            North => {
                for col in 0..self.width() {
                    for row in 0..self.height() {
                        iter(&mut self, dir, col, row);
                    }
                }
            }
            East => {
                for row in 0..self.height() {
                    for col in (0..self.width()).rev() {
                        iter(&mut self, dir, col, row);
                    }
                }
            }
            South => {
                for col in 0..self.width() {
                    for row in (0..self.height()).rev() {
                        iter(&mut self, dir, col, row);
                    }
                }
            }
            West => {
                for row in 0..self.height() {
                    for col in 0..self.width() {
                        iter(&mut self, dir, col, row);
                    }
                }
            }
        }

        self
    }

    fn total_load(&self) -> usize {
        let mut total = 0;
        for i in 0..self.height() {
            let n = self.grid[i]
                .iter()
                .filter(|cell| **cell == RoundRock)
                .count();
            total += n * (self.height() - i);
        }
        total
    }
}

fn cycle(grid: Grid) -> Grid {
    grid.tilt(North).tilt(West).tilt(South).tilt(East)
}

fn part1(input: &str) -> usize {
    let mut grid = Grid::parse(input);
    grid = grid.tilt(North);

    grid.total_load()
}

fn part2(input: &str) -> usize {
    const I: usize = 1000000000;
    let mut grid = Grid::parse(input);

    let mut cache = vec![grid.clone()];
    let mut skipped = false;

    let mut i = 0;
    while i < I {
        grid = cycle(grid);

        if !skipped {
            if cache.contains(&grid) {
                let j = cache.iter().position(|g| *g == grid).unwrap();
                i = I - ((I - (i + 1)) % (i - j + 1));
                skipped = true;
                continue;
            } else {
                cache.push(grid.clone());
            }
        }
        i += 1;
    }

    grid.total_load()
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
    assert_eq!(part1(input), 136);
    assert_eq!(part2(input), 64);
}
