use std::collections::HashSet;

#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq)]
struct GardenPos {
    row: usize,
    col: usize,
}

#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq)]
struct AbsolutePos {
    row: i32,
    col: i32,
}

impl AbsolutePos {
    fn to_garden_pos(self, garden: &Garden) -> GardenPos {
        GardenPos {
            row: if self.row >= 0 {
                (self.row % garden.height() as i32) as usize
            } else {
                (garden.height() as i32 - 1 - ((self.row.abs() - 1) % garden.height() as i32))
                    as usize
            },
            col: if self.col >= 0 {
                (self.col % garden.width() as i32) as usize
            } else {
                (garden.width() as i32 - 1 - ((self.col.abs() - 1) % garden.width() as i32))
                    as usize
            },
        }
    }
}

struct Garden {
    start: AbsolutePos,
    grid: Vec<Vec<bool>>,
}

impl Garden {
    fn parse(input: &str) -> Self {
        let mut start = (0, 0);
        for (row, line) in input.lines().enumerate() {
            for (col, char) in line.chars().enumerate() {
                if char == 'S' {
                    start.0 = row as i32;
                    start.1 = col as i32;
                    break;
                }
            }
        }
        Self {
            start: AbsolutePos {
                row: start.0,
                col: start.1,
            },
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

fn walk(garden: &Garden, steps: usize) -> usize {
    let mut ps = HashSet::new();
    ps.insert(garden.start);

    for _ in 0..steps {
        let mut tmp = HashSet::new();
        for apos in &ps {
            for (v, u) in [(0, 1), (0, -1), (1, 0), (-1, 0)] {
                let npos = AbsolutePos {
                    row: apos.row + v,
                    col: apos.col + u,
                };

                let gpos = npos.to_garden_pos(garden);
                if garden.grid[gpos.row][gpos.col] {
                    continue;
                }

                tmp.insert(npos);
            }
        }
        std::mem::swap(&mut ps, &mut tmp);
    }

    ps.len()
}

fn part1(input: &'static str, steps: usize) -> usize {
    let garden = Garden::parse(input);
    walk(&garden, steps)
}

fn part2(input: &'static str, steps: usize) -> usize {
    let garden = Garden::parse(input);
    let count = [65, 196, 327]
        .into_iter()
        .map(|i| walk(&garden, i))
        .collect::<Vec<_>>();

    let b0 = count[0];
    let b1 = count[1] - count[0];
    let b2 = count[2] - count[1];

    let n = steps / garden.width();
    b0 + b1 * n + (n * (n - 1) / 2) * (b2 - b1)
}

fn main() {
    let input = include_str!("../in.txt");

    println!("Part 1: {}", part1(input, 64));
    println!("Part 2: {}", part2(input, 26501365));
}

#[test]
fn example1() {
    let input = include_str!("../ex1.txt");

    assert_eq!(part1(input, 6), 16);
}
