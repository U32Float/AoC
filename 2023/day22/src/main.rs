use std::{
    collections::HashSet,
    ops::{Index, IndexMut},
};

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
struct Pos {
    x: usize,
    y: usize,
    z: usize,
}

impl Pos {
    fn parse(input: &str) -> Self {
        let s = input
            .split(',')
            .map(|s| s.parse().unwrap())
            .collect::<Vec<_>>();
        Self {
            x: s[0],
            y: s[1],
            z: s[2],
        }
    }

    fn down(self) -> Self {
        Self {
            z: self.z - 1,
            ..self
        }
    }

    fn up(self) -> Self {
        Self {
            z: self.z + 1,
            ..self
        }
    }
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
struct Brick {
    start: Pos,
    end: Pos,
}

impl Brick {
    fn axis(self) -> Axis {
        if self.start.x != self.end.x {
            Axis::X
        } else if self.start.y != self.end.y {
            Axis::Y
        } else if self.start.z != self.end.z {
            Axis::Z
        } else {
            Axis::X
        }
    }

    fn iter(&self) -> BrickIterator {
        BrickIterator {
            brick: self,
            axis: self.axis(),
            idx: 0,
        }
    }

    fn drop(self) -> Self {
        Self {
            start: self.start.down(),
            end: self.end.down(),
        }
    }

    fn above(self) -> Vec<Pos> {
        if self.axis() == Axis::Z {
            vec![self.end.up()]
        } else {
            self.iter().map(|p| p.up()).collect()
        }
    }

    fn below(self) -> Vec<Pos> {
        if self.axis() == Axis::Z {
            vec![self.start.down()]
        } else {
            self.iter().map(|p| p.down()).collect()
        }
    }
}

#[derive(PartialEq, Eq)]
enum Axis {
    X,
    Y,
    Z,
}

struct BrickIterator<'a> {
    brick: &'a Brick,
    axis: Axis,
    idx: usize,
}

impl Iterator for BrickIterator<'_> {
    type Item = Pos;

    fn next(&mut self) -> Option<Self::Item> {
        macro_rules! axis {
            ($x: ident) => {
                if self.brick.start.$x + self.idx <= self.brick.end.$x {
                    let ret = Some(Pos {
                        $x: self.brick.start.$x + self.idx,
                        ..self.brick.start
                    });
                    self.idx += 1;
                    ret
                } else {
                    None
                }
            };
        }
        match self.axis {
            Axis::X => axis!(x),
            Axis::Y => axis!(y),
            Axis::Z => axis!(z),
        }
    }
}

impl Brick {
    fn parse(input: &str) -> Self {
        let (start, end) = input.split_once('~').unwrap();
        Self {
            start: Pos::parse(start),
            end: Pos::parse(end),
        }
    }
}

struct Grid {
    grid: Vec<Vec<Vec<Option<usize>>>>,
}

impl Index<Pos> for Grid {
    type Output = Option<usize>;

    fn index(&self, index: Pos) -> &Self::Output {
        &self.grid[index.z][index.y][index.x]
    }
}

impl IndexMut<Pos> for Grid {
    fn index_mut(&mut self, index: Pos) -> &mut Self::Output {
        &mut self.grid[index.z][index.y][index.x]
    }
}

fn sim_bricks(input: &str) -> (Grid, Vec<Brick>) {
    let mut bricks = {
        let mut bricks = input.lines().map(|l| Brick::parse(l)).collect::<Vec<_>>();
        bricks.sort_by_key(|b| b.start.z);
        bricks
    };

    let ((min_x, min_y, min_z), (max_x, max_y, max_z)) = bricks.iter().fold(
        ((usize::MAX, usize::MAX, usize::MAX), (0, 0, 0)),
        |acc, b| {
            let ((min_x, min_y, min_z), (max_x, max_y, max_z)) = acc;
            (
                (
                    min_x.min(b.start.x),
                    min_y.min(b.start.y),
                    min_z.min(b.start.z),
                ),
                (max_x.max(b.end.x), max_y.max(b.end.y), max_z.max(b.end.z)),
            )
        },
    );

    let mut grid = Grid {
        grid: vec![vec![vec![None; max_x - min_x + 1]; max_y - min_y + 1]; max_z - min_z + 1],
    };

    for (idx, brick) in bricks.iter_mut().enumerate() {
        loop {
            let tmp = brick.drop();
            if tmp.iter().all(|p| grid[p].is_none() && p.z >= 1) {
                *brick = tmp;
            } else {
                for p in brick.iter() {
                    grid[p] = Some(idx);
                }
                break;
            }
        }
    }

    (grid, bricks)
}

fn part1(input: &str) -> usize {
    let (grid, bricks) = sim_bricks(input);
    bricks
        .iter()
        .filter(|b| {
            for p in b.above() {
                if p.z >= grid.grid.len() {
                    continue;
                }
                match grid[p] {
                    Some(j) => {
                        let mut support = HashSet::new();
                        for q in bricks[j].below() {
                            if let Some(q) = grid[q] {
                                support.insert(q);
                            }
                        }
                        if support.len() == 1 {
                            return false;
                        }
                    }
                    None => continue,
                }
            }
            true
        })
        .count()
}

fn part2(input: &str) -> usize {
    let (grid, bricks) = sim_bricks(input);
    let supports = bricks
        .iter()
        .map(|b| {
            let mut support = HashSet::new();
            for q in b.below() {
                if let Some(q) = grid[q] {
                    support.insert(q);
                }
            }
            support
        })
        .collect::<Vec<_>>();

    (0..bricks.len())
        .map(|i| {
            let mut removed = HashSet::new();
            removed.insert(i);
            loop {
                let mut tmp = vec![];
                for j in 0..bricks.len() {
                    if i == j || removed.contains(&j) {
                        continue;
                    }
                    if !supports[j].is_empty() && supports[j].is_subset(&removed) {
                        tmp.push(j);
                    }
                }
                if tmp.is_empty() {
                    break;
                } else {
                    removed.extend(tmp);
                }
            }
            removed.len() - 1
        })
        .sum()
}

fn main() {
    let input = include_str!("../in.txt");

    println!("Part 1: {}", part1(input));
    println!("Part 2: {}", part2(input));
}

#[test]
fn example1() {
    let input = include_str!("../ex1.txt");
    assert_eq!(part1(input), 5);
    assert_eq!(part2(input), 7);
}
