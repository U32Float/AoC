use std::{
    collections::{BinaryHeap, HashMap, HashSet},
    hash::Hash,
    ops::{Index, IndexMut},
};

// -----------------------------------------------------------------------------

#[derive(Debug, Clone, Copy, Eq, PartialEq, PartialOrd, Ord, Hash)]
enum Dir {
    Up,
    Down,
    Left,
    Right,
}

impl Dir {
    fn rot_cw(&self) -> Self {
        match self {
            Self::Up => Self::Right,
            Self::Down => Self::Left,
            Self::Left => Self::Up,
            Self::Right => Self::Down,
        }
    }

    fn rot_ccw(&self) -> Self {
        match self {
            Self::Up => Self::Left,
            Self::Down => Self::Right,
            Self::Left => Self::Down,
            Self::Right => Self::Up,
        }
    }

    fn delta(&self) -> (i32, i32) {
        match self {
            Self::Up => (-1, 0),
            Self::Down => (1, 0),
            Self::Left => (0, -1),
            Self::Right => (0, 1),
        }
    }
}

struct Map {
    grid: Vec<Vec<bool>>,
    start: (i32, i32),
    end: (i32, i32),
}

impl Index<(i32, i32)> for Map {
    type Output = bool;

    fn index(&self, index: (i32, i32)) -> &Self::Output {
        &self.grid[index.0 as usize][index.1 as usize]
    }
}

impl IndexMut<(i32, i32)> for Map {
    fn index_mut(&mut self, index: (i32, i32)) -> &mut Self::Output {
        &mut self.grid[index.0 as usize][index.1 as usize]
    }
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

    fn solve(&self) -> (usize, usize) {
        #[derive(Debug, Clone, Eq, PartialEq)]
        struct State {
            score: i32,
            pos: (i32, i32),
            dir: Dir,
            prev: Option<((i32, i32), Dir)>,
        }
        impl PartialOrd for State {
            fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
                Some(self.cmp(other))
            }
        }
        impl Ord for State {
            fn cmp(&self, other: &Self) -> std::cmp::Ordering {
                self.score.cmp(&other.score)
            }
        }
        impl Hash for State {
            fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
                self.pos.hash(state);
                self.dir.hash(state);
            }
        }

        let mut vs: HashMap<((i32, i32), Dir), (i32, HashSet<State>)> = HashMap::new();

        let mut queue = BinaryHeap::new();
        queue.push(State {
            score: 0,
            pos: self.start,
            dir: Dir::Right,
            prev: None,
        });

        let mut best = None;
        let mut paths = HashSet::new();

        while let Some(state) = queue.pop() {
            if state.pos == self.end {
                if best.map(|b| b == state.score).unwrap_or(true) {
                    best = Some(state.score);
                    paths.insert(state.prev);
                } else {
                    break;
                }
            }

            if vs.contains_key(&(state.pos, state.dir)) {
                if vs[&(state.pos, state.dir)].0 == state.score {
                    vs.get_mut(&(state.pos, state.dir)).unwrap().1.insert(state);
                }
                continue;
            }
            vs.insert((state.pos, state.dir), {
                let mut tmp = HashSet::new();
                tmp.insert(state.clone());
                (state.score, tmp)
            });

            let State {
                score,
                pos: (y, x),
                dir,
                prev: _,
            } = state;

            let (dy, dx) = dir.delta();
            if !self[(y + dy, x + dx)] {
                queue.push(State {
                    score: score - 1,
                    pos: (y + dy, x + dx),
                    dir,
                    prev: Some(((y, x), dir)),
                });
            }

            let dir_cw = dir.rot_cw();
            let (dy, dx) = dir_cw.delta();
            if !self[(y + dy, x + dx)] {
                queue.push(State {
                    score: score - 1001,
                    pos: (y + dy, x + dx),
                    dir: dir_cw,
                    prev: Some(((y, x), dir)),
                });
            }

            let dir_ccw = dir.rot_ccw();
            let (dy, dx) = dir_ccw.delta();
            if !self[(y + dy, x + dx)] {
                queue.push(State {
                    score: score - 1001,
                    pos: (y + dy, x + dx),
                    dir: dir_ccw,
                    prev: Some(((y, x), dir)),
                });
            }
        }

        let mut tiles = HashSet::new();
        let mut stack: Vec<Option<((i32, i32), Dir)>> = paths.into_iter().collect();
        while let Some(prev) = stack.pop() {
            if let Some((pos, dir)) = prev {
                tiles.insert(pos);
                for s in vs[&(pos, dir)].1.iter() {
                    stack.push(s.prev);
                }
            }
        }

        (best.unwrap().unsigned_abs() as usize, tiles.len() + 1)
    }
}

fn part1(input: &str) -> usize {
    Map::parse(input).solve().0
}

fn part2(input: &str) -> usize {
    Map::parse(input).solve().1
}

fn main() {
    let input = include_str!("../in.txt");
    println!("Part 1: {}", part1(input));
    println!("Part 2: {}", part2(input));
}

#[test]
fn example1() {
    let input = include_str!("../ex1.txt");
    assert_eq!(part1(input), 7036);
    assert_eq!(part2(input), 45);
}

#[test]
fn example2() {
    let input = include_str!("../ex2.txt");
    assert_eq!(part1(input), 11048);
    assert_eq!(part2(input), 64);
}
