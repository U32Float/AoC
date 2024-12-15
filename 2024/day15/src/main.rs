use itertools::Itertools;

#[derive(Debug, Clone, Copy, PartialEq)]
enum Dir {
    Up,
    Down,
    Left,
    Right,
}

impl Dir {
    fn delta(&self) -> (i32, i32) {
        match self {
            Dir::Up => (-1, 0),
            Dir::Down => (1, 0),
            Dir::Left => (0, -1),
            Dir::Right => (0, 1),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum Cell {
    Wall,
    Box,
    BoxLeft,
    BoxRight,
    Empty,
}

struct Map {
    grid: Vec<Vec<Cell>>,
    pos: (i32, i32),
    moves: Vec<Dir>,
}

impl Map {
    fn parse(input: &str) -> Self {
        let walls = input
            .lines()
            .take_while(|l| !l.is_empty())
            .map(|l| {
                l.chars()
                    .map(|c| match c {
                        '#' => Cell::Wall,
                        'O' => Cell::Box,
                        _ => Cell::Empty,
                    })
                    .collect()
            })
            .collect();
        let (r, c) = input
            .lines()
            .take_while(|l| !l.is_empty())
            .enumerate()
            .map(|(r, l)| (r, l.find('@')))
            .find(|(_, c)| c.is_some())
            .unwrap();

        let moves = input
            .lines()
            .flat_map(|l| {
                l.chars().filter_map(|c| match c {
                    '^' => Some(Dir::Up),
                    '>' => Some(Dir::Right),
                    '<' => Some(Dir::Left),
                    'v' => Some(Dir::Down),
                    _ => None,
                })
            })
            .collect();

        Self {
            grid: walls,
            pos: (r as i32, c.unwrap() as i32),
            moves,
        }
    }

    fn double(self) -> Self {
        let mut grid = vec![vec![]; self.grid.len()];
        for (r, c) in (0..self.grid.len()).cartesian_product(0..self.grid[0].len()) {
            match self.grid[r][c] {
                Cell::Wall => {
                    grid[r].push(Cell::Wall);
                    grid[r].push(Cell::Wall);
                }
                Cell::Box => {
                    grid[r].push(Cell::BoxLeft);
                    grid[r].push(Cell::BoxRight);
                }
                Cell::Empty => {
                    grid[r].push(Cell::Empty);
                    grid[r].push(Cell::Empty);
                }
                _ => unreachable!(),
            }
        }
        Self {
            grid,
            pos: (self.pos.0, self.pos.1 * 2),
            moves: self.moves,
        }
    }

    fn can_move(&mut self, (y, x): (i32, i32), dir: Dir) -> bool {
        let (dy, dx) = dir.delta();
        let mut stack = vec![(y, x)];
        while let Some((y, x)) = stack.pop() {
            let (v, u) = (y + dy, x + dx);
            let to = self.grid[v as usize][u as usize];
            match to {
                Cell::Wall => return false,
                Cell::Box => stack.push((v, u)),
                Cell::BoxLeft => {
                    if dy != 0 {
                        stack.push((v, u));
                        stack.push((v, u + 1));
                    } else {
                        stack.push((v, u));
                    }
                }
                Cell::BoxRight => {
                    if dy != 0 {
                        stack.push((v, u));
                        stack.push((v, u - 1));
                    } else {
                        stack.push((v, u));
                    }
                }
                Cell::Empty => (),
            }
        }
        true
    }

    fn mov(&mut self, (y, x): (i32, i32), dir: Dir) {
        let (dy, dx) = dir.delta();

        enum Cmd {
            Move((i32, i32)),
            Swap((i32, i32), (i32, i32)),
        }
        let mut stack = vec![Cmd::Move((y, x))];

        while let Some(cmd) = stack.pop() {
            match cmd {
                Cmd::Move((y, x)) => {
                    let (v, u) = (y + dy, x + dx);
                    let from = self.grid[y as usize][x as usize];
                    match from {
                        Cell::Wall => panic!(),
                        Cell::Box => {
                            stack.push(Cmd::Swap((y, x), (v, u)));
                            stack.push(Cmd::Move((v, u)));
                        }
                        Cell::BoxLeft => {
                            stack.push(Cmd::Swap((y, x), (v, u)));
                            if dy != 0 {
                                stack.push(Cmd::Swap((y, x + 1), (v, u + 1)));
                            }
                            stack.push(Cmd::Move((v, u)));
                            if dy != 0 {
                                stack.push(Cmd::Move((v, u + 1)));
                            }
                        }
                        Cell::BoxRight => {
                            stack.push(Cmd::Swap((y, x), (v, u)));
                            if dy != 0 {
                                stack.push(Cmd::Swap((y, x - 1), (v, u - 1)));
                            }
                            stack.push(Cmd::Move((v, u)));
                            if dy != 0 {
                                stack.push(Cmd::Move((v, u - 1)));
                            }
                        }
                        Cell::Empty => continue,
                    }
                }
                Cmd::Swap((y, x), (v, u)) => {
                    let tmp = self.grid[y as usize][x as usize];
                    self.grid[y as usize][x as usize] = self.grid[v as usize][u as usize];
                    self.grid[v as usize][u as usize] = tmp;
                }
            }
        }
    }

    fn walk(mut self) -> Self {
        for dir in self.moves.clone() {
            let (dy, dx) = dir.delta();
            let (y, x) = self.pos;
            if self.can_move((y, x), dir) {
                self.mov((y + dy, x + dx), dir);
                self.pos = (y + dy, x + dx);
            }
        }
        self
    }

    fn sum(&self) -> usize {
        self.grid
            .iter()
            .enumerate()
            .flat_map(|(y, row)| {
                row.iter()
                    .enumerate()
                    .filter(|(_, &c)| c == Cell::Box || c == Cell::BoxLeft)
                    .map(move |(x, _)| 100 * y + x)
            })
            .sum()
    }
}

fn part1(input: &str) -> usize {
    Map::parse(input).walk().sum()
}

fn part2(input: &str) -> usize {
    Map::parse(input).double().walk().sum()
}

fn main() {
    let input = include_str!("../in.txt");
    println!("Part 1: {}", part1(input));
    println!("Part 2: {}", part2(input));
}

#[test]
fn example1() {
    let input = include_str!("../ex1.txt");
    assert_eq!(part1(input), 10092);
    assert_eq!(part2(input), 9021);
}

#[test]
fn example2() {
    let input = include_str!("../ex2.txt");
    assert_eq!(part1(input), 2028);
}
