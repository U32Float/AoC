use std::collections::{HashMap, HashSet};

#[derive(PartialEq, Eq, Clone, Copy)]
enum Direction {
    East,
    South,
}

#[derive(PartialEq, Eq, Clone, Copy)]
enum Cell {
    Path,
    Forest,
    Slope(Direction),
}

struct Grid {
    grid: Vec<Vec<Cell>>,
}

impl Grid {
    fn parse(input: &str) -> Self {
        Self {
            grid: input
                .lines()
                .map(|l| {
                    l.chars()
                        .map(|c| match c {
                            '.' => Cell::Path,
                            '#' => Cell::Forest,
                            '>' => Cell::Slope(Direction::East),
                            'v' => Cell::Slope(Direction::South),
                            _ => panic!(),
                        })
                        .collect()
                })
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

fn part1(input: &str) -> usize {
    let grid = Grid::parse(input);

    let mut steps = HashMap::new();
    let mut stack = vec![((0, 1), 0, HashSet::new())];

    while let Some(((row, col), s, mut vs)) = stack.pop() {
        if vs.contains(&(row, col)) {
            continue;
        } else {
            vs.insert((row, col));
        }

        if steps.contains_key(&(row, col)) {
            if s > steps[&(row, col)] {
                steps.insert((row, col), s);
            } else {
                continue;
            }
        } else {
            steps.insert((row, col), s);
        }

        let expand = match &grid.grid[row as usize][col as usize] {
            Cell::Path => vec![(0, 1), (0, -1), (1, 0), (-1, 0)],
            Cell::Slope(dir) => match dir {
                Direction::East => vec![(0, 1)],
                Direction::South => vec![(1, 0)],
            },
            Cell::Forest => panic!(),
        };

        for (v, u) in expand {
            let (y, x) = (row + v, col + u);

            if x < 0 || x >= grid.width() as i32 || y < 0 || y >= grid.height() as i32 {
                continue;
            }

            match &grid.grid[y as usize][x as usize] {
                Cell::Path => stack.push(((y, x), s + 1, vs.clone())),
                Cell::Slope(dir) => match dir {
                    Direction::East => {
                        if (v, u) == (0, 1) {
                            stack.push(((y, x), s + 1, vs.clone()))
                        } else {
                            continue;
                        }
                    }
                    Direction::South => {
                        if (v, u) == (1, 0) {
                            stack.push(((y, x), s + 1, vs.clone()))
                        } else {
                            continue;
                        }
                    }
                },
                Cell::Forest => continue,
            }
        }
    }

    steps[&(grid.height() as i32 - 1, grid.width() as i32 - 2)]
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
struct Pos {
    col: i32,
    row: i32,
}

#[derive(Debug)]
struct Graph {
    start: Pos,
    end: Pos,
    nodes: HashMap<Pos, HashSet<(Pos, usize)>>,
}

impl Graph {
    fn new(grid: &Grid) -> Self {
        let start = Pos { col: 1, row: 0 };
        let end = Pos {
            col: grid.width() as i32 - 2,
            row: grid.height() as i32 - 1,
        };
        let mut nodes: HashMap<Pos, HashSet<(Pos, usize)>> = HashMap::new();

        let mut intersections = HashSet::new();
        for row in 0..grid.height() {
            for col in 0..grid.width() {
                let pos = Pos {
                    col: col as i32,
                    row: row as i32,
                };
                let mut dst = HashSet::new();
                let mut stack = vec![(pos, 0)];
                let mut vs = HashSet::new();
                vs.insert(pos);
                while let Some(state) = stack.pop() {
                    let (p, s) = state;

                    let mut expand = vec![];
                    for (v, u) in [(0, 1), (0, -1), (1, 0), (-1, 0)] {
                        let q = Pos {
                            col: p.col + u,
                            row: p.row + v,
                        };
                        if q.col < 0
                            || q.col >= grid.width() as i32
                            || q.row < 0
                            || q.row >= grid.height() as i32
                        {
                            continue;
                        }

                        if grid.grid[q.row as usize][q.col as usize] == Cell::Forest {
                            continue;
                        }

                        expand.push(q);
                    }

                    if p != pos && (p == start || p == end || expand.len() > 2) {
                        dst.insert((p, s));
                        intersections.insert(p);
                        continue;
                    } else {
                        for q in expand {
                            if vs.contains(&q) {
                                continue;
                            } else {
                                vs.insert(q);
                            }
                            stack.push((q, s + 1));
                        }
                    }
                }
                nodes.insert(pos, dst);
            }
        }

        nodes.retain(|p, _| intersections.contains(p));

        Self { nodes, start, end }
    }
}

fn part2(input: &str) -> usize {
    let grid = Grid::parse(input);
    let graph = Graph::new(&grid);

    let mut steps: HashMap<Pos, (Vec<Pos>, usize)> = HashMap::new();
    let mut stack = vec![(graph.start, 0, vec![graph.start])];

    while let Some((q, s, vs)) = stack.pop() {
        if steps.contains_key(&q) {
            if s > steps[&q].1 {
                steps.insert(q, (vs.clone(), s));
            }
        } else {
            steps.insert(q, (vs.clone(), s));
        }

        for (dst, dist) in &graph.nodes[&q] {
            if *dst == q {
                continue;
            }

            if vs.contains(&dst) {
                continue;
            } else {
                let mut vs = vs.clone();
                vs.push(*dst);
                stack.push((*dst, s + dist, vs));
            }
        }
    }

    steps[&graph.end].1
}

fn main() {
    let input = include_str!("../in.txt");
    println!("Part 1: {}", part1(input));
    println!("Part 2: {}", part2(input));
}

#[test]
fn example1() {
    let input = include_str!("../ex1.txt");
    assert_eq!(part1(input), 94);
    assert_eq!(part2(input), 154);
}
