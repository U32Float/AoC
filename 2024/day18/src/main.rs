use std::collections::{BinaryHeap, HashMap, HashSet};

fn parse(input: &str) -> Vec<(usize, usize)> {
    input
        .lines()
        .map(|l| {
            let mut xs = l.split(",").map(|x| x.parse().unwrap());
            (xs.next().unwrap(), xs.next().unwrap())
        })
        .collect()
}

fn part1(input: &str, size: usize, n: usize) -> usize {
    let bytes = parse(input);
    let grid = {
        let mut grid = vec![vec![false; size]; size];
        for (x, y) in &bytes[..n] {
            grid[*y][*x] = true;
        }
        grid
    };

    let mut vs = HashSet::new();
    let mut queue = BinaryHeap::new();
    queue.push((0i32, (0, 0)));

    while let Some((steps, (y, x))) = queue.pop() {
        if vs.contains(&(y, x)) {
            continue;
        }
        vs.insert((y, x));

        if (y as usize, x as usize) == (size - 1, size - 1) {
            return steps.unsigned_abs() as usize;
        }

        for &(dy, dx) in &[(0, 1), (0, -1), (1, 0), (-1, 0)] {
            let v = y + dy;
            let u = x + dx;
            if v < 0 || v >= size as i32 || u < 0 || u >= size as i32 {
                continue;
            }
            if !grid[v as usize][u as usize] {
                queue.push((steps - 1, (v, u)));
            }
        }
    }
    unreachable!()
}

fn part2(input: &str, size: usize) -> (usize, usize) {
    let bytes = parse(input);
    let grid = {
        let mut grid = HashMap::new();
        for (i, byte) in bytes.iter().enumerate() {
            grid.insert(*byte, i);
        }
        grid
    };

    #[derive(PartialEq, Eq)]
    struct Path {
        pos: (i32, i32),
        path: HashSet<(i32, i32)>,
        first_cut_off: Option<usize>,
    }
    impl PartialOrd for Path {
        fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
            Some(self.cmp(other))
        }
    }
    impl Ord for Path {
        fn cmp(&self, other: &Self) -> std::cmp::Ordering {
            self.first_cut_off
                .unwrap_or(0)
                .cmp(&other.first_cut_off.unwrap_or(0))
        }
    }

    let mut latest_cut_off = 0;
    let mut queue = BinaryHeap::new();
    queue.push(Path {
        pos: (0, 0),
        path: HashSet::from([(0, 0)]),
        first_cut_off: None,
    });

    while let Some(path) = queue.pop() {
        let Path {
            pos: (y, x),
            path,
            first_cut_off,
        } = path;

        for &(dy, dx) in &[(0, 1), (0, -1), (1, 0), (-1, 0)] {
            let v = y + dy;
            let u = x + dx;
            if v < 0 || v >= size as i32 || u < 0 || u >= size as i32 {
                continue;
            }
            if path.contains(&(v, u)) {
                continue;
            }
            let new_path = {
                let mut p = path.clone();
                p.insert((v, u));
                p
            };

            let cut_off = if let Some(cut_off) = grid.get(&(v as usize, u as usize)) {
                Some(first_cut_off.map(|c| c.min(*cut_off)).unwrap_or(*cut_off))
            } else {
                first_cut_off
            };

            if let Some(c) = cut_off {
                if c <= latest_cut_off {
                    continue;
                }
            }

            if (v as usize, u as usize) == (size - 1, size - 1) {
                latest_cut_off = cut_off.unwrap_or(latest_cut_off).max(latest_cut_off);
                continue;
            }

            queue.push(Path {
                pos: (v, u),
                path: new_path,
                first_cut_off: cut_off,
            });
        }
    }
    bytes[latest_cut_off]
}

fn main() {
    let input = include_str!("../in.txt");
    println!("Part 1: {}", part1(input, 71, 1024));
    println!("Part 2: {:?}", part2(input, 71));
}

#[test]
fn example1() {
    let input = include_str!("../ex1.txt");
    assert_eq!(part1(input, 7, 12), 22);
    assert_eq!(part2(input, 7), (6, 1));
}
