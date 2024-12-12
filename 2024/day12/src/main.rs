use std::collections::HashSet;

use itertools::Itertools;

// -----------------------------------------------------------------------------

type Point<T> = (T, T);
type Side<T> = (Point<T>, Point<T>);

fn flood_fill(
    grid: &[Vec<char>],
    (y, x): Point<usize>,
) -> (HashSet<Point<usize>>, HashSet<Side<i32>>, usize) {
    let plant = grid[y][x];
    let mut area = HashSet::new();
    let mut bounds = HashSet::new();
    let mut perimeter = 0;
    let mut stack = vec![(y, x)];
    while let Some((y, x)) = stack.pop() {
        if area.contains(&(y, x)) {
            continue;
        }
        area.insert((y, x));
        for (dy, dx) in [(-1, 0), (1, 0), (0, -1), (0, 1)] {
            let (v, u) = (y as i32 + dy, x as i32 + dx);
            if v < 0 || u < 0 {
                bounds.insert(((v, u), (dy, dx)));
                perimeter += 1;
                continue;
            }

            if area.contains(&(v as usize, u as usize)) {
                continue;
            }

            let n = grid
                .get(v as usize)
                .and_then(|r| r.get(u as usize))
                .unwrap_or(&'$');
            if *n == plant {
                stack.push((v as usize, u as usize));
            } else {
                bounds.insert(((v, u), (dy, dx)));
                perimeter += 1;
            }
        }
    }

    (area, bounds, perimeter)
}

fn num_sides(bounds: &HashSet<Side<i32>>, perimeter: usize) -> usize {
    let mut n = 0;
    let mut vs = HashSet::new();
    for &side in bounds {
        if vs.contains(&side) {
            continue;
        }
        vs.insert(side);

        let ((y, x), dir) = side;

        let mut c = 0;
        for (dy, dx) in [(-1, 0), (1, 0), (0, -1), (0, 1)] {
            let mut i = 1;
            loop {
                let (v, u) = (y + dy * i, x + dx * i);
                if bounds.contains(&((v, u), dir)) {
                    vs.insert(((v, u), dir));
                    c += 1;
                    i += 1;
                } else {
                    break;
                }
            }
        }
        if c >= 1 {
            n += c;
        }
    }
    perimeter - n
}

fn solve(input: &str, sides: bool) -> usize {
    let grid = input.lines().map(|l| l.chars().collect_vec()).collect_vec();
    let (w, h) = (grid[0].len(), grid.len());

    let mut price = 0;
    let mut vs = HashSet::new();
    for (y, x) in (0..h).cartesian_product(0..w) {
        if vs.contains(&(y, x)) {
            continue;
        }
        let (area, bounds, perimeter) = flood_fill(&grid, (y, x));
        vs = vs.union(&area).copied().collect();

        if sides {
            price += area.len() * num_sides(&bounds, perimeter);
        } else {
            price += area.len() * perimeter;
        }
    }

    price
}

fn part1(input: &str) -> usize {
    solve(input, false)
}

fn part2(input: &str) -> usize {
    solve(input, true)
}

fn main() {
    let input = include_str!("../in.txt");
    println!("Part 1: {}", part1(input));
    println!("Part 2: {}", part2(input));
}

#[test]
fn example1() {
    let input = include_str!("../ex1.txt");
    assert_eq!(part1(input), 140);
    assert_eq!(part2(input), 80);
}

#[test]
fn example2() {
    let input = include_str!("../ex2.txt");
    assert_eq!(part1(input), 1930);
    assert_eq!(part2(input), 1206);
}
