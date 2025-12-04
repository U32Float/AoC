use itertools::Itertools;
use std::collections::HashSet;

fn adjacents((y, x): (i32, i32)) -> [(i32, i32); 8] {
    [
        (-1, 0),
        (1, 0),
        (0, -1),
        (0, 1),
        (-1, -1),
        (-1, 1),
        (1, -1),
        (1, 1),
    ]
    .iter()
    .map(|(v, u)| (y + v, u + x))
    .collect_array()
    .unwrap()
}

struct Grid {
    width: i32,
    height: i32,
    grid: HashSet<(i32, i32)>,
}

impl Grid {
    fn parse(input: &str) -> Self {
        let width = input.lines().next().unwrap().len() as i32;
        let height = input.lines().count() as i32;
        let grid = input
            .lines()
            .enumerate()
            .fold(HashSet::new(), |mut grid, (y, l)| {
                l.chars().enumerate().for_each(|(x, c)| {
                    if c == '@' {
                        grid.insert((y as i32, x as i32));
                    }
                });
                grid
            });
        Self {
            width,
            height,
            grid,
        }
    }

    fn find_removables(
        &self,
        mask: impl IntoIterator<Item = (i32, i32)>,
    ) -> impl Iterator<Item = (i32, i32)> {
        mask.into_iter().filter(|(y, x)| {
            if self.grid.contains(&(*y, *x)) {
                adjacents((*y, *x))
                    .iter()
                    .map(|(v, u)| self.grid.contains(&(*v, *u)) as usize)
                    .sum::<usize>()
                    < 4
            } else {
                false
            }
        })
    }
}

fn solve(input: &str, max_one_round: bool) -> usize {
    let mut grid = Grid::parse(input);
    let mut mask = (0..grid.height)
        .cartesian_product(0..grid.width)
        .collect_vec();
    let mut count = 0;
    loop {
        let to_remove = grid
            .find_removables(mask.clone())
            .collect::<HashSet<(i32, i32)>>();
        if to_remove.is_empty() {
            break;
        }

        count += to_remove.len();
        if max_one_round {
            return count;
        }

        grid.grid = grid.grid.difference(&to_remove).copied().collect();
        mask = to_remove.iter().copied().flat_map(adjacents).collect();
    }
    count
}

fn part1(input: &str) -> usize {
    solve(input, true)
}

fn part2(input: &str) -> usize {
    solve(input, false)
}

fn main() {
    let input = include_str!("../in.txt");
    println!("Part 1: {}", part1(input));
    println!("Part 2: {}", part2(input));
}

#[test]
fn example1() {
    let input = include_str!("../ex1.txt");
    assert_eq!(part1(input), 13);
    assert_eq!(part2(input), 43);
}
