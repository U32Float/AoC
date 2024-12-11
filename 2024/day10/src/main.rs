use std::{collections::HashSet, vec};

use itertools::Itertools;
use ndarray::{s, Array2};

// -----------------------------------------------------------------------------

struct Grid {
    grid: Array2<i32>,
    delta_left: Array2<i32>,
    delta_right: Array2<i32>,
    delta_up: Array2<i32>,
    delta_down: Array2<i32>,
}

impl Grid {
    fn parse(input: &str) -> Self {
        let nums = input
            .replace("\n", "")
            .chars()
            .map(|c| c.to_digit(10).unwrap() as i32)
            .collect_vec();
        let m = input.lines().count();

        let grid = Array2::from_shape_vec((m, nums.len() / m), nums).unwrap();

        let mut delta_left = Array2::zeros(grid.dim());
        let mut delta_right = Array2::zeros(grid.dim());
        let mut delta_up = Array2::zeros(grid.dim());
        let mut delta_down = Array2::zeros(grid.dim());

        delta_left
            .slice_mut(s![.., 1..])
            .assign(&(&grid.slice(s![.., ..-1]) - &grid.slice(s![.., 1..])));
        delta_right
            .slice_mut(s![.., ..-1])
            .assign(&(&grid.slice(s![.., 1..]) - &grid.slice(s![.., ..-1])));
        delta_up
            .slice_mut(s![1.., ..])
            .assign(&(&grid.slice(s![..-1, ..]) - &grid.slice(s![1.., ..])));
        delta_down
            .slice_mut(s![..-1, ..])
            .assign(&(&grid.slice(s![1.., ..]) - &grid.slice(s![..-1, ..])));

        Self {
            grid,
            delta_left,
            delta_right,
            delta_up,
            delta_down,
        }
    }

    fn trail_score(&self, start: (usize, usize)) -> usize {
        let mut nines = HashSet::new();
        let mut stack = vec![start];
        while let Some((i, j)) = stack.pop() {
            if self.grid[(i, j)] == 9 {
                nines.insert((i, j));
                continue;
            }

            if self.delta_left[(i, j)] == 1 {
                stack.push((i, j - 1));
            }
            if self.delta_right[(i, j)] == 1 {
                stack.push((i, j + 1));
            }
            if self.delta_up[(i, j)] == 1 {
                stack.push((i - 1, j));
            }
            if self.delta_down[(i, j)] == 1 {
                stack.push((i + 1, j));
            }
        }
        nines.len()
    }

    fn rating(&self, start: (usize, usize)) -> usize {
        let mut rating = 0;
        let mut stack = vec![(start)];
        while let Some((i, j)) = stack.pop() {
            if self.grid[(i, j)] == 9 {
                rating += 1;
                continue;
            }

            if self.delta_left[(i, j)] == 1 {
                stack.push((i, j - 1));
            }
            if self.delta_right[(i, j)] == 1 {
                stack.push((i, j + 1));
            }
            if self.delta_up[(i, j)] == 1 {
                stack.push((i - 1, j));
            }
            if self.delta_down[(i, j)] == 1 {
                stack.push((i + 1, j));
            }
        }
        rating
    }
}

fn part1(input: &str) -> usize {
    let grid = Grid::parse(input);

    let trail_heads = grid
        .grid
        .indexed_iter()
        .filter(|(_, value)| **value == 0)
        .map(|(index, _)| index)
        .collect_vec();

    trail_heads
        .iter()
        .map(|&start| grid.trail_score(start))
        .sum()
}

fn part2(input: &str) -> usize {
    let grid = Grid::parse(input);

    let trail_heads = grid
        .grid
        .indexed_iter()
        .filter(|(_, value)| **value == 0)
        .map(|(index, _)| index)
        .collect_vec();

    trail_heads.iter().map(|&start| grid.rating(start)).sum()
}

fn main() {
    let input = include_str!("../in.txt");
    println!("Part 1: {}", part1(input));
    println!("Part 2: {}", part2(input));
}

#[test]
fn example1() {
    let input = include_str!("../ex2.txt");
    assert_eq!(part1(input), 36);
    assert_eq!(part2(input), 81);
}
