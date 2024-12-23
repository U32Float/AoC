#![feature(pattern)]

use std::{
    collections::{HashMap, HashSet},
    str::pattern::Pattern,
};

use itertools::Itertools;

// -----------------------------------------------------------------------------

fn parse(input: &'static str) -> HashSet<(&'static str, &'static str)> {
    let mut cs = HashSet::new();
    input.lines().for_each(|line| {
        let mut ps = line.split("-");
        let (p, q) = (ps.next().unwrap(), ps.next().unwrap());
        cs.insert((p, q));
        cs.insert((q, p));
    });
    cs
}

fn graph(
    cs: &HashSet<(&'static str, &'static str)>,
) -> HashMap<&'static str, HashSet<&'static str>> {
    let mut graph: HashMap<&str, HashSet<&str>> = HashMap::new();
    for (p, q) in cs {
        graph.entry(p).or_default().insert(q);
        graph.entry(q).or_default().insert(p);
    }
    graph
}

fn part1(input: &'static str) -> usize {
    let mut triangles = HashSet::new();
    let graph = graph(&parse(input));
    for p in graph.keys() {
        if !"t".is_prefix_of(p) {
            continue;
        }

        let qs = graph[p].clone();
        for q in &qs {
            for r in graph[q].intersection(&qs) {
                let mut t = [p, q, *r];
                t.sort();
                triangles.insert(t);
            }
        }
    }
    triangles.len()
}

fn part2(input: &'static str) -> String {
    let cs = parse(input);
    let graph = graph(&cs);

    struct State {
        lan: HashSet<&'static str>,
        ps: HashSet<&'static str>,
        xs: HashSet<&'static str>,
    }

    let mut best = HashSet::new();
    let mut stack = vec![State {
        lan: HashSet::new(),
        ps: graph.keys().copied().collect(),
        xs: HashSet::new(),
    }];

    while let Some(state) = stack.pop() {
        if state.ps.is_empty() && state.xs.is_empty() {
            if best.len() < state.lan.len() {
                best = state.lan.clone();
            }
            continue;
        }
        let mut ps = state.ps.clone();
        let mut xs = state.xs.clone();
        for p in &state.ps {
            stack.push(State {
                lan: {
                    let mut lan = state.lan.clone();
                    lan.insert(p);
                    lan
                },
                ps: ps.intersection(&graph[p]).cloned().collect(),
                xs: xs.intersection(&graph[p]).cloned().collect(),
            });
            ps.remove(p);
            xs.insert(p);
        }
    }

    let mut lan = best.into_iter().collect_vec();
    lan.sort();

    lan.join(",")
}

fn main() {
    let input = include_str!("../in.txt");
    println!("Part 1: {}", part1(input));
    println!("Part 2: {}", part2(input));
}

#[test]
fn example1() {
    let input = include_str!("../ex1.txt");
    assert_eq!(part1(input), 7);
    assert_eq!(part2(input), "co,de,ka,ta");
}
