use std::collections::{HashMap, VecDeque};

use delegate::delegate;
use nom::{
    bytes::complete::tag,
    character::complete::{alpha1, char},
    multi::separated_list1,
    IResult,
};
use num::integer::lcm;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Pulse {
    High,
    Low,
}

trait Relay {
    fn relay(&mut self, from: &'static str, pulse: Pulse) -> Vec<(&'static str, Pulse)>;
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Broadcaster {
    output: Vec<&'static str>,
}

impl Broadcaster {
    fn parse(i: &'static str) -> IResult<&str, Self> {
        let (i, _) = tag("broadcaster -> ")(i)?;
        let (i, ms) = separated_list1(tag(", "), alpha1)(i)?;

        Ok((i, Self { output: ms }))
    }
}

impl Relay for Broadcaster {
    fn relay(&mut self, _from: &'static str, pulse: Pulse) -> Vec<(&'static str, Pulse)> {
        self.output.iter().map(|m| (*m, pulse)).collect()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct FlipFlop {
    label: &'static str,
    on: bool,
    output: Vec<&'static str>,
}

impl FlipFlop {
    fn parse(i: &'static str) -> IResult<&str, Self> {
        let (i, _) = char('%')(i)?;
        let (i, label) = alpha1(i)?;
        let (i, _) = tag(" -> ")(i)?;
        let (i, ms) = separated_list1(tag(", "), alpha1)(i)?;

        Ok((
            i,
            Self {
                label,
                on: false,
                output: ms,
            },
        ))
    }
}

impl Relay for FlipFlop {
    fn relay(&mut self, _from: &'static str, pulse: Pulse) -> Vec<(&'static str, Pulse)> {
        match pulse {
            Pulse::High => vec![],
            Pulse::Low => {
                self.on ^= true;
                if self.on {
                    self.output.iter().map(|m| (*m, Pulse::High)).collect()
                } else {
                    self.output.iter().map(|m| (*m, Pulse::Low)).collect()
                }
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Conjunction {
    label: &'static str,
    memory: HashMap<&'static str, Pulse>,
    output: Vec<&'static str>,
}

impl Conjunction {
    fn parse(i: &'static str) -> IResult<&str, Self> {
        let (i, _) = char('&')(i)?;
        let (i, label) = alpha1(i)?;
        let (i, _) = tag(" -> ")(i)?;
        let (i, ms) = separated_list1(tag(", "), alpha1)(i)?;

        Ok((
            i,
            Self {
                label,
                memory: Default::default(),
                output: ms,
            },
        ))
    }
}

impl Relay for Conjunction {
    fn relay(&mut self, from: &'static str, pulse: Pulse) -> Vec<(&'static str, Pulse)> {
        self.memory.insert(from, pulse);

        if self.memory.values().all(|p| *p == Pulse::High) {
            self.output.iter().map(|m| (*m, Pulse::Low)).collect()
        } else {
            self.output.iter().map(|m| (*m, Pulse::High)).collect()
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Module {
    Broadcaster(Broadcaster),
    FlipFlop(FlipFlop),
    Conjunction(Conjunction),
}

impl Module {
    fn label(&self) -> &'static str {
        match self {
            Module::Broadcaster(_) => "broadcaster",
            Module::FlipFlop(m) => m.label,
            Module::Conjunction(m) => m.label,
        }
    }

    fn output(&self) -> &[&'static str] {
        match self {
            Module::Broadcaster(m) => &m.output,
            Module::FlipFlop(m) => &m.output,
            Module::Conjunction(m) => &m.output,
        }
    }

    fn parse(i: &'static str) -> IResult<&str, Self> {
        if let Ok((i, m)) = Broadcaster::parse(i) {
            Ok((i, Self::Broadcaster(m)))
        } else if let Ok((i, m)) = FlipFlop::parse(i) {
            Ok((i, Self::FlipFlop(m)))
        } else {
            let (i, m) = Conjunction::parse(i)?;
            Ok((i, Self::Conjunction(m)))
        }
    }
}

impl Relay for Module {
    delegate! {
        to match self {
            Self::Broadcaster(m) => m,
            Self::FlipFlop(m) => m,
            Self::Conjunction(m) => m,
        } {
            fn relay(&mut self, from: &'static str, pulse: Pulse) -> Vec<(&'static str, Pulse)>;
        }
    }
}

fn modules(input: &'static str) -> HashMap<&'static str, Module> {
    let mut modules = HashMap::new();

    for line in input.lines() {
        let m = Module::parse(line).unwrap().1;
        modules.insert(m.label(), m);
    }

    let _modules = modules.clone();
    for m in _modules.values() {
        for o in m.output() {
            if *o == "output" {
                continue;
            }
            if let Some(Module::Conjunction(con)) = modules.get_mut(o) {
                con.memory.insert(m.label(), Pulse::Low);
            }
        }
    }

    modules
}

fn part1(input: &'static str) -> usize {
    let mut modules = modules(input);

    let mut high = 0;
    let mut low = 0;
    for _ in 0..1000 {
        let mut queue = VecDeque::new();
        queue.push_front(("button", Pulse::Low, "broadcaster"));
        while let Some((from, pulse, to)) = queue.pop_front() {
            match pulse {
                Pulse::High => high += 1,
                Pulse::Low => low += 1,
            }
            if let Some(m1) = modules.get_mut(to) {
                for m in m1.relay(from, pulse).iter() {
                    queue.push_back((to, m.1, m.0));
                }
            }
        }
    }

    high * low
}

struct Group {
    input: &'static str,
    output: &'static str,
    modules: Vec<&'static str>,
}

fn solve_group(mut modules: HashMap<&'static str, Module>, group: &Group) -> usize {
    let init = modules.clone();
    let mut i = 0;
    loop {
        let mut queue = VecDeque::new();
        queue.push_front(("broadcaster", Pulse::Low, group.input));
        while let Some((from, pulse, to)) = queue.pop_front() {
            if from == group.output && !group.modules.contains(&to) {
                continue;
            }
            if let Some(m1) = modules.get_mut(to) {
                for m in m1.relay(from, pulse).iter() {
                    queue.push_back((to, m.1, m.0));
                }
            }
        }
        if modules == init {
            break;
        }
        i += 1;
    }
    i
}

fn part2(input: &'static str) -> usize {
    let modules = modules(input);

    let group1 = Group {
        input: "ct",
        output: "mh",
        modules: vec![
            "vs", "pg", "lz", "tk", "vk", "jg", "fr", "fl", "tp", "mj", "xg",
        ],
    };
    let group2 = Group {
        input: "hr",
        output: "zz",
        modules: vec![
            "lh", "pz", "fs", "mp", "mf", "lj", "zq", "cx", "jk", "th", "bh",
        ],
    };
    let group3 = Group {
        input: "ft",
        output: "cm",
        modules: vec![
            "jp", "qd", "pc", "sk", "qn", "rp", "xl", "gb", "rp", "bk", "bd", "lg",
        ],
    };
    let group4 = Group {
        input: "qm",
        output: "kd",
        modules: vec![
            "rn", "ml", "hv", "kb", "qj", "gk", "ks", "vq", "qv", "nv", "xk", "ks",
        ],
    };

    let p1 = solve_group(modules.clone(), &group1);
    let p2 = solve_group(modules.clone(), &group2);
    let p3 = solve_group(modules.clone(), &group3);
    let p4 = solve_group(modules.clone(), &group4);

    lcm(lcm(lcm(p1, p2), p3), p4)
}

fn main() {
    let input = include_str!("../in.txt");

    let p1 = part1(input);
    println!("Part 1: {}", p1);

    let p2 = part2(input);
    println!("Part 2: {}", p2);
}

#[test]
fn example1() {
    let input = include_str!("../ex1.txt");
    assert_eq!(part1(input), 32000000);
}

#[test]
fn example2() {
    let input = include_str!("../ex2.txt");
    assert_eq!(part1(input), 11687500);
}
