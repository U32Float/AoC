use std::collections::VecDeque;

fn hash(input: &str) -> usize {
    let mut h = 0;
    for char in input.chars() {
        h += char as usize;
        h *= 17;
        h %= 256;
    }
    h
}

fn part1(input: &str) -> usize {
    input.split(',').map(hash).sum()
}

#[derive(Default, Debug)]
struct Box {
    lenses: VecDeque<(&'static str, usize)>,
}

impl Box {
    fn set_lens(&mut self, label: &'static str, focal_length: usize) {
        if let Some(idx) = self.lenses.iter().position(|(l, _)| *l == label) {
            self.lenses[idx].1 = focal_length;
        } else {
            self.lenses.push_back((label, focal_length));
        }
    }

    fn remove_lens(&mut self, label: &'static str) {
        if let Some(idx) = self.lenses.iter().position(|(l, _)| *l == label) {
            self.lenses.remove(idx);
        }
    }

    fn focusing_power(&self) -> usize {
        self.lenses
            .iter()
            .enumerate()
            .map(|(i, (_, fl))| (i + 1) * fl)
            .sum()
    }
}

fn part2(input: &'static str) -> usize {
    let mut boxes: [Box; 256] = {
        let mut tmp = Vec::with_capacity(256);
        for _ in 0..256 {
            tmp.push(Box::default());
        }
        tmp.try_into().unwrap()
    };

    for instr in input.split(',') {
        if instr.contains('=') {
            let (label, fl) = instr.split_once('=').unwrap();
            let idx = hash(label);
            boxes[idx].set_lens(label, fl.parse().unwrap());
        } else {
            let label = &instr[..instr.len() - 1];
            let idx = hash(label);
            boxes[idx].remove_lens(label);
        }
    }

    boxes
        .iter()
        .enumerate()
        .map(|(i, b)| (i + 1) * b.focusing_power())
        .sum()
}

fn main() {
    let input = include_str!("../in.txt").trim();

    let p1 = part1(input);
    println!("Part 1: {}", p1);

    let p2 = part2(input);
    println!("Part 2: {}", p2);
}

#[test]
fn example1() {
    let input = include_str!("../ex1.txt").trim();
    assert_eq!(part1(input), 1320);
    assert_eq!(part2(input), 145);
}
