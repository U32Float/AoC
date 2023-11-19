use std::collections::{HashMap, HashSet, VecDeque};

fn count(map: &HashMap<&str, Vec<&str>>, key: &str, acc: usize) -> usize {
    let mut c = 0;
    if let Some(vs) = map.get(key) {
        for v in vs {
            c += count(map, v, acc + 1) + acc;
        }
    }
    c
}

fn main() {
    let input = include_str!("../in.txt");

    let mut lines: Vec<&str> = input.split("\n").collect();
    lines.pop();
    let links: Vec<(&str, &str)> = lines
        .into_iter()
        .map(|s| {
            let split: Vec<&str> = s.split(")").collect();
            (split[0], split[1])
        })
        .collect();

    let mut map: HashMap<&str, Vec<&str>> = HashMap::new();
    let mut map2: HashMap<&str, Vec<&str>> = HashMap::new();
    for (a, b) in links {
        let entry = map.entry(a).or_insert(vec![]);
        entry.push(b);

        let entry = map2.entry(a).or_insert(vec![]);
        entry.push(b);
        let entry = map2.entry(b).or_insert(vec![]);
        entry.push(a);
    }

    println!("Part 1: {}", count(&map, "COM", 1));

    let mut visited: HashSet<Vec<&str>> = HashSet::new();
    let mut queue: VecDeque<(&str, isize, Vec<&str>)> = VecDeque::new();
    queue.push_front(("YOU", -2, vec![]));
    while let Some((key, c, path)) = queue.pop_back() {
        if visited.contains(&path) {
            continue;
        }
        visited.insert(path.clone());
        if key == "SAN" {
            println!("Part 2: {}", c);
            break;
        }
        for v in map2.get(key).unwrap() {
            let mut p2 = path.clone();
            if p2.contains(&v) {
                continue;
            }
            p2.push(v);
            queue.push_front((v, c + 1, p2));
        }
    }
}
