struct Pattern {
    rows: Vec<Vec<bool>>,
    cols: Vec<Vec<bool>>,
}

impl Pattern {
    fn new(rows: Vec<Vec<bool>>) -> Self {
        let mut cols = vec![];
        for i in 0..rows[0].len() {
            let mut col = vec![];

            for j in 0..rows.len() {
                col.push(rows[j][i]);
            }

            cols.push(col);
        }

        Self { rows, cols }
    }

    fn width(&self) -> usize {
        self.rows[0].len()
    }

    fn height(&self) -> usize {
        self.rows.len()
    }
}

fn parse(input: &str) -> Vec<Pattern> {
    let mut ps = vec![];
    let mut tmp = vec![];

    for line in input.lines() {
        if line == "" {
            ps.push(Pattern::new(tmp));
            tmp = vec![];
            continue;
        }

        let mut row = vec![];
        for char in line.chars() {
            row.push(match char {
                '#' => true,
                '.' => false,
                _ => panic!(),
            });
        }
        tmp.push(row);
    }
    ps.push(Pattern::new(tmp));

    ps
}

fn num_smudges(a: &[bool], b: &[bool]) -> usize {
    let mut total = 0;

    for i in 0..a.len() {
        total += (a[i] != b[i]) as usize;
    }

    total
}

enum Mirror {
    Vertical(usize),
    SmudgedVertical(usize),
    Horizontal(usize),
    SmudgedHorizontal(usize),
}

fn find_mirrors(pattern: &Pattern) -> Vec<Mirror> {
    let mut mirrors = vec![];

    for row in 0..pattern.height() - 1 {
        let mut is_mirror = true;
        let mut smudges = 0;
        let mut i = 0;
        loop {
            if (row as i32 - i as i32) < 0 || row + 1 + i >= pattern.height() {
                break;
            }
            let n = num_smudges(&pattern.rows[row - i], &pattern.rows[row + 1 + i]);
            if n > 1 {
                is_mirror = false;
                break;
            }

            smudges += n;
            i += 1;
        }

        match (is_mirror, smudges) {
            (true, 0) => mirrors.push(Mirror::Horizontal(row)),
            (true, 1) => mirrors.push(Mirror::SmudgedHorizontal(row)),
            _ => (),
        }
    }

    for col in 0..pattern.width() - 1 {
        let mut is_mirror = true;
        let mut smudges = 0;
        let mut i = 0;
        loop {
            if (col as i32 - i as i32) < 0 || col + 1 + i >= pattern.width() {
                break;
            }
            let n = num_smudges(&pattern.cols[col - i], &pattern.cols[col + 1 + i]);
            if n > 1 {
                is_mirror = false;
                break;
            }

            smudges += n;
            i += 1;
        }

        match (is_mirror, smudges) {
            (true, 0) => mirrors.push(Mirror::Vertical(col)),
            (true, 1) => mirrors.push(Mirror::SmudgedVertical(col)),
            _ => (),
        }
    }

    mirrors
}

fn part1(input: &str) -> usize {
    let ps = parse(input);

    let mut total = 0;
    for pattern in ps {
        let ms = find_mirrors(&pattern);
        for m in ms {
            match m {
                Mirror::Vertical(col) => total += col + 1,
                Mirror::Horizontal(row) => total += 100 * (row + 1),
                _ => (),
            }
        }
    }

    total
}

fn part2(input: &str) -> usize {
    let ps = parse(input);

    let mut total = 0;
    for pattern in ps {
        let ms = find_mirrors(&pattern);
        for m in ms {
            match m {
                Mirror::SmudgedVertical(col) => total += col + 1,
                Mirror::SmudgedHorizontal(row) => total += 100 * (row + 1),
                _ => (),
            }
        }
    }

    total
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
    assert_eq!(part1(input), 405);
    assert_eq!(part2(input), 400);
}
