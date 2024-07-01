use std::{
    collections::HashMap,
    ops::{Index, IndexMut, Range},
};

use nom::{
    branch::alt,
    bytes::complete::{tag, take_while},
    character::complete::{alpha0, anychar, char, digit0, one_of},
    multi::separated_list1,
    IResult,
};

#[derive(Debug, Clone, Copy)]
enum Rating {
    X,
    M,
    A,
    S,
}

impl From<char> for Rating {
    fn from(value: char) -> Self {
        match value {
            'x' => Self::X,
            'm' => Self::M,
            'a' => Self::A,
            's' => Self::S,
            _ => panic!(),
        }
    }
}

#[derive(Debug, Clone, Copy)]
struct Part {
    x: usize,
    m: usize,
    a: usize,
    s: usize,
}

impl Part {
    fn parse(i: &str) -> IResult<&str, Self> {
        fn rating(i: &str) -> IResult<&str, usize> {
            let (i, _) = anychar(i)?;
            let (i, _) = char('=')(i)?;
            let (i, val) = digit0(i)?;

            Ok((i, val.parse().unwrap()))
        }

        let (i, _) = char('{')(i)?;
        let (i, rs) = separated_list1(tag(","), rating)(i)?;
        let (i, _) = char('}')(i)?;

        Ok((
            i,
            Self {
                x: rs[0],
                m: rs[1],
                a: rs[2],
                s: rs[3],
            },
        ))
    }

    fn total_rating(&self) -> usize {
        self.x + self.m + self.a + self.s
    }
}

impl Index<Rating> for Part {
    type Output = usize;

    fn index(&self, index: Rating) -> &Self::Output {
        match index {
            Rating::X => &self.x,
            Rating::M => &self.m,
            Rating::A => &self.a,
            Rating::S => &self.s,
        }
    }
}

#[derive(Debug, Clone)]
struct PartRange {
    x: Range<usize>,
    m: Range<usize>,
    a: Range<usize>,
    s: Range<usize>,
}

impl PartRange {
    fn possibilities(&self) -> usize {
        self.x.len() * self.m.len() * self.a.len() * self.s.len()
    }
}

impl Index<Rating> for PartRange {
    type Output = std::ops::Range<usize>;

    fn index(&self, index: Rating) -> &Self::Output {
        match index {
            Rating::X => &self.x,
            Rating::M => &self.m,
            Rating::A => &self.a,
            Rating::S => &self.s,
        }
    }
}

impl IndexMut<Rating> for PartRange {
    fn index_mut(&mut self, index: Rating) -> &mut Self::Output {
        match index {
            Rating::X => &mut self.x,
            Rating::M => &mut self.m,
            Rating::A => &mut self.a,
            Rating::S => &mut self.s,
        }
    }
}

#[derive(Debug, Copy, Clone)]
enum Return {
    Accept,
    Reject,
    Jump(&'static str),
}

impl Return {
    fn parse(i: &'static str) -> IResult<&str, Self> {
        match one_of::<_, _, ()>("AR")(i) {
            Ok((i, c)) => match c {
                'A' => Ok((i, Return::Accept)),
                'R' => Ok((i, Return::Reject)),
                _ => panic!(),
            },
            Err(_) => {
                let (i, label) = take_while(char::is_alphabetic)(i)?;
                Ok((i, Return::Jump(label)))
            }
        }
    }
}

#[derive(Debug)]
enum Expr {
    GT(Rating, usize, Return),
    LT(Rating, usize, Return),
    Return(Return),
}

impl Expr {
    fn parse(i: &'static str) -> IResult<&str, Self> {
        fn cmp(i: &'static str) -> IResult<&str, Expr> {
            let (i, r) = one_of("xmas")(i)?;
            let (i, cmp) = one_of("<>")(i)?;
            let (i, val) = digit0(i)?;
            let (i, _) = char(':')(i)?;
            let (i, ret) = Return::parse(i)?;

            match cmp {
                '>' => Ok((i, Expr::GT(r.into(), val.parse().unwrap(), ret))),
                '<' => Ok((i, Expr::LT(r.into(), val.parse().unwrap(), ret))),
                _ => panic!(),
            }
        }
        fn ret(i: &'static str) -> IResult<&str, Expr> {
            let (i, ret) = Return::parse(i)?;
            Ok((i, Expr::Return(ret)))
        }

        alt((cmp, ret))(i)
    }
}

#[derive(Debug)]
struct Rule {
    name: &'static str,
    exprs: Vec<Expr>,
}

impl Rule {
    fn parse(i: &'static str) -> IResult<&str, Rule> {
        let (i, name) = alpha0(i)?;
        let (i, _) = char('{')(i)?;
        let (i, exprs) = separated_list1(tag(","), Expr::parse)(i)?;
        let (i, _) = char('}')(i)?;

        Ok((i, Self { name, exprs }))
    }

    fn invoke(&self, part: Part) -> Return {
        for expr in &self.exprs {
            match expr {
                Expr::GT(rating, val, ret) => {
                    if part[*rating] > *val {
                        return *ret;
                    }
                }
                Expr::LT(rating, val, ret) => {
                    if part[*rating] < *val {
                        return *ret;
                    }
                }
                Expr::Return(ret) => return *ret,
            }
        }
        panic!()
    }
}

fn parse(input: &'static str) -> (HashMap<&'static str, Rule>, Vec<Part>) {
    let mut system = HashMap::new();
    let mut parts = vec![];

    let mut parse_rules = true;
    for line in input.lines() {
        if line == "" {
            parse_rules = false;
            continue;
        }

        if parse_rules {
            let rule = Rule::parse(line).unwrap().1;
            system.insert(rule.name, rule);
        } else {
            parts.push(Part::parse(line).unwrap().1);
        }
    }

    (system, parts)
}

fn part1(input: &'static str) -> usize {
    let (system, parts) = parse(input);

    parts
        .iter()
        .filter(|p| {
            let mut pc = "in";
            loop {
                match system[pc].invoke(**p) {
                    Return::Accept => return true,
                    Return::Reject => return false,
                    Return::Jump(label) => pc = label,
                }
            }
        })
        .map(|p| p.total_rating())
        .sum()
}

fn count(system: &HashMap<&'static str, Rule>, mut range: PartRange, ret: Return) -> usize {
    match ret {
        Return::Accept => range.possibilities(),
        Return::Reject => 0,
        Return::Jump(label) => {
            let mut total = 0;
            let rule = &system[label];
            for expr in &rule.exprs {
                match expr {
                    Expr::GT(rat, val, ret) => {
                        let mut r = range.clone();
                        r[*rat].start = (*val + 1).max(r[*rat].start);
                        total += count(system, r, *ret);

                        range[*rat].end = (*val + 1).min(range[*rat].end);
                    }
                    Expr::LT(rat, val, ret) => {
                        let mut r = range.clone();
                        r[*rat].end = (*val).min(r[*rat].end);
                        total += count(system, r, *ret);

                        range[*rat].start = (*val).max(range[*rat].start);
                    }
                    Expr::Return(ret) => total += count(system, range.clone(), *ret),
                }
            }
            total
        }
    }
}

fn part2(input: &'static str) -> usize {
    let (system, _) = parse(input);

    count(
        &system,
        PartRange {
            x: 1..4001,
            m: 1..4001,
            a: 1..4001,
            s: 1..4001,
        },
        Return::Jump("in"),
    )
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
    assert_eq!(part1(input), 19114);
    assert_eq!(part2(input), 167409079868000);
}
