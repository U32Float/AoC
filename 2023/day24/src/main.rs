use std::ops::Sub;

use itertools::Itertools;
use num_traits::Num;

#[derive(Debug, Clone, Copy)]
struct Vec3<T> {
    x: T,
    y: T,
    z: T,
}

impl<T: Num + Copy> Vec3<T> {
    fn parse(input: &str) -> Self {
        let s = input
            .split(", ")
            .map(|s| s.trim_start())
            .map(|s| T::from_str_radix(s, 10).unwrap_or(T::zero()))
            .collect::<Vec<_>>();

        Self {
            x: s[0],
            y: s[1],
            z: s[2],
        }
    }

    fn cross_product(self, other: Self) -> T {
        self.x * other.y - self.y * other.x
    }
}

impl Vec3<i64> {
    fn to_f64(self) -> Vec3<f64> {
        Vec3 {
            x: self.x as f64,
            y: self.y as f64,
            z: self.z as f64,
        }
    }
}

impl<T: Num> Sub for Vec3<T> {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        Self {
            x: self.x - rhs.x,
            y: self.y - rhs.y,
            z: self.z - rhs.z,
        }
    }
}

impl<T: Num> std::ops::Add for Vec3<T> {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Self {
            x: self.x + rhs.x,
            y: self.y + rhs.y,
            z: self.z + rhs.z,
        }
    }
}

impl<T: Copy + Num> std::ops::Mul<T> for Vec3<T> {
    type Output = Self;

    fn mul(self, rhs: T) -> Self::Output {
        Self {
            x: self.x * rhs,
            y: self.y * rhs,
            z: self.z * rhs,
        }
    }
}

#[derive(Debug, Clone, Copy)]
struct Hailstone {
    position: Vec3<i64>,
    velocity: Vec3<i64>,
}

impl Hailstone {
    fn parse(input: &str) -> Self {
        let (p, v) = input.split_once(" @ ").unwrap();

        Self {
            position: Vec3::parse(p),
            velocity: Vec3::parse(v),
        }
    }

    fn intersection(&self, other: &Self) -> Option<Vec3<f64>> {
        let p = self.position.to_f64();
        let r = self.velocity.to_f64() * 800000000000000.;
        let q = other.position.to_f64();
        let s = other.velocity.to_f64() * 800000000000000.;

        let d = r.cross_product(s);
        let e = (q - p).cross_product(r);

        if d == 0. && e == 0. {
            Some(Vec3 {
                x: (q - p).cross_product(r) / r.cross_product(r),
                y: (q + s - p).cross_product(r) / r.cross_product(r),
                z: 0.0,
            })
        } else if d == 0. && e != 0. {
            None
        } else {
            let t = (q - p).cross_product(s) / d;
            let u = (q - p).cross_product(r) / d;

            if d != 0. && (0.0..=1.).contains(&t) && (0.0..=1.).contains(&u) {
                Some(p + r * t)
            } else {
                None
            }
        }
    }
}

fn part1(input: &str, (min, max): (i64, i64)) -> usize {
    let hailstones = input.lines().map(Hailstone::parse).collect::<Vec<_>>();

    hailstones
        .iter()
        .tuple_combinations()
        .map(|(h1, h2)| h1.intersection(h2))
        .filter(|p| match p {
            Some(p) => (min..=max).contains(&(p.x as i64)) && (min..=max).contains(&(p.y as i64)),
            None => false,
        })
        .count()
}

fn main() {
    let input = include_str!("../in.txt");
    println!(
        "Part 1: {}",
        part1(input, (200000000000000, 400000000000000))
    );
}

#[test]
fn example1() {
    let input = include_str!("../ex1.txt");
    assert_eq!(part1(input, (7, 27)), 2);
}
