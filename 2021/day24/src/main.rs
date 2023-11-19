// mod test;

// #[inline]
// fn monad(num: &[u64]) -> u64 {
//     let x: u64;
//     unsafe {
//         std::arch::asm!(include_str!("../monad.asm"),
//         out(reg) x,
//         in(reg) num.as_ptr(),
//         );
//     }
//     x
// }

// #[inline]
// fn valid(n: i32) -> Option<u64> {
//     if n > 0 && n < 10 {
//         Some(n as u64)
//     } else {
//         None
//     }
// }

// #[inline]
// fn gen_num(n0: u64, n1: u64, n2: u64, n3: u64, n6: u64, n10: u64, n12: u64) -> Option<[u64; 14]> {
//     const A: [i32; 14] = [11, 13, 11, 10, -3, -4, 12, -8, -3, -12, 14, -6, 11, -12];
//     const B: [i32; 14] = [14, 8, 4, 10, 14, 10, 4, 14, 1, 6, 0, 9, 13, 12];
//     // S[-1] + B[i] + A[j] == N[j]
//     let n4 = valid(n3 as i32 + B[3] + A[4])?;
//     let n5 = valid(n2 as i32 + B[2] + A[5])?;
//     let n8 = valid(n1 as i32 + B[1] + A[8])?;
//     let n9 = valid(n0 as i32 + B[0] + A[9])?;
//     let n7 = valid(n6 as i32 + B[6] + A[7])?;
//     let n11 = valid(n10 as i32 + B[10] + A[11])?;
//     let n13 = valid(n12 as i32 + B[12] + A[13])?;
//     Some([n0, n1, n2, n3, n4, n5, n6, n7, n8, n9, n10, n11, n12, n13])
// }

// #[inline]
// fn cmp(a: &[u64], b: &[u64]) -> std::cmp::Ordering {
//     for i in 0..14 {
//         if a[i] < b[i] {
//             return std::cmp::Ordering::Less;
//         }
//     }
//     std::cmp::Ordering::Greater
// }

// #[rustfmt::skip]
// #[cfg(target_arch = "aarch64")]
// fn main() {
//     let mut m: [u64; 14] = [0; 14];
//     let mut n: [u64; 14] = [10; 14];
//     for n0 in 1..10 {
//     for n1 in 1..10 {
//     for n2 in 1..10 {
//     for n3 in 1..10 {
//     for n6 in 1..10 {
//     for n10 in 1..10 {
//     for n12 in 1..10 {
//         if let Some(num) = gen_num(n0, n1, n2, n3, n6, n10, n12) {

//         if monad(&num) == 0 {
//             if let std::cmp::Ordering::Greater = cmp(&num, &m) {
//                 m = num;
//             }
//             if let std::cmp::Ordering::Less = cmp(&num, &n) {
//                 n = num;
//             }
//         }
//     }
//     }}}}}}}
//     let l = m.iter().fold(0, |acc, x| acc * 10 + x);
//     let s = n.iter().fold(0, |acc, x| acc * 10 + x);

//     println!("Part 1: {l}");
//     println!("Part 2: {s}");
// }

fn main() {
    const LINKS: [usize; 14] = [9, 8, 5, 4, 3, 2, 7, 6, 1, 0, 11, 10, 13, 12];
    const A: [i32; 14] = [11, 13, 11, 10, -3, -4, 12, -8, -3, -12, 14, -6, 11, -12];
    const B: [i32; 14] = [14, 8, 4, 10, 14, 10, 4, 14, 1, 6, 0, 9, 13, 12];

    let mut n = [0; 14];
    for i in 0..14 {
        let l = LINKS[i];
        if l > i {
            let c = B[i] + A[l];
            if c >= 0 {
                n[i] = 9 - c;
                n[l] = 9;
            } else {
                n[i] = 9;
                n[l] = 9 + c;
            }
        }
    }
    let n: u64 = n.iter().fold(0, |acc, x| acc * 10 + *x as u64);
    println!("Part 1: {n}");

    let mut m = [0; 14];
    for i in 0..14 {
        let l = LINKS[i];
        if l > i {
            let c = B[i] + A[l];
            if c >= 0 {
                m[i] = 1;
                m[l] = c + 1;
            } else {
                m[i] = c.abs() + 1;
                m[l] = 1;
            }
        }
    }
    let m: u64 = m.iter().fold(0, |acc, x| acc * 10 + *x as u64);
    println!("Part 2: {m}");
}
