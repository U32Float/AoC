use ndarray::{s, Array3};

fn main() {
    #[cfg(debug_assertions)]
    let input = include_str!("../ex1.txt");
    #[cfg(debug_assertions)]
    let size = (2, 2);

    #[cfg(not(debug_assertions))]
    let input = include_str!("../in.txt");
    #[cfg(not(debug_assertions))]
    let size = (25, 6);

    let nums: Vec<u32> = input.chars().filter_map(|c| c.to_digit(10)).collect();
    let img =
        Array3::from_shape_vec((nums.len() / (size.0 * size.1), size.1, size.0), nums).unwrap();

    let zeros: Array3<usize> = img.mapv(|v| (v == 0) as usize);
    let count: Vec<usize> = (0..img.shape()[0])
        .map(|i| zeros.slice(s![i, .., ..]).sum())
        .collect();
    let (i, _) = count.iter().enumerate().min_by_key(|(_, c)| *c).unwrap();
    #[rustfmt::skip] let res = 
            img.index_axis(ndarray::Axis(0), i).mapv(|v| (v == 1) as usize).sum() *
            img.index_axis(ndarray::Axis(0), i).mapv(|v| (v == 2) as usize).sum();
    println!("Part 1: {}", res);

    let msg = img
        .map_axis(ndarray::Axis(0), |vs| {
            vs.fold(2, |acc, v| if acc == 2 { *v } else { acc })
        })
        .mapv(|v| if v == 1 { "#" } else { " " })
        .fold_axis(ndarray::Axis(1), String::new(), |v, acc| {
            format!("{}{}", acc, v)
        });
    println!("Part 2:");
    for line in msg.iter() {
        println!("{}", line.chars().rev().collect::<String>());
    }
}
