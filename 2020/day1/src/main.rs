const N: u64 = 2020;

fn part1(input: &[u64]) -> u64 {
    let x: u64;
    unsafe {
        std::arch::asm!("
            1:
                mov x0, {1}
                mov x1, {2}
                mov x2, #0
                mov x3, #8
                ldr x4, [{3}]
                mov x5, #8
                mul x1, x1, x5
                b 2f
            2:
                add x6, x0, x2
                ldr x6, [x6]
                add x8, x0, x3
                ldr x8, [x8]
                add x5, x6, x8
                cmp x5, x4
                beq 3f
                add x3, x3, #8
                cmp x3, x1
                bne 2b
                add x2, x2, #8
                mov x3, x2
                add x3, x3, #8
                b 2b
            3:
                mul {0}, x6, x8
        ",
        out(reg) x,
        in(reg) input.as_ptr(),
        in(reg) input.len(),
        in(reg) &N
        );
    }
    x
}

fn part2(input: &[u64]) -> u64 {
    let x: u64;
    unsafe {
        std::arch::asm!("
            1:
                mov x0, {1}
                mov x1, {2}
                mov x2, #0
                mov x3, #8
                mov x12, #16
                ldr x4, [{3}]
                mov x5, #8
                mul x1, x1, x5
                mov x11, x1
                sub x11, x11, #8
                b 2f
            2:
                add x6, x0, x2
                ldr x6, [x6]
                add x8, x0, x3
                ldr x8, [x8]
                add x10, x0, x12
                ldr x10, [x10]
                add x5, x6, x8
                add x5, x5, x10
                cmp x5, x4
                beq 3f
                add x12, x12, #8
                cmp x12, x1
                bne 2b
                add x3, x3, #8
                mov x12, x3
                add x12, x12, #8
                cmp x3, x11
                bne 2b
                add x2, x2, #8 
                mov x3, x2
                add x3, x3, #8
                mov x12, x3
                add x12, x12, #8
                b 2b
            3:
                mul x5, x6, x8
                mul {0}, x5, x10
        ",
        out(reg) x,
        in(reg) input.as_ptr(),
        in(reg) input.len(),
        in(reg) &N
        );
    }
    x
}

#[cfg(target_arch = "aarch64")]
fn main() {
    let args: Vec<_> = std::env::args().collect();
    let file = if args.len() > 1 {
        args[1].as_str()
    } else {
        "ex1"
    };
    let input: Vec<u64> = std::fs::read_to_string(format!("{file}.txt"))
        .unwrap()
        .lines()
        .map(|x| x.parse::<u64>().unwrap())
        .collect();

    println!("Part 1: {}", part1(&input));
    println!("Part 2: {}", part2(&input));
}
