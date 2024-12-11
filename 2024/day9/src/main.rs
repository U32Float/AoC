use itertools::Itertools;

// -----------------------------------------------------------------------------

#[inline(always)]
pub fn post_inc<T: From<u8> + std::ops::AddAssign<T> + Copy>(value: &mut T) -> T {
    let prev = *value;
    *value += T::from(1);
    prev
}

#[derive(Debug)]
struct Space {
    ptr: usize,
    size: usize,
}

#[derive(Debug)]
struct Disk {
    files: Vec<(usize, Space)>,
    empty_spaces: Vec<Space>,
}

impl Disk {
    fn parse(input: &str) -> Self {
        let mut ptr = 0;
        let mut id = 0;
        let mut files = vec![];
        let mut empty_spaces = vec![];

        for mut chunk in &input.chars().chunks(2) {
            let file = chunk.next().unwrap().to_digit(10).unwrap() as usize;
            let empty = chunk
                .next()
                .map(|c| c.to_digit(10).unwrap() as usize)
                .unwrap_or(0);
            files.push((post_inc(&mut id), Space { ptr, size: file }));
            empty_spaces.push(Space {
                ptr: ptr + file,
                size: empty,
            });
            ptr += file + empty;
        }

        Self {
            files,
            empty_spaces,
        }
    }

    fn normalize(&mut self) {
        let mut files = vec![];
        for (id, file) in self.files.iter() {
            for i in 0..file.size {
                files.push((
                    *id,
                    Space {
                        ptr: file.ptr + i,
                        size: 1,
                    },
                ));
            }
        }
        self.files = files;
    }

    fn move_files(&mut self) {
        self.files.sort_by_key(|(id, _)| *id);

        for (_, file) in self.files.iter_mut().rev() {
            let space = self
                .empty_spaces
                .iter_mut()
                .find(|s| file.size <= s.size && s.ptr < file.ptr);
            if space.is_none() {
                continue;
            }
            let space = space.unwrap();

            file.ptr = space.ptr;
            space.size -= file.size;
            space.ptr += file.size;
        }
    }

    fn checksum(&mut self) -> usize {
        self.files.sort_by_key(|(_, space)| space.ptr);

        let mut checksum = 0;
        for (id, file) in &self.files {
            checksum += std::iter::repeat(id)
                .zip(file.ptr..file.ptr + file.size)
                .map(|(id, ptr)| id * ptr)
                .sum::<usize>();
        }

        checksum
    }
}

fn part1(input: &str) -> usize {
    let mut disk = Disk::parse(input);
    disk.normalize();
    disk.move_files();
    disk.checksum()
}

fn part2(input: &str) -> usize {
    let mut disk = Disk::parse(input);
    disk.move_files();
    disk.checksum()
}

fn main() {
    let input = include_str!("../in.txt");
    println!("Part 1: {}", part1(input));
    println!("Part 2: {}", part2(input));
}

#[test]
fn example1() {
    let input = include_str!("../ex1.txt");
    assert_eq!(part1(input), 1928);
    assert_eq!(part2(input), 2858);
}
