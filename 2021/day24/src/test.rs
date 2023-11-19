#[test]
fn test_monad() {
    let file = std::fs::File::open("nums.txt").unwrap();
    let reader = std::io::BufReader::new(file);

    for line in std::io::BufRead::lines(reader) {
        let line = line.unwrap();
        let s: Vec<&str> = line.split(' ').collect();
        let z = s[1].parse::<u64>().unwrap();
        let num: Vec<u64> = s[0]
            .chars()
            .map(|x| x.to_digit(10).unwrap() as u64)
            .collect();
        assert_eq!(z, crate::monad(&num))
    }
}
