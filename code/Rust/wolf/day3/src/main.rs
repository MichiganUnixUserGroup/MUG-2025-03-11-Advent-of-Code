use std::io::{self, Read};
use regex::RegexBuilder;

// TODO: Add tests
// TODO: I _think_ use a RegexSet for part 2.  Not sure.

fn read_input() -> String {
    let mut input = String::new();
    let _ = io::stdin().read_to_string(&mut input);
    input
}

fn sum_of_muls(input: &str) -> i32 {
    let pattern = r"mul\(([0-9]{1,3}),([0-9]{1,3})\)";
    let re = RegexBuilder::new(pattern)
        .multi_line(true)
        .build()
        .unwrap();

    re.captures_iter(input)
        .map(|caps| {
            let (_, [x_str, y_str]) = caps.extract();
            let x: i32 = x_str.parse().unwrap();
            let y: i32 = y_str.parse().unwrap();
            x * y
        })
        .sum::<i32>()
}

fn main() {
    let input_string: String = read_input();
    let input = input_string.as_str();

    let part1_total: i32 = sum_of_muls(input);

    println!("Day 3, part 1: the total of all valid multiplications is {}.", part1_total);
}
