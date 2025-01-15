use std::io::{self, Read};
use lazy_static::lazy_static;
use regex::Regex;

const MUL_PATTERN: &str = r"(?ms)mul\(([0-9]{1,3}),([0-9]{1,3})\)";

lazy_static! {
    static ref MUL_REGEX: Regex = Regex::new(MUL_PATTERN).unwrap();
}

fn read_input() -> String {
    let mut input = String::new();
    let _ = io::stdin().read_to_string(&mut input);
    input
}

fn sum_of_muls(input: &str) -> i32 {
    MUL_REGEX.captures_iter(input)
        .map(|caps| {
            let (_, [x_str, y_str]) = caps.extract();
            let x: i32 = x_str.parse().unwrap();
            let y: i32 = y_str.parse().unwrap();
            x * y
        })
        .sum::<i32>()
}

fn sum_of_muls_with_conditions(input: &str) -> i32 {
    // I wish I could use RegexSet here, but that won't capture.
    // TODO: Better names for all variables, and for the function itself
    let do_re = Regex::new(r"(?ms)do\(\)").unwrap();
    let dont_re = Regex::new(r"(?ms)don\'t\(\)").unwrap();

    let mut capturing_muls = true;
    let mut processed_offset = 0;
    let end_offset = input.len();
    let mut total = 0;

    while processed_offset < end_offset {
        if capturing_muls {
            // Get the range until the next "don't()"
            let mut go_till_offset = end_offset;
            let mut next_processed_offset = end_offset;
            if let Some(m) = dont_re.find_at(input, processed_offset) {
                go_till_offset = m.start();
                next_processed_offset = m.end();
            }
            total += sum_of_muls(&input[processed_offset..go_till_offset]);
            processed_offset = next_processed_offset;
            capturing_muls = false;
        } else {
            // TODO: This should probably be "else if"
            // Find the next "do()", update the processed_offset, begin capturing
            if let Some(m) = do_re.find_at(input, processed_offset) {
                capturing_muls = true;
                processed_offset = m.end();
            }
        }
    }

    total
}

fn main() {
    let input_string: String = read_input();
    let input = input_string.as_str();

    let part1_total: i32 = sum_of_muls(input);
    let part2_total: i32 = sum_of_muls_with_conditions(input);

    println!("Day 3, part 1: the total of all valid multiplications is {}.", part1_total);
    println!("Day 3, part 2: the conditional total of multiplications is {}.", part2_total);
}

#[cfg(test)]
mod tests {
    use super::*;

    // These are the same strings from the problem description.  I'll compare against the
    // answers given in the same place.
    const PART1_INPUT: &str = "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))";
    const PART2_INPUT: &str = "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))";

    #[test]
    fn test_day3_part1() {
        let total = sum_of_muls(PART1_INPUT);
        assert_eq!(total, 161);
    }

    #[test]
    fn test_day3_part2() {
        let total = sum_of_muls_with_conditions(PART2_INPUT);
        assert_eq!(total, 48);
    }
}
