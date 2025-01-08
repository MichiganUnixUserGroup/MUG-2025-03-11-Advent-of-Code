use counter::Counter;
use regex::Regex;
use std::io::{self, BufRead};

// TODO: add documentation strings
// TODO: talk about why unwrap, or maybe expect, is the right thing to do in this program

fn read_the_two_lists() -> (Vec<i32>, Vec<i32>) {
    let re = Regex::new(r"^(\d+)\s+(\d+)$").unwrap();

    let stdin = io::stdin();
    let lines: Vec<String> = stdin
        .lock() // Locks stdin, providing a `BufRead`-capable type
        .lines() // Uses the `BufRead` trait
        .map(|line| line.expect("Failed to read line"))
        .collect();

    let mut left: Vec<i32> = vec![];
    let mut right: Vec<i32> = vec![];

    for line in lines {
        let captures = re.captures(&line).unwrap();
        // I hate that this takes two statements. I should write a helper that returns a tuple
        let l = captures.get(1).unwrap().as_str();
        let r = captures.get(2).unwrap().as_str();
        left.push(l.parse().unwrap());
        right.push(r.parse().unwrap());
    }

    (left, right)
}

fn distance_between_the_two_lists(lhs: &Vec<i32>, rhs: &Vec<i32>) -> i32 {
    // must clone because we will sort the lists
    let mut left: Vec<i32> = lhs.clone();
    let mut right: Vec<i32> = rhs.clone();
    // so annoying that sorting doesn't return a value ... it can't be used in a chain above
    left.sort_unstable();
    right.sort_unstable();

    left.iter().zip(right.iter()).map(|(l, r)| (l - r).abs()).sum()
}

fn similarity_score(left: &Vec<i32>, right: &Vec<i32>) -> i32 {
    let right_side_counts = right.iter().collect::<Counter<_>>();

    let mut similarity = 0;
    for l in left {
        if let Some(&count) = right_side_counts.get(&l) {
            similarity += l * count as i32;
        }
    }

    similarity
}

fn main() {
    // cargo run < your_data_file    to execute the code normally (it reads from stdin)
    let (left, right) = read_the_two_lists();
    println!(
        "Day 1, part 1: the distance between the two lists is {}.",
        distance_between_the_two_lists(&left, &right)
    );
    println!(
        "Day 1, part 2: the similarity score for the two lists is {}.",
        similarity_score(&left, &right)
    );
}

#[cfg(test)]
mod test {
    // cargo test     to run the tests
    use super::*;

    #[test]
    fn test_day1_part1() {
        let left = vec![3, 4, 2, 1, 3, 3];
        let right = vec![4, 3, 5, 3, 9, 3];

        let distance = distance_between_the_two_lists(&left, &right);
        assert_eq!(distance, 11);
    }

    #[test]
    fn test_day1_part2() {
        let left = vec![3, 4, 2, 1, 3, 3];
        let right = vec![4, 3, 5, 3, 9, 3];

        let similarity = similarity_score(&left, &right);
        assert_eq!(similarity, 31);
    }
}
