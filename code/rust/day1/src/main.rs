use counter::Counter;
use regex::Regex;
use std::io::{self, BufRead};

// Would an ordinary program need _this_ level of comments?  No.  This implementation is meant to
// be read, and particularly to be read by programmers who don't know Rust.  Also, I mostly avoid
// explicit loops here.  Where I can use the "functional style", I do.  Supposedly they are mostly
// equivalent and there is no speed advantage either way.

fn read_the_two_lists() -> (Vec<i32>, Vec<i32>) {
    // No parameters.  We get all our data from stdin.
    let stdin = io::stdin();

    // We're returning two lists. They need to be mutable so we can push in all the values.
    let mut left_vec: Vec<i32> = vec![];
    let mut right_vec: Vec<i32> = vec![];

    // Define a regular expression that captures two number strings separated by whitespace.
    let re = Regex::new(r"^(\d+)\s+(\d+)$").unwrap();

    // Step 1: read the input

    // Read all of stdin into a vector of strings, one string per line of input.
    let input_lines: Vec<String> = stdin
        .lock() // Locks stdin, providing a `BufRead`-capable type
        .lines() // Uses the `BufRead` trait
        .map(|line| line.expect("Failed to read line"))
        .collect();

    // Step 2: step through the lines, grabbing two numbers from each; pushing them into the result
    // lists.

    for line in input_lines {
        // This whole loop is an example of me _not_ handling errors.  Just one `unwrap` after
        // another.  This simple program calculates a simple result.  I probably don't need fancy
        // error handling, so `unwrap` and friends is appropriate.

        // Apply the regular expression to the current line.  `captures` lets you get to the captured
        // strings.  This might be a good place to do real error handling.  Maybe if we don't find
        // two numbers on the line we could just skip it (with continue) going on to the next line.
        let captures = re.captures(&line).unwrap();

        // I hate that this takes two statements. I should write a helper that returns a tuple.
        // Pull the two strings out of the match.  `l` and `r` are string slices, each one a
        // number.
        let l = captures.get(1).unwrap().as_str();
        let r = captures.get(2).unwrap().as_str();

        // Parse the two strings into actual numbers. Add one to each list.
        left_vec.push(l.parse().unwrap());
        right_vec.push(r.parse().unwrap());
    }

    // Must return ownership, therefore Vecs, not slices.
    (left_vec, right_vec)
}

fn distance_between_the_two_lists(left_slice: &[i32], right_slice: &[i32]) -> i32 {
    // The problem statement says the lists are sorted.  Sorting modifies in place, so we must 
    // make something mutable.
    let mut left_vec: Vec<i32> = left_slice.to_vec();
    let mut right_vec: Vec<i32> = right_slice.to_vec();

    // Step 1: sort the input lists

    // So annoying that sorting doesn't return a value ... it can't be used in a chain above.
    left_vec.sort_unstable();
    right_vec.sort_unstable();

    // Step 2: add up the distance at each step (in sync) through the lists

    // We're stepping through both lists at the same time.  So each list needs `.iter()`.  `.zip()`
    // works on two lists.  For each step, `.zip()` returns a tuple of two values, one from each
    // list.  This function is about the difference between the two sides, so `.map()` that
    // subtracts to find that difference, but we need it to be positive, so `.abs()`.  Finally,
    // `.sum()` adds them all up.  That sum is the final answer.
    left_vec.iter().zip(right_vec.iter()).map(|(l, r)| (l - r).abs()).sum()
}

fn similarity_score(left_slice: &[i32], right_slice: &[i32]) -> i32 {
    // Step 1: count up the values on the right side.  That's one of the two multiplicands in the
    // similarity score

    // A Counter takes any iterable and builds a dictionary where the keys are the things you
    // stepped through in the iterator, and the values are how many times you saw that thing.
    let right_counts = right_slice.iter().collect::<Counter<_>>();

    // Step 2: for each item in the left list, multiply by the number of times that item appears
    // in the right list.  Don't "uniquify" the left list.  If we hit the same number from the left
    // list multiple times, it counts every time.

    left_slice.iter().fold(0, |acc, &item| {
        // `.get` returns an `Option<&u32>` (because counts are unsigned).  Why it stores a
        // reference I don't know.  `.copied()` turns that into a `Option<u32>`.  It's no longer a
        // reference so I don't have dereference the whole lookup, nor give `&0` as the fallback
        // value.  It feels cleaner to me.
        acc + item * right_counts.get(&item).copied().unwrap_or(0) as i32
    })
}

fn main() {
    // Vec<i32> to own the values; but everybody else uses slices.
    let (left_vec, right_vec) = read_the_two_lists();
    let (left_slice, right_slice) = (left_vec.as_slice(), right_vec.as_slice());

    println!(
        "Day 1, part 1: the distance between the two lists is {}.",
        distance_between_the_two_lists(&left_slice, &right_slice)
    );

    println!(
        "Day 1, part 2: the similarity score for the two lists is {}.",
        similarity_score(&left_slice, &right_slice)
    );
}

#[cfg(test)]
mod test {
    use super::*;

    // These are the lists from the problem description.  I'll compare against the answers given
    // in the same place.  Don't need Vec<i32> here.  I can define the inputs as slices directly.
    // [i32; 6] means an array (slice) of i32's defined literally, of length 6.
    const LEFT: [i32; 6] = [3, 4, 2, 1, 3, 3];
    const RIGHT: [i32; 6] = [4, 3, 5, 3, 9, 3];

    #[test]
    fn test_day1_part1() {
        let distance = distance_between_the_two_lists(&LEFT, &RIGHT);
        assert_eq!(distance, 11);
    }

    #[test]
    fn test_day1_part2() {
        let similarity = similarity_score(&LEFT, &RIGHT);
        assert_eq!(similarity, 31);
    }
}
