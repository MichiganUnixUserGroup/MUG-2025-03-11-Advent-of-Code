use counter::Counter;
use std::cmp::Ord;
use std::io::{self, BufRead};

// How to run:
//   When you have Rust installed on your system, be in the day1 directory (that is, cd to the
//   directory that contains Cargo.toml).  You can run the tests by saying `cargo test`.  You can
//   run on real data by piping in input.  Here's what I say: `cargo run < day1.input`.  Your input
//   is different than mine; I didn't include my data.  Use your own.  You use the same data on a
//   given day for both part 1 and part 2 of the problem.

// Would an ordinary program need _this_ level of comments?  No.  This implementation is meant to
// be read, and particularly to be read by programmers who don't know Rust.  Also, I mostly avoid
// explicit loops here.  Where I can use the "functional style", I do.  Supposedly they are mostly
// equivalent and there is no speed advantage either way.  I do some things in this implementation
// just to learn more about Rust.  This is not the best possible solution to the problem, but
// there's a lot to learn from this implementation.

// This trait and its implementation directly below it are pure silliness.  They are so I can chain
// the creation of a Vec and sorting it into one expression.  I'm really doing it just to learn.
trait SortExt<T> {
    // I only use the unstable version, so that's all I implement; but you can imagine having a
    // stable version too.
    fn to_sorted_unstable_vec(&self) -> Vec<T>;
}

impl<T: Ord + Clone> SortExt<T> for [T] {
    fn to_sorted_unstable_vec(&self) -> Vec<T> {
        let mut v = self.to_vec();
        v.sort_unstable();
        v
    }
}

fn read_the_two_lists() -> (Vec<i32>, Vec<i32>) {
    // No parameters.  We get all our data from stdin.
    let stdin = io::stdin();

    // We're returning two lists. They need to be mutable so we can push in all the values.
    let mut left_vec: Vec<i32> = vec![];
    let mut right_vec: Vec<i32> = vec![];

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
        // Previously parsed `line` with a regex, but simple splitting is easier.
        let mut iter = line.split_whitespace();

        // This relies on entirely well-formed import.  Every line must contain two numbers.  Grab
        // the strings for them with `.next().unwrap()`, then turn them into i32's with `parse()`.
        let left = iter.next().unwrap().parse().unwrap();
        let right = iter.next().unwrap().parse().unwrap();

        left_vec.push(left);
        right_vec.push(right);
    }

    // Must return ownership, therefore Vecs, not slices.  No `return`.  No trailing `;`.  This is a
    // "tail expression", i.e. the return value of the function.
    (left_vec, right_vec)
}

fn distance_between_the_two_lists(left_slice: &[i32], right_slice: &[i32]) -> i32 {
    // The problem statement says the lists must be sorted.  We'll use our silly custom trait to
    // turn each slice into a Vec and sort it at the same time, that way they don't have to be
    // mutable.
    let left_vec: Vec<i32> = left_slice.to_sorted_unstable_vec();
    let right_vec: Vec<i32> = right_slice.to_sorted_unstable_vec();

    // We're stepping through both lists at the same time.  So each list needs `.iter()`.  `.zip()`
    // works on two lists.  For each step, `.zip()` returns a tuple of two values, one from each
    // list.  This function is about the difference between the two sides, so `.map()` that
    // subtracts to find that difference, but we need it to be positive, so `.abs()`.  Finally,
    // `.sum()` adds them all up.  That sum is the final answer.  Note again, this is a tail
    // expression: the return value of the function.
    left_vec.iter()
        .zip(right_vec.iter())
        .map(|(l, r)| (l - r).abs())
        .sum()
}

fn similarity_score(left_slice: &[i32], right_slice: &[i32]) -> i32 {
    // Step 1: count up the values on the right side.  That's one of the two multiplicands in the
    // similarity score

    // A Counter takes any iterable and builds a dictionary where the keys are the things you
    // stepped through in the iterator, and the values are how many times you saw that thing.
    let right_counts = right_slice.iter().collect::<Counter<_>>();

    // Step 2: for each item in the left list, multiply by the number of times that item appears
    // in the right list.  Don't "uniquify" the left list.  If we hit the same number from the left
    // list multiple times, it counts every time.  Note (again) that the entire following
    // expression is a tail expression: the return value of the function.

    left_slice.iter()
        .fold(0, |acc, &item| {
            // `.get` returns an `Option<&u32>` (because counts are unsigned).  Why it stores a
            // reference I don't know.  `.copied()` turns that into a `Option<u32>`.  It's no longer a
            // reference so I don't have dereference the whole lookup, nor give `&0` as the fallback
            // value.  It feels cleaner to me.  Note that in `fold` you don't modify `acc`, you return
            // a new value for it, and that's what you get as an argument next time around.  And how do
            // I return the new value?  With a tail expression of course!
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
