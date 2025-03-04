use counter::Counter;
use std::fs::File;
use std::io::{self, BufRead, BufReader};
use std::path::PathBuf;
use pyo3::prelude::*;

#[pyfunction]
#[pyo3(signature = (input_path=None))]
fn read_the_two_lists(input_path: Option<PathBuf>) -> (Vec<i32>, Vec<i32>) {
    // TODO: maybe I should fix the return type so I can raise an exception if an input_path
    //  is supplied, but it can't be opened.
    let input: Box<dyn BufRead> = match input_path {
        Some(path) => Box::new(BufReader::new(File::open(path).unwrap())),
        None => Box::new(BufReader::new(io::stdin().lock())),
    };

    // We're returning two lists. They need to be mutable so we can push in all the values.
    let mut left_vec: Vec<i32> = vec![];
    let mut right_vec: Vec<i32> = vec![];

    // Step 1: read the input

    // Read all of stdin into a vector of strings, one string per line of input.
    // It's easiest to do this in two steps because if I just did all the work here, in .map,
    // what would I use as an iterator consumer?
    let input_lines: Vec<String> = input
        .lines() // Uses the `BufRead` trait
        .map(|line| line.expect("Failed to read line"))
        .collect::<Vec<_>>();

    // Step 2: step through the lines, grabbing two numbers from each; pushing them into the result
    // lists.

    for line in input_lines {
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

#[pyfunction]
fn distance_between_the_two_lists(left: Vec<i32>, right: Vec<i32>) -> i32 {
    // mut because I have to sort them.  Clone because I don't want to effect the caller.
    let mut private_left: Vec<i32> = left.clone();
    let mut private_right: Vec<i32> = right.clone();

    private_left.sort();
    private_right.sort();

    // See my all-Rust implementation of day 1 for an explanation of why this works.
    private_left
        .iter()
        .zip(private_right.iter())
        .map(|(l, r)| (l - r).abs())
        .sum()
}

#[pyfunction]
fn similarity_score(left: Vec<i32>, right: Vec<i32>) -> i32 {
    // Step 1: count up the values on the right side.  That's one of the two multiplicands in the
    // similarity score

    // A Counter takes any iterable and builds a dictionary where the keys are the things you
    // stepped through in the iterator, and the values are how many times you saw that thing.
    let right_counts = right.iter().collect::<Counter<_>>();

    // Step 2: for each item in the left list, multiply by the number of times that item appears
    // in the right list.  Don't "uniquify" the left list.  If we hit the same number from the left
    // list multiple times, it counts every time.  Note (again) that the entire following
    // expression is a tail expression: the return value of the function.

    left.iter().fold(0, |acc, &item| {
        // `.get` returns an `Option<&u32>` (because counts are unsigned).  Why it stores a
        // reference I don't know.  `.copied()` turns that into a `Option<u32>`.  It's no longer a
        // reference so I don't have dereference the whole lookup, nor give `&0` as the fallback
        // value.  It feels cleaner to me.  Note that in `fold` you don't modify `acc`, you return
        // a new value for it, and that's what you get as an argument next time around.  And how do
        // I return the new value?  With a tail expression of course!
        acc + item * right_counts.get(&item).copied().unwrap_or(0) as i32
    })
}

/// A Python module implemented in Rust.
#[pymodule]
fn list_tools(m: &Bound<'_, PyModule>) -> PyResult<()> {
    m.add_function(wrap_pyfunction!(read_the_two_lists, m)?)?;
    m.add_function(wrap_pyfunction!(distance_between_the_two_lists, m)?)?;
    m.add_function(wrap_pyfunction!(similarity_score, m)?)?;
    Ok(())
}
