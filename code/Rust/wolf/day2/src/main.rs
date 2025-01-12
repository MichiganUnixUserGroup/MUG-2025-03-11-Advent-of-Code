use std::io::{self, BufRead};

type Level = i32;         // Each level in a report is an i32
type Delta = i32;         // The distance between two adjacent levels in a Report
type Report = Vec<Level>; // A report is a vector of levels

enum ReportSafety {
    Safe,
    Unsafe,
}

fn read_in_the_reports() -> Vec<Report> {
    // No parameters.  We get all our data from stdin.
    io::stdin()
        .lock()
        .lines()
        .map(|line| {
            // Instead of first reading all the lines, and then separately parsing each line as I
            // did in day1, I'm going to parse each line into a Report as I'm reading.

            // Here, line is a Result<String, std::io::Error>.  .expect unwraps it and I get the
            // string itself, or else we panic.
            let report_str = line.expect("Failed to read line");

            // With the string, .split_whitespace gives me an iterator.  The input is well-formed,
            // so each step in the iterator is a string of digits.  The .map returns the actual
            // i32 or as we're calling it: a Level.  The .collect gathers them into a Vec<Level>,
            // that is, a Report.
            report_str
                .split_whitespace()
                .map(|num_str| num_str.parse::<Level>().unwrap())
                .collect()
        })
        // This final .collect gathers the individual Reports into a Vec<Report>, and that is the
        // result of this function.
        .collect()
}

fn number_of_safe_reports(reports: &[Report]) -> i32 {
    reports
        .iter()
        .map(|report| {
            // Calculate the Deltas, the difference between adjacent Levels in the Report.  It's
            // the Deltas that we look at to determine if the Report is Safe or Unsafe.  There are
            // lots of ways to calculate the Deltas.  I picked the conceptually simple way.  For
            // each Delta what I care about is the Level to its left and the level to its right.
            // If I had those as a tuple, I could just do subtraction.  .zip of two lists gives me
            // a tuple.  So if I had a list of left-Levels and a list of right-Levels I could do
            // it.  The left-Levels is just all of the Levels except the last one.  The
            // right-Levels is all of the Levels starting from the second one.  If I pair these up
            // with .zip each tuple will have two adjacent Levels in it, and I will get exactly as
            // many tuples as there are gaps between the Levels.
            let deltas: Vec<Delta> = report[..report.len() - 1]
                .iter()
                .zip(&report[1..])
                .map(|(a, b)| b - a)
                .collect();

            // Check Condition 1: The magnitude of all the Deltas must be in range 1..=3
            if !deltas.iter().all(|&delta| (1..=3).contains(&delta.abs())) {
                ReportSafety::Unsafe
            } else {
                // Check Condition 2: All Deltas must have the same sign
                let first_sign = deltas[0].signum();
                if deltas.iter().all(|&delta| delta.signum() == first_sign) {
                    ReportSafety::Safe
                } else {
                    ReportSafety::Unsafe
                }
            }

            // The result of this .map is a single value: either Safe or Unsafe
        })
        .filter(|safety| matches!(safety, ReportSafety::Safe)) // Keep only the Safe reports
        .count() as i32 // Count the number of Safe reports
}

fn main() {
    let reports = read_in_the_reports();

    println!(
        "Day 2, part 1: there are {} safe reports.",
        number_of_safe_reports(&reports)
    );
}

#[cfg(test)]
mod tests {
    use super::*;

    // This is the same list of reports from the problem description.  I'll compare against the
    // answers given in the same place.
    fn test_reports() -> Vec<Report> {
        vec![
            vec![7, 6, 4, 2, 1],  // Unsafe
            vec![1, 2, 7, 8, 9],  // Unsafe
            vec![9, 7, 6, 2, 1],  // Safe
            vec![1, 3, 2, 4, 5],  // Unsafe
            vec![8, 6, 4, 4, 1],  // Unsafe
            vec![1, 3, 6, 7, 9],  // Safe
        ]
    }

    #[test]
    fn test_day2_part1() {
        let reports = test_reports();
        assert_eq!(number_of_safe_reports(&reports), 2);
    }
}
