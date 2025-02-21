use std::io::{self, BufRead};

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum ReportSafety {
    Safe,
    Unsafe,
}

fn is_safe(safety: &ReportSafety) -> bool {
    *safety == ReportSafety::Safe
}

fn read_in_the_reports() -> Vec<Vec<i32>> {
    io::stdin()
        .lock()
        .lines()
        .map(|line| {
            line.unwrap()
                .split_whitespace()
                .map(|num_str| num_str.parse::<i32>().unwrap())
                .collect()
        })
        .collect()
}

fn check_report(report: &Vec<i32>) -> ReportSafety {
    let deltas = report[..report.len() - 1]
        .iter()
        .zip(&report[1..])
        .map(|(a, b)| b - a)
        .collect::<Vec<i32>>();

    if !deltas.iter().all(|&delta| (1..=3).contains(&delta.abs())) {
        ReportSafety::Unsafe
    } else {
        let first_sign = deltas[0].signum();
        if deltas.iter().all(|&delta| delta.signum() == first_sign) {
            ReportSafety::Safe
        } else {
            ReportSafety::Unsafe
        }
    }
}

fn dont_try_to_make_report_safe(_unsafe_report: &Vec<i32>) -> ReportSafety {
    ReportSafety::Unsafe
}

fn try_to_make_report_safe(unsafe_report: &Vec<i32>) -> ReportSafety {
    for i in 0..unsafe_report.len() {
        let mut test_report = unsafe_report.clone();
        test_report.remove(i);
        if is_safe(&check_report(&test_report)) {
            return ReportSafety::Safe;
        }
    }
    ReportSafety::Unsafe
}

fn count_safe_reports(reports: &[Vec<i32>], dampener: fn(&Vec<i32>) -> ReportSafety) -> i32 {
    reports
        .iter()
        .filter(|report| is_safe(&check_report(report)) || is_safe(&dampener(report)))
        .count() as i32
}

fn main() {
    let reports = read_in_the_reports();

    println!(
        "Day 2, part 1: there are {} safe reports.",
        count_safe_reports(&reports, dont_try_to_make_report_safe)
    );

    println!(
        "Day 2, part 2: there are {} safe reports when using the problem dampener.",
        count_safe_reports(&reports, try_to_make_report_safe)
    );
}
