use day2::{
    check_unsafe_report_with_problem_dampener,
    check_unsafe_report_with_problem_dampener_using_brute_force, number_of_safe_reports,
    number_of_safe_reports_using_problem_dampener, read_in_the_reports,
};

#[cfg(debug_assertions)]
use day2::{check_report, levels_as_string, ReportSafety};

#[cfg(any(test, debug_assertions))]
use day2::Report;

#[cfg(debug_assertions)]
fn analyze_problem_dampeners(reports: &[Report]) {
    // TODO: iter passes down a &, filter then gets a &.  I thought returned what it got.  Then why
    // doesn't map get a reference?  Am I accidentally moving the underlying Reports?  I clone in
    // multiple places.  Am I doing the right thing?
    let unsafe_reports = reports
        .iter()
        .filter(|&report| check_report(report) == ReportSafety::Unsafe)
        .map(|report| report.clone())
        .collect::<Vec<Report>>();

    let different_outcomes = unsafe_reports
        .iter()
        .filter(|&report| {
            // Yes, this is less efficient.  I could just do the analysis here.
            check_unsafe_report_with_problem_dampener_using_brute_force(&report)
                != check_unsafe_report_with_problem_dampener(&report)
        })
        .map(|report| report.clone())
        .collect::<Vec<Report>>();

    for report in different_outcomes {
        // print the report
        let levels = levels_as_string(&report);
        println!("Here's the Unsafe Report [{}].", levels);

        println!(
            "The \"smart\" test said it was {:?}.",
            check_unsafe_report_with_problem_dampener(&report)
        );
        println!(
            "Brute-force test said it was {:?}.",
            check_unsafe_report_with_problem_dampener_using_brute_force(&report)
        );
    }
}

fn main() {
    let reports = read_in_the_reports();

    #[cfg(debug_assertions)]
    analyze_problem_dampeners(&reports);

    println!(
        "Day 2, part 1: there are {} safe reports.",
        number_of_safe_reports(&reports)
    );

    println!(
        "Day 2, part 2 (smart): there are {} safe reports when using the problem dampener.",
        number_of_safe_reports_using_problem_dampener(
            &reports,
            check_unsafe_report_with_problem_dampener
        )
    );

    println!(
        "Day 2, part 2 (brute-force): there are {} safe reports when using the problem dampener.",
        number_of_safe_reports_using_problem_dampener(
            &reports,
            check_unsafe_report_with_problem_dampener_using_brute_force
        )
    );
}

#[cfg(test)]
mod tests {
    use super::*;

    // This is the same list of reports from the problem description.  I'll compare against the
    // answers given in the same place.
    fn test_reports() -> Vec<Report> {
        vec![
            vec![7, 6, 4, 2, 1], // Unsafe
            vec![1, 2, 7, 8, 9], // Unsafe
            vec![9, 7, 6, 2, 1], // Safe
            vec![1, 3, 2, 4, 5], // Unsafe
            vec![8, 6, 4, 4, 1], // Unsafe
            vec![1, 3, 6, 7, 9], // Safe
        ]
    }

    #[test]
    fn test_day2_part1() {
        let reports = test_reports();
        assert_eq!(number_of_safe_reports(&reports), 2);
    }

    #[test]
    fn test_day2_part2_brute_force() {
        let reports = test_reports();
        assert_eq!(
            number_of_safe_reports_using_problem_dampener(
                &reports,
                check_unsafe_report_with_problem_dampener_using_brute_force
            ),
            4
        );
    }

    #[test]
    fn test_day2_part2_smart() {
        let reports = test_reports();
        assert_eq!(
            number_of_safe_reports_using_problem_dampener(
                &reports,
                check_unsafe_report_with_problem_dampener
            ),
            4
        );
    }
}
