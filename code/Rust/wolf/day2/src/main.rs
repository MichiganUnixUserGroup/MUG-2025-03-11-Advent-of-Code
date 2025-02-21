use day2::{
    count_safe_reports, dont_try_to_make_report_safe, read_in_the_reports,
    try_to_make_report_safe_brute_force, try_to_make_report_safe_smart,
};

#[cfg(test)]
use day2::Report;

fn main() {
    let reports = read_in_the_reports();

    println!(
        "Day 2, part 1: there are {} safe reports.",
        count_safe_reports(&reports, dont_try_to_make_report_safe)
    );

    println!(
        "Day 2, part 2 (smart): there are {} safe reports when using the problem dampener.",
        count_safe_reports(&reports, try_to_make_report_safe_smart)
    );

    println!(
        "Day 2, part 2 (brute-force): there are {} safe reports when using the problem dampener.",
        count_safe_reports(&reports, try_to_make_report_safe_brute_force)
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
        assert_eq!(
            count_safe_reports(&reports, dont_try_to_make_report_safe),
            2
        );
    }

    #[test]
    fn test_day2_part2_smart() {
        let reports = test_reports();
        assert_eq!(
            count_safe_reports(&reports, try_to_make_report_safe_smart),
            4
        );
    }

    #[test]
    fn test_day2_part2_brute_force() {
        let reports = test_reports();
        assert_eq!(
            count_safe_reports(&reports, try_to_make_report_safe_brute_force),
            4
        );
    }
}
