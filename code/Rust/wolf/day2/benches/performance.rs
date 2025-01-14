use day2::{
    read_in_the_reports,
    check_unsafe_report_with_problem_dampener,
    check_unsafe_report_with_problem_dampener_using_brute_force,
    number_of_safe_reports_using_problem_dampener,
};

use criterion::{criterion_group, criterion_main, Criterion};

fn benchmark_methods(c: &mut Criterion) {
    let reports = read_in_the_reports();

    c.bench_function(
        "Smart method",
        |b| b.iter(
            || number_of_safe_reports_using_problem_dampener(&reports, check_unsafe_report_with_problem_dampener)
        )
    );
    c.bench_function(
        "Brute-force method",
        |b| b.iter(||
            number_of_safe_reports_using_problem_dampener(
                &reports,
                check_unsafe_report_with_problem_dampener_using_brute_force,
            )
        )
    );
}

criterion_group!(benches, benchmark_methods);
criterion_main!(benches);
