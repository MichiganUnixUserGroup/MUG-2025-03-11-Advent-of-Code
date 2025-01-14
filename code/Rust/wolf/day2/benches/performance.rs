use day2::{
    read_in_the_reports,
    try_to_make_report_safe_smart,
    try_to_make_report_safe_brute_force,
    count_safe_reports,
};

use criterion::{criterion_group, criterion_main, Criterion};

fn benchmark_methods(c: &mut Criterion) {
    let reports = read_in_the_reports();

    c.bench_function("Smart method", |b| {
        b.iter(|| {
            count_safe_reports(
                &reports,
                try_to_make_report_safe_smart,
            )
        })
    });
    c.bench_function("Brute-force method", |b| {
        b.iter(|| {
            count_safe_reports(
                &reports,
                try_to_make_report_safe_brute_force,
            )
        })
    });
}

criterion_group!(benches, benchmark_methods);
criterion_main!(benches);
