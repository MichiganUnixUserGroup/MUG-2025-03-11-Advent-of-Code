use day2::{
    count_safe_reports, read_in_the_reports, try_to_make_report_safe_brute_force,
    try_to_make_report_safe_smart,
};

use criterion::{Criterion, criterion_group, criterion_main};

fn benchmark_methods(c: &mut Criterion) {
    let reports = read_in_the_reports();

    c.bench_function("Smart method", |b| {
        b.iter(|| count_safe_reports(&reports, try_to_make_report_safe_smart))
    });
    c.bench_function("Brute-force method", |b| {
        b.iter(|| count_safe_reports(&reports, try_to_make_report_safe_brute_force))
    });
}

criterion_group!(benches, benchmark_methods);
criterion_main!(benches);
