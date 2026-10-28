# Stage 1 plan: oracle harness

Scratch plan for [Stage 1](../roadmap.md#stage-1--oracle-harness). Not kept in
sync after the stage merges.

1. `tools/fetch-test-suite.sh [dest]`: download `semantic_tests_v3.5.0.zip`,
   check SHA-256 `523bed25…ea77`, unpack to `dest` (default
   `sbml-test-suite/`), write `dest/VERSION`. No-op when `VERSION` matches.
   Ignore `sbml-test-suite/` in `.gitignore` and `.Rbuildignore`.
2. `tests/testthat/helper-suite.R`: `suite_dir()` (env `ZUSBML_TEST_SUITE`,
   skip when unset unless `ZUSBML_REQUIRE_SUITE=true`), `suite_read_settings()`,
   `suite_read_results()`, `suite_read_tags()`, `suite_case()`,
   `suite_category()`, `suite_compare()` (the rule
   `|a - e| <= absolute + relative * |e|`), `suite_run_case()`,
   `suite_run()`, `suite_floor()`.
3. Fixture case `tests/testthat/fixtures/suite/00000/`, written for the
   package: settings, results, `model.m`. Unit tests for every helper, red
   first.
4. `tests/testthat/suite-floor.dcf`: the committed minimum counts; Stage 1
   floors are zero. `test-suite.R` runs every case and asserts no
   unexpected error and every count at or above its floor.
5. `.github/workflows/test-suite.yaml`: cache the suite, fetch, run
   `filter = "suite"` with the suite required, upload the per-case report.
6. `Suggests: deSolve`. Roadmap: per-stage targets, Done column.
