# zusbml: roadmap

Status: draft 2026-10-28. Implements [design.md](design.md).
Target: v0.1.0, the first CRAN release.
Tracking: [v0.1.0 (#1)](https://github.com/pedrobtz/zusbml/issues/1), with one
sub-issue per stage;
[CRAN-submission (#13)](https://github.com/pedrobtz/zusbml/issues/13).

Design §11 sketches eight stages. Here its Stage 0 is split into package
identity and the oracle harness, its Stage 7 into documentation, `Full CI`
and `v0.1.0 readiness`, and the remaining stages keep their order. Design
§12's "eight stages" counts feature work; the bracketing stages are the
portfolio's.

## Stages

| Stage | Title | CI | Issue | Pull request | Done |
| --- | --- | --- | --- | --- | --- |
| 0 | [Package identity](#stage-0--package-identity) | quick | [#2](https://github.com/pedrobtz/zusbml/issues/2) | [#15](https://github.com/pedrobtz/zusbml/pull/15) | 2026-10-28 |
| 1 | [Oracle harness](#stage-1--oracle-harness) | full | [#3](https://github.com/pedrobtz/zusbml/issues/3) | [#16](https://github.com/pedrobtz/zusbml/pull/16) | 2026-10-28 |
| 2 | [Document reader and model object](#stage-2--document-reader-and-model-object) | quick | [#4](https://github.com/pedrobtz/zusbml/issues/4) | [#17](https://github.com/pedrobtz/zusbml/pull/17) | 2026-10-28 |
| 3 | [MathML](#stage-3--mathml) | quick | [#5](https://github.com/pedrobtz/zusbml/issues/5) | [#18](https://github.com/pedrobtz/zusbml/pull/18) | 2026-10-28 |
| 4 | [Derivation without events](#stage-4--derivation-without-events) | quick | [#6](https://github.com/pedrobtz/zusbml/issues/6) | | 2026-10-28 |
| 5 | [Events and solver interfaces](#stage-5--events-and-solver-interfaces) | quick | [#7](https://github.com/pedrobtz/zusbml/issues/7) | | |
| 6 | [Writer and structural checks](#stage-6--writer-and-structural-checks) | quick | [#8](https://github.com/pedrobtz/zusbml/issues/8) | | |
| 7 | [Level 2, BioModels and property test](#stage-7--level-2-biomodels-and-property-test) | quick | [#9](https://github.com/pedrobtz/zusbml/issues/9) | | |
| 8 | [Documentation](#stage-8--documentation) | quick | [#10](https://github.com/pedrobtz/zusbml/issues/10) | | |
| 9 | [Full CI](#stage-9--full-ci) | full | [#11](https://github.com/pedrobtz/zusbml/issues/11) | | |
| 10 | [v0.1.0 readiness](#stage-10--v010-readiness) | full | [#12](https://github.com/pedrobtz/zusbml/issues/12) | | |

## Stage 0 — Package identity

**Issue.** [#2](https://github.com/pedrobtz/zusbml/issues/2), sub-issue of
[v0.1.0](https://github.com/pedrobtz/zusbml/issues/1).

**CI.** `quick`.

**Goal.** The package says what it is and CI checks it the portfolio's way.

**Work.**

- `DESCRIPTION`: real Title and Description from design §1, `Authors@R`
  with `aut`, `cre` and `cph`, `Version: 0.0.0.9000`, MIT.
- `.Rbuildignore` with every entry `CLAUDE.md`'s "Working with skills" lists
  (`^\.superpowers$` added 2026-10-28).
- `R-CMD-check` caller: add `{"os": "windows-latest", "r": "devel"}` to
  `runners`.
- `coverage` caller: pin `coverage.yml` to a commit with the `v1` tag in a
  comment, as `CLAUDE.md` requires of a workflow holding a write token.
- Replace the placeholder test with a test of package loading; `NEWS.md`
  says development version, not "Initial CRAN submission".
- `CLAUDE.md` from the template (written by `pkg-init` on 2026-10-28).

**Exit criteria.**

- `R CMD check` passes on the quick r-actions profile.
- Every quick-profile check on the pull request finishes in under five
  minutes.
- `DESCRIPTION` has no template placeholder.

**Design.** §1, §13 D11.

## Stage 1 — Oracle harness

**Issue.** [#3](https://github.com/pedrobtz/zusbml/issues/3), sub-issue of
[v0.1.0](https://github.com/pedrobtz/zusbml/issues/1).

**CI.** `full — first Suggests guard (deSolve) in tests and the first
package-local workflow: nosuggests and every platform leg.`

**Goal.** The Test Suite oracle runs in CI before there is anything to judge, so
every later stage reports a number against them.

**Work.**

- A script under `tools/` that downloads SBML Test Suite 3.5.0
  (`semantic_tests_v3.5.0.zip`), checks its checksum and unpacks it into a
  directory listed in `.gitignore` and `.Rbuildignore` (D5).
- Test helpers that parse a case's settings file and results CSV, and
  compare trajectories under its `absolute` and `relative` tolerances,
  in `tests/testthat/helper-suite.R` (D12).
- A Test Suite runner test that skips when the suite is absent and is
  required on the CI job that downloads it. Runner: deSolve (D13).
- A package-local Test Suite workflow (Q6).
- The supported tag set (Q2) and the expected pass count per stage, in
  `tests/testthat/` or `.agents/`, so the count can only go up.

**Exit criteria.**

- The Test Suite job downloads the suite, caches it, and the runner test
  runs: every case refused or pending, none erroring for an unexpected
  reason.
- `nosuggests` passes in the full profile.
- The supported tag set and the per-stage pass counts are committed.

**Floors.** Test Suite 3.5.0 has 1,823 semantic cases; 1,692 have a Level
3 Version 2 file and are run. Under Q2, 1,384 are supported (964 without
events, 420 with) and 308 unsupported (by tag, by `packagesPresent`, or
flux-balance). Revised 2026-10-28 in Stage 2: cases 01822 and 01823 declare
`comp` only in `packagesPresent`, which Stage 1's count missed.
`tests/testthat/suite-floor.dcf` holds the minimums the current stage must
meet. Targets, recorded 2026-10-28:

| After stage | read | pass | refused |
| --- | --- | --- | --- |
| 1 | 0 | 0 | 0 |
| 2 | 1,384 | 0 | 159 (SBML package cases, refused on read) |
| 4 | 1,384 | 964 | 308 |
| 5 | 1,384 | 1,384 | 308 |

A stage that falls short records the gap and the case ids here.

**Design.** §7, §9, §15 Q2 and Q3.

## Stage 2 — Document reader and model object

**Issue.** [#4](https://github.com/pedrobtz/zusbml/issues/4), sub-issue of
[v0.1.0](https://github.com/pedrobtz/zusbml/issues/1).

**CI.** `quick`.

**Goal.** Any Level 3 core document reads into an `sbml_model`, with math
kept as `zuxml` nodes until Stage 3.

**Work.**

- `Imports: zuxml`.
- `sbml_read()` for a file, text or raw: namespace, level and version,
  every `listOf*` container, defaults and `NA` per §6.1, required booleans.
- The `sbml_model` object, `print()`, `summary()` and the accessors
  (`sbml_species()`, `sbml_parameters()`, `sbml_compartments()`,
  `sbml_reactions()`, `sbml_rules()`, `sbml_events()`, `sbml_functions()`).
- `zusbml_parse_error` and `zusbml_unsupported` (SBML packages refused by
  name, `ignore_packages = TRUE`), per §8 and D8. `annotation` and `notes`
  kept as `zuxml` nodes.
- `zusbml_info()`.
- One test file per element kind.

**Exit criteria.**

- Every Level 3 Version 2 Test Suite file within the supported tags reads
  without error; every file using a refused package raises
  `zusbml_unsupported`.
- Each §8 parse error has a test asserting its class.
- `R CMD check` passes on the quick r-actions profile.

**Design.** §4.1, §4.3, §5, §6.1, §8, D8.

## Stage 3 — MathML

**Issue.** [#5](https://github.com/pedrobtz/zusbml/issues/5), sub-issue of
[v0.1.0](https://github.com/pedrobtz/zusbml/issues/1).

**CI.** `quick`.

**Goal.** Content MathML becomes R language objects and back, through one
operator table.

**Work.**

- `R/mathml.R`: the §6.2 table, both directions; lambdas to R functions;
  the `time`, Avogadro, `rateOf` and `delay` csymbols; `cn` types;
  `%.17g` numbers.
- The reader stores expressions instead of nodes; `sbml_math()`.
- `zusbml_write_error` for a call outside the table.

**Exit criteria.**

- Every §6.2 row has a test in each direction.
- Every MathML block in the supported Test Suite files converts to R and
  back to an equivalent expression.
- `R CMD check` passes on the quick r-actions profile.

**Design.** §4.2, §6.2, D6.

## Stage 4 — Derivation without events

**Issue.** [#6](https://github.com/pedrobtz/zusbml/issues/6), sub-issue of
[v0.1.0](https://github.com/pedrobtz/zusbml/issues/1).

**CI.** `quick`.

**Goal.** `sbml_odes()` and `sbml_initial_state()` turn an event-free model
into equations that reproduce the Test Suite's results.

**Work.**

- `R/derive.R`: states, right-hand sides with stoichiometry, conversion
  factors, amounts against concentrations, varying compartments;
  assignment ordering; local parameters inlined; `rateOf` replaced.
- `sbml_initial_state()` in dependency order.
- `zusbml_derive_error` for cycles and missing initial values;
  `zusbml_unsupported` for algebraic rules, `delay` and `fast` (D7).
- Hand-written minimal models for every §6.3 rule, expected equations
  written out.

**Exit criteria.**

- Every §6.3 rule has a hand-written model test.
- The event-free supported Test Suite cases pass under their tolerances,
  meeting Stage 1's expected count.
- `R CMD check` passes on the quick r-actions profile.

**Design.** §4.4, §6.3, D7, §15 Q1.

## Stage 5 — Events and solver interfaces

**Issue.** [#7](https://github.com/pedrobtz/zusbml/issues/7), sub-issue of
[v0.1.0](https://github.com/pedrobtz/zusbml/issues/1).

**CI.** `quick`.

**Goal.** Events are carried and run; the equations reach deSolve and
rxode2.

**Work.**

- Events in `sbml_odes()`: trigger, `initialValue`, `persistent`,
  priority, delay, `useValuesFromTriggerTime`, assignments.
- `sbml_derivs()`: `function(t, y, p)` plus a root function and event
  handler for deSolve.
- `sbml_to_rxode2()`: rxode2 text; events refused unless
  `events = "drop"` (D10).

**Exit criteria.**

- The supported Test Suite cases with events pass, meeting Stage 1's
  expected count.
- `sbml_to_rxode2()` output for a fixed set of event-free models parses
  in rxode2 when installed (test skips otherwise).
- `R CMD check` passes on the quick r-actions profile.

**Design.** §6.3 (events), §5, D10.

## Stage 6 — Writer and structural checks

**Issue.** [#8](https://github.com/pedrobtz/zusbml/issues/8), sub-issue of
[v0.1.0](https://github.com/pedrobtz/zusbml/issues/1).

**CI.** `quick`.

**Goal.** Every model writes as valid Level 3 Version 2 and reads back
identical.

**Work.**

- `sbml_write()`, Level 3 Version 2 only (D9).
- `sbml_write_json()` behind a `zujson` guard.
- `R/check.R`: `sbml_check()` returning findings as a data frame, never
  raising (§4.5).
- Round-trip test on every supported Test Suite file.

**Exit criteria.**

- Read, write, read gives an identical object on every supported Level 3
  Test Suite file.
- Every supported Test Suite case passes again after write and re-read.
- `R CMD check` passes on the quick r-actions profile.

**Design.** §4.1, §4.5, §5, §8, D9.

## Stage 7 — Level 2, BioModels and property test

**Issue.** [#9](https://github.com/pedrobtz/zusbml/issues/9), sub-issue of
[v0.1.0](https://github.com/pedrobtz/zusbml/issues/1).

**CI.** `quick`.

**Goal.** The rest of the §9 test plan: Level 2 input, real curated models,
and generated ones.

**Work.**

- Level 2 Versions 4 and 5 mapping (§6.4); Level 1 refused (Q4).
- The Level 2 Version 5 file of each supported Test Suite case derives to
  the same equations as its Level 3 file.
- Fifty fixed BioModels identifiers, downloaded in CI, read and
  round-tripped.
- Property test: random reaction networks written, read, derived and
  compared with the generator's equations.
- `zufuzz` targets for the document and MathML readers, if `zufuzz` is
  usable by then.
- The §10 performance targets measured and recorded.

**Exit criteria.**

- §6.4 has a test per mapping rule; paired Level 2 and Level 3 cases derive
  the same equations.
- All fifty BioModels identifiers read and round-trip in CI.
- The property test runs in CI.
- `R CMD check` passes on the quick r-actions profile.

**Design.** §6.4, §7, §9, §10, §15 Q4.

## Stage 8 — Documentation

**Issue.** [#10](https://github.com/pedrobtz/zusbml/issues/10), sub-issue of
[v0.1.0](https://github.com/pedrobtz/zusbml/issues/1).

**CI.** `quick`.

**Goal.** A newcomer can read a model and simulate it from the docs alone.

**Work.**

- `README.Rmd` (with `pkg-readme`): what it does, a BioModels example, the
  supported tag set and the Test Suite pass count (§12).
- An article: read, derive, simulate with deSolve, compare with the Test
  Suite.
- Every exported function documented with runnable examples on a small
  bundled MIT model written for the package.
- pkgdown reference index.

**Exit criteria.**

- README states the supported tag set and the pass count.
- The pkgdown site builds in CI.
- `R CMD check` passes on the quick r-actions profile.

**Design.** §5, §12.

## Stage 9 — Full CI

**Issue.** [#11](https://github.com/pedrobtz/zusbml/issues/11), sub-issue of
[v0.1.0](https://github.com/pedrobtz/zusbml/issues/1).

**CI.** `full — always.`

**Goal.** No new features. The full r-actions profile is green on the
default branch's code.

**Work.**

- Run the full profile with the `full-ci` label and fix everything it
  finds: every platform leg including Windows R-devel, the containers,
  `nosuggests`, `nold`, and gctorture if added.
- Fix any failure recorded on this issue from earlier merges.

**Exit criteria.**

- A full-profile run is green on the pull request and on its merge commit.

**Design.** §12.

## Stage 10 — v0.1.0 readiness

**Issue.** [#12](https://github.com/pedrobtz/zusbml/issues/12), sub-issue of
[v0.1.0](https://github.com/pedrobtz/zusbml/issues/1).

**CI.** `full — always.`

**Goal.** Only the submission itself is left.

**Work.**

- `Version: 0.1.0` and the matching `NEWS.md` heading.
- `cran-comments.md` listing the r-actions CI legs as test environments.
- `pkg-critical-review` run; blocking and required findings fixed.
- `pkg-cran-review` run; every finding resolved.
- `urlchecker::url_check()` and `spelling::spell_check_package()` clean.

**Exit criteria.**

- Every §12 acceptance criterion holds.
- `Imports` is `zuxml` only.
- A full-profile r-actions run is green on the release commit.

**Design.** §12, §15 Q5.

## After 0.1.0

From design §2 and §14, in the order expected:

- PEtab in `zupkpmx` on this package (zupkpmx#17).
- Algebraic rules and `delay` in derivation, once a consumer solves DAEs or
  DDEs.
- A C reader on `zuxml`'s streaming API, if documents in the hundreds of
  megabytes turn up (D2).
- Unit conversion through `zuunits`.
- SED-ML and COMBINE archives as `zusedml`; SBML packages, `comp` first;
  CellML as `zucellml`; annotation and MIRIAM extraction.

## Open questions

- **Q1** (Stage 4): does `sbml_odes()` also return a compiled closure? The
  design returns expressions only.
- **Q4** (Stage 7): read Level 1 at all? The design says no.
- **Q5** (Stage 10): CRAN submission or GitHub release; the board's
  `CRAN intent` decides, assumed `Planned`.

Decided 2026-10-28, before Stage 0: Q2 as the design proposes; Q3 deSolve
only (D13), with the comparison as a test helper instead of `daffiz` (D12);
Q6 a package-local Test Suite workflow; D2 and D6 as the design has them.
