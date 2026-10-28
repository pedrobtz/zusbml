# CLAUDE.md

<!-- markdownlint-disable-next-line MD013 -->
This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

`zusbml` reads an SBML document into one R object, writes that object back
as valid SBML Level 3 Version 2, and derives from it what a solver needs:
states, initial values, right-hand sides, assignments and events. The
derivation is the point. It reads Level 3 Versions 1 and 2 core and Level 2
Versions 4 and 5, all in pure R on `zuxml`'s tree API. It is not a
simulator, not libSBML, not a reader of SBML packages (`comp`, `fbc`, ...)
and not a model editor. `Imports` is `zuxml` and nothing else.

The specification is [`.agents/design.md`](.agents/design.md), adopted from
[RFC 0048](https://github.com/pedrobtz/packages/blob/main/rfcs/started/0048-zusbml-sbml-models.md).
It outranks this file: `CLAUDE.md` orients, the design decides. Its open
questions (Q1 to Q5) are still open.

## Current state

Scaffolded 2026-10-28 with `create-pkg.sh`; no functionality yet. The roadmap
in `.agents/roadmap.md` takes it to v0.1.0.

## Stage tracking

Progress toward the next version is tracked as GitHub sub-issues, so the
parent issue shows a progress bar such as "6 of 7".

- **One parent issue per target version**, titled with the bare version, for
  example `v0.1.0`. `v0.1.0` issue: created by `pkg-roadmap`.
- **One sub-issue per roadmap stage**, titled as the roadmap titles it, for
  example `Stage 3 — R interface`, and linking to that section. The number of
  stages is whatever the roadmap has; there is no fixed scale.
- **Every tracking issue carries the `stage` label**, so it can be told apart
  from bug and feature issues and left out of open-issue counts.
- **Close a stage by merging its pull request.** Put `Closes #<n>` in the
  body of the stage's pull request. Never close a stage whose exit criteria
  are not met; record a deviation in the roadmap first.
- **The roadmap stays authoritative.** The issues record which stages are
  done, not what a stage requires. Adding, removing or renaming a stage
  means editing the roadmap and the sub-issues in the same change.
- **Each stage has a CI tier, `quick` or `full`.** The roadmap sets it, and
  `quick` is the default. See Continuous integration for what each means.
  The second-to-last stage, `Full CI`, runs the full set and fixes what it
  finds before `v0.1.0 readiness`.
- Close the parent issue when CRAN accepts the version and it is tagged.
  Opening a parent issue implies at least one stage sub-issue; a parent with
  no sub-issues shows no progress on the board.

## Versioning

A new package develops at `0.0.0.9000` and targets `0.1.0` for its first CRAN
release. Set `Version: 0.1.0` and the matching `NEWS.md` heading when the
package enters `Pre-flight`. Bump to `0.1.0.9000` only after CRAN accepts it,
never before: a `.9000` version above an unreleased `0.1.0` heading is wrong.

Until the first release the R API may change freely. Sibling packages can
still depend on this one before CRAN does, through `Remotes:` or `LinkingTo:`.
`zupkpmx` (RFC 0046) will take its SBML models from here for
PEtab (zupkpmx#17), so the shape of `sbml_odes()` output is effectively an
API once that work starts. A package that others wait on should reach CRAN first.

## Commands

Run from the package root.

```sh
Rscript -e 'devtools::load_all()'                # load the package
Rscript -e 'devtools::document()'                # roxygen -> NAMESPACE, man/
Rscript -e 'devtools::test()'                    # full testthat suite
Rscript -e 'devtools::test(filter = "<name>")'   # one test file
Rscript -e 'devtools::test(shuffle = TRUE)'      # order independence
Rscript -e 'devtools::check()'                   # R CMD check
air format .                                     # format R sources
```

The SBML Test Suite oracle, occasional (CI runs it on every push):

```sh
tools/fetch-test-suite.sh                        # into sbml-test-suite/
ZUSBML_TEST_SUITE=$PWD/sbml-test-suite \
  Rscript -e 'devtools::test(filter = "^suite$")'
```

## Architecture

Design §4 has the five layers (document, MathML, model object, semantics,
validation). Built so far:

- [R/spec.R](R/spec.R) is the table of Level 3 core elements: each one's
  attributes (type, required) and allowed children. The reader walks
  documents against it, so supporting a new attribute or element means one
  entry here, and anything absent from it is a `zusbml_parse_error`.
- [R/read.R](R/read.R) parses with `zuxml`, settles level and version from
  the namespace, refuses or drops SBML packages, and turns each element into
  a list of typed attributes, SBase fields and children.
- [R/mathml.R](R/mathml.R) turns MathML into R expressions and back. One
  table (`math_ops`) drives both directions; `math_env` defines the
  operators kept under their SBML names (design D15). The Test Suite runner
  checks that every expression survives R -> MathML -> R unchanged.
- [R/derive.R](R/derive.R) is `sbml_odes()`: it gives every symbol one role
  (constant, assignment or state), builds right-hand sides from reactions
  and rate rules, orders assignments by dependency, and solves time 0 as
  one system of initial assignments, assignment rules and defaults.
  `derivs_function()` compiles the result into a `function(.t, .y, .p)`.
- [R/events.R](R/events.R) derives events and builds `sbml_derivs()`'s
  event engine: compiled root functions over each trigger's relational
  atoms, and a handler that applies section 4.12 (delays, persistence,
  priorities, cascades). Its state lives in a closure and restarts at
  time 0.
- [R/rxode2.R](R/rxode2.R) prints derived equations as rxode2 text.
- [R/model.R](R/model.R) builds the `sbml_model`: one data frame per element
  kind, list columns for math and nested parts, accessors and printing.
- [R/conditions.R](R/conditions.R) builds the classed conditions of design
  §8; their fields (`path`, `attribute`, `section`, `package`) are what tests
  assert on.

There is no native code: v0.1.0 is pure R (design D2), so there is no `src/`.

## Invariants that are easy to break

These are fixed by the design before any code exists.

- **`Imports: zuxml` only** (D3, §12). Anything else goes in `Suggests`
  (deSolve, `zujson`) and is called behind a
  `requireNamespace()` check. Adding an import breaks a release criterion.
- **No libSBML, anywhere** (D4): not vendored, not a system dependency,
  not a CI job.
- **Nothing LGPL is committed or shipped** (D5). The SBML Test Suite is
  downloaded by the `tools/` script into a gitignored directory, never
  committed under `inst/` or `tests/`. The same holds for BioModels files.
- **Refuse, don't approximate** (D7, D8). Algebraic rules, `delay`, `fast`
  reactions and SBML packages raise `zusbml_unsupported` naming the id and
  the specification section. A Test Suite case outside the supported tag set
  is asserted to be refused, never skipped.
- **Writing is Level 3 Version 2 only** (D9), and every supported Test
  Suite case must pass again after a write and re-read.
- **One operator table drives MathML in both directions** (§6.2). Adding a
  mapping on one side only breaks the round trip.
- **Internal names start with "."** (`.amount.S1`, `.local.r1.k`, `.time`,
  the `.t`, `.y`, `.p` arguments). SBML ids cannot contain ".", so these never
  collide with model symbols. Test Suite cases name symbols `time`, `pi` and
  `y`; a bare name of that kind broke derivation before (design D16).
- **The Test Suite pass count only goes up.** Stage 0 records the supported
  tags and the expected count per stage.

## Testing conventions

- **Self-sufficient.** Every test builds its own inputs inside the
  `test_that()` block. No file-scope objects.
- **Self-contained.** Global state goes through `withr::local_*()`, randomised
  input through `withr::local_seed()`. Tests write only under `tempdir()`.
- **Assert on condition classes, never message text.** Use
  `expect_error(..., class = "<pkg>_<kind>_error")`. Wording belongs in
  snapshot tests.
- **Order independence.** `devtools::test(shuffle = TRUE)` is part of the
  definition of done.
- **Borrow a conformance suite before writing one.** Mainstream
  implementations in Python, Java, Go or Node usually ship a test corpus.
  Use it as an oracle rather than inventing cases by hand. The SBML Test
  Suite 3.5.0 semantic cases, which also judge the writer by round
  trip, and fifty BioModels identifiers (design §7). None of them
  covers SBML packages, algebraic rules or `delay`, which are refused.
- **Helpers live in `tests/testthat/helper-*.R`.** `helper-suite.R` reads
  Test Suite cases, applies their tolerance rule (`suite_compare()`) and
  runs each case through as much of the pipeline as exists
  (`suite_run_case()`); `suite-floor.dcf` holds the counts that may only
  go up.
- **Keep the suite inside the CRAN time budget.** No tests yet.

## Definition of done

`devtools::document()` and `devtools::check()` clean, meaning 0 errors, 0
warnings and 0 notes. The one allowed note is "New submission" before the first
release. `devtools::test(shuffle = TRUE)` green. CI green at the change's
tier: the quick profile for a `quick` roadmap stage or an ordinary pull
request, the full profile for a `full` stage, `Full CI`, and every release.
A user-facing change also needs a test, roxygen documentation and a `NEWS.md`
entry.

## Releasing to CRAN

- **CI is the pre-submission check.** The `pedrobtz/r-actions` R CMD check runs
  `--as-cran` on the CRAN-like runners and containers, and it replaces
  win-builder, the macOS builder and R-hub. None of them is a release step,
  and `cran-comments.md` lists the CI legs as its test environments.
- **Entering `Pre-flight`.** The roadmap's feature stages are done, the
  version is set as in Versioning, `NEWS.md` and `cran-comments.md` are
  written, and CI is green. Only submission work remains.
- **Before submitting,** run `pkg-critical-review` once the roadmap is
  implemented and fix what it files, then `pkg-cran-review`, and resolve
  every finding. Both skills are in `pedrobtz/r-skills`.
- **The pretest is automated and does not read `cran-comments.md`.** A NOTE
  that is explained but not fixed gets the upload archived. Fix the NOTE, as
  under Vendored native code, instead of justifying it.
- **After acceptance,** `pkg-post-cran` tags the submitted commit with the
  bare version, `0.1.0` (no `v`), publishes the GitHub release, bumps to
  `0.1.0.9000`, closes the version's parent issue and moves the board card.

## Editing rules

- roxygen comments are the source. Never edit `man/` or `NAMESPACE` by hand.
- Edit `README.Rmd`, not the generated `README.md`.
- Keep prose simple and short. One idea per sentence.
- Wrap roxygen text at 80 characters and run `air format .` on R sources.
- Use `lower_snake_case`, and prefix internal native symbols with `<pkg>_`.
- Keep hard runtime dependencies at zero unless a recorded design decision
  says otherwise.
- Export and document public functions. Do not create roxygen topics for
  internal ones.

## Continuous integration

Workflows come from `pedrobtz/r-actions`. Call those reusable workflows instead
of writing bespoke ones, and pin a released tag rather than `main`. Pin a
workflow that holds a write token, such as `coverage.yml`, to a commit, with
the tag in a comment. The `R-CMD-check` workflow must call `r-cmd-check.yml`,
because a hand-rolled one does not count as the pre-submission check. This package
calls `r-cmd-check.yml@v1` and `coverage.yml` (pinned to a commit);
`pkgdown.yaml` and `test-suite.yaml`, the Test Suite oracle, are local.

Small edits get a small run. The shared workflows look at the files a pull
request changes, compared with its merge base:

- **Markdown only**, such as `README.md`, `NEWS.md` or `cran-comments.md`,
  skips every check. Only coverage and the pkgdown site run.
- **`DESCRIPTION` or `README.Rmd` as well**, and nothing else, adds one R CMD
  check on Ubuntu R-release.
- **Any other file** runs everything.

So put a README or `DESCRIPTION` touch-up in its own pull request. Once a pull
request changes code, every later push to it runs every workflow.

### Quick by default, full by exception

The full profile is slow. gctorture at step 20 alone has taken 36 to 107
minutes on sibling packages. Waiting on it between roadmap stages was the
main thing slowing roadmaps down, so:

- **Pull requests run the quick profile.** It should finish in under five
  minutes. If it doesn't, raise gctorture's `quick-step` or trim
  `quick-containers` in the caller workflows; never cut the full profile.
- **A `full` roadmap stage takes the `full-ci` label** and merges only on a
  green full run. The roadmap gives the reason in the stage's `**CI.**`
  line, for example first compiled code, a `PROTECT` or finalizer change, a
  vendored library, or a Windows-only path.
- **Don't wait on the full run a merge starts** on the default branch, and
  cancel older full runs it supersedes. A failure that only the full set
  shows goes on the `Full CI` stage issue as a comment with the run link,
  and is fixed there. A failure the quick profile also shows is fixed now.
- **`Full CI` and `v0.1.0 readiness` wait** for a green full run on their
  merge commit, and the release commit always needs one.
- Locally, run `gctorture(TRUE)` on the code a C change touches when it is
  cheap; the full run is the authority.

## Commits and pull requests

Short, imperative, sentence-case commit subjects, optionally scoped. Keep each
commit focused and do not sweep in unrelated files. A pull request explains the
user-visible outcome and the rationale, links related issues, lists the checks
that were run and the tests that were skipped, and flags platform-sensitive or
vendored changes. Performance claims need evidence.

Never commit or push to the default branch. Work on a branch, open a pull
request, and leave it for review. Do not merge a pull request unless you are
told to.

When you find a defect, in this package or in an upstream tool, open an issue
for it rather than only working around it.

## Working with skills

The portfolio's workflow skills live in `pedrobtz/r-skills` (`pkg-init`,
`pkg-roadmap`, `pkg-implement`, `pkg-critical-review`, `pkg-cran-review`,
`pkg-post-cran`, `pkg-readme`); the general process skills are
[Superpowers](https://github.com/obra/superpowers). `building-a-package.md`
in `pedrobtz/packages` walks through them in order. Both sets defer to this
file where they overlap, so the settlements are recorded here.

- **One design.** The specification is `.agents/design.md`, adopted from the
  package's RFC in `pedrobtz/packages/rfcs`. A package with an adopted design
  is past `brainstorming`; a design change goes through the RFC, not a new
  spec. Superpowers' default `docs/superpowers/specs/` is not used.
- **Plans at two levels.** `.agents/roadmap.md` holds the stages, one
  mergeable pull request each, with exit criteria and a CI tier
  (`pkg-roadmap`). Inside a stage, `writing-plans` breaks the work into
  tasks of one test cycle each, saved as `.agents/plans/stage-<n>.md`. The
  roadmap stays authoritative; a plan is scratch and is not kept in sync.
- **Build and merge.** `pkg-implement` runs the stage loop: branch
  `stage-<n>-<slug>` from the stage issue, implement the plan with
  `test-driven-development` (`Rscript -e 'devtools::test(filter = "<name>")'`
  is the red/green command), open the pull request, merge on green at the
  stage's CI tier. The confirmation the user gives at the start of that
  loop is the "told to merge" that Commits and pull requests requires, so
  the `finishing-a-development-branch` menu is not presented for stage
  pull requests. Any other pull request still waits for review.
- **Failures and claims.** A failing check or test goes through
  `systematic-debugging` before a fix. "Green", "done" and "ready for CRAN"
  are claimed only after `verification-before-completion`: run the command,
  read the output.
- **Review.** `requesting-code-review` gates tasks inside a stage.
  `pkg-critical-review` reviews the whole package once the roadmap is
  implemented, with Posit's reviewers, and files issues. CRAN's reviewer
  email and GitHub review threads go through `receiving-code-review`.
- **Keep scratch out of the tarball.** `R CMD build` reads `.Rbuildignore`,
  not `.gitignore`. It must list `^\.agents$`, `^\.superpowers$` (the
  Superpowers task ledger), `^\.worktrees$` (if git worktrees are kept in
  the package directory; a sibling directory avoids the question),
  `^\.claude$`, `^CLAUDE\.md$`, `^AGENTS\.md$`, `^CRAN-SUBMISSION$` and
  `^cran-comments\.md$`. A stray top-level file is a NOTE, and the pretest
  archives on NOTEs.
