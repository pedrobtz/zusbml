# RFC 0048: `zusbml`, SBML models read, written and turned into equations

Adopted from [RFC 0048](https://github.com/pedrobtz/packages/blob/main/rfcs/started/0048-zusbml-sbml-models.md)
on 2026-10-28. From here on this file is authoritative; the RFC is history.

- **Status:** Proposed. Merging this file adopts the design as the starting
  point for the package's own `.agents/design.md`; each decision in section
  13 stays the maintainer's until the package repository records it.
  D2, pure R on `zuxml` for v0.1.0, and D6, MathML as R language objects,
  are the ones most worth a second look.
- **Date:** 2026-10-28.
- **Packages:** `zusbml` (not yet created; crandb returns `not_found`, there
  is no `pedrobtz/zusbml` repository and no repository of that name on
  GitHub). `Imports: zuxml`. No `LinkingTo` in v0.1.0. `Suggests:
  testthat, deSolve, zujson` (D12, D13). Vendors nothing. MIT.
- **Source:** the conversation of 2026-10-28 on the "compete with Pumas"
  analysis, which proposed a PEtab importer for R and found that CRAN has
  no SBML reader (`rsbml` left CRAN in 2007 and on Bioconductor pins
  libSBML 5.10.2 as a system dependency; `SBMLR` is dormant; `dlill/petab`
  and `PEtab2dmod` are GitHub-only and need libSBML); zupkpmx#17, which
  this RFC narrows to the PEtab tables; the SBML Level 3 Version 2 Core
  specification (Release 2) and Level 2 Version 5; the SBML Test Suite
  3.4.0 (`sbmlteam/sbml-test-suite`, LGPL 2.1): semantic cases 00001 to
  01821, each with the model in every level and version it applies to, a
  settings file (`start`, `duration`, `steps`, `variables`, `absolute`,
  `relative`, `amount`, `concentration`) and a results CSV; BioModels'
  curated branch; libSBML 5.20 as the validator in CI, not vendored
  (LGPL); the content-MathML subset SBML admits; `zuxml` 0.1.0's R API
  and its C header (`zuxml_api_v2`). Nothing has been built.

## 1. What zusbml is

`zusbml` reads an SBML document into one R object, writes that object back
as valid SBML, and derives from it the equations a solver needs: the state
variables, their initial values, the right-hand sides, the assignments
evaluated alongside, and the events. The derivation is the point. SBML
describes reactions between species in compartments; a solver wants
`dX/dt` for each state, and the rules that turn one into the other, with
amounts against concentrations, changing compartment sizes, conversion
factors and boundary species, are where hand translations go wrong.

The one-line statement that governs every decision below:

> **Every curated SBML model readable, writable and runnable from R, with
> nothing but R installed.**

What it is not:

- **Not a simulator.** It produces equations; `zupkode`, deSolve or any
  solver runs them. `sbml_to_rxode2()` writes the text those engines read.
- **Not libSBML.** No validation-rule parity, no SBML packages, no
  layout. libSBML is used nowhere, not even in CI (D4); the writer is
  judged by round trips through the Test Suite (§7).
- **Not a reader of every SBML package.** Core only in v0.1.0. A document
  that requires `comp`, `fbc`, `qual`, `multi`, `distrib`, `layout` or
  `render` is refused by package name unless asked to ignore them (D8).
- **Not a model editor.** Construction is through the object's accessors
  and `sbml_write()`; there is no builder vocabulary in v0.1.0.

## 2. Scope

In v0.1.0:

- **Read** SBML Level 3 Versions 1 and 2 core, and Level 2 Versions 4 and
  5 through a mapping (§6.4).
- **Write** Level 3 Version 2 core.
- **Elements**: function definitions, unit definitions, compartments,
  species, parameters, initial assignments, assignment and rate rules,
  constraints, reactions with reactants, products, modifiers, kinetic
  laws and local parameters, and events with triggers, priorities,
  delays and event assignments (§6.1).
- **MathML**: the SBML content subset, both directions (§6.2).
- **Derivation**: `sbml_odes()` (§6.3), `sbml_initial_state()`,
  `sbml_derivs()` for deSolve-style callers, `sbml_to_rxode2()`.
- **Round trip**: read, write, read gives an identical object.

Out of v0.1.0, in the order expected (§14):

- Algebraic rules in derivation (they read and write; `sbml_odes()`
  refuses them, since they make a DAE).
- The `delay` csymbol in derivation (a DDE).
- SBML packages; SED-ML and COMBINE archives; CellML.
- A C reader on `zuxml`'s streaming API for very large documents.
- Unit conversion through `zuunits`; annotation and MIRIAM extraction.

## 3. Position in the `zu*` family

`zusbml` sits in the text-formats layer beside `zujson`, `zutoml` and
`zuxml`: a specified format with a conformance corpus, implemented from
the specification because the reference implementation is LGPL, filling
an absence in base R and CRAN. It is the first package in the layer with
a sibling in `Imports`; RFC 0022 (`zuxslt`) set that precedent for
`zuxml`.

- **`zuxml`** (CRAN 0.1.0): `Imports`. The R tree API (`xml_read()`,
  `xml_children()`, `xml_attrs()`, `xml_ns()`, `xml_write()`) is the
  whole XML layer in v0.1.0 (D2). Its C header, a streaming parser with
  element handlers and a versioned table, is the §14 route for documents
  in the hundreds of megabytes.
- **`zupkode`** (RFC 0047): not in v0.1.0 (D13); it is not on CRAN.
  deSolve runs the derived equations for the Test Suite comparison (§7).
  Once `zupkode` is on CRAN it can join `Suggests`, and the Test Suite
  becomes its second conformance corpus.
- **`zupkpmx`** (RFC 0046): consumer. zupkpmx#17 becomes a PEtab reader
  that takes the SBML model from `zusbml` and adds the conditions,
  observables, measurements and parameters tables.
- **`daffiz`**: not used (D12). It is not on CRAN, and the Test Suite's
  tolerance rule is one line in a test helper.
- **`zujson`**: `Suggests`. `sbml_write_json()` for the object, when
  someone wants the model without XML.
- **`zufuzz`**: development only. Fuzzes the MathML and document
  readers.
- **`zuunits`** (RFC 0034): later, for unit conversion of the carried
  unit definitions.
- **`zusedml`** and **`zucellml`**: sketches in `base-layer-candidates.md`
  §5, promoted only if `zusbml` finds users.

## 4. Architecture

Five layers, all R in v0.1.0:

1. **Document** (`R/read.R`, `R/write.R`). `zuxml` parses; this layer
   checks the SBML namespace and reads level and version from it, walks
   the `listOf*` containers, and turns each element into a typed R list
   with the attributes the specification names, defaults applied where
   the level defines them and `NA` where it does not (§6.1). Unknown
   elements in the SBML namespace are errors; elements in other
   namespaces are the package check (D8); `annotation` and `notes` are
   kept as serialized XML text (D14).
2. **MathML** (`R/mathml.R`). Content MathML to R language objects and
   back (§6.2). One operator table drives both directions.
3. **Model object** (`R/model.R`). `sbml_model`: a list with one data
   frame per element kind for the scalar attributes and parallel lists
   for the expressions, plus `level`, `version`, `id`, `name`, the
   model-level attributes (`substanceUnits`, `timeUnits`, `volumeUnits`,
   `extentUnits`, `conversionFactor`) and the kept annotations. Print,
   summary and accessors.
4. **Semantics** (`R/derive.R`). `sbml_odes()` builds the state set and
   right-hand sides (§6.3); `sbml_initial_state()` evaluates initial
   assignments in dependency order; `sbml_derivs()` wraps the result in
   a function for deSolve; `sbml_to_rxode2()` prints the rxode2 text.
5. **Validation** (`R/check.R`). The structural rules the derivation
   needs: unique ids, references resolve, a species is set by at most
   one of rule, reaction or constant, no assignment cycles. Not libSBML's
   full rule set.

## 5. Public R API (v0.1.0 shape)

```r
m <- sbml_read("BIOMD0000000012.xml")          # file, URL text or raw
m                                                # level, version, counts
sbml_species(m); sbml_parameters(m); sbml_compartments(m)
sbml_reactions(m)                                # data frame; kinetic laws as expressions
sbml_rules(m); sbml_events(m); sbml_functions(m) # R functions from lambdas
sbml_math(m$reactions$kinetic_law[[1]])          # prints the expression

eq <- sbml_odes(m)                               # states, rhs, assignments, events
eq$states; eq$rhs$P1; eq$assignments; eq$events
x0 <- sbml_initial_state(m)                      # named numeric at t = 0
f  <- sbml_derivs(m)                             # function(t, y, p) for deSolve
cat(sbml_to_rxode2(m))                 # rxode2 text; events refused by default

sbml_write(m, "out.xml")                         # Level 3 Version 2
sbml_write_json(m)                               # with zujson installed
sbml_check(m)                                    # the §4.5 rules, as a data frame
zusbml_info()
```

## 6. Semantics

### 6.1 Elements and defaults

Each element carries exactly the attributes the specification defines
for the document's level and version. Level 3 has no default for
compartment `size`, species `initialAmount` or `initialConcentration`,
or parameter `value`; these read as `NA`, and derivation errors when one
is used before an initial assignment sets it. Level 2 defaults, such as
compartment `size = 1` and `spatialDimensions = 3`, are applied on read
so the object means the same thing whatever the source level. Boolean
attributes with no default in Level 3 (`constant` on species and
parameters, `hasOnlySubstanceUnits`, `boundaryCondition`, `reversible`)
are required and their absence is an error, as the specification says.

### 6.2 MathML

Content MathML becomes R calls: `<apply><plus/>...</apply>` is `+`,
`<power/>` is `^`, `<ln/>` is `log()`, `<log/>` with `<logbase>` is
`log(x, base)`, `<root/>` with `<degree>` is `^ (1/n)` and `sqrt()`
when the degree is 2, `<piecewise>` is nested `if` calls, relational and
logical operators map one to one with `and`/`or` taking any arity,
`<ci>` is a name, `<cn>` is a double with `type` honoured (`integer`,
`real`, `e-notation`, `rational`), `<csymbol>` for time is the name
`time`, for Avogadro the constant `6.02214179e23`, for `rateOf` the
call `rateOf(x)` which derivation replaces by the right-hand side, and
for `delay` the call `delay(x, tau)` which derivation refuses. Lambdas
become R functions with the bound variables as formals. The reverse
direction prints the same table; a call outside the table is a write
error naming it. Precision: numbers round-trip through `sprintf("%.17g")`.

### 6.3 Derivation

`sbml_odes()` follows the Level 3 Version 2 core specification, section
4.11 for reactions and 4.9 for rules, and produces:

- **States**: every species that is not `boundaryCondition` and not
  `constant` and not the target of an assignment rule; every compartment
  and parameter that is the target of a rate rule; stoichiometries that
  are rate-rule targets (Level 3).
- **Right-hand side for a species**: the sum over reactions of
  stoichiometry times kinetic law, times the species' conversion factor
  when set, with sign by reactant or product. Species in amounts take it
  directly; species in concentrations divide by the compartment size,
  and when that size has a rate rule the derivation integrates the
  amount and defines concentration as an assignment, as the
  specification requires.
- **Assignments**: assignment rules and the concentration definitions
  above, ordered by dependency, evaluated before the right-hand sides.
  Local parameters are inlined into their kinetic law with unique names.
- **Events**: trigger expression, `initialValue`, `persistent`, priority
  expression, delay expression, `useValuesFromTriggerTime`, and the
  assignments. Carried as data; `sbml_derivs()` exposes them as a root
  function and a handler for deSolve, and `sbml_to_rxode2()` refuses
  them unless `events = "drop"`.
- **Refused**: algebraic rules, `delay`, and Level 3 Version 1 `fast`
  reactions, each with the element id and the specification section.

### 6.4 Level 2 mapping

Level 2 Versions 4 and 5 read into the same object: `stoichiometryMath`
becomes a stoichiometry expression, species types and compartment types
are dropped with a note, `fast` reactions are refused, and Level 2 unit
defaults become explicit unit definitions. Writing is always Level 3
Version 2, so a Level 2 model round-trips to an equivalent Level 3
document, not an identical one; the Test Suite's paired files are the
check that the two mean the same.

## 7. The oracle

Two oracles, neither vendored nor committed:

- **The SBML Test Suite** 3.5.0 (LGPL 2.1, never redistributed).
  `tools/` holds a script that downloads `semantic_tests_v3.5.0.zip`
  (12 MB) from the release, checks its checksum and unpacks it into a
  directory listed in `.gitignore` and `.Rbuildignore`; CI caches it.
  Unpacked it is about 37 MB in 17,000 files, too big to commit (D5).
  For every semantic case whose tags are within the supported set, the
  Level 3 Version 2 file is read, derived, run from the settings file's
  `start`, `duration` and `steps`, and the named `variables` are
  compared with the results CSV under its `absolute` and `relative`
  tolerances by a test helper applying the suite's rule,
  `|actual - expected| <= absolute + relative * |expected|` (D12). The
  runner is deSolve (D13). Cases with unsupported tags
  (`AlgebraicRule`, `Delay`, `comp`, `fbc`) are asserted to be refused,
  not skipped. The Level 2 Version 5 file of each case is read too and
  must derive to the same equations. The writer is judged here too:
  every supported case is read, written, read again, and must give the
  identical object and pass the same comparison.
- **BioModels**: fifty curated models fixed by identifier, read without
  error and round-tripped. Not redistributed either.

## 8. Errors

- `zusbml_parse_error`: file, element path (`model/listOfSpecies/species[3]`),
  attribute, and the specification section, for malformed documents.
- `zusbml_unsupported`: the construct, its id, and the SBML package or
  section it belongs to, for packages, algebraic rules in derivation,
  `delay` and `fast`.
- `zusbml_derive_error`: the cycle or the missing initial value, naming
  the ids involved.
- `zusbml_write_error`: the R call the MathML printer cannot express.
- `sbml_check()` never raises; it returns findings as a data frame.

## 9. Testing

- One test file per element kind, per direction of MathML, for
  derivation, for the Level 2 mapping and for the writer.
- Hand-written minimal models for every rule in §6.3, with expected
  equations written out.
- The Test Suite runner as a test that skips when the suite is not
  downloaded and is required on the r-actions leg that downloads it.
- Round trip on every Test Suite and BioModels document: read, write,
  read, identical object.
- Property test: random reaction networks generated in R, written,
  read back, derived, compared with the generator's own equations.
- `zufuzz` targets for the document reader and the MathML reader.

## 10. Performance targets

Reading is `zuxml` plus R list building; the targets keep the package
usable on BioModels and the Test Suite, and say what the §14 C reader
would buy.

| Operation | Target |
| --- | --- |
| read a 1 MB curated BioModels document | under 300 ms |
| read every Level 3 Version 2 Test Suite file | under 90 s |
| MathML to R, 10⁵ nodes | under 1 s |
| `sbml_odes()` on a 500-reaction model | under 500 ms |
| write and re-read a 1 MB document | under 600 ms |

## 11. Roadmap

Stages run the quick CI profile unless marked `full`. Each is one pull
request, merged on green.

| Stage | Delivers | Exit criterion |
| --- | --- | --- |
| 0 | repository, CI, Suite download | oracle runs on a fixture |
| 1 | document reader, model object, printing | Level 3 reads; §8 errors |
| 2 | MathML both ways | §6.2 table round-trips on the Test Suite |
| 3 | derivation without events | §6.3 models; Suite subset passes |
| 4 | events, `sbml_derivs()`, `sbml_to_rxode2()` | event cases; text runs |
| 5 | writer, `sbml_check()` | written Suite cases round-trip and pass |
| 6 | Level 2 mapping, BioModels sample, property test | §6.4; §9 complete |
| 7 (`full`) | README, article, release preparation | §12 |

Stage 0 records which Test Suite tags the package will support and the
pass count expected at each stage, so the number can only go up.

## 12. Acceptance criteria for v0.1.0

- Every behaviour in §6 and §8 has a passing test.
- Every Test Suite semantic case within the supported tag set passes
  under the case's own tolerances; every case outside it is refused by
  name. The supported set and the pass count are in the README.
- Every supported Test Suite case passes again after a write and re-read.
- Fifty fixed BioModels identifiers read and round-trip.
- `R CMD check --as-cran` is clean on all r-actions legs; `Imports` is
  `zuxml` only.
- Eight stages. A stage that needs a second pull request moves scope to
  §14, not the release.

## 13. Decisions

Marked **(M)** when the maintainer decided it and **(C)** when this RFC
decides it and the maintainer may overturn it.

- **D1 (M).** `zusbml` is its own package in the text-formats layer, not
  a reader inside `zupkpmx`. SBML has its own specification, test suite
  and users; the pharmacometrics package takes the model from here.
- **D2 (M, 2026-10-28).** v0.1.0 is pure R on `zuxml`'s tree API. The work is the
  semantics, not the parsing, and curated models are small. The
  streaming C API is the §14 route if documents in the hundreds of
  megabytes turn up.
- **D3 (C).** `Imports: zuxml`, the first sibling import in the layer,
  on RFC 0022's precedent. Vendoring an XML parser to avoid it would
  duplicate `zuxml`.
- **D4 (M, 2026-10-28).** libSBML is used nowhere: not vendored, not a
  system dependency, not a CI validator. The writer is judged by Test
  Suite round trips (§7). This replaces the RFC's libSBML CI job.
- **D5 (M, 2026-10-28).** The Test Suite (3.5.0) is downloaded by a
  `tools/` script into a gitignored directory and never committed or
  shipped: 37 MB unpacked, and LGPL 2.1 data in an MIT package is
  avoidable and avoided.
- **D6 (M, 2026-10-28).** MathML becomes R language objects, not a package-private
  tree. R already has an expression type, every consumer can `eval()`
  or walk it, and `zupkpmx` reads R expressions today.
- **D7 (C).** Derivation follows the Level 3 Version 2 specification
  text, section by section, and refuses rather than approximates:
  algebraic rules, `delay` and `fast` are errors with ids.
- **D8 (C).** SBML packages are refused by name; `ignore_packages =
  TRUE` reads the core and records what was dropped.
- **D9 (C).** Writing is Level 3 Version 2 only.
- **D10 (C).** Events are first-class in the object and in
  `sbml_derivs()`, and refused by `sbml_to_rxode2()` unless dropped on
  request, because rxode2 text cannot express triggered discrete
  assignments.
- **D11 (M).** The name is `zusbml`: a text-format package, so no `pk`
  infix. Functions are `sbml_*`, with `zusbml_info()` for the
  package-level entry point.
- **D12 (M, 2026-10-28).** The Test Suite comparison is a test helper,
  not `daffiz`, which is not on CRAN and would need
  `Additional_repositories` in `Suggests`.
- **D14 (M, 2026-10-28, Stage 2).** `notes`, `annotation` and a constraint's
  `message` are kept as serialized XML text, not `zuxml` nodes: nodes hold
  external pointers, so a re-read model would never be `identical()` and the
  §2 round trip could not be tested. Math is R expressions (D6) from Stage 3
  on. `notes` and `annotation` on `listOf*` containers are dropped. `zuxml`
  0.1.0 parses and serializes but cannot build nodes, so `sbml_write()` emits
  XML text itself.
- **D15 (M, 2026-10-28, Stage 3).** MathML operators with an exact base
  R equivalent become that call; the rest keep their SBML name as an R
  call: `quotient()`, `rem()`, `implies()`, `rateOf()`, `delay()`, and the
  reciprocal trigonometric functions (`sec()`, `arccoth()`, ...). An
  internal environment defines them, and derived equations evaluate in it.
  `quotient` truncates toward zero and `rem` takes the dividend's sign,
  as in MathML 2.0. This keeps R -> MathML -> R exact: every expression in
  the Test Suite (7,569 of them) survives it unchanged. Losses, all
  equivalent in value: n-ary `plus`, `times`, `and`, `or` and `xor` nest
  into binary calls; relations with more than two arguments chain with
  `&`; the `avogadro` csymbol becomes its number; `root` of degree n is
  `x^(1/n)`; a `piecewise` without `otherwise` gets `NaN`; units on `<cn>`
  (`sbml:units`) are not kept.
- **D13 (M, 2026-10-28).** deSolve alone runs the Test Suite comparison
  in v0.1.0 (`Suggests: deSolve`). `zupkode` is not on CRAN; it can be
  added after it is.

## 14. Later versions, in the order they are expected

- **PEtab in `zupkpmx`** on this package (zupkpmx#17).
- **Algebraic rules and `delay`** in derivation, once a consumer solves
  DAEs or DDEs.
- **A C reader** on `zuxml`'s streaming API for very large documents.
- **Unit conversion** through `zuunits`, checking kinetic-law units.
- **SED-ML and COMBINE archives** as `zusedml`, if users ask: the Test
  Suite already ships a SED-ML file per case.
- **SBML packages**, `comp` first if flattening is wanted.
- **CellML** as `zucellml`, sharing the MathML layer.
- **Annotation and MIRIAM** extraction to data frames.

## 15. Open questions

- **Q1.** Should `sbml_odes()` return R expressions, or also a compiled
  closure for speed? This RFC returns expressions and leaves speed to
  `zupkode`.
- **Q2.** Which Test Suite tags are in the supported set at v0.1.0?
  This RFC proposes everything except `AlgebraicRule`, `Delay`,
  `FastReaction` and the package tags, and lets Stage 0 confirm.
  **Decided 2026-10-28:** as proposed.
- **Q3.** Is deSolve an acceptable CI fallback runner, or should the
  Suite comparison require `zupkode`? This RFC allows deSolve so the
  package does not wait on RFC 0047. **Decided 2026-10-28:** deSolve
  only (D13).
- **Q4.** Should Level 1 documents be read at all? This RFC says no;
  BioModels has none in its curated branch.
- **Q5.** Is v0.1.0 a CRAN submission or a GitHub release? The board's
  `CRAN intent` for this card decides; this RFC assumes `Planned`.
