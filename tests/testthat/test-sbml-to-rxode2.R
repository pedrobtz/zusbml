test_that("sbml_to_rxode2() writes constants, initial values, assignments and rates", {
  m <- sbml_read(test_path("fixtures", "suite", "00000", "00000-sbml-l3v2.xml"))

  expect_snapshot(cat(sbml_to_rxode2(m)))
})

test_that("rxode2 text rewrites what rxode2 lacks", {
  rxode2_expr <- utils::getFromNamespace("rxode2_expr", "zusbml")

  expect_equal(rxode2_expr(quote(if (a > 1) b else c)), "ifelse(a > 1, b, c)")
  expect_equal(rxode2_expr(quote(log(x, 2))), "log(x)/log(2)")
  expect_equal(rxode2_expr(quote(quotient(a, b))), "(trunc(a/b))")
  expect_equal(rxode2_expr(quote(sec(time))), "(1/cos(t))")
  expect_equal(rxode2_expr(0.1), "0.10000000000000001")
})

test_that("sbml_to_rxode2() refuses events unless asked to drop them", {
  doc <- with_events(
    model_doc(parameters = par("p", 0, constant = FALSE)),
    evt("e", quote(time >= 1), list(p = 1))
  )
  m <- sbml_read(doc)

  err <- expect_error(sbml_to_rxode2(m), class = "zusbml_unsupported")
  expect_equal(err$id, "e")
  expect_match(sbml_to_rxode2(m, events = "drop"), "d/dt(p) = 0;", fixed = TRUE)
})

test_that("names rxode2 cannot take get a .u prefix", {
  rxode2_expr <- utils::getFromNamespace("rxode2_expr", "zusbml")
  m <- sbml_read(model_doc(parameters = paste0(par("_J0", 1), par("param", 2))))

  text <- sbml_to_rxode2(m)

  expect_match(text, ".u_J0 = 1;", fixed = TRUE)
  expect_match(text, ".uparam = 2;", fixed = TRUE)
  expect_equal(rxode2_expr(quote(time + .time), ids = "time"), ".utime + t")
})

test_that("negation keeps its operand in parentheses", {
  rxode2_expr <- utils::getFromNamespace("rxode2_expr", "zusbml")

  expect_equal(rxode2_expr(quote(!(a > b))), "!(a > b)")
  expect_equal(rxode2_expr(quote(implies(a > 1, b))), "(!(a > 1) | b)")
})

test_that("rxode2 parses the text for a set of event-free models", {
  # rxode2 is in Enhances, which the R CMD check legs do not install; the
  # biomodels CI job installs it and requires this test to run.
  if (identical(Sys.getenv("ZUSBML_REQUIRE_RXODE2"), "true")) {
    expect_true(requireNamespace("rxode2", quietly = TRUE))
  }
  skip_if_not_installed("rxode2")
  cases <- list(
    list(
      doc = test_path("fixtures", "suite", "00000", "00000-sbml-l3v2.xml"),
      states = c("S1", "S2")
    ),
    list(
      doc = model_doc(
        compartments = '<compartment id="C" size="2" constant="false"/>',
        species = sp("A", conc = 3),
        parameters = paste0(par("q", constant = FALSE), par("k", 2)),
        rules = paste0(
          rule("rate", "C", quote(sin(time))),
          rule(
            "assignment",
            "q",
            quote(if (A > 1) quotient(k, 3) else log(k, 2))
          )
        ),
        reactions = rxn("r", quote(k * A), reactants = "A")
      ),
      states = c("C", ".amount.A")
    )
  )

  for (case in cases) {
    text <- sbml_to_rxode2(sbml_read(case$doc))
    vars <- rxode2::rxModelVars(text)
    expect_setequal(vars$state, case$states)
  }
})
