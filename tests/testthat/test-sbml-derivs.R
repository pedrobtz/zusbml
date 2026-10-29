test_that("sbml_derivs() carries y0 and parms for a model without events", {
  m <- sbml_read(test_path("fixtures", "suite", "00000", "00000-sbml-l3v2.xml"))

  f <- sbml_derivs(m)

  expect_equal(attr(f, "y0"), c(S1 = 1, S2 = 0))
  expect_equal(attr(f, "parms"), c(C = 1, k1 = 1))
  expect_null(attr(f, "rootfunc"))
  expect_equal(f(0, attr(f, "y0"), attr(f, "parms"))[[1]], c(-1, 1))
})

test_that("sbml_derivs() reports requested outputs", {
  m <- sbml_read(test_path("fixtures", "suite", "00000", "00000-sbml-l3v2.xml"))

  f <- sbml_derivs(m, outputs = c("R1", "k1"))

  expect_equal(f(0, attr(f, "y0"), attr(f, "parms"))[[2]], c(R1 = 1, k1 = 1))
})

test_that("sbml_derivs() integrates with deSolve to the analytic solution", {
  skip_if_not_installed("deSolve")
  m <- sbml_read(test_path("fixtures", "suite", "00000", "00000-sbml-l3v2.xml"))
  f <- sbml_derivs(m)

  out <- deSolve::ode(
    attr(f, "y0"),
    c(0, 1, 2),
    f,
    attr(f, "parms"),
    rtol = 1e-10,
    atol = 1e-12
  )

  expect_equal(unname(out[, "S2"]), 1 - exp(-c(0, 1, 2)), tolerance = 1e-8)
})

test_that("time 0 events are applied to y0, and a rerun starts afresh", {
  skip_if_not_installed("deSolve")
  doc <- with_events(
    model_doc(
      parameters = paste0(
        par_xml("x", 0, constant = FALSE),
        par_xml("p", 0, constant = FALSE)
      ),
      rules = rule("rate", "x", 1)
    ),
    paste0(
      evt("now", quote(time >= 0), list(p = 1), initial = FALSE),
      evt("later", quote(time >= 1), list(p = quote(p + 10)))
    )
  )
  f <- sbml_derivs(sbml_read(doc))
  run <- function() {
    deSolve::ode(
      attr(f, "y0"),
      c(0, 2),
      f,
      attr(f, "parms"),
      method = "lsodar",
      rootfunc = attr(f, "rootfunc"),
      events = attr(f, "events")
    )
  }

  expect_equal(attr(f, "y0")[["p"]], 1)
  expect_equal(unname(run()[2, "p"]), 11)
  expect_equal(unname(run()[2, "p"]), 11)
})

test_that("events use the parms the solver passes", {
  skip_if_not_installed("deSolve")
  m <- sbml_read(system.file("extdata", "dosing.xml", package = "zusbml"))
  f <- sbml_derivs(m)
  p <- attr(f, "parms")
  p[["dose"]] <- 0

  out <- deSolve::ode(
    attr(f, "y0"),
    c(0, 11.9, 12.1),
    f,
    p,
    method = "lsodar",
    rootfunc = attr(f, "rootfunc"),
    events = attr(f, "events")
  )

  # No second dose: the gut keeps emptying through hour 12.
  expect_lt(out[3, "drug_gut"], out[2, "drug_gut"])
})

test_that("parms overrides reach initial assignments and y0", {
  doc <- model_doc(
    species = sp_xml("A", only_substance = TRUE),
    parameters = par_xml("dose", 5),
    initial = paste0(
      '<initialAssignment symbol="A">',
      mml(quote(2 * dose)),
      "</initialAssignment>"
    )
  )
  m <- sbml_read(doc)

  f <- sbml_derivs(m, parms = c(dose = 7))

  expect_equal(attr(f, "y0"), c(A = 14))
  expect_equal(attr(f, "parms")[["dose"]], 7)
  expect_equal(attr(sbml_derivs(m), "y0"), c(A = 10))
})

test_that("time 0 events follow parms overrides", {
  doc <- with_events(
    model_doc(
      parameters = paste0(par_xml("p", 0, constant = FALSE), par_xml("k", 1))
    ),
    evt("now", quote(time >= 0), list(p = quote(k * 3)), initial = FALSE)
  )

  f <- sbml_derivs(sbml_read(doc), parms = c(k = 2))

  expect_equal(attr(f, "y0")[["p"]], 6)
})

test_that("sbml_derivs() checks parms and outputs", {
  m <- sbml_read(test_path("fixtures", "suite", "00000", "00000-sbml-l3v2.xml"))

  err <- expect_error(
    sbml_derivs(m, parms = c(nope = 1)),
    class = "zusbml_arg_error"
  )
  expect_equal(err$argument, "parms")
  expect_error(sbml_derivs(m, parms = c(1, 2)), class = "zusbml_arg_error")
  expect_error(sbml_derivs(m, parms = c(S1 = 2)), class = "zusbml_arg_error")
  err <- expect_error(
    sbml_derivs(m, outputs = "nope"),
    class = "zusbml_arg_error"
  )
  expect_equal(err$argument, "outputs")
  expect_error(
    sbml_derivs(m, outputs = NA_character_),
    class = "zusbml_arg_error"
  )
  expect_error(sbml_derivs(m, outputs = 1), class = "zusbml_arg_error")
})

test_that("a NaN priority ranks lowest instead of failing", {
  skip_if_not_installed("deSolve")
  events <- paste0(
    evt(
      "odd",
      quote(time >= 1),
      list(p = quote(p * 10)),
      priority = NaN,
      trigger_time = FALSE
    ),
    evt(
      "plain",
      quote(time >= 1),
      list(p = quote(p + 1)),
      priority = 1,
      trigger_time = FALSE
    )
  )

  out <- simulate(growth(events, p = 1), c(0, 2))

  expect_equal(out$p[[2]], (1 + 1) * 10)
})

test_that("a delay that is not a finite non-negative number is a derive error", {
  skip_if_not_installed("deSolve")
  doc <- growth(evt("e", quote(time >= 1), list(p = 1), delay = -1))

  err <- expect_error(simulate(doc, c(0, 2)), class = "zusbml_derive_error")
  expect_equal(err$ids, "e")
})
