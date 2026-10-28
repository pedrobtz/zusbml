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
        par("x", 0, constant = FALSE),
        par("p", 0, constant = FALSE)
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
