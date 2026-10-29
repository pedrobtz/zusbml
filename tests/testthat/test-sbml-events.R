test_that("sbml_odes() carries events as data", {
  eq <- sbml_odes(sbml_read(growth(evt(
    "e",
    quote(x > 1),
    list(p = 5),
    delay = 0.5
  ))))

  e <- eq$events[[1]]
  expect_equal(e$id, "e")
  expect_identical(e$trigger, quote(x > 1))
  expect_identical(e$delay, 0.5)
  expect_true(e$persistent)
  expect_identical(e$assignments, list(p = 5))
})

test_that("an event fires when its trigger becomes true", {
  skip_if_not_installed("deSolve")
  out <- simulate(
    growth(evt("e", quote(x > 1), list(x = 0))),
    c(0, 0.5, 1.5, 2.5)
  )

  expect_equal(out$x, c(0, 0.5, 0.5, 0.5), tolerance = 1e-6)
})

test_that("a delayed event executes after its delay", {
  skip_if_not_installed("deSolve")
  out <- simulate(
    growth(evt("e", quote(time >= 1), list(p = 7), delay = 0.5)),
    c(0, 1.25, 1.75)
  )

  expect_equal(out$p, c(0, 0, 7))
})

test_that("values come from trigger time or from execution time", {
  skip_if_not_installed("deSolve")
  at_trigger <- simulate(
    growth(evt("e", quote(time >= 1), list(p = quote(x)), delay = 1)),
    c(0, 3)
  )
  at_execution <- simulate(
    growth(evt(
      "e",
      quote(time >= 1),
      list(p = quote(x)),
      delay = 1,
      trigger_time = FALSE
    )),
    c(0, 3)
  )

  expect_equal(at_trigger$p[[2]], 1, tolerance = 1e-6)
  expect_equal(at_execution$p[[2]], 2, tolerance = 1e-6)
})

test_that("a non-persistent event is cancelled when its trigger turns false", {
  skip_if_not_installed("deSolve")
  trigger <- quote(time >= 1 & time < 1.2)
  persistent <- simulate(
    growth(evt("e", trigger, list(p = 1), delay = 0.5)),
    c(0, 2)
  )
  fleeting <- simulate(
    growth(evt("e", trigger, list(p = 1), delay = 0.5, persistent = FALSE)),
    c(0, 2)
  )

  expect_equal(persistent$p[[2]], 1)
  expect_equal(fleeting$p[[2]], 0)
})

test_that("initialValue decides whether a trigger true at time 0 fires", {
  skip_if_not_installed("deSolve")
  fires <- simulate(
    growth(evt("e", quote(time >= 0), list(p = 3), initial = FALSE)),
    c(0, 1)
  )
  waits <- simulate(
    growth(evt("e", quote(time >= 0), list(p = 3), initial = TRUE)),
    c(0, 1)
  )

  expect_equal(fires$p, c(3, 3))
  expect_equal(waits$p, c(0, 0))
})

test_that("higher priority events execute first at the same time", {
  skip_if_not_installed("deSolve")
  # Values from execution time, so the order of execution shows.
  events <- paste0(
    evt(
      "low",
      quote(time >= 1),
      list(p = quote(p * 10)),
      priority = 1,
      trigger_time = FALSE
    ),
    evt(
      "high",
      quote(time >= 1),
      list(p = quote(p + 1)),
      priority = 2,
      trigger_time = FALSE
    )
  )

  out <- simulate(growth(events, p = 1), c(0, 2))

  expect_equal(out$p[[2]], (1 + 1) * 10)
})

test_that("values from trigger time are fixed before any event executes", {
  skip_if_not_installed("deSolve")
  events <- paste0(
    evt("low", quote(time >= 1), list(p = quote(p * 10)), priority = 1),
    evt("high", quote(time >= 1), list(p = quote(p + 1)), priority = 2)
  )

  out <- simulate(growth(events, p = 1), c(0, 2))

  expect_equal(out$p[[2]], 10)
})

test_that("an event can trigger another at the same time", {
  skip_if_not_installed("deSolve")
  events <- paste0(
    evt("first", quote(time >= 1), list(p = 1)),
    evt("second", quote(p > 0.5), list(x = 100))
  )

  out <- simulate(growth(events), c(0, 1.5))

  expect_equal(out$x[[2]], 100.5, tolerance = 1e-6)
})

test_that("an event that resizes a compartment keeps species amounts", {
  skip_if_not_installed("deSolve")
  doc <- with_events(
    model_doc(
      compartments = '<compartment id="C" size="1" constant="false"/>',
      species = sp_xml("S", conc = 2)
    ),
    evt("e", quote(time >= 1), list(C = 4))
  )

  out <- simulate(doc, c(0, 2), outputs = "S")

  expect_equal(out$S, c(2, 0.5))
})

test_that("an event assigning a concentration and its compartment uses the old size", {
  skip_if_not_installed("deSolve")
  doc <- with_events(
    model_doc(
      compartments = '<compartment id="C" size="1" constant="false"/>',
      species = sp_xml("S", conc = 2)
    ),
    evt("e", quote(time >= 1), list(C = 4, S = 3))
  )

  out <- simulate(doc, c(0, 2), outputs = "S")

  expect_equal(out$S[[2]], 3 * 1 / 4)
})

test_that("an event cannot assign a constant", {
  m <- sbml_read(with_events(
    model_doc(parameters = par_xml("k", 1)),
    evt("e", quote(time >= 1), list(k = 2))
  ))

  err <- expect_error(sbml_odes(m), class = "zusbml_derive_error")
  expect_equal(err$ids, c("e", "k"))
})

test_that("an event cannot assign a rule's variable", {
  m <- sbml_read(with_events(
    model_doc(
      parameters = paste0(par_xml("q", constant = FALSE), par_xml("k", 1)),
      rules = rule("assignment", "q", quote(k))
    ),
    evt("e", quote(time >= 1), list(q = 2))
  ))

  err <- expect_error(sbml_odes(m), class = "zusbml_derive_error")
  expect_equal(err$ids, c("e", "q"))
})

test_that("an event without a trigger never fires", {
  skip_if_not_installed("deSolve")
  doc <- with_events(
    growth(""),
    paste0(
      '<event id="e" useValuesFromTriggerTime="true"><listOfEventAssignments>',
      '<eventAssignment variable="p">',
      mml(9),
      "</eventAssignment>",
      "</listOfEventAssignments></event>"
    )
  )
  doc <- sub("<listOfEvents></listOfEvents>", "", doc, fixed = TRUE)

  expect_equal(simulate(doc, c(0, 1))$p, c(0, 0))
})

test_that("a concentration taken at trigger time converts with the size at execution", {
  skip_if_not_installed("deSolve")
  # C grows at rate 1 from 1; S is set to 3 by an event triggered at time 1
  # and executed at time 2, when C is 3: the amount becomes 9.
  doc <- with_events(
    model_doc(
      compartments = '<compartment id="C" size="1" constant="false"/>',
      species = sp_xml("S", conc = 2),
      rules = rule("rate", "C", 1)
    ),
    evt("e", quote(time >= 1), list(S = 3), delay = 1)
  )

  out <- simulate(doc, c(0, 2.5), outputs = "S")

  expect_equal(out[2, ".amount.S"], 9, tolerance = 1e-6)
})
