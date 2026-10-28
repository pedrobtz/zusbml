test_that("events read trigger, priority, delay and assignments", {
  mathml <- function(x) {
    paste0(
      '<math xmlns="http://www.w3.org/1998/Math/MathML"><cn>',
      x,
      "</cn></math>"
    )
  }
  m <- sbml_read(sbml_doc(paste0(
    '<listOfEvents><event id="e" useValuesFromTriggerTime="false">',
    '<trigger initialValue="true" persistent="false">',
    mathml(1),
    "</trigger>",
    "<priority>",
    mathml(2),
    "</priority>",
    "<delay>",
    mathml(3),
    "</delay>",
    '<listOfEventAssignments><eventAssignment variable="x">',
    mathml(4),
    "</eventAssignment></listOfEventAssignments>",
    "</event></listOfEvents>"
  )))

  e <- sbml_events(m)

  expect_equal(e$id, "e")
  expect_false(e$use_values_from_trigger_time)
  expect_true(e$trigger[[1]]$initial_value)
  expect_false(e$trigger[[1]]$persistent)
  expect_identical(e$trigger[[1]]$math, 1)
  expect_identical(e$priority[[1]]$math, 2)
  expect_identical(e$delay[[1]]$math, 3)
  expect_identical(e$assignments[[1]]$math[[1]], 4)
  expect_equal(e$assignments[[1]]$variable, "x")
})

test_that("an event without priority or delay has NULL there", {
  m <- sbml_read(sbml_doc(paste0(
    '<listOfEvents><event useValuesFromTriggerTime="true">',
    '<trigger initialValue="false" persistent="true"/></event></listOfEvents>'
  )))

  e <- sbml_events(m)

  expect_true(is.na(e$id))
  expect_null(e$priority[[1]])
  expect_null(e$delay[[1]])
  expect_equal(nrow(e$assignments[[1]]), 0)
})

test_that("a trigger needs initialValue and persistent", {
  doc <- sbml_doc(paste0(
    '<listOfEvents><event useValuesFromTriggerTime="true">',
    '<trigger persistent="true"/></event></listOfEvents>'
  ))

  err <- expect_error(sbml_read(doc), class = "zusbml_parse_error")
  expect_equal(err$attribute, "initialValue")
})
