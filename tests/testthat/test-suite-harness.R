test_that("suite_read_settings() parses keys, numbers and lists", {
  settings <- suite_read_settings(
    file.path(fixture_suite(), "00000", "00000-settings.txt")
  )

  expect_equal(settings$start, 0)
  expect_equal(settings$duration, 2)
  expect_equal(settings$steps, 4)
  expect_equal(settings$variables, c("S1", "S2"))
  expect_equal(settings$absolute, 1e-6)
  expect_equal(settings$relative, 1e-4)
  expect_equal(settings$amount, "S1")
  expect_equal(settings$concentration, "S2")
})

test_that("suite_read_settings() reads an empty list as character(0)", {
  path <- withr::local_tempfile(lines = c("start: 0", "concentration:"))

  settings <- suite_read_settings(path)

  expect_identical(settings$concentration, character(0))
})

test_that("suite_read_results() keeps column names as written", {
  results <- suite_read_results(
    file.path(fixture_suite(), "00000", "00000-results.csv")
  )

  expect_named(results, c("time", "S1", "S2"))
  expect_equal(nrow(results), 5)
  expect_equal(results$S1[[3]], exp(-1))
})

test_that("suite_read_tags() reads component tags, test tags and test type", {
  tags <- suite_read_tags(file.path(fixture_suite(), "00000", "00000-model.m"))

  expect_equal(
    tags$component,
    c("Compartment", "Species", "Reaction", "Parameter")
  )
  expect_equal(tags$test, c("Amount", "Concentration"))
  expect_equal(tags$type, "TimeCourse")
})

test_that("suite_read_tags() does not run an empty tag line into the next", {
  path <- withr::local_tempfile(
    lines = c("componentTags: Species", "testTags:", "testType: TimeCourse")
  )

  tags <- suite_read_tags(path)

  expect_identical(tags$test, character(0))
  expect_equal(tags$type, "TimeCourse")
})

test_that("suite_category() follows the supported tag set", {
  tags <- function(
    component = "Species",
    test = character(0),
    type = "TimeCourse"
  ) {
    list(component = component, test = test, type = type)
  }

  expect_equal(suite_category(tags()), "supported")
  expect_equal(suite_category(tags(component = "EventNoDelay")), "supported")
  expect_equal(suite_category(tags(component = "AlgebraicRule")), "unsupported")
  expect_equal(suite_category(tags(component = "CSymbolDelay")), "unsupported")
  expect_equal(suite_category(tags(test = "FastReaction")), "unsupported")
  expect_equal(suite_category(tags(component = "comp:Submodel")), "unsupported")
  expect_equal(suite_category(tags(test = "fbc:NonStrict")), "unsupported")
  expect_equal(
    suite_category(tags(type = "FluxBalanceSteadyState")),
    "unsupported"
  )
})

test_that("suite_compare() applies absolute plus relative tolerance", {
  expected <- data.frame(time = c(0, 1), S1 = c(1, 100))

  within <- data.frame(time = c(0, 1), S1 = c(1 + 0.9e-6, 100 + 0.0099))
  outside <- data.frame(time = c(0, 1), S1 = c(1, 100 + 0.0101))

  expect_true(suite_compare(within, expected, absolute = 1e-6, relative = 1e-4))
  expect_false(suite_compare(
    outside,
    expected,
    absolute = 1e-6,
    relative = 1e-4
  ))
})

test_that("suite_compare() matches non-finite values exactly", {
  expected <- data.frame(time = 0:2, S1 = c(NaN, Inf, -Inf))

  expect_true(suite_compare(expected, expected, absolute = 0, relative = 0))
  expect_false(suite_compare(
    data.frame(time = 0:2, S1 = c(0, Inf, -Inf)),
    expected,
    absolute = 1,
    relative = 1
  ))
})

test_that("suite_compare() fails on a missing column or a row count mismatch", {
  expected <- data.frame(time = 0:1, S1 = c(1, 2))

  expect_false(suite_compare(
    data.frame(time = 0:1),
    expected,
    absolute = 1,
    relative = 1
  ))
  expect_false(suite_compare(
    data.frame(time = 0, S1 = 1),
    expected,
    absolute = 1,
    relative = 1
  ))
})

test_that("suite_case() gathers a case's files and tags", {
  case <- suite_case(fixture_suite(), "00000")

  expect_equal(case$id, "00000")
  expect_equal(case$category, "supported")
  expect_equal(case$settings$variables, c("S1", "S2"))
  expect_equal(basename(case$model), "00000-sbml-l3v2.xml")
})

test_that("suite_run_case() reports a case as pending before the reader exists", {
  case <- suite_case(fixture_suite(), "00000")

  expect_equal(suite_run_case(case)$status, "pending")
})

test_that("suite_run() returns one row per case with a status", {
  report <- suite_run(fixture_suite())

  expect_named(report, c("id", "category", "status", "message"))
  expect_equal(report$id, "00000")
  expect_equal(report$status, "pending")
})

test_that("suite_floor() reads the committed floors as integers", {
  floor <- suite_floor()

  expect_named(floor, c("stage", "read", "pass", "refused"))
  expect_true(all(vapply(floor, is.integer, logical(1))))
})

test_that("suite_dir() skips when the suite is absent and not required", {
  withr::local_envvar(ZUSBML_TEST_SUITE = "", ZUSBML_REQUIRE_SUITE = "")

  expect_condition(suite_dir(), class = "skip")
})

test_that("suite_dir() errors when the suite is absent but required", {
  withr::local_envvar(ZUSBML_TEST_SUITE = "", ZUSBML_REQUIRE_SUITE = "true")

  expect_error(suite_dir(), class = "zusbml_suite_missing_error")
})
