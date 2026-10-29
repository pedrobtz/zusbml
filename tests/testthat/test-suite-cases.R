test_that("a Level 2 file outside the tolerances is reported", {
  skip_if_not_installed("deSolve")
  dir <- withr::local_tempdir()
  case_dir <- copy_fixture(dir)
  l2 <- file.path(case_dir, "00000-sbml-l2v5.xml")
  writeLines(sub('id="k1" value="1"', 'id="k1" value="2"', readLines(l2)), l2)

  outcome <- suite_run_case(suite_case(dir, "00000"))

  expect_equal(outcome$status, "pass")
  expect_equal(outcome$level2, "fail: tolerances")
})

test_that("a Level 2 file that derives other equations is reported", {
  skip_if_not_installed("deSolve")
  dir <- withr::local_tempdir()
  case_dir <- copy_fixture(dir)
  l2 <- file.path(case_dir, "00000-sbml-l2v5.xml")
  # The same rate with its factors in another order simulates the same.
  writeLines(
    sub(
      "<ci> C </ci><ci> k1 </ci>",
      "<ci> k1 </ci><ci> C </ci>",
      readLines(l2)
    ),
    l2
  )

  outcome <- suite_run_case(suite_case(dir, "00000"))

  expect_equal(outcome$status, "pass")
  expect_equal(outcome$level2, "fail: equations differ in assignments")
})

test_that("suite_equation_differences() names the parts that differ", {
  eq <- function(rhs, assignments = list(), constants = numeric(0)) {
    list(
      states = data.frame(name = names(rhs)),
      rhs = rhs,
      assignments = assignments,
      constants = constants
    )
  }

  same <- eq(list(S = quote(k * .time)))
  expect_equal(
    suite_equation_differences(same, eq(list(S = quote(k * time)))),
    character(0)
  )
  expect_equal(
    suite_equation_differences(same, eq(list(S = quote(time * k)))),
    "rhs"
  )
  expect_equal(
    suite_equation_differences(same, eq(list(T = quote(k * time)))),
    c("states", "rhs")
  )
  expect_equal(
    suite_equation_differences(
      eq(list(S = quote(k)), list(a = 1, b = 2)),
      eq(list(S = quote(k)), list(b = 2, a = 1))
    ),
    "assignments"
  )
  # With a symbol named time, `.time` is the csymbol and `time` is not.
  expect_equal(
    suite_equation_differences(
      eq(list(S = quote(.time)), constants = c(time = 1)),
      eq(list(S = quote(time)), constants = c(time = 1))
    ),
    "rhs"
  )
})

test_that("a supported case whose model cannot be read is a read failure", {
  dir <- withr::local_tempdir()
  case_dir <- copy_fixture(dir)
  writeLines("<sbml/>", file.path(case_dir, "00000-sbml-l3v2.xml"))

  outcome <- suite_run_case(suite_case(dir, "00000"))

  expect_equal(outcome$status, "read_fail")
})

test_that("a supported case the package refuses is a failure", {
  dir <- withr::local_tempdir()
  case_dir <- copy_fixture(dir)
  writeLines(
    paste0(
      '<sbml xmlns="http://www.sbml.org/sbml/level3/version1/core" ',
      'xmlns:comp="http://www.sbml.org/sbml/level3/version1/comp/version1" ',
      'level="3" version="1" comp:required="true"><model/></sbml>'
    ),
    file.path(case_dir, "00000-sbml-l3v2.xml")
  )

  outcome <- suite_run_case(suite_case(dir, "00000"))

  expect_equal(outcome$category, "supported")
  # The reader refuses it, so it was never read.
  expect_equal(outcome$status, "read_fail")
})

test_that("a supported case refused after reading is a failure", {
  dir <- withr::local_tempdir()
  case_dir <- copy_fixture(dir)
  path <- file.path(case_dir, "00000-sbml-l3v2.xml")
  rule <- paste0(
    "<listOfRules><algebraicRule>",
    '<math xmlns="http://www.w3.org/1998/Math/MathML"><ci> k1 </ci></math>',
    "</algebraicRule></listOfRules></model>"
  )
  writeLines(sub("</model>", rule, readLines(path), fixed = TRUE), path)

  outcome <- suite_run_case(suite_case(dir, "00000"))

  expect_equal(outcome$category, "supported")
  expect_equal(outcome$status, "fail")
})

test_that("an unsupported case the package derives is a failure", {
  dir <- withr::local_tempdir()
  case_dir <- copy_fixture(dir)
  writeLines(
    "componentTags: AlgebraicRule\ntestType: TimeCourse",
    file.path(case_dir, "00000-model.m")
  )

  outcome <- suite_run_case(suite_case(dir, "00000"))

  expect_equal(outcome$category, "unsupported")
  expect_equal(outcome$status, "fail")
})

test_that("an error that is not a zusbml condition is a harness error", {
  local_mocked_bindings(
    sbml_read = function(...) stop("boom"),
    .package = "zusbml"
  )

  outcome <- suite_run_case(suite_case(fixture_suite(), "00000"))

  expect_equal(outcome$status, "error")
  expect_equal(outcome$message, "boom")
})

test_that("math that changes on a round trip is a failure", {
  local_mocked_bindings(
    r_to_mathml = function(expr, ids = character(0)) {
      '<math xmlns="http://www.w3.org/1998/Math/MathML"><cn>0</cn></math>'
    },
    .package = "zusbml"
  )

  outcome <- suite_run_case(suite_case(fixture_suite(), "00000"))

  expect_equal(outcome$status, "fail")
  expect_match(outcome$message, "MathML round trip changed")
})

test_that("a model that changes on write and re-read is a failure", {
  other <- paste(
    readLines(file.path(fixture_suite(), "00000", "00000-sbml-l3v2.xml")),
    collapse = "\n"
  )
  other <- sub('id="k1" value="1"', 'id="k1" value="2"', other)
  local_mocked_bindings(
    sbml_write = function(m, path = NULL) other,
    .package = "zusbml"
  )

  outcome <- suite_run_case(suite_case(fixture_suite(), "00000"))

  expect_equal(outcome$status, "fail")
  expect_equal(outcome$message, "read, write, read changed the model")
})

test_that("shards partition the cases, with the slowest one first", {
  dir <- withr::local_tempdir()
  ids <- c(sprintf("%05d", 1:20), "00966")
  for (id in ids) {
    dir.create(file.path(dir, id))
    file.create(file.path(dir, id, paste0(id, "-sbml-l3v2.xml")))
    slow <- id %in% c("00966", "00003", "00007", "00011")
    writeLines(
      if (slow) "testTags: RandomEventExecution" else "testTags: Amount",
      file.path(dir, id, paste0(id, "-model.m"))
    )
  }

  for (n in 2:3) {
    shards <- lapply(seq_len(n), function(i) {
      withr::with_envvar(
        c(ZUSBML_SUITE_SHARD = paste0(i, "/", n), ZUSBML_SUITE_SKIP = NA),
        suite_cases(dir)
      )
    })

    expect_setequal(unlist(shards), ids)
    expect_equal(anyDuplicated(unlist(shards)), 0)
    expect_true("00966" %in% shards[[1]])
  }
})

test_that("ZUSBML_SUITE_SKIP removes exactly the named cases", {
  dir <- withr::local_tempdir()
  for (id in c("00001", "00002", "00003")) {
    dir.create(file.path(dir, id))
    file.create(file.path(dir, id, paste0(id, "-sbml-l3v2.xml")))
  }
  withr::local_envvar(
    ZUSBML_SUITE_SHARD = NA,
    ZUSBML_SUITE_SKIP = "00002, 00009"
  )

  expect_equal(suite_cases(dir), c("00001", "00003"))
})

test_that("suite_floor_failures() lowers the floors for skipped cases", {
  floor <- list(read = 3L, pass = 3L, refused = 1L, level2 = 1L)
  report <- data.frame(
    id = c("1", "2", "3"),
    status = c("pass", "pass", "refused"),
    level2 = c("pass", "none", "none")
  )

  expect_equal(suite_floor_failures(report, floor, skipped = "4"), character(0))
  expect_equal(
    suite_floor_failures(report, floor, skipped = character(0)),
    c("read: 2 below the floor of 3", "pass: 2 below the floor of 3")
  )
})

test_that("the read floor counts read cases, failing or not, and no others", {
  floor <- list(read = 3L, pass = 1L, refused = 0L, level2 = 0L)
  report <- function(status) {
    data.frame(id = seq_along(status), status = status, level2 = "none")
  }

  expect_equal(
    suite_floor_failures(
      report(c("pass", "fail", "fail")),
      floor,
      character(0)
    ),
    character(0)
  )
  expect_equal(
    suite_floor_failures(
      report(c("pass", "fail", "read_fail", "error", "refused")),
      floor,
      character(0)
    ),
    "read: 2 below the floor of 3"
  )
})
