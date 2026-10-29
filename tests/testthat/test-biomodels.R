test_that("fifty BioModels are fixed", {
  expect_length(biomodels_ids(), 50)
})

test_that("the fixed BioModels read, round-trip, derive and check clean", {
  dir <- biomodels_dir()
  ids <- biomodels_ids()
  # The value of `expr`, or NULL after a failure naming the model and step:
  # a model that errors is reported without hiding the others, and the
  # expectations below run outside it so that their differences show.
  step <- function(id, what, expr) {
    tryCatch(expr, error = function(cnd) {
      fail(paste0(id, " (", what, "): ", conditionMessage(cnd)))
      NULL
    })
  }

  for (id in ids) {
    m <- step(id, "read", sbml_read(file.path(dir, paste0(id, ".xml"))))
    if (is.null(m)) {
      next
    }
    back <- step(id, "write and re-read", biomodels_comparable(m))
    if (!is.null(back)) {
      expect_identical(back, m, info = id)
    }
    expect_equal(nrow(sbml_check(m)), 0, info = id)
    f <- step(id, "derive", sbml_derivs(m))
    if (is.null(f)) {
      next
    }
    dy <- step(id, "evaluate", f(0, attr(f, "y0"), attr(f, "parms"))[[1]])
    if (is.null(dy)) {
      next
    }
    expect_equal(length(dy), length(attr(f, "y0")), info = id)
    expect_true(all(is.finite(dy)), info = id)
  }
})
