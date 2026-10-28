test_that("fifty BioModels are fixed", {
  expect_length(biomodels_ids(), 50)
})

test_that("the fixed BioModels read, round-trip, derive and check clean", {
  dir <- biomodels_dir()
  ids <- biomodels_ids()

  for (id in ids) {
    outcome <- tryCatch(
      {
        m <- sbml_read(file.path(dir, paste0(id, ".xml")))
        expect_identical(biomodels_comparable(m), m, info = id)
        expect_equal(nrow(sbml_check(m)), 0, info = id)
        f <- sbml_derivs(m)
        dy <- f(0, attr(f, "y0"), attr(f, "parms"))[[1]]
        expect_length(dy, length(attr(f, "y0")))
        expect_true(all(is.finite(dy)), info = id)
        NULL
      },
      error = function(cnd) paste(id, conditionMessage(cnd))
    )
    # Report a model that errors without hiding the others.
    expect_null(outcome)
  }
})
