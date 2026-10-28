test_that("print() shows level, version and non-zero counts", {
  m <- sbml_read(test_path("fixtures", "suite", "00000", "00000-sbml-l3v2.xml"))

  expect_snapshot(print(m))
})

test_that("print() names ignored packages and handles an empty model", {
  doc <- sbml_doc(
    version = 1,
    sbml_attrs = paste0(
      ' xmlns:fbc="http://www.sbml.org/sbml/level3/version1/fbc/version2"',
      ' fbc:required="false"'
    )
  )

  expect_snapshot(print(sbml_read(doc, ignore_packages = TRUE)))
})

test_that("summary() returns the counts", {
  m <- sbml_read(test_path("fixtures", "suite", "00000", "00000-sbml-l3v2.xml"))

  s <- summary(m)

  expect_s3_class(s, "summary.sbml_model")
  expect_equal(s$counts[["species"]], 2L)
  expect_equal(s$counts[["reactions"]], 1L)
  expect_snapshot(print(s))
})

test_that("accessors refuse anything but a model", {
  accessors <- list(
    sbml_compartments,
    sbml_species,
    sbml_parameters,
    sbml_reactions,
    sbml_rules,
    sbml_events,
    sbml_functions,
    sbml_initial_assignments,
    sbml_constraints,
    sbml_unit_definitions
  )

  for (accessor in accessors) {
    expect_error(accessor(list()))
  }
})

test_that("zusbml_info() states what is read and written", {
  info <- zusbml_info()

  expect_equal(info$read, c("2.4", "2.5", "3.1", "3.2"))
  expect_equal(info$write, "3.2")
  expect_identical(info$packages, character(0))
})

test_that("the bundled example reads", {
  m <- sbml_read(system.file("extdata", "decay.xml", package = "zusbml"))

  expect_equal(m$id, "decay")
})
