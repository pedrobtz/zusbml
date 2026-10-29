test_that("sbml_write_json() writes tables as records and math as R code", {
  skip_if_not_installed("zujson")
  m <- sbml_read(test_path("fixtures", "suite", "00000", "00000-sbml-l3v2.xml"))

  json <- zujson::json_parse(sbml_write_json(m))

  expect_equal(json$id, "decay")
  expect_equal(vapply(json$species, function(s) s$id, ""), c("S1", "S2"))
  expect_equal(json$reactions[[1]]$kinetic_law$math, "C * k1 * S1")
})

test_that("sbml_write_json() writes to a file", {
  skip_if_not_installed("zujson")
  m <- sbml_read(test_path("fixtures", "suite", "00000", "00000-sbml-l3v2.xml"))
  path <- withr::local_tempfile(fileext = ".json")

  expect_identical(sbml_write_json(m, path), path)
  expect_identical(
    zujson::json_parse(readChar(path, file.size(path), useBytes = TRUE)),
    zujson::json_parse(sbml_write_json(m))
  )
})
