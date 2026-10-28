test_that("unit definitions read their units", {
  m <- sbml_read(sbml_doc(paste0(
    '<listOfUnitDefinitions><unitDefinition id="per_mM">',
    '<listOfUnits><unit kind="mole" exponent="-1" scale="-3" multiplier="1"/>',
    '<unit kind="litre" exponent="1" scale="0" multiplier="1"/></listOfUnits>',
    "</unitDefinition></listOfUnitDefinitions>"
  )))

  defs <- sbml_unit_definitions(m)
  units <- defs$units[[1]]

  expect_equal(defs$id, "per_mM")
  expect_equal(units$kind, c("mole", "litre"))
  expect_equal(units$exponent, c(-1, 1))
  expect_identical(units$scale, c(-3L, 0L))
  expect_equal(units$multiplier, c(1, 1))
})

test_that("every unit attribute is required in Level 3", {
  doc <- sbml_doc(paste0(
    '<listOfUnitDefinitions><unitDefinition id="u"><listOfUnits>',
    '<unit kind="mole" exponent="1" multiplier="1"/>',
    "</listOfUnits></unitDefinition></listOfUnitDefinitions>"
  ))

  err <- expect_error(sbml_read(doc), class = "zusbml_parse_error")
  expect_equal(err$attribute, "scale")
})

test_that("a non-integer scale is a parse error", {
  doc <- sbml_doc(paste0(
    '<listOfUnitDefinitions><unitDefinition id="u"><listOfUnits>',
    '<unit kind="mole" exponent="1" scale="1.5" multiplier="1"/>',
    "</listOfUnits></unitDefinition></listOfUnitDefinitions>"
  ))

  expect_error(sbml_read(doc), class = "zusbml_parse_error")
})
