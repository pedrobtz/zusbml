test_that("compartments read every Level 3 attribute", {
  m <- sbml_read(sbml_doc(paste0(
    "<listOfCompartments>",
    '<compartment id="C" name="cell" spatialDimensions="2" size="0.5" ',
    'units="area" constant="false" metaid="mc" sboTerm="SBO:0000290"/>',
    '<compartment id="D" constant="true"/>',
    "</listOfCompartments>"
  )))

  comps <- sbml_compartments(m)

  expect_equal(comps$id, c("C", "D"))
  expect_equal(comps$name, c("cell", NA))
  expect_equal(comps$spatial_dimensions, c(2, NA))
  expect_equal(comps$size, c(0.5, NA))
  expect_equal(comps$units, c("area", NA))
  expect_equal(comps$constant, c(FALSE, TRUE))
  expect_equal(comps$metaid, c("mc", NA))
  expect_equal(comps$sbo_term, c("SBO:0000290", NA))
})

test_that("a compartment without constant is a parse error", {
  doc <- sbml_doc(
    '<listOfCompartments><compartment id="C"/></listOfCompartments>'
  )

  err <- expect_error(sbml_read(doc), class = "zusbml_parse_error")
  expect_equal(err$attribute, "constant")
})

test_that("an empty model has typed, empty tables", {
  comps <- sbml_compartments(sbml_read(sbml_doc()))

  expect_equal(nrow(comps), 0)
  expect_type(comps$size, "double")
  expect_type(comps$constant, "logical")
})
