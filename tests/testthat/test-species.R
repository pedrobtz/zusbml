test_that("species read every Level 3 attribute", {
  m <- sbml_read(sbml_doc(one_species(paste(
    ' name="glucose" initialConcentration="2.5" substanceUnits="mole"',
    'conversionFactor="cf"'
  ))))

  s <- sbml_species(m)

  expect_equal(s$id, "S")
  expect_equal(s$name, "glucose")
  expect_equal(s$compartment, "C")
  expect_true(is.na(s$initial_amount))
  expect_equal(s$initial_concentration, 2.5)
  expect_equal(s$substance_units, "mole")
  expect_false(s$has_only_substance_units)
  expect_false(s$boundary_condition)
  expect_false(s$constant)
  expect_equal(s$conversion_factor, "cf")
})

test_that("species without a Level 3 default read as NA", {
  s <- sbml_species(sbml_read(sbml_doc(one_species())))

  expect_true(is.na(s$initial_amount))
  expect_true(is.na(s$initial_concentration))
})

test_that("each required species boolean is enforced", {
  for (attr in c("hasOnlySubstanceUnits", "boundaryCondition", "constant")) {
    doc <- sub(paste0(attr, '="false" '), "", sbml_doc(one_species()))
    doc <- sub(paste0(" ", attr, '="false"'), "", doc)

    err <- expect_error(sbml_read(doc), class = "zusbml_parse_error")
    expect_equal(err$attribute, attr)
  }
})

test_that("a species needs its compartment", {
  doc <- sub('compartment="C" ', "", sbml_doc(one_species()))

  err <- expect_error(sbml_read(doc), class = "zusbml_parse_error")
  expect_equal(err$attribute, "compartment")
})

test_that("XML Schema doubles INF, -INF and NaN are read", {
  doc <- sbml_doc(paste0(
    "<listOfParameters>",
    '<parameter id="a" value="INF" constant="true"/>',
    '<parameter id="b" value="-INF" constant="true"/>',
    '<parameter id="c" value="NaN" constant="true"/>',
    '<parameter id="d" value="1.5e-3" constant="true"/>',
    "</listOfParameters>"
  ))

  expect_identical(
    sbml_parameters(sbml_read(doc))$value,
    c(Inf, -Inf, NaN, 1.5e-3)
  )
})
