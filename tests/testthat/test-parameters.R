test_that("parameters read every Level 3 attribute", {
  p <- sbml_parameters(sbml_read(sbml_doc(paste0(
    "<listOfParameters>",
    '<parameter id="k" name="rate" value="0.1" units="per_second" constant="true"/>',
    '<parameter id="x" constant="false"/>',
    "</listOfParameters>"
  ))))

  expect_equal(p$id, c("k", "x"))
  expect_equal(p$name, c("rate", NA))
  expect_equal(p$value, c(0.1, NA))
  expect_equal(p$units, c("per_second", NA))
  expect_equal(p$constant, c(TRUE, FALSE))
})

test_that("booleans accept 1 and 0", {
  p <- sbml_parameters(sbml_read(sbml_doc(paste0(
    '<listOfParameters><parameter id="a" constant="1"/>',
    '<parameter id="b" constant="0"/></listOfParameters>'
  ))))

  expect_equal(p$constant, c(TRUE, FALSE))
})
