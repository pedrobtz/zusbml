test_that("function definitions, initial assignments and constraints read", {
  mathml <- '<math xmlns="http://www.w3.org/1998/Math/MathML"><cn>1</cn></math>'
  lambda <- paste0(
    '<math xmlns="http://www.w3.org/1998/Math/MathML">',
    "<lambda><bvar><ci>x</ci></bvar><ci>x</ci></lambda></math>"
  )
  m <- sbml_read(sbml_doc(paste0(
    '<listOfFunctionDefinitions><functionDefinition id="f">',
    lambda,
    "</functionDefinition></listOfFunctionDefinitions>",
    '<listOfInitialAssignments><initialAssignment symbol="x">',
    mathml,
    "</initialAssignment></listOfInitialAssignments>",
    "<listOfConstraints><constraint>",
    mathml,
    '<message><p xmlns="http://www.w3.org/1999/xhtml">x is positive</p></message>',
    "</constraint></listOfConstraints>"
  )))

  expect_equal(sbml_functions(m)$id, "f")
  expect_equal(sbml_initial_assignments(m)$symbol, "x")
  expect_match(sbml_constraints(m)$message, "x is positive", fixed = TRUE)
})

test_that("a function definition needs an id", {
  doc <- sbml_doc(
    "<listOfFunctionDefinitions><functionDefinition/></listOfFunctionDefinitions>"
  )

  err <- expect_error(sbml_read(doc), class = "zusbml_parse_error")
  expect_equal(err$attribute, "id")
})

test_that("a function definition that is not a lambda names the file", {
  path <- withr::local_tempfile(fileext = ".xml")
  writeLines(
    sbml_doc(paste0(
      '<listOfFunctionDefinitions><functionDefinition id="f">',
      '<math xmlns="http://www.w3.org/1998/Math/MathML"><cn>1</cn></math>',
      "</functionDefinition></listOfFunctionDefinitions>"
    )),
    path
  )

  err <- expect_error(sbml_read(path), class = "zusbml_parse_error")
  expect_identical(err$file, path)
  expect_identical(err$section, "4.3")
})
