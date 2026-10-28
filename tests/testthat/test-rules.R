test_that("rules read their type, variable and math", {
  mathml <- '<math xmlns="http://www.w3.org/1998/Math/MathML"><cn>1</cn></math>'
  m <- sbml_read(sbml_doc(paste0(
    "<listOfRules>",
    '<assignmentRule variable="a">',
    mathml,
    "</assignmentRule>",
    '<rateRule variable="b">',
    mathml,
    "</rateRule>",
    "<algebraicRule>",
    mathml,
    "</algebraicRule>",
    "</listOfRules>"
  )))

  rules <- sbml_rules(m)

  expect_equal(rules$type, c("assignment", "rate", "algebraic"))
  expect_equal(rules$variable, c("a", "b", NA))
  expect_identical(rules$math, list(1, 1, 1))
})

test_that("an assignment rule needs a variable", {
  doc <- sbml_doc("<listOfRules><assignmentRule/></listOfRules>")

  err <- expect_error(sbml_read(doc), class = "zusbml_parse_error")
  expect_equal(err$attribute, "variable")
})

test_that("a listOfRules holds only rules", {
  doc <- sbml_doc(
    '<listOfRules><parameter id="p" constant="true"/></listOfRules>'
  )

  expect_error(sbml_read(doc), class = "zusbml_parse_error")
})
