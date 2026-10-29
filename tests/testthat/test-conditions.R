# Condition classes and fields are asserted where each condition is raised;
# these snapshots pin the wording users read.

test_that("a parse error names the element, attribute and section", {
  doc <- sbml_doc('<listOfParameters><parameter id="p"/></listOfParameters>')

  expect_snapshot(sbml_read(doc), error = TRUE)
})

test_that("an unsupported package is named", {
  doc <- sbml_doc(
    version = 1,
    sbml_attrs = paste0(
      ' xmlns:comp="http://www.sbml.org/sbml/level3/version1/comp/version1"',
      ' comp:required="true"'
    )
  )

  expect_snapshot(sbml_read(doc), error = TRUE)
})

test_that("an algebraic rule is refused in derivation", {
  m <- sbml_read(model_doc(
    parameters = par_xml("p", 1, constant = FALSE),
    rules = paste0("<algebraicRule>", mml(quote(p - 1)), "</algebraicRule>")
  ))

  expect_snapshot(sbml_odes(m), error = TRUE)
})

test_that("an assignment cycle names its ids", {
  m <- sbml_read(model_doc(
    parameters = paste0(
      par_xml("a", constant = FALSE),
      par_xml("b", constant = FALSE)
    ),
    rules = paste0(
      rule("assignment", "a", quote(b)),
      rule("assignment", "b", quote(a))
    )
  ))

  expect_snapshot(sbml_odes(m), error = TRUE)
})

test_that("a write error names the R call", {
  r_to_mathml <- utils::getFromNamespace("r_to_mathml", "zusbml")

  expect_snapshot(r_to_mathml(quote(trunc(x))), error = TRUE)
})

test_that("an argument error names the argument", {
  expect_snapshot(sbml_read(1), error = TRUE)
  expect_snapshot(sbml_odes(list()), error = TRUE)
})

test_that("a parse error in nested math names its path", {
  doc <- model_doc(
    parameters = par_xml("p", constant = FALSE),
    rules = paste0(
      '<assignmentRule variable="p">',
      '<math xmlns="http://www.w3.org/1998/Math/MathML">',
      "<apply><plus/><cn>1</cn><ci>.x</ci></apply></math></assignmentRule>"
    )
  )

  expect_snapshot(sbml_read(doc), error = TRUE)
})
