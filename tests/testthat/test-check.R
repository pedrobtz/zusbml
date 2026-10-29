test_that("a well-formed model has no findings", {
  m <- sbml_read(test_path("fixtures", "suite", "00000", "00000-sbml-l3v2.xml"))

  found <- sbml_check(m)

  expect_equal(nrow(found), 0)
  expect_named(found, c("rule", "id", "message"))
})

test_that("duplicate ids are found", {
  m <- sbml_read(model_doc(
    parameters = paste0(par_xml("C", 1), par_xml("k", 2))
  ))

  found <- sbml_check(m)

  expect_equal(found$rule, "duplicate_id")
  expect_equal(found$id, "C")
})

test_that("references that do not resolve are found", {
  m <- sbml_read(model_doc(
    species = sp_xml("A", amount = 1, compartment = "D"),
    parameters = par_xml("p", constant = FALSE),
    rules = rule("assignment", "p", quote(k * 2)),
    reactions = rxn("r", 1, reactants = "B")
  ))

  found <- sbml_check(m)

  expect_true(all(found$rule == "unknown_reference"))
  expect_setequal(found$id, c("D", "B", "k"))
})

test_that("a modifier that is not a species is found", {
  m <- sbml_read(model_doc(
    species = sp_xml("A", amount = 1),
    reactions = sub(
      "<kineticLaw>",
      paste0(
        '<listOfModifiers><modifierSpeciesReference species="M"/>',
        "</listOfModifiers><kineticLaw>"
      ),
      rxn("r", 1, reactants = "A"),
      fixed = TRUE
    )
  ))

  found <- sbml_check(m)

  expect_equal(found$rule, "unknown_reference")
  expect_equal(found$id, "M")
})

test_that("two rules on one variable are found", {
  m <- sbml_read(model_doc(
    parameters = par_xml("p", 1, constant = FALSE),
    rules = paste0(rule("assignment", "p", 3), rule("rate", "p", 1))
  ))

  found <- sbml_check(m)

  expect_equal(found$rule, "multiple_definitions")
  expect_equal(found$id, "p")
})

test_that("local parameters and bound variables are known in their math", {
  m <- sbml_read(model_doc(
    functions = paste0(
      '<functionDefinition id="f">',
      mml(quote(function(x) x + y)),
      "</functionDefinition>"
    ),
    species = sp_xml("A", amount = 1),
    reactions = rxn(
      "r",
      quote(k * A),
      reactants = "A",
      locals = '<localParameter id="k" value="1"/>'
    )
  ))

  found <- sbml_check(m)

  expect_equal(found$id, "y")
})

test_that("symbols set twice are found", {
  m <- sbml_read(model_doc(
    species = sp_xml("A", amount = 1),
    parameters = par_xml("p", 1, constant = FALSE),
    initial = paste0(
      '<initialAssignment symbol="p">',
      mml(2),
      "</initialAssignment>"
    ),
    rules = paste0(rule("assignment", "p", 3), rule("rate", "A", 1)),
    reactions = rxn("r", 1, reactants = "A")
  ))

  found <- sbml_check(m)

  expect_true(all(found$rule == "multiple_definitions"))
  expect_setequal(found$id, c("p", "A"))
})

test_that("rules and events on constants are found", {
  doc <- with_events(
    model_doc(
      parameters = paste0(par_xml("k", 1), par_xml("j", 1)),
      rules = rule("rate", "k", 1)
    ),
    evt("e", quote(time > 1), list(j = 2))
  )

  found <- sbml_check(sbml_read(doc))

  expect_true(all(found$rule == "constant_target"))
  expect_setequal(found$id, c("k", "j"))
})

test_that("assignment cycles are found, and sbml_check() does not raise", {
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

  found <- expect_no_error(sbml_check(m))

  expect_equal(unique(found$rule), "assignment_cycle")
  expect_setequal(found$id, c("a", "b"))
})
