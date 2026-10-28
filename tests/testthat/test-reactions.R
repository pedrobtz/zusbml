test_that("reactions read species references, modifiers and the kinetic law", {
  m <- sbml_read(reaction_doc(paste0(
    '<reaction id="r" name="decay" reversible="true" compartment="C">',
    '<listOfReactants><speciesReference id="sr" species="S" stoichiometry="2" constant="true"/></listOfReactants>',
    '<listOfProducts><speciesReference species="S" constant="false"/></listOfProducts>',
    '<listOfModifiers><modifierSpeciesReference species="S"/></listOfModifiers>',
    '<kineticLaw metaid="kl"><math xmlns="http://www.w3.org/1998/Math/MathML"><ci>k</ci></math>',
    '<listOfLocalParameters><localParameter id="k" value="3"/></listOfLocalParameters>',
    "</kineticLaw></reaction>"
  )))

  r <- sbml_reactions(m)
  law <- r$kinetic_law[[1]]

  expect_equal(r$id, "r")
  expect_equal(r$name, "decay")
  expect_true(r$reversible)
  expect_true(is.na(r$fast))
  expect_equal(r$compartment, "C")
  expect_equal(r$reactants[[1]]$id, "sr")
  expect_equal(r$reactants[[1]]$stoichiometry, 2)
  expect_true(r$reactants[[1]]$constant)
  expect_true(is.na(r$products[[1]]$stoichiometry))
  expect_false(r$products[[1]]$constant)
  expect_equal(r$modifiers[[1]]$species, "S")
  expect_equal(law$local_parameters$id, "k")
  expect_equal(law$local_parameters$value, 3)
  expect_equal(law$metaid, "kl")
  expect_match(law$math, "<ci>k</ci>", fixed = TRUE)
})

test_that("a reaction without a kinetic law has NULL there", {
  r <- sbml_reactions(sbml_read(reaction_doc(
    '<reaction id="r" reversible="false"/>'
  )))

  expect_null(r$kinetic_law[[1]])
  expect_equal(nrow(r$reactants[[1]]), 0)
})

test_that("Level 3 Version 1 reactions require fast", {
  without <- reaction_doc('<reaction id="r" reversible="false"/>', version = 1)
  with <- reaction_doc(
    '<reaction id="r" reversible="false" fast="false"/>',
    version = 1
  )

  err <- expect_error(sbml_read(without), class = "zusbml_parse_error")
  expect_equal(err$attribute, "fast")
  expect_false(sbml_reactions(sbml_read(with))$fast)
})

test_that("Level 3 Version 2 reactions have no fast attribute", {
  doc <- reaction_doc('<reaction id="r" reversible="false" fast="false"/>')

  err <- expect_error(sbml_read(doc), class = "zusbml_parse_error")
  expect_equal(err$attribute, "fast")
})

test_that("a species reference needs constant", {
  doc <- reaction_doc(paste0(
    '<reaction id="r" reversible="false"><listOfReactants>',
    '<speciesReference species="S"/></listOfReactants></reaction>'
  ))

  err <- expect_error(sbml_read(doc), class = "zusbml_parse_error")
  expect_equal(
    err$path,
    "sbml/model/listOfReactions/reaction[1]/listOfReactants/speciesReference[1]"
  )
})

test_that("math outside the MathML namespace is a parse error", {
  doc <- reaction_doc(
    '<reaction id="r" reversible="false"><kineticLaw><math/></kineticLaw></reaction>'
  )

  err <- expect_error(sbml_read(doc), class = "zusbml_parse_error")
  expect_equal(err$section, "3.4")
})
