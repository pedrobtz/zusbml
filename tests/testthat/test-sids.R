test_that("ids and references must be SIds", {
  bad <- c(".y", ".amount.S1", "a b", "1x", "", "x-y")

  for (id in bad) {
    doc <- model_doc(
      parameters = paste0('<parameter id="', id, '" constant="true"/>')
    )

    err <- expect_error(sbml_read(doc), class = "zusbml_parse_error", info = id)
    expect_equal(err$attribute, "id", info = id)
    expect_equal(err$section, "3.1.7", info = id)
  }
})

test_that("references to other elements must be SIds too", {
  doc <- model_doc(species = sp("A", amount = 1, compartment = ".y"))

  err <- expect_error(sbml_read(doc), class = "zusbml_parse_error")
  expect_equal(err$attribute, "compartment")
})

test_that("names in math must be SIds", {
  for (name in c(".p", "...", "..1", "  ")) {
    doc <- model_doc(
      parameters = par("p", constant = FALSE),
      rules = paste0(
        '<assignmentRule variable="p"><math xmlns="http://www.w3.org/1998/Math/MathML">',
        "<ci>",
        name,
        "</ci></math></assignmentRule>"
      )
    )

    expect_error(sbml_read(doc), class = "zusbml_parse_error", info = name)
  }
})

test_that("SIds may start with an underscore and use digits", {
  m <- sbml_read(model_doc(parameters = par("_J0_1", 2)))

  expect_equal(sbml_parameters(m)$id, "_J0_1")
})

test_that("metaid is an XML ID, not an SId", {
  m <- sbml_read(model_doc(
    parameters = '<parameter id="p" metaid="meta.p-1" constant="true"/>'
  ))

  expect_equal(sbml_parameters(m)$metaid, "meta.p-1")
})
