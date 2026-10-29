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
  doc <- model_doc(species = sp_xml("A", amount = 1, compartment = ".y"))

  err <- expect_error(sbml_read(doc), class = "zusbml_parse_error")
  expect_equal(err$attribute, "compartment")
})

test_that("names in math must be SIds", {
  for (name in c(".p", "...", "..1", "  ")) {
    doc <- model_doc(
      parameters = par_xml("p", constant = FALSE),
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
  m <- sbml_read(model_doc(parameters = par_xml("_J0_1", 2)))

  expect_equal(sbml_parameters(m)$id, "_J0_1")
})

test_that("metaid is an XML ID, not an SId", {
  m <- sbml_read(model_doc(
    parameters = '<parameter id="p" metaid="meta.p-1" constant="true"/>'
  ))

  expect_equal(sbml_parameters(m)$metaid, "meta.p-1")
})

test_that("an SId with a newline is refused", {
  for (id in c("k1&#10;", "k&#10;1", "&#10;k1")) {
    doc <- model_doc(
      parameters = paste0('<parameter id="', id, '" constant="true"/>')
    )

    err <- expect_error(sbml_read(doc), class = "zusbml_parse_error", info = id)
    expect_equal(err$attribute, "id", info = id)
  }
})

test_that("every SId-valued attribute is checked", {
  doc <- function(bad) {
    v <- function(attr, value) {
      paste0(" ", attr, '="', if (attr == bad) ".x" else value, '"')
    }
    model_doc(
      model_attrs = paste0(
        v("substanceUnits", "mole"),
        v("timeUnits", "second"),
        v("volumeUnits", "litre"),
        v("areaUnits", "metre"),
        v("lengthUnits", "metre"),
        v("extentUnits", "mole"),
        v("conversionFactor", "cf")
      ),
      compartments = paste0("<compartment", v("id", "C"), ' constant="true"/>'),
      species = paste0(
        '<species id="S"',
        v("compartment", "C"),
        ' hasOnlySubstanceUnits="false" boundaryCondition="false"',
        ' constant="false"/>'
      ),
      parameters = paste0(
        '<parameter id="cf" value="1" constant="true"',
        v("units", "dimensionless"),
        "/>",
        par_xml("p", constant = FALSE)
      ),
      initial = paste0(
        "<initialAssignment",
        v("symbol", "S"),
        ">",
        mml(1),
        "</initialAssignment>"
      ),
      rules = paste0(
        "<assignmentRule",
        v("variable", "p"),
        ">",
        mml(1),
        "</assignmentRule>"
      ),
      reactions = paste0(
        '<reaction id="r" reversible="false"><listOfReactants>',
        "<speciesReference",
        v("species", "S"),
        ' constant="true"/></listOfReactants></reaction>'
      )
    )
  }
  expect_s3_class(sbml_read(doc("none")), "sbml_model")
  l3 <- c(
    "id",
    "variable",
    "symbol",
    "species",
    "compartment",
    "conversionFactor",
    "units",
    "substanceUnits",
    "timeUnits",
    "volumeUnits",
    "areaUnits",
    "lengthUnits",
    "extentUnits"
  )
  for (attr in l3) {
    err <- expect_error(
      sbml_read(doc(attr)),
      class = "zusbml_parse_error",
      info = attr
    )
    expect_equal(err$attribute, attr, info = attr)
    expect_equal(err$section, "3.1.7", info = attr)
  }
  for (attr in c("outside", "compartmentType", "speciesType")) {
    doc <- l2_doc(paste0(
      '<listOfCompartments><compartment id="C" ',
      attr,
      '=".x"/></listOfCompartments>'
    ))
    err <- expect_error(
      sbml_read(doc),
      class = "zusbml_parse_error",
      info = attr
    )
    expect_equal(err$attribute, attr, info = attr)
  }
})

test_that("the first invalid SId in document order is reported", {
  doc <- model_doc(
    parameters = paste0(par_xml("ok"), par_xml(".b"), par_xml(".a"))
  )

  err <- expect_error(sbml_read(doc), class = "zusbml_parse_error")
  expect_equal(err$path, "sbml/model/listOfParameters/parameter[2]")
})
