test_that("print() shows level, version and non-zero counts", {
  m <- sbml_read(test_path("fixtures", "suite", "00000", "00000-sbml-l3v2.xml"))

  expect_snapshot(print(m))
})

test_that("print() names ignored packages and handles an empty model", {
  doc <- sbml_doc(
    version = 1,
    sbml_attrs = paste0(
      ' xmlns:fbc="http://www.sbml.org/sbml/level3/version1/fbc/version2"',
      ' fbc:required="false"'
    )
  )

  expect_snapshot(print(sbml_read(doc, ignore_packages = TRUE)))
})

test_that("summary() returns the counts", {
  m <- sbml_read(test_path("fixtures", "suite", "00000", "00000-sbml-l3v2.xml"))

  s <- summary(m)

  expect_s3_class(s, "summary.sbml_model")
  expect_equal(s$counts[["species"]], 2L)
  expect_equal(s$counts[["reactions"]], 1L)
  expect_snapshot(print(s))
})

test_that("accessors refuse anything but a model", {
  accessors <- list(
    sbml_compartments,
    sbml_species,
    sbml_parameters,
    sbml_reactions,
    sbml_rules,
    sbml_events,
    sbml_functions,
    sbml_initial_assignments,
    sbml_constraints,
    sbml_unit_definitions
  )

  for (accessor in accessors) {
    expect_error(
      accessor(list()),
      class = "zusbml_arg_error",
      info = deparse(accessor)[[1]]
    )
  }
})

test_that("every table of an empty model has the columns of a full one", {
  full_doc <- with_events(
    model_doc(
      functions = paste0(
        '<functionDefinition id="f">',
        mml(quote(function(x) x)),
        "</functionDefinition>"
      ),
      species = paste0(sp_xml("A", amount = 1), sp_xml("B", amount = 0)),
      parameters = paste0(par_xml("k", 1), par_xml("p", constant = FALSE)),
      initial = paste0(
        '<initialAssignment symbol="k">',
        mml(2),
        "</initialAssignment>"
      ),
      rules = rule("assignment", "p", quote(k * A)),
      reactions = sub(
        "</listOfProducts>",
        paste0(
          '</listOfProducts><listOfModifiers><modifierSpeciesReference species="A"/>',
          "</listOfModifiers>"
        ),
        rxn(
          "r",
          quote(q * A),
          reactants = "A",
          products = "B",
          locals = '<localParameter id="q" value="1"/>'
        ),
        fixed = TRUE
      )
    ),
    evt("e", quote(A < 0.5), list(k = 3), delay = 1, priority = 1)
  )
  full_doc <- sub(
    "<model>",
    paste0(
      '<model><listOfUnitDefinitions><unitDefinition id="per_second">',
      '<listOfUnits><unit kind="second" exponent="-1" scale="0" ',
      'multiplier="1"/></listOfUnits></unitDefinition></listOfUnitDefinitions>'
    ),
    full_doc,
    fixed = TRUE
  )
  full_doc <- sub(
    "</model>",
    paste0(
      "<listOfConstraints><constraint>",
      mml(quote(A >= 0)),
      "</constraint></listOfConstraints></model>"
    ),
    full_doc,
    fixed = TRUE
  )
  full <- sbml_read(full_doc)
  empty <- sbml_read(sbml_doc())
  types <- function(df) vapply(df, typeof, character(1))
  accessors <- list(
    compartments = sbml_compartments,
    species = sbml_species,
    parameters = sbml_parameters,
    reactions = sbml_reactions,
    rules = sbml_rules,
    events = sbml_events,
    functions = sbml_functions,
    initial_assignments = sbml_initial_assignments,
    constraints = sbml_constraints,
    unit_definitions = sbml_unit_definitions
  )

  for (name in names(accessors)) {
    expect_gt(nrow(accessors[[name]](full)), 0)
    expect_equal(nrow(accessors[[name]](empty)), 0, info = name)
    expect_identical(
      types(accessors[[name]](empty)),
      types(accessors[[name]](full)),
      info = name
    )
  }
})

test_that("a reaction without participants has typed, empty tables", {
  full <- sub(
    "</listOfProducts>",
    paste0(
      '</listOfProducts><listOfModifiers><modifierSpeciesReference species="A"/>',
      "</listOfModifiers>"
    ),
    rxn(
      "full",
      quote(q * A),
      reactants = "A",
      products = "A",
      locals = '<localParameter id="q" value="1"/>'
    ),
    fixed = TRUE
  )
  doc <- model_doc(
    species = sp_xml("A", amount = 1),
    parameters = par_xml("k", 1),
    reactions = paste0(full, rxn("bare", quote(k)))
  )
  reactions <- sbml_reactions(sbml_read(doc))
  tables <- list(
    reactants = reactions$reactants,
    products = reactions$products,
    modifiers = reactions$modifiers,
    local_parameters = lapply(reactions$kinetic_law, `[[`, "local_parameters")
  )
  types <- function(df) vapply(df, typeof, character(1))
  species_reference_rows <- utils::getFromNamespace(
    "species_reference_rows",
    "zusbml"
  )

  for (side in names(tables)) {
    expect_equal(nrow(tables[[side]][[1]]), 1, info = side)
    expect_equal(nrow(tables[[side]][[2]]), 0, info = side)
    expect_identical(
      types(tables[[side]][[2]]),
      types(tables[[side]][[1]]),
      info = side
    )
  }

  refs <- species_reference_rows(sbml_read(model_doc(
    parameters = par_xml("k", 1),
    reactions = rxn("bare", quote(k))
  )))
  expect_equal(nrow(refs), 0)
  expect_identical(
    types(refs),
    c(
      reaction = "character",
      side = "character",
      species = "character",
      id = "character",
      stoichiometry = "double",
      constant = "logical"
    )
  )
})

test_that("zusbml_info() states what is read and written", {
  info <- zusbml_info()

  expect_equal(info$read, c("2.4", "2.5", "3.1", "3.2"))
  expect_equal(info$write, "3.2")
  expect_identical(info$packages, character(0))
})

test_that("the bundled example reads", {
  m <- sbml_read(system.file("extdata", "decay.xml", package = "zusbml"))

  expect_equal(m$id, "decay")
})
