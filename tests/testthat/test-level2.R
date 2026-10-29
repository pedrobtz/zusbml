test_that("Level 2 Versions 4 and 5 read, with Level 2 defaults applied", {
  doc <- function(version) {
    l2_doc(
      paste0(
        '<listOfCompartments><compartment id="C" size="1"/></listOfCompartments>',
        '<listOfSpecies><species id="S" compartment="C" initialAmount="1"/></listOfSpecies>',
        '<listOfParameters><parameter id="k" value="2"/></listOfParameters>',
        '<listOfReactions><reaction id="r"><listOfReactants>',
        '<speciesReference species="S"/></listOfReactants>',
        "<kineticLaw>",
        mml(quote(k * S)),
        "</kineticLaw></reaction></listOfReactions>"
      ),
      version = version
    )
  }

  for (version in 4:5) {
    m <- sbml_read(doc(version))
    info <- paste("Level 2 Version", version)

    expect_equal(c(m$level, m$version), c(2L, version), info = info)
    expect_equal(sbml_compartments(m)$spatial_dimensions, 3, info = info)
    expect_true(sbml_compartments(m)$constant, info = info)
    s <- sbml_species(m)
    expect_false(s$has_only_substance_units, info = info)
    expect_false(s$boundary_condition, info = info)
    expect_false(s$constant, info = info)
    expect_true(sbml_parameters(m)$constant, info = info)
    r <- sbml_reactions(m)
    expect_true(r$reversible, info = info)
    expect_false(r$fast, info = info)
    expect_equal(r$reactants[[1]]$stoichiometry, 1, info = info)
    expect_true(r$reactants[[1]]$constant, info = info)
  }
})

test_that("Level 2 built-in units become the model's units", {
  m <- sbml_read(l2_doc(paste0(
    '<listOfUnitDefinitions><unitDefinition id="substance"><listOfUnits>',
    '<unit kind="mole" scale="-3"/></listOfUnits></unitDefinition></listOfUnitDefinitions>'
  )))

  expect_equal(m$substance_units, "substance")
  expect_equal(m$extent_units, "substance")
  expect_equal(m$time_units, "second")
  expect_equal(m$volume_units, "litre")
  u <- sbml_unit_definitions(m)$units[[1]]
  expect_equal(c(u$exponent, u$scale, u$multiplier), c(1, -3, 1))
})

test_that("a kinetic law's <parameter>s are its local parameters", {
  m <- sbml_read(l2_doc(paste0(
    '<listOfReactions><reaction id="r"><kineticLaw>',
    mml(quote(k)),
    '<listOfParameters><parameter id="k" value="5"/></listOfParameters>',
    "</kineticLaw></reaction></listOfReactions>"
  )))

  expect_equal(sbml_reactions(m)$kinetic_law[[1]]$local_parameters$value, 5)
})

test_that("stoichiometryMath becomes an assignment rule on the reference's id", {
  m <- sbml_read(l2_doc(paste0(
    '<listOfCompartments><compartment id="C" size="1"/></listOfCompartments>',
    '<listOfSpecies><species id="S" compartment="C" initialAmount="1"/></listOfSpecies>',
    '<listOfParameters><parameter id="n" value="2"/></listOfParameters>',
    '<listOfReactions><reaction id="r"><listOfReactants><speciesReference species="S">',
    "<stoichiometryMath>",
    mml(quote(n * 3)),
    "</stoichiometryMath>",
    "</speciesReference></listOfReactants><kineticLaw>",
    mml(1),
    "</kineticLaw></reaction></listOfReactions>"
  )))

  ref <- sbml_reactions(m)$reactants[[1]]
  rules <- sbml_rules(m)

  expect_equal(ref$id, "r_S_stoichiometry")
  expect_false(ref$constant)
  expect_equal(rules$variable, "r_S_stoichiometry")
  expect_identical(rules$math[[1]], quote(n * 3))
  expect_identical(
    sbml_odes(m)$rhs$S,
    call("/", call("-", quote(r_S_stoichiometry * r)), quote(C))
  )
})

test_that("an id made for stoichiometryMath avoids the ids already taken", {
  ref <- function(n) {
    paste0(
      '<speciesReference species="S"><stoichiometryMath>',
      mml(n),
      "</stoichiometryMath></speciesReference>"
    )
  }
  m <- sbml_read(l2_doc(paste0(
    '<listOfCompartments><compartment id="C" size="1"/></listOfCompartments>',
    '<listOfSpecies><species id="S" compartment="C" initialAmount="1"/></listOfSpecies>',
    '<listOfParameters><parameter id="r_S_stoichiometry" value="2"/></listOfParameters>',
    '<listOfReactions><reaction id="r"><listOfReactants>',
    ref(1),
    "</listOfReactants><listOfProducts>",
    ref(3),
    "</listOfProducts><kineticLaw>",
    mml(1),
    "</kineticLaw></reaction></listOfReactions>"
  )))

  r <- sbml_reactions(m)

  expect_equal(r$reactants[[1]]$id, "r_S_stoichiometry_2")
  expect_equal(r$products[[1]]$id, "r_S_stoichiometry_3")
  expect_equal(
    sbml_rules(m)$variable,
    c("r_S_stoichiometry_2", "r_S_stoichiometry_3")
  )
})

test_that("Level 2 triggers fire on change only and always execute", {
  m <- sbml_read(l2_doc(paste0(
    '<listOfParameters><parameter id="p" value="0" constant="false"/></listOfParameters>',
    '<listOfEvents><event id="e"><trigger>',
    mml(quote(time > 1)),
    "</trigger>",
    '<listOfEventAssignments><eventAssignment variable="p">',
    mml(1),
    "</eventAssignment></listOfEventAssignments></event></listOfEvents>"
  )))

  e <- sbml_events(m)

  expect_true(e$trigger[[1]]$initial_value)
  expect_true(e$trigger[[1]]$persistent)
  expect_true(e$use_values_from_trigger_time)
})

test_that("compartment and species types, charge and outside are dropped and noted", {
  m <- sbml_read(l2_doc(paste0(
    '<listOfCompartmentTypes><compartmentType id="ct"/></listOfCompartmentTypes>',
    '<listOfSpeciesTypes><speciesType id="st"/></listOfSpeciesTypes>',
    '<listOfCompartments><compartment id="O" size="1"/>',
    '<compartment id="C" size="1" outside="O" compartmentType="ct"/></listOfCompartments>',
    '<listOfSpecies><species id="S" compartment="C" charge="2" speciesType="st"/></listOfSpecies>'
  )))

  expect_setequal(
    m$dropped,
    c("compartmentType", "speciesType", "outside", "charge")
  )
  expect_snapshot(print(m))
})

test_that("Level 2 models are written as equivalent Level 3 Version 2", {
  m <- sbml_read(l2_doc(paste0(
    '<listOfCompartments><compartment id="C" size="2"/></listOfCompartments>',
    '<listOfSpecies><species id="S" compartment="C" initialConcentration="1"/></listOfSpecies>',
    '<listOfReactions><reaction id="r" reversible="false"><listOfReactants>',
    '<speciesReference species="S"/></listOfReactants><kineticLaw>',
    mml(quote(S)),
    "</kineticLaw></reaction></listOfReactions>"
  )))

  back <- sbml_read(sbml_write(m))

  expect_equal(c(back$level, back$version), c(3L, 2L))
  expect_identical(sbml_odes(back)$rhs, sbml_odes(m)$rhs)
  expect_equal(sbml_initial_state(back), sbml_initial_state(m))
})

test_that("Level 2 fast reactions are refused at derivation", {
  m <- sbml_read(l2_doc(paste0(
    '<listOfCompartments><compartment id="C" size="1"/></listOfCompartments>',
    '<listOfSpecies><species id="S" compartment="C" initialAmount="1"/></listOfSpecies>',
    '<listOfReactions><reaction id="r" fast="true"><listOfReactants>',
    '<speciesReference species="S"/></listOfReactants><kineticLaw>',
    mml(1),
    "</kineticLaw></reaction></listOfReactions>"
  )))

  err <- expect_error(sbml_odes(m), class = "zusbml_unsupported")
  expect_equal(err$id, "r")
})
