# Build a small SBML Level 3 document around a <model> body, so each test
# states exactly the elements it is about.
sbml_doc <- function(
  body = "",
  version = 2,
  model_attrs = "",
  sbml_attrs = ""
) {
  paste0(
    '<?xml version="1.0" encoding="UTF-8"?>',
    '<sbml xmlns="http://www.sbml.org/sbml/level3/version',
    version,
    '/core" level="3" version="',
    version,
    '"',
    sbml_attrs,
    ">",
    "<model",
    model_attrs,
    ">",
    body,
    "</model></sbml>"
  )
}

one_species <- function(attrs = "") {
  paste0(
    '<listOfCompartments><compartment id="C" constant="true"/>',
    "</listOfCompartments><listOfSpecies>",
    '<species id="S" compartment="C" hasOnlySubstanceUnits="false" ',
    'boundaryCondition="false" constant="false"',
    attrs,
    "/>",
    "</listOfSpecies>"
  )
}

reaction_doc <- function(reaction, version = 2) {
  sbml_doc(
    paste0(one_species(), "<listOfReactions>", reaction, "</listOfReactions>"),
    version = version
  )
}

# MathML for an R expression, so derivation tests can state math as R.
mml <- function(expr) {
  r_to_mathml <- utils::getFromNamespace("r_to_mathml", "zusbml")
  r_to_mathml(expr)
}

# A model from its parts, each a string of SBML elements.
model_doc <- function(
  compartments = '<compartment id="C" size="2" constant="true"/>',
  species = "",
  parameters = "",
  rules = "",
  reactions = "",
  initial = "",
  functions = "",
  model_attrs = "",
  version = 2
) {
  wrap <- function(tag, x) {
    if (nzchar(x)) paste0("<", tag, ">", x, "</", tag, ">") else ""
  }
  sbml_doc(
    paste0(
      wrap("listOfFunctionDefinitions", functions),
      wrap("listOfCompartments", compartments),
      wrap("listOfSpecies", species),
      wrap("listOfParameters", parameters),
      wrap("listOfInitialAssignments", initial),
      wrap("listOfRules", rules),
      wrap("listOfReactions", reactions)
    ),
    model_attrs = model_attrs,
    version = version
  )
}

sp_xml <- function(
  id,
  amount = NULL,
  conc = NULL,
  compartment = "C",
  only_substance = FALSE,
  boundary = FALSE,
  constant = FALSE,
  extra = ""
) {
  paste0(
    '<species id="',
    id,
    '" compartment="',
    compartment,
    '"',
    if (!is.null(amount)) paste0(' initialAmount="', amount, '"'),
    if (!is.null(conc)) paste0(' initialConcentration="', conc, '"'),
    ' hasOnlySubstanceUnits="',
    tolower(only_substance),
    '"',
    ' boundaryCondition="',
    tolower(boundary),
    '"',
    ' constant="',
    tolower(constant),
    '"',
    extra,
    "/>"
  )
}

par_xml <- function(id, value = NULL, constant = TRUE) {
  paste0(
    '<parameter id="',
    id,
    '"',
    if (!is.null(value)) paste0(' value="', value, '"'),
    ' constant="',
    tolower(constant),
    '"/>'
  )
}

rxn <- function(
  id,
  law,
  reactants = character(0),
  products = character(0),
  stoich = 1,
  locals = "",
  ref_extra = ""
) {
  refs <- function(x) {
    paste0(
      '<speciesReference species="',
      x,
      '" stoichiometry="',
      stoich,
      '" constant="true"',
      ref_extra,
      "/>",
      collapse = ""
    )
  }
  paste0(
    '<reaction id="',
    id,
    '" reversible="false">',
    if (length(reactants)) {
      paste0("<listOfReactants>", refs(reactants), "</listOfReactants>")
    },
    if (length(products)) {
      paste0("<listOfProducts>", refs(products), "</listOfProducts>")
    },
    "<kineticLaw>",
    mml(law),
    if (nzchar(locals)) {
      paste0("<listOfLocalParameters>", locals, "</listOfLocalParameters>")
    },
    "</kineticLaw></reaction>"
  )
}

rule <- function(type, variable, expr) {
  paste0(
    "<",
    type,
    'Rule variable="',
    variable,
    '">',
    mml(expr),
    "</",
    type,
    "Rule>"
  )
}

odes <- function(...) sbml_odes(sbml_read(model_doc(...)))

# An event element from R expressions.
evt <- function(
  id,
  trigger,
  assignments,
  delay = NULL,
  priority = NULL,
  persistent = TRUE,
  initial = TRUE,
  trigger_time = TRUE
) {
  part <- function(tag, expr) {
    if (is.null(expr)) "" else paste0("<", tag, ">", mml(expr), "</", tag, ">")
  }
  paste0(
    '<event id="',
    id,
    '" useValuesFromTriggerTime="',
    tolower(trigger_time),
    '">',
    '<trigger initialValue="',
    tolower(initial),
    '" persistent="',
    tolower(persistent),
    '">',
    mml(trigger),
    "</trigger>",
    part("priority", priority),
    part("delay", delay),
    "<listOfEventAssignments>",
    paste0(
      '<eventAssignment variable="',
      names(assignments),
      '">',
      vapply(assignments, mml, character(1)),
      "</eventAssignment>",
      collapse = ""
    ),
    "</listOfEventAssignments></event>"
  )
}

with_events <- function(doc, events) {
  sub(
    "</model>",
    paste0("<listOfEvents>", events, "</listOfEvents></model>"),
    doc,
    fixed = TRUE
  )
}

# Run a model with sbml_derivs() and deSolve at the given times.
simulate <- function(doc, times, outputs = character(0)) {
  f <- sbml_derivs(sbml_read(doc), outputs)
  out <- deSolve::ode(
    attr(f, "y0"),
    times,
    f,
    attr(f, "parms"),
    method = "lsodar",
    rtol = 1e-10,
    atol = 1e-12,
    rootfunc = attr(f, "rootfunc"),
    events = attr(f, "events")
  )
  as.data.frame(out)
}

# x grows at rate 1 from 0; events act on it or on p.
growth <- function(events, p = 0) {
  with_events(
    model_doc(
      parameters = paste0(
        par_xml("x", 0, constant = FALSE),
        par_xml("p", p, constant = FALSE)
      ),
      rules = rule("rate", "x", 1)
    ),
    events
  )
}

# A Level 2 document around a <model> body.
l2_doc <- function(body = "", version = 4) {
  paste0(
    '<sbml xmlns="http://www.sbml.org/sbml/level2/version',
    version,
    '" level="2" version="',
    version,
    '"><model id="m">',
    body,
    "</model></sbml>"
  )
}
