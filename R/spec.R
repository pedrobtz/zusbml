# The SBML Level 3 core elements this package reads, one entry per element:
# its attributes (type and whether required) and the children it may hold.
# The reader walks documents against this table, so an element or attribute
# missing from it is a parse error rather than silently dropped (design
# sections 4.1 and 6.1). Sections cite the Level 3 Version 2 Core
# specification, Release 2.

sbml_ns_core <- c(
  "3.1" = "http://www.sbml.org/sbml/level3/version1/core",
  "3.2" = "http://www.sbml.org/sbml/level3/version2/core"
)
sbml_ns_other <- c(
  "1" = "http://www.sbml.org/sbml/level1",
  "2.1" = "http://www.sbml.org/sbml/level2",
  "2.2" = "http://www.sbml.org/sbml/level2/version2",
  "2.3" = "http://www.sbml.org/sbml/level2/version3",
  "2.4" = "http://www.sbml.org/sbml/level2/version4",
  "2.5" = "http://www.sbml.org/sbml/level2/version5"
)
mathml_ns <- "http://www.w3.org/1998/Math/MathML"

# Attributes every SBase may carry in Level 3.
sbase_attributes <- list(
  id = list("string", FALSE),
  name = list("string", FALSE),
  metaid = list("string", FALSE),
  sboTerm = list("string", FALSE)
)

# SBase attributes plus the element's own; an element's entry overrides the
# SBase one, as `id` does where an element requires it.
attr_spec <- function(...) {
  own <- list(...)
  c(sbase_attributes[setdiff(names(sbase_attributes), names(own))], own)
}

req <- function(type) list(type, TRUE)
opt <- function(type, default = NULL) list(type, FALSE, default)

sbml_elements <- list(
  sbml = list(
    section = "4.1",
    attributes = list(
      level = req("integer"),
      version = req("integer"),
      metaid = opt("string"),
      sboTerm = opt("string")
    ),
    children = "model"
  ),
  model = list(
    section = "4.2",
    attributes = attr_spec(
      substanceUnits = opt("string"),
      timeUnits = opt("string"),
      volumeUnits = opt("string"),
      areaUnits = opt("string"),
      lengthUnits = opt("string"),
      extentUnits = opt("string"),
      conversionFactor = opt("string")
    ),
    children = c(
      "listOfFunctionDefinitions",
      "listOfUnitDefinitions",
      "listOfCompartments",
      "listOfSpecies",
      "listOfParameters",
      "listOfInitialAssignments",
      "listOfRules",
      "listOfConstraints",
      "listOfReactions",
      "listOfEvents"
    )
  ),
  functionDefinition = list(
    section = "4.3",
    attributes = attr_spec(id = req("string")),
    children = "math"
  ),
  unitDefinition = list(
    section = "4.4",
    attributes = attr_spec(id = req("string")),
    children = "listOfUnits"
  ),
  unit = list(
    section = "4.4.2",
    attributes = attr_spec(
      kind = req("string"),
      exponent = req("double"),
      scale = req("integer"),
      multiplier = req("double")
    ),
    children = character(0)
  ),
  compartment = list(
    section = "4.5",
    attributes = attr_spec(
      id = req("string"),
      spatialDimensions = opt("double"),
      size = opt("double"),
      units = opt("string"),
      constant = req("boolean")
    ),
    children = character(0)
  ),
  species = list(
    section = "4.6",
    attributes = attr_spec(
      id = req("string"),
      compartment = req("string"),
      initialAmount = opt("double"),
      initialConcentration = opt("double"),
      substanceUnits = opt("string"),
      hasOnlySubstanceUnits = req("boolean"),
      boundaryCondition = req("boolean"),
      constant = req("boolean"),
      conversionFactor = opt("string")
    ),
    children = character(0)
  ),
  parameter = list(
    section = "4.7",
    attributes = attr_spec(
      id = req("string"),
      value = opt("double"),
      units = opt("string"),
      constant = req("boolean")
    ),
    children = character(0)
  ),
  initialAssignment = list(
    section = "4.8",
    attributes = attr_spec(symbol = req("string")),
    children = "math"
  ),
  algebraicRule = list(
    section = "4.9.2",
    attributes = attr_spec(),
    children = "math"
  ),
  assignmentRule = list(
    section = "4.9.3",
    attributes = attr_spec(variable = req("string")),
    children = "math"
  ),
  rateRule = list(
    section = "4.9.4",
    attributes = attr_spec(variable = req("string")),
    children = "math"
  ),
  constraint = list(
    section = "4.10",
    attributes = attr_spec(),
    children = c("math", "message")
  ),
  reaction = list(
    section = "4.11",
    attributes = attr_spec(
      id = req("string"),
      reversible = req("boolean"),
      compartment = opt("string")
    ),
    children = c(
      "listOfReactants",
      "listOfProducts",
      "listOfModifiers",
      "kineticLaw"
    )
  ),
  speciesReference = list(
    section = "4.11.3",
    attributes = attr_spec(
      species = req("string"),
      stoichiometry = opt("double"),
      constant = req("boolean")
    ),
    children = character(0)
  ),
  modifierSpeciesReference = list(
    section = "4.11.4",
    attributes = attr_spec(species = req("string")),
    children = character(0)
  ),
  kineticLaw = list(
    section = "4.11.5",
    attributes = attr_spec(),
    children = c("math", "listOfLocalParameters")
  ),
  localParameter = list(
    section = "4.11.5.2",
    attributes = attr_spec(
      id = req("string"),
      value = opt("double"),
      units = opt("string")
    ),
    children = character(0)
  ),
  event = list(
    section = "4.12",
    attributes = attr_spec(useValuesFromTriggerTime = req("boolean")),
    children = c("trigger", "priority", "delay", "listOfEventAssignments")
  ),
  trigger = list(
    section = "4.12.2",
    attributes = attr_spec(
      initialValue = req("boolean"),
      persistent = req("boolean")
    ),
    children = "math"
  ),
  priority = list(
    section = "4.12.3",
    attributes = attr_spec(),
    children = "math"
  ),
  delay = list(
    section = "4.12.4",
    attributes = attr_spec(),
    children = "math"
  ),
  eventAssignment = list(
    section = "4.12.5",
    attributes = attr_spec(variable = req("string")),
    children = "math"
  )
)

# listOf containers and the element kinds each may hold.
sbml_lists <- list(
  listOfFunctionDefinitions = "functionDefinition",
  listOfUnitDefinitions = "unitDefinition",
  listOfUnits = "unit",
  listOfCompartments = "compartment",
  listOfSpecies = "species",
  listOfParameters = "parameter",
  listOfInitialAssignments = "initialAssignment",
  listOfRules = c("algebraicRule", "assignmentRule", "rateRule"),
  listOfConstraints = "constraint",
  listOfReactions = "reaction",
  listOfReactants = "speciesReference",
  listOfProducts = "speciesReference",
  listOfModifiers = "modifierSpeciesReference",
  listOfLocalParameters = "localParameter",
  listOfEvents = "event",
  listOfEventAssignments = "eventAssignment"
)

# Level 3 Version 1 differs in one place the reader cares about: reaction's
# required `fast` attribute, removed in Version 2.
element_spec <- function(kind, version, level = 3L) {
  if (level == 2L) {
    return(level2_spec(kind))
  }
  spec <- sbml_elements[[kind]]
  if (kind == "reaction" && version == 1L) {
    spec$attributes$fast <- req("boolean")
  }
  spec
}

# ---- Level 2 Versions 4 and 5 (design section 6.4) -------------------------

# Level 2 differs from Level 3 in its defaults, in a few attributes Level 3
# removed, which are read and dropped (`level2_dropped`), and in a few
# elements: compartment and species types (dropped), stoichiometryMath (an
# assignment rule in Level 3), a kinetic law's local parameters (written as
# <parameter>), and triggers without initialValue or persistent.
level2_dropped <- c("compartmentType", "speciesType", "charge", "outside")

level2_overrides <- list(
  sbml = list(children = "model"),
  model = list(
    attributes = sbase_attributes,
    children = c(
      "listOfFunctionDefinitions",
      "listOfUnitDefinitions",
      "listOfCompartmentTypes",
      "listOfSpeciesTypes",
      "listOfCompartments",
      "listOfSpecies",
      "listOfParameters",
      "listOfInitialAssignments",
      "listOfRules",
      "listOfConstraints",
      "listOfReactions",
      "listOfEvents"
    )
  ),
  unit = list(
    attributes = attr_spec(
      kind = req("string"),
      exponent = opt("double", 1),
      scale = opt("integer", 0L),
      multiplier = opt("double", 1)
    )
  ),
  compartmentType = list(
    section = "L2V4 4.5",
    attributes = attr_spec(),
    children = character(0)
  ),
  speciesType = list(
    section = "L2V4 4.6",
    attributes = attr_spec(),
    children = character(0)
  ),
  compartment = list(
    attributes = attr_spec(
      id = req("string"),
      spatialDimensions = opt("double", 3),
      size = opt("double"),
      units = opt("string"),
      constant = opt("boolean", TRUE),
      compartmentType = opt("string"),
      outside = opt("string")
    )
  ),
  species = list(
    attributes = attr_spec(
      id = req("string"),
      compartment = req("string"),
      initialAmount = opt("double"),
      initialConcentration = opt("double"),
      substanceUnits = opt("string"),
      hasOnlySubstanceUnits = opt("boolean", FALSE),
      boundaryCondition = opt("boolean", FALSE),
      constant = opt("boolean", FALSE),
      speciesType = opt("string"),
      charge = opt("integer")
    )
  ),
  parameter = list(
    attributes = attr_spec(
      id = req("string"),
      value = opt("double"),
      units = opt("string"),
      constant = opt("boolean", TRUE)
    )
  ),
  reaction = list(
    attributes = attr_spec(
      id = req("string"),
      reversible = opt("boolean", TRUE),
      fast = opt("boolean", FALSE)
    )
  ),
  speciesReference = list(
    attributes = attr_spec(
      species = req("string"),
      stoichiometry = opt("double")
    ),
    children = "stoichiometryMath"
  ),
  stoichiometryMath = list(
    section = "L2V4 4.13.3",
    attributes = attr_spec(),
    children = "math"
  ),
  kineticLaw = list(children = c("math", "listOfParameters")),
  event = list(
    attributes = attr_spec(
      useValuesFromTriggerTime = opt("boolean", TRUE)
    ),
    children = c("trigger", "delay", "listOfEventAssignments")
  ),
  trigger = list(attributes = attr_spec())
)

level2_spec <- function(kind) {
  spec <- sbml_elements[[kind]] %||% list(section = NA_character_)
  override <- level2_overrides[[kind]]
  for (field in names(override)) {
    spec[[field]] <- override[[field]]
  }
  spec
}

level2_lists <- list(
  listOfCompartmentTypes = "compartmentType",
  listOfSpeciesTypes = "speciesType",
  listOfParameters = "parameter"
)
