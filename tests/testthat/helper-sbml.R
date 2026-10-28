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
