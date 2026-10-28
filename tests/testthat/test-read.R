test_that("sbml_read() reads a path, XML text, a raw vector and a file URL", {
  path <- test_path("fixtures", "suite", "00000", "00000-sbml-l3v2.xml")
  text <- paste(readLines(path), collapse = "\n")

  from_path <- sbml_read(path)

  expect_s3_class(from_path, "sbml_model")
  expect_identical(sbml_read(text), from_path)
  expect_identical(sbml_read(charToRaw(text)), from_path)
  expect_identical(
    sbml_read(paste0("file://", normalizePath(path))),
    from_path
  )
})

test_that("sbml_read() reads Level 3 Versions 1 and 2", {
  v1 <- sbml_read(sbml_doc(version = 1))
  v2 <- sbml_read(sbml_doc(version = 2))

  expect_equal(c(v1$level, v1$version), c(3L, 1L))
  expect_equal(c(v2$level, v2$version), c(3L, 2L))
})

test_that("sbml_read() reads the model's attributes", {
  m <- sbml_read(sbml_doc(
    model_attrs = paste(
      ' id="m" name="My model" substanceUnits="mole" timeUnits="second"',
      'volumeUnits="litre" extentUnits="mole" conversionFactor="cf"'
    )
  ))

  expect_equal(m$id, "m")
  expect_equal(m$name, "My model")
  expect_equal(m$substance_units, "mole")
  expect_equal(m$time_units, "second")
  expect_equal(m$volume_units, "litre")
  expect_equal(m$extent_units, "mole")
  expect_equal(m$conversion_factor, "cf")
  expect_true(is.na(m$area_units))
})

test_that("sbml_read() refuses Level 1 and Level 2 before Version 4", {
  l2 <- '<sbml xmlns="http://www.sbml.org/sbml/level2/version3" level="2" version="3"><model/></sbml>'
  l1 <- '<sbml xmlns="http://www.sbml.org/sbml/level1" level="1" version="2"><model/></sbml>'

  expect_error(sbml_read(l2), class = "zusbml_unsupported")
  expect_error(sbml_read(l1), class = "zusbml_unsupported")
})

test_that("sbml_read() reports malformed XML as a parse error", {
  expect_error(sbml_read("<sbml><model></sbml>"), class = "zusbml_parse_error")
})

test_that("sbml_read() rejects a root that is not SBML", {
  err <- expect_error(
    sbml_read('<sbml xmlns="urn:other" level="3" version="2"/>'),
    class = "zusbml_parse_error"
  )
  expect_equal(err$section, "4.1")
})

test_that("sbml_read() requires a model", {
  doc <- '<sbml xmlns="http://www.sbml.org/sbml/level3/version2/core" level="3" version="2"/>'

  expect_error(sbml_read(doc), class = "zusbml_parse_error")
})

test_that("sbml_read() requires level and version to match the namespace", {
  doc <- '<sbml xmlns="http://www.sbml.org/sbml/level3/version2/core" level="3" version="1"><model/></sbml>'

  err <- expect_error(sbml_read(doc), class = "zusbml_parse_error")
  expect_equal(err$attribute, "level")
})

test_that("sbml_read() reports an unknown element with its path", {
  doc <- sbml_doc("<listOfSpecies><specie/></listOfSpecies>")

  err <- expect_error(sbml_read(doc), class = "zusbml_parse_error")
  expect_equal(err$path, "sbml/model/listOfSpecies/specie[1]")
})

test_that("sbml_read() reports an unknown child element", {
  err <- expect_error(
    sbml_read(sbml_doc("<listOfThings/>")),
    class = "zusbml_parse_error"
  )
  expect_equal(err$path, "sbml/model/listOfThings")
})

test_that("sbml_read() reports an unknown attribute", {
  err <- expect_error(
    sbml_read(sbml_doc(one_species(' colour="red"'))),
    class = "zusbml_parse_error"
  )
  expect_equal(err$attribute, "colour")
  expect_equal(err$path, "sbml/model/listOfSpecies/species[1]")
})

test_that("sbml_read() reports a missing required attribute", {
  doc <- sbml_doc(
    '<listOfParameters><parameter id="p"/></listOfParameters>'
  )

  err <- expect_error(sbml_read(doc), class = "zusbml_parse_error")
  expect_equal(err$attribute, "constant")
  expect_equal(err$section, "4.7")
})

test_that("sbml_read() reports a value of the wrong type", {
  bad_bool <- sbml_doc(
    '<listOfParameters><parameter id="p" constant="yes"/></listOfParameters>'
  )
  bad_double <- sbml_doc(
    '<listOfParameters><parameter id="p" value="1,5" constant="true"/></listOfParameters>'
  )

  expect_error(sbml_read(bad_bool), class = "zusbml_parse_error")
  err <- expect_error(sbml_read(bad_double), class = "zusbml_parse_error")
  expect_equal(err$attribute, "value")
})

test_that("sbml_read() reports a repeated single child", {
  doc <- sbml_doc(paste0(
    '<listOfReactions><reaction id="r" reversible="false">',
    "<kineticLaw/><kineticLaw/></reaction></listOfReactions>"
  ))

  err <- expect_error(sbml_read(doc), class = "zusbml_parse_error")
  expect_equal(err$path, "sbml/model/listOfReactions/reaction[1]/kineticLaw")
})

test_that("sbml_read() numbers repeated elements in paths", {
  doc <- sbml_doc(paste0(
    '<listOfParameters><parameter id="a" constant="true"/>',
    '<parameter id="b"/></listOfParameters>'
  ))

  err <- expect_error(sbml_read(doc), class = "zusbml_parse_error")
  expect_equal(err$path, "sbml/model/listOfParameters/parameter[2]")
})

test_that("sbml_read() records the file in parse errors", {
  path <- withr::local_tempfile(fileext = ".xml")
  writeLines(sbml_doc("<listOfThings/>"), path)

  err <- expect_error(sbml_read(path), class = "zusbml_parse_error")
  expect_equal(err$file, path)
})

test_that("sbml_read() refuses SBML packages by name", {
  doc <- sbml_doc(
    version = 1,
    sbml_attrs = paste0(
      ' xmlns:fbc="http://www.sbml.org/sbml/level3/version1/fbc/version2"',
      ' xmlns:comp="http://www.sbml.org/sbml/level3/version1/comp/version1"',
      ' fbc:required="false" comp:required="true"'
    )
  )

  err <- expect_error(sbml_read(doc), class = "zusbml_unsupported")
  expect_setequal(err$package, c("fbc", "comp"))
  expect_equal(err$construct, "SBML package")
})

test_that("sbml_read() refuses package elements and attributes found inside", {
  fbc <- ' xmlns:fbc="http://www.sbml.org/sbml/level3/version1/fbc/version2"'
  element <- sbml_doc("<fbc:listOfObjectives/>", version = 1, sbml_attrs = fbc)
  attribute <- sbml_doc(
    one_species(' fbc:charge="1"'),
    version = 1,
    sbml_attrs = fbc
  )

  expect_error(sbml_read(element), class = "zusbml_unsupported")
  err <- expect_error(sbml_read(attribute), class = "zusbml_unsupported")
  expect_equal(err$package, "fbc")
})

test_that("ignore_packages = TRUE reads the core and records what was dropped", {
  doc <- sbml_doc(
    paste0(one_species(' fbc:charge="1"'), "<fbc:listOfObjectives/>"),
    version = 1,
    sbml_attrs = paste0(
      ' xmlns:fbc="http://www.sbml.org/sbml/level3/version1/fbc/version2"',
      ' fbc:required="false"'
    )
  )

  m <- sbml_read(doc, ignore_packages = TRUE)

  expect_equal(m$ignored_packages, "fbc")
  expect_equal(sbml_species(m)$id, "S")
})

test_that("sbml_read() checks its arguments", {
  expect_error(sbml_read(1))
  expect_error(sbml_read(sbml_doc(), ignore_packages = NA))
  expect_error(sbml_read(file.path(withr::local_tempdir(), "missing.xml")))
})

test_that("sbml_read() keeps notes and annotation as XML text", {
  doc <- sbml_doc(
    paste0(
      '<notes><p xmlns="http://www.w3.org/1999/xhtml">Hi</p></notes>',
      '<annotation><x xmlns="urn:x"/></annotation>',
      one_species()
    )
  )

  m <- sbml_read(doc)

  expect_match(
    m$notes,
    "<p xmlns=\"http://www.w3.org/1999/xhtml\">Hi</p>",
    fixed = TRUE
  )
  expect_match(m$annotation, "<x xmlns=\"urn:x\"/>", fixed = TRUE)
  expect_true(is.na(sbml_species(m)$notes))
})

test_that("reading the same document twice gives identical objects", {
  doc <- sbml_doc(one_species(' metaid="s1" sboTerm="SBO:0000247"'))

  expect_identical(sbml_read(doc), sbml_read(doc))
})

test_that("a directory is refused before reading", {
  err <- expect_error(
    sbml_read(withr::local_tempdir()),
    class = "zusbml_arg_error"
  )
  expect_equal(err$argument, "x")
})
