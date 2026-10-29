test_that("reading what sbml_write() wrote gives the same model", {
  doc <- with_events(
    model_doc(
      functions = paste0(
        '<functionDefinition id="f">',
        mml(quote(function(x) x^2)),
        "</functionDefinition>"
      ),
      compartments = '<compartment id="C" name="cell &amp; wall" size="0.25" constant="false" metaid="mc" sboTerm="SBO:0000290"/>',
      species = paste0(
        sp_xml("A", conc = 1e-30),
        sp_xml("B", amount = 2, boundary = TRUE)
      ),
      parameters = paste0(
        par_xml("k", 0.1),
        par_xml("inf", "INF"),
        par_xml("nan", "NaN"),
        par_xml("p", constant = FALSE)
      ),
      initial = paste0(
        '<initialAssignment symbol="p">',
        mml(quote(f(k))),
        "</initialAssignment>"
      ),
      rules = rule("rate", "C", quote(if (time > 1) 0 else k)),
      reactions = rxn(
        "r",
        quote(k * A),
        reactants = "A",
        products = "B",
        stoich = 2.5,
        locals = '<localParameter id="k" value="3"/>'
      )
    ),
    evt(
      "e",
      quote(A < 0.5),
      list(p = quote(p + 1)),
      delay = 1,
      priority = 2,
      persistent = FALSE
    )
  )
  doc <- sub(
    "<model>",
    paste0(
      "<model><notes><p xmlns=\"http://www.w3.org/1999/xhtml\">A &lt; B</p></notes>",
      "<annotation><x xmlns=\"urn:x\" a=\"1\"/></annotation>",
      "<listOfUnitDefinitions><unitDefinition id=\"mM\"><listOfUnits>",
      "<unit kind=\"mole\" exponent=\"1\" scale=\"-3\" multiplier=\"1\"/>",
      "</listOfUnits></unitDefinition></listOfUnitDefinitions>"
    ),
    doc,
    fixed = TRUE
  )
  m <- sbml_read(doc)

  expect_identical(sbml_read(sbml_write(m)), m)
})

test_that("sbml_write() writes to a file and returns the path", {
  m <- sbml_read(test_path("fixtures", "suite", "00000", "00000-sbml-l3v2.xml"))
  path <- withr::local_tempfile(fileext = ".xml")

  expect_identical(sbml_write(m, path), path)
  expect_identical(sbml_read(path), m)
})

test_that("Level 3 Version 1 models are written as Version 2", {
  m <- sbml_read(sbml_doc(one_species(), version = 1))

  back <- sbml_read(sbml_write(m))

  expect_equal(c(back$level, back$version), c(3L, 2L))
  expect_equal(sbml_species(back), sbml_species(m))
})

test_that("constraint messages and ids named time survive writing", {
  doc <- model_doc(
    parameters = paste0(par_xml("time", 2), par_xml("q", constant = FALSE)),
    rules = paste0(
      "<assignmentRule variable=\"q\"><math xmlns=\"http://www.w3.org/1998/Math/MathML\">",
      "<apply><plus/><ci>time</ci><csymbol definitionURL=\"http://www.sbml.org/sbml/symbols/time\">t</csymbol></apply>",
      "</math></assignmentRule>"
    )
  )
  doc <- sub(
    "</model>",
    paste0(
      "<listOfConstraints><constraint>",
      mml(TRUE),
      "<message><p xmlns=\"http://www.w3.org/1999/xhtml\">ok</p></message>",
      "</constraint></listOfConstraints></model>"
    ),
    doc,
    fixed = TRUE
  )
  m <- sbml_read(doc)

  expect_identical(sbml_read(sbml_write(m)), m)
  expect_identical(sbml_rules(m)$math[[1]], quote(time + .time))
})

test_that("math with no MathML equivalent is a write error", {
  m <- sbml_read(model_doc(
    parameters = par_xml("p", 1, constant = FALSE),
    rules = rule("assignment", "p", 1)
  ))
  m$rules$math[[1]] <- quote(trunc(2.5))

  err <- expect_error(sbml_write(m), class = "zusbml_write_error")
  expect_equal(err$construct, "trunc")
})

test_that("sbml_write() refuses anything but a model", {
  expect_error(sbml_write(list()), class = "zusbml_arg_error")
})

test_that("tabs and newlines in attribute values survive a round trip", {
  m <- sbml_read(model_doc(
    parameters = '<parameter id="p" name="a&#10;b&#9;c&#13;d" constant="true"/>'
  ))

  expect_equal(sbml_parameters(m)$name, "a\nb\tc\rd")
  expect_identical(sbml_read(sbml_write(m)), m)
})

test_that("quotes, ampersands and angle brackets in names are escaped", {
  m <- sbml_read(model_doc(
    parameters = '<parameter id="p" name="&quot;a&quot; &amp; &lt;b&gt;" constant="true"/>'
  ))

  expect_identical(sbml_read(sbml_write(m)), m)
})

test_that("text that is not well-formed XML is a write error", {
  m <- sbml_read(model_doc(parameters = par_xml("p", 1)))
  m$notes <- "a < b"

  err <- expect_error(sbml_write(m), class = "zusbml_write_error")
  expect_equal(err$construct, "notes")
})

test_that("control characters are a write error", {
  m <- sbml_read(model_doc(parameters = par_xml("p", 1)))
  m$parameters$name <- "a\001b"

  expect_error(sbml_write(m), class = "zusbml_write_error")
})

test_that("NA math is a write error", {
  r_to_mathml <- utils::getFromNamespace("r_to_mathml", "zusbml")

  err <- expect_error(r_to_mathml(NA_real_), class = "zusbml_write_error")
  expect_equal(err$construct, "NA")
})

test_that("ids in notes and annotation do not decide how time is read", {
  doc <- model_doc(
    parameters = par_xml("q", constant = FALSE),
    rules = paste0(
      '<assignmentRule variable="q"><math xmlns="http://www.w3.org/1998/Math/MathML">',
      '<apply><plus/><csymbol definitionURL="http://www.sbml.org/sbml/symbols/time">t</csymbol>',
      "<pi/></apply></math></assignmentRule>"
    )
  )
  doc <- sub(
    "<model>",
    paste0(
      '<model><notes><body xmlns="http://www.w3.org/1999/xhtml">',
      '<p id="time">x</p><p id="pi">y</p></body></notes>',
      '<annotation><x:a xmlns:x="urn:x" id="time"/></annotation>'
    ),
    doc,
    fixed = TRUE
  )
  m <- sbml_read(doc)

  expect_identical(sbml_rules(m)$math[[1]], quote(time + pi))
  expect_identical(sbml_read(sbml_write(m)), m)
})

test_that("a fast reaction is a write error", {
  doc <- model_doc(
    species = sp_xml("A", amount = 1),
    reactions = sub(
      'reversible="false"',
      'reversible="false" fast="true"',
      rxn("r", 1, reactants = "A"),
      fixed = TRUE
    ),
    version = 1
  )
  m <- sbml_read(doc)

  err <- expect_error(sbml_write(m), class = "zusbml_write_error")
  expect_equal(err$construct, "fast reaction")

  m <- sbml_read(sub('fast="true"', 'fast="false"', doc, fixed = TRUE))
  expect_type(sbml_write(m), "character")
})
