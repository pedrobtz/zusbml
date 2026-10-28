test_that("an amount species changes by stoichiometry times rate", {
  eq <- odes(
    species = paste0(
      sp("A", amount = 5, only_substance = TRUE),
      sp("B", amount = 0, only_substance = TRUE)
    ),
    parameters = par("k", 0.5),
    reactions = rxn(
      "r",
      quote(k * A),
      reactants = "A",
      products = "B",
      stoich = 2
    )
  )

  expect_equal(eq$states$name, c("A", "B"))
  expect_identical(eq$rhs$A, call("-", quote(2 * r)))
  expect_identical(eq$rhs$B, quote(2 * r))
  expect_identical(eq$assignments$r, quote(k * A))
  expect_equal(eq$initial, c(A = 5, B = 0))
  expect_equal(eq$constants, c(C = 2, k = 0.5))
})

test_that("a concentration species divides by its compartment", {
  eq <- odes(
    species = sp("A", conc = 3),
    reactions = rxn("r", 1, reactants = "A")
  )

  expect_identical(eq$rhs$A, quote(-r / C))
  expect_equal(eq$initial, c(A = 3))
})

test_that("initialAmount of a concentration species is divided by the size", {
  eq <- odes(species = sp("A", amount = 4))

  expect_equal(eq$initial, c(A = 2))
})

test_that("a species in a compartment that changes size is integrated as an amount", {
  eq <- odes(
    compartments = '<compartment id="C" size="2" constant="false"/>',
    species = sp("A", conc = 3),
    rules = rule("rate", "C", 1),
    reactions = rxn("r", 1, reactants = "A")
  )

  expect_setequal(eq$states$name, c("C", ".amount.A"))
  expect_identical(eq$rhs[[".amount.A"]], quote(-r))
  expect_identical(eq$assignments$A, quote(.amount.A / C))
  expect_equal(eq$initial[[".amount.A"]], 6)
})

test_that("a constant species in a growing compartment keeps its amount", {
  eq <- odes(
    compartments = '<compartment id="C" size="2" constant="false"/>',
    species = sp("A", conc = 3, constant = TRUE),
    rules = rule("rate", "C", 1)
  )

  expect_equal(eq$constants[[".amount.A"]], 6)
  expect_identical(eq$assignments$A, quote(.amount.A / C))
})

test_that("boundary species are not changed by reactions", {
  eq <- odes(
    species = sp("A", amount = 1, boundary = TRUE),
    reactions = rxn("r", 1, reactants = "A")
  )

  expect_identical(eq$rhs$A, 0)
})

test_that("constant species and parameters are constants", {
  eq <- odes(
    species = sp("A", conc = 1, constant = TRUE),
    parameters = par("k", 2)
  )

  expect_equal(eq$constants, c(C = 2, A = 1, k = 2))
  expect_equal(nrow(eq$states), 0)
})

test_that("conversion factors multiply the change, species before model", {
  eq <- odes(
    species = paste0(
      sp(
        "A",
        amount = 1,
        only_substance = TRUE,
        extra = ' conversionFactor="fa"'
      ),
      sp("B", amount = 0, only_substance = TRUE)
    ),
    parameters = paste0(par("fa", 2), par("fm", 3)),
    reactions = rxn("r", 1, reactants = "A", products = "B"),
    model_attrs = ' conversionFactor="fm"'
  )

  expect_identical(eq$rhs$A, quote(-r * fa))
  expect_identical(eq$rhs$B, quote(r * fm))
})

test_that("a species reference id stands for its stoichiometry", {
  doc <- model_doc(
    species = sp("A", amount = 1, only_substance = TRUE),
    parameters = par("n", 3, constant = FALSE),
    rules = rule("assignment", "sA", quote(n * 2)),
    reactions = paste0(
      '<reaction id="r" reversible="false"><listOfReactants>',
      '<speciesReference id="sA" species="A" constant="false"/>',
      "</listOfReactants><kineticLaw>",
      mml(1),
      "</kineticLaw></reaction>"
    )
  )

  eq <- sbml_odes(sbml_read(doc))

  expect_identical(eq$rhs$A, call("-", quote(sA * r)))
  expect_identical(eq$assignments$sA, quote(n * 2))
})

test_that("a species reference without stoichiometry is a derive error", {
  doc <- model_doc(
    species = sp("A", amount = 1),
    reactions = paste0(
      '<reaction id="r" reversible="false"><listOfReactants>',
      '<speciesReference species="A" constant="true"/>',
      "</listOfReactants><kineticLaw>",
      mml(1),
      "</kineticLaw></reaction>"
    )
  )

  err <- expect_error(sbml_odes(sbml_read(doc)), class = "zusbml_derive_error")
  expect_equal(err$ids, c("r", "A"))
})

test_that("assignment rules are ordered by dependency", {
  eq <- odes(
    parameters = paste0(
      par("a", constant = FALSE),
      par("b", constant = FALSE),
      par("k", 2)
    ),
    rules = paste0(
      rule("assignment", "a", quote(b + 1)),
      rule("assignment", "b", quote(k * time))
    )
  )

  expect_equal(names(eq$assignments), c("b", "a"))
})

test_that("an assignment cycle is a derive error naming its ids", {
  doc <- model_doc(
    parameters = paste0(par("a", constant = FALSE), par("b", constant = FALSE)),
    rules = paste0(
      rule("assignment", "a", quote(b)),
      rule("assignment", "b", quote(a))
    )
  )

  err <- expect_error(sbml_odes(sbml_read(doc)), class = "zusbml_derive_error")
  expect_setequal(err$ids, c("a", "b"))
})

test_that("local parameters are inlined under unique names", {
  eq <- odes(
    species = sp("A", amount = 1, only_substance = TRUE),
    parameters = par("k", 99),
    reactions = rxn(
      "r",
      quote(k * A),
      reactants = "A",
      locals = '<localParameter id="k" value="0.5"/>'
    )
  )

  expect_identical(eq$assignments$r, quote(.local.r.k * A))
  expect_equal(eq$constants[[".local.r.k"]], 0.5)
  expect_equal(eq$constants[["k"]], 99)
})

test_that("function definitions are inlined", {
  eq <- odes(
    functions = paste0(
      '<functionDefinition id="f">',
      mml(quote(function(x, y) x * y + 1)),
      "</functionDefinition>"
    ),
    parameters = paste0(par("p", constant = FALSE), par("k", 3)),
    rules = rule("assignment", "p", quote(f(k, time)))
  )

  expect_identical(eq$assignments$p, quote(k * time + 1))
})

test_that("rate rules make states of parameters and compartments", {
  eq <- odes(
    compartments = '<compartment id="C" size="1" constant="false"/>',
    parameters = par("p", 0, constant = FALSE),
    rules = paste0(rule("rate", "p", 2), rule("rate", "C", quote(p)))
  )

  expect_identical(eq$rhs$p, 2)
  expect_identical(eq$rhs$C, quote(p))
})

test_that("rateOf() is replaced by the right-hand side it names", {
  eq <- odes(
    parameters = paste0(
      par("p", 0, constant = FALSE),
      par("q", constant = FALSE),
      par("k", 4)
    ),
    rules = paste0(
      rule("rate", "p", quote(k)),
      rule("assignment", "q", quote(rateOf(p) + rateOf(k)))
    )
  )

  expect_identical(eq$assignments$q, quote(k + 0))
})

test_that("rateOf() of a concentration integrated as an amount uses the quotient rule", {
  eq <- odes(
    compartments = '<compartment id="C" size="2" constant="false"/>',
    species = sp("A", conc = 1),
    parameters = par("q", constant = FALSE),
    rules = paste0(
      rule("rate", "C", 1),
      rule("assignment", "q", quote(rateOf(A)))
    )
  )

  expect_identical(eq$assignments$q, call("/", quote(0 - A * 1), quote(C)))
})

test_that("initial assignments and assignment rules are solved together at time 0", {
  eq <- odes(
    species = sp("A", only_substance = TRUE),
    parameters = paste0(par("k", 2), par("b", constant = FALSE)),
    initial = paste0(
      '<initialAssignment symbol="A">',
      mml(quote(b * 10)),
      "</initialAssignment>"
    ),
    rules = rule("assignment", "b", quote(k + 1))
  )

  expect_equal(eq$initial, c(A = 30))
})

test_that("a missing value that is used is a derive error", {
  doc <- model_doc(
    species = sp("A", only_substance = TRUE),
    reactions = rxn("r", quote(A), reactants = "A")
  )

  err <- expect_error(sbml_odes(sbml_read(doc)), class = "zusbml_derive_error")
  expect_equal(err$ids, "A")
})

test_that("a rule without math has no effect", {
  eq <- odes(
    parameters = par("p", 3, constant = FALSE),
    rules = '<assignmentRule variable="p"/>'
  )

  expect_identical(eq$rhs$p, 0)
  expect_equal(eq$initial, c(p = 3))
})

test_that("a reaction without a kinetic law is a derive error", {
  doc <- model_doc(
    species = sp("A", amount = 1),
    reactions = '<reaction id="r" reversible="false"><listOfReactants><speciesReference species="A" stoichiometry="1" constant="true"/></listOfReactants></reaction>'
  )

  err <- expect_error(sbml_odes(sbml_read(doc)), class = "zusbml_derive_error")
  expect_equal(err$ids, "r")
})

test_that("algebraic rules, delay and fast reactions are refused", {
  algebraic <- model_doc(
    parameters = par("p", 1, constant = FALSE),
    rules = paste0("<algebraicRule>", mml(quote(p - 1)), "</algebraicRule>")
  )
  delay <- model_doc(
    parameters = paste0(par("p", 1, constant = FALSE), par("q", 1)),
    rules = rule("assignment", "p", quote(delay(q, 1)))
  )
  fast <- model_doc(
    species = sp("A", amount = 1),
    reactions = '<reaction id="r" reversible="false" fast="true"/>',
    version = 1
  )

  err <- expect_error(
    sbml_odes(sbml_read(algebraic)),
    class = "zusbml_unsupported"
  )
  expect_equal(err$construct, "algebraicRule")
  err <- expect_error(sbml_odes(sbml_read(delay)), class = "zusbml_unsupported")
  expect_equal(err$construct, "delay")
  err <- expect_error(sbml_odes(sbml_read(fast)), class = "zusbml_unsupported")
  expect_equal(err$id, "r")
})

test_that("ids named time and pi do not clash with the csymbol and constant", {
  doc <- model_doc(
    parameters = paste0(
      par("time", 10),
      par("pi", 7),
      par("a", constant = FALSE),
      par("b", constant = FALSE)
    ),
    rules = paste0(
      "<assignmentRule variable=\"a\"><math xmlns=\"http://www.w3.org/1998/Math/MathML\">",
      "<apply><plus/><ci>time</ci><ci>pi</ci></apply></math></assignmentRule>",
      "<assignmentRule variable=\"b\"><math xmlns=\"http://www.w3.org/1998/Math/MathML\">",
      "<apply><plus/><csymbol encoding=\"text\" definitionURL=\"http://www.sbml.org/sbml/symbols/time\">t</csymbol><pi/></apply>",
      "</math></assignmentRule>"
    )
  )

  eq <- sbml_odes(sbml_read(doc))

  expect_identical(eq$assignments$a, quote(time + pi))
  expect_identical(eq$assignments$b, call("+", quote(.time), pi))
})

test_that("sbml_initial_state() gives the states at time 0", {
  m <- sbml_read(test_path("fixtures", "suite", "00000", "00000-sbml-l3v2.xml"))

  expect_equal(sbml_initial_state(m), c(S1 = 1, S2 = 0))
})

test_that("the derived equations integrate to the analytic solution", {
  skip_if_not_installed("deSolve")
  derivs_function <- utils::getFromNamespace("derivs_function", "zusbml")
  m <- sbml_read(test_path("fixtures", "suite", "00000", "00000-sbml-l3v2.xml"))
  eq <- sbml_odes(m)

  out <- deSolve::ode(
    eq$initial,
    c(0, 1, 2),
    derivs_function(eq),
    eq$constants,
    rtol = 1e-10,
    atol = 1e-12
  )

  expect_equal(unname(out[, "S1"]), exp(-c(0, 1, 2)), tolerance = 1e-8)
})

test_that("sbml_odes() refuses anything but a model", {
  expect_error(sbml_odes(list()), class = "zusbml_arg_error")
})
