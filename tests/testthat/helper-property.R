# A random reaction network (design section 9's property test): its SBML
# document and the generator's own right-hand side, in the order of the
# species ids. Mass action, integer stoichiometry, a mix of amount and
# concentration species in one compartment.
random_network <- function() {
  n <- sample(2:5, 1)
  species <- paste0("S", seq_len(n))
  by_amount <- sample(c(TRUE, FALSE), n, replace = TRUE)
  size <- round(stats::runif(1, 0.5, 2), 3)
  initial <- round(stats::runif(n, 0.1, 3), 3)
  reactions <- lapply(seq_len(sample(1:5, 1)), function(j) {
    list(
      id = paste0("R", j),
      k = round(stats::runif(1, 0.1, 2), 3),
      reactants = sample(species, sample(1:2, 1)),
      products = sample(species, sample(0:2, 1)),
      stoich = sample(1:3, 1)
    )
  })

  doc <- model_doc(
    compartments = paste0(
      '<compartment id="C" size="',
      size,
      '" constant="true"/>'
    ),
    species = paste0(
      vapply(
        seq_len(n),
        function(i) {
          sp_xml(
            species[[i]],
            amount = initial[[i]],
            only_substance = by_amount[[i]]
          )
        },
        ""
      ),
      collapse = ""
    ),
    parameters = paste0(
      vapply(reactions, function(r) par_xml(paste0("k", r$id), r$k), ""),
      collapse = ""
    ),
    reactions = paste0(
      vapply(
        reactions,
        function(r) {
          law <- Reduce(
            function(a, b) call("*", a, b),
            lapply(c(paste0("k", r$id), r$reactants), as.name)
          )
          rxn(
            r$id,
            law,
            reactants = r$reactants,
            products = r$products,
            stoich = r$stoich
          )
        },
        ""
      ),
      collapse = ""
    )
  )

  # Symbol values from the state: amounts for amount species, otherwise
  # concentrations.
  rhs <- function(y) {
    names(y) <- species
    change <- stats::setNames(numeric(n), species)
    for (r in reactions) {
      rate <- r$k * prod(y[r$reactants])
      change[r$reactants] <- change[r$reactants] - r$stoich * rate
      change[r$products] <- change[r$products] + r$stoich * rate
    }
    change[!by_amount] <- change[!by_amount] / size
    change
  }
  # The initial value of each species' symbol.
  y0 <- stats::setNames(ifelse(by_amount, initial, initial / size), species)
  list(doc = doc, species = species, rhs = rhs, y0 = y0)
}
