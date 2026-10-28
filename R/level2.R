# Level 2 Versions 4 and 5 into the Level 3 shape (design section 6.4). The
# reader has already applied Level 2's attribute defaults; this turns the
# parsed model into what a Level 3 document would hold, so the model object
# means the same thing whatever the source level. Writing is always Level 3
# Version 2, so a Level 2 model round-trips to an equivalent document, not
# an identical one.

convert_level2 <- function(model, ctx) {
  ch <- model$children
  if (!is.null(ch$listOfCompartmentTypes)) {
    ctx$dropped <- union(ctx$dropped, "compartmentType")
  }
  if (!is.null(ch$listOfSpeciesTypes)) {
    ctx$dropped <- union(ctx$dropped, "speciesType")
  }
  ch$listOfCompartmentTypes <- NULL
  ch$listOfSpeciesTypes <- NULL

  taken <- ctx$ids
  fresh_id <- function(base) {
    id <- base
    n <- 1L
    while (id %in% taken) {
      n <- n + 1L
      id <- paste0(base, "_", n)
    }
    taken <<- c(taken, id)
    id
  }

  rules <- ch$listOfRules %||% list()
  ch$listOfReactions <- lapply(ch$listOfReactions, function(r) {
    law <- r$children$kineticLaw
    if (!is.null(law)) {
      law$children$listOfLocalParameters <- law$children$listOfParameters
      law$children$listOfParameters <- NULL
      r$children$kineticLaw <- law
    }
    for (side in c("listOfReactants", "listOfProducts")) {
      r$children[[side]] <- lapply(r$children[[side]], function(sr) {
        math <- sr$children$stoichiometryMath
        sr$children$stoichiometryMath <- NULL
        if (is.null(math)) {
          # Level 2's default stoichiometry is 1, fixed for the run.
          if (is.na(sr$attributes$stoichiometry)) {
            sr$attributes$stoichiometry <- 1
          }
          sr$attributes$constant <- TRUE
          return(sr)
        }
        # stoichiometryMath is an assignment rule on the reference's id in
        # Level 3 (Level 3 Version 2 Core, section 4.11.3).
        id <- sr$attributes$id
        if (is.na(id)) {
          id <- fresh_id(paste0(
            r$attributes$id,
            "_",
            sr$attributes$species,
            "_stoichiometry"
          ))
          sr$attributes$id <- id
        }
        sr$attributes$constant <- FALSE
        rules[[length(rules) + 1]] <<- list(
          kind = "assignmentRule",
          attributes = list(
            variable = id,
            id = NA_character_,
            name = NA_character_,
            metaid = NA_character_,
            sboTerm = NA_character_
          ),
          notes = NA_character_,
          annotation = NA_character_,
          children = list(math = math$children$math)
        )
        sr
      })
    }
    r
  })
  if (length(rules) > 0) {
    ch$listOfRules <- rules
  }

  # Level 2 events fire only on a false-to-true change after time 0 and
  # always execute once triggered.
  ch$listOfEvents <- lapply(ch$listOfEvents, function(e) {
    if (!is.null(e$children$trigger)) {
      e$children$trigger$attributes$initialValue <- TRUE
      e$children$trigger$attributes$persistent <- TRUE
    }
    e
  })
  model$children <- ch

  # Level 2's built-in units become the model's units: a unit definition
  # that redefines a built-in name is used, otherwise the base unit.
  defined <- vapply(
    ch$listOfUnitDefinitions,
    function(u) u$attributes$id,
    character(1)
  )
  unit <- function(name, base) if (name %in% defined) name else base
  model$attributes$substanceUnits <- unit("substance", "mole")
  model$attributes$timeUnits <- unit("time", "second")
  model$attributes$volumeUnits <- unit("volume", "litre")
  model$attributes$lengthUnits <- unit("length", "metre")
  model$attributes$areaUnits <- unit("area", NA_character_)
  model$attributes$extentUnits <- model$attributes$substanceUnits
  model$attributes$conversionFactor <- NA_character_
  model
}
