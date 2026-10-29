# x, or y when x is NULL.
`%||%` <- function(x, y) if (is.null(x)) y else x

# x, or y when x is NA.
`%|%` <- function(x, y) if (is.na(x)) y else x
