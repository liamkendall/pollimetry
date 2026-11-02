.choice <- function(x, choices, name) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !x %in% choices)
    stop(name, " must be one of: ", paste(choices, collapse = ", "), call. = FALSE)
  x
}
.positive <- function(x, name) {
  if (!is.numeric(x) || !is.null(dim(x)) || !length(x) ||
      anyNA(x) || any(!is.finite(x)) || any(x <= 0))
    stop(name, " must contain finite positive numeric values.", call. = FALSE)
  invisible(x)
}
