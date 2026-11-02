.frame <- function(x, columns, name) {
  if (!is.data.frame(x) || !nrow(x) || anyDuplicated(names(x)))
    stop(name, " must be a non-empty data frame with unique column names.", call. = FALSE)
  missing <- setdiff(columns, names(x))
  if (length(missing)) stop("Missing columns: ", paste(missing, collapse = ", "), call. = FALSE)
}
.levels <- function(x, choices, name) {
  if (!(is.character(x) || is.factor(x)) || anyNA(x) || any(!as.character(x) %in% choices))
    stop(name, " must contain only: ", paste(choices, collapse = ", "), call. = FALSE)
}
.groups <- function(x, name) {
  if (!(is.character(x) || is.factor(x)) || anyNA(x) || any(!nzchar(as.character(x))))
    stop(name, " must contain non-missing, non-empty names.", call. = FALSE)
}
.model <- function(name) {
  if (!requireNamespace("pollimetrydata", quietly = TRUE))
    stop("Install the optional pollimetrydata package before predicting: ",
         "https://github.com/liamkendall/pollimetrydata. No models are downloaded automatically.",
         call. = FALSE)
  if (!requireNamespace("brms", quietly = TRUE))
    stop("The brms package is required for Bayesian predictions.", call. = FALSE)
  getExportedValue("pollimetrydata", name)
}
