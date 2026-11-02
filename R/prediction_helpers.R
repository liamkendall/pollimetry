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
.model <- local({
  cache <- new.env(parent = emptyenv())
  
  function(name) {
    if (!requireNamespace("brms", quietly = TRUE))
      stop("The brms package is required for Bayesian predictions.",
           call. = FALSE)
    
    files <- c(
      bee_IT = "bee_IT.rdata",
      bee_sex = "bee_sex_mod.rdata",
      bee_tax_mod = "bee_tax_mod.rdata",
      bee_phy_mod = "bee_phy_mod.rdata",
      hov_IT = "hov_IT.rdata",
      hov_sex = "hov_sex_mod.rdata",
      hov_tax_mod = "hov_tax_mod.rdata",
      all_IT_bm_mod = "all_IT_bm_mod.rdata",
      all_social_bm_mod = "all_social_bm_mod.rdata"
    )
    
    if (length(name) != 1L || is.na(name) || !name %in% names(files))
      stop("Unknown model requested.", call. = FALSE)
    
    if (requireNamespace("pollimetrydata", quietly = TRUE) &&
        utils::packageVersion("pollimetrydata") >= "1.0.1")
      return(getExportedValue("pollimetrydata", name))
    
    if (exists(name, envir = cache, inherits = FALSE))
      return(get(name, envir = cache, inherits = FALSE))
    
    url <- paste0(
      "https://raw.githubusercontent.com/",
      "liamkendall/pollimetrydata/master/data/",
      files[[name]]
    )
    
    message("Downloading model '", name, "' from pollimetrydata...")
    destination <- tempfile(fileext = ".rdata")
    on.exit(unlink(destination), add = TRUE)
    
    old_options <- options(timeout = max(300, getOption("timeout", 60)))
    on.exit(options(old_options), add = TRUE)
    
    model_env <- new.env(parent = baseenv())
    
    fit <- tryCatch({
      status <- utils::download.file(
        url, destination, mode = "wb", quiet = TRUE
      )
      if (status != 0L)
        stop("Download failed.")
      
      load(destination, envir = model_env)
      
      if (!exists(name, envir = model_env, inherits = FALSE))
        stop("Downloaded file does not contain the requested model.")
      
      object <- get(name, envir = model_env, inherits = FALSE)
      if (!inherits(object, "brmsfit"))
        stop("Downloaded object is not a brms model.")
      
      object
    }, error = function(e) {
      stop(
        "Could not download model '", name, "': ",
        conditionMessage(e),
        "\nCheck your internet connection or install pollimetrydata >= 1.0.1.",
        call. = FALSE
      )
    })
    
    assign(name, fit, envir = cache)
    fit
  }
})
