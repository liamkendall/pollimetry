#' @name foraging.range
#' @title Predict typical and maximum bee foraging ranges
#' @description Predict realized or potential bee foraging ranges using the models of Kendall et al. (2022).
#' @param data A nonempty data frame with positive, finite ITD measurements in mm. The social model requires sociality: "Highly Eusocial", "Primitively Eusocial" or "Solitary". Group effects require species in Genus_species format; genus is derived from species.
#' @param model.type One string: "social" (default) or "ITD". Both models use estimated body weight; the social model also uses sociality.
#' @param random.effects One string: "reduced" (default, no group effects), "phylo" (genus effect), or "full" (genus and species effects). Publication and metric effects are excluded for all choices.
#' @param measure.type One string: "realized" (default) or "potential".
#' @return A data frame with two rows per input specimen, typical then maximum range. range.estimate, range.lower and range.upper are in kilometres; bounds give a 95% posterior predictive interval. wgt is estimated female dry weight in mg and wgt10 is its base-10 logarithm. Additional columns include range.type and, when species is supplied, genus.
#' @details The response is log10(range in km), so predictions and interval endpoints are back-transformed with 10^x. range.estimate is 10 raised to the mean log10-scale predictive draw, not arithmetic mean distance. Intervals include residual variation. Female body weight is estimated once per input row using the bee sex model without group effects, then reused for both range types. Body-weight uncertainty is not propagated through the range model. Excluded group effects are set to zero, not integrated out. New included grouping levels use uncertainty sampling from existing levels, not a new phylogenetic placement. Use set.seed() for reproducibility. Requires brms. Uses pollimetrydata (>= 1.0.1) when installed; otherwise downloads only the required fitted models from GitHub. Downloaded models are cached in memory for the current R session. An internet connection is required for each model's first download in a session. No Bayesian models are fitted during prediction.
#' @examples if (requireNamespace("pollimetrydata", quietly = TRUE) && 
#' utils::packageVersion("pollimetrydata") >= "1.0.1") {
#'   x <- data.frame(ITD = c(2.9, 2.5),
#'                   sociality = c("Highly Eusocial", "Solitary"))
#'   foraging.range(x)
#' }
#' @references Kendall et al. (2022). The potential and realized foraging movements of bees are differentially determined by body size and sociality. Ecology, 103(11), e3809. \doi{10.1002/ecy.3809}.
#' @export
foraging.range <- function(data, model.type = "social",
                           random.effects = "reduced", measure.type = "realized") {
  model.type <- .choice(model.type, c("social", "ITD"), "model.type")
  random.effects <- .choice(random.effects, c("full", "phylo", "reduced"), "random.effects")
  measure.type <- .choice(measure.type, c("realized", "potential"), "measure.type")
  .frame(data, c("ITD", if (model.type == "social") "sociality",
                if (random.effects != "reduced") "species"), "data")
  .positive(data$ITD, "ITD")
  if (model.type == "social") .levels(data$sociality,
    c("Highly Eusocial", "Primitively Eusocial", "Solitary"), "sociality")
  if ("species" %in% names(data)) .groups(data$species, "species")
  data.out <- data[rep(seq_len(nrow(data)), each = 2L), , drop = FALSE]
  rownames(data.out) <- NULL
  if ("species" %in% names(data.out)) data.out$genus <- sub("_.*$", "", data.out$species)
  measures <- if (measure.type == "realized") c("Typ-Realised", "Max-Realised") else
    c("Typ-Potential", "Max-Potential")
  data.out$range.4 <- rep(measures, nrow(data))
  mod <- .model(if (model.type == "social") "all_social_bm_mod" else "all_IT_bm_mod")
  size <- bodysize(data.frame(ITD = data$ITD, Sex = "Female"),
                   type = "sex", taxa = "bee")
  data.out$wgt <- rep(size$Est.Weight, each = 2L)
  data.out$wgt10 <- log10(data.out$wgt)
  groups <- switch(random.effects, full = c("species", "genus"), phylo = "genus", reduced = character())
  for (nm in groups) {
    missing <- setdiff(data.out[[nm]], mod$data[[nm]])
    if (length(missing)) warning("New ", nm, " levels: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  re <- switch(random.effects, full = ~(1 | genus) + (1 | species), phylo = ~(1 | genus), reduced = NA)
  out <- 10^stats::predict(mod, newdata = data.out, allow_new_levels = TRUE,
                            sample_new_levels = "uncertainty", re_formula = re, probs = c(0.025, 0.975), cores = 1)
  labels <- c("Typ-Realised" = "Realized typical range", "Typ-Potential" = "Potential typical range",
              "Max-Realised" = "Realized maximum range", "Max-Potential" = "Potential maximum range")
  data.out$range.type <- unname(labels[data.out$range.4])
  keep <- grepl(if (measure.type == "realized") "Realized" else "Potential", data.out$range.type)
  data.out$range.4 <- NULL
  out <- as.data.frame(out[, c("Estimate", "Q2.5", "Q97.5"), drop = FALSE])
  names(out) <- c("range.estimate", "range.lower", "range.upper")
  result <- cbind(data.out, out)[keep, , drop = FALSE]
  rownames(result) <- NULL
  result
}
