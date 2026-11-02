# Software-maintenance refit candidate. Not yet tested with current brms/Stan.
# Exact inputs extracted from pollimetrydata/data/bee_IT.rdata; original md5: 80e881428cd8f89f08df290c8a938de2
# Sampling counts/seeds come from the stored fit; historical init settings are not reconstructed.
# Backend is explicitly cmdstanr; toolchain compatibility and diagnostics remain to be checked.
# Run from the package root. No model is replaced; results go into outputs/.
# Retains stored priors including non-empty historical default priors.
if (!requireNamespace("brms", quietly = TRUE)) stop("Install brms first.")
base <- "model-building/body-size"
dataset_env <- new.env(parent = baseenv())
load("data/pollimetry_dataset.rdata", envir = dataset_env)
dataset <- dataset_env$pollimetry_dataset
training_spec <- attr(dataset, "model_training")[["bee_IT"]]
training_data <- dataset[training_spec$rows, , drop = FALSE]
for (column in names(training_spec$factor_levels)) {
  training_data[[column]] <- factor(as.character(training_data[[column]]),
    levels = training_spec$factor_levels[[column]])
}
for (column in names(training_spec$factor_attributes)) {
  attributes(training_data[[column]]) <- training_spec$factor_attributes[[column]]
}
training_data <- training_data[, training_spec$columns, drop = FALSE]
rownames(training_data) <- training_spec$row_names
priors <- c(
  brms::set_prior("normal(0, 0.4)", class = "b"),
  brms::set_prior("normal(1, 1)", class = "b", coef = "logIT"),
  brms::set_prior("normal(0, 1)", class = "Intercept"),
  brms::set_prior("normal(0, 0.4)", class = "sd"),
  brms::set_prior("normal(0, 0.4)", class = "sigma")
)
model_formula <- brms::bf(log(Spec.wgt) ~ log(IT) + (1 | Region/Species))
output_dir <- file.path(base, "outputs")
dir.create(output_dir, showWarnings = FALSE)
output_file <- file.path(output_dir, "bee_IT_refit.rds")
if (file.exists(output_file)) stop("Output already exists; move it before refitting.")
fit <- brms::brm(
  formula = model_formula, data = training_data,
  family = stats::gaussian(link = "identity"), prior = priors,
  chains = 4, cores = min(4, parallel::detectCores()),
  iter = 2000, warmup = 1000, thin = 1,
  seed = 247316589, control = list(adapt_delta = 0.99, max_treedepth = 15),
  backend = "cmdstanr"
)
saveRDS(fit, output_file, compress = "xz")
