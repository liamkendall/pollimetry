# Software-maintenance refit candidate. Not yet tested with current brms/Stan.
# Exact inputs extracted from pollimetrydata/data/all_IT_bm_mod.rdata; original md5: 37f90eb4450d354d18347fcfb0e75406
# Sampling counts/seeds come from the stored fit; historical init settings are not reconstructed.
# Backend is explicitly cmdstanr; toolchain compatibility and diagnostics remain to be checked.
# Foraging adapt_delta and max_treedepth come from forage/script/A-02-models.R.
# Run from the package root. No model is replaced; results go into outputs/.
# Retains stored priors including non-empty historical default priors.
if (!requireNamespace("brms", quietly = TRUE)) stop("Install brms first.")
base <- "model-building/foraging-range"
dataset_env <- new.env(parent = baseenv())
load("data/forage_dataset.rdata", envir = dataset_env)
dataset <- dataset_env$forage_dataset
training_spec <- attr(dataset, "model_training")[["all_IT_bm_mod"]]
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
  brms::set_prior("normal(0, 2)", class = "b"),
  brms::set_prior("normal(-2, 2)", class = "Intercept"),
  brms::set_prior("normal(0, 0.5)", class = "sd"),
  brms::set_prior("student_t(3, 0, 2.5)", class = "sigma")
)
model_formula <- brms::bf(dist10 ~ range.4 * wgt10 + (1 | publication) + (1 | metric) + (1 | species) + (1 | gr(genus, cov = phy.g.vcv)))
covariance <- readRDS(file.path(base, "phylogenies/foraging_genus_covariance.rds"))
output_dir <- file.path(base, "outputs")
dir.create(output_dir, showWarnings = FALSE)
output_file <- file.path(output_dir, "all_IT_bm_mod_refit.rds")
if (file.exists(output_file)) stop("Output already exists; move it before refitting.")
fit <- brms::brm(
  formula = model_formula, data = training_data,
  family = stats::gaussian(link = "identity"), prior = priors,
  data2 = covariance,
  chains = 4, cores = min(4, parallel::detectCores()),
  iter = 3000, warmup = 1500, thin = 1,
  seed = 264804054, control = list(adapt_delta = 0.999, max_treedepth = 15),
  backend = "cmdstanr"
)
saveRDS(fit, output_file, compress = "xz")
