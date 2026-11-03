[![DOI](https://zenodo.org/badge/120989038.svg)](https://zenodo.org/badge/latestdoi/120989038)
[![CRAN status](https://www.r-pkg.org/badges/version/pollimetry)](https://www.r-pkg.org/badges/version/pollimetry)
![](https://cranlogs.r-pkg.org/badges/grand-total/pollimetry?color=green)
![](https://cranlogs.r-pkg.org/badges/pollimetry?color=green)

Pollimetry: Robust estimates of body size and co-varying traits in  pollinating insects
==========

Tools to estimate pollinator body size and co-varying ecological traits.

To install
==========
```
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}
devtools::install_github("liamkendall/pollimetry")
library(pollimetry)
```

The Bayesian functions use fitted models from `pollimetrydata`. Installing version 1.0.1 or later is optional and enables offline predictions. Otherwise, only the required models are downloaded from GitHub and reused within the current R session. The equation-based functions below do not require these downloads.

```
# The companion package contains large fitted model objects.
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}
# Optional: install all fitted models for offline use
devtools::install_github("liamkendall/pollimetrydata")
```

Estimating body size
====================

The `bodysize` function uses Bayesian generalised linear mixed models (BGLMMs) to provide posterior estimates (with 90% posterior predictive intervals) of pollinator body size (i.e. dry body weight (mg)) using the intertegular distance (ITD), species taxonomy or phylogeny (bees only `type="phylo"`), sex and biogeography (at present only Australia, Europe, North America and South America). Three columns are appended to the input data frame: `Est.Weight`, `CI_Lower` and `CI_Upper`. The intervals include residual variation. `Est.Weight` is the exponentiated mean prediction on the log scale, rather than the arithmetic mean weight. See `bodysize` details for more information.

Pre-existing equations for Diptera, Hymenoptera and Lepidopteran taxa using body length (`lengthsize`), body length * width (`lengthwidthsize`) and head width (`headwidthsize`) are also provided.

Supply body length and head width in millimetres; supply length × width in square millimetres. Measurements must be nonempty numeric vectors containing only finite, positive values, with no missing values. `Eq` selects one equation or a supported group using a single character string. Single equations return numeric vectors; equation groups return matrices with one row per measurement and named equation columns.


See the function help pages for supported equation choices. These equations return point predictions without uncertainty intervals.

Allometric traits
=================

Users can predict, i) bee tongue length (`tonguelength`) using combined data from Cariveau et al. (2016) and Melin et al. (2019), including Melittidae, ii) bee foraging distances (`foragedist`) from equations described in van Nieuwstadt and Ruano Iraheta (1996) for Meliponini, Greenleaf et al. (2007) and Guedot et al. (2009) for Osmia spp, iii) wing loading (`wingloading`) from Bullock (1999) and, iv)  field nectar loads (`nectarload`) from Henry and Rodet (2018).

The `foraging.range()` function uses the Bayesian models of Kendall et al. (2022) to estimate typical and maximum realized or potential foraging ranges in kilometres, with 95% posterior predictive intervals. Predictions are transformed from the models' log10 scale. The point estimate is the back-transformed mean on that scale, not an arithmetic mean distance. The older `foragedist()` function is deprecated; its published equations remain available, with units documented in its help page.

The package provides three datasets: `pollimetry_dataset` (body size), `forage_dataset` (foraging range), and `tongue_dataset` (combined tongue-length training data).

Example
========

Install and call the pollimetry library as follows:

```
devtools::install_github("liamkendall/pollimetry")
library(pollimetry)
```

This will install pollimetry and its dependency `brms`.
You can check the raw data used in the paper as follows:

```
data("pollimetry_dataset", package = "pollimetry")
head(pollimetry_dataset)
?pollimetry_dataset #for metadata.
```
Let's predict some body sizes from a dataframe using taxonomy:

```
(example <- cbind.data.frame(ITD = c(1.2, 2.3),
                            Sex = c("Female","Male"),
                            Family = c("Apidae","Andrenidae"),
                            Region = c("NorthAmerica","Europe"),
                            Species = c("Ceratina_dupla","Andrena_flavipes")))

bodysize(x = example, taxa = "bee", type = "taxo")
?bodysize
```

Now let's estimate realized foraging ranges using ITD:

```
foraging.range(
  data.frame(ITD = c(2, 3)),
  model.type = "ITD",
  random.effects = "reduced",
  measure.type = "realized"
)
```


Manuscript R code
=================

Kendall, L. (2018). Pollimetry R code. Figshare.
doi:10.6084/m9.figshare.7357217.v1
