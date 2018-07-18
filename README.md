[![DOI](https://zenodo.org/badge/120989038.svg)](https://zenodo.org/badge/latestdoi/120989038)
Pollimetry: Predictive allometry for pollinating insects
==========

Tools to estimate pollinator body size as well as bee tongue length and foraging distances.

To install
==========
```
if (!requireNamespace("devtools")) {
  install.packages("devtools")
}
devtools::install_github("liamkendall/pollimetry")
library(pollimetry)
```

Estimating body size
====================

The `bodysize` function uses Bayesian generalised linear mixed models (BGLMMs) to provide posterior estimates (along with S.E. and 95% confidence intervals) of pollinator body size (i.e. dry body weight (mg)) using the intertegular distance (ITD), species taxonomy or phylogeny (bees only `type="phylo"`), sex and biogeography (at present only Australia, Europe, North America and South America). Estimates (and variance components) are returned as four additional columns bound to the original dataframe. These models will be periodically updated using novel data as and when it becomes available. See `bodysize` details for more information.

Pre-existing equations for Diptera, Hymenoptera and Lepidopteran taxa using body length (`lengthsize`), body length * width (`lengthwidthsize`) and head width (`headwidthsize`) are also provided.

Allometric traits
=================

Users can predict bee tongue length (`tonguelength`) from Cariveau et al. (2015) and bee foraging distances (`foragedist`) from equations described in van Nieuwstadt and Iraheta (1996) for Meliponini, Greenleaf et al. (2007) and Guedot et al. (2009) for Osmia spp.

Future traits of interest
=========================

Many other ecological traits crucial to pollination are likely to be allometric. Therefore, we hope to examine these unexplored body size - trait relationships with the aim of including new predictive models in the future. 

Example
========

`Pollimetry` needs the developmental version of brms, you can get it here:

```
if (!requireNamespace("devtools")) {
  install.packages("devtools")
}
devtools::install_github("paul-buerkner/brms")

```

Second, you can install and call the pollimetry library as follows:

```
devtools::install_github("liamkendall/pollimetry",)
library(pollimetry)
```

You can check the raw data used in the paper as follows:

```
load("data/bee_mean_dataset.rdata")
head(bee_mean_dataset)
?bee_mean_dataset #for metadata.
#See also hov_mean_dataset.rdata and pollimetry_dataset.rdata
```
Let's predict some body sizes from a dataframe using taxonomy:

```
(example <- cbind.data.frame(IT = c(1.2, 2.3), 
                            Sex = c("Female","Male"), 
                            Family = c("Apidae","Andrenidae"),
                            Region = c("NorthAmerica","Europe"),
                            Species = c("Ceratina_dupla","Andrena_flavipes")))

bodysize(x = example, taxa = "bee", type = "taxo")
```

Now let's calculate some foraging distances based only in ITDs:

```
foragedist(c(10,5,2), type = "GreenleafAll") 
```

Other functions included are `headwidthsize()`, `lengthwidthsize()`, and `lengthsize()` for X. And `tonguelength()` for estimating tongue length.

```




