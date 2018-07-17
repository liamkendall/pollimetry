Pollimetry: Predictive allometry for pollinating insects
==========

Tools to estimate pollinator body size measured as dry weight (mg), bee tongue length and foraging distances.

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

The `bodysize` function uses Bayesian generalised linear mixed models to provide posterior estimates (along with S.E. and 95% confidence intervals) of pollinator body size (i.e. dry weight (mg)) using intertegular distance (ITD) values while taking into account specimen sex, taxonomy or phylogeny and biogeographical region of origin. Estimates (and variance components) are returned as four additional columns bound to original dataframe.

Pre-existing equations for Diptera, Hymenoptera and Lepidopteran taxa using body length (`lengthsize`), body length * width (`lengthwidthsize`) and head width (`headwidthsize`) are also provided.

Allometric traits
=================

Currently, users can predict bee tongue length (`tonguelength`) from Cariveau et al. (2015) and bee foraging distances (`foragedist`) from equations described in van Nieuwstadt and Iraheta (1996) for Meliponini, Greenleaf et al. (2007) and Guedot et al. (2009) for Osmia spp.
