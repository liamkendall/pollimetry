# pollimetry 1.1.1

* Introduce Bayesian bee foraging range predictions through foraging.range(), based on Kendall et al. (2022), with typical and maximum realized or potential ranges and 95% posterior predictive intervals.
* Extend tongue-length predictions with Melittidae measurements from Melin et al. (2019), combined with Cariveau et al. (2016). Provide the combined training data as tongue_dataset for offline use.
* Update existing Bayesian model fits in the companion pollimetrydata package for current brms/Stan software.
* Support Pipizinae in hoverfly taxonomic body size predictions.
* Fix equation selection in length-based functions and use the documented H97HA head-width equation identifier.
* Improve validation of input measurements, categories and required columns.
* Consolidate body size and foraging training datasets and retain metadata for reconstructing model inputs.
* Update function and dataset documentation, prediction units and uncertainty descriptions.
* Remove unused dependencies and update package build exclusions.
