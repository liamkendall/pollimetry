## Release

This update introduces Bayesian foraging range predictions and extends
tongue length predictions to Melittidae using the combined Cariveau and
Melin datasets. It also improves input validation and documentation.

## Test environment

- macOS Sequoia 15.7.9
- R 4.6.1 (2026-06-24)
- brms 2.23.0; pollimetrydata 1.0.1

## R CMD check results

pollimetry 1.1.1: 0 errors, 0 warnings, 0 notes.

## Optional model package

Bayesian predictions optionally use pollimetrydata (>= 1.0.1), available at
https://github.com/liamkendall/pollimetrydata.
Installation of version 1.0.1 from the public GitHub source archive
was verified in a separate temporary library without authentication.

Without this package, required models are downloaded on demand 
and cached in memory for the current session. 
The download fallback and cache reuse were verified for hov_IT. 
Examples require the installed companion package and do not download models.
