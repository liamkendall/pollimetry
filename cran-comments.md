## Draft — update after final checks

This update introduces Bayesian foraging range predictions and extends tongue-length
predictions to Melittidae using the combined Cariveau and Melin datasets.
It also updates existing model fits in the companion package, improves input
validation and documentation, and bundles tongue-model training data for offline use.

## Test environments and results

The preceding local source (version 1.1.0 with the maintenance changes) passed
R CMD check on the maintainer's Mac with 0 errors, 0 warnings and 0 notes.

Record the exact R and macOS versions and the final version 1.1.1 check
results here before submission. Cross-platform checks have not yet been
recorded. Do not submit this draft unchanged.

## Optional model package

Bayesian predictions use fitted objects from the optional pollimetrydata
package at https://github.com/liamkendall/pollimetrydata.
The release installation path and availability of the updated companion
models must be verified before submission.
