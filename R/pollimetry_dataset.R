#' Body size measurements of pollinating insects.
#'
#' A dataset of 4,434 bee and hoverfly specimens, containing dry body mass (mg), intertegular distance (mm), body length (mm), taxonomy and collection metadata.
#' @format A data frame with 4,434 rows and 26 columns.
#' \describe{
#' \item{\code{Superfamily}}{Taxonomic identifiers.}
#' \item{\code{Family}}{Taxonomic identifiers.}
#' \item{\code{Subfamily}}{Taxonomic identifiers.}
#' \item{\code{Tribe}}{Taxonomic identifiers.}
#' \item{\code{Genus}}{Taxonomic identifiers.}
#' \item{\code{Subgenus}}{Taxonomic identifiers.}
#' \item{\code{Species}}{Taxonomic identifiers.}
#' \item{\code{Sex}}{Specimen sex.}
#' \item{\code{Spec.wgt}}{Dry body mass in mg.}
#' \item{\code{IT}}{Intertegular distance in mm.}
#' \item{\code{BL}}{Body length in mm.}
#' \item{\code{Tag}}{Specimen identifier.}
#' \item{\code{Region}}{Geographical and collection-location metadata.}
#' \item{\code{Country}}{Geographical and collection-location metadata.}
#' \item{\code{Locality}}{Geographical and collection-location metadata.}
#' \item{\code{Location}}{Geographical and collection-location metadata.}
#' \item{\code{Latitude}}{Collection coordinates in decimal degrees.}
#' \item{\code{Longitude}}{Collection coordinates in decimal degrees.}
#' \item{\code{Measurement}}{Original measurement, preservation, collection-date, plant and collection-method metadata.}
#' \item{\code{Pres.time}}{Original measurement, preservation, collection-date, plant and collection-method metadata.}
#' \item{\code{Col.date}}{Original measurement, preservation, collection-date, plant and collection-method metadata.}
#' \item{\code{Plant}}{Original measurement, preservation, collection-date, plant and collection-method metadata.}
#' \item{\code{Col.method}}{Original measurement, preservation, collection-date, plant and collection-method metadata.}
#' \item{\code{log(Spec.wgt)}}{Stored transformed predictors and grouping identifier used by the original models.}
#' \item{\code{log(IT)}}{Stored transformed predictors and grouping identifier used by the original models.}
#' \item{\code{Region:Species}}{Stored transformed predictors and grouping identifier used by the original models.}
#' }
#' @details Original observations are retained. Stored model predictors are missing outside the original training subsets. The model_training attribute contains specifications for seven models, including row selections, columns, factor levels, factor attributes and row names required to reconstruct their training data.
#' @references Kendall et al. (2019). Pollinator size and its consequences: Robust estimates of body size in pollinating insects. \doi{10.1002/ece3.4835}.
#' @usage data(pollimetry_dataset)
#' @keywords datasets
"pollimetry_dataset"
