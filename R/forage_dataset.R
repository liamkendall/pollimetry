#' Bee foraging range measurements.
#'
#' A dataset of 425 foraging range estimates, with body size, sociality, taxonomic and study metadata.
#' @format A data frame with 425 rows and 23 columns.
#' \describe{
#' \item{\code{publication}}{Source publication and publication year.}
#' \item{\code{year}}{Source publication and publication year.}
#' \item{\code{region}}{Study location and habitat.}
#' \item{\code{country}}{Study location and habitat.}
#' \item{\code{habitat}}{Study location and habitat.}
#' \item{\code{family}}{Taxonomic identifiers and sex.}
#' \item{\code{genus}}{Taxonomic identifiers and sex.}
#' \item{\code{species}}{Taxonomic identifiers and sex.}
#' \item{\code{sex}}{Taxonomic identifiers and sex.}
#' \item{\code{range.type}}{Range category, potential or realized measure, and measurement method.}
#' \item{\code{measure.type}}{Range category, potential or realized measure, and measurement method.}
#' \item{\code{metric}}{Range category, potential or realized measure, and measurement method.}
#' \item{\code{N}}{Reported sample size.}
#' \item{\code{distance}}{Reported foraging distance in metres.}
#' \item{\code{sd}}{Reported uncertainty metadata: standard deviation, standard error and 95 percent confidence interval.}
#' \item{\code{se}}{Reported uncertainty metadata: standard deviation, standard error and 95 percent confidence interval.}
#' \item{\code{ci95}}{Reported uncertainty metadata: standard deviation, standard error and 95 percent confidence interval.}
#' \item{\code{itd}}{Intertegular distance in mm.}
#' \item{\code{sociality}}{Social organisation.}
#' \item{\code{notes}}{Original study notes.}
#' \item{\code{dist10}}{Stored log10 foraging distance in kilometres.}
#' \item{\code{range.4}}{Stored combination of range and measure categories.}
#' \item{\code{wgt10}}{Stored predicted log10 body mass in mg used in the original models.}
#' }
#' @details Stored model predictors are missing outside the original training subsets. The model_training attribute contains specifications for two models, including row selections, columns, factor levels, factor attributes and row names required to reconstruct their training data. Body masses have not been re-estimated.
#' @references Kendall et al. (2022). The potential and realized foraging movements of bees are differentially determined by body size and sociality. Ecology, 103(11), e3809. \doi{10.1002/ecy.3809}.
#' @usage data(forage_dataset)
#' @keywords datasets
"forage_dataset"
