#' Combined bee tongue-length training data.
#'
#' Combined training measurements from Cariveau et al. (2016) and Melin et al. (2019), covering six bee families, including Melittidae.
#' @format A data frame with 112 rows and eight columns.
#' \describe{
#' \item{\code{Family}}{Bee family.}
#' \item{\code{mean_tongue_length_mm}}{Mean total proboscis length in mm.}
#' \item{\code{IT}}{Mean intertegular distance in mm.}
#' \item{\code{mean_glossa_length_mm}}{Mean glossa length in mm.}
#' \item{\code{mean_prementum_length_mm}}{Mean prementum length in mm.}
#' \item{\code{genus}}{Genus and species identifiers retained from each source.}
#' \item{\code{species}}{Genus and species identifiers retained from each source.}
#' \item{\code{source}}{Source publication.}
#' }
#' @details Contains the 101 original Cariveau training rows followed by 11 Melin species means. Melin measurements are averaged within Family, genus and species using the same aggregation as the prediction function previously used. Cariveau rows are retained without further aggregation. This is the combined training dataset used by tonguelength; no observations have been added. The original Cariveau file was obtained from the ibartomeus/traitbase repository (raw_data/Cariveau_2016.rda).
#' @references Cariveau et al. (2016). The Allometry of Bee Proboscis Length and Its Uses in Ecology. \doi{10.1371/journal.pone.0151482}.
#' 
#' Melin et al. (2019). The allometry of proboscis length in Melittidae (Hymenoptera: Apoidae) and an estimate of their foraging distance using museum collections. \doi{10.1371/journal.pone.0217839}.
#' @usage data(tongue_dataset)
#' @keywords datasets
"tongue_dataset"
