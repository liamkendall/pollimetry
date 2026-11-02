#' @name headwidthsize
#'
#' @title Converts pollinator (Diptera, Hymenoptera and Lepidoptera) head width (mm) to body size (dry weight (mg)).
#'
#' @description Calculates dry body weight (mg) using head width (mm) from Hodar (1997).
#'
#' @param HW A nonempty numeric vector of head widths in mm. All values must be finite, strictly positive and nonmissing.
#'
#' @param Eq A single character string selecting an equation from Hodar (1997).
#' Options implemented are: H97DB (Brachycera), H97DN (Nematocera), H97HA (Hymenoptera), H97LH (Heterocera) and H97LR (Rhopalocera).
#'
#' @return A numeric vector of predicted dry weights (mg), one per input measurement.
#'
#' @examples
#' headwidthsize(HW = c(1, 2, 3), Eq = "H97HA")
#' @references \itemize{
#' \item Kendall et al. (2019) Pollinator size and its consequences: Robust estimates of body size in pollinating insects. \emph{Ecology and Evolution}, 9(4), 1702-1714. \doi{10.1002/ece3.4835}.
#' \item Hodar, J. A. (1997). The use of regresion equations for the estimation of prey length and biomass in diet studies of insectivore vertebrates. \emph{Miscellania Zoologica}, 20(2), 1-10.
#' }
#'
#' @export
headwidthsize <- function(HW, Eq = "H97DB"){
  .positive(HW, "HW")
  .choice(Eq, c("H97DB", "H97DN", "H97HA", "H97LH", "H97LR"), "Eq")
  if(!Eq %in% c("H97DB",
                "H97DN",
                "H97HA","H97LH","H97LR")) {
    stop("Eq should be one of 'H97DB', 'H97DN', 'H97HA' , 'H97LH' or'H97LR'")
  } else {
    H97DB <- 0.655*(HW)^2.526
    H97DN <- 3.942*(HW)^3.106
    H97HA <- 1.999*(HW)^2.09
    H97LH <- 2.053*(HW)^2.804
    H97LR <- 1.634*(HW)^2.793
    if (Eq == "H97DB") out <- H97DB
    if (Eq == "H97DN") out <- H97DN
    if (Eq == "H97HA") out <- H97HA
    if (Eq == "H97LH") out <- H97LH
    if (Eq == "H97LR") out <- H97LR
    out
  }
}
