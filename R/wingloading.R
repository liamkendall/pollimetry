#' @name wingloading
#' 
#' @title Converts intertegular distance (mm) to wing loading (mg mm2).
#' 
#' @description Calculates wing loading (mg mm2) expressed as dry weight per total wing area using intertegular distance (mm) from Bullock (1999).
#' 
#' @param IT vector of intertegular distance measurements (mm).
#'
#' @return A dataframe with wing loading (mg mm2) is returned for each specimen.
#' 
#' @examples
#' wingloading(IT=c(10,5,2))
#' @references \itemize{
#' \item Kendall et al. (2019) Pollinator size and its consequences: Robust estimates of body size in pollinating insects. Ecology and Evolution, 9(4), 1702-1714. \doi{10.1002/ece3.4835}.
#' \item Bullock, S. H. (1999). Relationships among body size, wing size and mass in bees from a tropical dry forest in Mexico. Journal of the Kansas Entomological Society, 426-439.
#' }
#' 
#' @export
wingloading <- function(IT){
  out <- 0.0119 + 0.668 * IT
  out
}
