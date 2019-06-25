#' @name nectarload
#' 
#' @title Converts bee body length (mm) to total field nectar load (ul).
#' 
#' @description Calculates total field nectar load (ul) using body length (mm) from Henry & Rodet (2008).
#' 
#' @param BL vector of body length measurements (mm).
#'
#' @return A dataframe with bee total field nectar load (ul) is returned for each specimen.
#' 
#' @examples
#' nectarload(BL=c(10,5,2))
#' @references \itemize{
#' \item Kendall et al. (2019) Pollinator size and its consequences: Robust estimates of body size in pollinating insects. Ecology and Evolution, 9(4), 1702-1714. \url{https://doi.org/10.1002/ece3.4835}.
#' \item Henry, M., & Rodet, G. (2018). Controlling the impact of the managed honeybee on wild bees in protected areas. Scientific reports, 8(1), 9308. \url{https://doi.org/10.1038/s41598-018-27591-y}.
#' }
#' @export
nectarload <- function(BL){
    out <- 0.005 * BL^3.0618
    out
  }
