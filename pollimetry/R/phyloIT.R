#' @name phyloIT
#' 
#' @title Converts IT to body size in bees using predictive PGLS model.
#' 
#' @description Calculates dry body weight (mg) from Kendall et al. (XXXX) using intertegular distance values (IT).  
#' 
#' @param IT A vector of bee intertegular spans (IT) measurements (mm).
#'
#' @return A dataframe with bee body size (mg) is returned for each species.
#' 
#' @examples
#' allIT(IT=c(10,5,2))
#' @references Kendall, Bartomeus... Rader (20XX) Pollinator size and its consequences: Predictive allometry for pollinating insects 
#' 
#' @export
phyloIT <- function(IT){
            size=exp(Bee_PGLS1$coefficients[1]+log(IT)*Bee_PGLS1$coefficients[2])
            as.numeric(size)
}
