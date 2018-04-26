#' @name meanIT
#' 
#' @title Converts IT to body size in bees using species mean dataframe.
#' 
#' @description Calculates dry body weight (mg) from Kendall et al. (XXXX) using intertegular distance values (IT).  
#' 
#' @param IT A vector of bee intertegular spans (IT) measurements (mm).
#'
#' @return A dataframe with bee body size (mg) is returned for each species.
#' 
#' @examples
#' meanIT(IT=c(10,5,2))
#' @references Kendall, Bartomeus... Rader (20XX) Pollimetry: Pollinator size and its consequences 
#' 
#' @export
meanIT <- function(IT){
  
  mean_model=lmer(log(Spec.wgt)~log(IT)+(1|Measurement),bee_mean)
  mean_coefs=tidy(mean_model)
                  bodysize=exp(mean_coefs[1,2]+log(IT)*mean_coefs[2,2])
                  bodysize
}
meanIT(IT=2)
