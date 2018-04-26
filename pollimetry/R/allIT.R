#' @name allIT
#' 
#' @title Converts IT to body size in bees using predictive MEM.
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
allIT <- function(IT){
  all_model=lmer(log(Spec.wgt)~log(IT)+(1|Measurement),bee_all)
  all_coefs=tidy(all_model)
  bodysize=exp(all_coefs[1,2]+log(IT)*all_coefs[2,2])
  bodysize
}
allIT(IT=2)

all_coefs
mean_coefs
Cane(1)
r.squaredGLMM(lmer(log(Spec.wgt)~log(IT)+(1|Measurement),bee_all))
