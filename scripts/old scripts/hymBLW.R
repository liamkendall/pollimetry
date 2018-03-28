#' @name hymBLW
#' 
#' @title Converts body length*body width to body weight (mg) for ants, bees and/or wasps.
#' 
#' @description Calculates dry body weight (mg) using body length*body width values (mm) from Sage 1993.  
#' 
#' @param BL A vector of ant, bee and/or wasp body length*body width measurments (mm).
#'
#' @param Eq a vector of a predictive allometry for ants, bees and wasps. Options implemented are: S93HBLW (Braconidae), S93HFLW (Formicidae), S93HHLW (NOT SURE), S93HILW (Ichneumonidae), S93HALW (ALL), S93HPLW (Pompilidae), S93HVLW (Vespidae).
#'
#' @return A dataframe with ant, bee and/or wasp body size (mg) is returned for each specimen from selected equation.
#' 
#' @examples
#' hymBLW(BLW=c(10,5,2), Eq = "All")
#' @references Kendall, Bartomeus... Rader (20XX) Pollinator size and its consequences: Predictive allometry for pollinating insects 
#' #ADD Sage
#' 
#' @export
hymBLW <- function(BLW, Eq = "All"){
  if(!Eq %in% c("S93HBLW", 
                "S93HFLW", 
                "S93HHLW",
                "S93HILW",
                "S93HALW",
                "S93HPLW","S93HVLW")) {
    stop("Eq should be one of 'S93HBLW', 'S93HFLW', 'S93HHLW' , 'S93HILW','S93HALW','S93HPLW','S93HVLW'")
  } else {
  S93HBLW <- exp(-2.19+1.445*log(BLW))
  S93HFLW <- exp(-2.378+1.473*log(BLW))
  S93HHLW <- exp(-1.946+1.444*log(BLW))
  S93HILW <- exp(-2.497+1.445*log(BLW))
  S93HALW <- exp(-2.375+1.456*log(BLW))
  S93HPLW <-  exp(-1.946+1.444*log(BLW))
  S93HVLW <-exp(-1.537+1.319*log(BLW))
  if (Eq == "S93HBLW") out <- S93HBLW
  if (Eq == "S93HFLW") out <- S93HFLW
  if (Eq == "S93HHLW") out <- S93HHLW
  if (Eq == "S93HILW") out <- S93HILW
  if (Eq == "S93HALW") out <- S93HALW
  if (Eq == "S93HPLW") out <- S93HPLW
  if (Eq == "S93HVLW") out <- S93HVLW
out
}
}