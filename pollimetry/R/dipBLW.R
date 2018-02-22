#' @name dipBLW
#' 
#' @title Converts fly body length*body width to body weight (mg).
#' 
#' @description Calculates dry body weight (mg) from the allometries of Sage (1983) allometries (See 'Details') using body length*body width values (mm).  
#' 
#' @param BL A vector of fly body length*body width measurments (mm).
#'
#' @param Eq a vector of a predictive allometry for flies. Options implemented are: S93DBLW, S93DCLW, S93DALW and S93DNLW.
#'
#' @return A dataframe with fly body size (mg) is returned for each specimen from selected equation.
#' 
#' @examples
#' dipBLW(BLW=c(10,5,2), Eq = "S93DBLW")
#' @references Kendall, Bartomeus... Rader (20XX) Pollinator size and its consequences: Predictive allometry for pollinating insects 
#' #ADD Sage
#' 
#' @export
dipBLW <- function(BLW, Eq = "All"){
if(!Eq %in% c("Brachycera", 
                "Cyclorrapha", 
                "All",
                "Nematocera")) {
  stop("Eq should be one of 'Brachycera', 'Cyclorrapha', 'Nematocera' or 'All'")
} else {
  Brachycera <- exp(-2.2+1.259*log(BLW))
  Cyclorrapha <- exp(-2.02+1.298*log(BLW))  
  Sage_All <- exp(-2.197+1.309*log(BLW))
  Nematocera <- exp(-2.217+1.288*log(BLW))
  if (Eq == "Brachycera") out <- Brachycera
  if (Eq == "Cyclorrapha") out <- Cyclorrapha
  if (Eq == "All") out <- Sage_All
  if (Eq == "Nematocera") out <- Nematocera
  out
  }
}

  