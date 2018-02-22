#' @name lepBLW
#' 
#' @title Converts body length*body width to body weight (mg) for moths and/or butterflies.
#' 
#' @description Calculates dry body weight (mg) using body length*body width values (mm).  
#' 
#' @param BL A vector of fly body length*body width measurments (mm).
#'
#' @param Eq a vector of a predictive allometry for moths and/or butterflies Options implemented are:W13LLW (All), , S93LALW (All), S93LGLW (Geometridae), S93LMLW (Microlepidoptera), S93LCLW (Noctuoidea/Arctiinae) and S93LNLW (Noctuoidea).
#'
#' @return A dataframe with moth and/or butterfly body size (mg) is returned for each specimen from selected equation.
#' 
#' @examples
#' dipBLW(BLW=c(10,5,2), Eq = "S93DBLW")
#' @references Kendall, Bartomeus... Rader (20XX) Pollinator size and its consequences: Predictive allometry for pollinating insects 
#' #ADD Sage
#' 
#' @export
lepBLW <- function(BLW, Eq = "All"){
  if(!Eq %in% c("S93LGLW", 
                "S93LMLW", 
                "S93LALW",
                "W13LLW",
                "S93LCLW",
                "S93LNLW","All")) {
    stop("Eq should be one of 'S93LGLW', 'S93LMLW', 'S93LALW' , 'W13LLW','S93LCLW','S93LNLW','All")
  } else {
    S93LGLW <- exp(-2.343+1.387*log(BLW))}
    S93LMLW <- exp(-2.715+1.395*log(BLW))}
    S93LALW <- exp(-2.607+1.457*log(BLW))}
    W13LLW <- exp(-2.1+1.37*log(BLW))}
    S93LCLW <- exp(-1.658+1.222*log(BLW))}
    S93LNLW <-  exp(-1.607+1.214*log(BLW))}
    if (Eq == "S93LGLW") out <- Brachycera
    if (Eq == "S93LMLW") out <- Cyclorrapha
    if (Eq == "S93LALW") out <- Sage_All
    if (Eq == "W13LLW") out <- Nematocera
    if (Eq == "W13LLW") out <- Nematocera
    if (Eq == "W13LLW") out <- Nematocera
    if (Eq == "All") out <- cbind(W13LLW,S93LALW)
    out
  }
  }