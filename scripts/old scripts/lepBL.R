#' @name lepBL
#' 
#' @title Converts body length to body weight (mg) for moths and butterflies.
#' 
#' @description Calculates dry body weight (mg) from existing allometries (See 'Details') using body length values (mm).  
#' 
#' @param BL A vector of moth and/or butterfly body length (BL) measurements (mm).
#'
#' @param Eq a vector of a predictive allometry for moths and/or butterflies. Options implemented are (ordered by year released):
#' S93LG (Geometridae),S93LM (Microlepidoptera), BN06L, G97F, G97L, JS00L, R77L, S80LCF, S80LCR, S80LMF, S93LA, W13L, S93LC (Arctiinae), S93LN (Noctuoidea), All
#' 
#' @return A dataframe with moth and/or butterfly body size (mg) is returned for each specimen from selected equation.
#' 
#' @examples
#' lepBL(BL=c(10,5,2), Eq = "S93BD")
#' @references Kendall, Bartomeus... Rader (20XX) Pollinator size and its consequences: Predictive allometry for pollinating insects 
#' #ADD ALL REFERENCES
#' 
#' @export
lepBL <- function(BL, Eq = "S93LG"){
  if(!Eq %in% c("S93LG","S93LM", "BN06L", "G97L", 
                "JS00L", "R77L", "S80LCF", "S80LCR", "S80LMF", 
                "S93LA", "W13L", "S93LC", "S93LN", "All")){
    stop("Equation should be one of 'S93LG','S93LM', 'BN06L', 'G97L',
         'JS00L', 'R77L', 'S80LCF', 'S80LCR', 'S80LMF', 'S93LA', 
         'W13L', 'S93LC', 'S93LN'")
  } else {
    S93LG <- exp(-4.172+2.628*log(BL))
    S93LM <- exp(-4.913+2.918*log(BL))
    BN06L <- exp(log(0.001)+2.313*log(BL))
    G97L <- exp(-4.7915+2.8585*log(BL))
    JS00L <- exp(-3.268+2.243*log(BL))
  R77L <- exp(-4.037+2.903*log(BL))
  S80LCF <- exp(log(0.026)+2.5*log(BL))
  S80LCR <- exp(log(0.078)+1.32*log(BL))
  S80LMF <- exp(log(0.014)+2.55*log(BL))
  S93LA <- exp(-5.036+3.122*log(BL))
  W13L <- exp(-3.83+2.77*log(BL))
  S93LC <- exp(-3.755+2.658*log(BL))
  S93LN <- exp(-3.337+2.499*log(BL))

  if (Eq ==  "S93LG") out <- S93LG
  if (Eq == "S93LM") out <- S93LM
  if (Eq == "BN06L") out <- BN06L
  if (Eq == "G97L") out <- G97L
  if (Eq == "JS00L") out <- JS00L
  if (Eq == "R77L") out <- R77L
  if (Eq == "S80LCF") out <- S80LCF
  if (Eq == "S80LCR") out <- S80LCR
  if (Eq == "S80LMF") out <- S80LMF
  if (Eq == "S93LA") out <- S93LA
  if (Eq == "W13L") out <- W13L
  if (Eq == "S93LC") out <- S93LC
  if (Eq == "S93LN") out <- S93LN
  if (Eq == "ALL") out <- cbind(BN06L,G97L,JS00L,R77L,S80LCF, S80LCR,S80LMF,S93LA,W13L)
  if (Eq == "Noctuidea") out <- cbind(S93LC, S93LN)
  out
}
}
