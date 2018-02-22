#' @name hymBL
#' 
#' @title Converts body length to body weight (mg) for ants, bees and wasps.
#' 
#' @description Calculates dry body weight (mg) from existing allometries (See 'Details') using body length values (mm).  
#' 
#' @param BL A vector of moth/butterfly body length (BL) measurments (mm).
#'
#' @param Eq a vector of a predictive allometry for ants, bees and wasps. 
#' Options implemented are (ordered by year released): Sabo02HA (Apidae), S93HB (Braconidae), BN06HF (Formicidae), GR84F (Formicidae), JS00HF (Formicidae),
#'  R77A (Formicidae), S80FCF (Formicidae), S80FCR (Formicidae), S80FMF (Formicidae), S93HF (Formicidae), S93HH (NOT SURE), S93HI (Ichneumonidae), 
#'  BN06H1, G97H, G97F (Formicidae), GR84H, JS00HA, R77H, S80HCF, 
#'  S80HCR, S80HMF, S93HA, Sabo02H, W13H, S93HP (Pompilidae), S93HV (Vespidae), Sabo02HV (Vespidae), All (12 equations: BN06H1, G97H, G97F, GR84H, JS00HA, R77H, S80HCF, S80HCR, S80HMF, S93HA, Sabo02H, W13H)
#'  and Formicidae (BN06HF, GR84F, JS00HF, R77A, S80FCF, S80FCR, S80FMF, S93HF, S93HH, S93HI, BN06H1, G97H, G97F).
#'  
#' @return A dataframe with ant, bee and/or wasp body size (mg) is returned for each specimen from selected equation.
#' 
#' @examples
#' hymBL(BL=c(10,5,2), Eq = "S93BD")
#' @references Kendall, Bartomeus... Rader (20XX) Pollinator size and its consequences: Predictive allometry for pollinating insects 
#' #ADD ALL REFERENCES
#' 
#' @export
hymBL <- function(BL, Eq = "S93HB"){
  if(!Eq %in% c("Sabo02HA", "S93HB", "BN06HF", "GR84F", "JS00HF",
"R77A", "S80FCF", "S80FCR", "S80FMF", "S93HF", "S93HH", "S93HI", 
"BN06H1", "G97H", "G97F", "GR84H", "JS00HA", "R77H", "S80HCF", 
"S80HCR", "S80HMF", "S93HA", "Sabo02H",
"W13H", "S93HP", "S93HV", "Sabo02HV","All","Formicidae","Vespidae")){
    stop("Equation should be one of 'Sabo02HA', 'S93HB', 'BN06HF',
'GR84F', 'JS00HF','R77A', 'S80FCF', 'S80FCR', 'S80FMF', 'S93HF', 
'S93HH', 'S93HI', 'BN06H1', 'G97H', 'G97F', 'GR84H', 'JS00HA',
'R77H', 'S80HCF', 'S80HCR', 'S80HMF', 'S93HA', 'Sabo02H', 
         'W13H', 'S93HP', 'S93HV', 'Sabo02HV','All','Formicidae','Vespidae'")
  } else {
    Sabo02HA <- 0.006*(BL)^3.407
    S93HB <- exp(-3.854+2.441*log(BL))
    BN06HF <- exp(log(0.001)+2.33*log(BL))
    GR84F <- exp(-3.997+log(BL)*2.489)
    JS00HF <- exp(-3.730+2.103*log(BL))
    R77A <- exp(-4.029+2.572*log(BL))
    S80FCF <- exp(log(0.012)+2.72*log(BL))
    S80FCR <- exp(log(0.021)+2.31*log(BL))
    S80FMF <- exp(log(0.034)+2.19*log(BL))
    S93HF <- exp(-4.727+2.919*log(BL))
    S93HH <- exp(-2.891+2.302*log(BL))
    S93HI <- exp(-4.149+2.464*log(BL))
    BN06H1 <- exp(log(6.783)+2.544*log(BL))
    G97H <- exp(-3.5917+2.6429*log(BL))
    G97F <- exp(-3.1415+2.3447*log(BL))
    GR84H <- exp(-2.86+(BL)*0.478)
    JS00HA <- exp(-3.556+2.193*log(BL))
    R77H <- exp(-3.871+2.407*log(BL))
    S80HCF <- exp(log(0.043)+2.07*log(BL))
    S80HCR <- exp(log(0.022)+2.29*log(BL))
    S80HMF <- exp(log(0.016)+2.55*log(BL))
    S93HA <- exp(-4.284+2.696*log(BL))
    Sabo02H <- 0.56*(BL)^1.56
    W13H <- exp(-4.3+3*log(BL))
    S93HP <- exp(-2.341+2.006*log(BL))
    S93HV <- exp(-3.54+2.782*log(BL))
    Sabo02HV <- 0.001*(BL)^3.723
    if (Eq ==  "Sabo02HA") out <- Sabo02HA
    if (Eq == "S93HB") out <- S93HB
    if (Eq == "BN06HF") out <- BN06HF
    if (Eq == "GR84F") out <- GR84F
    if (Eq == "JS00HF") out <- JS00HF
    if (Eq == "R77A") out <- R77A
    if (Eq == "S80FCF") out <- S80FCF
    if (Eq == "S80FCR") out <- S80FCR
    if (Eq == "S80FMF") out <- S80FMF
    if (Eq == "S93HF") out <- S93HF
    if (Eq == "S93HH") out <- S93HH
    if (Eq == "S93HI") out <- S93HI
    if (Eq == "BN06H1") out <- BN06H1
    if (Eq == "G97H") out <- G97H
    if (Eq == "G97F") out <- G97F
    if (Eq == "GR84H") out <- GR84H
    if (Eq == "JS00HA") out <- JS00HA
    if (Eq == "R77H") out <- R77H
    if (Eq == "S80HCF") out <- S80HCF
    if (Eq == "S80HCR") out <- S80HCR
    if (Eq == "S80HMF") out <- S80HMF
    if (Eq == "S93HA") out <- S93HA
    if (Eq == "Sabo02H") out <- Sabo02H
    if (Eq == "W13H") out <- W13H
    if (Eq == "S93HP") out <- S93HP
    if (Eq == "S93HV") out <- S93HV
    if (Eq == "Sabo02HV") out <- Sabo02HV
    if (Eq == "Vespidae") out <- cbind(S93HV,Sabo02HV)
    if (Eq == "Formicidae") out <- cbind(BN06HF,GR84F,JS00HF,R77A,S80FCF,S80FCR,S80FMF,S93HF,S93HH,S93HI,BN06H1,G97H,G97F)
    if (Eq == "All") out = cbind(BN06H1,G97H, G97F,GR84H,JS00HA,R77H,S80HCF,S80HCR,S80HMF,S93HA,Sabo02H,W13H)
    out
}
}
  