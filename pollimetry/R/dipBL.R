#' @name dipBL
#' 
#' @title Converts fly body length to body weight (mg).
#' 
#' @description Calculates dry body weight (mg) from existing allometries (See 'Details') using body length values (mm).  
#' 
#' @param BL A vector of Diptera body length (BL) measurments (mm).
#'
#' @param Eq a vector of a predictive allometry for flies. Options implemented are (ordered by year released): R77D = Rogers 1977,
#' S93DB, Sabo02DB, Sabo02DA, Sabo02DBB, S93DC, BN06D, G97D, GR84D, JS00DA, R77D, S80DCF, S80DCR, S80DMF, S93DA, W13D, JS00DN, S93DN, Sabo02DN
#'
#' 
#' @return A dataframe with fly body size (mg) is returned for each species from selected equation.
#' 
#' @examples
#' dipBL(BL=c(10,5,2), Eq = "S93BD")
#' @references Kendall, Bartomeus... Rader (20XX) Pollinator size and its consequences: Predictive allometry for pollinating insects 
#' #ADD ALL REFERENCES
#' 
#' ###
#' 
#' @export
dipBL <- function(BL, Eq = "S93DB"){
  if(!Eq %in% c("S93DB","Sabo02DB", "Sabo02DA", "Sabo02DBB", "R77D",
                            "S93DC", "BN06D", "G97D", "GR84D", "JS00DA",
                "S80DCF", "S80DCR", "S80DMF", "S93DA", "W13D", "JS00DN", 
                "S93DN", "Sabo02DN")){
    stop("Equation should be one of 'R77D',
 'S93DB', 'Sabo02DB', 'Sabo02DA', 'Sabo02DBB', 
 'S93DC', 'BN06D', 'G97D', 'GR84D', 'JS00DA', 
'S80DCF', 'S80DCR', 'S80DMF', 'S93DA', 'W13D', 'JS00DN', 
 'S93DN', 'Sabo02DN'")
 } else {
      S93DB <- exp(-3.374+2.158*log(BL))
      Sabo02DB <- 0.006*(BL)^3.05
      Sabo02DA <- 0.38*(BL)^1.5
      Sabo02DBB <- 0.007*(BL)^3.337
      S93DC <- exp(-3.619+2.632*log(BL))
      BN06D <- -0.041+0.010*(BL)}
      G97D <- exp(-3.4294+2.5943*log(BL))
      GR84D <- exp(-3.653+log(BL)*2.546)
      JS00DA <- exp(-2.462+1.881*log(BL))
      R77D <- exp(-3.293+2.366*log(BL))
      S80DCF <- exp(log(0.074)+1.64*log(BL))
      S80DCR <- exp(log(0.068)+1.59*log(BL))
      S80DMF <- exp(log(0.022)+2.42*log(BL))
      S93DA <- exp(-3.184+2.23*log(BL))
      W13D <- exp(-3.29+2.65*log(BL))
      JS00DN <- exp(-2.462+1.881*log(BL))
      S93DN <- exp(-3.675+2.212*log(BL))
      Sabo02DN <- 0.1*(BL)^1.57
      if (Eq ==  "S93DB") out <- S93DB
      if (Eq == "Sabo02DB") out <- Sabo02DB
      if (Eq == "Sabo02DA") out <- Sabo02DA
      if (Eq == "Sabo02DBB") out <- Sabo02DBB
      if (Eq == "S93DC") out <- S93DC
      if (Eq == "BN06D") out <- BN06D
      if (Eq == "G97D") out <- G97D
      if (Eq == "GR84D") out <- GR84D
      if (Eq == "JS00DA") out <- JS00DA
      if (Eq == "R77D") out <- R77D
      if (Eq == "S80DCF") out <- S80DCF
      if (Eq == "S80DCR") out <- S80DCR
      if (Eq == "S80DMF") out <- S80DMF
      if (Eq == "S93DA") out <- S93DA
      if (Eq == "W13D") out <- W13D
      if (Eq == "JS00DN") out <- JS00DN
      if (Eq == "S93DN") out <- S93DN
      if (Eq == "Sabo02DN") out <- Sabo02DN
      out
    }

