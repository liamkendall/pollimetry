#' @name SizeForage
#' 
#' @title Converts body weight to measures of foraging distance for bees.
#' 
#' @description Calculates foraging distance from Greenleaf et al. (2007) (using intertegular distance values (IT)),
#'  van Nieuwstadt and Iraheta (1996) using head width values for Meliponini (stingless bees) and 
#'  Guédot et al. (2009) using dry weight values (mg) for Osmia species.
#'
#' @param BW A vector of either bee intertegular spans (IT) measurments in cm, head width values in mm or dry weight values (mg).
#' 
#' @param Type The type of foraging distance desired. Options are "GrAll", GrMhd ("Maximum homing distance"),
#' GrThd ("Typical homing distance"), GrMfd ("Maximum feeder training distance"), GrMcd("Maximum communication distance"),"Osmia", "MeliMR" , "MeliFT" or "MeliAll". See details in Greenleaf et al. (2007), Guédot et al. (2009) and van Nieuwstadt and Iraheta (1996) and 
#' 
#' @return A dataframe with bee and foraging distance (Km) 
#' is returned for each bees species.
#' mhd: Maximum homing distance
#' thd: Typical homing distance
#' mfd: Maximum feeder training distance
#' mcd: Maximum communication distance
#' MeliMR: Meliponini mark-recapture 95% return distance
#' MeliFT: Meliponini feeder training 95% foraging distance
#'
#' @examples
#' SizeForage(c(10,5,2))
#' SizeForage(c(10,5,2), type = "MeliMR") 
#' 
#' @references Greenleaf, S.S., Williams, N.M., Winfree, R. & Kremen, C. (2007) Bee foraging ranges and their relationship to body size. Oecologia, 153, 589-596.
#'
#' @export
SizeForage <- function(BW, Type = "GreenleafAll"){
  if(!Type %in% c("GreenleafAll", 
                  "GrMhd", 
                  "GrThd",
                  "GrMfd", 
                  "GrMcd","Osmia","MeliMR","MeliFT","MeliAll")) {
    stop("Type should be one of 'GreenleafAll', 'GrMhd', 'GrThd', 'GrMfd', 
         'GrMcd','Osmia",'MeliMR','MeliFT','MeliAll')
  } else {
    GrMhd  <- 10^((-1.363) + 3.366*log10(BW))  
    GrThd  <- 10^((-1.643) + 3.242*log10(BW))  
    GrMfd  <- 10^((-0.760) + 2.313*log10(BW))  
    GrMcd  <- 10^((-0.993) + 2.788*log10(BW))  
    MeliMR <- 560.8*(BW) - 808.2
    MeliFT <- 550.9*(BW) - 579.1
    Osmia  <- 54.526*(BW) - 866.63
    if (Type == "GreenleafAll") out <- cbind(GrMhd, GrThd, GrMfd, GrMcd)
    if (Type == "GrMhd") out <- GrMhd
    if (Type == "GrThd") out <- GrThd
    if (Type == "GrMfd") out <- GrMfd
    if (Type == "GrMcd") out <- GrMcd
    if (Type == "MeliMR") out <- MeliMR
    if (Type == "MeliFT") out <- MeliFT
    if (Type == "MeliAll") out <- cbind(MeliMR,MeliFT)
    if (Type == "Osmia") out <- Osmia
    out
  }
}
plot(SizeForage(bee_all$IT,Type="GreenleafAll")[[1]]~bee_all$Spec.wgt)
