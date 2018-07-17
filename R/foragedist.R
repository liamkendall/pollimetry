#' @name foragedist
#' 
#' @title Converts body weight to measures of foraging distance for bees.
#' 
#' @description Calculates foraging distance from Greenleaf et al. (2007) (using intertegular distance values (IT)),
#'  van Nieuwstadt and Iraheta (1996) using head width values for Meliponini (stingless bees) and 
#'  Guedot et al. (2009) using dry weight values (mg) for Osmia species.
#'
#' @param x A vector of either bee intertegular spans (IT) measurements in cm, head width values in mm or dry weight values (mg).
#' 
#' @param Type The type of foraging distance desired. Options are "GrAll", GrMhd ("Maximum homing distance"),
#' GrThd ("Typical homing distance"), GrMfd ("Maximum feeder training distance"), GrMcd("Maximum communication distance"),"Osmia", "MeliMR" , "MeliFT" or "MeliAll". See details in Greenleaf et al. (2007), Guedot et al. (2009) and van Nieuwstadt and Iraheta (1996). 
#' 
#' @return A dataframe with bee and foraging distance (Km) is returned for each bees species.
#'
#' @examples
#' foragedist(c(10,5,2), type = "MeliMR") 
#' 
#' @references Greenleaf, S.S., Williams, N.M., Winfree, R. & Kremen, C. (2007) Bee foraging ranges and their relationship to body size. Oecologia, 153, 589-596. Guedot, C., Bosch, J., & Kemp, W. P. (2009). Relationship between body size and homing ability in the genus Osmia (Hymenoptera; Megachilidae). Ecological Entomology, 34(1), 158-161. Van Nieuwstadt, M. G. L., & Iraheta, C. R. (1996). Relation between size and foraging range in stingless bees (Apidae, Meliponinae). Apidologie, 27(4), 219-228.
#'
#' @export
foragedist <- function(x, Type = "GreenleafAll"){
  if(!Type %in% c("GreenleafAll", 
                  "GrMhd", 
                  "GrThd",
                  "GrMfd", 
                  "GrMcd","Osmia","MeliMR","MeliFT","MeliAll")) {
    stop("Type should be one of 'GreenleafAll', 'GrMhd', 'GrThd', 'GrMfd', 
         'GrMcd','MeliMR','MeliFT','MeliAll','Osmia")
  } else {
    GrMhd  <- 10^((-1.363) + 3.366*log10(x))  
    GrThd  <- 10^((-1.643) + 3.242*log10(x))  
    GrMfd  <- 10^((-0.760) + 2.313*log10(x))  
    GrMcd  <- 10^((-0.993) + 2.788*log10(x))  
    MeliMR <- 560.8*(x) - 808.2
    MeliFT <- 550.9*(x) - 579.1
    Osmia  <- 54.526*(x) - 866.63
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

