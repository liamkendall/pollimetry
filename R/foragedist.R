#' @name foragedist
#' 
#' @title Converts body weight to measures of foraging distance for bees.
#' 
#' @description Calculates foraging distance from Greenleaf et al. (2007) using intertegular distance (ITD) values,
#'  van Nieuwstadt and Iraheta (1996) using head width (HW) values for Meliponini (stingless bees) and 
#'  Guedot et al. (2009) using dry weight (mg) values for Osmia species.
#'
#' @param x A vector of either bee intertegular spans (IT) measurements in mm, head width values in mm or dry weight values (mg).
#' 
#' @param type The type of foraging distance desired. Options are "GreenleafAll", GrMhd ("Maximum homing distance"),
#' GrThd ("Typical homing distance"), GrMfd ("Maximum feeder training distance"), GrMcd("Maximum communication distance"),"Osmia", "MeliMR" , "MeliFT" or "MeliAll". See details in Greenleaf et al. (2007), Guedot et al. (2009) and van Nieuwstadt and Iraheta (1996). 
#' 
#' @return A dataframe with bee and foraging distance (Km) is returned for each bees species.
#'
#' @examples
#' foragedist(c(10,5,2), type = "MeliMR")
#' 
#' @references \itemize{
#' \item Kendall et al. (2019) Pollinator size and its consequences: Robust estimates of body size in pollinating insects. \emph{Ecology and Evolution}, 9(4), 1702-1714. \url{https://doi.org/10.1002/ece3.4835}.
#' \item Greenleaf et al. (2007) Bee foraging ranges and their relationship to body size. \emph{Oecologia}, 153, 589-596. \url{https://doi.org/10.1007/s00442-007-0752-9}.
#' \item Guedot et al. (2009). Relationship between body size and homing ability in the genus Osmia (Hymenoptera; Megachilidae). \emph{Ecological Entomology}, 34(1), 158-161. \url{https://doi.org/10.1111/j.1365-2311.2008.01054.x}.
#' \item van Nieuwstadt, M. G. L., & Iraheta, C. R. (1996). Relation between size and foraging range in stingless bees (Apidae, Meliponinae). \emph{Apidologie}, 27(4), 219-228.
#' }
#'
#' @export
foragedist <- function(x, type = "GreenleafAll"){
  if(!type %in% c("GreenleafAll", 
                  "GrMhd", 
                  "GrThd",
                  "GrMfd", 
                  "GrMcd","Osmia","MeliMR","MeliFT","MeliAll")) {
    stop("type should be one of 'GreenleafAll', 'GrMhd', 'GrThd', 'GrMfd', 
         'GrMcd','MeliMR','MeliFT','MeliAll','Osmia")
  } else {
    GrMhd  <- 10^((-1.363) + 3.366*log10(x))  
    GrThd  <- 10^((-1.643) + 3.242*log10(x))  
    GrMfd  <- 10^((-0.760) + 2.313*log10(x))  
    GrMcd  <- 10^((-0.993) + 2.788*log10(x))  
    MeliMR <- 560.8*(x) - 808.2
    MeliFT <- 550.9*(x) - 579.1
    Osmia  <- 54.526*(x) - 866.63
    if (type == "GreenleafAll") out <- cbind(GrMhd, GrThd, GrMfd, GrMcd)
    if (type == "GrMhd") out <- GrMhd
    if (type == "GrThd") out <- GrThd
    if (type == "GrMfd") out <- GrMfd
    if (type == "GrMcd") out <- GrMcd
    if (type == "MeliMR") out <- MeliMR
    if (type == "MeliFT") out <- MeliFT
    if (type == "MeliAll") out <- cbind(MeliMR,MeliFT)
    if (type == "Osmia") out <- Osmia
    out
  }
}

