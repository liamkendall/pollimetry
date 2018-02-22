#' @name dipBL
#' 
#' @title Converts body length to Diptera dry body weight (mg)
#' 
#' @description Calculates dry body weight (mg) from existing allometries (See 'Details') using body length values (mm).  
#' 
#' @param BL A vector of Diptera body length (BL) measurments (mm).
#'
#' @param Group a vector of fly group (Suborder/Family).
#'  Only implemented 7 groups: "Diptera",suborders:"Brachycera", "Nematocera",
#'  the infraorder: "Cyclorrapha", and brachyceran families: "Asilidae","Bombyliidae".
#' 
#' @return A dataframe with fly body size (mg) is returned for each species from relevant equations.
#' 
#' @examples
#' ITbodysize(c(10,5,2), Family = c("Diptera", "Cyclorrapha", "Bombyliidae"))
#' @references Kendall, Bartomeus... Rader (20XX) Pollinator size and its consequences: Predictive allometry for pollinating insects 
#' #ADD ALL REFERENCES
#' 
#' ###
#' 
#' @export
dipBL <- function(BL, Equation){
  if(!length(BL) == length(Equation)){
    stop("IT and Group should be the same length")
  }
  check_Equation <- Group %in% c('Diptera', 'Brachycera', 
                                'Nematocera', 'Cyclorrapha','Asilidae','Bombyliidae')
  if(any(check_GROUP == FALSE)){
    stop("Group should be one of: 'Diptera', 'Brachycera', 
                                'Nematocera', 'Cyclorrapha','Asilidae','Bombyliidae'")
  }
  
  out=
    data.frame(matrix(NA, nrow = 1:length(BL), ncol = 20))
  colnames()
  
  data.frame(length of BL,column 1 (number 1:X BL), column 2 GROUP,COLUMN 3:21 each equation )
###IF DIPTERA, return functions BN06D, G97D, GR84D, JS00DA,R77D, 
  #S80DCF,S80DCR,S80DMF,S93DA, W13D
  
  ###IF GROUP = Brachycera, return BN06D, G97D, GR84D, JS00DA,R77D, 
  #S80DCF,S80DCR,S80DMF,S93DA, W13D
  
 + X, X, X
  
#Same length per group - just == NA for non-applicable grouping 
  
  ###IF GROUP = Nematocera return BN06D, G97D, GR84D, JS00DA,R77D, 
  #S80DCF,S80DCR,S80DMF,S93DA, W13D
  
  X, X, X
  
  ###IF GROUP = Cyclorrapha return BN06D, G97D, GR84D, JS00DA,R77D, 
  #S80DCF,S80DCR,S80DMF,S93DA, W13D
  
  X, X, X
  
  ###IF GROUP = Asilidae return BN06D, G97D, GR84D, JS00DA,R77D, 
  #S80DCF,S80DCR,S80DMF,S93DA, W13D X, X, X
  
  ###IF GROUP = Bombyliidae return BN06D, G97D, GR84D, JS00DA,R77D, 
  #S80DCF,S80DCR,S80DMF,S93DA, W13D X, X, X
  
  equations[==c("ALL"),4]  

  out <- exp(log(Family_intercepts2$intercepts_Family)
                  + as.numeric(allo_coefs[allo_coefs$term==c("IT"),][2])*log(IT))  
  out
  }
dipBL()

