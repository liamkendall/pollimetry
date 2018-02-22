#' @name opal
#' 
#' @title Converts body length to pollinator body size in.
#' 
#' @description Calculates dry body weight (mg) from Kendall et al. (XXXX) using body length values (mm).  
#' 
#' @param BL A vector of pollinator body length (BL) measurments (mm).
#' 
#' @param Taxa A vector of pollinator taxa. Currently implements: "Diptera","Hymenoptera","Lepidoptera".
#' 
#' 
#' @param Family a vector of bee families. Only implemented 6 out of the extant 7 families: 
#' "Andrenidae", "Apidae", "Colletidae", "Halictidae", "Megachilidae", "Melittidae". 
#' "NA" exports all models for
#' 
#' @return A dataframe with bee body size (mg) is returned for each species.
#' 
#' @examples
#' ITbodysize(c(10,5,2), Family = c("Andrenidae", "Apidae", "Colletidae"))
#' @references Kendall, Bartomeus... Rader (20XX) Pollinator size and its consequences: Predictive allometry for pollinating insects 
#' 
#' @export
ITbodysize <- function(IT, Family){
  if(!length(IT) == length(Family)){
    stop("IT and Family should be the same length")
  }
  check_Family <- Family %in% c("Andrenidae", "Apidae", 
                                "Colletidae", "Halictidae", "Megachilidae","Melittidae")
  if(any(check_Family == FALSE)){
    stop("Family should be one of: 'Andrenidae', 'Apidae', 
         'Colletidae', 'Halictidae', 'Megachilidae','Melittidae'")
  }
  Family_intercepts <- data.frame(Family = c("Andrenidae", "Apidae", "Colletidae", "Halictidae"
                                             , "Megachilidae","Melittidae"),
                                  intercepts_Family = c(
                                    as.numeric(allo_coefs[allo_coefs$term==c("(Intercept)"),][2]),
                                    as.numeric(allo_coefs[allo_coefs$term==c("FamilyApidae"),][2]),
                                    as.numeric(allo_coefs[allo_coefs$term==c("FamilyColletidae"),][2]),
                                    as.numeric(allo_coefs[allo_coefs$term==c("FamilyHalictidae"),][2]),
                                    as.numeric(allo_coefs[allo_coefs$term==c("FamilyMegachilidae"),][2]),
                                    as.numeric(allo_coefs[allo_coefs$term==c("FamilyMelittidae"),][2])))
  Family_intercepts2 <- merge(data.frame(id = c(1:length(Family)), Families = Family), 
                              Family_intercepts)
  Family_intercepts2 <- Family_intercepts2[order(Family_intercepts2$id),]
  
  bodysize <- exp(log(Family_intercepts2$intercepts_Family)
                  + as.numeric(allo_coefs[allo_coefs$term==c("IT"),][2])*log(IT))  
  bodysize
  }
ITbodysize(IT = exp(bee[1:3,c("IT")]), Family = bee[1:3,c("Family")])

