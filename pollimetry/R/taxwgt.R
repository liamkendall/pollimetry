#' @name taxwgt
#' 
#' @title Pollinator dry weight estimates based on taxonomy.
#' 
#' @description Calculates dry body weight (mg) from Kendall et al. (XXXX) using known weights.  
#' 
#' @param IT A vector of bee intertegular spans (IT) measurments (mm).
#' 
#' @param Family a vector of bee families. Only implemented 6 out of the extant 7 families: 
#' "Andrenidae", "Apidae", "Colletidae", "Halictidae", "Megachilidae", "Melittidae".
#' 
#' @param Region a vector of biogeographic region. Only implemented 6 out of the extant 7 families: 
#' "Andrenidae", "Apidae", "Colletidae", "Halictidae", "Megachilidae", "Melittidae".
#' 
#' @param Sex a vector of bee gender. Only implemented 6 out of the extant 7 families: 
#' "Andrenidae", "Apidae", "Colletidae", "Halictidae", "Megachilidae", "Melittidae".
#' 
#' @return A dataframe with bee body size (mg) is returned for each species.
#' 
#' @examples
#' ITsize(IT=c(10,5,2),Family=c("Apidae","Apidae","Melittidae",Region=c("Europe","Europe","Europe"),sex=c("F","M","F"))
#' @references Kendall, Bartomeus... Rader (20XX) Pollinator size and its consequences: Predictive allometry for pollinating insects 
#' 
#' @export
ITsize <- function(IT){
  if(!length(IT) == length(Family)){
    stop("IT and Family should be the same length")
  }
  check_Family <- Family %in% c("Andrenidae", "Apidae", 
                                "Colletidae", "Halictidae", "Megachilidae","Melittidae")
  if(any(check_Family == FALSE)){
    stop("Family should be one of: 'Andrenidae', 'Apidae', 
         'Colletidae', 'Halictidae', 'Megachilidae','Melittidae'")
  }
  if(!length(IT) == length(Region)){
    stop("IT and Region should be the same length")
  }
  check_Region <- Region %in% c("Andrenidae", "Apidae", 
                                "Colletidae", "Halictidae", "Megachilidae","Melittidae")
  if(any(check_Region == FALSE)){
    stop("Family should be one of: 'Andrenidae', 'Apidae', 
         'Colletidae', 'Halictidae', 'Megachilidae','Melittidae'")
  }
  if(!length(IT) == length(Family)){
    stop("IT and Family should be the same length")
  }
  check_Sex <- Sex %in% c("Andrenidae", "Apidae", 
                          "Colletidae", "Halictidae", "Megachilidae","Melittidae")
  if(any(check_Sex == FALSE)){
    stop("Family should be one of: 'Andrenidae', 'Apidae', 
         'Colletidae', 'Halictidae', 'Megachilidae','Melittidae'")
  }
  
  bee_geog_lme=lmer(log(Spec.wgt) ~ log(IT)  + Family + Sex + Region + #fix factors
                      log(IT):Family + log(IT):Region + log(IT):Sex + #interactions
                      (1|Measurement),REML=FALSE,bee_mean)
  geog_coefs=tidy(bee_geog_lme)
  Family_intercepts <- data.frame(Family = c("Andrenidae", "Apidae", "Colletidae", "Halictidae"
                                             , "Megachilidae","Melittidae"),
                                  intercepts_Family = c(0,
                                                        as.numeric(geog_coefs[geog_coefs$term==c("FamilyApidae"),][2]),
                                                        as.numeric(geog_coefs[geog_coefs$term==c("FamilyColletidae"),][2]),
                                                        as.numeric(geog_coefs[geog_coefs$term==c("FamilyHalictidae"),][2]),
                                                        as.numeric(geog_coefs[geog_coefs$term==c("FamilyMegachilidae"),][2]),
                                                        as.numeric(geog_coefs[geog_coefs$term==c("FamilyMelittidae"),][2])))
  Family_intercepts2 <- merge(data.frame(id = c(1:length(Family)), Families = Family), 
                              Family_intercepts)
  Family_intercepts2 <- Family_intercepts2[order(Family_intercepts2$id),]
  
  ##############
  Region_intercepts <- data.frame(Region = c("Europe", "Australia", "NorthAmerica"),
                                  intercepts_Region = c(
                                    as.numeric(geog_coefs[geog_coefs$term==c("(Intercept)"),][2]),
                                    as.numeric(geog_coefs[geog_coefs$term==c("FamilyApidae"),][2]),
                                    as.numeric(geog_coefs[geog_coefs$term==c("FamilyColletidae"),][2]),
                                    as.numeric(geog_coefs[geog_coefs$term==c("FamilyHalictidae"),][2]),
                                    as.numeric(geog_coefs[allo_coefs$term==c("FamilyMegachilidae"),][2]),
                                    as.numeric(allo_coefs[allo_coefs$term==c("FamilyMelittidae"),][2])))
  Family_intercepts2 <- merge(data.frame(id = c(1:length(Family)), Families = Family), 
                              Family_intercepts)
  Family_intercepts2 <- Family_intercepts2[order(Family_intercepts2$id),]
  
  
  
  bodysize <- exp(log(Family_intercepts2$intercepts_Family)
                  + as.numeric(allo_coefs[allo_coefs$term==c("IT"),][2])*log(IT))  
  bodysize
  }
#ITbodysize(IT = exp(bee[1:3,c("IT")]), Family = bee[1:3,c("Family")])

