#' @name ITtongue
#' 
#' @title Converts IT measures tongue length for bees.
#' 
#' @description Calculates tongue length (mm) from Cariveau et al. (2015) using intertegular distance values (IT).  
#' 
#' @param IT A vector of bee intertegular spans (IT) measurments in cm.
#' @param family a vector of bee families. Only implemented 5 out of the extant 7 families: 
#' "Andrenidae", "Apidae", "Colletidae", "Halictidae", "Megachilidae".
#' @param mouthpart The mouth part you are interested in. Options are "all", glossa", "prementum" and "tongue" (i.e. gloss + prementum)
#' 
#' @return A dataframe with bee tongue length (mm) is returned for each bees species.
#' 
#' @examples
#' ITtongue(c(10,5,2), family = c("Andrenidae", "Apidae", "Colletidae"))
#' ITtongue(c(10,5,2), family = c("Andrenidae", "Apidae", "Colletidae"), mouthpart = "tongue")
#' @references Cariveau, Nayak, Bartomeus, Zientek, Ascher, Winfree (2015) The allometry of bee tongue length an its uses in ecology and evolution 
#' 
#'
#' @export
ITtongue <- function(IT, family, mouthpart = "all"){
  if(!length(IT) == length(family)){
    stop("IT and family should be the same length")
  }
  check_family <- family %in% c("Andrenidae", "Apidae", 
                                "Colletidae", "Halictidae", "Megachilidae")
  if(any(check_family == FALSE)){
    stop("family should be one of: 'Andrenidae', 'Apidae', 
         'Colletidae', 'Halictidae', 'Megachilidae'")
  }
  check_mouthpart <- mouthpart %in% c("all", "glossa", "prementum", "tongue")
  if(any(check_mouthpart == FALSE)){
    stop("mouthpart should be one of: 'all', glossa', 'prementum', 'tongue'")
  }
  family_intercepts <- data.frame(families = c("Andrenidae", "Apidae", 
                                               "Colletidae", "Halictidae", "Megachilidae"),
                                  intercepts_tongue = c(1.06, 2.13, 0.86, 1.38, 1.87),
                                  intercepts_glossa = c(0.23, 1.27, 0.21, 0.43, 1.16),
                                  intercepts_prementum = c(0.88, 0.91, 0.56, 0.89, 0.77),
                                  slopes_prementum = c(0.83, 0.73, 1.14, 1.04,0.68))
  family_intercepts2 <- merge(data.frame(id = c(1:length(family)), families = family), 
                              family_intercepts)
  family_intercepts2 <- family_intercepts2[order(family_intercepts2$id),]
  tongue <- exp(log(family_intercepts2$intercepts_tongue)
                + 0.96*log(IT))  
  glossa <- exp(log(family_intercepts2$intercepts_glossa)
                + 1.04*log(IT))  
  prementum <- exp(log(family_intercepts2$intercepts_prementum)
                   + family_intercepts2$slopes_prementum*log(IT))
  if (mouthpart == "all") out <- cbind(tongue, glossa, prementum)
  if (mouthpart == "tongue") out <- tongue
  if (mouthpart == "glossa") out <- glossa
  if (mouthpart == "prementum") out <- prementum
  out
  }
