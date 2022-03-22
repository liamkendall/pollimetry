#' @name tonguelength
#' 
#' @title Converts ITD (mm) to tongue length for bees.
#' 
#' @description Calculates tongue length (mm) from Cariveau et al. (2015) and Melin et al. (2019) using intertegular distance (ITD) values (mm).  
#' 
#' @param x A dataframe with the following two columns: bee intertegular distance (IT) measurements in mm and Family, a vector of bee families. Only implemented 6 out of the extant 7 families: 
#' "Andrenidae", "Apidae", "Colletidae", "Halictidae", "Melittidae" and "Megachilidae".
#' 
#' @param mouthpart The mouthpart you are interested in. Options are: "all", "glossa", "prementum" and "tongue" (i.e. gloss + prementum)
#' 
#' @return A dataframe with bee tongue length (mm) and 95% confidence intervals are returned for each bees species bound to your original dataframe.
#' 
#' @examples
#' example=cbind.data.frame(IT=c(1.3,2.3),
#'                          Family=c("Andrenidae","Apidae"))
#' tonguelength(example,mouthpart="all")
#' 
#' @references \itemize{
#' \item Kendall et al. (2019) Pollinator size and its consequences: Robust estimates of body size in pollinating insects. \emph{Ecology and Evolution}, 9(4), 1702-1714. \url{https://doi.org/10.1002/ece3.4835}.
#' \item Cariveau et al. (2016) The allometry of bee tongue length an its uses in ecology and evolution. \emph{PloS one}, 11(3): e0151482. \url{https://doi.org/10.1371/journal.pone.0151482}.
#' \item Melin et al. (2019) The allometry of proboscis length in Melittidae (Hymenoptera: Apoidae) and an estimate of their foraging distance using museum specimens. \emph{PloS one}, 14(6), e0217839. \url{https://doi.org/10.1371/journal.pone.0217839}.
#' }
#' 
#' @importFrom stats lm
#' 
#' @importFrom plyr rbind.fill
#' 
#' @importFrom dplyr %>% group_by summarize
#' 
#' @export
tonguelength <- function(x, mouthpart = "all"){
  check_family <- x$family %in% c("Andrenidae", "Apidae", 
                                  "Colletidae", "Halictidae", "Megachilidae")
  if(any(check_family == FALSE)){
    stop("family should be one of: 'Andrenidae', 'Apidae', 
         'Colletidae', 'Halictidae', 'Megachilidae'")
  }
  check_mouthpart <- x$mouthpart %in% c("all", "glossa", "prementum", "tongue")
  if(any(check_mouthpart == FALSE)){
    stop("mouthpart should be one of: 'all', glossa', 'prementum', 'tongue'")
  }
  #quiet global variable concerns
  Melin_et_al_2019 <- Family <- genus <- species <- proboscis_length_mm <-  ITD <- glossa_length_mm <- prementum_length_mm <- tongues <- NULL
  
  #load datasets
  #Cariveau et al 2016
  repmis::source_data("https://github.com/ibartomeus/traitbase/raw/master/raw_data/Cariveau_2016.rda", envir = environment())
  
  #Melin et al 2019
  data(Melin_et_al_2019,envir = environment())
  
  #summarise and combine
  mel.dat <- Melin_et_al_2019 %>% 
    group_by(Family, genus, species) %>%
    summarize(mean_tongue_length_mm = mean(proboscis_length_mm), 
              mean_IT_length_mm = mean(ITD),
              mean_glossa_length_mm = mean(glossa_length_mm),
              mean_prementum_length_mm = mean(prementum_length_mm),
              sample_size = length(species)) #average by species, rename the same as Cariveau dataset
  
  tongue.dat <- rbind.fill(tongues,mel.dat) #bind both data frames
  colnames(tongue.dat)[7] ="IT" #rename IT column
  
  #fit models
  proboscis=lm(log(mean_tongue_length_mm)~Family+log(IT),tongue.dat)
  glossa=lm(log(mean_glossa_length_mm)~log(IT)+Family,tongue.dat)
  prementum=lm(log(mean_prementum_length_mm)~log(IT)*Family,tongue.dat)
  
  #print predictions
  if(mouthpart=="all"){
    out<-exp(predict(proboscis,x,interval = c("confidence"),
                     level = 0.95))
    out2<-exp(predict(glossa,x,interval = c("confidence"),
                      level = 0.95))
    out3<-exp(predict(prementum,x,interval = c("confidence"),
                      level = 0.95))
    out=cbind(out,out2,out3)
    colnames(out)=c("Proboscis","P.lwr.CI","P.upr.CI","Glossa","G.lwr.CI","G.upr.CI","Prementum","Pr.lwr.CI","Pr.upr.CI")
    out=cbind(x,out)
    out
  }else{
    if(mouthpart=="glossa"){
      out<-exp(predict(glossa,x,interval = c( "confidence"),
                       level = 0.95))
      colnames(out)=c("Prementum","lwr.CI","upr.CI")
    }
    if(mouthpart=="prementum"){
      out<-exp(predict(prementum,x,interval = c( "confidence"),
                       level = 0.95))
      colnames(out)=c("Prementum","lwr.CI","upr.CI")
    }
    if(mouthpart=="tongue"){
      out<-exp(predict(proboscis,x,interval = c( "confidence"),
                       level = 0.95))
      colnames(out)=c("Proboscis","lwr.CI","upr.CI")
    }
    out=cbind(x,out)
    out
  }
}


