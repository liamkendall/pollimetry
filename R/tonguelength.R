#' @name tonguelength
#'
#' @title Converts ITD (mm) to tongue length for bees.
#'
#' @description Calculates tongue length (mm) from Cariveau et al. (2016) and Melin et al. (2019) using intertegular distance (ITD) values (mm).
#'
#' @param x A dataframe with the following two columns: IT (finite, positive intertegular distance in mm) and Family (capital F). Missing measurements are not accepted. Only six out of the extant seven families are implemented:
#' "Andrenidae", "Apidae", "Colletidae", "Halictidae", "Melittidae" and "Megachilidae".
#'
#' @param mouthpart The mouthpart you are interested in. Options are: "all", "glossa", "prementum" and "tongue" (i.e. glossa + prementum)
#'
#' @return The input data frame with predicted mouthpart lengths (mm) and 95% confidence bounds appended. The glossa-only result is named Glossa.
#' @details Models are fitted to tongue_dataset, which combines the Cariveau data with species means from Melin et al. (2019). Predictions and confidence bounds are exponentiated from the natural-log scale. They describe fitted geometric means, not arithmetic means or individual prediction intervals. Total tongue length is modelled separately, so its prediction need not equal the sum of separately predicted glossa and prementum lengths. No data are downloaded during a call.
#'
#' @examples
#' example=cbind.data.frame(IT=c(1.3,2.3),
#'                          Family=c("Andrenidae","Apidae"))
#' tonguelength(example,mouthpart="all")
#'
#' @references \itemize{
#' \item Kendall et al. (2019) Pollinator size and its consequences: Robust estimates of body size in pollinating insects. \emph{Ecology and Evolution}, 9(4), 1702-1714. \doi{10.1002/ece3.4835}.
#' \item Cariveau et al. (2016) The Allometry of Bee Proboscis Length and Its Uses in Ecology. \emph{PLOS ONE}, 11(3): e0151482. \doi{10.1371/journal.pone.0151482}.
#' \item Melin et al. (2019) The allometry of proboscis length in Melittidae (Hymenoptera: Apoidae) and an estimate of their foraging distance using museum collections. \emph{PLOS ONE}, 14(6), e0217839. \doi{10.1371/journal.pone.0217839}.
#' }
#'
#'
#'
#'
#' @export
tonguelength <- function(x, mouthpart = "all"){
  .frame(x, c("IT", "Family"), "x")
  .positive(x$IT, "IT")
  .levels(x$Family, c("Andrenidae", "Apidae", "Colletidae", "Halictidae", "Megachilidae", "Melittidae"), "Family")
  .choice(mouthpart, c("all", "glossa", "prementum", "tongue"), "mouthpart")
  training <- new.env(parent = emptyenv())
  utils::data("tongue_dataset", package = "pollimetry", envir = training)
  tongue.dat <- training$tongue_dataset

  #fit models
  proboscis=stats::lm(log(mean_tongue_length_mm)~Family+log(IT),tongue.dat)
  glossa=stats::lm(log(mean_glossa_length_mm)~log(IT)+Family,tongue.dat)
  prementum=stats::lm(log(mean_prementum_length_mm)~log(IT)*Family,tongue.dat)

  #print predictions
  if(mouthpart=="all"){
    out<-exp(stats::predict(proboscis,x,interval = c("confidence"),
                     level = 0.95))
    out2<-exp(stats::predict(glossa,x,interval = c("confidence"),
                      level = 0.95))
    out3<-exp(stats::predict(prementum,x,interval = c("confidence"),
                      level = 0.95))
    out=cbind(out,out2,out3)
    colnames(out)=c("Proboscis","P.lwr.CI","P.upr.CI","Glossa","G.lwr.CI","G.upr.CI","Prementum","Pr.lwr.CI","Pr.upr.CI")
    out=cbind(x,out)
    out
  }else{
    if(mouthpart=="glossa"){
      out<-exp(stats::predict(glossa,x,interval = c( "confidence"),
                       level = 0.95))
      colnames(out)=c("Glossa","lwr.CI","upr.CI")
    }
    if(mouthpart=="prementum"){
      out<-exp(stats::predict(prementum,x,interval = c( "confidence"),
                       level = 0.95))
      colnames(out)=c("Prementum","lwr.CI","upr.CI")
    }
    if(mouthpart=="tongue"){
      out<-exp(stats::predict(proboscis,x,interval = c( "confidence"),
                       level = 0.95))
      colnames(out)=c("Proboscis","lwr.CI","upr.CI")
    }
    out=cbind(x,out)
    out
  }
}
