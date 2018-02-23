#' @name beebody
#' 
#' @title Converts the intertegular distance (IT) to body weight for bees.
#' 
#' @description long description here...
#'
#' @param IT A vector of bee intertegular spans (IT) measurements in mm.
#' @param Country Country where specimens collected. Accepted countries are... 
#' @param Family The bee family of each specimen
#' @param Latitude A vector of Latitude
#' @param Sex The sex of each specimen
#' #param Type IDEA:WE could put resolution in here?? IT or IT+x or IT+x1+x2
#' 
#' @return A dataframe with bee body weight is returned for each specimen.
#'
#' @examples
#' beebody(c(10,5,2))
#' beebody(IT=c(10,5,2), Country = rep("Australia",3), Family = rep("Apidae", 3), Latitude = rep(-30, 3), Sex = rep("Male",3)) 
#' 
#' @references 
#' @export

library(broom) #ADD to DEPENDENCIES

beebody <- function(IT, Country = NULL, Family = NULL, Latitude = NULL, Sex = NULL, data = bee_train){
  if(data = bee_train){
    load(bee_train)
  } else{
    load(bee_all)
  } #Think more about this, and how much flexibility we give.
  if(any(!Country %in% c("Australia", #need to allow NULL
                  "Spain", 
                  "Britain",
                  "Ireland", 
                  "Germany",
                  "USA"))) { #THIS limits to actual countries, make more generic? Regions?
    stop("Country should be one of 'Australia', 'Spain', 'Britain', 'Ireland', 'Germany' or 'USA'")
  }
  if(any(!Family %in% c("Andrenidae","Apidae", 
                    "Colletidae","Melittidae", 
                     "Megachilidae","Halictidae"))) {
    stop("Family should be one of 'Andrenidae', 'Apidae', 'Colletidae', 'Halictidae', 'Megachilidae' or 'Melittidae'")
  }
  if(is.numeric((IT)) == FALSE) {
    stop("IT should be numeric")
  }
  if(any(!Sex %in% c("Male","Female"))) {
    stop("Family should be one of 'Male' or 'Female'")
  }
  else {
    #Think about the BEST model. Latitude? Country may be overfitting the data? Sampling bias??
    Model_parameters <- tidy(summary(lm(formula = log(Spec.wgt) ~ 0 + Country +
                                        Family + log(IT) + Latitude + Sex + 
                                        Country:log(IT), data = data))) #Not necessarily this data set
    Model_parameters[,1] = c("Australia","Britain","Ireland", "Spain", 
            "Apidae","Colletidae","Halictidae","Megachilidae","Melittidae","IT",
                "Latitude","Male","Britain","Ireland","Spain" )
     
     Country_intercepts=Model_parameters[1:4,]
     colnames(Country_intercepts)[1] <- "Country" 
     
     Family_intercepts=Model_parameters[5:9,]
     colnames(Family_intercepts)[1] <- "Family"
     Family_intercepts[6,]=c("Andrenidae",0,0,0,0)
     Family_intercepts[,2] <- as.numeric(Family_intercepts[,2]) 
     
     Sex_intercept=Model_parameters[12,]
     colnames(Sex_intercept)[1] <- "Sex"
     Sex_intercept[2,]=c("Female",0,0,0,0)
     Sex_intercept[,2] <- as.numeric(Sex_intercept[,2]) 
     
     Co_int_intercepts=Model_parameters[13:15,]
     colnames(Co_int_intercepts)[1] <- "Country"
     Co_int_intercepts[4,]=c("Australia",0,0,0,0)
     Co_int_intercepts[,2] <- as.numeric(Co_int_intercepts[,2]) 
     
     Country_intercepts2=merge(data.frame(id = c(1:length(Country)), Countries = Country), 
           Country_intercepts, by.x = "Countries", by.y = "Country", all.x = TRUE) #MAKE SURE DO NOT BERAKS WITH NULL
     Country_intercepts2 <- Country_intercepts2[order(Country_intercepts2$id),]
     Family_intercepts2=merge(data.frame(id = c(1:length(Family)), Family = Family), 
                               Family_intercepts, all.x = TRUE)
     Family_intercepts2 <- Family_intercepts2[order(Family_intercepts2$id),]
     Sex_intercept2=merge(data.frame(id = c(1:length(Sex)), Sex = Sex), 
                          Sex_intercept, all.x = TRUE)
     Sex_intercept2 <- Sex_intercept2[order(Sex_intercept2$id),]
     Co_int_intercepts2=merge(data.frame(id = c(1:length(Country)), 
                                         Country = Country), 
                              Co_int_intercepts)
     Co_int_intercepts2 <- Co_int_intercepts2[order(Co_int_intercepts2$id),]

     #THIS MAY BE TO APPAEAR EALIER
     if(is.null(Country)){
       Country_intercepts2$estimate <- #make it the mean response? and Co_int_intercepts2
     }
     if(is.null(Family)){
       #idem...
     } #Up to Liam :)
     out = exp(Country_intercepts2$estimate+
                 Family_intercepts2$estimate+
                 Sex_intercept2$estimate+
                 log(IT)*Model_parameters[which(Model_parameters$term == "IT"),]$estimate+
                 Latitude*Model_parameters[which(Model_parameters$term == "Latitude"),]$estimate+
                 log(IT)*Co_int_intercepts2$estimate)
     out
}
}


set.seed(1234)
IT = runif(400,2,10)
predicted <- beebody(IT, 
        Country = rep(c("Australia", 
                        "Spain", 
                        "Britain",
                        "Ireland"),100), 
        Family = rep(c("Apidae", 
        "Colletidae", 
        "Megachilidae",
        "Halictidae"), 100), 
        Latitude = rep(runif(400,-30, 40)), 
        Sex = rep(c("Male", "Female"),200)) 

plot(IT ~ predicted)


predicted <- beebody(IT = bee_test$IT, 
                     Country = bee_test$Country, 
                     Family = bee_test$Family, 
                     Latitude = bee_test$Latitude, 
                     Sex = bee_test$Sex) 
plot(bee_test$Spec.wgt ~ predicted)
bee_test$predicted <- predicted
bee_test[which(bee_test$Spec.wgt > 0.06),]
library(ModelMetrics)

rmse(bee_test$Spec.wgt, predicted)





