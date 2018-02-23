#' @name beebody
#' 
#' @title Converts the intertegular distance (IT) to body weight for bees.
#' 
#' @description 
#'
#' @param IT A vector of bee intertegular spans (IT) measurements in mm.
#' 
#'@param Country Country where specimens collected
#'IDEA:WE could put resolution in here?? IT or IT+x or IT+x1+x2
#' 
#' @param Family The bee family of each specimen
#' 
#' @param Latitude A vector of Latitude
#' 
#' @param Sex The sex of each specimen
#' 
#' #param Type IDEA:WE could put resolution in here?? IT or IT+x or IT+x1+x2
#' 
#' @return A dataframe with bee body weight is returned for each specimen.
#'
#' @examples
#' beebody(c(10,5,2))
#' beebody(IT=c(10,5,2), Country = "Australia", Family = "Apidae", Latitude =c(-30),Sex = "Male") 
#' 
#' @references 

Where do you put libraries? broom, reshape
#' @export

beebody <- function(IT, Country, Family, Latitude, Sex){
  if(!Country %in% c("Australia", 
                  "Spain", 
                  "Britain",
                  "Ireland", 
                  "Germany","USA")) {
    stop("Country should be one of 'Australia', 'Spain', 'Britain', 'Ireland', 'Germany' or 'USA'")
  }
  if(!Family %in% c("Andrenidae","Apidae", 
                    "Colletidae","Melittidae", 
                     "Megachilidae","Halictidae")) {
    stop("Family should be one of 'Andrenidae', 'Apidae', 'Colletidae', 'Halictidae', 'Megachilidae' or 'Melittidae'")
  }
  if(is.numeric((IT)) == FALSE) {
    stop("IT should be numeric")
  }
  if(!Sex %in% c("Male","Female")) {
    stop("Family should be one of 'Male' or 'Female'")
  }
  else {
    Model_parameters <- tidy(summary(lm(formula = Spec.wgt ~0+ Country +
                                        Family + IT + Latitude + Sex + 
                                        Country:IT, data = bee_train))) #Not necessarily this data set
    Model_parameters[,1] = c("Australia","Britain","Ireland", "Spain", 
            "Apidae","Colletidae","Halictidae","Megachilidae","Melittidae","IT",
                "Latitude","Male","Britain","Ireland","Spain" )
     
     Country_intercepts=Model_parameters[1:4,]
     colnames(Country_intercepts)[1] <- "Country"
     
     Family_intercepts=Model_parameters[5:9,]
     colnames(Family_intercepts)[1] <- "Family"
     Family_intercepts[6,]=c("Andrenidae",0,0,0,0)
    
    Sex_intercept=Model_parameters[12,]
     colnames(Sex_intercept)[1] <- "Sex"
     Sex_intercept[2,]=c("Female",0,0,0,0)
     
     Co_int_intercepts=Model_parameters[13:15,]
     colnames(Co_int_intercepts)[1] <- "Country"
     Co_int_intercepts[4,]=c("Australia",0,0,0,0)
     
     Country_intercepts2=merge(data.frame(id = c(1:length(Country)), Countries = Country), 
           Country_intercepts)
     Country_intercepts2 <- Country_intercepts2[order(Country_intercepts2$id),]
     Family_intercepts2=merge(data.frame(id = c(1:length(Family)), families = Family), 
                               Family_intercepts)
     Family_intercepts2 <- Family_intercepts2[order(Family_intercepts2$id),]
     Sex_intercept2=merge(data.frame(id = c(1:length(Sex)), Sex = Sex), 
                          Sex_intercept)
     Sex_intercept2 <- Sex_intercept2[order(Sex_intercept2$id),]
     Co_int_intercepts2=merge(data.frame(id = c(1:length(Country)), 
                                         Co_int = Country), 
                              Co_int_intercepts)
     Co_int_intercepts2 <- Co_int_intercepts2[order(Co_int_intercepts2$id),]

     out = exp(Country_intercepts2$estimate+
                 Family_intercepts2$estimate+
                 Sex_intercept2$Sex+
               IT*Model_parameters["IT",]$estimate+
               Latitude*Model_parameters["Latitude",]$estimate+
                 IT*Co_int_intercepts2$estimate)
     out
}
}
