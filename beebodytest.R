bee_body_test=bee_test[,c("Family","Country","Sex","Latitude","IT")]

beebody(Country=bee_body_test$Country,Family=bee_body_test$Family,
        IT=bee_body_test$IT,Sex=bee_body_test$Sex,Latitude=bee_body_test$Latitude)
debugonce(beebody)

######

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

Country_intercepts2=merge(data.frame(id = c(1:length(bee_test$Country)), Countries = bee_test$Country), 
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