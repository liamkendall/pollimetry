bee_body_test=bee_test[,c("Family","Country","Sex","Latitude","IT")]

beebody(Country=bee_body_test$Country,Family=bee_body_test$Family,
        IT=bee_body_test$IT,Sex=bee_body_test$Sex,Latitude=bee_body_test$Latitude)
debugonce(beebody)
