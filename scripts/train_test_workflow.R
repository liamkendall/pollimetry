##########################
## TRAINING AND TESTING ##
##########################

library(ModelMetrics)

##Assemble best models
###Next models are more complex, seems redundant to test them

bee_train_mod=lmer(log(Spec.wgt) ~ log(IT)  + Family + Sex + Region + #fix factors
                    log(IT):Family + log(IT):Region + log(IT):Sex + #interactions
                    (1|Measurement),data=bee_train,REML=FALSE)
bee_train_red_mod=lmer(formula = log(Spec.wgt) ~ log(IT) + Family + Sex + log(IT):Family + 
                        log(IT):Sex + (1 | Measurement), data = bee_train, REML = FALSE)

bee_test$bee_1pred=predict(bee_train_mod,newdata=bee_test)
bee_test$bee_2pred=predict(bee_train_red_mod,newdata=bee_test)

rmse(bee_test$Spec.wgt,exp(bee_test$bee_2pred))#0.007
rmse(bee_test$Spec.wgt,exp(bee_test$bee_1pred)) #0.0567
par(pty="s")
plot(log(Spec.wgt)~log(IT),bee_train)
points(bee_pred2~log(bee_test$IT),col=2)

rmse(log(bee_test$Spec.wgt),bee_test$bee_2pred)



rmse(log(bee_test$Spec.wgt),bee_test$bee_1pred)