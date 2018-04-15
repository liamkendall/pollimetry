##########################
## TRAINING AND TESTING ##
##########################

library(ModelMetrics)

##Assemble best models
###Next models are more complex, seems redundant to test them

bee_dr_mods[1]$`56`
bee_1=lmer(log(Spec.wgt) ~ Family + Latitude + log(IT) + (1 | Measurer) +  Family:log(IT) + Latitude:log(IT),
data = bee_train)

bee_test$bee_1pred=predict(bee_1,newdata=bee_test)

rmse(bee_test$Spec.wgt, bee_test$bee_1pred)#0.007
rmse(bee_test$Spec.wgt,bee_test$Cane) #0.0567

climate_cane_plot=ggplot(bee_mean,aes(x = log(Spec.wgt), y = log(IT)))+
  geom_point(pch=0)+
  theme_bw()+
  geom_smooth(data=bee_test,aes(x = log(bee_1pred),y = log(IT)),method="lm",col="orange",se = FALSE)+
  geom_smooth(data = bee_test,aes(x = log(Cane), y = log(IT)), method="lm",se=FALSE)

#

# load the library
library(caret)

bee_mean
# define training control
train_control <- trainControl(method = "LGOCV", p = 0.8, number = 1000,
                              savePredictions = T)
# train the model
bee1.boot <- train(log(Spec.wgt) ~ Family + Latitude + log(IT) +  
                     Family:log(IT) + Latitude:log(IT), data=bee_mean,
                   method = "lm",
                   trControl = train_control)
