##BEE ALL MODEL VALIDATION

library(ModelMetrics)
library(caret)
set.seed(123)
bee_all_subset=sample(seq_len(nrow(bee_all)), size = floor(0.8 * nrow(bee_all)))
bee_all_test <- bee_mean[-bee_all_subset, ]
bee_all_train <- bee_mean[bee_all_subset, ]

Cane <- function(IT){exp(0.6453 + 2.4691*log(IT))}


bee_all_test_mod=lmer(log(Spec.wgt) ~ log(IT)  + Family + Sex + Region + #fix factors
                    log(IT):Family + log(IT):Region + log(IT):Sex + #interactions
                      (1|Species) +(1|Measurement),data=bee_all_train,REML=FALSE)
bee_all_test_red_mod=lmer(formula = log(Spec.wgt) ~ log(IT) + Family + Sex + log(IT):Family + 
                        log(IT):Sex + (1|Species) +(1 | Measurement), data = bee_all_train, REML = FALSE)

bee_all_test_red_mod2=lmer(formula = log(Spec.wgt) ~ log(IT) + (1|Species) +(1 | Measurement), data = bee_all_train, REML = FALSE)
?predict.merMod
bee_all_pred1=predict(bee_all_test_mod,newdata=bee_all_test,allow.new.levels=TRUE)
bee_all_pred2=predict(bee_all_test_red_mod,newdata=bee_all_test,allow.new.levels=TRUE)
bee_all_pred3=predict(bee_all_test_red_mod2,newdata=bee_all_test,allow.new.levels=TRUE)
bee_all_pred4=0.6453 + 2.4691*log(bee_all_test$IT)
bee_all_pred5=predict(Bee_PGLS1,newdata=bee_all_test)
par(pty="s")
par(mfrow=c(1,2))

bee_all_rmse=cbind(
  rmse(log(bee_all_test$Spec.wgt),bee_all_pred1),
  rmse(log(bee_all_test$Spec.wgt),bee_all_pred2),
  rmse(log(bee_all_test$Spec.wgt),bee_all_pred3),
  rmse(log(bee_all_test$Spec.wgt),bee_all_pred4),
  rmse(log(bee_all_test$Spec.wgt),bee_all_pred5))
colnames(bee_all_rmse)=c("Full","Reduced","IT","Cane","PGLS")
rownames(bee_all_rmse)=c("RMSE")
bee_all_rmse

plot((IT)~(Spec.wgt),bee_all_train,ylab="Specimen weight (mg)",xlab="ITD (mm)")
points((bee_all_test$IT)~exp(bee_all_pred1),col=2,cex=1/bee_all_rmse[1])
points((bee_all_test$IT)~exp(bee_all_pred2),col=3,cex=1/bee_all_rmse[2])


plot((IT)~(Spec.wgt),bee_all_train,ylab="Specimen weight (mg)",xlab="ITD (mm)")
points((bee_all_test$IT)~exp(bee_all_pred3),col=4,cex=1/bee_all_rmse[3])
points((bee_all_test$IT)~exp(bee_all_pred4),col=5,cex=1/bee_all_rmse[4])
points((bee_all_test$IT)~exp(bee_all_pred5),col=6,cex=1/bee_all_rmse[5])
