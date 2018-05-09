Training and test sets

```{r}  
#Split into training and test set
set.seed(123)
bee_subset=sample(seq_len(nrow(bee_mean)), size = floor(0.8 * nrow(bee_mean)))
bee_test <- bee_mean[-bee_subset, ]
bee_train <- bee_mean[bee_subset, ]

Cane <- function(IT){exp(0.6453 + 2.4691*log(IT))}

bee_test_mod=lmer(log(Spec.wgt) ~ log(IT)  + Family + Sex + Region + #fix factors
                    log(IT):Family + log(IT):Region + #interactions
                    (1|Measurement)+(1|Species),data=bee_train,REML=FALSE)
bee_test_red_mod=lmer(formula = log(Spec.wgt) ~ log(IT) + Family + Sex + log(IT):Family+ (1 | Measurement)+(1|Species), data = bee_train, REML = FALSE)

bee_test_red_mod2=lmer(formula = log(Spec.wgt) ~ log(IT) + (1 | Measurement)+(1|Species), data = bee_train, REML = FALSE)

bee_pred1=predict(bee_test_mod,newdata=bee_test,allow.new.levels=TRUE)
bee_pred2=predict(bee_test_red_mod,newdata=bee_test,allow.new.levels=TRUE)
bee_pred3=predict(bee_test_red_mod2,newdata=bee_test,allow.new.levels=TRUE)
bee_pred4=0.6453 + 2.4691*log(bee_test$IT)
bee_pred5=predict(Bee_PGLS1,newdata=bee_test)
par(pty="s")
par(mfrow=c(1,2))
plot((IT)~(Spec.wgt),bee_test,ylab="ITD (mm)",xlab="Dry weight (mg)")
points((bee_test$IT)~exp(bee_pred1),col=2)
points((bee_test$IT)~exp(bee_pred2),col=3)


plot((IT)~(Spec.wgt),bee_test,ylab="ITD (mm)",xlab="Dry weight (mg)")
points((bee_test$IT)~exp(bee_pred3),col=4)
points((bee_test$IT)~exp(bee_pred4),col=5)
points((bee_test$IT)~exp(bee_pred5),col=6)

bee_rmse=cbind(
  rmse(log(bee_test$Spec.wgt),bee_pred1),
  rmse(log(bee_test$Spec.wgt),bee_pred2),
  rmse(log(bee_test$Spec.wgt),bee_pred3),
  rmse(log(bee_test$Spec.wgt),bee_pred4),
  rmse(log(bee_test$Spec.wgt),bee_pred5))
colnames(bee_rmse)=c("Full","Reduced","IT","Cane","PGLS")
rownames(bee_rmse)=c("RMSE")
bee_rmse
```

##HOVERFLIES
```{r}
library(ModelMetrics)
set.seed(123)
hov_subset=sample(seq_len(nrow(hov_mean)), size = floor(0.8 * nrow(hov_mean)))
hov_test <- hov_mean[-hov_subset, ]
hov_train <- hov_mean[hov_subset, ]

hov_test_mod=lmer(formula = log(Spec.wgt) ~ log(IT) + Region + Sex + (1|Species) + (1 | Measurement), 
                  data = hov_train, REML = FALSE)
hov_test_red_mod=lmer(formula = log(Spec.wgt) ~ log(IT) + Sex+(1|Species)+ (1 | Measurement), data = hov_train, REML = FALSE)

hov_test_red_mod2=lmer(formula = log(Spec.wgt) ~ log(IT) + (1|Species) + (1 | Measurement), data = hov_train, REML = FALSE)

hov_pred1=predict(hov_test_mod,newdata=hov_test,allow.new.levels=TRUE)
hov_pred2=predict(hov_test_red_mod,newdata=hov_test,allow.new.levels=TRUE)
hov_pred3=predict(hov_test_red_mod2,newdata=hov_test,allow.new.levels=TRUE)

par(pty="s")
plot((IT)~Spec.wgt,hov_test,ylab="ITD (mm)",xlab="Dry weight (mg)")
points((hov_test$IT)~exp(hov_pred1),col=2)
points((hov_test$IT)~exp(hov_pred2),col=3)
points((hov_test$IT)~exp(hov_pred3),col=4)


hov_rmse=cbind(rmse(log(hov_test$Spec.wgt),hov_pred1),
               rmse(log(hov_test$Spec.wgt),hov_pred2),
               rmse(log(hov_test$Spec.wgt),hov_pred3))
colnames(hov_rmse)=c("Full","Reduced","IT")
rownames(hov_rmse)=c("RMSE")
hov_rmse
```