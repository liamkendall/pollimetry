##########################
## TRAINING AND TESTING ##
##########################

library(ModelMetrics)

##Assemble best models
#FCLP
FCLPtrain=lm(formula = log (Spec.wgt) ~ 0 + Climate + Family + Latitude + 
               log(IT) + Pres.time + Sex + Climate:Latitude + Climate: log(IT) + 
               log(IT):Pres.time, data = bee_test)
#FCL
FCLtrain = lm(formula = log (Spec.wgt) ~ 0 + Climate+ log(IT) + Latitude + 
                Sex + Family + log (IT):Latitude +  log(IT):Sex + log(IT):Climate + 
                log(IT):Family + Climate:Latitude, data = bee_train)
#FC
FCtrain=lm(formula = log (Spec.wgt) ~ 0 + Climate + log (IT) + Sex + Family + 
             log (IT):Sex +  log(IT):Climate + log (IT):Family, data = bee_train)
#Family

Flmtrain=lm(formula = log (Spec.wgt) ~ 0 + Family +  log(IT), data = bee_train)
#Climate
Clmtrain=lm(formula = log (Spec.wgt) ~ 0 + Climate +  Sex + Climate:log (IT), data = bee_train)

#JUST IT
ITtrain=lm(formula =  log(Spec.wgt) ~log(IT), data = bee_train)

bee_test$FCLPpred=exp(predict(FCLPtrain,newdata=bee_test))
bee_test$FCLpred=exp(predict(FCLtrain,newdata=bee_test))
bee_test$FCpred=exp(predict(FCtrain,newdata=bee_test))
bee_test$Flmpred=exp(predict(Flmtrain,newdata=bee_test))
bee_test$Clmpred=exp(predict(Clmtrain,newdata=bee_test))
bee_test$PGLSpred=exp(predict(bee_pgls1,newdata=bee_test))
bee_test$ITpred=exp(predict(ITtrain,newdata=bee_test))

RMSE=rbind(
rmse(bee_test$Spec.wgt, FCLPpred), #0.005
rmse(bee_test$Spec.wgt, FCLpred), #0.005
rmse(bee_test$Spec.wgt, FCpred), #0.01
rmse(bee_test$Spec.wgt, Flmpred), #0.01
rmse(bee_test$Spec.wgt, Clmpred), #0.008
rmse(bee_test$Spec.wgt, PGLSpred), #0.02
rmse(bee_mean$Spec.wgt,bee_mean$clim_pgls_pred), #0.02
rmse(bee_mean$Spec.wgt,bee_mean$Cane)) #0.05
rownames(RMSE)=c("Full","FCL","FC","Fam","Cli","pgls","Cli.pgls","Cane1987")
colnames(RMSE)[1]="RMSE"

plot( (Spec.wgt)~ (FCLPpred),data=bee_test,pch=0) #0.005
points( (Spec.wgt)~ (FCLpred),data=bee_test,pch=2) #0.005
points( (Spec.wgt)~ (FCpred),data=bee_test,pch=3) #0.01
points( (Spec.wgt)~ (Flmpred),data=bee_test,pch=4) #0.01
points( (Spec.wgt)~ (Clmpred),data=bee_test,pch=5) #0.008
points( (Spec.wgt)~ (PGLSpred),data=bee_test,pch=6) #0.02
points( (Spec.wgt)~ (Cane),data=bee_test,pch=7) #0.05

exp(predict(bee_pgls1,newdata=bee_test))

bee_mean$FCLPpred=exp(predict(FCLP,newdata=bee_mean))
bee_mean$FCLpred=exp(predict(FCL,newdata=bee_mean))
bee_mean$FCpred=exp(predict(FC,newdata=bee_mean))
bee_mean$Flmpred=exp(predict(Flm,newdata=bee_mean))
bee_mean$Clmpred=exp(predict(Clm,newdata=bee_mean))
bee_mean$PGLSpred=exp(predict(bee_pgls1,newdata=bee_mean))
bee_mean$clim_pgls_pred=exp(predict(climate_pgls,newdata=bee_mean))


str(bee_test)
melt(bee_test)

climate_cane_plot=ggplot(bee_mean,aes(x = Spec.wgt, y = IT))+
  geom_point(pch=0,cex=0.1*bee_mean$Latitude)+
  theme_bw()+
  geom_smooth(data = bee_mean,aes(y = IT,x = PGLSpred),col="orange",formula = y~ log(x), method="glm",se=FALSE)+
  coord_cartesian(ylim=c(0,12),xlim=c(0,0.275))+
  geom_point(data = poll_country$USA,aes(y = IT,x = Spec.wgt),pch=0)+
  geom_smooth(data = bee_test,aes(y = IT,x = Cane,formula = y ~ log(x),
              method="glm",se=FALSE)+
  geom_smooth(data = bee_test,aes(y = IT,x = FCLPpred),formula = y ~ log(x),
              method="glm",se=FALSE)+
geom_smooth(data = bee_test,aes(y = IT,x = Clmpred),formula = y ~ log(x),
            method="glm",se=FALSE)+
geom_smooth(data = bee_test,aes(y = IT,x = Flmpred),formula = y ~ log(x),
            method="glm",se=FALSE)+
  geom_smooth(data = bee_mean,aes(y = IT,x = PGLSpred),formula = y ~ log(x),
                                               method="glm",se=TRUE)+
  geom_smooth(data = bee_mean,aes(y = IT,x = clim_pgls_pred),formula = y ~ log(x),
              method="glm",se=TRUE)+
geom_smooth(data = bee_test,aes(y = IT,x = ITpred),formula = y~ log(x),
            method="glm",se=TRUE)
climate_cane_plot

