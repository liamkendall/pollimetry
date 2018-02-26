##########################
## TRAINING AND TESTING ##
##########################

library(ModelMetrics)

Full_bee_mod=lm(formula = log(Spec.wgt) ~ 0+Climate + Family + Latitude + log(IT) + 
               Sex + Climate:log(IT) + Latitude:log(IT), data = bee_mean)

Train_mod=lm(formula = log(Spec.wgt) ~ 0+Climate + Family + Latitude + log(IT) + 
             Sex + Climate:log(IT) + Latitude:log(IT), data = bee_train)
summary(Train_mod)
plot(bee_test$Spec.wgt~exp(predict(Train_mod, newdata=bee_test)))

bee_test$pred=exp(predict(Train_mod, newdata=bee_test))
bee_mean$pred=exp(predict(Full_bee_mod, newdata=bee_mean))
bee_all$pred=exp(predict(Train_mod, newdata=bee_all))

rmse(bee_test$Spec.wgt , bee_test$Cane)
#0.08
rmse(bee_test$Spec.wgt , exp(predicted))
#0.006
rmse(bee_all$Spec.wgt , bee_all$Cane)

par(pty="s")
plot(Spec.wgt~IT,bee_test)
points(Cane~IT,bee_mean,pch=1,col="red")

Full_bee_mod
plot(pred~IT,bee_mean,pch=1,col=Climate,ylim=c(0,0.3))
points(Cane~IT,bee_mean,pch=1,col="darkred")
points(Spec.wgt~IT,bee_train,pch=1,col="darkgreen")

plot(pred~Spec.wgt,bee_mean,pch=1,col="darkblue",ylim=c(0,0.3))
points(Cane~Spec.wgt,bee_mean,pch=1,col="darkred")
points(Spec.wgt~IT,bee_train,pch=1,col="darkgreen")


climate_cane_plot=ggplot(bee_mean,aes(x = IT, y = Spec.wgt))+theme_bw()+geom_point(pch=0,cex=2)+
  geom_smooth(data=bee_mean,formula = y~poly(x,2),aes(col=Climate),method="glm",se=FALSE)+
  geom_smooth(data=bee_mean,aes(x = IT,y=Cane),formula = y~poly(x,2),method="glm",se=FALSE)+
  coord_cartesian(ylim=c(0,0.7),xlim=c(0,12))+
  geom_point(data = poll_country$USA,aes(x = IT,y = Spec.wgt),pch=0,col="blue")+
geom_smooth(data = bee_all,aes(x = IT,y = pred),col="orange",formula = y~poly(x,2),
            method="glm",se=FALSE)

climate_cane_plot


CaneClimate <- data.frame(Site = c("GC","UFS","NEG"),
                   Longitude = c(-85.529473,-85.529473,-85.529473),
                   Latitude = c(32.531318,32.531318,32.531318))
CaneClimate <- data.frame(CaneClimate,
                   rndCoord.lon = RoundCoordinates(CaneClimate$Longitude),
                   rndCoord.lat = RoundCoordinates(CaneClimate$Latitude))
CaneClimate <- data.frame(CaneClimate,ClimateZ=LookupCZ(CaneClimate))
