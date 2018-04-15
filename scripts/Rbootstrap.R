##CROSS VALIDATION bootstrap alternative to training/testing

#Weight by RMSE for models selected to useful

# load the library
library(caret)

bee_mean
# define training control
train_control <- trainControl(method = "LGOCV", p = 0.8, number = 1000,
                              savePredictions = T)
# train the model
FCL.boot <- train(log(Spec.wgt) ~ Region+Family + log(IT)  + Sex +  Family:log(IT),
               method = "lm",data = bee_mean,
               trControl = train_control)

warnings()
predict(FCLP.boo,newdata=bee_mean)
FCL.boot = train(log(Spec.wgt) ~ 0 + Climate+ log(IT) + Latitude + 
                Sex + Family + log (IT):Latitude +  log(IT):Sex + log(IT):Climate + 
                log(IT):Family + Climate:Latitude, data=bee_mean,
                method = "lm",
                trControl = train_control)
predict(FCL.boot,newdata=bee_mean)
FC.boot = train(log (Spec.wgt) ~ 0 + Climate + log (IT) + Sex + Family + 
                   log (IT):Sex +  log(IT):Climate + log (IT):Family, data = bee_mean,
                 method = "lm",
                 trControl = train_control)
Fam.boot = train(log(Spec.wgt) ~ 0 + Family + log(IT), data = bee_mean,
                method = "lm",
                trControl = train_control)
Clim.boot = train(log(Spec.wgt) ~ 0 + Climate +  Sex + Climate:log (IT), data = bee_mean,
                 method = "lm",
                 trControl = train_control)
IT.boot = train(log(Spec.wgt) ~log(IT), data = bee_mean,
                  method = "lm",
                  trControl = train_control)
IT.boot = train(log(Spec.wgt)~log(IT), data=bee_phylo[-47,], correlation=Bee_phy_vcov,
                method = "gls",
                trControl = train_control)


Subfamily.boot = train(log(Spec.wgt) ~ Subfamily, data=bee_mean,
                 method = "lm",
                 trControl = train_control)

options(na.action=na.omit)
predict(Genus.boot,newdata=forage)
warnings()

Fam.boot 
Clim.boot
IT.boot

bee_mean$FCLP.boot=exp(predict(FCLP.boot,newdata=bee_mean))
bee_mean$FCL.boot=exp(predict(FCL.boot,newdata=bee_mean))
bee_mean$FC.boot=exp(predict(FC.boot,newdata=bee_mean))
bee_mean$Fam.boot=exp(predict(Fam.boot,newdata=bee_mean))
bee_mean$Clim.boot=exp(predict(Clim.boot,newdata=bee_mean))
bee_mean$IT.boot=exp(predict(IT.boot,newdata=bee_mean))

boot_plot=ggplot(bee_mean,aes(x = Spec.wgt, y = IT))+
  geom_point(pch=1,cex=2)+geom_point(data=poll_country$USA,aes(y=IT,Spec.wgt),pch=2)+
  geom_smooth(data = bee_mean,aes(y = IT,x = FCLP.boot),col="orange",
              method = 'nls', formula = y ~ a * x^b,se=FALSE)+
  coord_cartesian(ylim=c(0,12),xlim=c(0,0.3))+geom_point()+
                geom_smooth(data = bee_mean,aes(y = IT,x = Cane),
                            formula = y ~ log(x),method="glm",col="darkblue",se=FALSE)+
                geom_smooth(data = bee_mean,aes(y = IT,x = FCL.boot),
                            method = 'nls', formula = y ~ a * x^b,col="darkred")+
                geom_smooth(data = bee_mean,aes(y = IT,x = FC.boot),
                            method = 'nls', formula = y ~ a * x^b,se=FALSE)+
                geom_smooth(data = bee_mean,aes(y = IT,x = Fam.boot),
                            method = 'nls', formula = y ~ a * x^b,se=FALSE)+
                geom_smooth(data = bee_mean,aes(y = IT,x = Clim.boot),
                            method = 'nls', formula = y ~ a * x^b,se=TRUE,col="darkgreen")+
                geom_smooth(data = bee_mean,aes(y = IT,x = IT.boot),
                            method = 'nls', formula = y ~ a * x^b,se=TRUE,col="darkblue")#+
                geom_smooth(data = bee_mean,aes(y = IT,x = pgls_pred),
                            method = 'nls', formula = y ~ a * x^b,se=TRUE)
 boot_plot

 ##library lmeresampler
 
 
 ## running a parametric bootstrap
 
 boo1 <- bootstrap(model = bee_model,fn='fixef',type = "parametric", B = 100)
 require("boot")
 as.data.frame(boo1)

 
 boot.ci(boo1, index = 1, type=c("norm", "basic", "perc"))
 boot.ci(boo1, index = 6, type=c("norm", "basic", "perc"))
 ## you can also examine the bootstrap samples graphically
 plot(boo1, index = 1)
 
boot1=bootMer(bee_model,FUN='fixef',use.u=TRUE,type="parametric")
boot.ci(boot1,index = 1, type=c("norm", "basic", "perc"))
