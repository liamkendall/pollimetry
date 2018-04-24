library(ggplot2)
ggplot(data=bee_all,aes(log(Spec.wgt),log(IT)))+geom_point(pch=0,col=1)+
  geom_smooth(aes(),method="lm",formula = y ~ x,se=FALSE)+theme_bw()+
  geom_line(data=PGLSpred,aes(x=exp(pred),y=exp(IT)))
  
  
  

PGLSpred=predict(Bee_PGLS1,newdata=bee_test)
PGLSpred=cbind.data.frame((bee_test$Spec.wgt*1000),PGLSpred)
colnames(PGLSpred)=c("IT","pred")

  
  
ggplot(data=bee_mean,aes(log(IT),log(Spec.wgt)))+geom_point(pch=0,cex=0.1*bee_mean$Latitude)+
  geom_smooth(aes(col=bee_mean$Country),method="lm",formula = y ~ x,se=FALSE)+theme_bw()            

ggplot(data=forage,aes(log(Max),log(Spec.wgt)))+geom_point(pch=0)+
  geom_smooth(aes(col=forage$Type),method="lm",formula = y ~ x,se=FALSE)+theme_bw()            

ggplot(data=forage,aes(log(Mean),log(Spec.wgt)))+geom_point(pch=0)+
  geom_smooth(aes(col=forage$Type),method="lm",formula = y ~ x,se=FALSE)+theme_bw()            


str(bee_all$Climate)
plot(log(IT)~log(Spec.wgt),bee_all,col=Region)
identify(log(bee_all$IT)~log(bee_all$Spec.wgt))


bee_all[c(963  ,966,  988,  991,  992 ,1014 ,1018),]





