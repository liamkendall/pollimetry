
par(pty="s",mfrow=c(2,3))
plot(Spec.wgt~IT,bee_mean[bee_mean$Family=="Apidae" & bee_mean$Sex =="Female",],
       cex=1,pch=8,col="black",main="Apidae",ylim=c(0,800),xlab="ITD (mm)",ylab="Dry weight (mg)")

points(exp(predict(bee2,newdata=bee_mean[bee_mean$Family=="Apidae" & bee_mean$Sex =="Female",]
                 ,re_formula = NA)[,1])~
       bee_mean[bee_mean$Family=="Apidae" & bee_mean$Sex =="Female",]$IT,col="darkblue")
points(exp(predict(bee2,newdata=bee_mean[bee_mean$Family=="Apidae" & bee_mean$Sex =="Female",]
                  )[,1])~
         bee_mean[bee_mean$Family=="Apidae" & bee_mean$Sex =="Female",]$IT,pch=2,col="darkblue")
points(exp(predict(bee_p1,newdata=bee_mean[bee_mean$Family=="Apidae" & bee_mean$Sex =="Female",]
                   ,re_formula = NA)[,1])~
         bee_mean[bee_mean$Family=="Apidae" & bee_mean$Sex =="Female",]$IT,col="darkred")
points(exp(predict(bee_p1,newdata=bee_mean[bee_mean$Family=="Apidae" & bee_mean$Sex =="Female",]
                   )[,1])~
         bee_mean[bee_mean$Family=="Apidae" & bee_mean$Sex =="Female",]$IT,col="darkred",pch=2)


##ANDRENIDAE
plot(Spec.wgt~IT,bee_mean[bee_mean$Family=="Andrenidae" & bee_mean$Sex =="Female",],
     cex=1,pch=8,col="black",main="Andrenidae",xlab="ITD (mm)",ylab="Dry weight (mg)",ylim=c(0,120))
points(exp(predict(bee2,newdata=bee_mean[bee_mean$Family=="Andrenidae" & bee_mean$Sex =="Female",]
                   ,re_formula = NA)[,1])~
         bee_mean[bee_mean$Family=="Andrenidae" & bee_mean$Sex =="Female",]$IT,col="darkblue")
points(exp(predict(bee2,newdata=bee_mean[bee_mean$Family=="Andrenidae" & bee_mean$Sex =="Female",]
                   )[,1])~
         bee_mean[bee_mean$Family=="Andrenidae" & bee_mean$Sex =="Female",]$IT,pch=2,col="darkblue")

points(exp(predict(bee_p1,newdata=bee_mean[bee_mean$Family=="Andrenidae" & bee_mean$Sex =="Female",]
                   ,re_formula = NA)[,1])~
         bee_mean[bee_mean$Family=="Andrenidae" & bee_mean$Sex =="Female",]$IT,col="darkred")
points(exp(predict(bee_p1,newdata=bee_mean[bee_mean$Family=="Andrenidae" & bee_mean$Sex =="Female",]
                   )[,1])~
         bee_mean[bee_mean$Family=="Andrenidae" & bee_mean$Sex =="Female",]$IT,col="darkred",pch=2)

##Colletidae
plot(Spec.wgt~IT,bee_mean[bee_mean$Family=="Colletidae" & bee_mean$Sex =="Female",],
     cex=1,pch=8,col="black",main="Colletidae",ylim=c(0,50),xlab="ITD (mm)",ylab="Dry weight (mg)")
points(exp(predict(bee2,newdata=bee_mean[bee_mean$Family=="Colletidae" & bee_mean$Sex =="Female",]
                   ,re_formula = NA)[,1])~
         bee_mean[bee_mean$Family=="Colletidae" & bee_mean$Sex =="Female",]$IT,col="darkblue")
points(exp(predict(bee2,newdata=bee_mean[bee_mean$Family=="Colletidae" & bee_mean$Sex =="Female",]
                   )[,1])~
         bee_mean[bee_mean$Family=="Colletidae" & bee_mean$Sex =="Female",]$IT,pch=2,col="darkblue")
points(exp(predict(bee_p1,newdata=bee_mean[bee_mean$Family=="Colletidae" & bee_mean$Sex =="Female",]
                   ,re_formula = NA)[,1])~
         bee_mean[bee_mean$Family=="Colletidae" & bee_mean$Sex =="Female",]$IT,col="darkred")
points(exp(predict(bee_p1,newdata=bee_mean[bee_mean$Family=="Colletidae" & bee_mean$Sex =="Female",]
                  )[,1])~
         bee_mean[bee_mean$Family=="Colletidae" & bee_mean$Sex =="Female",]$IT,col="darkred",pch=2)

##Halictidae
plot(Spec.wgt~IT,bee_mean[bee_mean$Family=="Halictidae" & bee_mean$Sex =="Female",],
     cex=1,pch=8,col="black",main="Halictidae",xlab="ITD (mm)",ylab="Dry weight (mg)")
points(exp(predict(bee2,newdata=bee_mean[bee_mean$Family=="Halictidae" & bee_mean$Sex =="Female",]
                   ,re_formula = NA)[,1])~
         bee_mean[bee_mean$Family=="Halictidae" & bee_mean$Sex =="Female",]$IT,col="darkblue")
points(exp(predict(bee2,newdata=bee_mean[bee_mean$Family=="Halictidae" & bee_mean$Sex =="Female",]
                   )[,1])~
         bee_mean[bee_mean$Family=="Halictidae" & bee_mean$Sex =="Female",]$IT,pch=2,col="darkblue")
points(exp(predict(bee_p1,newdata=bee_mean[bee_mean$Family=="Halictidae" & bee_mean$Sex =="Female",]
                   ,re_formula = NA)[,1])~
         bee_mean[bee_mean$Family=="Halictidae" & bee_mean$Sex =="Female",]$IT,col="darkred")
points(exp(predict(bee_p1,newdata=bee_mean[bee_mean$Family=="Halictidae" & bee_mean$Sex =="Female",]
                   )[,1])~
         bee_mean[bee_mean$Family=="Halictidae" & bee_mean$Sex =="Female",]$IT,col="darkred",pch=2)

##Megachilidae
plot(Spec.wgt~IT,bee_mean[bee_mean$Family=="Megachilidae" & bee_mean$Sex =="Female",],
     cex=1,pch=8,col="black",main="Megachilidae",xlab="ITD (mm)",ylab="Dry weight (mg)",ylim=c(0,80))
points(exp(predict(bee2,newdata=bee_mean[bee_mean$Family=="Megachilidae" & bee_mean$Sex =="Female",]
                   ,re_formula = NA)[,1])~
         bee_mean[bee_mean$Family=="Megachilidae" & bee_mean$Sex =="Female",]$IT,col="darkblue")
points(exp(predict(bee2,newdata=bee_mean[bee_mean$Family=="Megachilidae" & bee_mean$Sex =="Female",]
                   )[,1])~
         bee_mean[bee_mean$Family=="Megachilidae" & bee_mean$Sex =="Female",]$IT,pch=2,col="darkblue")
points(exp(predict(bee_p1,newdata=bee_mean[bee_mean$Family=="Megachilidae" & bee_mean$Sex =="Female",]
                   ,re_formula = NA)[,1])~
         bee_mean[bee_mean$Family=="Megachilidae" & bee_mean$Sex =="Female",]$IT,col="darkred")
points(exp(predict(bee_p1,newdata=bee_mean[bee_mean$Family=="Megachilidae" & bee_mean$Sex =="Female",]
                   )[,1])~
         bee_mean[bee_mean$Family=="Megachilidae" & bee_mean$Sex =="Female",]$IT,col="darkred",pch=2)


##Melittidae
plot(Spec.wgt~IT,bee_mean[bee_mean$Family=="Melittidae" & bee_mean$Sex =="Female",],
     cex=1,pch=8,col="black",main="Melittidae",xlab="ITD (mm)",ylab="Dry weight (mg)",ylim=c(0,40))
points(exp(predict(bee2,newdata=bee_mean[bee_mean$Family=="Melittidae" & bee_mean$Sex =="Female",]
                   ,re_formula = NA)[,1])~
         bee_mean[bee_mean$Family=="Melittidae" & bee_mean$Sex =="Female",]$IT,col="darkblue")
points(exp(predict(bee2,newdata=bee_mean[bee_mean$Family=="Melittidae" & bee_mean$Sex =="Female",]
                   )[,1])~
         bee_mean[bee_mean$Family=="Melittidae" & bee_mean$Sex =="Female",]$IT,pch=2,col="darkblue")
points(exp(predict(bee_p1,newdata=bee_mean[bee_mean$Family=="Melittidae" & bee_mean$Sex =="Female",]
                   ,re_formula = NA)[,1])~
         bee_mean[bee_mean$Family=="Melittidae" & bee_mean$Sex =="Female",]$IT,col="darkred")
points(exp(predict(bee_p1,newdata=bee_mean[bee_mean$Family=="Melittidae" & bee_mean$Sex =="Female",]
                  )[,1])~
         bee_mean[bee_mean$Family=="Melittidae" & bee_mean$Sex =="Female",]$IT,col="darkred",pch=2)


##HOVERFLIES

par(mfrow=c(1,2),pty="s")
plot(Spec.wgt~IT,hov_mean[hov_mean$Subfamily=="Syrphinae" & hov_mean$Sex =="Female",],
     cex=1,pch=8,col="black",main="Syrphinae",xlab="ITD (mm)",ylab="Dry weight (mg)",ylim=c(0,40))
points(exp(predict(hov2,newdata=hov_mean[hov_mean$Subfamily=="Syrphinae" & hov_mean$Sex =="Female",]
                   ,re_formula = NA)[,1])~
         hov_mean[hov_mean$Subfamily=="Syrphinae" & hov_mean$Sex =="Female",]$IT,col="darkblue")
points(exp(predict(hov2,newdata=hov_mean[hov_mean$Subfamily=="Syrphinae" & hov_mean$Sex =="Female",]
                   )[,1])~
         hov_mean[hov_mean$Subfamily=="Syrphinae" & hov_mean$Sex =="Female",]$IT,col="darkblue",pch=2)
points(exp(predict(hov7,newdata=hov_mean[hov_mean$Subfamily=="Syrphinae" & hov_mean$Sex =="Female",]
                   ,re_formula = NA)[,1])~
         hov_mean[hov_mean$Subfamily=="Syrphinae" & hov_mean$Sex =="Female",]$IT,col="darkred")
points(exp(predict(hov7,newdata=hov_mean[hov_mean$Subfamily=="Syrphinae" & hov_mean$Sex =="Female",]
                   )[,1])~
         hov_mean[hov_mean$Subfamily=="Syrphinae" & hov_mean$Sex =="Female",]$IT,col="darkred",pch=2)

#Eristalinae
plot(Spec.wgt~IT,hov_mean[hov_mean$Subfamily=="Eristalinae" & hov_mean$Sex =="Female",],
     cex=1,pch=8,col="black",main="Eristalinae",xlab="ITD (mm)",ylab="Dry weight (mg)",ylim=c(0,100))

points(exp(predict(hov2,newdata=hov_mean[hov_mean$Subfamily=="Eristalinae" & hov_mean$Sex =="Female",]
                   ,re_formula = NA)[,1])~
         hov_mean[hov_mean$Subfamily=="Eristalinae" & hov_mean$Sex =="Female",]$IT,col="darkblue")
points(exp(predict(hov2,newdata=hov_mean[hov_mean$Subfamily=="Eristalinae" & hov_mean$Sex =="Female",]
                  )[,1])~
         hov_mean[hov_mean$Subfamily=="Eristalinae" & hov_mean$Sex =="Female",]$IT,col="darkblue",pch=2)
points(exp(predict(hov7,newdata=hov_mean[hov_mean$Subfamily=="Eristalinae" & hov_mean$Sex =="Female",]
                   ,re_formula = NA)[,1])~
         hov_mean[hov_mean$Subfamily=="Eristalinae" & hov_mean$Sex =="Female",]$IT,col="darkred")
points(exp(predict(hov7,newdata=hov_mean[hov_mean$Subfamily=="Eristalinae" & hov_mean$Sex =="Female",]
                  )[,1])~
         hov_mean[hov_mean$Subfamily=="Eristalinae" & hov_mean$Sex =="Female",]$IT,col="darkred",pch=2)
