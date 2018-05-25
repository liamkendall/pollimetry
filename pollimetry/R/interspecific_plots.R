##INTERSPECIFIC PLOTS

bee1=ggplot(bee_mean,aes(x=log(IT),y=log(Spec.wgt),col=Region))+geom_point(col=1,pch=1)+
  geom_smooth(method="lm",se=FALSE)+theme_bw()
bee2=ggplot(bee_mean,aes(x=log(IT),y=log(Spec.wgt),col=Sex))+geom_point(col=1,pch=1)+
  geom_smooth(method="lm",se=FALSE)+theme_bw()
bee3=ggplot(bee_mean,aes(x=log(IT),y=log(Spec.wgt),col=Family))+geom_point(col=1,pch=1)+
  geom_smooth(method="lm",se=FALSE)+theme_bw()


bee1=ggplotGrob(bee1)
bee2=ggplotGrob(bee2)
bee3=ggplotGrob(bee3)
grid.draw(cbind(bee1, bee2,bee3, size = "first"))

hov1=ggplot(hov_mean,aes(x=log(IT),y=log(Spec.wgt),col=Region))+geom_point(col=1,pch=1)+
  geom_smooth(method="lm",se=FALSE)+theme_bw()
hov2=ggplot(hov_mean,aes(x=log(IT),y=log(Spec.wgt),col=Sex))+geom_point(col=1,pch=1)+
  geom_smooth(method="lm",se=FALSE)+theme_bw()
hov3=ggplot(hov_mean,aes(x=log(IT),y=log(Spec.wgt),col=Subfamily))+geom_point(col=1,pch=1)+
  geom_smooth(method="lm",se=FALSE)+theme_bw()


hov1=ggplotGrob(hov1)
hov2=ggplotGrob(hov2)
hov3=ggplotGrob(hov3)
grid.draw(cbind(hov1, hov2,hov3, size = "first"))

3.73 x 12.9

##INTRASPECIFIC PLOTS

int_bee1=ggplot(data=bee_top5,aes(y=log(Spec.wgt),x=log(IT),col=Species))+
  geom_point(pch=1)+
  geom_smooth(method="lm",se=FALSE)+
  theme_bw()+ggtitle("Bees")
int_hov1=ggplot(data=hov_top5,aes(y=log(Spec.wgt),x=log(IT),col=Species))+
  geom_point(pch=1)+
  geom_smooth(method="lm",se=FALSE)+
  theme_bw()+ggtitle("Hoverflies")

int_bee2=ggplotGrob(int_bee1)
int_hov2=ggplotGrob(int_hov1)
grid.draw(cbind(int_bee2, int_hov2, size = "first"))

3.73 x 9.9

##PHYLO

bee_wgt_phy=contMap(bee_pruned,WGT,plot=FALSE)
plot(bee_wgt_phy,fsize=c(0.4,1),lwd=0.75,leg.txt="log(WGT)")
