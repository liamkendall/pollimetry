
#Libraries

require(ggplot2)
require(gridExtra)

#Bees
BEE1=ggplot(bee_all,aes(x=(Spec.wgt),y=(IT),colour=Region))+coord_equal(xlim=c(0,320),ylim=c(0,8))+
geom_point(aes(),lwd=2,shape=1)+theme_bw()+theme(aspect.ratio=1)+ggtitle("ITD")
BEE1
BEE2=ggplot(bee_all,aes(x=(Spec.wgt),y=(IT),colour=Family))+coord_equal(xlim=c(0,320),ylim=c(0,8))+
  geom_point(aes(),lwd=2,shape=1)+theme_bw()+theme(aspect.ratio=1)+ggtitle("ITD")
BEE2
BEE3=ggplot(bee_all,aes(x=(Spec.wgt),y=(IT),colour=Family))+coord_equal(xlim=c(0,50),ylim=c(0.5,5))+
  geom_point(aes(),lwd=2,shape=1)+theme_bw()+theme(aspect.ratio=1)+ggtitle("ITD")
BEE3

Plots=grid.arrange(BEE1,BEE2,BEE3,ncol=3,nrow=1)

ggsave("bee_plots.pdf",Plots,device="pdf",width = 15, height =10)

#Hoverflies
HOV4=ggplot(hov_all,aes(x=log(Spec.wgt),y=log(IT),colour=Country))+coord_equal(xlim=c(-10,-0),ylim=c(-0.5,2.5))+
  geom_smooth(method="lm", formula=y~x,se=F)+  
  geom_point(aes(),lwd=2,shape=1)+theme_bw()+theme(aspect.ratio=1)+ggtitle("Hoverflies")

HOV5=ggplot(hov_all,aes(x=log(Spec.wgt),y=log(IT),colour=Subfamily))+coord_equal(xlim=c(-10,-0),ylim=c(-0.5,2.5))+
  geom_smooth(method="lm", formula=y~x,se=F)+  
  geom_point(aes(),lwd=2,shape=1)+theme_bw()+theme(aspect.ratio=1)+ggtitle("  ")

HOV6=ggplot(HOV,aes(x=log(Spec.wgt),y=log(IT),colour=Tribe))+coord_equal(xlim=c(-10,-0),ylim=c(-0.5,2.5))+
  geom_smooth(method="lm", formula=y~x,se=F)+  
  geom_point(aes(),lwd=2,shape=1)+theme_bw()+theme(aspect.ratio=1)+ggtitle("  ")

ggsave("Plots #2.pdf",grid.arrange(BEE4,BEE5,BEE6,HOV4,HOV5,HOV6,ncol=3,nrow=2),device="pdf",width = 15, height = 7.5)

