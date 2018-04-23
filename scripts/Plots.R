
#Libraries

require(ggplot2)
require(gridExtra)

#Bees
BEE1=ggplot(bee_all,aes(x=log(Spec.wgt),y=log(IT),colour=Genus))+coord_equal(xlim=c(-10,-0),ylim=c(-0.5,2.5))+
geom_point(aes(),lwd=2,shape=1)+theme_bw()+theme(aspect.ratio=1)+ggtitle("ITD")
BEE1
BEE2=ggplot(bee_all,aes(x=(Spec.wgt),y=(IT),colour=Region))+coord_equal(xlim=c(-10,0),ylim=c(-0.5,2.5))+
  geom_smooth(method="lm", formula=y~x,se=F)+
  geom_point(aes(),lwd=2,shape=1)+theme_bw()+theme(aspect.ratio=1)+ggtitle("ITD")
BEE2
BEE3=ggplot(bee_all,aes(x=(Spec.wgt),y=(IT),colour=Family))+coord_equal(xlim=c(-10,0),ylim=c(-0.5,2.5))+
  geom_smooth(method="lm", formula=y~x,se=F)+
  geom_point(aes(),lwd=2,shape=3)+theme_bw()+theme(aspect.ratio=1)+ggtitle("ITD")
BEE3
BEE4=ggplot(bee_all,aes(x=(Spec.wgt),y=(IT),colour=Cl_simp))+coord_equal(xlim=c(-10,0),ylim=c(-0.5,2.5))+
  geom_smooth(method="lm", formula=y~x,se=F)+
  geom_point(aes(),lwd=2,shape=1)+theme_bw()+theme(aspect.ratio=1)+ggtitle("ITD")
BEE4



Plots=grid.arrange(BEE1,BEE2,BEE3,BEE4,ncol=2,nrow=2)

ggsave("Plots #1.pdf",Plots,device="pdf",width = 15, height = 7.5)

#Bees
BEE4=ggplot(BEE,aes(x=log(Spec.wgt),y=log(IT_cor),colour=Region))+coord_equal(xlim=c(-10,-0),ylim=c(-0.5,2.5))+
geom_smooth(method="lm", formula=y~x,se=F)+  
  geom_point(aes(),lwd=2,shape=1)+theme_bw()+theme(aspect.ratio=1)+ggtitle("Bees")

BEE5=ggplot(bee_all,aes(x=log(Spec.wgt),y=log(IT_cor),colour=Family))+coord_equal(xlim=c(-10,-0),ylim=c(-0.5,2.5))+
  geom_smooth(method="lm", formula=y~x,se=F)+  
  geom_point(aes(),lwd=2,shape=1)+theme_bw()+theme(aspect.ratio=1)+ggtitle("  ")

BEE6=ggplot(bee_all,aes(x=log(Spec.wgt),y=log(IT_cor),colour=Subfamily))+coord_equal(xlim=c(-10,-0),ylim=c(-0.5,2.5))+
  geom_smooth(method="lm", formula=y~x,se=F)+  
  geom_point(aes(),lwd=2,shape=1)+theme_bw()+theme(aspect.ratio=1)+ggtitle("  ")



#Hoverflies
HOV4=ggplot(hov_all,aes(x=log(Spec.wgt),y=log(IT_cor),colour=Country))+coord_equal(xlim=c(-10,-0),ylim=c(-0.5,2.5))+
  geom_smooth(method="lm", formula=y~x,se=F)+  
  geom_point(aes(),lwd=2,shape=1)+theme_bw()+theme(aspect.ratio=1)+ggtitle("Hoverflies")

HOV5=ggplot(hov_all,aes(x=log(Spec.wgt),y=log(IT_cor),colour=Subfamily))+coord_equal(xlim=c(-10,-0),ylim=c(-0.5,2.5))+
  geom_smooth(method="lm", formula=y~x,se=F)+  
  geom_point(aes(),lwd=2,shape=1)+theme_bw()+theme(aspect.ratio=1)+ggtitle("  ")

HOV6=ggplot(HOV,aes(x=log(Spec.wgt),y=log(IT),colour=Tribe))+coord_equal(xlim=c(-10,-0),ylim=c(-0.5,2.5))+
  geom_smooth(method="lm", formula=y~x,se=F)+  
  geom_point(aes(),lwd=2,shape=1)+theme_bw()+theme(aspect.ratio=1)+ggtitle("  ")

ggsave("Plots #2.pdf",grid.arrange(BEE4,BEE5,BEE6,HOV4,HOV5,HOV6,ncol=3,nrow=2),device="pdf",width = 15, height = 7.5)


plo
