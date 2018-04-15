#Data frames

PredAllo=read.csv("PredAlloPoll10218.csv",header=T)

##Individual data frames
PredAllo.split=split(PredAllo,PredAllo$Taxa)
BEE=PredAllo.split[[1]]
HOV=PredAllo.split[[2]]

#Libraries

require(ggplot2)
require(gridExtra)

#Hoverflies

HOV1=ggplot(HOV,aes(x=log(Spec.wgt),y=log(IT),colour=Region))+coord_equal(xlim=c(-7.5,-2.5),ylim=c(-0.5,2.5))+
  geom_smooth(method="lm", formula=y~x,se=F)+
  geom_point(aes(),lwd=2,shape=1)+theme_bw()+theme(aspect.ratio=1)+ggtitle("Hfly intertegular distance")

HOV2=ggplot(HOV,aes(x=log(Spec.wgt),y=log(BL),colour=Region))+coord_equal(xlim=c(-7.5,-2.5),ylim=c(1.75,3.25))+
  geom_smooth(method="lm", formula=y~x,se=F)+
  geom_point(aes(),lwd=2,shape=1)+theme_bw()+theme(aspect.ratio=1)+ggtitle("Hfly body length")

HOV3=ggplot(HOV,aes(x=log(Spec.wgt),y=log(ITxBL),colour=Region))+coord_equal(xlim=c(-7.5,-2.5),ylim=c(1.75,5.25))+
  geom_smooth(method="lm", formula=y~x,se=F)+
  geom_point(aes(),lwd=2,shape=1)+theme_bw()+theme(aspect.ratio=1)+ggtitle("Hfly IT x BL")

grid.arrange(HOV1,HOV2,HOV3,ncol=3)
bee_all$Measurement
#Bees
BEE1=ggplot(bee_mean,aes(x=log(Spec.wgt),y=log(IT),colour=Measurement))+coord_equal(xlim=c(-10,-0),ylim=c(-0.5,2.5))+
  geom_smooth(method="lm", formula=y~x,se=F)+
  geom_point(aes(),lwd=2,shape=1)+theme_bw()+theme(aspect.ratio=1)+ggtitle("Bee intertegular distance")
BEE1
BEE2=ggplot(bee_all,aes(x=log(Spec.wgt),y=log(BL),colour=Region))+coord_equal(xlim=c(-10,0),ylim=c(1,4))+
  geom_smooth(method="lm", formula=y~x,se=F)+
  geom_point(aes(),lwd=2,shape=1)+theme_bw()+theme(aspect.ratio=1)+ggtitle("Bee body length")
BEE2


Plots=grid.arrange(BEE1,BEE2,HOV1,HOV2,ncol=3,nrow=2)

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
