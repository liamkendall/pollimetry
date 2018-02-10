#Data frames

PredAllo=read.csv("PredAlloPoll10218.csv",header=T)

##Individual data frames
PredAllo.split=split(PredAllo,PredAllo$Taxa)
BEE=PredAllo.split[[1]]
HOV=PredAllo.split[[2]]

#Excluding Spain for now
HOV=HOV[1:162,]

#Libraries

require(ggplot2)
require(gridExtra)

#Hoverflies

HOV1=ggplot(HOV,aes(x=log(Spec.wgt),y=log(IT)))+coord_equal(xlim=c(-7.5,-2.5),ylim=c(-0.5,2.5))+
  geom_point(aes(colour=Country),lwd=2,shape=1)+theme_bw()+theme(aspect.ratio=1)+ggtitle("Hfly intertegular distance")

HOV2=ggplot(HOV,aes(x=log(Spec.wgt),y=log(BL)))+coord_equal(xlim=c(-7.5,-2.5),ylim=c(1.75,3.25))+
  geom_point(aes(colour=Country),lwd=2,shape=1)+theme_bw()+theme(aspect.ratio=1)+ggtitle("Hfly body length")

HOV3=ggplot(HOV,aes(x=log(Spec.wgt),y=log(ITxBL)))+coord_equal(xlim=c(-7.5,-2.5),ylim=c(1.75,5.25))+
  geom_point(aes(colour=Country),lwd=2,shape=1)+theme_bw()+theme(aspect.ratio=1)+ggtitle("Hfly IT x BL")

grid.arrange(HOV1,HOV2,HOV3,ncol=3)

#Bees
BEE1=ggplot(BEE,aes(x=log(Spec.wgt),y=log(IT)))+coord_equal(xlim=c(-10,-0),ylim=c(-0.5,2.5))+
  geom_point(aes(colour=Country),lwd=2,shape=1)+theme_bw()+theme(aspect.ratio=1)+ggtitle("Bee intertegular distance")
BEE1
BEE2=ggplot(BEE,aes(x=log(Spec.wgt),y=log(BL)))+coord_equal(xlim=c(-10,0),ylim=c(1,4))+
  geom_point(aes(colour=Country),lwd=2,shape=1)+theme_bw()+theme(aspect.ratio=1)+ggtitle("Bee body length")
BEE2
BEE3=ggplot(BEE,aes(x=log(Spec.wgt),y=log(ITxBL)))+coord_equal(xlim=c(-10,0),ylim=c(0,7))+
  geom_point(aes(colour=Country),lwd=2,shape=1)+theme_bw()+theme(aspect.ratio=1)+ggtitle("Bee IT x BL")
BEE3

Plots=grid.arrange(BEE1,BEE2,BEE3,HOV1,HOV2,HOV3,ncol=3,nrow=2)

ggsave("Plots #1.pdf",Plots,device="pdf",width = 15, height = 7.5)

