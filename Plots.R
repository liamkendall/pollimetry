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
  geom_point(aes(colour=Country))+theme_bw()+theme(aspect.ratio=1)+ggtitle("Intertegular distance")
+geom_smooth(method='lm',formula=log(HOV$Spec.wgt)~log(HOV$IT))

HOV2=ggplot(HOV,aes(x=log(Spec.wgt),y=log(BL)))+coord_equal(xlim=c(-7.5,-2.5),ylim=c(1.75,3.25))+
  geom_point(aes(colour=Country))+theme_bw()+theme(aspect.ratio=1)+ggtitle("Body length")

HOV3=ggplot(HOV,aes(x=log(Spec.wgt),y=log(ITxBL)))+coord_equal(xlim=c(-7.5,-2.5),ylim=c(1.75,5.25))+
  geom_point(aes(colour=Country))+theme_bw()+theme(aspect.ratio=1)+ggtitle("IT x BL")

grid.arrange(HOV1,HOV2,HOV3,ncol=3)

#Bees
BEE1=ggplot(BEE,aes(x=log(Spec.wgt),y=log(IT)))+coord_equal(xlim=c(-10,-0),ylim=c(-0.5,2.5))+
  geom_point(aes(colour=Country))+theme_bw()+theme(aspect.ratio=1)+ggtitle("Intertegular distance")
BEE1
BEE2=ggplot(BEE,aes(x=log(Spec.wgt),y=log(BL)))+coord_equal(xlim=c(-10,0),ylim=c(1,4))+
  geom_point(aes(colour=Country))+theme_bw()+theme(aspect.ratio=1)+ggtitle("Body length")
BEE2
BEE3=ggplot(BEE,aes(x=log(Spec.wgt),y=log(ITxBL)))+coord_equal(xlim=c(-10,0),ylim=c(0,7))+
  geom_point(aes(colour=Country))+theme_bw()+theme(aspect.ratio=1)+ggtitle("IT x BL")
BEE3
grid.arrange(BEE1,BEE2,BEE3,ncol=3)
