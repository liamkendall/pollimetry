##INTERSPECIFIC PLOTS

#LOG LM
bee1=ggplot(bee_mean,aes(x=log(IT),y=log(Spec.wgt),col=Region))+geom_point(col=1,pch=1)+
  geom_smooth(method = "lm", formula = y ~ x,se=FALSE)+theme_bw()
bee2=ggplot(bee_mean,aes(x=log(IT),y=log(Spec.wgt),col=Sex))+geom_point(col=1,pch=1)+
  geom_smooth(method = "lm",se=FALSE)+theme_bw()
bee3=ggplot(bee_mean,aes(x=log(IT),y=log(Spec.wgt),col=Family))+geom_point(col=1,pch=1)+
  geom_smooth(method = "lm",se=FALSE)+theme_bw()

bee4=ggplot(bee_phylo,aes(x=log(IT),y=log(Spec.wgt),col=Region))+geom_point(col=1,pch=1)+
  geom_smooth(method = "lm", formula = y ~ x,se=FALSE)+theme_bw()
bee5=ggplot(bee_mean,aes(x=log(IT),y=log(Spec.wgt),col=Sex))+geom_point(col=1,pch=1)+
  geom_smooth(method = "lm",se=FALSE)+theme_bw()
bee6=ggplot(bee_mean,aes(x=log(IT),y=log(Spec.wgt),col=Family))+geom_point(col=1,pch=1)+
  geom_smooth(method = "lm",se=FALSE)+theme_bw()

#GFAM
bee1=ggplot(bee_phylo,aes(y=IT,x=Spec.wgt,col=Region))+geom_point(col=1,pch=1)+
geom_smooth(method = "gam", formula = y ~ s(log(x)),se=FALSE)+theme_bw()
bee2=ggplot(bee_mean,aes(y=IT,x=Spec.wgt,col=Sex))+geom_point(col=1,pch=1)+
  geom_smooth(method = "gam", formula = y ~ s(log(x)),se=FALSE)+theme_bw()
bee3=ggplot(bee_mean,aes(y=IT,x=Spec.wgt))+geom_point(col=1,pch=1)+
  geom_smooth(method = "gam", formula = y ~ s(log(x)),se=FALSE)+theme_bw()#+facet_grid(Family ~.)


bee1=ggplotGrob(bee1)
bee2=ggplotGrob(bee2)
bee3=ggplotGrob(bee3)
grid.draw(cbind(bee1, bee2,bee3, size = "first"))

hov1=ggplot(hov_mean,aes(x=(IT),y=(Spec.wgt),col=Region))+geom_point(col=1,pch=1)+
  geom_smooth(method="lm",se=FALSE)+theme_bw()
hov2=ggplot(hov_mean,aes(x=(IT),y=(Spec.wgt),col=Sex))+geom_point(col=1,pch=1)+
  geom_smooth(method="lm",se=FALSE)+theme_bw()
hov3=ggplot(hov_mean,aes(x=(IT),y=(Spec.wgt),col=Subfamily))+geom_point(col=1,pch=1)+
  geom_smooth(method="lm",se=FALSE)+theme_bw()


hov1=ggplotGrob(hov1)
hov2=ggplotGrob(hov2)
hov3=ggplotGrob(hov3)
grid.draw(cbind(hov1, hov2,hov3, size = "first"))

lay <- rbind(c(1,2,3),
             c(4,5,6))
plot_grid(bee1,bee2,bee3,hov1,hov2,hov3,ncol=3,nrow=2,layout_matrix = lay)

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

##PHYLO #GENERA
bee.plot=read.tree(file="raw_data/Bee_phylogeny_Hedtke_etal2013/12862_2013_2375_MOESM3_ESM.txt",keep.multi = TRUE)
##Use tree 1 (376 genera) #Genera-level phylogney
bee.plot=bee.plot[[1]]
bee.plot=as.phylo(bee.plot)
bee.plot=root(bee.plot,outgroup="Tachysphex")
#bee.plot=force.ultrametric(bee.plot) #Not sure if this is required
bee.plot=drop.tip(bee.plot, setdiff(bee.plot$tip.label,bee_phylo$Genus))
bee.plot=genus.to.species.tree(bee.plot, species=bee_phylo$Species)


bee_wgt_phy=contMap(bee.tree,WGT,plot=FALSE)

plot(bee_wgt_phy,ftype="off",fsize=c(0.4,0.4),lwd=0.95,xlim=c(0,1.25*max(nodeHeights(tree))),
     ftype="i",leg.txt="ln(Body size)")

mel_tips<-c("Macropis_europaea","Hesperapis_carinata")
megachil_tips<-c("Megachile_captionis","Anthidium_sticticum")
apid_tips<-c("Ceratina_mikmaqi","Amegilla_chlorocyanea")
andren_tips<-c("Panurgus_dargius","Andrena_bicolor")
halicti_tips<-c("Dufourea_marginata","Lipotriches_flavoviridis")
colletid_tips<-c("Leioproctus_leisp1","Hylaeus_communis")
cladelabels(bee_pruned,node=findMRCA(bee_pruned,mel_tips),text="Melittidae",
            orientation="horizontal",offset=8)
cladelabels(bee_pruned,node=findMRCA(bee_pruned,megachil_tips),text="Megachilidae",
            orientation="horizontal",offset=8)
cladelabels(bee_pruned,node=findMRCA(bee_pruned,apid_tips),text="Apidae",
            orientation="horizontal",offset=8)
cladelabels(bee_pruned,node=findMRCA(bee_pruned,andren_tips),text="Andrenidae",
            orientation="horizontal",offset=8)
cladelabels(bee_pruned,node=findMRCA(bee_pruned,halicti_tips),text="Halictidae",
            orientation="horizontal",offset=8)
cladelabels(bee_pruned,node=findMRCA(bee_pruned,colletid_tips),text="Colletidae",
            orientation="horizontal",offset=8)


Region=as.data.frame(bee_phylo$Region)
rownames(Region)=rownames(bee_phylo)
Region<-as.matrix((Region))[,1]


fmode<-as.factor(setNames(bee_phylo$Region,rownames(bee_phylo)))
dotTree(bee_pruned,fmode,colors=setNames(c("blue","red","green"),
                                       c("Australia","Europe","North America")),ftype="off",fsize=c(0.4,1))

##DENSITY PLOTS
ggplot(bee_mean,aes(x=log(Spec.wgt),col=Region))+geom_density(cex=0.75)+theme_bw()+facet_grid(Family ~.)
ggplot(bee_mean,aes(x=log(Spec.wgt),col=Sex))+geom_density(cex=0.75)+theme_bw()+facet_grid(Family ~.)

ggplot(hov_mean,aes(x=log(Spec.wgt),col=Region))+geom_density(cex=0.75)+theme_bw()+facet_grid(Subfamily~.)
ggplot(hov_mean,aes(x=log(Spec.wgt),col=Sex))+geom_density(cex=0.75)+theme_bw()+facet_grid(Subfamily~.)

ggplot(bee_mean,aes(x=log(Spec.wgt),col=Country))+geom_density(cex=0.75)+theme_bw()

ggplot(hov_all,aes(x=log(Spec.wgt),col=Country))+geom_density(cex=0.75)+theme_bw()

bee_RMSE
Hov_RMSE
