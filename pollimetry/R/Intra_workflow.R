#Intraspecific variation ~ Sample size

#X axis = standard deviation | error of each trait: WGT, IT, BL
#Y axis = sample size


##variation within a species rather than all speceis

#######Simple but long
#Sz=sample size

#Remove Cane

Sz_s=all[1:1791,]



##Repeat with full data frame, by genus 
str(all)
all[!duplicated(all[,1])]

str(Sz_s[unique(Sz_s[,19]),])
str(Specimen.sz)
Specimen.sz=data.frame(table(Sz_s$Species,Sz_s$Sex))
Specimen.sz[Specimen.sz == 0] <- NA
Specimen.sz=Specimen.sz[rowSums(is.na(Specimen.sz)) == 0,]
str(Specimen.sz)

Specimen.sz$Var1<- with(Specimen.sz, paste0(Var1, Var2))

##Creating data.frame
Sp.mean=data.frame(c(as.numeric(unlist(aggregate(Spec.wgt~Species+Sex,Sz_s,FUN="mean")[3]))
                     ,as.numeric(unlist(aggregate(IT~Species+Sex,Sz_s,FUN="mean")[3])),
                     as.numeric(unlist(aggregate(BL~Species+Sex,Sz_s,FUN="mean")[3]))
))
str(Sp.mean)
780/3
Spm=data.frame(rep(1:260,1))
Spm[,2]=Sp.mean[1:260,]
Spm[,3]=Sp.mean[261:520,]
Spm[,4]=Sp.mean[521:780,]
rownames(Spm)=Specimen.sz$Var1

Spm[,1]=Specimen.sz$Freq
data.frame(rep(1:260,1))
##Standard deviations

Sp.sd=data.frame(c(as.numeric(unlist(aggregate(Spec.wgt~Species+Sex,Sz_s,FUN="sd")[3])),
                   as.numeric(unlist(aggregate(IT~Species+Sex,Sz_s,FUN="sd")[3])),
                   as.numeric(unlist(aggregate(BL~Species+Sex,Sz_s,FUN="sd")[3]))
))
Spm[,5]=Sp.sd[1:260,]/sqrt(Spm[,1])
Spm[,6]=Sp.sd[261:520,]/sqrt(Spm[,1])
Spm[,7]=Sp.sd[521:780,]/sqrt(Spm[,1])

colnames(Spm)=c("N","Wgt","IT","Bl","Wse","ITse","BLse")
str(Spm)
#Remove singletons
Spm2=Spm[Spm[,1] > 1,]
Spm3=Spm[Spm[,1] > 2,]
Spm4=Spm[Spm[,1] > 5,]
Spm5=Spm[Spm[,1] > 10,]
str(Spm2)
ggplot(Spm2,aes(x=N))


##MElt not functioning currently
##Melt to long-form for plotting - #Spm2 = >2 specimens
melt(Spm2, id = as.factor(rep(1:260,1), measure = c("N","Wgt","IT","BL", "Wse", "ITse","BLSe"))
     str(Spm2)

Spm2_melt=melt(Spm2,id.variables=c(as.character(rownames(Spm2)),measure.vars=Spm2[,1:7])
)     
str(Spm2)
128*7
    Spm2_melt2=Spm2_melt[512:896,]
     
     Spm2_melt[,3]=rep(Spm2_melt[512:896,1],1)
     str(Spm2_melt2)
     colnames(Spm2_melt2)=c("VarSE","val","SzS")
     
     levels(Spm2_melt2$VarSE)=list(c("WgtM" , "ITM"  , "BlM" ,  "N","Wgt" , "IT","BL"))
     ##THE plot
     #Sz_p1_unt=ggplot(Spm2_melt2, aes( x = SzS , y = val,col=VarSE))+
     scale_color_hue(l=60, c=40)+theme_bw()+
       geom_point(cex=2,pch=1)+
       theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
       geom_smooth(method="glm",formula=y ~ x,method.args = list(family = gaussian(link = "log")),se=FALSE)+
       xlim(0,251)+ylim(0,4)
     Sz_p1_unt
     #Sz_p1_unt2=ggplot(Spm2_melt2, aes( x = SzS , y = val,col=VarSE))+
     scale_color_hue(l=60, c=40)+theme_bw()+
       theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
       geom_point(cex=2,pch=1)+coord_equal(ratio=1)+ coord_cartesian(ylim=c(0, 1),xlim=c(0,100))+
       geom_smooth(method="glm",formula=(y) ~ x,method.args = list(family = gaussian(link="log")),se=FALSE)
     
     p=grid.arrange(Sz_p1_unt,Sz_p1_unt2,nrow=1,ncol=2)
     p
     Sz_p1_unt2
     
     
     #Model of variance ~ Sample size
     ##residuals are terrible - not sure exactly how to transform as SEM proportional to x axis(Sample size)
     Var_lm=glm(val~0+log(SzS)+VarSE,family = gaussian,Spm2_melt2)
     plot(Var_lm)
     Var_lm_out=tidy(Var_lm)
     Var_lm_out
     
     ##provide distirbution of values absed on level of intraspecific variation in sample size
     -1.75+0.14
     rnorm
     lognormal(var=Spm$$SD ~for species)
     
     ###no it would be flat distribution