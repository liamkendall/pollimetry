##bee foraging workflow

forage=read.csv(file="data/bee_foraging.csv",header=T)
head(forage)
str(forage)
table(forage$Tribe)

forage[1:134,c("Spec.wgt")]
forage[1:134,c("IT")]

options(na.action=na.ignore)
forage[1:134,c("Spec.wgt")]=exp(predict(Flm,newdata=forage[1:134,]))
forage=forage[-240,]
forage=forage[-239,]

forage[135:263,c("Spec.wgt")]=exp(predict(Genus.lm,newdata=forage[135:265,]))
forage[135:263,c("Spec.wgt")]=exp(predict(Tribe.lm,newdata=forage[135:263,]))

plot(log(Max)~log(Spec.wgt),col=Tribe,data=forage)
points(log(Mean)~log(Spec.wgt),col=Family,data=forage)
summary(glmer(log(IT)~Type+
             log(Max)+(1|Reference)+(1|Species),family="poisson",forage))
