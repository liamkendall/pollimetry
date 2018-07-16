##BODY LENGTH VS ITD

BL_poll_all=poll_all[!is.na(poll_all$BL),]
str(BL_poll_all)

bl_split=split(BL_poll_all,BL_poll_all$Superfamily
)
bee_bl=bl_split$Apoidea
hov_bl=bl_split$Syrphoidea

bee_blit=lm(log(Spec.wgt)~log(IT),bee_bl)

bee_blbl=lm(log(Spec.wgt)~log(BL),bee_bl)

summary(lm(log(bee_bl$IT)~log(bee_bl$BL),data=bee_bl))

summary(lm(log(hov_bl$IT)~log(hov_bl$BL),data=hov_bl))


cor.test(x=log(hov_bl$IT),y=log(hov_bl$BL),data=hov_bl)

summary(bee_blit)

summary(bee_blbl)

AICc(bee_blit,bee_blbl)

hov_blit=lm(log(Spec.wgt)~log(IT),hov_bl)

hov_blbl=lm(log(Spec.wgt)~log(BL),hov_bl)

summary(hov_blit)
summary(hov_blbl)

anova(bee_blit,bee_blbl)

anova(hov_blit,hov_blbl)
