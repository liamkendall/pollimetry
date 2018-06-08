##Relationship between BL AND IT
bee_bl=bee_all[!is.na(bee_all$BL),]
hov_bl=hov_all[!is.na(hov_all$BL),]

bee_bl_mean=aggregate(Latitude~Family+Subfamily+Tribe+Region+Subfamily+Genus+Species+Sex,bee_bl,mean)

bee_bl_mean$Spec.wgt=as.numeric(unlist(aggregate(Spec.wgt~Family+Subfamily+Tribe+Region+
                                                Subfamily+Genus+Species+Sex,bee_bl,mean)[8]))
bee_bl_mean$IT=(as.numeric(unlist(aggregate(IT~Family+Subfamily+Tribe+Region+
                                           Subfamily+Genus+Species+Sex,bee_bl,mean)[8])))
bee_bl_mean$BL=(as.numeric(unlist(aggregate(BL~Family+Subfamily+Tribe+Region+
                                              Subfamily+Genus+Species+Sex,bee_bl,mean)[8])))


hov_bl_mean=aggregate(Latitude~Family+Subfamily+Tribe+Region+Subfamily+Genus+Species+Sex,hov_bl,mean)

hov_bl_mean$Spec.wgt=as.numeric(unlist(aggregate(Spec.wgt~Family+Subfamily+Tribe+Region+
                                                   Subfamily+Genus+Species+Sex,hov_bl,mean)[8]))
hov_bl_mean$IT=(as.numeric(unlist(aggregate(IT~Family+Subfamily+Tribe+Region+
                                              Subfamily+Genus+Species+Sex,hov_bl,mean)[8])))
hov_bl_mean$BL=(as.numeric(unlist(aggregate(BL~Family+Subfamily+Tribe+Region+
                                              Subfamily+Genus+Species+Sex,hov_bl,mean)[8])))


###BEES

summary(lm(log(IT)~ log(BL),bee_bl_mean))

summary(lm(log(Spec.wgt)~log(BL),bee_bl_mean))
summary(lm(log(Spec.wgt)~log(IT),bee_bl_mean))



###HOV
summary(lm(log (IT)~ log(BL),hov_bl_mean))

summary(lm(log (Spec.wgt)~log(BL),hov_bl_mean))
summary(lm(log (Spec.wgt)~log (IT),hov_bl_mean))



hov_bl_rmse=cbind(rmse((hov_bl$Spec.wgt),(lengthsize(hov_bl$BL,"DIP")[,1])),
rmse((hov_bl$Spec.wgt),(lengthsize(hov_bl$BL,"DIP")[,2])),
rmse((hov_bl$Spec.wgt),(lengthsize(hov_bl$BL,"DIP")[,3])),
rmse((hov_bl$Spec.wgt),(lengthsize(hov_bl$BL,"DIP")[,4])),
rmse((hov_bl$Spec.wgt),(lengthsize(hov_bl$BL,"DIP")[,5])),
rmse((hov_bl$Spec.wgt),(lengthsize(hov_bl$BL,"DIP")[,6])),
rmse((hov_bl$Spec.wgt),(lengthsize(hov_bl$BL,"DIP")[,7])),
rmse((hov_bl$Spec.wgt),(lengthsize(hov_bl$BL,"DIP")[,8])),
rmse((hov_bl$Spec.wgt),(lengthsize(hov_bl$BL,"DIP")[,9])),
rmse((hov_bl$Spec.wgt),(lengthsize(hov_bl$BL,"DIP")[,10])))

mean(hov_bl_rmse)
7.993184/sqrt(10)
2.527667

bee_bl_rmse=cbind(rmse((bee_bl$Spec.wgt), (lengthsize(bee_bl$BL,"HYM")[,1])),
rmse( (bee_bl$Spec.wgt), (lengthsize(bee_bl$BL,"HYM")[,2])),
rmse( (bee_bl$Spec.wgt), (lengthsize(bee_bl$BL,"HYM")[,3])),
rmse( (bee_bl$Spec.wgt), (lengthsize(bee_bl$BL,"HYM")[,4])),
rmse( (bee_bl$Spec.wgt), (lengthsize(bee_bl$BL,"HYM")[,5])),
rmse( (bee_bl$Spec.wgt), (lengthsize(bee_bl$BL,"HYM")[,6])),
rmse( (bee_bl$Spec.wgt), (lengthsize(bee_bl$BL,"HYM")[,7])),
rmse( (bee_bl$Spec.wgt), (lengthsize(bee_bl$BL,"HYM")[,8])),
rmse( (bee_bl$Spec.wgt), (lengthsize(bee_bl$BL,"HYM")[,9])),
rmse( (bee_bl$Spec.wgt), (lengthsize(bee_bl$BL,"HYM")[,10])),
rmse( (bee_bl$Spec.wgt), (lengthsize(bee_bl$BL,"HYM")[,11])),
rmse( (bee_bl$Spec.wgt), (lengthsize(bee_bl$BL,"HYM")[,12])))

mean(bee_bl_rmse)
36.36586 +- 8.295096
sd(bee_bl_rmse)/sqrt(12)

rmse((bee_all$Spec.wgt),(Cane(IT=bee_all$IT)))
