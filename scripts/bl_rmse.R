lengthsize(hov_bl$BL,"DIP")

rmse((hov_bl$Spec.wgt),(lengthsize(hov_bl$BL,"DIP")[,1]))
rmse((hov_bl$Spec.wgt),(lengthsize(hov_bl$BL,"DIP")[,2]))
rmse((hov_bl$Spec.wgt),(lengthsize(hov_bl$BL,"DIP")[,3]))
rmse((hov_bl$Spec.wgt),(lengthsize(hov_bl$BL,"DIP")[,4]))
rmse((hov_bl$Spec.wgt),(lengthsize(hov_bl$BL,"DIP")[,5]))
rmse((hov_bl$Spec.wgt),(lengthsize(hov_bl$BL,"DIP")[,6]))
rmse((hov_bl$Spec.wgt),(lengthsize(hov_bl$BL,"DIP")[,7]))
rmse((hov_bl$Spec.wgt),(lengthsize(hov_bl$BL,"DIP")[,8]))
rmse((hov_bl$Spec.wgt),(lengthsize(hov_bl$BL,"DIP")[,9]))
rmse((hov_bl$Spec.wgt),(lengthsize(hov_bl$BL,"DIP")[,10]))
rmse((hov_bl$Spec.wgt),(lengthsize(hov_bl$BL,"DIP")[,11]))


rmse(log(bee_bl$Spec.wgt),log(lengthsize(bee_bl$BL,"HYM")[,1]))
rmse(log(bee_bl$Spec.wgt),log(lengthsize(bee_bl$BL,"HYM")[,2]))
rmse(log(bee_bl$Spec.wgt),log(lengthsize(bee_bl$BL,"HYM")[,3]))
rmse(log(bee_bl$Spec.wgt),log(lengthsize(bee_bl$BL,"HYM")[,4]))
rmse(log(bee_bl$Spec.wgt),log(lengthsize(bee_bl$BL,"HYM")[,5]))
rmse(log(bee_bl$Spec.wgt),log(lengthsize(bee_bl$BL,"HYM")[,6]))
rmse(log(bee_bl$Spec.wgt),log(lengthsize(bee_bl$BL,"HYM")[,7]))
rmse(log(bee_bl$Spec.wgt),log(lengthsize(bee_bl$BL,"HYM")[,8]))
rmse(log(bee_bl$Spec.wgt),log(lengthsize(bee_bl$BL,"HYM")[,9]))
rmse(log(bee_bl$Spec.wgt),log(lengthsize(bee_bl$BL,"HYM")[,10]))
rmse(log(bee_bl$Spec.wgt),log(lengthsize(bee_bl$BL,"HYM")[,11]))
rmse(log(bee_bl$Spec.wgt),log(lengthsize(bee_bl$BL,"HYM")[,12]))



exp(0.4)
rmse(log(bee_all$Spec.wgt),log(Cane(IT=bee_all$IT)))
