bee_ics=rbind(cbind(median(bee_mcmc_list[[1]]$R2),
median(bee_mcmc_list[[2]]$R2),
median(bee_mcmc_list[[3]]$R2),
median(bee_mcmc_list[[4]]$R2),
median(bee_mcmc_list[[5]]$R2),
median(bee_mcmc_list[[6]]$R2),
median(bee_mcmc_list[[7]]$R2),
median(bee_mcmc_list[[8]]$R2),
median(bee_mcmc_list[[9]]$R2),
median(bee_mcmc_list[[10]]$R2)),

cbind(bee_mcmc_list[[1]]$kfold$estimates[3,1],
bee_mcmc_list[[2]]$kfold$estimates[3,1],
bee_mcmc_list[[3]]$kfold$estimates[3,1],
bee_mcmc_list[[4]]$kfold$estimates[3,1],
bee_mcmc_list[[5]]$kfold$estimates[3,1],
bee_mcmc_list[[6]]$kfold$estimates[3,1],
bee_mcmc_list[[7]]$kfold$estimates[3,1],
bee_mcmc_list[[8]]$kfold$estimates[3,1],
bee_mcmc_list[[9]]$kfold$estimates[3,1],
bee_mcmc_list[[10]]$kfold$estimates[3,1]),

cbind(
  rmse(exp(predict(bee_mcmc_list[[1]],newdata = bee_mean)[,1]),bee_mean$Spec.wgt),
  rmse(exp(predict(bee_mcmc_list[[2]],newdata = bee_mean)[,1]),bee_mean$Spec.wgt),
  rmse(exp(predict(bee_mcmc_list[[3]],newdata = bee_mean)[,1]),bee_mean$Spec.wgt),
  rmse(exp(predict(bee_mcmc_list[[4]],newdata = bee_mean)[,1]),bee_mean$Spec.wgt),
  rmse(exp(predict(bee_mcmc_list[[5]],newdata = bee_mean)[,1]),bee_mean$Spec.wgt),
  rmse(exp(predict(bee_mcmc_list[[6]],newdata = bee_mean)[,1]),bee_mean$Spec.wgt),
  rmse(exp(predict(bee_mcmc_list[[7]],newdata = bee_mean)[,1]),bee_mean$Spec.wgt),
  rmse(exp(predict(bee_mcmc_list[[8]],newdata = bee_mean)[,1]),bee_mean$Spec.wgt),
  rmse(exp(predict(bee_mcmc_list[[9]],newdata = bee_mean)[,1]),bee_mean$Spec.wgt),
  rmse(exp(predict(bee_mcmc_list[[10]],newdata = bee_mean)[,1]),bee_mean$Spec.wgt)))


write.csv(bee_ics,"bee_ics.csv")

hov_ics=rbind(
cbind(median(hov_mcmc_list[[1]]$R2),
      median(hov_mcmc_list[[2]]$R2),
      median(hov_mcmc_list[[3]]$R2),
      median(hov_mcmc_list[[4]]$R2),
      median(hov_mcmc_list[[5]]$R2),
      median(hov_mcmc_list[[6]]$R2),
      median(hov_mcmc_list[[7]]$R2),
      median(hov_mcmc_list[[8]]$R2),
      median(hov_mcmc_list[[9]]$R2)),

cbind(hov_kfold_list[[1]]$estimates[3,1],
      hov_kfold_list[[2]]$estimates[3,1],
      hov_kfold_list[[3]]$estimates[3,1],
      hov_kfold_list[[4]]$estimates[3,1],
      hov_kfold_list[[5]]$estimates[3,1],
      hov_kfold_list[[6]]$estimates[3,1],
      hov_kfold_list[[7]]$estimates[3,1],
      hov_kfold_list[[8]]$estimates[3,1],
      hov_kfold_list[[9]]$estimates[3,1]),cbind(rmse(exp(predict(hov1)[,1]),hov_mean$Spec.wgt),
                                                rmse(exp(predict(hov2)[,1]),hov_mean$Spec.wgt),
                                                rmse(exp(predict(hov3)[,1]),hov_mean$Spec.wgt),
                                                rmse(exp(predict(hov4)[,1]),hov_mean$Spec.wgt),
                                                rmse(exp(predict(hov5)[,1]),hov_mean$Spec.wgt),
                                                rmse(exp(predict(hov6)[,1]),hov_mean$Spec.wgt),
                                                rmse(exp(predict(hov7)[,1]),hov_mean$Spec.wgt),
                                                rmse(exp(predict(hov8)[,1]),hov_mean$Spec.wgt),
                                                rmse(exp(predict(hov9)[,1]),hov_mean$Spec.wgt)))

write.csv(hov_ics,"hov_ics.csv")

hov_mcmc_list[[7]]

hov_mcmc_list[[1]]$fit

rmse(Cane(bee_mean$IT),bee_mean$Spec.wgt)


