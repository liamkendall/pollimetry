#Cane training test sets

validation1=model1[fold$subsets[fold$which == 1], ]
validation2=model1[fold$subsets[fold$which == 2], ]
validation3=model1[fold$subsets[fold$which == 3], ]
validation4=model1[fold$subsets[fold$which == 4], ]
validation5=model1[fold$subsets[fold$which == 5], ]

str(validation2)
VC1=Cane(IT=validation1$IT)
VC2=Cane(IT=validation2$IT)
VC3=Cane(IT=validation3$IT)
VC4=Cane(IT=validation4$IT)
VC5=Cane(IT=validation5$IT)

rmse(log(VC1),log(validation1$Spec.wgt))
rmse(log(VC2),log(validation2$Spec.wgt))
rmse(log(VC3),log(validation3$Spec.wgt))
rmse(log(VC4),log(validation4$Spec.wgt))
rmse(log(VC5),log(validation5$Spec.wgt))
