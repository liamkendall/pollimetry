#Cane training test sets

validation1=model1[fold$subsets[fold$which == 1], ]
validation2=model1[fold$subsets[fold$which == 2], ]
validation3=model1[fold$subsets[fold$which == 3], ]
validation4=model1[fold$subsets[fold$which == 4], ]
validation5=model1[fold$subsets[fold$which == 5], ]
validation6=model1[fold$subsets[fold$which == 6], ]
validation7=model1[fold$subsets[fold$which == 7], ]
validation8=model1[fold$subsets[fold$which == 8], ]
validation9=model1[fold$subsets[fold$which == 9], ]
validation10=model1[fold$subsets[fold$which == 10], ]

VC1=Cane(IT=validation1$IT)
VC2=Cane(IT=validation2$IT)
VC3=Cane(IT=validation3$IT)
VC4=Cane(IT=validation4$IT)
VC5=Cane(IT=validation5$IT)
VC6=Cane(IT=validation6$IT)
VC7=Cane(IT=validation7$IT)
VC8=Cane(IT=validation8$IT)
VC9=Cane(IT=validation9$IT)
VC10=Cane(IT=validation10$IT)

VCR1=rmse(VC1, validation1$Spec.wgt)
VCR2=rmse(VC2,validation2$Spec.wgt)
VCR3=rmse(VC3,validation3$Spec.wgt)
VCR4=rmse(VC4,validation4$Spec.wgt)
VCR5=rmse(VC5,validation5$Spec.wgt)
VCR6=rmse(VC6,validation6$Spec.wgt)
VCR7=rmse(VC7,validation7$Spec.wgt)
VCR8=rmse(VC8,validation8$Spec.wgt)
VCR9=rmse(VC9,validation9$Spec.wgt)
VCR10=rmse(VC10,validation10$Spec.wgt)
