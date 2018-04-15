#LASSO_workflow


#Option 2: LASSO----
#this implies also dividing the datset in build and test datasets.
#https://www.r-bloggers.com/ridge-regression-and-the-lasso/

##Training and test datasets
bee_test
bee_train


library(glmnet)
train_sparse <- sparse.model.matrix(~.,bee_train[c("Country","Latitude","Family","IT","Sex")])
test_sparse <- sparse.model.matrix(~.,bee_test[c("Region","Latitude","Family","IT","Sex")])
fit <- glmnet(train_sparse,bee_train$Spec.wgt)
pred <- predict(fit, test_sparse, type="class")
par(mfrow=c(1,1))
plot(fit, xvar = "lambda", label = TRUE)


# use cv.glmnet to find best lambda/penalty 
#- choosing small nfolds for cv due to… 
# s is the penalty parameter
cv <- cv.glmnet(train_sparse,bee_train$Spec.wgt,nfolds=3)
pred <- predict(fit, test_sparse,type="response", s=cv$lambda.min)
plot(cv, xvar = "lambda",label = TRUE)
plot(fit, xvar = "dev", label = TRUE)

coef.apprx = coef(fit, s = 0.5, exact = FALSE) 
coef.apprx
coef.exact = coef(fit, s = 0.5, exact = TRUE, x=x, y=y) 
coef.exact
cbind2(coef.exact, coef.apprx)

cvfit = cv.glmnet( train_sparse,bee_train$Spec.wgt, type.measure = "mse", nfolds = 20)
cvfit