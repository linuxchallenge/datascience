# SVM
####################################################################
# 4. Model Building
# 4.3 Non-Linear Model - Kernels

# Cross Validation
# 5.2 Tunning Non-linear model

####################################################################
# 4. Model Building
#---------------------------------------------------------------------
# 4.3 Non-Linear - Kernels
######################################################################

# RBF kernel 
model_rbf <- ksvm(spam ~ ., data =train,scale=FALSE, kernel = "rbfdot")

# Predicting the model results 
Eval_RBF<- predict(model_rbf, test)

#confusion matrix - RBF Kernel
confusionMatrix(Eval_RBF,test$spam)
# Accuracy    : 0.9268
# Sensitivity : 0.9593        
# Specificity : 0.8768 

#####################################################################
#Hyperparameter tuning and Cross Validation - Non-Linear - SVM 
######################################################################

# We will use the train function from caret package to perform crossvalidation

# Making grid of "sigma" and C values. 
grid <- expand.grid(.sigma=seq(0.01, 0.05, by=0.01), .C=seq(1, 5, by=1))

# Performing 5-fold cross validation
fit.svm_radial <- train(spam~., data=train, method="svmRadial", metric=metric, 
                        tuneGrid=grid, trControl=trainControl)

# Printing cross validation result
print(fit.svm_radial)
# Best tune at sigma = 0.01 & C=2, Accuracy - 0.935

# Plotting model results
plot(fit.svm_radial)

######################################################################
# Checking overfitting - Non-Linear - SVM
######################################################################

# Validating the model results on test data
evaluate_non_linear<- predict(fit.svm_radial, test)
confusionMatrix(evaluate_non_linear, test$spam)

# Accuracy    - 0.9333
# Sensitivity - 0.9569
# Specificity - 0.9044

######################################################################