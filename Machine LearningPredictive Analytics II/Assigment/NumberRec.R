############################ SVM Number Recogniser #################################
# 1. Business Understanding
# 2. Data Understanding
# 3. Data Preparation
# 4. Model Building 
#  4.1 Linear kernel
#  4.2 RBF Kernel
# 5 Hyperparameter tuning and cross validation

#####################################################################################

# 1. Business Understanding: 

#You are required to develop a model using Support Vector Machine which
# should correctly classify the handwritten digits based on the pixel values given as features.
#####################################################################################

# 2. Data Understanding: 
#MNIST Database stands for the Modified National Institute of Standards and Technology. It is a
# curated database of handwritten digits used for computer vision, pattern recognition and to train 
# Machine Learning Algorithms.
#The database contains images of handwritten digits each with varying writing styles. 
#Training Dataset has 60,000 images of handwritten digits while Testing Dataset has 10,000 images of 
# handwritten digits.
#Each image is normalized to a 28rowsx28column pixel matrix. The dataset generated from these images has 784 attributes with each attribute representing one element of the pixel matrix.
#Each record of the 784 attributes is filled in with a value between 0-255 both inclusive, these values are representative of the ink density of the corresponding pixel from the image.
#Pixels are organized row-wise. Pixel values are 0 to 255. 0 means background (white), 255 means foreground (black). 
#The Objective of this assignent is to implement SVM algorithm to create a multiclassifier to distinguish these numbers based on the pixel information.
#We will also conduct hyperparameter tuning using 3-fold cross validation.
#For a detailed understanding of the dataset please refer to the following website: http://yann.lecun.com/exdb/mnist/


#3. Data Preparation: 


#Loading Neccessary libraries

library(kernlab)
library(readr)
library(caret)
library(dplyr)
library(ggplot2)
library(gridExtra)

#Loading Data

Data <- read.csv("mnist_train.csv", stringsAsFactors = T)

#Understanding Dimensions
dim(Data)

# Rename all colums so that it will consistent.
for (i in 0:dim(Data)[2]){
  colnames(Data)[i] <- i
}

# Read test data.
test <- read_csv("mnist_test.csv")

# Rename all colums so that it will consistent.
for (i in 0:dim(test)[2]){
  colnames(test)[i] <- i
}

# Renaming 
colnames(Data)[1] <- "Number"
Data$Number<-factor(Data$Number)

colnames(test)[1] <- "Number"
test$Number<-factor(test$Number)

#Structure of the dataset
str(Data)

#Exploring the data
summary(Data)

# Merge test and traing data for data processing.
mergedDataSet <- rbind(Data,test)


#checking missing value
sapply(mergedDataSet, function(x) sum(is.na(x)))

#Remove unnecerray columns for analysis i.e., columns which contains all the rows same value like NA / 0 / 1 / empty / space
mergedDataSet <- Filter(function(x)!all( x==0 | x==1), mergedDataSet)

# Check if any value less than 0.
sapply(mergedDataSet[,-1], min)
min(sapply(mergedDataSet[,-1], min))

# Check if any value greater than 255.
sapply(mergedDataSet[,-1], max)
max(sapply(mergedDataSet[,-1], max))

# Get back test and training data.
train_index<- nrow(Data)
Data<- mergedDataSet[1:train_index,]
test<- mergedDataSet[(train_index+1):nrow(mergedDataSet), ]

# Display sample image for visulaization.
digit <- matrix(as.numeric(Data[1,-1]), nrow = 28) #look at one digit
image(digit, col = grey.colors(255))

# Sample 10 % of data. There are 60000 data. For each number if we take random
# 600 data, then it will be 6000 data and it will have equal represntation of data.
# Only 10 % of data is taken so that compuation can be done in considerable time.
train <- data.frame()
for (i in 0:9){
  newdata <- subset(Data, Number == i)
  train.indices = sample(1:nrow(newdata), 600)
  train = rbind(train, newdata[train.indices, ])
}


#Constructing Model

#Using Linear Kernel
Model_linear <- ksvm(Number~ ., data = train, scale = FALSE, kernel = "vanilladot")
Eval_linear<- predict(Model_linear, test)

#confusion matrix - Linear Kernel
#Accuracy : 0.9126   
confusionMatrix(Eval_linear,test$Number)

#Using RBF Kernel
Model_RBF <- ksvm(Number~ ., data = train, scale = FALSE, kernel = "rbfdot")
Eval_RBF<- predict(Model_RBF, test)

Model_RBF
# Support Vectors : 2710 sigma =  1.63190420646579e-07 C = 1

#confusion matrix - RBF Kernel
# Accuracy : 0.9537  
confusionMatrix(Eval_RBF,test$Number)



############   Hyperparameter tuning and Cross Validation #####################

# We will use the train function from caret package to perform Cross Validation. 

#traincontrol function Controls the computational nuances of the train function.
# i.e. method =  CV means  Cross Validation.
#      Number = 2 implies Number of folds in CV.

trainControl <- trainControl(method="cv", number=3)


# Metric <- "Accuracy" implies our Evaluation metric is Accuracy.
metric <- "Accuracy"

#Expand.grid functions takes set of hyperparameters, that we shall pass to our model.
set.seed(7)

grid <- expand.grid(.sigma=c(0.63190420646579e-07, 1.63190420646579e-07, 2.63190420646579e-07), .C=c(0.5,1,2) )

#train function takes Target ~ Prediction, Data, Method = Algorithm
#Metric = Type of metric, tuneGrid = Grid of Parameters,
# trcontrol = Our traincontrol method.
fit.svm <- train(Number~., data=train, method="svmRadial", metric=metric, 
                 tuneGrid=grid, trControl=trainControl)
print(fit.svm)
plot(fit.svm)

# Try another set of parameters.
grid <- expand.grid(.sigma=c(3.63190420646579e-07, 4.63190420646579e-07, 5.63190420646579e-07), .C=c(3,4) )
fit.svm1 <- train(Number~., data=train, method="svmRadial", metric=metric, 
                 tuneGrid=grid, trControl=trainControl)

#  4.63190420646579e-07 abd c=3 has optimal value.

# Building the Final RBF [Gaussian Radial Basis kernel] Model with the above Tuned HyperParameters
RBF_nonlin_final_mod <- ksvm(Number~.,
                             data=train,
                             kernel="rbfdot",
                             scale=FALSE,
                             C=3,
                             kpar=list(sigma=4.631904e-07))

# Final Model's Performance on Training Data
Eval_RBF<- predict(RBF_nonlin_final_mod, Data)

# Accuracy : 0.9669 
confusionMatrix(Eval_RBF,Data$Number)

# Final Model's Performance on test Data
Eval_RBF<- predict(RBF_nonlin_final_mod, test)

# Accuracy : 0.9657    
confusionMatrix(Eval_RBF,test$Number)

# Accuracy of train and test data matches.