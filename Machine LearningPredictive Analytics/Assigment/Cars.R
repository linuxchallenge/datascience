cars<-read.csv("CarPrice_Assignment.csv", stringsAsFactors = FALSE)

View(cars)

#Consider only company name as the independent variable for the model building. 
cars$CarName <- sapply(strsplit(cars$CarName,' '), "[", 1)

#Now, we need to remove duplicate values (if any) in the dataset.
cars <- unique(cars)

# check the structure of the car dataset.
str(cars)

# check for missing values and treat if any.
sum(is.na(cars))

dummy_1 <- data.frame(model.matrix( ~fueltype, data = cars))
dummy_1 <- dummy_1[,-1]
cars$fueltype <- dummy_1

dummy_1 <- data.frame(model.matrix( ~aspiration, data = cars))
dummy_1 <- dummy_1[,-1]
cars$aspiration <- dummy_1

dummy_1 <- data.frame(model.matrix( ~doornumber, data = cars))
dummy_1 <- dummy_1[,-1]
cars$doornumber <- dummy_1

dummy_1 <- data.frame(model.matrix( ~enginelocation, data = cars))
dummy_1 <- dummy_1[,-1]
cars$enginelocation <- dummy_1

#Converting "carbody" into dummies . 
dummy_1 <- data.frame(model.matrix( ~carbody, data = cars))
dummy_1 <- dummy_1[,-1]
grep("carbody", colnames(cars))
cars <- cbind(cars[,-grep("carbody", colnames(cars))], dummy_1)


dummy_1 <- data.frame(model.matrix( ~drivewheel, data = cars))
dummy_1 <- dummy_1[,-1]
grep("drivewheel", colnames(cars))
cars <- cbind(cars[,-grep("drivewheel", colnames(cars))], dummy_1)

dummy_1 <- data.frame(model.matrix( ~enginetype, data = cars))
dummy_1 <- dummy_1[,-1]
grep("enginetype", colnames(cars))
cars <- cbind(cars[,-grep("enginetype", colnames(cars))], dummy_1)

dummy_1 <- data.frame(model.matrix( ~cylindernumber, data = cars))
dummy_1 <- dummy_1[,-1]
grep("cylindernumber", colnames(cars))
cars <- cbind(cars[,-grep("cylindernumber", colnames(cars))], dummy_1)

dummy_1 <- data.frame(model.matrix( ~fuelsystem, data = cars))
dummy_1 <- dummy_1[,-1]
grep("fuelsystem", colnames(cars))
cars <- cbind(cars[,-grep("fuelsystem", colnames(cars))], dummy_1)

dummy_1 <- data.frame(model.matrix( ~CarName, data = cars))
dummy_1 <- dummy_1[,-1]
grep("CarName", colnames(cars))
cars <- cbind(cars[,-grep("CarName", colnames(cars))], dummy_1)


str(cars)


# Next step is to treat the outliers (if any). To check for outliers, we find out the quantile values at each 1% interval and wherever there is a high jump from one
# quantile to another
quantile(cars$wheelbase, seq(0,1,0.01))

quantile(cars$carlength, seq(0,1,0.01))

quantile(cars$carwidth, seq(0,1,0.01))

quantile(cars$carheight, seq(0,1,0.01))

quantile(cars$curbweight, seq(0,1,0.01))

quantile(cars$enginesize, seq(0,1,0.01))

quantile(cars$boreratio, seq(0,1,0.01))

quantile(cars$stroke, seq(0,1,0.01))

quantile(cars$compressionratio, seq(0,1,0.01))

quantile(cars$horsepower, seq(0,1,0.01))
cars$horsepower[which(cars$horsepower > 184)]<-184

quantile(cars$peakrpm, seq(0,1,0.01))

quantile(cars$citympg, seq(0,1,0.01))

quantile(cars$highwaympg, seq(0,1,0.01))

quantile(cars$price, seq(0,1,0.01))

# Derived variables. 
cars$mpgratio <- cars$citympg / cars$highwaympg

cars$horsepower_rpmratio <- cars$peakrpm / cars$horsepower

# separate training and testing data
set.seed(100)
trainindices= sample(1:nrow(cars), 0.7*nrow(cars))
train = cars[trainindices,]
test = cars[-trainindices,]

model_1 <-lm(price~.,data=train)
summary(model_1)

# Lets load the library in which stepAIC function exists
#install.packages("MASS")
#install.packages("car")
library(MASS)
library(car)
step <- stepAIC(model_1, direction="both")

step

model_2 <- lm(formula = price ~ car_ID + aspiration + enginelocation + wheelbase + 
                carlength + carwidth + curbweight + enginesize + boreratio + 
                peakrpm + citympg + highwaympg + carbodyhardtop + carbodyhatchback + 
                carbodysedan + carbodywagon + drivewheelrwd + enginetypedohcv + 
                enginetypel + enginetypeohc + enginetypeohcf + enginetyperotor + 
                cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                cylindernumberthree + fuelsystem2bbl + fuelsystemmpfi + fuelsystemspdi + 
                CarNamebmw + CarNamedodge + CarNamehonda + CarNameisuzu + 
                CarNamejaguar + CarNamemaxda + CarNamemazda + CarNamemercury + 
                CarNamemitsubishi + CarNamenissan + CarNameNissan + CarNameplymouth + 
                CarNameporcshce + CarNamerenault + CarNamesaab + 
                CarNametoyouta + CarNamevokswagen + CarNamevolkswagen + CarNamevolvo + 
                CarNamevw + mpgratio + horsepower_rpmratio, data = train)

summary(model_2)
# Adjusted R-squared:  0.9681
vif(model_2)

# remove all 0 star wheelbase, carlength, peakrpm, citympg, highwaympg
# carbodyhardtop, carbodysedan, drivewheelrwd, enginetypedohcv,
# enginetypeohcf, enginetyperotor, cylindernumberfive, cylindernumberfour
# cylindernumbersix, cylindernumberthree, fuelsystemspdi, CarNamehonda,
# CarNameisuzu , CarNamejaguar, CarNamemercury, CarNamenissan, CarNameNissan,
# CarNameporcshce, CarNamerenault, CarNamesaab, CarNametoyouta, CarNamevokswagen, 
# CarNamevolkswagen, CarNamevw, mpgratio
model_3 <- lm(formula = price ~ car_ID + aspiration + enginelocation +  
                carwidth + curbweight + enginesize + boreratio + 
                carbodyhatchback + 
                carbodywagon + 
                enginetypel + enginetypeohc +  
                fuelsystem2bbl + fuelsystemmpfi +  
                CarNamebmw + CarNamedodge + 
                CarNamemaxda + CarNamemazda + 
                CarNamemitsubishi + CarNameplymouth + 
                CarNamevolvo + 
                horsepower_rpmratio, data = train)

summary(model_3)
# Adjusted R-squared:  0.9376  
vif(model_3)

# Remove zero star boreratio, carbodyhatchback, CarNamemaxda, CarNamemazda, CarNameplymouth
model_4 <- lm(formula = price ~ car_ID + aspiration + enginelocation +  
                carwidth + curbweight + enginesize + 
                carbodywagon + 
                enginetypel + enginetypeohc +  
                fuelsystem2bbl + fuelsystemmpfi +  
                CarNamebmw + CarNamedodge + 
                CarNamemitsubishi + 
                CarNamevolvo + 
                horsepower_rpmratio, data = train)
summary(model_4)
#Adjusted R-squared:  0.9382  
vif(model_4)

# Remove single star carbodywagon fuelsystem2bbl CarNamedodge CarNamemitsubishi CarNamevolvo
model_5 <- lm(formula = price ~ car_ID + aspiration + enginelocation +  
                carwidth + curbweight + enginesize + 
                enginetypel + enginetypeohc +  
                CarNamebmw + 
                horsepower_rpmratio, data = train)

summary(model_5)
# Adjusted R-squared:  0.925
vif(model_5)


# Looks like there is correlation between carwidth and engine size
cor(train$carwidth, train$enginesize)

# Looks like there is correlation between curbweight and engine size
cor(train$curbweight, train$enginesize)

# Remove curbweight
model_6 <- lm(formula = price ~ car_ID + aspiration + enginelocation +  
                carwidth + enginesize + 
                enginetypel + enginetypeohc +  
                CarNamebmw + 
                horsepower_rpmratio, data = train)

summary(model_6)
# Adjusted R-squared:  0.913  
vif(model_6)


# Remove car_ID and horsepower_rpmratio
model_7 <- lm(formula = price ~ aspiration + enginelocation +  
                carwidth + enginesize + 
                enginetypel + enginetypeohc +  
                CarNamebmw 
                , data = train)
summary(model_7)
#Adjusted R-squared:  0.9028.
vif(model_7)

# Remove enginetypel and horsepower_rpmratio
model_8 <- lm(formula = price ~ aspiration + enginelocation +  
                carwidth + enginesize + 
                enginetypeohc +  
                CarNamebmw 
              , data = train)
summary(model_8)
#Adjusted R-squared:  0.8989.
vif(model_8)

# Remove aspiration and enginetypeohc
model_9 <- lm(formula = price ~ enginelocation +  
                carwidth + enginesize + 
                CarNamebmw 
              , data = train)
summary(model_9)
#Adjusted R-squared:  0.8927.
vif(model_9)

# predicting the results in test dataset
Predict_1 <- predict(model_9,test[,-grep("price", colnames(cars))])
test$test_price <- Predict_1

# Now, we need to test the r square between actual and predicted price. 
r <- cor(test$price,test$test_price)
rsquared <- cor(test$price,test$test_price)^2
rsquared
# R squared is 0.8477359.
