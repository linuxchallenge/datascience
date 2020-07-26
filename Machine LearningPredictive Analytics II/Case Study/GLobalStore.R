############################ Retail-Giant Sales Forecasting #################################
# 1. Business Understanding
# 2. Data Understanding
# 3. Data Preparation
# 4. Model Building forecasting
#  4.1 classical decomposition
#  4.2 auto ARIMA
# 5 Model evaluation

#####################################################################################
# 1. Business Understanding: 
library(lubridate)
library(forecast)
library(tseries)
require(graphics)
library(ggplot2)
library(dplyr)
library(plotrix)

#Loading Data
Data <- read.csv("Global Superstore.csv", stringsAsFactors = F)

#Structure of the dataset
str(Data)

#Exploring the data
summary(Data)

# Convert to factors
cols <- c("Ship.Mode", "Segment", "City", "State", "Country", "Market", "Region", "Category", "Sub.Category", "Order.Priority")
Data[cols] <- lapply(Data[cols], factor)

Data <- Data[-grep("Customer.Name", colnames(Data))]

Data$Order.Date <- parse_date_time(Data$Order.Date, "dmY", truncated = 3)
Data$Ship.Date <- parse_date_time(Data$Ship.Date, "dmY", truncated = 3)

sapply(Data, function(x) sum(is.na(x))) # shows all NA

#Since postal code has lot NA remove.
Data <- Data[-grep("Postal.Code", colnames(Data))]

#Now, we need to remove duplicate values (if any) in the dataset.
Data <- unique(Data)

# We need to first segment the whole dataset into the 21 subsets based on the 
# market and the customer segment level

fewSet <- Data[, c("Order.Date", "Market", "Segment", "Sales", "Quantity", "Profit")]
fewSet$Market <- factor(fewSet$Market)
fewSet$Segment <- factor(fewSet$Segment)
fewSet$short.date = strftime(fewSet$Order.Date, "%Y/%m")

fewSet%>%group_by(short.date,Market,Segment)%>%summarize(
 agg_sales=sum(Sales),
 agg_qty=sum(Quantity),
 agg_pft=sum(Profit)) ->fewset_agg

fewset_agg%>%group_by(Market,Segment)%>%summarize(
  agg_pft1=sum(agg_pft),
cv=sd(agg_pft, na.rm=TRUE)/mean(agg_pft, na.rm=TRUE))->fewset_aggcv

fewset_aggcv$seg<-paste(fewset_aggcv$Market,fewset_aggcv$Segment,sep="")
fewset_aggcv$seg <-as.numeric(as.factor(fewset_aggcv$seg))

barp(fewset_aggcv$agg_pft1/10000, col = fewset_aggcv$Market,
     main="Compare CV & profit across market segments",
     xlab="MarketSegment",ylab="Profit in 10 K USD" )
lines(fewset_aggcv$cv,col="blue",lwd=2)
points(fewset_aggcv$cv,col="orange")

fewset_aggcv[order(-fewset_aggcv$agg_pft1),]
##  Market Segment     agg_pft1    cv     seg
##  <fct>  <fct>          <dbl> <dbl>     <dbl>
##  1 APAC   Consumer     222818. 0.632     4
##  2 EU     Consumer     188688. 0.624    13
##  3 US     Consumer     134119. 1.01     19
##  4 APAC   Corporate    129737. 0.698     5
##  5 EU     Corporate    123394. 0.764    14
##  6 LATAM  Consumer     120633. 0.661    16
##  7 US     Corporate     91979. 1.00     20
##  8 APAC   Home Office   83445. 1.05      6
##  9 EU     Home Office   60748. 1.12     15
##  10 US     Home Office   60299. 1.10     21

## APAC Consumer & EU Consumer are the most consistent profitable market segments
####################################################################

data_EU_Consumer <- subset(fewset_agg, fewset_agg$Segment=="Consumer" & fewset_agg$Market=="EU")
data_APAC_Consumer <- subset(fewset_agg, fewset_agg$Segment=="Consumer" & fewset_agg$Market=="APAC")

timeser<- ts(data_EU_Consumer[,"agg_sales"], frequency = 12, start = 2011)
plot(timeser)

ts<-ts (timeser, frequency = 12, start = 2011) # freq 12 => Monthly data. 
plot(decompose(ts))

#####################################################################################
### Moving average function.
#####################################################################################

movingAverage <- function(timeser) {
  #Smoothing the series - Moving Average Smoothing
  w <-1
  smoothedseries <- stats::filter(timeser, 
                                  filter=rep(1/(2*w+1),(2*w+1)), 
                                  method='convolution', sides=2)
  
  #Smoothing left end of the time series
  diff <- smoothedseries[w+2] - smoothedseries[w+1]
  for (i in seq(w,1,-1)) {
    smoothedseries[i] <- smoothedseries[i+1] - diff
  }
  
  #Smoothing right end of the time series
  n <- length(timeser)
  diff <- smoothedseries[n-w] - smoothedseries[n-w-1]
  for (i in seq(n-w+1, n)) {
    smoothedseries[i] <- smoothedseries[i-1] + diff
  }
  return (smoothedseries)
  
}

#####################################################################################
### Analysis of EU consumer sales.
#####################################################################################

timeser<- ts(data_EU_Consumer[,"agg_sales"], frequency = 12, start = 2011)
length(timeser)
timeser <- ts(timeser[1:42])
plot(timeser)
smoothedseries <- movingAverage(timeser)

#Plot the smoothed time series
lines(smoothedseries, col="blue", lwd=2)
x <- 1:42
smootheddf <- as.data.frame(cbind(x, as.vector(smoothedseries)))
colnames(smootheddf) <- c('Month', 'Data')

#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

lmfit <- lm(Data ~ sin(0.4*Month)*poly(Month,1)+
              cos(0.09*Month)*poly(Month,1), data=smootheddf)
global_pred <- predict(lmfit, Month=x)
summary(global_pred)
lines(x, global_pred, col='red', lwd=2)

local_pred <- timeser-global_pred
plot(local_pred, col='red', type = "l")
acf(local_pred)
acf(local_pred, type="partial")
armafit <- auto.arima(local_pred)

tsdiag(armafit)
armafit

#We'll check if the residual series is white noise
resi <- local_pred-fitted(armafit)

adf.test(resi,alternative = "stationary")
#The Augmented Dickey-Fuller Test shows a p-value of 0.01. 
#This Implies that the residual after extraxting the global and local components for demand is stationairy.


kpss.test(resi)
#The KPSS Test for Level Stationarity shows a p-value>0.1
#This Implies that the residual after extraxting the global and local components for demand is stationairy.

global_pred_out <- predict(lmfit,data.frame(Month = 43:48))
fcast <- global_pred_out


#Now, let's compare our prediction with the actual values, using MAPE
MAPE_class_dec <- accuracy(fcast,data_EU_Consumer$agg_sales[43:48])
MAPE_class_dec
#ME     RMSE      MAE      MPE     MAPE
#Test set 4172.437 13736.59 10622.04 2.259545 20.73958

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit
class_dec_pred <- c(ts(global_pred),ts(global_pred_out))
plot(smoothedseries, col = "black")
lines(class_dec_pred, col = "red")
lines(43:48,ts(global_pred_out),col="blue",lwd=2)

#So, that was classical decomposition, now let's do an ARIMA fit
autoarima <- auto.arima(timeser)
autoarima
tsdiag(autoarima)
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")

#Again, let's check if the residual series is white noise
resi_auto_arima <- timeser - fitted(autoarima)

adf.test(resi_auto_arima,alternative = "stationary")
kpss.test(resi_auto_arima)

#Also, let's evaluate the model using MAPE
fcast_auto_arima <- predict(autoarima, n.ahead = 6)

MAPE_auto_arima <- accuracy(fcast_auto_arima$pred,data_EU_Consumer$agg_sales[43:48])
MAPE_auto_arima
#ME     RMSE     MAE    MPE    MAPE
#Test set 12935.21 19499.14 16687.6 17.678 28.9226

## As the classical decomposition model has better accuracy than
## auto arima model, next 6 months forecast is done using the classical 
## decomposition method as given:
EU_sales_pred <- predict(lmfit,data.frame(Month = 49:54))
EU_sales_pred
##  1        2        3        4        5        6 
## 68506.39 74397.71 79502.23 83628.33 86762.67 89076.07 

#####################################################################################
### Analysis of EU consumer Quantity
#####################################################################################

timeser<- ts(data_EU_Consumer[,"agg_qty"], frequency = 12, start = 2011)
ts<-ts (timeser, frequency = 12, start = 2011) # freq 12 => Monthly data. 
plot(decompose(ts))

length(timeser)
timeser <- ts(timeser[1:42])
plot(timeser)

smoothedseries <- movingAverage(timeser)

#Plot the smoothed time series
lines(smoothedseries, col="blue", lwd=2)

x <- 1:42
smootheddf <- as.data.frame(cbind(x, as.vector(smoothedseries)))
colnames(smootheddf) <- c('Month', 'Data')


#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

lmfit <- lm(Data ~ sin(0.5*Month)*poly(Month,1)+
              cos(0.09*Month)*poly(Month,1)+
              tan(0.02*Month), data=smootheddf)
global_pred <- predict(lmfit, Month=x)
summary(global_pred)
lines(x, global_pred, col='red', lwd=2)


local_pred <- timeser-global_pred
plot(local_pred, col='red', type = "l")
acf(local_pred)
acf(local_pred, type="partial")
armafit <- auto.arima(local_pred)

tsdiag(armafit)
armafit

#We'll check if the residual series is white noise
resi <- local_pred-fitted(armafit)

adf.test(resi,alternative = "stationary")
#The Augmented Dickey-Fuller Test shows a p-value=0.02. 
#This Implies that the residual after extraxting the global and local components for demand is stationairy.


kpss.test(resi)
#The KPSS Test for Level Stationarity shows a p-value>0.1
#This Implies that the residual after extraxting the global and local components for demand is stationairy.


global_pred_out <- predict(lmfit,data.frame(Month = 43:48))
fcast <- global_pred_out

#Now, let's compare our prediction with the actual values, using MAPE
MAPE_class_dec <- accuracy(fcast,data_EU_Consumer$agg_qty[43:48])
MAPE_class_dec
#ME     RMSE     MAE       MPE     MAPE
#Test set -19.25783 189.3226 127.333 -10.38272 21.98432

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit
class_dec_pred <- c(ts(global_pred),ts(global_pred_out))
plot(smoothedseries, col = "black")
lines(class_dec_pred, col = "red")
lines(43:48,ts(global_pred_out),col="blue",lwd=2)

#So, that was classical decomposition, now let's do an ARIMA fit
autoarima <- auto.arima(timeser)
autoarima
tsdiag(autoarima)
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")

#Again, let's check if the residual series is white noise
resi_auto_arima <- timeser - fitted(autoarima)

adf.test(resi_auto_arima,alternative = "stationary")
kpss.test(resi_auto_arima)

#Also, let's evaluate the model using MAPE
fcast_auto_arima <- predict(autoarima, n.ahead = 6)

MAPE_auto_arima <- accuracy(fcast_auto_arima$pred,data_EU_Consumer$agg_qty[43:48])
MAPE_auto_arima
#ME     RMSE      MAE      MPE     MAPE
#Test set 242.746 316.7626 253.8108 27.53891 30.13319

## As the classical decomposition model has better accuracy than
## auto arima model, next 6 months forecast is done using the classical 
## decomposition method as given:

EU_inv_pred <- predict(lmfit,data.frame(Month = 49:54))
round(EU_inv_pred)
##     1    2    3    4    5    6 
##    943  981 1025 1085 1171 1290 

#####################################################################################
### Analysis of APAC consumer sales.
#####################################################################################

timeser<- ts(data_APAC_Consumer[,"agg_sales"], frequency = 12, start = 2011)
ts<-ts (timeser, frequency = 12, start = 2011) # freq 12 => Monthly data. 
plot(decompose(ts))

length(timeser)
timeser <- ts(timeser[1:42])
plot(timeser)

smoothedseries <- movingAverage(timeser)

#Plot the smoothed time series
lines(smoothedseries, col="blue", lwd=2)

x <- 1:42
smootheddf <- as.data.frame(cbind(x, as.vector(smoothedseries)))
colnames(smootheddf) <- c('Month', 'Data')


#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

lmfit <- lm(Data ~ sin(0.5*Month)*poly(Month,1)+
              cos(0.05*Month)*poly(Month,1)+
              tan(0.02*Month), data=smootheddf)
global_pred <- predict(lmfit, Month=x)
summary(global_pred)
lines(x, global_pred, col='red', lwd=2)


local_pred <- timeser-global_pred
plot(local_pred, col='red', type = "l")
acf(local_pred)
acf(local_pred, type="partial")
armafit <- auto.arima(local_pred)

tsdiag(armafit)
armafit

#We'll check if the residual series is white noise
resi <- local_pred-fitted(armafit)

adf.test(resi,alternative = "stationary")
#The Augmented Dickey-Fuller Test shows a p-value<0.01. 
#This Implies that the residual after extraxting the global and local components for demand is stationairy.


kpss.test(resi)
#The KPSS Test for Level Stationarity shows a p-value>0.1
#This Implies that the residual after extraxting the global and local components for demand is stationairy.


global_pred_out <- predict(lmfit,data.frame(Month = 43:48))
fcast <- global_pred_out

#Now, let's compare our prediction with the actual values, using MAPE
MAPE_class_dec <- accuracy(fcast,data_APAC_Consumer$agg_sales[43:48])
MAPE_class_dec

#ME     RMSE      MAE      MPE     MAPE
#Test set 6515.586 14997.64 12937.14 5.024683 20.83351

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit
class_dec_pred <- c(ts(global_pred),ts(global_pred_out))
plot(smoothedseries, col = "black")
lines(class_dec_pred, col = "red")
lines(43:48,ts(global_pred_out),col="blue",lwd=2)


#So, that was classical decomposition, now let's do an ARIMA fit
autoarima <- auto.arima(timeser)
autoarima
tsdiag(autoarima)
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")

#Again, let's check if the residual series is white noise
resi_auto_arima <- timeser - fitted(autoarima)

adf.test(resi_auto_arima,alternative = "stationary")
kpss.test(resi_auto_arima)

#Also, let's evaluate the model using MAPE
fcast_auto_arima <- predict(autoarima, n.ahead = 6)
MAPE_auto_arima <- accuracy(fcast_auto_arima$pred,data_APAC_Consumer$agg_sales[43:48])
MAPE_auto_arima
#ME     RMSE      MAE      MPE     MAPE
#Test set 15848.24 22755.75 18780.19 19.73091 27.68952

## As the classical decomposition model has better accuracy than
## auto arima model, next 6 months forecast is done using the classical 
## decomposition method as given:
APAC_sales_pred <- predict(lmfit,data.frame(Month = 49:54))
APAC_sales_pred
##  1        2        3        4        5        6 
## 63405.05 65629.26 68534.04 72620.16 78283.06 85754.44 

#####################################################################################
### Analysis of APAC consumer Quantity
#####################################################################################

timeser<- ts(data_APAC_Consumer[,"agg_qty"])
length(timeser)
timeser <- ts(timeser[1:42])
plot(timeser)

smoothedseries <- movingAverage(timeser)

#Plot the smoothed time series
lines(smoothedseries, col="blue", lwd=2)

x <- 1:42
smootheddf <- as.data.frame(cbind(x, as.vector(smoothedseries)))
colnames(smootheddf) <- c('Month', 'Data')


#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

lmfit <- lm(Data ~ sin(0.5*Month) * poly(Month,1) + cos(0.1*Month) * poly(Month,1)
            + tan(0.02*Month), data=smootheddf)
lmfit
global_pred <- predict(lmfit, Month=x)
summary(global_pred)
lines(x, global_pred, col='red', lwd=2)


local_pred <- timeser-global_pred
plot(local_pred, col='red', type = "l")
acf(local_pred)
acf(local_pred, type="partial")
armafit <- auto.arima(local_pred)

tsdiag(armafit)
armafit

#We'll check if the residual series is white noise
resi <- local_pred-fitted(armafit)

adf.test(resi,alternative = "stationary")
#The Augmented Dickey-Fuller Test shows a p-value<0.01. 
#This Implies that the residual after extraxting the global and local components for demand is stationairy.


kpss.test(resi)
#The KPSS Test for Level Stationarity shows a p-value>0.1
#This Implies that the residual after extraxting the global and local components for demand is stationairy.


global_pred_out <- predict(lmfit,data.frame(Month = 43:48))
fcast <- global_pred_out

#Now, let's compare our prediction with the actual values, using MAPE
MAPE_class_dec <- accuracy(fcast,data_APAC_Consumer$agg_qty[43:48])
MAPE_class_dec
#ME     RMSE      MAE       MPE     MAPE
#Test set -23.16084 127.4945 107.1754 -8.815864 18.79196


#Let's also plot the predictions along with original values, to
#get a visual feel of the fit
class_dec_pred <- c(ts(global_pred),ts(global_pred_out))
plot(smoothedseries, col = "black")
lines(class_dec_pred, col = "red")
lines(43:48,ts(global_pred_out),col="blue",lwd=2)

#So, that was classical decomposition, now let's do an ARIMA fit
autoarima <- auto.arima(timeser)
autoarima
tsdiag(autoarima)
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")

#Again, let's check if the residual series is white noise
resi_auto_arima <- timeser - fitted(autoarima)

adf.test(resi_auto_arima,alternative = "stationary")
kpss.test(resi_auto_arima)

#Also, let's evaluate the model using MAPE
fcast_auto_arima <- predict(autoarima, n.ahead = 6)

MAPE_auto_arima <- accuracy(fcast_auto_arima$pred,data_APAC_Consumer$agg_qty[43:48])
MAPE_auto_arima
#ME     RMSE      MAE       MPE     MAPE
#Test set 12 174.3722 147.6667 -7.362467 26.24458

## As the classical decomposition model has better accuracy than
## auto arima model, next 6 months forecast is done using the classical 
## decomposition method as given:
APAC_inv_pred <- predict(lmfit,data.frame(Month = 49:54))
round(APAC_inv_pred)
##  1    2    3    4    5    6 
## 976 1053 1143 1251 1380 1532 

##########################################################################
## Observations:
## 1) Classical decomposition method is found to give more accuracy than 
## the auto arima model
## 2) Hence all forecasts above are done using the classical decomposition
## models.
## 3) EU Consumer segment prediction from jan 2015 till June 2015
##   1        2        3        4        5        6 
## 68506.39 74397.71 79502.23 83628.33 86762.67 89076.07 
## 4) EU Consumer segment inventory prediction
##     1    2    3    4    5    6 
##    943  981 1025 1085 1171 1290 
## 5) APAC Consumer segment prediction
##  1        2        3        4        5        6 
## 63405.05 65629.26 68534.04 72620.16 78283.06 85754.44 
## 6) APAC Consumer segmnet inventory prediction
##  1    2    3    4    5    6 
## 976 1053 1143 1251 1380 1532 
#########################################################################
