#############################HR Solution###################
################################################################
#Business Understanding
#Data Understanding
#Data Preparation & EDA
#Model Building 
#Model Evaluation
################################################################




### Business Understanding:

# A large company named XYZ, employs, at any given point of time, around 4000 employees. 
# However, every year, around 15% of its employees leave the company and need to be replaced 
# with the talent pool available in the job market. The management believes that this level 
# of attrition (employees leaving, either on their own or because they got fired) is bad for
# the company, because of the following reasons -
#    
#    1: The former employees' projects get delayed, which makes it difficult to meet timelines, 
#   resulting in a reputation loss among consumers and partners
#  
#    2: A sizeable department has to be maintained, for the purposes of recruiting new talent
#  
#    3: More often than not, the new employees have to be trained for the job and/or given time to 
# acclimatise themselves to the company
# Hence, the management has contracted an HR analytics firm to understand what factors 
# they should focus on, in order to curb attrition.


################################################################





# Install and Load the required packages
#install.packages("MASS")
#install.packages("car")
#install.packages("e1071")
#install.packages("caret", dependencies = c("Depends", "Suggests"))
#install.packages("cowplot")
#install.packages("GGally")

library(MASS)
library(car)
library(e1071)
library(caret)
library(ggplot2)
library(cowplot)
library(caTools)
library(dplyr)

emp_survey <- read.csv("employee_survey_data.csv", stringsAsFactors = T)
gen_data <- read.csv("general_data.csv", stringsAsFactors = T)
man_survey <- read.csv("manager_survey_data.csv", stringsAsFactors = T)

str(emp_survey)
str(gen_data)
str(man_survey)


# Collate the data together in one single file, 
# check for duplicate
length(unique(emp_survey$EmployeeID))    
length(unique(gen_data$EmployeeID))
length(unique(man_survey$EmployeeID))


setdiff(emp_survey$EmployeeID,gen_data$EmployeeID) # Identical EmployeeID across these datasets
setdiff(emp_survey$EmployeeID,man_survey$EmployeeID) # Identical EmployeeID across these datasets

employee<- merge(emp_survey,gen_data, by="EmployeeID", all = F)
employee<- merge(employee,man_survey, by="EmployeeID", all = F)
View(employee)

in_time <- read.csv("in_time.csv", stringsAsFactors = F)
out_time <- read.csv("out_time.csv", stringsAsFactors = F)

library(lubridate)


in_time[,-1] <-  lapply(in_time[,-1], FUN = function(column) {
  parse_date_time(column, "%Y-%m-%d %H:%M:%S")
})

out_time[,-1] <-  lapply(out_time[,-1], FUN = function(column) {
  parse_date_time(column, "%Y-%m-%d %H:%M:%S")
})

in_time <- in_time[-1]
out_time <- out_time[-1]

# Need to find average of time spend, mean of time in and time out.
in_time_median <-  apply(in_time, 1, FUN = function(column) {
  vec <- format(as.POSIXct(strptime(column,"%Y-%m-%d %H:%M:%S",tz="")),format = "%H:%M")
  vec <-parse_date_time(vec, "%H:%M")
  tmp <- median(vec, na.rm = TRUE)
  tmp <- tmp + 62167219200
  as.POSIXlt(tmp, format="%H:%M")$hour
  
})

employee$in_time_median <- in_time_median

out_time_median <-  apply(out_time, 1, FUN = function(column) {
  vec <- format(as.POSIXct(strptime(column,"%Y-%m-%d %H:%M:%S",tz="")),format = "%H:%M")
  vec <-parse_date_time(vec, "%H:%M")
  tmp <- median(vec, na.rm = TRUE)
  tmp <- tmp + 62167219200
  as.POSIXlt(tmp, format="%H:%M")$hour  
})

employee$out_time_median <- out_time_median

timespend <- out_time - in_time
timespend[] <- lapply(timespend, function(x) as.numeric(x, units="hours"))
timespend_median <- apply(timespend, 1, FUN = function(x) {
  mean(x, na.rm = TRUE)
})

employee$timespend_median <- timespend_median


################################################################

### Data Preparation & Exploratory Data Analysis

# Understanding the structure of the collated file
str(employee) 

library(gridExtra)
library(grid)

#ggplot(employee) + 
#  geom_bar(aes(x=Attrition),position=position_dodge(width=0.7),alpha=0.5, fill="pink") + 
#  labs(title="Attriction",x="Attriction",y="No of Employees")

plot1 <- ggplot(employee) + 
  geom_bar(aes(x=BusinessTravel,fill=Attrition),position=position_dodge(width=0.7),alpha=0.7) + 
  labs(title="BusinessTravel & Attrition",x="Business Travel",y="No of Employees",fill="Attrition")


plot2 <- ggplot(employee) + 
  geom_bar(aes(x=Department,fill=Attrition),position=position_dodge(width=0.7),alpha=0.7) + 
  labs(title="Department & Attrition",x="Department",y="No of Employees",fill="Attrition")


plot3 <- ggplot(employee) + 
  geom_bar(aes(x=EducationField,fill=Attrition),position=position_dodge(width=0.7),alpha=0.7) + 
  labs(title="Education Field & Attrition",x="Education Field",y="No of Employees",fill="Attrition")

plot4 <- ggplot(employee) + 
  geom_bar(aes(x=Gender,fill=Attrition),position=position_dodge(width=0.7),alpha=0.7) + 
  labs(title="Gender & Attrition",x="Gender",y="No of Employees",fill="Attrition")

plot5 <- ggplot(employee) + 
  geom_bar(aes(x=JobRole,fill=Attrition),position=position_dodge(width=0.7),alpha=0.7) + 
  labs(title="JobRole & Attrition",x="JobRole",y="No of Employees",fill="Attrition")

plot6 <- ggplot(employee) + 
  geom_bar(aes(x=MaritalStatus,fill=Attrition),position=position_dodge(width=0.7),alpha=0.7) + 
  labs(title="MaritalStatus & Attrition",x="Marital Status",y="No of Employees",fill="Attrition")

plot7 <- ggplot(employee) + 
  geom_bar(aes(x=StockOptionLevel,fill=Attrition),position=position_dodge(width=0.7),alpha=0.7) + 
  labs(title="StockOptionLevel & Attrition",x="StockOption Level",y="No of Employees",fill="Attrition")

grid.arrange(plot1,plot2,plot3,plot4)
grid.arrange(plot5,plot6,plot7)

plot10 <- ggplot(employee) + geom_boxplot(aes(x=Attrition,y=Age,fill=Attrition)) +
  labs(title="Age and Attrition",x="Attrition",y="Age")

plot10_hist <- ggplot(employee) + geom_histogram(aes(x=Age,fill=Attrition),col="black",binwidth=1) +
  labs(title="Age distribution",x="Age",y="No of Employees")

plot11 <- ggplot(employee) + geom_boxplot(aes(x=Attrition,y=DistanceFromHome,fill=Attrition)) +
  labs(title="Distance From Home and Attrition",x="Attrition",y="Distance From Home")

plot11_hist <- ggplot(employee) + geom_histogram(aes(x=DistanceFromHome,fill=Attrition),col="black",binwidth=1) +
  labs(title="Distance From Home Distribution",x="Distance from home",y="No of Employees")

plot12 <- ggplot(employee) + geom_bar(aes(x=Education,fill=Attrition),position=position_dodge(width=0.7),alpha=0.7) +
  scale_y_continuous(breaks=seq(0,1800,100)) +
  labs(title="Education and Attrition",x="Employee Count",y="Education")

plot13 <- ggplot(employee) + geom_bar(aes(x=JobLevel,fill=Attrition),position=position_dodge(width=0.7),alpha=0.7) +
  scale_y_continuous(breaks=seq(1,1800,100)) +
  labs(title="JobLevel and Attrition",x="Employee Count",y="JobLevel")

grid.arrange(plot10, plot11,plot12)
grid.arrange(plot10_hist,plot11_hist)

plot14 <- ggplot(employee) + geom_boxplot(aes(x=Attrition,y=MonthlyIncome,fill=Attrition)) +
  labs(title="MonthlyIncome and Attrition",x="Attrition",y="MonthlyIncome")

plot14_hist <- ggplot(employee) + 
  geom_histogram(aes(x=MonthlyIncome,fill=Attrition),binwidth=5000,col="black") +
  labs(title="Monthly Income Distribution",x="Monthly Income",y="No of employees",fill="Attrition")

plot15 <- ggplot(employee) + geom_boxplot(aes(x=Attrition,y=NumCompaniesWorked,fill=Attrition)) +
  labs(title="NumCompaniesWorked and Attrition",x="Attrition",y="NumCompaniesWorked")

plot16 <- ggplot(employee) + geom_boxplot(aes(x=Attrition,y=PercentSalaryHike,fill=Attrition)) +
  labs(title="PercentSalaryHike and Attrition",x="Attrition",y="PercentSalaryHike")

plot17_hist <- ggplot(employee) + 
  geom_histogram(aes(x=timespend_median,fill=Attrition),col="black",binwidth=1) +
  labs(title="Time spend",y="No of employees",x="Time spend mean")

plot18_hist <- ggplot(employee) + 
  geom_histogram(aes(x=out_time_median,fill=Attrition),col="black",binwidth=1) +
  labs(title="Time spend",y="No of employees",x="Out time median")

plot19_hist <- ggplot(employee) + 
  geom_histogram(aes(x=in_time_median,fill=Attrition),col="black",binwidth=1) +
  labs(title="Time spend",y="No of employees",x="In time median")


plot20 <- ggplot(employee) + geom_boxplot(aes(x=Attrition,y=TotalWorkingYears,fill=Attrition)) +
  labs(title="TotalWorkingYears and Attrition",x="Attrition",y="TotalWorkingYears")

plot20_hist <- ggplot(employee) + 
  geom_histogram(aes(x=TotalWorkingYears,fill=Attrition),col="black",binwidth=1) +
  labs(title="TotalWorkingYears",x="TotalWorkingYears",y="No of Employees")

plot21 <- ggplot(employee) + geom_bar(aes(x=TrainingTimesLastYear,fill=Attrition),position=position_dodge(width=0.7),alpha=0.7) +
  labs(title="TrainingTimesLastYear and Attrition",x="Employee Count",y="TrainingTimesLastYear")

plot22 <- ggplot(employee) + geom_boxplot(aes(x=Attrition,y=YearsAtCompany,fill=Attrition)) +
  labs(title="YearsAtCompany and Attrition",x="Attrition",y="YearsAtCompany")

plot22_hist <- ggplot(employee) + 
  geom_histogram(aes(x=YearsAtCompany,fill=Attrition),col="black",binwidth=1) +
  labs(title="YearsAtCompany",x="YearsAtCompany",y="No of Employees")



grid.arrange(plot14,plot15,plot16,plot20)
grid.arrange(plot14_hist,plot20_hist,plot22_hist,nrow=2,ncol=2)
grid.arrange(plot21, plot22)
grid.arrange(plot17_hist, plot18_hist, plot19_hist)

# Histogram and Boxplots for numeric variables 
box_theme<- theme(axis.line=element_blank(),axis.title=element_blank(), 
                  axis.ticks=element_blank(), axis.text=element_blank())

box_theme_y<- theme(axis.line.y=element_blank(),axis.title.y=element_blank(), 
                    axis.ticks.y=element_blank(), axis.text.y=element_blank(),
                    legend.position="none")

plot_grid(ggplot(employee, aes(MonthlyIncome))+ geom_histogram(binwidth = 10000),
          ggplot(employee, aes(x="",y=MonthlyIncome))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

plot_grid(ggplot(employee, aes(PercentSalaryHike))+ geom_histogram(binwidth = 1),
          ggplot(employee, aes(x="",y=PercentSalaryHike))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

plot_grid(ggplot(employee, aes(DistanceFromHome))+ geom_histogram(binwidth = 1),
          ggplot(employee, aes(x="",y=DistanceFromHome))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1) 


################################################################



################################################################
### Data Preparation

# De-Duplication
# not needed

#Remove unnecerray columns for analysis i.e., columns which contains all the rows same value like NA / 0 / 1 / empty / space
employee <- Filter(function(x)!all(is.na(x) | x==0 | x==1 | x=="" | x==" " | x==8), employee)


# Missing value
sapply(employee, function(x) sum(is.na(x))) # shows all NA

# since numbers are less best is to remove these observations from the analysis
employee <- employee[!is.na(employee$EnvironmentSatisfaction),]
employee <- employee[!is.na(employee$JobSatisfaction),]
employee <- employee[!is.na(employee$WorkLifeBalance),]
employee <- employee[!is.na(employee$NumCompaniesWorked),]
employee <- employee[!is.na(employee$TotalWorkingYears),]

sapply(employee, function(x) sum(is.na(x))) # shows all NA

# Convert to factors.
employee$Education <- factor(employee$Education)
employee$JobLevel <- factor(employee$JobLevel)
employee$EnvironmentSatisfaction <- factor(employee$EnvironmentSatisfaction)
employee$JobInvolvement <- factor(employee$JobInvolvement)
employee$JobSatisfaction <- factor(employee$JobSatisfaction)
employee$PerformanceRating <- factor(employee$PerformanceRating)
employee$WorkLifeBalance <- factor(employee$WorkLifeBalance)
employee$StockOptionLevel <- factor(employee$StockOptionLevel)
employee$Attrition<- ifelse(employee$Attrition=="Yes",1,0)

# Attriction rate is 16 %.
sum(employee$Attrition) / length(employee$Attrition)

# Next step is to treat the outliers (if any). To check for outliers, we find out the quantile values 
# at each 1% interval and wherever there is a high jump from one
# quantile to another
q1 <- quantile(employee$Age, seq(0,1,0.01))
q2 <- quantile(employee$MonthlyIncome, seq(0,1,0.01))
qqnorm(employee$Age, pch = 1,  main = "Age plot",
       xlab = "age", ylab = "Percntile")

qqnorm(employee$MonthlyIncome, pch = 1,  main = "Montly income",
       xlab = "Income", ylab = "Percntile")

ggplot(employee) + 
  geom_histogram(aes(x=YearsAtCompany,fill=Attrition),col="black",binwidth=1) +
  labs(title="YearsAtCompany",x="YearsAtCompany",y="No of Employees")

################################################################
# Feature standardisation

# Normalising continuous features 
employee$DistanceFromHome<- scale(employee$DistanceFromHome) 
#employee$EmployeeCount<- scale(employee$EmployeeCount) 
employee$MonthlyIncome<- scale(employee$MonthlyIncome) 
employee$NumCompaniesWorked<- scale(employee$NumCompaniesWorked) 
employee$PercentSalaryHike<- scale(employee$PercentSalaryHike) 

employee$TotalWorkingYears<- scale(employee$TotalWorkingYears) 
employee$TrainingTimesLastYear<- scale(employee$TrainingTimesLastYear) 
employee$YearsAtCompany<- scale(employee$YearsAtCompany) 
employee$YearsSinceLastPromotion<- scale(employee$YearsSinceLastPromotion) 
employee$YearsWithCurrManager<- scale(employee$YearsWithCurrManager) 
employee$Age<- scale(employee$Age) 
employee$out_time_median<- scale(employee$out_time_median) 
employee$in_time_median<- scale(employee$in_time_median) 
employee$timespend_median<- scale(employee$timespend_median) 

# Since over 18 has one value can be removed.
employee <- employee[ -c(grep("Over18", colnames(employee))) ]

# creating dummy variables for factor attributes
employee_chr<- employee[,-c(grep("EmployeeID", colnames(employee)),
                            grep("Age", colnames(employee)),
                            grep("Attrition", colnames(employee)),
                            grep("DistanceFromHome", colnames(employee)),
                            grep("NumCompaniesWorked", colnames(employee)),
                            grep("PercentSalaryHike", colnames(employee)),
                            grep("TrainingTimesLastYear", colnames(employee)),  
                            grep("YearsAtCompany", colnames(employee)),  
                            grep("YearsSinceLastPromotion", colnames(employee)),  
                            grep("YearsWithCurrManager", colnames(employee)),  
                            grep("MonthlyIncome", colnames(employee)),                              
                            grep("out_time_median", colnames(employee)),  
                            grep("in_time_median", colnames(employee)),  
                            grep("timespend_median", colnames(employee)),                              
                            grep("TotalWorkingYears", colnames(employee)))]
dummies<- data.frame(sapply(employee_chr, 
                            function(x) data.frame(model.matrix(~x-1,data =employee_chr))[,-1]))

employee_final<- cbind(employee[,c(grep("Age", colnames(employee)),
                                  grep("Attrition", colnames(employee)),
                                  grep("DistanceFromHome", colnames(employee)),
                                  grep("NumCompaniesWorked", colnames(employee)),
                                  grep("PercentSalaryHike", colnames(employee)),
                                  grep("TrainingTimesLastYear", colnames(employee)),  
                                  grep("YearsAtCompany", colnames(employee)),  
                                  grep("YearsSinceLastPromotion", colnames(employee)),  
                                  grep("YearsWithCurrManager", colnames(employee)),  
                                  grep("MonthlyIncome", colnames(employee)),         
                                  grep("out_time_median", colnames(employee)),  
                                  grep("in_time_median", colnames(employee)),  
                                  grep("timespend_median", colnames(employee)),                              
                                  grep("TotalWorkingYears", colnames(employee)))],dummies) 
str(employee_final)


########################################################################
# splitting the data between train and test
set.seed(100)

indices = sample.split(employee_final$Attrition, SplitRatio = 0.7)

train = employee_final[indices,]

test = employee_final[!(indices),]

########################################################################
# Logistic Regression: 

#Initial model
model_1 = glm(Attrition ~ ., data = train, family = "binomial")
summary(model_1) 

# Stepwise selection
library("MASS")
model_2<- stepAIC(model_1, direction="both")

summary(model_2)

# Removing multicollinearity through VIF check
library(car)
vif(model_2)
# AIC=2092.79

model_3 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + timespend_median + 
                 TotalWorkingYears + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
                 BusinessTravel.xTravel_Rarely + Education.x5 + EducationField.xLife.Sciences + 
                 EducationField.xMarketing + EducationField.xMedical + EducationField.xOther + 
                 EducationField.xTechnical.Degree + JobLevel.x2 + JobLevel.x5 + 
                 JobRole.xHuman.Resources + JobRole.xManager + JobRole.xManufacturing.Director + 
                 JobRole.xResearch.Director + JobRole.xSales.Executive + MaritalStatus.xMarried + 
                 MaritalStatus.xSingle + StockOptionLevel.x1 + JobInvolvement.x3, 
               family = "binomial", data = train)

# AIC: 2092.8
summary(model_3)

# Remove Education.x5, JobLevel.x2, JobLevel.x5  JobRole.xHuman.Resources
# MaritalStatus.xMarried, StockOptionLevel.x1, JobInvolvement.x3, JobRole.xManager  
# JobRole.xSales.Executive
# All ., 1 star.
model_4 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + timespend_median + 
                 TotalWorkingYears + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
                 BusinessTravel.xTravel_Rarely + EducationField.xLife.Sciences + 
                 EducationField.xMarketing + EducationField.xMedical + EducationField.xOther + 
                 EducationField.xTechnical.Degree +  
                 JobRole.xManufacturing.Director + 
                 JobRole.xResearch.Director + 
                 MaritalStatus.xSingle, 
               family = "binomial", data = train)
# AIC: 2117.6
summary(model_4)
vif(model_4)

# Remove JobRole.xResearch.Director
# BusinessTravel.xTravel_Rarely 
# Remove 1 and two star
model_5 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + timespend_median + 
                 TotalWorkingYears + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
                 EducationField.xLife.Sciences + 
                 EducationField.xMarketing + EducationField.xMedical + EducationField.xOther + 
                 EducationField.xTechnical.Degree +  
                 JobRole.xManufacturing.Director + 
                 MaritalStatus.xSingle, 
               family = "binomial", data = train)
# AIC: 2128.8
summary(model_5)
vif(model_5)

# Remove EducationField.xMedical for low vif, EducationField.xLife.Sciences 
# and EducationField.xMedical have high vif value.
model_6 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + timespend_median + 
                 TotalWorkingYears + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
                 EducationField.xLife.Sciences + 
                 EducationField.xMarketing + EducationField.xOther + 
                 EducationField.xTechnical.Degree +  
                 JobRole.xManufacturing.Director + 
                 MaritalStatus.xSingle, 
               family = "binomial", data = train)
# AIC: 2144.2
summary(model_6)
vif(model_6)

# remove EducationField.xLife.Sciences, EducationField.xMarketing, EducationField.xOther, 
# EducationField.xTechnical.Degree
# Remove zero star.
model_7 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + timespend_median + 
                 TotalWorkingYears + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
                 JobRole.xManufacturing.Director + 
                 MaritalStatus.xSingle, 
               family = "binomial", data = train)
# AIC: 2139.9
summary(model_7)
vif(model_7)

# WorkLifeBalance.x2 and WorkLifeBalance.x3 has vif, but can remove WorkLifeBalance.x2 and WorkLifeBalance.x4.
model_8 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + timespend_median + 
                 TotalWorkingYears + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x3 + 
                 BusinessTravel.xTravel_Frequently + 
                 JobRole.xManufacturing.Director + 
                 MaritalStatus.xSingle, 
               family = "binomial", data = train)
#AIC: 2155.6
summary(model_8)
vif(model_8)

# Remove  TrainingTimesLastYear  EnvironmentSatisfaction.x2 , EnvironmentSatisfaction.x3
model_9 <- glm(formula = Attrition ~ Age + NumCompaniesWorked +  
                 YearsSinceLastPromotion + YearsWithCurrManager + timespend_median + 
                 TotalWorkingYears + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x3 + 
                 BusinessTravel.xTravel_Frequently + 
                 JobRole.xManufacturing.Director + 
                 MaritalStatus.xSingle, 
               family = "binomial", data = train)
# AIC: 2191.2
summary(model_9)
vif(model_9)


########################################################################
# With 11 significant variables in the model

final_model<- model_9

#######################################################################

### Model Evaluation

### Test Data ####

#predicted probabilities of Churn 1 for test data

test_pred = predict(final_model, type = "response", 
                    newdata = test)


# Let's see the summary 

summary(test_pred)

test$prob <- test_pred
View(test)
# Let's use the probability cutoff of 50%.

test_pred_attriction <- factor(ifelse(test_pred >= 0.50, "Yes", "No"))
test_actual_attriction <- factor(ifelse(test$Attrition==1,"Yes","No"))


table(test_actual_attriction,test_pred_attriction)


#######################################################################
test_pred_attriction <- factor(ifelse(test_pred >= 0.40, "Yes", "No"))

#install.packages("e1071")
library(e1071)
library(caret)
test_conf <- confusionMatrix(test_pred_attriction, test_actual_attriction, positive = "Yes")
test_conf
#######################################################################


#######################################################################

#########################################################################################
# Let's Choose the cutoff value. 
# 

# Let's find out the optimal probalility cutoff 

perform_fn <- function(cutoff) 
{
  predicted_attriction <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_attriction, test_actual_attriction, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Creating cutoff values from 0.003575 to 0.812100 for plotting and initiallizing a matrix of 100 X 3.

# Summary of test probability

summary(test_pred)

s = seq(.01,.80,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 


plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.01)]


test_cutoff_attriction <- factor(ifelse(test_pred >=.17757, "Yes", "No"))

conf_final <- confusionMatrix(test_cutoff_attriction, test_actual_attriction, positive = "Yes")

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc

sens

spec

View(test)


##################################################################################################
### KS -statistic - Test Data ######

test_cutoff_attriction <- ifelse(test_cutoff_attriction=="Yes",1,0)
test_actual_attriction <- ifelse(test_actual_attriction=="Yes",1,0)

#install.packages("ROCR")
library(ROCR)
#on testing  data
pred_object_test<- prediction(test_cutoff_attriction, test_actual_attriction)

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test)

####################################################################
# Lift & Gain Chart 

# plotting the lift chart

# Loading dplyr package 
require(dplyr)
library(dplyr)

lift <- function(labels , predicted_prob,groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}

attriction_decile = lift(test_actual_attriction, test_pred, groups = 10)
