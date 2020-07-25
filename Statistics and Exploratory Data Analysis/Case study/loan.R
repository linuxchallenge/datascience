# Load
library("dplyr")
library("stringr")
library("ggplot2")

# Loading dataset 
loanRecords <- read.csv("loan.csv", stringsAsFactors=FALSE)

# Structure of dataset
str(loanRecords)

#Data Cleaning

#Finding Duplicate Records
loanRecords_distn <- distinct(loanRecords)

duplicate_records <- nrow(loanRecords) - nrow(loanRecords_distn)
if(duplicate_records == 0) {
  print("No Duplicate Records Based on Load ID")
} else {
  paste("Duplicate Records Based on Load ID : ", duplicate_records)
}

#Remove unnecerray columns for analysis i.e., columns which contains all the rows same value like NA / 0 / 1 / empty / space
loanRecords_distn <- Filter(function(x)!all(is.na(x) | x==0 | x==1 | x=="" | x==" "), loanRecords_distn)

# Convert rate and term to numeric.
loanRecords_distn$term <- str_extract(loanRecords_distn$term, "[[:digit:]]+")
loanRecords_distn$int_rate = substr(loanRecords_distn$int_rate,1,nchar(loanRecords_distn$int_rate)-1)
loanRecords_distn$int_rate <- as.numeric(loanRecords_distn$int_rate)


#Change few columns into factors
cols <- c("grade","sub_grade","home_ownership","verification_status","loan_status","pymnt_plan","purpose","addr_state","application_type","term")
loanRecords_distn[cols] <- lapply(loanRecords_distn[cols], factor)
str(loanRecords_distn)

#Removing Characters from Terms, Employee Length & Revol Util columns
regexp_for_rem_chars <- "[a-zA-Z + < / %]"

loanRecords_distn$emp_length <- gsub(regexp_for_rem_chars, "", loanRecords_distn$emp_length)
loanRecords_distn$term <- gsub(regexp_for_rem_chars, "", loanRecords_distn$term)
loanRecords_distn$revol_util <- gsub(regexp_for_rem_chars, "", loanRecords_distn$revol_util)
loanRecords_distn$emp_length <- as.numeric(loanRecords_distn$emp_length)
loanRecords_distn$term <- as.numeric(loanRecords_distn$term)
loanRecords_distn$revol_util <- as.numeric(loanRecords_distn$revol_util)

# Change date and time.
loanRecords_distn$earliest_cr_line <- paste0("1-", loanRecords_distn$earliest_cr_line)
loanRecords_distn$earliest_cr_line <- as.Date(loanRecords_distn$earliest_cr_line,format="%d-%b-%y")
loanRecords_distn$earliest_cr_line[loanRecords_distn$earliest_cr_line >= '2009-01-01'] <- NA
loanRecords_distn$earliest_cr_line <- format(as.Date(loanRecords_distn$earliest_cr_line, format="%Y-%m-%d"),"%Y")
loanRecords_distn$earliest_cr_line <- as.numeric(loanRecords_distn$earliest_cr_line)
loanData <- loanRecords_distn
highValueloanData <- subset(loanData, loanData$loan_amnt >=25000)


#Data Analysis
#Univariant analysis
# We can see default rate is 15 %.
D<-nrow(loanData[which(loanData$loan_status == "Charged Off"),])
P<-nrow(loanData[which(loanData$loan_status == "Fully Paid"),])
DefaultRate <- round((D/(D+P)*100))

# We can see 30 % of loan are from 25000 - 35000 amount.
quantile(loanData$loan_amnt, seq(0, 1, 0.05))

# Calculate the balance remaining.
loanData$balance <- loanData$loan_amnt - loanData$total_rec_prncp

# We see 10 % of principle amount is lost beacuse of bad loans.
(aggregate(x = loanData$balance, by = list(loanData$loan_status == 'Charged Off'), FUN = sum)[,2][2]) / sum(loanData$loan_amnt)  * 100

#Segmented Univariant analysis
############ Segmented Univariant analysis ############################
# We can see for charged off loan median is around 10000. Also for charged off there 
# is considerable loan above 30K.
ggplot(loanData, aes(y=loan_amnt, x=loan_status)) + geom_boxplot()

ggplot(loanData, aes(x=grade, y=loan_amnt, fill= grade)) + geom_boxplot() +  labs(title ="Loan Amount by Loan Grade",  x= "Loan Grade", y="Loan Amount" )

ggplot(loanData, aes(y=installment, x=loan_status)) + geom_boxplot()

ggplot(loanData, aes(x=installment, fill=loan_status)) + geom_histogram(position="fill")

# We see as grade decreases charged_off increases.
ggplot(loanData, aes(x=grade, fill = loan_status)) + geom_bar(position="fill")
ggplot(loanData, aes(x=sub_grade, fill=loan_status)) + geom_bar(position="fill")

# There is not much relation between home ownership and loan status
ggplot(loanData, aes(x=home_ownership, fill=loan_status)) + geom_bar(position="fill")

# There is not much relation between purpose and loan status
ggplot(loanData, aes(x=purpose, fill=loan_status)) + geom_bar(position="fill")

# Higher the interest rate charged_off increase.
ggplot(loanData, aes(x=int_rate, fill=loan_status)) + geom_histogram(bin=1, position="fill") 
# Also loan grade decreases when iterest rate increases.
ggplot(loanData, aes(x=int_rate, fill=grade)) + geom_histogram(bin=1, position="fill") 

# Higher the revol_util, bad loans increases.
ggplot(loanData, aes(x=revol_util, fill=loan_status)) + geom_histogram(position="fill")

ggplot(loanData, aes(x=annual_inc, fill=loan_status)) + geom_histogram(bin=2000, position="fill")

# If loan amount increases, then loan status decreases.
ggplot(loanData, aes(x=loan_amnt, fill=loan_status)) + geom_histogram(position="fill")

# we can observe for pub_rec_banrupcies 2 there is more chance of default.
ggplot(loanData, aes(x=pub_rec_bankruptcies, fill=loan_status)) + geom_bar(position="fill")

ggplot(loanData, aes(x=verification_status, fill=loan_status)) + geom_bar(position="fill") +  labs(title ="Verified & Not Verified Loans")

ggplot(loanData, aes(x=emp_length, fill=loan_status)) + geom_bar(position="fill")

ggplot(loanData, aes(x=application_type, fill=loan_status)) + geom_bar(position="fill")

# We can observe if cr_line is there in last 5 years, then chance of default is more.
ggplot(loanData, aes(x=earliest_cr_line, fill=loan_status)) + geom_bar(position="fill")

ggplot(loanData, aes(x=term, fill=loan_status)) + geom_bar(position="fill") +  labs(title ="Loan Term Duration",  x= "Loan Term in Months" )


ggplot(loanData, aes(y=loan_amnt, x=addr_state, fill = loan_status)) + geom_boxplot() + labs(title ="Loan Amount by Loan State",  x= "State", y="Loan Amount")


ggplot(loanData, aes(x=dti, fill=loan_status)) + geom_histogram(position="fill")

ggplot(loanData, aes(y=dti, x=loan_status, , fill= loan_status)) + geom_boxplot() +  labs(title ="debt to income ratio to loan status",  x= "Loan status", y="Debt to income ratio" )

