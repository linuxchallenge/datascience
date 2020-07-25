################################ Stringr #####################################
##############################################################################
########################### Optional Exercise ################################

library(stringr)

# Loading sales data
sales <- read.csv("stringr_sales_data.csv",stringsAsFactors = F)
str(sales)

##############################################################################

# Orders in 2012
sales_12 <- str_count(sales$Order.ID, pattern="-2012-")
sum(sales_12)

##############################################################################

# Phone category
sales_phone <- str_count(sales$Product.ID, pattern = "-PH-")
sum(sales_phone)

##############################################################################

# Extracting the first and last name's initial alphabets
# The general pattern is:
first_last <- str_split_fixed("First Last", pattern = " ", n = 2)
str(first_last)
first_last
str_extract(first_last, pattern = "[A-Z]{1}")

##############################################################################

# Spliting the first and last name. 
split_prod <- str_split_fixed(sales$Customer.Name," ", 2)
str(split_prod)
head(split_prod)

##############################################################################

# Can be easily converted to a df
split_prod <- data.frame(split_prod)
str(split_prod)
head(split_prod)

##############################################################################

# Extracting initials character from first and last name

# First names first character
first_initials <- str_extract(split_prod$X1,"[A-z]{1}")
head(first_initials)

# Last name's first character
last_initials <- str_extract(split_prod$X2,"[A-z]{1}")

##############################################################################

# Concatenating first and last names first character in "cust_code" vactor.
cust_code <- str_c(first_initials, last_initials)
head(cust_code)

##############################################################################

# Append the "cust_code" with sales data
sales <- cbind(sales[,c(1:3)],cust_code,sales[, c(4:8)])
str(sales)

##############################################################################

# Extracting characters from the "Customer.ID" and store it in "new_cust" column.
sales$new_cust <- str_extract(sales$Customer.ID,"[A-z]+")
head(sales$new_cust)

##############################################################################

# Matching with "new_cust" values with "cust_code". 
sales$check <- ifelse(is.na(sales$cust_code==sales$new_cust),1,0)
head(sales$check)
sum(sales$check) # 34 are not matching

##############################################################################

# Let's see these 34 rows
sales_check_1 <- sales[which(sales$check==1),]

# You can see that in 'sales_check_1'(34 rows), the "Customer.Name" first and last name is seperated by
# "-"(desh) not with " ".

##############################################################################


# Nokia product sales 

# Before calculating the sales figure, lets fix case sensitivity problem in "Product.Name".
# Lower case all "Product.Name" column. 

sales$Product.Name <- str_to_lower(sales$Product.Name, locale = "en")

# creating a df containing "nokia" word in "Product.Name" column in sales data.

nokia_sales <- sales[which(str_detect(sales$Product.Name,"nokia")==T),]


##############################################################################

# Calculating sum of total sales of nokia company

sum(nokia_sales$Sales)

###########################################################################################