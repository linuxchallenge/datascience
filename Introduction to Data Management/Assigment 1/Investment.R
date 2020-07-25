library(tidyr)
library(dplyr)
library(plyr)

library(stringr)

# Loading sales data
#round2 <- read.csv("rounds2.csv",stringsAsFactors = F)
round2 <- read.csv("rounds2.csv")
company <- read.delim("companies.txt", sep="\t")

names(round2)[names(round2) == 'company_permalink'] <- 'permalink'

# Modify to upper case, so merge works properly.
company$permalink <- toupper(company$permalink)
round2$permalink <- toupper(round2$permalink)

# 1.1 1
length(unique(round2$permalink))

# 1.1 2
length(unique(company$permalink))

# 1.1 3
# can be used as primary key.
comp_per <- length(unique(company$permalink))

round_per <- length(unique(round2$permalink))

# 1.1 4
if (round_per > comp_per)
  print("In round there are more company then company")

master_frame <- merge(x=round2,y=company,by="permalink")

# 2.1 1 - 4
#1. Average funding amount of venture type	 
Venture <- subset(master_frame, funding_round_type == "venture")
mean(Venture$raised_amount_usd, na.rm = TRUE)
#11305424 

#2. Average funding amount of angel type	 
angel <- subset(master_frame, funding_round_type == "angel")
mean(angel$raised_amount_usd, na.rm = TRUE)
# 958694.5

#3. Average funding amount of seed type	 
seed <- subset(master_frame, funding_round_type == "seed")
mean(seed$raised_amount_usd, na.rm = TRUE)
# 719818

#4. Average funding amount of private equity type	 
pe <- subset(master_frame, funding_round_type == "private_equity")
mean(pe$raised_amount_usd, na.rm = TRUE)
# 73308593 

avg_inst <- aggregate(master_frame$raised_amount_usd, by=list(Category=master_frame$funding_round_type), FUN=mean, na.rm=TRUE)
avg_inst <- arrange(avg_inst, desc(x))
avg_inst <- subset(avg_inst, ((Category=="venture") | (Category=="angel") | (Category=="seed") | (Category=="private_equity")))
avg_inst <- subset(avg_inst, x <= 15000000 & x >= 5000000)
avg_inst[1,1]

# 3
#newdata <- subset(master_frame, raised_amount_usd <= 15000000 & raised_amount_usd >= 5000000)
newdata <- subset(master_frame, master_frame$funding_round_type == avg_inst[1,1])
country <- aggregate(newdata$raised_amount_usd, by=list(Category=newdata$country_code), FUN=sum, na.rm=TRUE)
country <- arrange(country, desc(x))
country <- country[!apply(country, 1, function(x) any(x=="")),] 
country <- head(country, 9)


# 4
newdata <- separate(data = newdata, col = category_list, into = c("primary_sector", "secondary"), sep = "\\|")


mapping <- read.csv("mapping.csv")
names(mapping)[names(mapping) == 'category_list'] <- 'primary_sector'
mapping$primary_sector <- str_replace(mapping$primary_sector, "2.0", "123456")
mapping$primary_sector <- str_replace(mapping$primary_sector, "[0]", "na")
mapping$primary_sector <- str_replace(mapping$primary_sector, "123456", "2.0")

newdata$primary_sector <- toupper(newdata$primary_sector)
mapping$primary_sector <- toupper(mapping$primary_sector)

newdata <- merge(x=newdata, y=mapping, by = "primary_sector", all.x=TRUE)
newdata[, 18][is.na(newdata[, 18])] <- 1
newdata[, 17][is.na(newdata[, 17])] <- 0
newdata[, 19:25][is.na(newdata[, 19:25])] <- 0

newdata <- gather(newdata, key=primary, value=primary_value, c(17:25) )
newdata <- newdata[-which(newdata$primary_value == 0), ]

d1 <- subset(newdata, newdata$raised_amount_usd >= 5000000  & newdata$country_code == 'USA')
d1 <- subset(d1, d1$raised_amount_usd <= 15000000)

d1_agg_sum <- aggregate(d1$raised_amount_usd, by=list(Category=d1$primary), FUN=sum, na.rm=TRUE)
d1_agg_len <- count(d1, "primary")
names(d1_agg_sum)[names(d1_agg_sum) == 'Category'] <- 'primary'
d1_agg <- merge(x= d1_agg_sum, y = d1_agg_len)
d1_agg <- arrange(d1_agg, desc(x))
d1 <- merge(x=d1, y=d1_agg, by = "primary")




d2 <- subset(newdata, newdata$raised_amount_usd >= 5000000  & newdata$country_code == 'GBR')
d2 <- subset(d2, d2$raised_amount_usd <= 15000000)

d2_agg_sum <- aggregate(d2$raised_amount_usd, by=list(Category=d2$primary), FUN=sum, na.rm=TRUE)
d2_agg_len <- count(d2, "primary")
names(d2_agg_sum)[names(d2_agg_sum) == 'Category'] <- 'primary'
d2_agg <- merge(x= d2_agg_sum, y = d2_agg_len)
d2_agg <- arrange(d2_agg, desc(x))
d2 <- merge(x=d2, y=d2_agg, by = "primary")


d3 <- subset(newdata, newdata$raised_amount_usd >= 5000000  & newdata$country_code == 'IND')
d3 <- subset(d3, d3$raised_amount_usd <= 15000000)

d3_agg_sum <- aggregate(d3$raised_amount_usd, by=list(Category=d3$primary), FUN=sum, na.rm=TRUE)
d3_agg_len <- count(d3, "primary")
names(d3_agg_sum)[names(d3_agg_sum) == 'Category'] <- 'primary'
d3_agg <- merge(x= d3_agg_sum, y = d3_agg_len)
d3_agg <- arrange(d3_agg, desc(x))
d3 <- merge(x=d3, y=d3_agg, by = "primary")




# Table 5.1
# 1 
sum(d1_agg$freq)
sum(d1_agg$x)
d1_agg[1, 1]
d1_agg[2, 1]
d1_agg[3, 1]
d1_agg[1, 3]
d1_agg[2, 3]
d1_agg[3, 3]

d1_company <- subset(d1, d1$primary == d1_agg[1, 1])
d1_company <- aggregate(d1_company$raised_amount_usd, by=list(Category=d1_company$name), FUN=sum, na.rm=TRUE)
d1_company <- arrange(d1_company, desc(x))
d1_company[1,1]

d1_company <- subset(d1, d1$primary == d1_agg[2, 1])
d1_company <- aggregate(d1_company$raised_amount_usd, by=list(Category=d1_company$name), FUN=sum, na.rm=TRUE)
d1_company <- arrange(d1_company, desc(x))
d1_company[1,1]


sum(d2_agg$freq)
sum(d2_agg$x)
d2_agg[1, 1]
d2_agg[2, 1]
d2_agg[3, 1]
d2_agg[1, 3]
d2_agg[2, 3]
d2_agg[3, 3]
d2_company <- subset(d2, d2$primary == d2_agg[1, 1])
d2_company <- aggregate(d2_company$raised_amount_usd, by=list(Category=d2_company$name), FUN=sum, na.rm=TRUE)
d2_company <- arrange(d2_company, desc(x))
d2_company[1,1]

d2_company <- subset(d2, d2$primary == d2_agg[2, 1])
d2_company <- aggregate(d2_company$raised_amount_usd, by=list(Category=d2_company$name), FUN=sum, na.rm=TRUE)
d2_company <- arrange(d2_company, desc(x))
d2_company[1,1]




sum(d3_agg$freq)
sum(d3_agg$x)
d3_agg[1, 1]
d3_agg[2, 1]
d3_agg[3, 1]
d3_agg[1, 3]
d3_agg[2, 3]
d3_agg[3, 3]

d3_company <- subset(d3, d3$primary == d3_agg[1, 1])
d3_company <- aggregate(d3_company$raised_amount_usd, by=list(Category=d3_company$name), FUN=sum, na.rm=TRUE)
d3_company <- arrange(d3_company, desc(x))
d3_company[1,1]
d3_company <- subset(d3, d3$primary == d3_agg[2, 1])
d3_company <- aggregate(d3_company$raised_amount_usd, by=list(Category=d3_company$name), FUN=sum, na.rm=TRUE)
d3_company <- arrange(d3_company, desc(x))
d3_company[1,1]

