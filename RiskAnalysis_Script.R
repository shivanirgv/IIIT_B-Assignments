library(dplyr)
library(stringr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(scales)

#Set working directory
#Commenting it out for submission
#setwd("C:/Users/Kishore/Desktop/Surya/Program/Risk Analysis Case Study")

#Load the data in .csv file into uberdata dataframe
masterdata <- read.csv("Loan.csv",stringsAsFactors = FALSE,strip.white = TRUE,na.strings=c("","NA"))


#Check the number of rows and columns in the dataframe
nrow(masterdata)
ncol(masterdata)


#See the summary statistics for the data in the uberdata dataframe
summary(masterdata)

masterdata_clean <- masterdata[,-c(5,)]

#############################################Univariate Analysis################################################

#Lets see some statistics on the loan amounts in the data set

#Loan Amounts requested 
summary(masterdata$loan_amnt)
boxplot(masterdata$loan_amnt)

#Loan Amounts requested
summary(masterdata$loan_amnt)
boxplot(masterdata$funded_amnt)

#Term of loans - create a bar chart or a pie chart 
summary(as.factor(masterdata$term))
ggplot(masterdata$term,aes(x=Pickup.point)) + geom_bar(width = .4, fill = "#0073C2FF",stat="count") + 
  geom_text(aes(label =count), vjust = -0.3) +
  labs(title= "XXX",y="Count")+  
  theme(plot.title = element_text(size=10,hjust = 0.5))

#Interest % - labels etc.
summary(masterdata$int_rate)
masterdata$int_rate <- str_remove_all(masterdata$int_rate,"%")
boxplot(as.numeric(masterdata$int_rate))


#Grades - Pie Chart - Rosalin
#SubGrades - Pie Chart  - Rosalin

#Experience - Histogram - Sujani
View(masterdata)
#Home Ownership - Bar - Shivani
ggplot(masterdata,aes(x=home_ownership)) + geom_bar(width = .4, fill = "#0073C2FF",stat="count") +
  labs(x= "Home Ownership", y="Count")+  theme(plot.title = element_text(size=10,hjust = 0.5))

#Annual Income - Histogram - Shivani
ggplot(masterdata,aes(x=annual_inc)) + geom_histogram(binwidth = 50000, fill="red") + labs(x= "Annual Income", y="Count")
#Verification Status - Bar - Sujani

#Loan Status - %age values as Bar/Pie  - Surya
summary(as.factor(masterdata$loan_status))

#Purpose - Word cloud + Bar Chart - Surya
summary(as.factor(masterdata$purpose))

#ZipCode - Map - Surya
#Address State - Map - Surya

#dti
summary(masterdata$dti)
boxplot(masterdata$dti)

#delinquent_2yrs - Sujani

#earliest_creditLine - Date format issue to be resolved - Sujani
#mths_since_last_delinquent - Shivani

ggplot(masterdata,aes(x=mths_since_last_delinq)) + geom_bar() 
#mths_since_last_record - Shivani
ggplot(masterdata,aes(x=mths_since_last_record)) + geom_bar(stat="count") + labs(x= "Months Since last Public record", y="Count")

