#Business Understanding
#You work for a consumer finance company which specialises in lending various types of loans to urban customers. When the company receives
#a loan application, the company has to make a decision for loan approval based on the applicant’s profile. Two types of risks are associated 
#with the bank’s decision:
#If the applicant is likely to repay the loan, then not approving the loan results in a loss of business to the company
#If the applicant is not likely to repay the loan, i.e. he/she is likely to default, then approving the loan may lead to a financial loss for the company
#The data given below contains the information about past loan applicants and whether they ‘defaulted’ or not. The aim is to identify patterns 
#which indicate if a person is likely to default, which may be used for taking actions such as denying the loan, reducing the amount of
#loan, lending (to risky applicants) at a higher interest rate, etc.

#When a person applies for a loan, there are two types of decisions that could be taken by the company:
#Loan accepted: If the company approves the loan, there are 3 possible scenarios described below:
#Fully paid: Applicant has fully paid the loan (the principal and the interest rate)
#Current: Applicant is in the process of paying the instalments, i.e. the tenure of the loan is not yet completed. These candidates are not labelled as 'defaulted'.
#Charged-off: Applicant has not paid the instalments in due time for a long period of time, i.e. he/she has defaulted on the loan 
#Loan rejected: The company had rejected the loan (because the candidate does not meet their requirements etc.). Since the loan was rejected, 
#there is no transactional history of those applicants with the company and so this data is not available with the company (and thus in this dataset)

#Business Objectives
#The company wants to understand the driving factors (or driver variables) behind loan default, i.e. the variables which are strong indicators 
#of default.  The company can utilise this knowledge for its portfolio and risk assessment. 



library(dplyr)
library(stringr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(scales)
library(wordcloud2)
library(corrplot)

#Load the data in .csv file into uberdata dataframe
masterdata <- read.csv("Loan.csv",stringsAsFactors = FALSE,strip.white = TRUE,na.strings=c("N/A","n/a","","NA"))

#Check the number of rows and columns in the dataframe
nrow(masterdata)
ncol(masterdata)

#See the summary statistics for the data in the uberdata dataframe
summary(masterdata)


#############################################Univariate Analysis################################################

#Lets see some statistics on the loan amounts in the data set

#Loan Amounts requested 
#Summarise the stats for loan amount
summary(masterdata$loan_amnt)
# Draw a box plot for loan amount
#Result 
#1) Quite a lot of outliers 
#2) Median is $10000. Data seems to be normally distributed as median is midway in the interquartile range. 
boxplot(masterdata$loan_amnt, notch=TRUE, 
        col=(c("darkgreen")), 
        xlab="Box plot for Loan Amount", ylab="$")

#Term of loans 
#Aggregate data by term of loans
dfLoanTerm <- masterdata %>%
  group_by(term) %>%
  summarise(counts = n()) %>%
  mutate(percentage=trunc(counts*100/sum(counts)))  

#Plot term of loans aggregate data as a bar chart
#Result - Majority of the loans in the sample dataset are for 36months(73%). 26% of loans have 60 months as loan term
ggplot(dfLoanTerm,aes(x=term,y=percentage)) + geom_bar(width = .4, fill = "#0073C2FF",stat="identity") +
  theme(plot.title = element_text(size=10,hjust = 0.5)) +
  geom_text(aes(label =percentage), position = position_stack(vjust = 0.9))+
  labs(title= "% Customers vs Term",x= "Term",y="% Customers")+  
  theme(plot.title = element_text(size=10,hjust = 0.5))


#Interest % 
#Summarise the stats for interest rate
summary(masterdata$int_rate)
#Strip all % from the data for analysis purpose
masterdata$int_rate <- str_remove_all(masterdata$int_rate,"%")
# Draw a box plot for interest rate
#Result 
#1) There are quite a few outliers which shows that few loans tend to have very high interest rates 
#2) Median is 11.86%. Data seems to be normally distributed as median is midway in the interquartile range. 
boxplot(masterdata$int_rate, 
        col=(c("darkgreen")), 
        xlab="Box plot for Interest Rate", ylab="$")

#Grades
#Aggregate data by grade
dfGrade <- masterdata %>%
  group_by(grade) %>%
  summarise(counts = n()) %>%
  mutate(percentage=trunc(counts*100/sum(counts))) 

#Create a bar chart for grade
#Result  Approx 30% loans are grade B.~75% of the loans are of Grade A, B C. 
ggplot(dfGrade,aes(x=grade,y=percentage)) + geom_bar(width = .4, fill = "#0073C2FF",stat="identity") +
  theme(plot.title = element_text(size=10,hjust = 0.5)) +
  geom_text(aes(label =percentage), position = position_stack(vjust = 0.9))+
  labs(title= "Loan Grade Distribution",x="Loan Grade",y="% Customers")+  
  theme(plot.title = element_text(size=10,hjust = 0.5))

#SubGrades
#Aggregate data by grade
dfSubGrade <- masterdata %>%
  group_by(sub_grade) %>%
  summarise(counts = n()) %>%
  mutate(percentage=trunc(counts*100/sum(counts))) 

#Create a bar chart for sub grade
#Result  Approx 30% loans are grade B.~75% of the loans are of Grade A, B C. 
ggplot(dfSubGrade,aes(x=sub_grade,y=percentage)) + geom_bar(width = .4, fill = "#0073C2FF",stat="identity") +
  theme(plot.title = element_text(size=10,hjust = 0.5)) +
  geom_text(aes(label =percentage), position = position_stack(vjust = 0.9))+
  labs(title= "Loan Sub Grade Distribution",x="Loan Sub Grade",y="% Customers")+  
  theme(plot.title = element_text(size=10,hjust = 0.5))

#Experience
#Aggregate data by emp_length
dfEmpgLength <- masterdata %>%
  group_by(emp_length) %>%
  summarise(counts = n()) %>%
  mutate(percentage=trunc(counts*100/sum(counts))) 

#Create a bar chart for emp_length
#Result  22% of the loans are for customers with 10+ years emp length. This is the largest category 
# 40% of customers have emp length less than 3 yrs
ggplot(dfEmpgLength,aes(x=emp_length,y=percentage)) + geom_bar(width = .4, fill = "#0073C2FF",stat="identity") +
  theme(plot.title = element_text(size=10,hjust = 0.5)) +
  geom_text(aes(label =percentage), position = position_stack(vjust = 0.9))+
  labs(title= "Employee Length Distribution",x="Employee Length",y="% Customers")+  
  theme(plot.title = element_text(size=10,hjust = 0.5))

#Home Ownership
#Aggregate the data based on home_ownership
dfHomeOwnerShip <- masterdata %>%
  group_by(home_ownership) %>%
  summarise(counts = n()) %>%
  mutate(percentage=trunc(counts*100/sum(counts))) 

#Create a bar chart for home_ownership
#Result
# 47%(majority) of the customers stay in rented accomodation. 44% of the customers have mortgages and 7% have their own homes.
ggplot(dfHomeOwnerShip,aes(x=home_ownership,y=percentage)) + geom_bar(width = .4, fill = "#0073C2FF",stat="identity") +
    theme(plot.title = element_text(size=10,hjust = 0.5)) +
  geom_text(aes(label =percentage), position = position_stack(vjust = 0.9)) +
  labs(title= "% Customers vs Home Ownership",x= "Home Ownership",y="% Customers")+  
  theme(plot.title = element_text(size=10,hjust = 0.5))

#Annual Income 
# Summarize the stats for annual_inc
summary(masterdata$annual_inc)

#Create a histogram of annual income to see the income distribution
#Results - There are quite a few outliers as depcited in the plot. Majority of the customers have annual incomes 
#less than $100K
qplot(masterdata$annual_inc,
      geom="histogram",
      binwidth = 10000,  
      main = "Histogram for Annual Income", 
      xlab = "Annual Income",  
      fill=I("blue"), 
      col=I("red"), 
      alpha=I(.2),
      xlim=c(2000,600000))

#Verification Status
#Aggregate data by verification status
dfVerStatus <- masterdata %>%
  group_by(verification_status) %>%
  summarise(counts = n()) %>%
  mutate(percentage=trunc(counts*100/sum(counts)))  

#Create a bar plot for verification status
#Result - 42 % of the customers are not verified. 
ggplot(dfVerStatus,aes(x=verification_status,y=percentage)) + geom_bar(width = .4, fill = "#0073C2FF",stat="identity") +
  theme(plot.title = element_text(size=10,hjust = 0.5)) +
  geom_text(aes(label =percentage), position = position_stack(vjust = 0.9))+
  labs(title= "% Customers vs Verification Status",x= "Verification Status",y="% Customers")+  
  theme(plot.title = element_text(size=10,hjust = 0.5))

#Loan Status
#Aggregate data by loan status
dfLoanStatus <- masterdata %>%
group_by(loan_status) %>%
summarise(counts = n()) %>%
mutate(percentage=trunc(counts*100/sum(counts)))  

#Create a bar plot for loan status
#Result - 82%(majority) of the loans are Fully Paid. 11% of the loans are in Charged Off status.
ggplot(dfLoanStatus,aes(x=loan_status,y=percentage)) + geom_bar(width = .4, fill = "#0073C2FF",stat="identity") +
   theme(plot.title = element_text(size=10,hjust = 0.5)) +
  geom_text(aes(label =percentage), position = position_stack(vjust = 0.9))+
labs(title= "% Customers vs Loan Status",x= "Loan Status",y="% Customers")+  
  theme(plot.title = element_text(size=10,hjust = 0.5))

#Title - Word cloud
dfTitle <- data.frame(table(masterdata$title))

#Draw a word cloud to visually see major loan purpose reasons
wordcloud2(dfTitle, size = 3,minSize = 2)


#delinquent_2yrs - Sujani
#Aggregate data by delinq_2yrs
dfdelinq2yrs <- masterdata %>%
  group_by(delinq_2yrs) %>%
  summarise(counts = n()) %>%
  mutate(percentage=trunc(counts*100/sum(counts))) 

#Create a bar chart for delinq_2yrs
#Results - 89% of the customers have 0 delinquencies in the last 2 yrs
ggplot(dfdelinq2yrs,aes(x=delinq_2yrs,y=percentage)) + geom_bar(width = .4, fill = "#0073C2FF",stat="identity") +
  theme(plot.title = element_text(size=10,hjust = 0.5)) +
  geom_text(aes(label =percentage), position = position_stack(vjust = 0.9)) +
  labs(title= "% Customers vs No of delinquencies in last 2yrs",x= "Count of Delinquencies",y="% Customers")+  
  theme(plot.title = element_text(size=10,hjust = 0.5))

#################################################MULTI VARIATE ANALYSIS#######################################
#Correlation Analysis
#Create a subset dataframe of relevant columns from the master data frame
dfCorrelation <- select(masterdata,loan_amnt,term,int_rate,installment,annual_inc,dti,delinq_2yrs,inq_last_6mths,open_acc,revol_util)

#Convert NA values in data to 0 for correlation analysis
dfCorrelation[is.na(dfCorrelation)] <- 0


#Strip "months" from the term data for analysis purpose
dfCorrelation$term <- str_remove_all(dfCorrelation$term," months")
#Strip "%" from the term data for analysis purpose
dfCorrelation$revol_util <- str_remove_all(dfCorrelation$revol_util,"%")



#Convert term to numeric for correlation analysis
dfCorrelation$term <-as.numeric(dfCorrelation$term)
#Convert revol_util to numeric for correlation analysis
dfCorrelation$int_rate <-as.numeric(dfCorrelation$int_rate)
#Convert loan_amt to numeric for correlation analysis
dfCorrelation$revol_util <-as.numeric(dfCorrelation$revol_util)

#Create the correlation matrix for the columns in the new subset dataframe
corr_matrix<- round(cor(dfCorrelation),2)

#Create a correlation plot based on the correlation matrix identified
col<- colorRampPalette(c("red", "white", "blue"))(20)
corrplot(corr_matrix,method = "pie", type="upper",order="hclust",col=col)

#Result
#There are no variables where correlation is very positive
#Term and revol_util have highest correlation(1) followed by Installment and loan amount(.93) have the highest correlation
#The other somewhat positive correlation is between interest rate and term(.45) and interest rate and revol util(.45)
#No significant negative correlation were observed

#Create a box plot for comparing loan amount range against grade of loan
ggplot(masterdata,aes(y = loan_amnt, x = grade,fill=grade)) + geom_boxplot()
#Result - 
#Median value of loan amounts with grade A is least
#For Grades B, C, D the medians are approximately same
# As the loan grades move towards E to G the median loan amount is increasing.


#Strip "%" from the term data for analysis purpose
masterdata$int_rate <- str_remove_all(masterdata$int_rate,"%")
#Create a box plot for comparing int_rate range against grade of loan
ggplot(masterdata,aes(y = as.numeric(int_rate), x = grade,fill=grade)) + geom_boxplot()
#Result - 
#Median value of int_rate with grade A is least with an increasing trend from Grade A to Grade G
#i.e LendingClub is charing higher interest rates for higher grade loans

#Aggregate data by grade and loan status
dfLoanStatusvsGrades <- masterdata %>%
  group_by(grade,loan_status) %>%
  summarise(counts = n()) %>%
  mutate(percentage=trunc(counts*100/sum(counts)))

#plot bar chart for grade vs loan status
ggplot(data = dfLoanStatusvsGrades, aes(x=dfLoanStatusvsGrades$grade,y=percentage,fill=loan_status)) + geom_bar(stat = "identity")
#Result - we see that default rate i.e Charged off loans increase as we move from better grade(A) to bad grade(G)

#Derive metrics

#Create Interest in Low, Medium, High and Very High categories
masterdata <- mutate(masterdata, IntCategory = ifelse(int_rate <= 10, "Low",
                                                      ifelse(int_rate <=15 , "Medium",
                                                             ifelse(int_rate <=20, "High",
                                                                    "Very High"))))
#Aggregate data
dfLoanStatusvsInt <- masterdata %>%
  group_by(loan_status,IntCategory) %>%
  summarise(counts = n()) %>%
  mutate(percentage=trunc(counts*100/sum(counts))) 



ggplot(data = dfLoanStatusvsInt, aes(x=loan_status,y=percentage,fill=IntCategory)) + geom_bar(stat = "identity")

#Create Revolving utilization in Low, Medium, High and Very High categories
masterdata <- mutate(masterdata, RevUtilCategory = ifelse(revol_util <= 30, "Low",
                                                          ifelse(revol_util <=50 , "Medium",
                                                                 ifelse(revol_util <=75, "High",
                                                                        "Very High"))))
#Aggregate data
dfLoanStatusvsRevUtilCategory <- masterdata %>%
  group_by(loan_status,RevUtilCategory) %>%
  summarise(counts = n()) %>%
  mutate(percentage=trunc(counts*100/sum(counts))) 

#Plot
ggplot(data = dfLoanStatusvsRevUtilCategory, aes(x=loan_status,y=percentage,fill=RevUtilCategory)) + geom_bar(stat = "identity")


#Create Installment into Low, Medium, High and Very High categories
masterdata <- mutate(masterdata, InstallmentCategory = ifelse(installment <= 150, "Low",
                                                              ifelse(installment <=300 , "Medium",
                                                                     ifelse(installment <=450, "High",
                                                                            "Very High"))))
                                                                                                                                                       
#Aggregate data
dfLoanStatusvsInstallmentCategory <- masterdata %>%
  group_by(loan_status,InstallmentCategory) %>%
  summarise(counts = n()) %>%
  mutate(percentage=trunc(counts*100/sum(counts))) 

#Plot
ggplot(data = dfLoanStatusvsInstallmentCategory, aes(x=loan_status,y=percentage,fill=InstallmentCategory)) + geom_bar(stat = "identity")




#Create Annual Income in Low, Medium, High and Very High categories
masterdata <- mutate(masterdata, IncomeCategory = ifelse(annual_inc <= 40000, "<40k",
                                                         ifelse(annual_inc <=60000 , "40k-60k",
                                                                ifelse(annual_inc <=80000, "60k-80k",
                                                                       ifelse(annual_inc <=100000, "80k-100k",">100k"
                                                                       )))))


#Aggregate data
dfLoanStatusvsIncomeCategory <- masterdata %>%
  group_by(loan_status,IncomeCategory) %>%
  summarise(counts = n()) %>%
  mutate(percentage=trunc(counts*100/sum(counts))) 

#Plot
ggplot(data = dfLoanStatusvsIncomeCategory, aes(x=loan_status,y=percentage,fill=IncomeCategory)) + geom_bar(stat = "identity")

