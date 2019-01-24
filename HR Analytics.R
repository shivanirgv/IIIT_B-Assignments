#Problem Statement
#A large company named XYZ, employs, at any given point of time, around 4000 employees. However, every year, around 15% of its employees
#leave the company and need to be replaced with the talent pool available in the job market. The management believes that this level of 
#attrition (employees leaving, either on their own or because they got fired) is bad for the company, because of the following reasons -

#The former employees’ projects get delayed, which makes it difficult to meet timelines, resulting in a reputation loss among consumers
#and partners
#A sizeable department has to be maintained, for the purposes of recruiting new talent
#More often than not, the new employees have to be trained for the job and/or given time to acclimatise themselves to the company
#Hence, the management has contracted an HR analytics firm to understand what factors they should focus on, in order to curb attrition.
#In other words, they want to know what changes they should make to their workplace, in order to get most of their employees to stay.
#Also, they want to know which of these variables is most important and needs to be addressed right away.

#Goal of the case study
#You are required to model the probability of attrition using a logistic regression. The results thus obtained will be used by the
#management to understand what changes they should make to their workplace, in order to get most of their employees to stay.



library(tidyr)
library(dplyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(gridExtra)
library(corrplot)
library(ROCR)
library(scales)
library(gridExtra)
library(MASS)
library(car)
library(GGally)
library(e1071)
library(caret)
library(cowplot)
library(caTools)
library(ggplot2)
setwd("C:/Users/ACER/Desktop/IIITB-Assignments/Module4/Group case study/PA-I_Case_Study_HR_Analytics")
emp_survey_data<- read.csv("employee_survey_data.csv", stringsAsFactors = F)
general_data<- read.csv("general_data.csv", stringsAsFactors = F)
in_time<- read.csv("in_time.csv", stringsAsFactors = F)
mngr_survey_data<- read.csv("manager_survey_data.csv", stringsAsFactors = F)
out_time<- read.csv("out_time.csv", stringsAsFactors = F)

##Data Cleaning and Preparation

#Checking for NA values in the individual fields of the manager survey data

sapply(mngr_survey_data, function(a) length(which(is.na(a))))
#No NA values found

#Checking for NA values in the individual fields of the general data dataset

sapply(emp_survey_data, function(a)length(which(is.na(a))))

#Checking for NA values in the individual fields of the general data dataset

sapply(general_data, function(a) length(which(is.na(a))))
#There are 19 NA values in the Number of companies worked and 9 in the Total Working Years.
#For the Number of companies worked variable, if the difference between the total working years
#and number of years working in the present company is 1, then Number of companies worked would 
#be 2. If the total working years is equal to number of years at the company, that would mean
#Number of companies would be 1. Hence, calculating the difference and replacing the respective NA values.

general_data$workyears_diff<- general_data$TotalWorkingYears - general_data$YearsAtCompany

general_data$NumCompaniesWorked[(which(is.na(general_data$NumCompaniesWorked)))] <- ifelse(general_data$workyears_diff[(which(is.na(general_data$NumCompaniesWorked)))] == 1, 2, 
         ifelse(general_data$workyears_diff[(which(is.na(general_data$NumCompaniesWorked)))] == 0, 1, NA))

#Removing the column workyears_diff as it is no longer required
general_data$workyears_diff <- NULL

#For Total working years, if number of companies worked is 0 or 1, NA can be replaced by the number of years at company.

general_data$TotalWorkingYears[(which(is.na(general_data$TotalWorkingYears)))] <- ifelse(general_data$NumCompaniesWorked[(which(is.na(general_data$TotalWorkingYears)))] == 0,
general_data$YearsAtCompany[(which(is.na(general_data$TotalWorkingYears)))], ifelse(general_data$NumCompaniesWorked[(which(is.na(general_data$TotalWorkingYears)))] == 1,
general_data$YearsAtCompany[(which(is.na(general_data$TotalWorkingYears)))], NA))

####### Examining  duplicates  #######
####### No duplicates found #######

which(duplicated(general_data$EmployeeID))
which(duplicated(emp_survey_data))
which(duplicated(mngr_survey_data))
which(duplicated(in_time))
which(duplicated(out_time))
####### No duplicates found #######

View(general_data)

#The column names of employee id in the in_time and out_time are missing 
colnames(in_time)[1]  <- "EmployeeID"
colnames(out_time)[1] <- "EmployeeID"

#Converting wide to long format
in_time<- gather(in_time, in_date, in_date_time, X01.01.2015:X31.12.2015)
out_time<- gather(out_time, out_date, out_date_time, X01.01.2015:X31.12.2015)

#Checking for NA values in the in_time and out_time datasets
sum(is.na(in_time))
sum(is.na(out_time))

#Both have same number of NA values.
in_time<-na.omit(in_time)
out_time<-na.omit(out_time)

#Combining in and out data into a single dataset
inout_time<- data.frame(cbind(in_time$EmployeeID, in_time$in_date_time, out_time$out_date_time))
View(inout_time)
library(lubridate)
#Converting the in and out time date and time format
inout_time$X2<- parse_date_time(inout_time$X2, c("dmY_HM"), tz="")
inout_time$X3 <- parse_date_time(inout_time$X3, c("dmY_HM"), tz="")

#Calculating the work hours of employees
inout_time$work_hrs<- difftime(inout_time$X3, inout_time$X2, units = c("hours"))

#Calculating average working hours of each employee
avg_work_hrs <- aggregate(inout_time$work_hrs, by= list(inout_time$X1),mean, na.rm=TRUE)
colnames(avg_work_hrs)[1]<- "EmployeeID"
colnames(avg_work_hrs)[2]<- "Average_workhrs"

##Data merge

# Collate the data together in one single file
length(unique(tolower(general_data$EmployeeID)))    
length(unique(tolower(emp_survey_data$EmployeeID))) 
length(unique(tolower(mngr_survey_data$EmployeeID)))
length(unique(tolower(avg_work_hrs$EmployeeID)))
#All give 4410 as the result, confirming EmployeeID is key 

#Checking for difference in the EmployeeID field 
         
setdiff(general_data$EmployeeID, emp_survey_data$EmployeeID)  
setdiff(general_data$EmployeeID, mngr_survey_data$EmployeeID)
setdiff(general_data$EmployeeID, avg_work_hrs$EmployeeID)
#Since all the employee ids are identical in all dataframes, merging can be done

master_file <- merge(general_data , emp_survey_data, by = "EmployeeID")
master_file <- merge(master_file, mngr_survey_data, by = "EmployeeID")
master_file <- merge(master_file, avg_work_hrs, by = "EmployeeID")
str(master_file)

master_file$Average_workhrs<- as.numeric(master_file$Average_workhrs)

sum(is.na(master_file))
#97 NA values which account for only 2% of the data, so can be omitted
master_file<- na.omit(master_file)

### Barcharts for categorical features with stacked master_file information
bar_theme1<- theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
                   legend.position="none")
## variable analysis

theme_set(theme_cowplot(font_size=10)) # reduce default font size

plot_grid(ggplot(master_file, aes(x=Department,fill=Attrition))+ geom_bar() ,
          ggplot(master_file, aes(x=EducationField,fill=Attrition))+ geom_bar()+bar_theme1,
          align = "v",labels = "AUTO", ncol = 1,axis = 'l')
plot_grid(ggplot(master_file, aes(x=BusinessTravel,fill=Attrition))+ geom_bar(),
          ggplot(master_file, aes(x=Gender,fill=Attrition))+ geom_bar()+bar_theme1,
          align = "v",labels = "AUTO", ncol = 1,axis = 'l')

plot_grid(ggplot(master_file, aes(x=MaritalStatus,fill=Attrition))+ geom_bar(),
          ggplot(master_file, aes(x=JobLevel,fill=Attrition))+ geom_bar()+bar_theme1,
          align = "v",labels = "AUTO", ncol = 1,axis = 'l')
          plot_grid(ggplot(master_file, aes(x=JobRole,fill=Attrition))+ geom_bar()+bar_theme1,
          align = "v",labels = "AUTO", ncol = 1,axis = 'l')

##Conclusions from the plots
#1. Employees who rarely travel tend to change company more.
#2. Employees in the Research & Development have higher tendency to change company.
#3. Employees with educational nackground of Life Sciences have higher attrition rate.
#4. Male employees have higher tendency of switching company.
#5. Employees of Job level 1 & 2 have equal to eachother and higher than other levels, of attrition rate.
#6. Married people show higher attrition rate.

# Histogram and Boxplots for numeric variables 
box_theme<- theme(axis.line=element_blank(),axis.title=element_blank(), 
                  axis.ticks=element_blank(), axis.text=element_blank())

box_theme_y<- theme(axis.line.y=element_blank(),axis.title.y=element_blank(), 
                    axis.ticks.y=element_blank(), axis.text.y=element_blank(),
                    legend.position="none")

plot_grid(ggplot(master_file, aes(YearsWithCurrManager))+ geom_histogram(binwidth = 10),
          ggplot(master_file, aes(x="",y=YearsWithCurrManager))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)


plot_grid(ggplot(master_file, aes(DistanceFromHome))+ geom_histogram(binwidth = 10),
          ggplot(master_file, aes(x="",y=DistanceFromHome))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

plot_grid(ggplot(master_file, aes(PercentSalaryHike))+ geom_histogram(binwidth = 10),
          ggplot(master_file, aes(x="",y=PercentSalaryHike))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

plot_grid(ggplot(master_file, aes(YearsSinceLastPromotion))+ geom_histogram(binwidth = 20),
          ggplot(master_file, aes(x="",y=YearsSinceLastPromotion))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

plot_grid(ggplot(master_file, aes(MonthlyIncome))+ geom_histogram(binwidth = 1000),
        ggplot(master_file, aes(x="",y=MonthlyIncome))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

plot_grid(ggplot(master_file, aes(YearsAtCompany))+ geom_histogram(binwidth = 5),
          ggplot(master_file, aes(x="",y=YearsAtCompany))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)


#Outliers found in three variables YearsSinceLastPromotion, MonthlyIncome and YearsAtCompany.
# Treating them by replacing with the median.

quantile(master_file$YearsSinceLastPromotion,probs = c(.1,.2,.3,.4,.5,.6,.7,.8,.9))
summary(master_file$YearsSinceLastPromotion)
master_file$YearsSinceLastPromotion[which(master_file$YearsSinceLastPromotion > 4)] <- 1

quantile(master_file$MonthlyIncome,probs = c(.1,.2,.3,.4,.5,.6,.7,.8,.9))
summary(master_file$MonthlyIncome)
master_file$MonthlyIncome[which(master_file$MonthlyIncome > 65029)] <- 49360

quantile(master_file$YearsAtCompany,probs = c(.1,.2,.3,.4,.5,.6,.7,.8,.9))
summary(master_file$YearsAtCompany)
master_file$YearsAtCompany[which(master_file$YearsAtCompany > 8)] <- 5

# Correlation between numeric variables
library(GGally)
ggpairs(master_file[, c("Age","MonthlyIncome","PercentSalaryHike")])

View(master_file)
#Variables like Over18, EmployeeCount, StandardHours have identical values in their respective columns, hence
#serve no purpose in analysis. Also, Employee Id can is no more required. Therefore, removing these columns.
master_file<- master_file[,-c(1,9,16,18)]

#Converting target variable Attrition from No/Yes character to factorwith levels 0/1 
master_file$Attrition<- ifelse(master_file$Attrition=="Yes",1,0)

# Checking attrition rate of the employees

Attrition_rate <- sum(master_file$Attrition)/nrow(master_file) 
Attrition_rate
#16.16% attrition rate. 

# Creating a dataframe of categorical features
masterfile_chr<- master_file[,c(3,4,7,8,10,11)]

# converting categorical attributes to factor
masterfile_chr<- data.frame(sapply(masterfile_chr, function(x) factor(x)))
str(masterfile_chr)

# Creating a dataframe of continuous features
masterfile_num<- master_file[,-c(2,3,4,7,8,10,11)]

# Normalising continuous features
str(masterfile_num)
masterfile_num_scale<- masterfile_num [, c(1:19)]
masterfile_num_scale<- data.frame(sapply(masterfile_num_scale, function(x) scale(x)))
Attrition<-  data.frame(Attrition = master_file$Attrition)

# creating dummy variables for factor attributes
dummies<- data.frame(sapply(masterfile_chr, function(x) data.frame(model.matrix(~x-1,data = masterfile_chr))))
View(dummies)

# Final dataset
library(caTools)

masterfile_final<- cbind(Attrition,masterfile_num_scale,dummies) 
View(masterfile_final)

#Splitting the data between train and test
set.seed(100)

indices = sample.split(masterfile_final$Attrition, SplitRatio = 0.7)

train = masterfile_final[indices,]

test = masterfile_final[!(indices),]

## Logistic Regression: 

#Initial model
model_1 = glm(Attrition ~ ., data = train, family = "binomial")
summary(model_1)

# Stepwise selection
library("MASS")
model_2<- stepAIC(model_1, direction="both")

summary(model_2)
vif(model_2)

#Excluding Education because it has high p-value and high vif

model_3<- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                StockOptionLevel + TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction + 
                JobSatisfaction + WorkLifeBalance + Average_workhrs + BusinessTravel.xNon.Travel + 
                BusinessTravel.xTravel_Frequently + Department.xHuman.Resources + 
                JobRole.xManufacturing.Director + JobRole.xLaboratory.Technician +
                JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                JobRole.xSales.Executive + MaritalStatus.xDivorced + MaritalStatus.xMarried, 
              family = "binomial", data = train)

summary(model_3)
vif(model_3)

#Excluding YearsSinceLastPromotion  because it has high p-value and high vif 

model_4<- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                StockOptionLevel + TotalWorkingYears + TrainingTimesLastYear + 
               YearsWithCurrManager + EnvironmentSatisfaction + 
                JobSatisfaction + WorkLifeBalance + Average_workhrs + BusinessTravel.xNon.Travel + 
                BusinessTravel.xTravel_Frequently + Department.xHuman.Resources + 
                JobRole.xManufacturing.Director + JobRole.xLaboratory.Technician +
                JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                JobRole.xSales.Executive + MaritalStatus.xDivorced + MaritalStatus.xMarried, 
              family = "binomial", data = train)

summary(model_4)
vif(model_4)

#Excluding JobRole.xManufacturing.Director because it has high p-value and high vif 

model_5<- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                StockOptionLevel + TotalWorkingYears + TrainingTimesLastYear + 
                YearsWithCurrManager + EnvironmentSatisfaction + 
                JobSatisfaction + WorkLifeBalance + Average_workhrs + BusinessTravel.xNon.Travel + 
                BusinessTravel.xTravel_Frequently + Department.xHuman.Resources + 
               JobRole.xLaboratory.Technician +
                JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                JobRole.xSales.Executive + MaritalStatus.xDivorced + MaritalStatus.xMarried, 
              family = "binomial", data = train)


summary(model_5)
vif(model_5)

#Excluding JobRole.xLaboratory.Technician because it has high p-value and high vif 

model_6<- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                StockOptionLevel + TotalWorkingYears + TrainingTimesLastYear + 
                YearsWithCurrManager + EnvironmentSatisfaction + 
                JobSatisfaction + WorkLifeBalance + Average_workhrs + BusinessTravel.xNon.Travel + 
                BusinessTravel.xTravel_Frequently + Department.xHuman.Resources + 
                JobRole.xLaboratory.Technician +
                JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                JobRole.xSales.Executive + MaritalStatus.xDivorced + MaritalStatus.xMarried, 
              family = "binomial", data = train)

summary(model_6)
vif(model_6)

#Excluding StockOptionLevel because it has high p-value and high vif 

model_7<- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
               TotalWorkingYears + TrainingTimesLastYear + 
                YearsWithCurrManager + EnvironmentSatisfaction + 
                JobSatisfaction + WorkLifeBalance + Average_workhrs + BusinessTravel.xNon.Travel + 
                BusinessTravel.xTravel_Frequently + Department.xHuman.Resources + 
                JobRole.xLaboratory.Technician +
                JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                JobRole.xSales.Executive + MaritalStatus.xDivorced + MaritalStatus.xMarried, 
              family = "binomial", data = train)

summary(model_7)
vif(model_7)

#Excluding JobRole.xResearch.Scientist because it has high p-value and high vif 

model_8<- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                TotalWorkingYears + TrainingTimesLastYear + 
                YearsWithCurrManager + EnvironmentSatisfaction + 
                JobSatisfaction + WorkLifeBalance + Average_workhrs + BusinessTravel.xNon.Travel + 
                BusinessTravel.xTravel_Frequently + Department.xHuman.Resources + 
                JobRole.xLaboratory.Technician +
                JobRole.xResearch.Director +
                JobRole.xSales.Executive + MaritalStatus.xDivorced + MaritalStatus.xMarried, 
              family = "binomial", data = train)


summary(model_8)
vif(model_8)

#Excluding JobRole.xLaboratory.Technician  because it has high p-value and high vif 

model_9<-  glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                 TotalWorkingYears + TrainingTimesLastYear + 
                 YearsWithCurrManager + EnvironmentSatisfaction + 
                 JobSatisfaction + WorkLifeBalance + Average_workhrs + BusinessTravel.xNon.Travel + 
                 BusinessTravel.xTravel_Frequently + Department.xHuman.Resources + 
                JobRole.xResearch.Director +
                 JobRole.xSales.Executive + MaritalStatus.xDivorced + MaritalStatus.xMarried, 
               family = "binomial", data = train)

summary(model_9)
vif(model_9)



#Excluding JobRole.xSales.Executive  because it has high p-value and high vif 

model_10<-  glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                 TotalWorkingYears + TrainingTimesLastYear + 
                 YearsWithCurrManager + EnvironmentSatisfaction + 
                 JobSatisfaction + WorkLifeBalance + Average_workhrs + BusinessTravel.xNon.Travel + 
                 BusinessTravel.xTravel_Frequently + Department.xHuman.Resources + 
                 JobRole.xResearch.Director +
                  MaritalStatus.xDivorced + MaritalStatus.xMarried, 
               family = "binomial", data = train)

summary(model_10)
vif(model_10)

#Excluding JobRole.xResearch.Director  because it has high p-value and high vif 

model_11<-  glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                  TotalWorkingYears + TrainingTimesLastYear + 
                  YearsWithCurrManager + EnvironmentSatisfaction + 
                  JobSatisfaction + WorkLifeBalance + Average_workhrs + BusinessTravel.xNon.Travel + 
                  BusinessTravel.xTravel_Frequently + Department.xHuman.Resources + 
                  MaritalStatus.xDivorced + MaritalStatus.xMarried, 
                family = "binomial", data = train)

summary(model_11)
vif(model_11)

#Excluding BusinessTravel.xNon.Travel  because it has high p-value and high vif 

model_12<-  glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                  TotalWorkingYears + TrainingTimesLastYear + 
                  YearsWithCurrManager + EnvironmentSatisfaction + 
                  JobSatisfaction + WorkLifeBalance + Average_workhrs +  
                  BusinessTravel.xTravel_Frequently + Department.xHuman.Resources + 
                  MaritalStatus.xDivorced + MaritalStatus.xMarried, 
                family = "binomial", data = train)

summary(model_12)
vif(model_12)

#Excluding TrainingTimesLastYear  because it has high p-value and high vif 

model_13<-  glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                  TotalWorkingYears +  
                  YearsWithCurrManager + EnvironmentSatisfaction + 
                  JobSatisfaction + WorkLifeBalance + Average_workhrs +  
                  BusinessTravel.xTravel_Frequently + Department.xHuman.Resources + 
                  MaritalStatus.xDivorced + MaritalStatus.xMarried, 
                family = "binomial", data = train)

summary(model_13)
vif(model_13)


#12 variables are left and they all show high significance.
final_model<- model_13

## Model Evaluation

## Test Data 

#Predicted probabilities of Attrition for test data

test_pred = predict(final_model, type = "response", 
                    newdata = test[,-1])

summary(test_pred)

test$prob <- test_pred
View(test)

#Checking correlation of the variables from final model with attrition.

##CONCLUSION

#The factors that lead to the attrition of Employees are the following:
#Age
cor(masterfile_final$Attrition,masterfile_final$Age)     
#Age has a negative correlation with the attrition attribute, which means as people tend to get older they tend to
#stay in the company, i.e. younger people have higher chancer of attrition

#NumCompaniesWorked 
cor(masterfile_final$Attrition,masterfile_final$NumCompaniesWorked)
# There is a positive yet low correlation with Number of companies worked.

#TotalWorkingYears 
cor(masterfile_final$Attrition,masterfile_final$TotalWorkingYears)
#If total working years is less, chances of attrition is higher

#YearsWithCurrManager  
cor(masterfile_final$Attrition,masterfile_final$YearsWithCurrManager)
#Years with current manager being less, chances of attrition is higher

#EnvironmentSatisfaction
cor(masterfile_final$Attrition,masterfile_final$EnvironmentSatisfaction)
#If environment satisfaction among employees is less, chances of attrition is higher

#JobSatisfaction
cor(masterfile_final$Attrition,masterfile_final$JobSatisfaction)
#If Job satisfaction among employees is less, chances of attrition is higher

#WorkLifeBalance
cor(masterfile_final$Attrition,masterfile_final$WorkLifeBalance)
#If Work-life balance is less, chances of attrition is higher

#Average_workhrs
cor(masterfile_final$Attrition,masterfile_final$Average_workhrs)
#If average work hours are more, chances of attrition is higher

#BusinessTravel.xTravel_Frequently
cor(masterfile_final$Attrition,masterfile_final$BusinessTravel.xTravel_Frequently)
#Employees who frequently travel for business tend to have less attrition level

#Department.xHuman.Resources 
cor(masterfile_final$Attrition,masterfile_final$Department.xHuman.Resources)
#Employees from human resources department show lesser attrition rate

#MaritalStatus.xDivorced        
cor(masterfile_final$Attrition,masterfile_final$MaritalStatus.xDivorced)
#Divorced people show less attrition rate

#MaritalStatus.xMarried 
cor(masterfile_final$Attrition,masterfile_final$MaritalStatus.xMarried)
#Married employees have less attrition rate.


# Using probability cutoff of 50%.

test_pred_attrition<- factor(ifelse(test_pred >= 0.50, "Yes", "No"))
test_actual_attrition <- factor(ifelse(test$Attrition==1,"Yes","No"))

table(test_actual_attrition,test_pred_attrition)

test_pred_attrition <- factor(ifelse(test_pred >= 0.40, "Yes", "No"))

test_conf <- confusionMatrix(test_pred_attrition, test_actual_attrition, positive = "Yes")
test_conf

#Choosing the cutoff value. 

# finding out the optimal probalility cutoff
perform_fn <- function(cutoff) 
{
  predicted_attrition <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_attrition, test_actual_attrition, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

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

#Choosing a cutoff value of 0.3132 for final model

library(caret)
test_cutoff_attrition<- factor(ifelse(test_pred >=0.3132, "Yes", "No"))

conf_final <- confusionMatrix(test_cutoff_attrition, test_actual_attrition, positive = "Yes")

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc

sens

spec

View(test)
#Accuracy of 85%
#Sensitivity of 52%
#Specificity of 92%

##KS-statistic - Test Data

test_cutoff_attrition<- ifelse(test_cutoff_attrition=="Yes",1,0)
test_actual_attrition <- ifelse(test_actual_attrition=="Yes",1,0)


library(ROCR)
#Testing  data
pred_object_test<- prediction(test_cutoff_attrition, test_actual_attrition)

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])
max(ks_table_test)

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

attrition_decile = lift(test_actual_attrition, test_pred, groups = 10)

plot(attrition_decile$Cumlift, type="l", lwd=2, col="blue", xlim = c(0,10), ylim = c(0,4),main = "Lift Chart",xlab = "Decile",
     ylab = "Lift")
abline(h=1, col="red")
axis(1, 1:10)
abline(h=0:10, v=0:10, lty=3)

#Plot Gain Chart
ks_plot(test_actual_attrition, test_cutoff_attrition)
