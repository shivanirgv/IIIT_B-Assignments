#install.packages('ggplot2')
#install.packages('DataExplorer')
#install.packages('Hmisc')
#install.packages('dplyr')
#install.packages("lubridate")
#install.packages("cowplot")
#install.packages("tidyr")
#install.packages("data.table")
#install.packages('DAAG')
#install.packages('lmvar')
#install.packages("klaR")
#install.packages("DataCombine")
#install.packages("data.table")

#load the required libraries
library(ggplot2)
library(DataExplorer)
library(readxl)
library(dplyr)
library(Hmisc)
library(lubridate)
library(tidyr)
library(cowplot)
library(data.table)
library(stringr)
library(chron)
library(corrplot)
library(MASS)
library(car)
library(caret)
library(DAAG)
library(lmvar)
library(DataCombine)
library(data.table)

#####################################################################################
#############################load the data ##########################################
#####################################################################################
#load the data, convert the "\\N" string to NA while loading 

master_ga_final.data <- read.csv('data\\master_ga_final.csv', header = T, stringsAsFactors = F)

######################### Adding Moving Average Variables ############################################################
#Calculating increase in MA variables
master_ga_final_with_ma.data <- master_ga_final.data
master_ga_final_with_ma.data <- cbind(master_ga_final_with_ma.data,calcIncrInMAVariable('discount_per',master_ga_final_with_ma.data))
master_ga_final_with_ma.data <- cbind(master_ga_final_with_ma.data,calcIncrInMAVariable('avg_productmrp',master_ga_final_with_ma.data))

############################# 1. LINEAR MODEL  ################################
# As data points are few, hence cross validation to be used. 
# Entire data frame used for training.
train = master_ga_final_with_ma.data[,-which(names(master_ga_final_with_ma.data) %in% c("Week","tot_productmrp","norders","nunits","repeated_orders","value_per_visitor"))]
train <- as.data.frame(scale(train))

# Build model 1 containing all variables
model_1 <-lm(total_gmv~.,data=train)
summary(model_1) # Multiple R-squared:  0.836,	Adjusted R-squared:  0.6272
#######

train_1 <- model_1$model
model_2 <- lm(total_gmv~.,data=train_1)

# In stepAIC function, lets pass first model i.e model_1 and 
# direction is set as both, to enable both forward selection 
# of variables and backward elimination of variables simultaneously 

step <- stepAIC(model_2, direction="both")
step

# stepAIC does multiple iterations and checks which variables to keep
# Step result contains only the variables it considers to be important in the model. 
# some insignifican variables have been removed. 
# Lets store the last model equation of stepwise method into model_2

model_3 <- lm(formula = total_gmv ~ avg_sla + discount_per + cod_orders + 
                earlyDelivery_orders + onTimeDelivery_orders + Total_Investment + 
                TV + Digital + Sponsorship + Content_Marketing + Online_Marketing + 
                Affiliates + SEM + Other + inc_discount_per_MA3 + inc_avg_productmrp_MA3, 
              data = train_1)

summary(model_3) # Multiple R-squared:  0.815,	Adjusted R-squared:  0.728 
sort(vif(model_3))

# remove Total_investment : very high VIF
model_4 <- lm(formula = total_gmv ~ avg_sla + discount_per + cod_orders + 
                earlyDelivery_orders + onTimeDelivery_orders + 
                TV + Digital + Sponsorship + Content_Marketing + Online_Marketing + 
                Affiliates + SEM + Other + inc_discount_per_MA3 + inc_avg_productmrp_MA3, 
              data = train_1)

summary(model_4) # Multiple R-squared:  0.7737,	Adjusted R-squared:  0.6767
sort(vif(model_4))

# remove Online_marketing : high VIF, high p value
model_5 <- lm(formula = total_gmv ~ avg_sla + discount_per + cod_orders + 
                earlyDelivery_orders + onTimeDelivery_orders + 
                TV + Digital + Sponsorship + Content_Marketing + 
                Affiliates + SEM + Other + inc_discount_per_MA3 + inc_avg_productmrp_MA3, 
              data = train_1)

summary(model_5) # Multiple R-squared:  0.7737,	Adjusted R-squared:  0.6856
sort(vif(model_5))

# remove Digital : high VIF, p value
model_6 <- lm(formula = total_gmv ~ avg_sla + discount_per + cod_orders + 
                earlyDelivery_orders + onTimeDelivery_orders + 
                TV + Sponsorship + Content_Marketing + 
                Affiliates + SEM + Other + inc_discount_per_MA3 + inc_avg_productmrp_MA3, 
              data = train_1)

summary(model_6) # Multiple R-squared:  0.7565,	Adjusted R-squared:  0.6709
sort(vif(model_6))

# remove discount_per : high VIF, p value
model_7 <- lm(formula = total_gmv ~ avg_sla + cod_orders + 
                earlyDelivery_orders + onTimeDelivery_orders + 
                TV + Sponsorship + Content_Marketing + 
                Affiliates + SEM + Other + inc_discount_per_MA3 + inc_avg_productmrp_MA3, 
              data = train_1)

summary(model_7) # Multiple R-squared:  0.9258,	Adjusted R-squared:  0.9072
sort(vif(model_7))

# remove SEM : high VIF value
model_8 <- lm(formula = total_gmv ~ avg_sla + cod_orders + 
                earlyDelivery_orders + onTimeDelivery_orders + 
                TV + Sponsorship + Content_Marketing + 
                Affiliates + Other + inc_discount_per_MA3 + inc_avg_productmrp_MA3, 
              data = train_1)

summary(model_8) # Multiple R-squared:  0.7251,	Adjusted R-squared:  0.6476
sort(vif(model_8))

# remove inc_discount_per_MA3 : high VIF, p value
model_9 <- lm(formula = total_gmv ~ avg_sla + cod_orders + 
                earlyDelivery_orders + onTimeDelivery_orders + 
                TV + Sponsorship + Content_Marketing + 
                Affiliates + Other + inc_avg_productmrp_MA3, 
              data = train_1)

summary(model_9) # Multiple R-squared:  0.7245,	Adjusted R-squared:  0.6556
sort(vif(model_9))

# remove Content marketing : high VIF, p value
model_10 <- lm(formula = total_gmv ~ avg_sla + cod_orders + 
                earlyDelivery_orders + onTimeDelivery_orders + 
                TV + Sponsorship + 
                Affiliates + Other + inc_avg_productmrp_MA3, 
              data = train_1)

summary(model_10) # Multiple R-squared:  0.7177,	Adjusted R-squared:  0.6557
sort(vif(model_10))

# remove TV : high VIF, p value
model_11 <- lm(formula = total_gmv ~ avg_sla + cod_orders + 
                 earlyDelivery_orders + onTimeDelivery_orders + 
                 Sponsorship + 
                 Affiliates + Other + inc_avg_productmrp_MA3, 
               data = train_1)

summary(model_11) # Multiple R-squared:  0.6936,	Adjusted R-squared:  0.6352
sort(vif(model_11))

# remove onTimeDelivery_orders : high p value
model_12 <- lm(formula = total_gmv ~ avg_sla + cod_orders + 
                 earlyDelivery_orders + 
                 Sponsorship + 
                 Affiliates + Other + inc_avg_productmrp_MA3, 
               data = train_1)

summary(model_12) # Multiple R-squared:  0.6906,	Adjusted R-squared:  0.6403
sort(vif(model_12))

# remove Sponsorship : mod VIF, mod p value
model_13 <- lm(formula = total_gmv ~ avg_sla + cod_orders + 
                 earlyDelivery_orders + 
                 Affiliates + Other + inc_avg_productmrp_MA3, 
               data = train_1)

summary(model_13) # Multiple R-squared:  0.67,	Adjusted R-squared:  0.625
sort(vif(model_13))

# remove cod_orders : high p value
model_14 <- lm(formula = total_gmv ~ avg_sla + 
                 earlyDelivery_orders + 
                 Affiliates + Other + inc_avg_productmrp_MA3, 
               data = train_1)

summary(model_14) # Multiple R-squared:  0.6666,	Adjusted R-squared:  0.6296
sort(vif(model_14))

# remove Other : high p value
model_15 <- lm(formula = total_gmv ~ avg_sla + 
                 earlyDelivery_orders + 
                 Affiliates + inc_avg_productmrp_MA3, 
               data = train_1)

summary(model_15) # Multiple R-squared:  0.6608,	Adjusted R-squared:  0.6314
sort(vif(model_15))

# remove earlyDelivery_orders : high p value
model_16 <- lm(formula = total_gmv ~ avg_sla + Affiliates + inc_avg_productmrp_MA3, 
               data = train_1)

summary(model_16) # Multiple R-squared:  0.6443,	Adjusted R-squared:  0.6216
sort(vif(model_16))

final_model <- model_16
# AIC of generated model
AIC(final_model) # 101.06

# Final model
# Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)            -0.02754    0.08700  -0.317 0.752952    
# avg_sla                -0.33117    0.09947  -3.329 0.001699 ** 
#  Affiliates              0.85948    0.09952   8.636 2.91e-11 ***
#  inc_avg_productmrp_MA3  0.31486    0.08741   3.602 0.000759 ***

########## Test accuracy #######
### Cross validation 

################################

train_control <- trainControl(method="cv", number=5)
train <- na.omit(train_1)
# Fit Naive Bayes Model
model <- train(total_gmv ~ avg_sla + Affiliates + inc_avg_productmrp_MA3 
               , data=train, trControl=train_control, method="lm")
# Summarise Results
print(model) # r sq 0.567
# RMSE       Rsquared   MAE      
# 0.6441919  0.5670257  0.4816533

DAAG::cv.lm(data = train, form.lm = formula(total_gmv ~ avg_sla + Affiliates + inc_avg_productmrp_MA3),
m = 5, dots = FALSE, seed = 29, plotit = c("Observed","Residual"),
main="Small symbols show cross-validation predicted values",
legend.pos="topleft", printit = TRUE)
# overall ms 0.393

############################ Elasticity ###############################
elast <- calcElasticity(final_model,train,0)
elast
#variable elasticity direction
#1                avg_sla     -0.539  Negative
#2             Affiliates        2.2  Positive
#3 inc_avg_productmrp_MA3    -0.0453  Negative

##plot the elasticity
plot1 <- plotElasticity(elast,"Game Accessory - Linear Model")

############################# 2. Multiplicative MODEL ################################
# As data points are few, hence cross validation to be used. 
# Entire data frame used for training.
train = master_ga_final.data[,-which(names(master_ga_final.data) %in% c("Week","tot_productmrp","norders","nunits","repeated_orders","value_per_visitor"))]
train <- as.data.frame(sapply(train,function(x) ifelse(x==0,0.0001,x)))
train <- as.data.frame(sapply(train,function(x) log(x)))
train <- as.data.frame(scale(train))
sum(is.na(train))
# Build model 1 containing all variables
model_1 <-lm(total_gmv~.,data=train)
summary(model_1) # adj r2 0.9384, r2 0.9639
#######

# In stepAIC function, lets pass first model i.e model_1 and 
# direction is set as both, to enable both forward selection 
# of variables and backward elimination of variables simultaneously 

step <- stepAIC(model_1, direction="both")
step

# stepAIC does multiple iterations and checks which variables to keep
# Step result contains only the variables it considers to be important in the model. 
# some insignifican variables have been removed. 
# Lets store the last model equation of stepwise method into model_2

model_2 <- lm(formula = total_gmv ~ avg_deliverybdays + avg_deliverycdays + 
                avg_sla + avg_prod_proc_sla + earlyDelivery_orders + onTimeDelivery_orders + 
                Total_Investment + Content_Marketing + Online_Marketing + 
                Affiliates, data = train)

summary(model_2) # adj r2 0.9528
sort(vif(model_2))

# remove Affiliates : high VIF, p value
model_3 <- lm(formula = total_gmv ~ avg_deliverybdays + avg_deliverycdays + 
                avg_sla + avg_prod_proc_sla + earlyDelivery_orders + onTimeDelivery_orders + 
                Total_Investment + Content_Marketing + Online_Marketing  
                , data = train)

summary(model_3) # adj r2 0.9511
sort(vif(model_3))

# remove Total investment: high VIF, mod p value
model_4 <- lm(formula = total_gmv ~ avg_deliverybdays + avg_deliverycdays + 
                avg_sla + avg_prod_proc_sla + earlyDelivery_orders + onTimeDelivery_orders + 
                Content_Marketing + Online_Marketing  
              , data = train)

summary(model_4) # adj r2 0.9477
sort(vif(model_4))

# remove avg_deliverycdays : high p value
model_5 <- lm(formula = total_gmv ~ avg_deliverybdays + 
                avg_sla + avg_prod_proc_sla + earlyDelivery_orders + onTimeDelivery_orders + 
                Content_Marketing + Online_Marketing  
              , data = train)

summary(model_5) # adj r2 0.9465
sort(vif(model_5))

# remove earlyDelivery_orders: high p value
model_6 <- lm(formula = total_gmv ~ avg_deliverybdays + 
                avg_sla + avg_prod_proc_sla + onTimeDelivery_orders + 
                Content_Marketing + Online_Marketing  
              , data = train)

summary(model_6) # adj r2 0.9461
sort(vif(model_6))

# remove avg_sla: high p value
model_7 <- lm(formula = total_gmv ~ avg_deliverybdays + 
                avg_prod_proc_sla + onTimeDelivery_orders + 
                Content_Marketing + Online_Marketing  
              , data = train)

summary(model_7) # adj r2 0.9445
sort(vif(model_7))

# remove avg_deliverybdays: high p value
model_8 <- lm(formula = total_gmv ~ avg_prod_proc_sla + onTimeDelivery_orders + 
                Content_Marketing + Online_Marketing  
              , data = train)

summary(model_8) # adj r2 0.9422
sort(vif(model_8))

# remove content marketing: high VIF value
model_9 <- lm(formula = total_gmv ~ avg_prod_proc_sla + onTimeDelivery_orders + Online_Marketing  
              , data = train)

summary(model_9) # adj r2 0.9128
sort(vif(model_9))

final_model <- model_9

# AIC of generated model
AIC(final_model) # 27.4

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)           -1.090e-17  4.019e-02   0.000   1.0000    
# avg_prod_proc_sla      1.107e-01  4.354e-02   2.543   0.0141 *  
#  onTimeDelivery_orders  7.155e-01  5.304e-02  13.489  < 2e-16 ***
#  Online_Marketing       3.187e-01  5.194e-02   6.136 1.34e-07 ***

########## Test accuracy #######
### Cross validation 

################################

train_control <- trainControl(method="cv", number=5)
train <- na.omit(train)
# Fit Naive Bayes Model
model <- train(total_gmv ~ avg_prod_proc_sla + onTimeDelivery_orders + Online_Marketing 
               , data=train, trControl=train_control, method="lm")
# Summarise Results
print(model) # r sq 0.65
# RMSE   Rsquared   MAE      
# 0.302  0.65      0.237

DAAG::cv.lm(data = train, form.lm = formula(total_gmv ~ avg_prod_proc_sla + onTimeDelivery_orders + Online_Marketing),
            m = 5, dots = FALSE, seed = 29, plotit = c("Observed","Residual"),
            main="Small symbols show cross-validation predicted values",
            legend.pos="topleft", printit = TRUE)
# overall ms 0.0921

############################ Elasticity ###############################
# in case of mult model, beta coefficient is elasticity
elast <- calcElasticity(final_model,master_ga_final.data,1)
elast

#variable elasticity direction
#1     avg_prod_proc_sla      0.111  Positive
#2 onTimeDelivery_orders      0.715  Positive
#3      Online_Marketing      0.319  Positive

##plot the elasticity
plot2 <- plotElasticity(elast,"Game Accessory - Mult Model")

############################# 3. Koyck MODEL ################################
# As data points are few, hence cross validation to be used. 
# Entire data frame used for training.
train = master_ga_final_with_ma.data[,-which(names(master_ga_final_with_ma.data) %in% c("Week","tot_productmrp","norders","nunits","repeated_orders","value_per_visitor"))]
train <- slide(train, Var = "total_gmv", slideBy = -1)
train <- as.data.frame(scale(train))
sum(is.na(train))
# Build model 1 containing all variables
model_1 <-lm(total_gmv~.,data=train)
summary(model_1) # adj r2 0.657, r2 0.856
#######

# In stepAIC function, lets pass first model i.e model_1 and 
# direction is set as both, to enable both forward selection 
# of variables and backward elimination of variables simultaneously 

step <- stepAIC(model_1, direction="both")
step

# stepAIC does multiple iterations and checks which variables to keep
# Step result contains only the variables it considers to be important in the model. 
# some insignifican variables have been removed. 
# Lets store the last model equation of stepwise method into model_2

model_2 <- lm(formula = total_gmv ~ avg_sla + avg_prod_proc_sla + discount_per + 
                cod_orders + Total_Investment + TV + Digital + Sponsorship + 
                Content_Marketing + Online_Marketing + Affiliates + SEM + 
                Other + inc_discount_per_MA2 + inc_discount_per_MA4 + inc_avg_productmrp_MA2 + 
                `total_gmv-1`, data = train)

summary(model_2) # adj r2 0.745
sort(vif(model_2))

# remove online_marketing high VIF value
model_3 <- lm(formula = total_gmv ~ avg_sla + avg_prod_proc_sla + discount_per + 
                cod_orders + Total_Investment + TV + Digital + Sponsorship + 
                Content_Marketing + Affiliates + SEM + 
                Other + inc_discount_per_MA2 + inc_discount_per_MA4 + inc_avg_productmrp_MA2 + 
                `total_gmv-1`, data = train)

summary(model_3) # adj r2 0.69
sort(vif(model_3))

# remove Total_investment, high VIF p value
model_4 <- lm(formula = total_gmv ~ avg_sla + avg_prod_proc_sla + discount_per + 
                cod_orders + TV + Digital + Sponsorship + 
                Content_Marketing + Affiliates + SEM + 
                Other + inc_discount_per_MA2 + inc_discount_per_MA4 + inc_avg_productmrp_MA2 + 
                `total_gmv-1`, data = train)

summary(model_4) # adj r2 0.686
sort(vif(model_4))

# remove SEM, high VIF p value
model_5 <- lm(formula = total_gmv ~ avg_sla + avg_prod_proc_sla + discount_per + 
                cod_orders + TV + Digital + Sponsorship + 
                Content_Marketing + Affiliates + 
                Other + inc_discount_per_MA2 + inc_discount_per_MA4 + inc_avg_productmrp_MA2 + 
                `total_gmv-1`, data = train)

summary(model_5) # adj r2 0.689
sort(vif(model_5))

# remove inc_discount_per_MA4, high VIF value
model_6 <- lm(formula = total_gmv ~ avg_sla + avg_prod_proc_sla + discount_per + 
                cod_orders + TV + Digital + Sponsorship + 
                Content_Marketing + Affiliates + 
                Other + inc_discount_per_MA2 + inc_avg_productmrp_MA2 + 
                `total_gmv-1`, data = train)

summary(model_6) # adj r2 0.597
sort(vif(model_6))

# remove inc_discount_per_MA2, high p value
model_7 <- lm(formula = total_gmv ~ avg_sla + avg_prod_proc_sla + discount_per + 
                cod_orders + TV + Digital + Sponsorship + 
                Content_Marketing + Affiliates + 
                Other + inc_avg_productmrp_MA2 + 
                `total_gmv-1`, data = train)

summary(model_7) # adj r2 0.603
sort(vif(model_7))

# remove total_gmv-1, high p value
model_8 <- lm(formula = total_gmv ~ avg_sla + avg_prod_proc_sla + discount_per + 
                cod_orders + TV + Digital + Sponsorship + 
                Content_Marketing + Affiliates + 
                Other + inc_avg_productmrp_MA2 
                , data = train)

summary(model_8) # adj r2 0.609
sort(vif(model_8))

# remove discount_per, high p value
model_9 <- lm(formula = total_gmv ~ avg_sla + avg_prod_proc_sla + 
                cod_orders + TV + Digital + Sponsorship + 
                Content_Marketing + Affiliates + 
                Other + inc_avg_productmrp_MA2 
              , data = train)

summary(model_9) # adj r2 0.609
sort(vif(model_9))

# remove Affiliates, high VIF value
model_10 <- lm(formula = total_gmv ~ avg_sla + avg_prod_proc_sla + 
                 cod_orders + TV + Digital + Sponsorship + 
                 Content_Marketing + 
                 Other + inc_avg_productmrp_MA2 
               , data = train)

summary(model_10) # adj r2 0.398
sort(vif(model_10))

# remove Other, high p value
model_11 <- lm(formula = total_gmv ~ avg_sla + avg_prod_proc_sla + 
                 cod_orders + TV + Digital + Sponsorship + 
                 Content_Marketing + 
                 inc_avg_productmrp_MA2 
               , data = train)

summary(model_11) # adj r2 0.41
sort(vif(model_11))

# remove Sponsorship, high VIF p value
model_12 <- lm(formula = total_gmv ~ avg_sla + avg_prod_proc_sla + 
                 cod_orders + TV + Digital + 
                 Content_Marketing + 
                 inc_avg_productmrp_MA2 
               , data = train)

summary(model_12) # adj r2 0.419
sort(vif(model_12))

# remove cod_orders high p value
model_13 <- lm(formula = total_gmv ~ avg_sla + avg_prod_proc_sla + 
                 TV + Digital + 
                 Content_Marketing + 
                 inc_avg_productmrp_MA2 
               , data = train)

summary(model_13) # adj r2 0.427
sort(vif(model_13))

# remove avg_prod_proc_sla, high p value
model_14 <- lm(formula = total_gmv ~ avg_sla + TV + Digital + Content_Marketing + inc_avg_productmrp_MA2 
               , data = train)

summary(model_14) # adj r2 0.432
sort(vif(model_14))

# remove inc_avg_productmrp_MA2, high p value
model_15 <- lm(formula = total_gmv ~ avg_sla + TV + Digital + Content_Marketing  
               , data = train)

summary(model_15) # adj r2 0.42
sort(vif(model_15))

# remove avg_sla, high p value
model_16 <- lm(formula = total_gmv ~ TV + Digital + Content_Marketing  
               , data = train)

summary(model_16) # adj r2 0.42
sort(vif(model_16))

final_model <- model_16
# AIC of generated model
AIC(final_model) #134

# Final model
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)       -1.56e-16   1.08e-01    0.00   1.0000 
# TV                 3.88e-01   1.24e-01    3.12   0.0030 **
#  Digital           -7.85e-01   2.55e-01   -3.08   0.0034 **
#  Content_Marketing  8.84e-01   2.69e-01    3.28   0.0019 **
########## Test accuracy #######

### Cross validation 

################################

train_control <- trainControl(method="cv", number=5)
train <- na.omit(train)
# Fit Naive Bayes Model
model <- train(total_gmv ~ TV + Digital + Content_Marketing 
               , data=train, trControl=train_control, method="lm")
# Summarise Results
print(model) # r sq 0.33
# RMSE   Rsquared   MAE      
# 0.808  0.33      0.599

DAAG::cv.lm(data = train, form.lm = formula(total_gmv ~ TV + Digital + Content_Marketing),
            m = 5, dots = FALSE, seed = 29, plotit = c("Observed","Residual"),
            main="Small symbols show cross-validation predicted values",
            legend.pos="topleft", printit = TRUE)
#Sum of squares = 1.87    Mean square = 0.19    n = 10
# overall ms 0.713

############################ Elasticity ###############################
elast <- calcElasticity(final_model,train,0)
elast
# variable elasticity direction
#1                TV      0.800  Positive
#2           Digital     -0.427  Negative
#3 Content_Marketing      1.131  Positive

##plot the elasticity
plot3 <-plotElasticity(elast,"Game Accessory - Koyck Model")
plot3

############################# 4. Distributive MODEL ################################
# As data points are few, hence cross validation to be used. 
# Entire data frame used for training.
train = master_ga_final_with_ma.data[,-which(names(master_ga_final_with_ma.data) %in% c("Week","tot_productmrp","norders","nunits","repeated_orders","value_per_visitor"))]
train <- slide(train, Var = "total_gmv", slideBy = -1,NewVar = "total_gmv_1")
train <- slide(train, Var = "total_gmv", slideBy = -2,NewVar = "total_gmv_2")
train <- slide(train, Var = "total_gmv", slideBy = -3,NewVar = "total_gmv_3")

#tagging the lag for avg productmrp and discount_per as well
train <- slide(train, Var = "avg_productmrp", slideBy = -1,NewVar = "avg_productmrp_1")
train <- slide(train, Var = "avg_productmrp", slideBy = -2,NewVar = "avg_productmrp_2")
train <- slide(train, Var = "avg_productmrp", slideBy = -3,NewVar = "avg_productmrp_3")

train <- slide(train, Var = "discount_per", slideBy = -1,NewVar = "discount_per_1")
train <- slide(train, Var = "discount_per", slideBy = -2,NewVar = "discount_per_2")
train <- slide(train, Var = "discount_per", slideBy = -3,NewVar = "discount_per_3")

train <- as.data.frame(scale(train))

sum(is.na(train))

# Build model 1 containing all variables
model_1 <-lm(total_gmv~.,data=train)
summary(model_1) # adj r2 0.6975, r2 0.9213
#######

# In stepAIC function, lets pass first model i.e model_1 and 
# direction is set as both, to enable both forward selection 
# of variables and backward elimination of variables simultaneously 

step <- stepAIC(model_1, direction="both")
step

# stepAIC does multiple iterations and checks which variables to keep
# Step result contains only the variables it considers to be important in the model. 
# some insignifican variables have been removed. 
# Lets store the last model equation of stepwise method into model_2

model_2 <- lm(formula = total_gmv ~ avg_deliverybdays + avg_sla + Total_Investment + 
                TV + Digital + Sponsorship + Content_Marketing + Online_Marketing + 
                Affiliates + SEM + Other + NPS + npromotion_in_week + inc_discount_per_MA3 + 
                inc_discount_per_MA4 + inc_avg_productmrp_MA2 + inc_avg_productmrp_MA3 + 
                inc_avg_productmrp_MA4 + total_gmv_1 + total_gmv_2 + avg_productmrp_1 + 
                avg_productmrp_2 + avg_productmrp_3 + discount_per_3, data = train)

summary(model_2) # adj r2 0.8334
sort(vif(model_2))

# remove inc_avg_productmrp_MA2; high VIF
model_3 <- lm(formula = total_gmv ~ avg_deliverybdays + avg_sla + Total_Investment + 
                TV + Digital + Sponsorship + Content_Marketing + Online_Marketing + 
                Affiliates + SEM + Other + NPS + npromotion_in_week + inc_discount_per_MA3 + 
                inc_discount_per_MA4 + inc_avg_productmrp_MA3 + 
                inc_avg_productmrp_MA4 + total_gmv_1 + total_gmv_2 + avg_productmrp_1 + 
                avg_productmrp_2 + avg_productmrp_3 + discount_per_3, data = train)

summary(model_3) # adj r2 0.833
sort(vif(model_3))

# remove Content Marketing; high VIF value
model_4 <- lm(formula = total_gmv ~ avg_deliverybdays + avg_sla + Total_Investment + 
                TV + Digital + Sponsorship + Online_Marketing + 
                Affiliates + SEM + Other + NPS + npromotion_in_week + inc_discount_per_MA3 + 
                inc_discount_per_MA4 + inc_avg_productmrp_MA3 + 
                inc_avg_productmrp_MA4 + total_gmv_1 + total_gmv_2 + avg_productmrp_1 + 
                avg_productmrp_2 + avg_productmrp_3 + discount_per_3, data = train)

summary(model_4) # adj r2 0.7363
sort(vif(model_4))

# remove Total Investment; high VIF, p value
model_5 <- lm(formula = total_gmv ~ avg_deliverybdays + avg_sla + 
                TV + Digital + Sponsorship + Online_Marketing + 
                Affiliates + SEM + Other + NPS + npromotion_in_week + inc_discount_per_MA3 + 
                inc_discount_per_MA4 + inc_avg_productmrp_MA3 + 
                inc_avg_productmrp_MA4 + total_gmv_1 + total_gmv_2 + avg_productmrp_1 + 
                avg_productmrp_2 + avg_productmrp_3 + discount_per_3, data = train)

summary(model_5) # adj r2 0.7255
sort(vif(model_5))

# remove Affiliates, high p VIF value
model_6 <- lm(formula = total_gmv ~ avg_deliverybdays + avg_sla + 
                TV + Digital + Sponsorship + Online_Marketing + 
                SEM + Other + NPS + npromotion_in_week + inc_discount_per_MA3 + 
                inc_discount_per_MA4 + inc_avg_productmrp_MA3 + 
                inc_avg_productmrp_MA4 + total_gmv_1 + total_gmv_2 + avg_productmrp_1 + 
                avg_productmrp_2 + avg_productmrp_3 + discount_per_3, data = train)

summary(model_6) # adj r2 0.7242
sort(vif(model_6))

# remove Online Marketing high p, VIF value
model_7 <- lm(formula = total_gmv ~ avg_deliverybdays + avg_sla + 
                TV + Digital + Sponsorship + 
                SEM + Other + NPS + npromotion_in_week + inc_discount_per_MA3 + 
                inc_discount_per_MA4 + inc_avg_productmrp_MA3 + 
                inc_avg_productmrp_MA4 + total_gmv_1 + total_gmv_2 + avg_productmrp_1 + 
                avg_productmrp_2 + avg_productmrp_3 + discount_per_3, data = train)

summary(model_7) # adj r2 0.7323
sort(vif(model_7))

# remove TV: high p, VIF value
model_8 <- lm(formula = total_gmv ~ avg_deliverybdays + avg_sla + 
                Digital + Sponsorship + 
                SEM + Other + NPS + npromotion_in_week + inc_discount_per_MA3 + 
                inc_discount_per_MA4 + inc_avg_productmrp_MA3 + 
                inc_avg_productmrp_MA4 + total_gmv_1 + total_gmv_2 + avg_productmrp_1 + 
                avg_productmrp_2 + avg_productmrp_3 + discount_per_3, data = train)

summary(model_8) # adj r2 0.7276
sort(vif(model_8))

# remove avg_deliverybdays high p, VIF value
model_9 <- lm(formula = total_gmv ~ avg_sla + 
                Digital + Sponsorship + 
                SEM + Other + NPS + npromotion_in_week + inc_discount_per_MA3 + 
                inc_discount_per_MA4 + inc_avg_productmrp_MA3 + 
                inc_avg_productmrp_MA4 + total_gmv_1 + total_gmv_2 + avg_productmrp_1 + 
                avg_productmrp_2 + avg_productmrp_3 + discount_per_3, data = train)

summary(model_9) # adj r2 0.7176
sort(vif(model_9))

# remove total_gmv-2 high p value
model_10 <- lm(formula = total_gmv ~ avg_sla + 
                 Digital + Sponsorship + 
                 SEM + Other + NPS + npromotion_in_week + inc_discount_per_MA3 + 
                 inc_discount_per_MA4 + inc_avg_productmrp_MA3 + 
                 inc_avg_productmrp_MA4 + total_gmv_1 + avg_productmrp_1 + 
                 avg_productmrp_2 + avg_productmrp_3 + discount_per_3, data = train)

summary(model_10) # adj r2 0.714
sort(vif(model_10))

# remove ic_discound_per_MA4 high p, VIF value
model_11 <- lm(formula = total_gmv ~ avg_sla + 
                 Digital + Sponsorship + 
                 SEM + Other + NPS + npromotion_in_week + inc_discount_per_MA3 + 
                 inc_avg_productmrp_MA3 + 
                 inc_avg_productmrp_MA4 + total_gmv_1 + avg_productmrp_1 + 
                 avg_productmrp_2 + avg_productmrp_3 + discount_per_3, data = train)

summary(model_11) # adj r2 0.6688
sort(vif(model_11))

# remove inc_avg_productmrp_MA4 high p, VIF value
model_12 <- lm(formula = total_gmv ~ avg_sla + 
                 Digital + Sponsorship + 
                 SEM + Other + NPS + npromotion_in_week + inc_discount_per_MA3 + 
                 inc_avg_productmrp_MA3 + 
                 total_gmv_1 + avg_productmrp_1 + 
                 avg_productmrp_2 + avg_productmrp_3 + discount_per_3, data = train)

summary(model_12) # adj r2 0.628
sort(vif(model_12))

# remove inc_avg_productmrp_MA3 high p value
model_13 <- lm(formula = total_gmv ~ avg_sla + 
                 Digital + Sponsorship + 
                 SEM + Other + NPS + npromotion_in_week + inc_discount_per_MA3 + 
                 total_gmv_1 + avg_productmrp_1 + 
                 avg_productmrp_2 + avg_productmrp_3 + discount_per_3, data = train)

summary(model_13) # adj r2 0.6364
sort(vif(model_13))

# remove Sponsorship high p value
model_14 <- lm(formula = total_gmv ~ avg_sla + 
                 Digital +  
                 SEM + Other + NPS + npromotion_in_week + inc_discount_per_MA3 + 
                 total_gmv_1 + avg_productmrp_1 + 
                 avg_productmrp_2 + avg_productmrp_3 + discount_per_3, data = train)

summary(model_14) # adj r2 0.6282
sort(vif(model_14))

# remove discount_per-3  high p value
model_15 <- lm(formula = total_gmv ~ avg_sla + 
                 Digital +  
                 SEM + Other + NPS + npromotion_in_week + inc_discount_per_MA3 + 
                 total_gmv_1 + avg_productmrp_1 + 
                 avg_productmrp_2 + avg_productmrp_3, data = train)

summary(model_15) # adj r2 0.6316
sort(vif(model_15))

# remove Digital  high VIF value
model_16 <- lm(formula = total_gmv ~ avg_sla + 
                 SEM + Other + NPS + npromotion_in_week + inc_discount_per_MA3 + 
                 total_gmv_1 + avg_productmrp_1 + 
                 avg_productmrp_2 + avg_productmrp_3, data = train)

summary(model_16) # adj r2 0.5188
sort(vif(model_16))

# remove `avg_productmrp-3  high p value
model_17 <- lm(formula = total_gmv ~ avg_sla + 
                 SEM + Other + NPS + npromotion_in_week + inc_discount_per_MA3 + 
                 total_gmv_1 + avg_productmrp_1 + 
                 avg_productmrp_2, data = train)

summary(model_17) # adj r2 0.5082
sort(vif(model_17))

# remove NPS  high p value
model_18 <- lm(formula = total_gmv ~ avg_sla + 
                 SEM + Other + npromotion_in_week + inc_discount_per_MA3 + 
                 total_gmv_1 + avg_productmrp_1 + 
                 avg_productmrp_2, data = train)

summary(model_18) # adj r2 0.5168
sort(vif(model_18))

# remove total_gmv-1  high p value
model_19 <- lm(formula = total_gmv ~ avg_sla + 
                 SEM + Other + npromotion_in_week + inc_discount_per_MA3 + 
                 avg_productmrp_1 + 
                 avg_productmrp_2, data = train)

summary(model_19) # adj r2 0.5244
sort(vif(model_19))

# remove npromotion_in_week  high p value
model_20 <- lm(formula = total_gmv ~ avg_sla + 
                 SEM + Other + inc_discount_per_MA3 + 
                 avg_productmrp_1 + 
                 avg_productmrp_2, data = train)

summary(model_20) # adj r2 0.5245
sort(vif(model_20))

# remove avg_sla  high p value
model_21 <- lm(formula = total_gmv ~ SEM + Other + inc_discount_per_MA3 + 
                 avg_productmrp_1 + 
                 avg_productmrp_2, data = train)

summary(model_21) # adj r2 0.5249
sort(vif(model_21))

# remove inc_discount_per_MA3  high p value
model_22 <- lm(formula = total_gmv ~ SEM + Other + avg_productmrp_1 + avg_productmrp_2, data = train)

summary(model_22) # adj r2 0.5211
sort(vif(model_22))

final_model <- model_22
# AIC of generated model
AIC(final_model) # 115

# Final model
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)         0.01143    0.09538   0.120 0.905155    
# SEM                 0.48589    0.10221   4.754 1.92e-05 ***
#  Other               0.41196    0.09961   4.136 0.000145 ***
#  `avg_productmrp-1` -0.45068    0.09713  -4.640 2.81e-05 ***
#  `avg_productmrp-2` -0.42520    0.09828  -4.326 7.85e-05 ***

########## Test accuracy #######

### Cross validation 

################################

train_control <- trainControl(method="cv", number=5)
train <- na.omit(train)
# Fit Naive Bayes Model
model <- train(total_gmv ~ SEM + Other + avg_productmrp_1 + avg_productmrp_2 
               , data=train, trControl=train_control, method="lm")
# Summarise Results
print(model) # r sq 0.517
# RMSE   Rsquared   MAE      
# 0.736  0.517     0.559

DAAG::cv.lm(data = train, form.lm = formula(total_gmv ~ SEM + Other + avg_productmrp_1 + avg_productmrp_2),
            m = 5, dots = FALSE, seed = 29, plotit = c("Observed","Residual"),
            main="Small symbols show cross-validation predicted values",
            legend.pos="topleft", printit = TRUE)
# overall ms 0.563

############################ Elasticity ###############################

elast <- calcElasticity(final_model,train,0)
elast
# variable elasticity direction
#1              SEM      0.504  Positive
#2            Other      0.364  Positive
#3 avg_productmrp_1     -0.094  Negative
#4 avg_productmrp_2    -0.0464  Negative

##plot the elasticity
plot4 <- plotElasticity(elast,"Game Accessory - Dist Lag Model")
plot4

############################# 5. Distributive + Multiplicative MODEL ################################
# As data points are few, hence cross validation to be used. 
# Entire data frame used for training.
train = master_ga_final.data[,-which(names(master_ga_final.data) %in% c("Week","tot_productmrp","norders","nunits","repeated_orders","value_per_visitor"))]
train <- slide(train, Var = "total_gmv", slideBy = -1,NewVar = "total_gmv_1")
train <- slide(train, Var = "total_gmv", slideBy = -2,NewVar = "total_gmv_2")
train <- slide(train, Var = "total_gmv", slideBy = -3,NewVar = "total_gmv_3")

#tagging the lag for avg productmrp and discount_per as well
train <- slide(train, Var = "avg_productmrp", slideBy = -1,NewVar = "avg_productmrp_1")
train <- slide(train, Var = "avg_productmrp", slideBy = -2,NewVar = "avg_productmrp_2")
train <- slide(train, Var = "avg_productmrp", slideBy = -3,NewVar = "avg_productmrp_3")

train <- slide(train, Var = "discount_per", slideBy = -1,NewVar = "discount_per_1")
train <- slide(train, Var = "discount_per", slideBy = -2,NewVar = "discount_per_2")
train <- slide(train, Var = "discount_per", slideBy = -3,NewVar = "discount_per_3")

train <- as.data.frame(sapply(train,function(x) ifelse(x==0,0.0001,x)))
train <- as.data.frame(sapply(train,function(x) log(x)))
train <- as.data.frame(scale(train))
sum(is.na(train))
# Build model 1 containing all variables
model_1 <-lm(total_gmv~.,data=train)
summary(model_1) # adj r2 0.9384, r2 0.9639
#######

# In stepAIC function, lets pass first model i.e model_1 and 
# direction is set as both, to enable both forward selection 
# of variables and backward elimination of variables simultaneously 

step <- stepAIC(model_1, direction="both")
step

# stepAIC does multiple iterations and checks which variables to keep
# Step result contains only the variables it considers to be important in the model. 
# some insignifican variables have been removed. 
# Lets store the last model equation of stepwise method into model_2

model_2 <- lm(formula = total_gmv ~ avg_deliverybdays + avg_deliverycdays + 
                avg_sla + avg_productmrp + avg_prod_proc_sla + earlyDelivery_orders + 
                onTimeDelivery_orders + Total_Investment + TV + Digital + 
                Sponsorship + Content_Marketing + Affiliates + SEM + Radio + 
                Other + npromotion_in_week + week_has_promotion + total_gmv_1 + 
                total_gmv_3 + avg_productmrp_1 + avg_productmrp_2 + avg_productmrp_3 + 
                discount_per_1 + discount_per_2 + discount_per_3, data = train)

summary(model_2) # adj r2 0.833
sort(vif(model_2))

# remove Total_investment. very high VIF
model_3 <- lm(formula = total_gmv ~ avg_deliverybdays + avg_deliverycdays + 
                avg_sla + avg_productmrp + avg_prod_proc_sla + earlyDelivery_orders + 
                onTimeDelivery_orders + TV + Digital + 
                Sponsorship + Content_Marketing + Affiliates + SEM + Radio + 
                Other + npromotion_in_week + week_has_promotion + total_gmv_1 + 
                total_gmv_3 + avg_productmrp_1 + avg_productmrp_2 + avg_productmrp_3 + 
                discount_per_1 + discount_per_2 + discount_per_3, data = train)

summary(model_3) # adj r2 0.967
sort(vif(model_3))

# remove other. high VIF, p value
model_4 <- lm(formula = total_gmv ~ avg_deliverybdays + avg_deliverycdays + 
                avg_sla + avg_productmrp + avg_prod_proc_sla + earlyDelivery_orders + 
                onTimeDelivery_orders + TV + Digital + 
                Sponsorship + Content_Marketing + Affiliates + SEM + Radio + 
                npromotion_in_week + week_has_promotion + total_gmv_1 + 
                total_gmv_3 + avg_productmrp_1 + avg_productmrp_2 + avg_productmrp_3 + 
                discount_per_1 + discount_per_2 + discount_per_3, data = train)

summary(model_4) # adj r2 0.965
sort(vif(model_4))

# remove week_has_promotion. high VIF, p value
model_5 <- lm(formula = total_gmv ~ avg_deliverybdays + avg_deliverycdays + 
                           avg_sla + avg_productmrp + avg_prod_proc_sla + earlyDelivery_orders + 
                           onTimeDelivery_orders + TV + Digital + 
                           Sponsorship + Content_Marketing + Affiliates + SEM + Radio + 
                           npromotion_in_week + total_gmv_1 + 
                           total_gmv_3 + avg_productmrp_1 + avg_productmrp_2 + avg_productmrp_3 + 
                           discount_per_1 + discount_per_2 + discount_per_3, data = train)

summary(model_5) # adj r2 0.966
sort(vif(model_5))

# remove SEM, high VIF, p value
model_6 <- lm(formula = total_gmv ~ avg_deliverybdays + avg_deliverycdays + 
                avg_sla + avg_productmrp + avg_prod_proc_sla + earlyDelivery_orders + 
                onTimeDelivery_orders + TV + Digital + 
                Sponsorship + Content_Marketing + Affiliates + Radio + 
                npromotion_in_week + total_gmv_1 + 
                total_gmv_3 + avg_productmrp_1 + avg_productmrp_2 + avg_productmrp_3 + 
                discount_per_1 + discount_per_2 + discount_per_3, data = train)

summary(model_6) # adj r2 0.967
sort(vif(model_6))

# remove total_gmv-1 high VIF, p value
model_7 <- lm(formula = total_gmv ~ avg_deliverybdays + avg_deliverycdays + 
                avg_sla + avg_productmrp + avg_prod_proc_sla + earlyDelivery_orders + 
                onTimeDelivery_orders + TV + Digital + 
                Sponsorship + Content_Marketing + Affiliates + Radio + 
                week_has_promotion + 
                total_gmv_3 + avg_productmrp_1 + avg_productmrp_2 + avg_productmrp_3 + 
                discount_per_1 + discount_per_2 + discount_per_3, data = train)

summary(model_7) # adj r2 0.965
sort(vif(model_7))

# remove avg_sla high p value
model_8 <- lm(formula = total_gmv ~ avg_deliverybdays + avg_deliverycdays + 
                avg_productmrp + avg_prod_proc_sla + earlyDelivery_orders + 
                onTimeDelivery_orders + TV + Digital + 
                Sponsorship + Content_Marketing + Affiliates + Radio + 
                week_has_promotion + 
                total_gmv_3 + avg_productmrp_1 + avg_productmrp_2 + avg_productmrp_3 + 
                discount_per_1 + discount_per_2 + discount_per_3, data = train)

summary(model_8) # adj r2 0.966
sort(vif(model_8))

# remove avg_deliverycdays high p value
model_9 <- lm(formula = total_gmv ~ avg_deliverybdays + 
                avg_productmrp + avg_prod_proc_sla + earlyDelivery_orders + 
                onTimeDelivery_orders + TV + Digital + 
                Sponsorship + Content_Marketing + Affiliates + Radio + 
                week_has_promotion + 
                total_gmv_3 + avg_productmrp_1 + avg_productmrp_2 + avg_productmrp_3 + 
                discount_per_1 + discount_per_2 + discount_per_3, data = train)

summary(model_9) # adj r2 0.967
sort(vif(model_9))

# remove week_has_promotion high p value
model_10 <- lm(formula = total_gmv ~ avg_deliverybdays + 
                avg_productmrp + avg_prod_proc_sla + earlyDelivery_orders + 
                onTimeDelivery_orders + TV + Digital + 
                Sponsorship + Content_Marketing + Affiliates + Radio + 
                total_gmv_3 + avg_productmrp_1 + avg_productmrp_2 + avg_productmrp_3 + 
                discount_per_1 + discount_per_2 + discount_per_3, data = train)

summary(model_10) # adj r2 0.967
sort(vif(model_10))

# remove discount_per_3 high p value
model_11 <- lm(formula = total_gmv ~ avg_deliverybdays + 
                 avg_productmrp + avg_prod_proc_sla + earlyDelivery_orders + 
                 onTimeDelivery_orders + TV + Digital + 
                 Sponsorship + Content_Marketing + Affiliates + Radio + 
                 total_gmv_3 + avg_productmrp_1 + avg_productmrp_2 + avg_productmrp_3 + 
                 discount_per_1 + discount_per_2, data = train)

summary(model_11) # adj r2 0.968
sort(vif(model_11))

# remove avg_productmrp_2 high p value
model_12 <- lm(formula = total_gmv ~ avg_deliverybdays + 
                 avg_productmrp + avg_prod_proc_sla + earlyDelivery_orders + 
                 onTimeDelivery_orders + TV + Digital + 
                 Sponsorship + Content_Marketing + Affiliates + Radio + 
                 total_gmv_3 + avg_productmrp_1 + avg_productmrp_3 + 
                 discount_per_1 + discount_per_2, data = train)

summary(model_12) # adj r2 0.968
sort(vif(model_12))

# remove discount_per_2 high p value
model_13 <- lm(formula = total_gmv ~ avg_deliverybdays + 
                 avg_productmrp + avg_prod_proc_sla + earlyDelivery_orders + 
                 onTimeDelivery_orders + TV + Digital + 
                 Sponsorship + Content_Marketing + Affiliates + Radio + 
                 total_gmv_3 + avg_productmrp_1 + avg_productmrp_3 + 
                 discount_per_1 , data = train)

summary(model_13) # adj r2 0.968
sort(vif(model_13))

# remove avg_productmrp high p value
model_14 <- lm(formula = total_gmv ~ avg_deliverybdays + 
                 avg_prod_proc_sla + earlyDelivery_orders + 
                 onTimeDelivery_orders + TV + Digital + 
                 Sponsorship + Content_Marketing + Affiliates + Radio + 
                 total_gmv_3 + avg_productmrp_1 + avg_productmrp_3 + 
                 discount_per_1 , data = train)

summary(model_14) # adj r2 0.968
sort(vif(model_14))

# remove avg_deliverybdays high p value
model_15 <- lm(formula = total_gmv ~ avg_prod_proc_sla + earlyDelivery_orders + 
                 onTimeDelivery_orders + TV + Digital + 
                 Sponsorship + Content_Marketing + Affiliates + Radio + 
                 total_gmv_3 + avg_productmrp_1 + avg_productmrp_3 + 
                 discount_per_1 , data = train)

summary(model_15) # adj r2 0.965
sort(vif(model_15))

# remove discount_per_1 high p value
model_16 <- lm(formula = total_gmv ~ avg_prod_proc_sla + earlyDelivery_orders + 
                 onTimeDelivery_orders + TV + Digital + 
                 Sponsorship + Content_Marketing + Affiliates + Radio + 
                 total_gmv_3 + avg_productmrp_1 + avg_productmrp_3 
                  , data = train)

summary(model_16) # adj r2 0.963
sort(vif(model_16))

# remove Affiliates: high VIF value
model_17 <- lm(formula = total_gmv ~ avg_prod_proc_sla + earlyDelivery_orders + 
                 onTimeDelivery_orders + TV + Digital + 
                 Sponsorship + Content_Marketing + Radio + 
                 total_gmv_3 + avg_productmrp_1 + avg_productmrp_3 
               , data = train)

summary(model_17) # adj r2 0.92
sort(vif(model_17))

# remove TV: high VIF, p value
model_18 <- lm(formula = total_gmv ~ avg_prod_proc_sla + earlyDelivery_orders + 
                 onTimeDelivery_orders + Digital + 
                 Sponsorship + Content_Marketing + Radio + 
                 total_gmv_3 + avg_productmrp_1 + avg_productmrp_3 
               , data = train)

summary(model_18) # adj r2 0.918
sort(vif(model_18))

# remove earlyDelivery_orders: high VIF, p value
model_19 <- lm(formula = total_gmv ~ avg_prod_proc_sla +  
                 onTimeDelivery_orders + Digital + 
                 Sponsorship + Content_Marketing + Radio + 
                 total_gmv_3 + avg_productmrp_1 + avg_productmrp_3 
               , data = train)

summary(model_19) # adj r2 0.918
sort(vif(model_19))

# remove content marketing: high VIF, p value
model_20 <- lm(formula = total_gmv ~ avg_prod_proc_sla +  
                 onTimeDelivery_orders + Digital + 
                 Sponsorship + Radio + 
                 total_gmv_3 + avg_productmrp_1 + avg_productmrp_3 
               , data = train)

summary(model_20) # adj r2 0.918
sort(vif(model_20))

# remove Digital: high VIF, p value
model_21 <- lm(formula = total_gmv ~ avg_prod_proc_sla +  
                 onTimeDelivery_orders + 
                 Sponsorship + Radio + 
                 total_gmv_3 + avg_productmrp_1 + avg_productmrp_3 
               , data = train)

summary(model_21) # adj r2 0.918
sort(vif(model_21))

# remove avg_productmrp_3: high p value
model_22 <- lm(formula = total_gmv ~ avg_prod_proc_sla +  
                 onTimeDelivery_orders + 
                 Sponsorship + Radio + 
                 total_gmv_3 + avg_productmrp_1 
               , data = train)

summary(model_22) # adj r2 0.92
sort(vif(model_22))

# remove avg_productmrp_1: high p value
model_23 <- lm(formula = total_gmv ~ avg_prod_proc_sla +  
                 onTimeDelivery_orders + 
                 Sponsorship + Radio + 
                 total_gmv_3 , data = train)

summary(model_23) # adj r2 0.92
sort(vif(model_23))

# remove total_gmv_3: high p value
model_24 <- lm(formula = total_gmv ~ avg_prod_proc_sla + onTimeDelivery_orders + Sponsorship + Radio
                 , data = train)

summary(model_24) # adj r2 0.92
sort(vif(model_24))

final_model <- model_24
# AIC of generated model
AIC(final_model) # 24.2

# Final model
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)           -6.68e-17   3.87e-02    0.00    1.000    
# avg_prod_proc_sla      1.22e-01   4.23e-02    2.87    0.006 ** 
#  onTimeDelivery_orders  7.37e-01   5.04e-02   14.62  < 2e-16 ***
#  Sponsorship            2.31e-01   4.82e-02    4.80  1.5e-05 ***
#  Radio                  2.17e-01   4.06e-02    5.35  2.3e-06 ***

########## Test accuracy #######

### Cross validation 

################################

train_control <- trainControl(method="cv", number=5)
train <- na.omit(train)
# Fit Naive Bayes Model
model <- train(total_gmv ~ avg_prod_proc_sla + onTimeDelivery_orders + Sponsorship + Radio 
               , data=train, trControl=train_control, method="lm")
# Summarise Results
print(model) # r sq 0.712
# RMSE   Rsquared   MAE      
# 0.314  0.712     0.222

DAAG::cv.lm(data = train, form.lm = formula(total_gmv ~ avg_prod_proc_sla + onTimeDelivery_orders + Sponsorship + Radio),
            m = 5, dots = FALSE, seed = 29, plotit = c("Observed","Residual"),
            main="Small symbols show cross-validation predicted values",
            legend.pos="topleft", printit = TRUE)
# overall ms 0.0927

############################ Elasticity ###############################

elast <- calcElasticity(final_model,train,1)
elast
# variable elasticity direction
# 1     avg_prod_proc_sla      0.122  Positive
# 2 onTimeDelivery_orders      0.737  Positive
# 3           Sponsorship      0.231  Positive
# 4                 Radio      0.217  Positive

##plot the elasticity
plot5 <- plotElasticity(elast,"Game Accessory - Dist Lag + Mult Model")
plot5


p <- plot_grid(plot1,plot2,plot3,plot4,plot5, align = "v",ncol = 2)
title <- ggdraw() + draw_label("Gaming Accessory Model Elasticity", fontface='bold')
plot_grid(title, p, ncol=1, rel_heights=c(0.1, 1))
