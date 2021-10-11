install.packages('ggplot2')
install.packages('DataExplorer')
install.packages('Hmisc')
install.packages('dplyr')
install.packages("lubridate")
install.packages("cowplot")
install.packages("tidyr")
install.packages("data.table")
install.packages('DAAG')
install.packages('lmvar')
install.packages("klaR")
install.packages("DataCombine")

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
#####################################################################################
#############################load the data ##########################################
#####################################################################################
#load the data, convert the "\\N" string to NA while loading 

master_ca_final.data <- read.csv('data\\master_ca_final.csv', header = T, stringsAsFactors = F)

######################### Adding Moving Average Variables ############################################################
# Calculating Moving Avg for mrp and discount variables
master_ca_final_with_ma.data <- master_ca_final.data
master_ca_final_with_ma.data <- cbind(master_ca_final_with_ma.data,calcIncrInMAVariable('discount_per',master_ca_final_with_ma.data))
master_ca_final_with_ma.data <- cbind(master_ca_final_with_ma.data,calcIncrInMAVariable('avg_productmrp',master_ca_final_with_ma.data))





 ############################# 1. LINEAR MODEL  ################################
# As data points are few, hence cross validation to be used. 
# Entire data frame used for training.
train = master_ca_final_with_ma.data[,-which(names(master_ca_final_with_ma.data) %in% c("Week","tot_productmrp","norders","nunits","repeated_orders","value_per_visitor"))]
train <- as.data.frame(scale(train))
# Build model 1 containing all variables
model_1 <-lm(total_gmv~.,data=train)
summary(model_1) # Multiple R-squared:  0.816,	Adjusted R-squared:  0.57
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

model_2 <- lm(formula = total_gmv ~ avg_productmrp + avg_prod_proc_sla + 
                discount_per + earlyDelivery_orders + Total_Investment + 
                TV + Digital + Sponsorship + Content_Marketing + Online_Marketing + 
                Affiliates + SEM + Other + NPS + npromotion_in_week + week_has_promotion + 
                inc_discount_per_MA2 + inc_discount_per_MA3 + inc_discount_per_MA4 + 
                inc_avg_productmrp_MA4, data = train)

summary(model_2) # Multiple R-squared:  0.807,	Adjusted R-squared:  0.673
sort(vif(model_2))

# Remove 'Total_Investment' var : high VIF
model_3 <- lm(formula = total_gmv ~ avg_productmrp + avg_prod_proc_sla + 
                discount_per + earlyDelivery_orders + 
                TV + Digital + Sponsorship + Content_Marketing + Online_Marketing + 
                Affiliates + SEM + Other + NPS + npromotion_in_week + week_has_promotion + 
                inc_discount_per_MA2 + inc_discount_per_MA3 + inc_discount_per_MA4 + 
                inc_avg_productmrp_MA4, data = train)

summary(model_3) # Multiple R-squared:  0.761,	Adjusted R-squared:  0.61
sort(vif(model_3))

# Remove 'Online_Marketing' : high VIF value
model_4 <- lm(formula = total_gmv ~ avg_productmrp + avg_prod_proc_sla + 
                discount_per + earlyDelivery_orders + 
                TV + Digital + Sponsorship + Content_Marketing + 
                Affiliates + SEM + Other + NPS + npromotion_in_week + week_has_promotion + 
                inc_discount_per_MA2 + inc_discount_per_MA3 + inc_discount_per_MA4 + 
                inc_avg_productmrp_MA4, data = train)
summary(model_4) # Multiple R-squared:  0.752,	Adjusted R-squared:  0.609  
sort(vif(model_4))

# Remove 'Content_Marketing': high VIF value and p value
model_5 <- lm(formula = total_gmv ~ avg_productmrp + avg_prod_proc_sla + 
                discount_per + earlyDelivery_orders + 
                TV + Digital + Sponsorship + 
                Affiliates + SEM + Other + NPS + npromotion_in_week + week_has_promotion + 
                inc_discount_per_MA2 + inc_discount_per_MA3 + inc_discount_per_MA4 + 
                inc_avg_productmrp_MA4, data = train)
summary(model_5) # Multiple R-squared:  0.749,	Adjusted R-squared:  0.616
sort(vif(model_5))

# Remove 'SEM' : high VIF value and p value
model_6 <- lm(formula = total_gmv ~ avg_productmrp + avg_prod_proc_sla + 
                discount_per + earlyDelivery_orders + 
                TV + Digital + Sponsorship + 
                Affiliates + Other + NPS + npromotion_in_week + week_has_promotion + 
                inc_discount_per_MA2 + inc_discount_per_MA3 + inc_discount_per_MA4 + 
                inc_avg_productmrp_MA4, data = train)
summary(model_6) # Multiple R-squared:  0.73,	Adjusted R-squared:  0.599 
sort(vif(model_6))

# Remove 'inc_discount_per_MA4': high VIF value
model_7 <- lm(formula = total_gmv ~ avg_productmrp + avg_prod_proc_sla + 
                discount_per + earlyDelivery_orders + 
                TV + Digital + Sponsorship + 
                Affiliates + Other + NPS + npromotion_in_week + week_has_promotion + 
                inc_discount_per_MA2 + inc_discount_per_MA3 + 
                inc_avg_productmrp_MA4, data = train)
summary(model_7) # Multiple R-squared:  0.695,	Adjusted R-squared:  0.561
sort(vif(model_7))

# Remove discount_per  high VIF value
model_8 <- lm(formula = total_gmv ~ avg_productmrp + avg_prod_proc_sla + 
                 earlyDelivery_orders + 
                TV + Digital + Sponsorship + 
                Affiliates + Other + NPS + npromotion_in_week + week_has_promotion + 
                inc_discount_per_MA2 + inc_discount_per_MA3 + 
                inc_avg_productmrp_MA4, data = train)
summary(model_8) # Multiple R-squared:  0.662,	Adjusted R-squared:  0.526 
sort(vif(model_8))

# Remove avg_productmrp due to high p value and VIF
model_9 <- lm(formula = total_gmv ~ avg_prod_proc_sla + 
                earlyDelivery_orders + 
                TV + Digital + Sponsorship + 
                Affiliates + Other + NPS + npromotion_in_week + week_has_promotion + 
                inc_discount_per_MA2 + inc_discount_per_MA3 + 
                inc_avg_productmrp_MA4, data = train)
summary(model_9) # Multiple R-squared:  0.649,	Adjusted R-squared:  0.523 
sort(vif(model_9))

# Remove NPS; high VIF and P value
model_10 <- lm(formula = total_gmv ~ avg_prod_proc_sla + 
                 earlyDelivery_orders + 
                 TV + Digital + Sponsorship + 
                 Affiliates + Other + npromotion_in_week + week_has_promotion + 
                 inc_discount_per_MA2 + inc_discount_per_MA3 + 
                 inc_avg_productmrp_MA4, data = train)
summary(model_10) #  Multiple R-squared:  0.599,	Adjusted R-squared:  0.468
sort(vif(model_10))

# Remove inc_discount_per_MA2; high VIF
model_11 <- lm(formula = total_gmv ~ avg_prod_proc_sla + 
                 earlyDelivery_orders + 
                 TV + Digital + Sponsorship + 
                 Affiliates + Other + npromotion_in_week + week_has_promotion + 
                 inc_discount_per_MA3 + 
                 inc_avg_productmrp_MA4, data = train)
summary(model_11) #Multiple R-squared:  0.572,	Adjusted R-squared:  0.448
sort(vif(model_11))

# Remove week_has_promotion; high VIF and p value
model_12 <- lm(formula = total_gmv ~ avg_prod_proc_sla + 
                 earlyDelivery_orders + 
                 TV + Digital + Sponsorship + 
                 Affiliates + Other + npromotion_in_week + 
                 inc_discount_per_MA3 + 
                 inc_avg_productmrp_MA4, data = train)
summary(model_12) # Multiple R-squared:  0.538,	Adjusted R-squared:  0.419
sort(vif(model_12))

# Remove earlyDelivery_orders; high VIF and p value
model_13 <- lm(formula = total_gmv ~ avg_prod_proc_sla + 
                 TV + Digital + Sponsorship + 
                 Affiliates + Other + npromotion_in_week + 
                 inc_discount_per_MA3 + 
                 inc_avg_productmrp_MA4, data = train)
summary(model_13) # Multiple R-squared:  0.537,	Adjusted R-squared:  0.432
sort(vif(model_13))

# Remove Digital; high p
model_14 <- lm(formula = total_gmv ~ avg_prod_proc_sla + 
                 TV + Sponsorship + 
                 Affiliates + Other + npromotion_in_week + 
                 inc_discount_per_MA3 + 
                 inc_avg_productmrp_MA4, data = train)
summary(model_14) # Multiple R-squared:  0.533,	Adjusted R-squared:  0.442
sort(vif(model_14))

# Remove Other; high p
model_15 <- lm(formula = total_gmv ~ avg_prod_proc_sla + 
                 TV + Sponsorship + 
                 Affiliates + npromotion_in_week + 
                 inc_discount_per_MA3 + 
                 inc_avg_productmrp_MA4, data = train)
summary(model_15) # Multiple R-squared:  0.526,	Adjusted R-squared:  0.447 
sort(vif(model_15))

# Remove npromotion_in_week; high p
model_16 <- lm(formula = total_gmv ~ avg_prod_proc_sla + 
                 TV + Sponsorship + 
                 Affiliates + 
                 inc_discount_per_MA3 + 
                 inc_avg_productmrp_MA4, data = train)
summary(model_16) # Multiple R-squared:  0.523,	Adjusted R-squared:  0.456
sort(vif(model_16))

# Remove TV; high p
model_17 <- lm(formula = total_gmv ~ avg_prod_proc_sla + 
                 Sponsorship + 
                 Affiliates + 
                 inc_discount_per_MA3 + 
                 inc_avg_productmrp_MA4, data = train)
summary(model_17) # Multiple R-squared:  0.514,	Adjusted R-squared:  0.459
sort(vif(model_17))

# Remove avg_prod_proc_sla; high p
model_18 <- lm(formula = total_gmv ~ Sponsorship + 
                 Affiliates + 
                 inc_discount_per_MA3 + 
                 inc_avg_productmrp_MA4, data = train)
summary(model_18) # Multiple R-squared:  0.505,	Adjusted R-squared:  0.461
sort(vif(model_18))

# Remove inc_discount_per_MA3; high p
model_19 <- lm(formula = total_gmv ~ Sponsorship + 
                 Affiliates + 
                 inc_avg_productmrp_MA4, data = train)
summary(model_19) # Multiple R-squared:  0.487,	Adjusted R-squared:  0.454 
sort(vif(model_19))


final_model <- model_19

# AIC of generated model
AIC(final_model) # 117

#### Final model for CameraAccessory ##########
#Coefficients:
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept)             -0.0487     0.1049   -0.46    0.645    
#Sponsorship              0.1946     0.1157    1.68    0.099 .  
#Affiliates               0.6459     0.1268    5.09  6.4e-06 ***
#  inc_avg_productmrp_MA4   0.1766     0.1050    1.68    0.099 . 

########## Test accuracy #######
### Cross validation 

################################

train_control <- trainControl(method="cv", number=5)
train <- na.omit(train)
# Fit Naive Bayes Model
model <- train(total_gmv ~ Sponsorship + 
                 Affiliates + 
                 inc_avg_productmrp_MA4 
                 , data=train, trControl=train_control, method="lm")
# Summarise Results
print(model) # r sq 0.546
#  RMSE   Rsquared  MAE  
#  0.751  0.546     0.541

DAAG::cv.lm(data = train, form.lm = formula(total_gmv ~ Sponsorship + 
                                              Affiliates + 
                                              inc_avg_productmrp_MA4
                                              ),
            m = 5, dots = FALSE, seed = 29, plotit = c("Observed","Residual"),
            main="Small symbols show cross-validation predicted values",
            legend.pos="topleft", printit = TRUE)
#Sum of squares = 3.99    Mean square = 0.4    n = 10
#Overall (Sum over all 17 folds) 
#ms 
#0.71 



############################ Elasticity ###############################
elast <- calcElasticity(final_model,train,0)
elast

#                variable elasticity direction
#1            Sponsorship   3.35e-01  Positive
#2             Affiliates   1.73e+00  Positive
#3 inc_avg_productmrp_MA4  -2.04e-17  Negative

##plot the elasticity

plot1 <- plotElasticity(elast,"Camera Accessory - Linear Model")

############################# 2. Multiplicative MODEL ################################
# As data points are few, hence cross validation to be used. 
# Entire data frame used for training.
train = master_ca_final_with_ma.data[,-which(names(master_ca_final_with_ma.data) %in% c("Week","tot_productmrp","norders","nunits","repeated_orders","value_per_visitor","inc_avg_productmrp_MA2","inc_avg_productmrp_MA3","inc_avg_productmrp_MA4","inc_discount_per_MA2","inc_discount_per_MA3","inc_discount_per_MA4"))]
train <- as.data.frame(sapply(train,function(x) ifelse(x==0,0.0001,x)))
train <- as.data.frame(sapply(train,function(x) log(x)))
train <- as.data.frame(scale(train))
sum(is.na(train))
# Build model 1 containing all variables
model_1 <-lm(total_gmv~.,data=train)
summary(model_1) # Multiple R-squared:  0.964,	Adjusted R-squared:  0.937
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

model_2 <- lm(formula = total_gmv ~ avg_deliverycdays + avg_productmrp + 
                discount_per + earlyDelivery_orders + onTimeDelivery_orders + 
                TV + Digital + Sponsorship + Content_Marketing + Affiliates + 
                Other + week_has_promotion, data = train)

summary(model_2) # Multiple R-squared:  0.961,	Adjusted R-squared:  0.95
sort(vif(model_2))

# remove Affiliates : high VIF value
model_3 <- lm(formula = total_gmv ~ avg_deliverycdays + avg_productmrp + 
                discount_per + earlyDelivery_orders + onTimeDelivery_orders + 
                TV + Sponsorship + Content_Marketing + Digital + 
                Other + week_has_promotion, data = train)

summary(model_3) # Multiple R-squared:  0.936,	Adjusted R-squared:  0.919
sort(vif(model_3))

# remove TV : high VIF value
model_4 <- lm(formula = total_gmv ~ avg_deliverycdays + avg_productmrp + 
                discount_per + earlyDelivery_orders + onTimeDelivery_orders + 
                Sponsorship + Content_Marketing + Digital + 
                Other + week_has_promotion, data = train)

summary(model_4) # Multiple R-squared:  0.935,	Adjusted R-squared:  0.92
sort(vif(model_4))

# remove earlyDelivery_orders : high VIF value
model_5 <- lm(formula = total_gmv ~ avg_deliverycdays + avg_productmrp + 
                discount_per + onTimeDelivery_orders + 
                Sponsorship + Content_Marketing + Digital + 
                Other + week_has_promotion, data = train)

summary(model_5) # Multiple R-squared:  0.928,	Adjusted R-squared:  0.912
sort(vif(model_5))

# remove avg_productmrp: high p and VIF value
model_6 <- lm(formula = total_gmv ~ avg_deliverycdays + 
                discount_per + onTimeDelivery_orders + 
                Sponsorship + Content_Marketing + Digital + 
                Other + week_has_promotion, data = train)

summary(model_6) # Multiple R-squared:  0.928,	Adjusted R-squared:  0.914
sort(vif(model_6))

# remove avg_deliverycdays: high p value
model_7 <- lm(formula = total_gmv ~ discount_per + onTimeDelivery_orders + 
                Sponsorship + Content_Marketing + Digital + 
                Other + week_has_promotion, data = train)

summary(model_7) # Multiple R-squared:  0.927,	Adjusted R-squared:  0.916
sort(vif(model_7))

# remove Content_Marketing: high VIF value
model_8 <- lm(formula = total_gmv ~ discount_per + onTimeDelivery_orders + 
                Sponsorship + Digital + 
                Other + week_has_promotion, data = train)

summary(model_8) # Multiple R-squared:  0.925,	Adjusted R-squared:  0.915
sort(vif(model_8))

# remove week_has_promotion: high p value
model_9 <- lm(formula = total_gmv ~ discount_per + onTimeDelivery_orders + 
                Sponsorship + Digital + 
                Other, data = train)

summary(model_9) # Multiple R-squared:  0.925,	Adjusted R-squared:  0.917
sort(vif(model_9))

# remove Digital: high p value
model_10 <- lm(formula = total_gmv ~ discount_per + onTimeDelivery_orders + 
                 Sponsorship + 
                 Other, data = train)

summary(model_10) # Multiple R-squared:  0.92,	Adjusted R-squared:  0.913
sort(vif(model_10))

# remove discount_per: high p value
model_11 <- lm(formula = total_gmv ~ onTimeDelivery_orders + 
                 Sponsorship + 
                 Other, data = train)

summary(model_11) # Multiple R-squared:  0.913,	Adjusted R-squared:  0.908
sort(vif(model_11))

# remove Other - high vif
model_12 <- lm(formula = total_gmv ~ onTimeDelivery_orders + 
                 Sponsorship, data = train)
summary(model_12) # Multiple R-squared:  0.883,	Adjusted R-squared:  0.879
sort(vif(model_12))


final_model <- model_12

# AIC of generated model
AIC(final_model) # 43.5

########## Test accuracy #######
### Cross validation 

################################
train_control <- trainControl(method="cv", number=5)
# Fit Naive Bayes Model
model <- train(total_gmv ~ onTimeDelivery_orders + 
                 Sponsorship, data=train, trControl=train_control, method="lm")
# Summarise Results
print(model) # R Squ 0.453
#  RMSE   Rsquared  MAE  
# 0.371  0.453     0.286

DAAG::cv.lm(data = train, form.lm = formula(total_gmv ~ onTimeDelivery_orders + Sponsorship),
            m = 5, dots = FALSE, seed = 29, plotit = c("Observed","Residual"),
            main="Small symbols show cross-validation predicted values",
            legend.pos="topleft", printit = TRUE)
#Sum of squares = 3.12    Mean square = 0.17    n = 18 
#Overall (Sum over all 10 folds) 
#ms 
#0.152 

############################ Elasticity ###############################
elast <- calcElasticity(final_model,master_ca_final_with_ma.data,1)
elast

#               variable elasticity direction
#1 onTimeDelivery_orders      0.841  Positive
#2           Sponsorship      0.189  Positive

##plot the elasticity

plot2 <- plotElasticity(elast,"Camera Accessory - Multiplicative Mode")


############################# 3. Distributive MODEL ################################
# As data points are few, hence cross validation to be used. 
# Entire data frame used for training.
train = master_ca_final_with_ma.data[,-which(names(master_ca_final_with_ma.data) %in% c("Week","tot_productmrp","norders","nunits","repeated_orders","value_per_visitor"))]
train <- slide(train, Var = "total_gmv", slideBy = -1)
train <- slide(train, Var = "total_gmv", slideBy = -2)
train <- slide(train, Var = "total_gmv", slideBy = -3)
train <- as.data.frame(scale(train))
sum(is.na(train))
# Build model 1 containing all variables
model_1 <-lm(total_gmv~.,data=train)
summary(model_1) # Multiple R-squared:  0.823,	Adjusted R-squared:  0.519 
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

model_2 <- lm(formula = total_gmv ~ avg_productmrp + avg_prod_proc_sla + 
                discount_per + Total_Investment + TV + Digital + Sponsorship + 
                Content_Marketing + Online_Marketing + Affiliates + SEM + 
                Other + NPS + npromotion_in_week + week_has_promotion + inc_discount_per_MA2 + 
                inc_discount_per_MA3 + inc_discount_per_MA4 + inc_avg_productmrp_MA4 + 
                `total_gmv-1` + `total_gmv-2`, data = train)

summary(model_2) # Multiple R-squared:  0.814,	Adjusted R-squared:  0.675
sort(vif(model_2))

# remove Total_Investment: high VIF
model_3 <- lm(formula = total_gmv ~ avg_productmrp + avg_prod_proc_sla + 
                discount_per + TV + Digital + Sponsorship + 
                Content_Marketing + Online_Marketing + Affiliates + SEM + 
                Other + NPS + npromotion_in_week + week_has_promotion + inc_discount_per_MA2 + 
                inc_discount_per_MA3 + inc_discount_per_MA4 + inc_avg_productmrp_MA4 + 
                `total_gmv-1` + `total_gmv-2`, data = train)

summary(model_3) # Multiple R-squared:  0.76,	Adjusted R-squared:  0.595
sort(vif(model_3))

# remove Online_Marketing; high VIF, high p value
model_4 <- lm(formula = total_gmv ~ avg_productmrp + avg_prod_proc_sla + 
                discount_per + TV + Digital + Sponsorship + 
                Content_Marketing + Affiliates + SEM + 
                Other + NPS + npromotion_in_week + week_has_promotion + inc_discount_per_MA2 + 
                inc_discount_per_MA3 + inc_discount_per_MA4 + inc_avg_productmrp_MA4 + 
                `total_gmv-1` + `total_gmv-2`, data = train)

summary(model_4) # Multiple R-squared:  0.76,	Adjusted R-squared:  0.608
sort(vif(model_4))

# remove Content_Marketing high VIF value
model_5 <- lm(formula = total_gmv ~ avg_productmrp + avg_prod_proc_sla + 
                discount_per + TV + Digital + Sponsorship + 
                Affiliates + SEM + 
                Other + NPS + npromotion_in_week + week_has_promotion + inc_discount_per_MA2 + 
                inc_discount_per_MA3 + inc_discount_per_MA4 + inc_avg_productmrp_MA4 + 
                `total_gmv-1` + `total_gmv-2`, data = train)

summary(model_5) # Multiple R-squared:  0.746,	Adjusted R-squared:  0.599 
sort(vif(model_5))

# remove SEM high VIF value
model_6 <- lm(formula = total_gmv ~ avg_productmrp + avg_prod_proc_sla + 
                discount_per + TV + Digital + Sponsorship + 
                Affiliates + 
                Other + NPS + npromotion_in_week + week_has_promotion + inc_discount_per_MA2 + 
                inc_discount_per_MA3 + inc_discount_per_MA4 + inc_avg_productmrp_MA4 + 
                `total_gmv-1` + `total_gmv-2`, data = train)

summary(model_6) # Multiple R-squared:  0.727,	Adjusted R-squared:  0.582
sort(vif(model_6))

# remove inc_discount_per_MA3 due to high VIF
model_7 <- lm(formula = total_gmv ~ avg_productmrp + avg_prod_proc_sla + 
                discount_per + TV + Digital + Sponsorship + 
                Affiliates + 
                Other + NPS + npromotion_in_week + week_has_promotion + inc_discount_per_MA2 + 
                inc_discount_per_MA4 + inc_avg_productmrp_MA4 + 
                `total_gmv-1` + `total_gmv-2`, data = train)

summary(model_7) # Multiple R-squared:  0.703,	Adjusted R-squared:  0.559 
sort(vif(model_7))

# remove discount_per high VIF value
model_8 <- lm(formula = total_gmv ~ avg_productmrp + avg_prod_proc_sla + 
                TV + Digital + Sponsorship + 
                Affiliates + 
                Other + NPS + npromotion_in_week + week_has_promotion + inc_discount_per_MA2 + 
                inc_discount_per_MA4 + inc_avg_productmrp_MA4 + 
                `total_gmv-1` + `total_gmv-2`, data = train)

summary(model_8) # Multiple R-squared:  0.687,	Adjusted R-squared:  0.549 
sort(vif(model_8))

# remove inc_avg_productmrp_MA4 high p value
model_9 <- lm(formula = total_gmv ~ avg_productmrp + avg_prod_proc_sla + 
                TV + Digital + Sponsorship + 
                Affiliates + 
                Other + NPS + npromotion_in_week + week_has_promotion + inc_discount_per_MA2 + 
                inc_discount_per_MA4 + 
                `total_gmv-1` + `total_gmv-2`, data = train)

summary(model_9) # Multiple R-squared:  0.685,	Adjusted R-squared:  0.559
sort(vif(model_9))

# remove Digital high p value
model_10 <- lm(formula = total_gmv ~ avg_productmrp + avg_prod_proc_sla + 
                 TV + Sponsorship + 
                 Affiliates + 
                 Other + NPS + npromotion_in_week + week_has_promotion + inc_discount_per_MA2 + 
                 inc_discount_per_MA4 + 
                 `total_gmv-1` + `total_gmv-2`, data = train)

summary(model_10) # Multiple R-squared:  0.682,	Adjusted R-squared:  0.567
sort(vif(model_10)) 

# remove week_has_promotion high VIF value and p  value
model_11 <- lm(formula = total_gmv ~ avg_productmrp + avg_prod_proc_sla + 
                 TV + Sponsorship + 
                 Affiliates + 
                 Other + NPS + npromotion_in_week + inc_discount_per_MA2 + 
                 inc_discount_per_MA4 + 
                 `total_gmv-1` + `total_gmv-2`, data = train)

summary(model_11) # Multiple R-squared:  0.663,	Adjusted R-squared:  0.554
sort(vif(model_11)) 

# remove npromotion_in_week marketing high p value
model_12 <- lm(formula = total_gmv ~ avg_productmrp + avg_prod_proc_sla + 
                 TV + Sponsorship + 
                 Affiliates + 
                 Other + NPS + inc_discount_per_MA2 + 
                 inc_discount_per_MA4 + 
                 `total_gmv-1` + `total_gmv-2`, data = train)

summary(model_12) # Multiple R-squared:  0.652,	Adjusted R-squared:  0.552 
sort(vif(model_12)) 

# remove avg_prod_proc_sla high p value
model_13 <- lm(formula = total_gmv ~ avg_productmrp + 
                 TV + Sponsorship + 
                 Affiliates + 
                 Other + NPS + inc_discount_per_MA2 + 
                 inc_discount_per_MA4 + 
                 `total_gmv-1` + `total_gmv-2`, data = train)

summary(model_13) # Multiple R-squared:  0.631,	Adjusted R-squared:  0.536
sort(vif(model_13))

# Remove total_gmv-2 high p and VIF value
model_14 <- lm(formula = total_gmv ~ avg_productmrp + 
                 TV + Sponsorship + 
                 Affiliates + 
                 Other + NPS + inc_discount_per_MA2 + 
                 inc_discount_per_MA4 + 
                 `total_gmv-1`, data = train)

summary(model_14) # Multiple R-squared:  0.613,	Adjusted R-squared:  0.526
sort(vif(model_14))

# remove total_gmv-1 high p value
model_15 <- lm(formula = total_gmv ~ avg_productmrp + 
                 TV + Sponsorship + 
                 Affiliates + 
                 Other + NPS + inc_discount_per_MA2 + 
                 inc_discount_per_MA4, data = train)

summary(model_15) # Multiple R-squared:  0.588,	Adjusted R-squared:  0.507 
sort(vif(model_15))

# remove inc_discount_per_MA2 high p value
model_16 <- lm(formula = total_gmv ~ avg_productmrp + 
                 TV + Sponsorship + 
                 Affiliates + 
                 Other + NPS + 
                 inc_discount_per_MA4, data = train)

summary(model_16) # Multiple R-squared:  0.562,	Adjusted R-squared:  0.489 
sort(vif(model_16)) 

# remove Other high p value
model_17 <- lm(formula = total_gmv ~ avg_productmrp + 
                 TV + Sponsorship + 
                 Affiliates + 
                 NPS + 
                 inc_discount_per_MA4, data = train)

summary(model_17) # Multiple R-squared:  0.533,	Adjusted R-squared:  0.468
sort(vif(model_17)) 

# remove TV marketing high p value
model_18 <- lm(formula = total_gmv ~ avg_productmrp + 
                 Sponsorship + 
                 Affiliates + 
                 NPS + 
                 inc_discount_per_MA4, data = train)

summary(model_18) # Multiple R-squared:  0.518,	Adjusted R-squared:  0.463
sort(vif(model_18)) 

# remove NPS high p value
model_19 <- lm(formula = total_gmv ~ avg_productmrp + 
                 Sponsorship + 
                 Affiliates + 
                 inc_discount_per_MA4, data = train)

summary(model_19) # Multiple R-squared:  0.493,	Adjusted R-squared:  0.448
sort(vif(model_19)) 

# remove Sponsorship high p value
model_20 <- lm(formula = total_gmv ~ avg_productmrp + 
                 Affiliates + 
                 inc_discount_per_MA4, data = train)

summary(model_20) # Multiple R-squared:  0.483,	Adjusted R-squared:  0.449 
sort(vif(model_20)) 

# remove inc_discount_per_MA4 high p value
model_21 <- lm(formula = total_gmv ~ avg_productmrp + 
                 Affiliates, data = train)

summary(model_21) # Multiple R-squared:  0.446,	Adjusted R-squared:  0.424 
sort(vif(model_21)) 

final_model <- model_21

# AIC of generated model
AIC(final_model) # 126

########## Test accuracy #######
### Cross validation 

################################
train_control <- trainControl(method="cv", number=5)
# Fit Naive Bayes Model
model <- train(total_gmv ~ avg_productmrp + 
                 Affiliates, data=train, trControl=train_control, method="lm")
# Summarise Results
print(model) # r2 0.438
#  RMSE   Rsquared  MAE 
#0.719  0.438     0.53

DAAG::cv.lm(data = train, form.lm = formula(total_gmv ~ avg_productmrp + 
                                              Affiliates),
            m = 5, dots = FALSE, seed = 29, plotit = c("Observed","Residual"),
            main="Small symbols show cross-validation predicted values",
            legend.pos="topleft", printit = TRUE)
#Sum of squares = 2.67    Mean square = 0.27    n = 10 
#Overall (Sum over all 10 folds) 
#ms 
#0.595

############################ Elasticity ###############################
elast <- calcElasticity(final_model,train,0)
elast

#             variable elasticity direction
#1 avg_productmrp      -2.48  Negative
#2     Affiliates      -5.08  Negative

##plot the elasticity

plot3 <- plotElasticity(elast,"Camera Accessory - Distributive Lag Mode")

####################################################################################
###################### 4. Distributive Lag + Mutiplicative model
# As data points are few, hence cross validation to be used. 
# Entire data frame used for training.
train = master_ca_final_with_ma.data[,-which(names(master_ca_final_with_ma.data) %in% c("Week","tot_productmrp","norders","nunits","repeated_orders","value_per_visitor","inc_avg_productmrp_MA2","inc_avg_productmrp_MA3","inc_avg_productmrp_MA4","inc_discount_per_MA2","inc_discount_per_MA3","inc_discount_per_MA4"))]
train <- slide(train, Var = "total_gmv", slideBy = -1)
train <- slide(train, Var = "total_gmv", slideBy = -2)
train <- slide(train, Var = "total_gmv", slideBy = -3)


train <- as.data.frame(sapply(train,function(x) ifelse(x==0,0.0001,x)))
train <- as.data.frame(sapply(train,function(x) log(x)))
train <- as.data.frame(scale(train))
sum(is.na(train))


# Build model 1 containing all variables
model_1 <-lm(total_gmv~.,data=train)
summary(model_1) #Multiple R-squared:  0.986,	Adjusted R-squared:  0.971 
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

model_2 <- lm(formula = total_gmv ~ avg_deliverycdays + avg_productmrp + 
                avg_prod_proc_sla + discount_per + earlyDelivery_orders + 
                onTimeDelivery_orders + Total_Investment + TV + Digital + 
                Sponsorship + Content_Marketing + Affiliates + SEM + Radio + 
                Other + week_has_promotion + `total_gmv-1` + `total_gmv-2` + 
                `total_gmv-3`, data = train)

summary(model_2) #Multiple R-squared:  0.985,	Adjusted R-squared:  0.975
sort(vif(model_2)) 

# removing Radio due to high VIF

model_3 <- lm(formula = total_gmv ~ avg_deliverycdays + avg_productmrp + 
                avg_prod_proc_sla + discount_per + earlyDelivery_orders + 
                onTimeDelivery_orders + Total_Investment + TV + Digital + 
                Sponsorship + Content_Marketing + Affiliates + SEM + Other + 
                week_has_promotion + `total_gmv-1` + `total_gmv-2` + 
                `total_gmv-3`, data = train)

summary(model_3) #Multiple R-squared:  0.983,	Adjusted R-squared:  0.972 
sort(vif(model_3)) 


# removing Total_Investment due to high VIF and p value

model_4 <- lm(formula = total_gmv ~ avg_deliverycdays + avg_productmrp + 
                avg_prod_proc_sla + discount_per + earlyDelivery_orders + 
                onTimeDelivery_orders + TV + Digital + 
                Sponsorship + Content_Marketing + Affiliates + SEM + Other + 
                week_has_promotion + `total_gmv-1` + `total_gmv-2` + 
                `total_gmv-3`, data = train)

summary(model_4) #Multiple R-squared:  0.98,	Adjusted R-squared:  0.97 
sort(vif(model_4)) 


# removing Affiliates due to high VIF

model_5 <- lm(formula = total_gmv ~ avg_deliverycdays + avg_productmrp + 
                avg_prod_proc_sla + discount_per + earlyDelivery_orders + 
                onTimeDelivery_orders + TV + Digital + 
                Sponsorship + Content_Marketing + SEM + Other + 
                week_has_promotion + `total_gmv-1` + `total_gmv-2` + 
                `total_gmv-3`, data = train)

summary(model_5) #Multiple R-squared:  0.962,	Adjusted R-squared:  0.944
sort(vif(model_5)) 


# removing earlyDelivery_orders due to high VIF

model_6 <- lm(formula = total_gmv ~ avg_deliverycdays + avg_productmrp + 
                avg_prod_proc_sla + discount_per + 
                onTimeDelivery_orders + Digital + 
                Sponsorship + Content_Marketing + SEM + Other + 
                week_has_promotion + `total_gmv-1` + `total_gmv-2` + 
                `total_gmv-3`, data = train)

summary(model_6) #Multiple R-squared:  0.951,	Adjusted R-squared:  0.932 
sort(vif(model_6)) 

# removing SEM due to high VIF

model_7 <- lm(formula = total_gmv ~ avg_deliverycdays + avg_productmrp + 
                avg_prod_proc_sla + discount_per + 
                onTimeDelivery_orders + Digital + 
                Sponsorship + Content_Marketing + Other + 
                week_has_promotion + `total_gmv-1` + `total_gmv-2` + 
                `total_gmv-3`, data = train)

summary(model_7) #Multiple R-squared:  0.946,	Adjusted R-squared:  0.927
sort(vif(model_7)) 

# removing discount_per due to high p value

model_8 <- lm(formula = total_gmv ~ avg_deliverycdays + avg_productmrp + 
                avg_prod_proc_sla + 
                onTimeDelivery_orders + Digital + 
                Sponsorship + Content_Marketing + Other + 
                week_has_promotion + `total_gmv-1` + `total_gmv-2` + 
                `total_gmv-3`, data = train)

summary(model_8) #Multiple R-squared:  0.946,	Adjusted R-squared:  0.929
sort(vif(model_8)) 

# removing total_gmv-3 due to high p value

model_9 <- lm(formula = total_gmv ~ avg_deliverycdays + avg_productmrp + 
                avg_prod_proc_sla + 
                onTimeDelivery_orders + Digital + 
                Sponsorship + Content_Marketing + Other + 
                week_has_promotion + `total_gmv-1` + `total_gmv-2`, data = train)

summary(model_9) #Multiple R-squared:  0.945,	Adjusted R-squared:  0.929  
sort(vif(model_9)) 


# removing Content_Marketing due to high VIF and p value

model_10 <- lm(formula = total_gmv ~ avg_deliverycdays + avg_productmrp + 
                 avg_prod_proc_sla + 
                 onTimeDelivery_orders + Digital + 
                 Sponsorship + Other + 
                 week_has_promotion + `total_gmv-1` + `total_gmv-2`, data = train)

summary(model_10) #Multiple R-squared:  0.945,	Adjusted R-squared:  0.931
sort(vif(model_10)) 


# removing avg_prod_proc_sla due to high p value

model_11 <- lm(formula = total_gmv ~ avg_deliverycdays + avg_productmrp + 
                 onTimeDelivery_orders + Digital + 
                 Sponsorship + Other + 
                 week_has_promotion + `total_gmv-1` + `total_gmv-2`, data = train)

summary(model_11) #Multiple R-squared:  0.944,	Adjusted R-squared:  0.932 
sort(vif(model_11)) 


# removing avg_productmrp due to high p value

model_12 <- lm(formula = total_gmv ~ avg_deliverycdays + 
                 onTimeDelivery_orders + Digital + 
                 Sponsorship + Other + 
                 week_has_promotion + `total_gmv-1` + `total_gmv-2`, data = train)

summary(model_12) #Multiple R-squared:  0.944,	Adjusted R-squared:  0.933
sort(vif(model_12)) 

# removing avg_deliverycdays due to high p value

model_13 <- lm(formula = total_gmv ~ onTimeDelivery_orders + Digital + 
                 Sponsorship + Other + 
                 week_has_promotion + `total_gmv-1` + `total_gmv-2`, data = train)

summary(model_13) #Multiple R-squared:  0.942,	Adjusted R-squared:  0.933
sort(vif(model_13)) 



# removing week_has_promotion due to high p value

model_14 <- lm(formula = total_gmv ~ onTimeDelivery_orders + Digital + 
                 Sponsorship + Other + 
                 `total_gmv-1` + `total_gmv-2`, data = train)

summary(model_14) #Multiple R-squared:  0.941,	Adjusted R-squared:  0.933 
sort(vif(model_14))

# removing Sponsorship due to high p value

model_15 <- lm(formula = total_gmv ~ onTimeDelivery_orders + Digital + 
                 Other + 
                 `total_gmv-1` + `total_gmv-2`, data = train)

summary(model_15) #Multiple R-squared:  0.938,	Adjusted R-squared:  0.931 
sort(vif(model_15)) 


# removing total_gmv-2 due to high p value

model_16 <- lm(formula = total_gmv ~ onTimeDelivery_orders + Digital + 
                 Other + 
                 `total_gmv-1`, data = train)

summary(model_16) #Multiple R-squared:  0.934,	Adjusted R-squared:  0.929
sort(vif(model_16)) 

# removing Digital due to high p value

model_17 <- lm(formula = total_gmv ~ onTimeDelivery_orders + 
                 Other + 
                 `total_gmv-1`, data = train)

summary(model_17) #Multiple R-squared:  0.927,	Adjusted R-squared:  0.922 
sort(vif(model_17)) 

# removing Other due to high p value

model_18 <- lm(formula = total_gmv ~ onTimeDelivery_orders + 
                 `total_gmv-1`, data = train)

summary(model_18) #Multiple R-squared:  0.919,	Adjusted R-squared:  0.915
sort(vif(model_18)) 

final_model <- model_18

# AIC of generated model
AIC(final_model) # 24.5

# Final Model is model 18
#Coefficients:
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept)             0.0180     0.0405    0.45     0.66    
#onTimeDelivery_orders   0.8213     0.0451   18.21  < 2e-16 ***
#  `total_gmv-1`           0.2446     0.0455    5.37  2.1e-06 ***


########## Test accuracy #######
### Cross validation 

################################
train_control <- trainControl(method="cv", number=5)
train <- na.omit(train)
# Fit Naive Bayes Model``
model <- train(total_gmv ~ onTimeDelivery_orders + 
                 `total_gmv-1`, data=train, trControl=train_control, method="lm")
# Summarise Results
print(model) # r2 0.543
#  RMSE   Rsquared  MAE  
# 0.288  0.543     0.204

DAAG::cv.lm(data = train, form.lm = formula(total_gmv ~ onTimeDelivery_orders + 
                                              `total_gmv-1`),
            m = 5, dots = FALSE, seed = 29, plotit = c("Observed","Residual"),
            main="Small symbols show cross-validation predicted values",
            legend.pos="topleft", printit = TRUE)
#Sum of squares = 0.23    Mean square = 0.02    n = 10 

#Overall (Sum over all 10 folds) 
#ms 
#0.0929 

############################ Elasticity ###############################
elast <- calcElasticity(final_model,train,1)
elast

#               variable elasticity direction
#1 onTimeDelivery_orders      0.821  Positive
#2         `total_gmv-1`      0.245  Positive

##plot the elasticity

plot4 <- plotElasticity(elast,"Camera Accessory - Mutiplicative + Distributive Lag Mode")


#############################################################################################
#################################### Koycks Model ###########################################

# As data points are few, hence cross validation to be used. 
# Entire data frame used for training.
#take the lag value of 1 week for the total gmv
train = master_ca_final_with_ma.data[,-which(names(master_ca_final_with_ma.data) %in% c("Week","tot_productmrp","norders","nunits","repeated_orders","value_per_visitor"))]
train <- slide(train, Var = "total_gmv", slideBy = -1)

#scale the data
train <- as.data.frame(scale(train))

model_1 <- lm(total_gmv ~ ., data = train)
summary(model_1) #Multiple R-squared:  0.817,	Adjusted R-squared:  0.553

model_2 <- stepAIC(model_1, direction="both")
model_2

model_3 <- lm(formula = total_gmv ~ avg_productmrp + avg_prod_proc_sla + 
                discount_per + earlyDelivery_orders + Total_Investment + 
                TV + Digital + Sponsorship + Content_Marketing + Online_Marketing + 
                Affiliates + SEM + Other + NPS + npromotion_in_week + week_has_promotion + 
                inc_discount_per_MA2 + inc_discount_per_MA3 + inc_discount_per_MA4 + 
                inc_avg_productmrp_MA4, data = train)
summary(model_3) #Multiple R-squared:  0.807,	Adjusted R-squared:  0.673

sort(vif(model_3)) 

#Total_Investment has high VIF

model_4 <- lm(formula = total_gmv ~ avg_productmrp + avg_prod_proc_sla + 
                discount_per + earlyDelivery_orders + 
                TV + Digital + Sponsorship + Content_Marketing + Online_Marketing + 
                Affiliates + SEM + Other + NPS + npromotion_in_week + week_has_promotion + 
                inc_discount_per_MA2 + inc_discount_per_MA3 + inc_discount_per_MA4 + 
                inc_avg_productmrp_MA4, data = train)
summary(model_4) #Multiple R-squared:  0.761,	Adjusted R-squared:  0.61 

sort(vif(model_4))

#Online_Marketing has high VIF

model_5 <- lm(formula = total_gmv ~ avg_productmrp + avg_prod_proc_sla + 
                discount_per + earlyDelivery_orders + 
                TV + Digital + Sponsorship + Content_Marketing + 
                Affiliates + SEM + Other + NPS + npromotion_in_week + week_has_promotion + 
                inc_discount_per_MA2 + inc_discount_per_MA3 + inc_discount_per_MA4 + 
                inc_avg_productmrp_MA4, data = train)
summary(model_5) #Multiple R-squared:  0.752,	Adjusted R-squared:  0.609

sort(vif(model_5))

#Content_Marketing has high VIF 

model_6 <- lm(formula = total_gmv ~ avg_productmrp + avg_prod_proc_sla + 
                discount_per + earlyDelivery_orders + 
                TV + Digital + Sponsorship + 
                Affiliates + SEM + Other + NPS + npromotion_in_week + week_has_promotion + 
                inc_discount_per_MA2 + inc_discount_per_MA3 + inc_discount_per_MA4 + 
                inc_avg_productmrp_MA4, data = train)
summary(model_6) #Multiple R-squared:  0.749,	Adjusted R-squared:  0.616

sort(vif(model_6)) 

#SEM has high VIF 

model_7 <- lm(formula = total_gmv ~ avg_productmrp + avg_prod_proc_sla + 
                discount_per + earlyDelivery_orders + 
                TV + Digital + Sponsorship + 
                Affiliates + Other + NPS + npromotion_in_week + week_has_promotion + 
                inc_discount_per_MA2 + inc_discount_per_MA3 + inc_discount_per_MA4 + 
                inc_avg_productmrp_MA4, data = train)
summary(model_7) #Multiple R-squared:  0.73,	Adjusted R-squared:  0.599 

sort(vif(model_7))

#inc_discount_per_MA4 has high VIF value

model_8 <- lm(formula = total_gmv ~ avg_productmrp + avg_prod_proc_sla + 
                discount_per + earlyDelivery_orders + 
                TV + Digital + Sponsorship + 
                Affiliates + Other + NPS + npromotion_in_week + week_has_promotion + 
                inc_discount_per_MA2 + inc_discount_per_MA3 + 
                inc_avg_productmrp_MA4, data = train)
summary(model_8) #Multiple R-squared:  0.695,	Adjusted R-squared:  0.561 

sort(vif(model_8) )

#discount_per has high VIF and p value

model_9 <- lm(formula = total_gmv ~ avg_productmrp + avg_prod_proc_sla + 
                inc_discount_per_MA3 + earlyDelivery_orders + 
                TV + Digital + Sponsorship + 
                Affiliates + Other + NPS + npromotion_in_week + week_has_promotion + 
                inc_discount_per_MA2 + 
                inc_avg_productmrp_MA4, data = train)
summary(model_9) #Multiple R-squared:  0.662,	Adjusted R-squared:  0.526 

sort(vif(model_9) )

#avg_productmrp has high p-value and VIF

model_10 <- lm(formula = total_gmv ~ avg_prod_proc_sla + 
                 inc_discount_per_MA3 + earlyDelivery_orders + 
                 TV + Digital + Sponsorship + 
                 Affiliates + Other + NPS + npromotion_in_week + week_has_promotion + 
                 inc_discount_per_MA2 + 
                 inc_avg_productmrp_MA4, data = train)
summary(model_10) #Multiple R-squared:  0.649,	Adjusted R-squared:  0.523

sort(vif(model_10)) 

#Digital has high p value

model_11 <- lm(formula = total_gmv ~ avg_prod_proc_sla + 
                 inc_discount_per_MA3 + earlyDelivery_orders + 
                 TV + Sponsorship + 
                 Affiliates + Other + NPS + npromotion_in_week + week_has_promotion + 
                 inc_discount_per_MA2 + 
                 inc_avg_productmrp_MA4, data = train)
summary(model_11) #Multiple R-squared:  0.649,	Adjusted R-squared:  0.536 

sort(vif(model_11) )

#Other has high p-value

model_12 <- lm(formula = total_gmv ~ avg_prod_proc_sla + 
                 inc_discount_per_MA3 + earlyDelivery_orders + 
                 TV + Sponsorship + 
                 Affiliates + NPS + npromotion_in_week + week_has_promotion + 
                 inc_discount_per_MA2 + 
                 inc_avg_productmrp_MA4, data = train)
summary(model_12) #Multiple R-squared:  0.642,	Adjusted R-squared:  0.539

sort(vif(model_12) )

#avg_prod_proc_sla has high p-value

model_13 <- lm(formula = total_gmv ~ inc_discount_per_MA3 + earlyDelivery_orders + 
                 TV + Sponsorship + 
                 Affiliates + NPS + npromotion_in_week + week_has_promotion + 
                 inc_discount_per_MA2 + 
                 inc_avg_productmrp_MA4, data = train)
summary(model_13) #Multiple R-squared:  0.637,	Adjusted R-squared:  0.544 

sort(vif(model_13) )


#earlyDelivery_orders has high p-value

model_14 <- lm(formula = total_gmv ~ inc_discount_per_MA3 + 
                 TV + Sponsorship + 
                 Affiliates + NPS + npromotion_in_week + week_has_promotion + 
                 inc_discount_per_MA2 + 
                 inc_avg_productmrp_MA4, data = train)
summary(model_14) #Multiple R-squared:  0.635,	Adjusted R-squared:  0.553

sort(vif(model_14) )

#TV has high p-value

model_15 <- lm(formula = total_gmv ~ inc_discount_per_MA3 + 
                 Sponsorship + 
                 Affiliates + NPS + npromotion_in_week + week_has_promotion + 
                 inc_discount_per_MA2 + 
                 inc_avg_productmrp_MA4, data = train)
summary(model_15) #Multiple R-squared:  0.626,	Adjusted R-squared:  0.553 

sort(vif(model_15) )

#inc_discount_per_MA2 has high p-value

model_16 <- lm(formula = total_gmv ~ inc_discount_per_MA3 + 
                 Sponsorship + 
                 Affiliates + NPS + npromotion_in_week + week_has_promotion + 
                 inc_avg_productmrp_MA4, data = train)
summary(model_16) #Multiple R-squared:  0.59,	Adjusted R-squared:  0.522 

sort(vif(model_16))


#NPS has high p-value

model_17 <- lm(formula = total_gmv ~ inc_discount_per_MA3 + 
                 Sponsorship + 
                 Affiliates + npromotion_in_week + week_has_promotion + 
                 inc_avg_productmrp_MA4, data = train)
summary(model_17) #Multiple R-squared:  0.559,	Adjusted R-squared:  0.497  

sort(vif(model_17))


#inc_discount_per_MA3 has high p-value

model_18 <- lm(formula = total_gmv ~ Sponsorship + 
                 Affiliates + npromotion_in_week + week_has_promotion + 
                 inc_avg_productmrp_MA4, data = train)
summary(model_18) #Multiple R-squared:  0.541,	Adjusted R-squared:  0.488  

sort(vif(model_18))

#inc_avg_productmrp_MA4 has high p-value

model_19 <- lm(formula = total_gmv ~ Sponsorship + 
                 Affiliates + npromotion_in_week + week_has_promotion, data = train)
summary(model_19) #Multiple R-squared:  0.506,	Adjusted R-squared:  0.465  

sort(vif(model_19))


#npromotion_in_week has high VIF and p value

model_20 <- lm(formula = total_gmv ~ Sponsorship + 
                 Affiliates + week_has_promotion, data = train)
summary(model_20) #Multiple R-squared:  0.444,	Adjusted R-squared:  0.41  

sort(vif(model_20))

#week_has_promotion has high p value

model_21 <- lm(formula = total_gmv ~ Sponsorship + 
                 Affiliates, data = train)
summary(model_21) #Multiple R-squared:  0.432,	Adjusted R-squared:  0.409 

sort(vif(model_21))

final_model <- model_21

# AIC of generated model
AIC(final_model) # 127

##final model
#Coefficients:
#              Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 1.80e-16   1.06e-01    0.00     1.00    
#Sponsorship 1.68e-01   1.20e-01    1.40     0.17    
#Affiliates  5.62e-01   1.20e-01    4.67  2.3e-05 ***



#####cross validation ################################

train_control <- trainControl(method="cv", number=10)
# Fit Naive Bayes Model
model <- train(total_gmv ~ Sponsorship + 
                 Affiliates, data=train, trControl=train_control, method="lm")
# Summarise Results
print(model) # R2 0.515
#  RMSE   Rsquared  MAE  
#0.728  0.515     0.556

DAAG::cv.lm(data = train, form.lm = formula(total_gmv ~ Sponsorship + 
                                              Affiliates),
            m = 5, dots = FALSE, seed = 29, plotit = c("Observed","Residual"),
            main="Small symbols show cross-validation predicted values",
            legend.pos="topleft", printit = TRUE)
#Sum of squares = 2.6    Mean square = 0.26    n = 10 

#Overall (Sum over all 10 folds) 
#ms 
#0.665

############################ Elasticity ###############################
elast <- calcElasticity(final_model,train,0)
elast

#     variable elasticity direction
#1 Sponsorship      0.399  Positive
#2  Affiliates     -4.359  Negative

##plot the elasticity

plot5 <- plotElasticity(elast,"Camera Accessory - Koyck Mode")


############################# ploting Elasticity of all models together ###############
p <- plot_grid(plot1,plot2,plot3,plot4,plot5, align = "v",ncol = 2)
title <- ggdraw() + draw_label("Camera Accessory Model Elasticity", fontface='bold')
plot_grid(title, p, ncol=1, rel_heights=c(0.1, 1))

plotElasticity_grid(plot1,plot2,plot3,plot4,plot5)
