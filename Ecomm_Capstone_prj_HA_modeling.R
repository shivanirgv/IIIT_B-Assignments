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
install.packages("data.table")

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

master_ha_final.data <- read.csv('data\\master_ha_final.csv', header = T, stringsAsFactors = F)

######################### Adding Moving Average Variables ############################################################
#Calculating increase in MA variables
master_ha_final_with_ma.data <- master_ha_final.data
master_ha_final_with_ma.data <- cbind(master_ha_final_with_ma.data,calcIncrInMAVariable('discount_per',master_ha_final_with_ma.data))
master_ha_final_with_ma.data <- cbind(master_ha_final_with_ma.data,calcIncrInMAVariable('avg_productmrp',master_ha_final_with_ma.data))


############################# 1. LINEAR MODEL  ################################
# As data points are few, hence cross validation to be used. 
# Entire data frame used for training.
train = master_ha_final_with_ma.data[,-which(names(master_ha_final_with_ma.data) %in% c("Week","tot_productmrp","norders","nunits","repeated_orders","value_per_visitor"))]
train <- as.data.frame(scale(train))
# Build model 1 containing all variables
model_1 <-lm(total_gmv~.,data=train)
summary(model_1) # Multiple R-squared:  0.856,	Adjusted R-squared:  0.644
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

model_2 <- lm(formula = total_gmv ~ avg_sla + avg_productmrp + avg_prod_proc_sla + 
                earlyDelivery_orders + onTimeDelivery_orders + Total_Investment + 
                TV + Digital + Sponsorship + Content_Marketing + Online_Marketing + 
                Affiliates + Radio + Other + NPS + npromotion_in_week + inc_discount_per_MA2 + 
                inc_discount_per_MA3 + inc_avg_productmrp_MA2 + inc_avg_productmrp_MA3 + 
                inc_avg_productmrp_MA4, data = train)

summary(model_2) # Multiple R-squared:  0.836,	Adjusted R-squared:  0.703 
sort(vif(model_2))

# remove npromotion_in_week : very high P value
model_3 <- lm(formula = total_gmv ~ avg_sla + avg_productmrp + avg_prod_proc_sla + 
                earlyDelivery_orders + onTimeDelivery_orders + Total_Investment + 
                TV + Digital + Sponsorship + Content_Marketing + Online_Marketing + 
                Affiliates + Radio + Other + NPS + inc_discount_per_MA2 + 
                inc_discount_per_MA3 + inc_avg_productmrp_MA2 + inc_avg_productmrp_MA3 + 
                inc_avg_productmrp_MA4, data = train)

summary(model_3) # Multiple R-squared:  0.826,	Adjusted R-squared:  0.696
sort(vif(model_3))

# remove Content_Marketing : high p value
model_4 <- lm(formula = total_gmv ~ avg_sla + avg_productmrp + avg_prod_proc_sla + 
                earlyDelivery_orders + onTimeDelivery_orders + Total_Investment + 
                TV + Digital + Sponsorship + Online_Marketing + 
                Affiliates + Radio + Other + NPS + inc_discount_per_MA2 + 
                inc_discount_per_MA3 + inc_avg_productmrp_MA2 + inc_avg_productmrp_MA3 + 
                inc_avg_productmrp_MA4, data = train)

summary(model_4) # Multiple R-squared:  0.81,	Adjusted R-squared:  0.681
sort(vif(model_4))

# remove NPS : high p value
model_5 <- lm(formula = total_gmv ~ avg_sla + avg_productmrp + avg_prod_proc_sla + 
                earlyDelivery_orders + onTimeDelivery_orders + Total_Investment + 
                TV + Digital + Sponsorship + Online_Marketing + 
                Affiliates + Radio + Other + inc_discount_per_MA2 + 
                inc_discount_per_MA3 + inc_avg_productmrp_MA2 + inc_avg_productmrp_MA3 + 
                inc_avg_productmrp_MA4, data = train)

summary(model_5) # Multiple R-squared:  0.796,	Adjusted R-squared:  0.67
sort(vif(model_5))

# remove avg_sla : high VIF, p value
model_6 <- lm(formula = total_gmv ~ avg_productmrp + avg_prod_proc_sla + 
                earlyDelivery_orders + onTimeDelivery_orders + Total_Investment + 
                TV + Digital + Sponsorship + Online_Marketing + 
                Affiliates + Radio + Other + inc_discount_per_MA2 + 
                inc_discount_per_MA3 + inc_avg_productmrp_MA2 + inc_avg_productmrp_MA3 + 
                inc_avg_productmrp_MA4, data = train)

summary(model_6) # Multiple R-squared:  0.781,	Adjusted R-squared:  0.656
sort(vif(model_6))

# remove Online_Marketing : high VIF,P value
model_7 <- lm(formula = total_gmv ~ avg_productmrp + avg_prod_proc_sla + 
                earlyDelivery_orders + onTimeDelivery_orders + Total_Investment + 
                TV + Digital + Sponsorship +  
                Affiliates + Radio + Other + inc_discount_per_MA2 + 
                inc_discount_per_MA3 + inc_avg_productmrp_MA2 + inc_avg_productmrp_MA3 + 
                inc_avg_productmrp_MA4, data = train)

summary(model_7) # Multiple R-squared:  0.769,	Adjusted R-squared:  0.65
sort(vif(model_7))

# remove Other : high VIF, p value
model_8 <- lm(formula = total_gmv ~ avg_productmrp + avg_prod_proc_sla + 
                earlyDelivery_orders + onTimeDelivery_orders + Total_Investment + 
                TV + Digital + Sponsorship +  
                Affiliates + Radio + inc_discount_per_MA2 + 
                inc_discount_per_MA3 + inc_avg_productmrp_MA2 + inc_avg_productmrp_MA3 + 
                inc_avg_productmrp_MA4, data = train)

summary(model_8) # Multiple R-squared:  0.757,	Adjusted R-squared:  0.644
sort(vif(model_8))

# remove inc_avg_productmrp_MA4 : high VIF, p value
model_9 <- lm(formula = total_gmv ~ avg_productmrp + avg_prod_proc_sla + 
                earlyDelivery_orders + onTimeDelivery_orders + Total_Investment + 
                TV + Digital + Sponsorship +  
                Affiliates + Radio + inc_discount_per_MA2 + 
                inc_discount_per_MA3 + inc_avg_productmrp_MA2 + inc_avg_productmrp_MA3, data = train)

summary(model_9) # Multiple R-squared:  0.734,	Adjusted R-squared:  0.625 
sort(vif(model_9))

# remove inc_discount_per_MA2 : high VIF
model_10 <- lm(formula = total_gmv ~ avg_productmrp + avg_prod_proc_sla + 
                 earlyDelivery_orders + onTimeDelivery_orders + Total_Investment + 
                 TV + Digital + Sponsorship +  
                 Affiliates + Radio +  
                 inc_discount_per_MA3 + inc_avg_productmrp_MA2 + inc_avg_productmrp_MA3, data = train)

summary(model_10) # Multiple R-squared:  0.718,	Adjusted R-squared:  0.613
sort(vif(model_10))


# remove inc_discount_per_MA3 : high VIF,p value
model_11 <- lm(formula = total_gmv ~ avg_productmrp + avg_prod_proc_sla + 
                 earlyDelivery_orders + onTimeDelivery_orders + Total_Investment + 
                 TV + Digital + Sponsorship +  
                 Affiliates + Radio +  
                 inc_avg_productmrp_MA2 + inc_avg_productmrp_MA3, data = train)

summary(model_11) #Multiple R-squared:  0.707,	Adjusted R-squared:  0.609
sort(vif(model_11))

# remove inc_avg_productmrp_MA2 : high VIF, p value
model_12 <- lm(formula = total_gmv ~ avg_productmrp + avg_prod_proc_sla + 
                 earlyDelivery_orders + onTimeDelivery_orders + Total_Investment + 
                 TV + Digital + Sponsorship +  
                 Affiliates + Radio +  
                 inc_avg_productmrp_MA3, data = train)

summary(model_12) #Multiple R-squared:  0.691,	Adjusted R-squared:  0.599
sort(vif(model_12))

# remove inc_avg_productmrp_MA3 : high VIF,p value
model_13 <- lm(formula = total_gmv ~ avg_productmrp + avg_prod_proc_sla + 
                 earlyDelivery_orders + onTimeDelivery_orders + Total_Investment + 
                 TV + Digital + Sponsorship +  
                 Affiliates + Radio, data = train)

summary(model_13) #Multiple R-squared:  0.648,	Adjusted R-squared:  0.56 
sort(vif(model_13))

# remove Total_Investment : high p value
model_14 <- lm(formula = total_gmv ~ avg_productmrp + avg_prod_proc_sla + 
                 earlyDelivery_orders + onTimeDelivery_orders +  
                 TV + Digital + Sponsorship +  
                 Affiliates + Radio, data = train)

summary(model_14) #Multiple R-squared:  0.602,	Adjusted R-squared:  0.515 
sort(vif(model_14))

# remove Affiliates : high VIF,p value
model_15 <- lm(formula = total_gmv ~ avg_productmrp + avg_prod_proc_sla + 
                 earlyDelivery_orders + onTimeDelivery_orders +  
                 TV + Digital + Sponsorship +  
                 Radio, data = train)

summary(model_15) #Multiple R-squared:  0.598,	Adjusted R-squared:  0.522 
sort(vif(model_15))


# remove Sponsorship : high VIF,p value
model_16 <- lm(formula = total_gmv ~ avg_productmrp + avg_prod_proc_sla + 
                 earlyDelivery_orders + onTimeDelivery_orders +  
                 TV + Digital +   
                 Radio, data = train)

summary(model_16) #Multiple R-squared:  0.595,	Adjusted R-squared:  0.529 
sort(vif(model_16))

# remove TV : high p value
model_17 <- lm(formula = total_gmv ~ avg_productmrp + avg_prod_proc_sla + 
                 earlyDelivery_orders + onTimeDelivery_orders +  
                 Digital +   
                 Radio, data = train)

summary(model_17) #Multiple R-squared:  0.589,	Adjusted R-squared:  0.532 
sort(vif(model_17))

#avg_prod_proc_sla high P value
model_18 <- lm(formula = total_gmv ~ avg_productmrp +  
                 earlyDelivery_orders + onTimeDelivery_orders +  
                 Digital +   
                 Radio, data = train)

summary(model_18) #Multiple R-squared:  0.555,	Adjusted R-squared:  0.506
sort(vif(model_18))

#Radio: high P value
model_19 <- lm(formula = total_gmv ~ avg_productmrp +  
                 earlyDelivery_orders + onTimeDelivery_orders +  
                 Digital,data = train)

summary(model_19) #Multiple R-squared:  0.525,	Adjusted R-squared:  0.483
sort(vif(model_19))

#earlyDelivery_orders high P value
model_20 <- lm(formula = total_gmv ~ avg_productmrp +  
                 onTimeDelivery_orders +  
                 Digital,data = train)

summary(model_20) #Multiple R-squared:  0.495,	Adjusted R-squared:  0.463 
sort(vif(model_20))

#onTimeDelivery_orders high P value
model_21 <- lm(formula = total_gmv ~ avg_productmrp +  
                 Digital,data = train)

summary(model_21) #Multiple R-squared:  0.45,	Adjusted R-squared:  0.427 
sort(vif(model_21))


final_model <- model_21
AIC(final_model) #121.2116

# Final model
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)    1.68e-16   1.06e-01    0.00        1    
#avg_productmrp 4.96e-01   1.07e-01    4.63  2.8e-05 ***
#  Digital        4.73e-01   1.07e-01    4.41  5.7e-05 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 0.757 on 48 degrees of freedom
#Multiple R-squared:  0.45,	Adjusted R-squared:  0.427 
#F-statistic: 19.7 on 2 and 48 DF,  p-value: 5.81e-07


############################ Test accuracy #############################
##################### Cross validation 

################################

train_control <- trainControl(method="cv", number=5)
train <- na.omit(train)
# Fit Naive Bayes Model
model <- train(total_gmv ~ avg_productmrp + Digital,data=train, trControl=train_control, method="lm")
# Summarise Results
print(model) 
# RMSE       Rsquared   MAE      
# 0.7463  0.526    0.5455

DAAG::cv.lm(data = train, form.lm = formula(total_gmv ~ avg_productmrp + Digital),
            m = 5, dots = FALSE, seed = 29, plotit = c("Observed","Residual"),
            main="Small symbols show cross-validation predicted values",
            legend.pos="topleft", printit = TRUE)
# overall ms 0.73

############################ Elasticity ###############################
elast <- calcElasticity(final_model,train,0)
elast

#variable elasticity direction
#1 avg_productmrp      0.275  Positive
#2        Digital      0.587  Positive 

##plot the elasticity
plot1 <- plotElasticity(elast,"HOme Audio - Linear Model")
plot1

############################# 2. Multiplicative MODEL ################################

# As data points are few, hence cross validation to be used. 
# Entire data frame used for training.
train = master_ha_final.data[,-which(names(master_ha_final.data) %in% c("Week","tot_productmrp","norders","nunits","repeated_orders","value_per_visitor"))]
train <- as.data.frame(sapply(train,function(x) ifelse(x==0,0.0001,x)))
train <- as.data.frame(sapply(train,function(x) log(x)))
train <- as.data.frame(scale(train))
sum(is.na(train))
# Build model 1 containing all variables
model_1 <-lm(total_gmv~.,data=train)
summary(model_1) # Multiple R-squared:  0.767,	Adjusted R-squared:  0.585
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

model_2 <- lm(formula = total_gmv ~ discount_per + earlyDelivery_orders + 
                onTimeDelivery_orders + Total_Investment + Digital + Content_Marketing + 
                Online_Marketing + Affiliates + Radio, data = train)

summary(model_2) # Multiple R-squared:  0.74,	Adjusted R-squared:  0.683
sort(vif(model_2))

# remove Online_Marketing : high VIF, p value
model_3 <- lm(formula = total_gmv ~ discount_per + earlyDelivery_orders + 
                onTimeDelivery_orders + Total_Investment + Digital + Content_Marketing + 
                Affiliates + Radio, data = train)

summary(model_3) # Multiple R-squared:  0.698,	Adjusted R-squared:  0.64  
sort(vif(model_3))


# remove Affiliates: high VIF, mod p value
model_4 <- lm(formula = total_gmv ~ discount_per + earlyDelivery_orders + 
                onTimeDelivery_orders + Total_Investment + Digital + Content_Marketing + 
                Radio, data = train)

summary(model_4) # Multiple R-squared:  0.689,	Adjusted R-squared:  0.638  
sort(vif(model_4))

# remove Digital : high p value
model_5 <- lm(formula = total_gmv ~ discount_per + earlyDelivery_orders + 
                onTimeDelivery_orders + Total_Investment + Content_Marketing + 
                Radio, data = train)

summary(model_5) # Multiple R-squared:  0.688,	Adjusted R-squared:  0.645
sort(vif(model_5))

# remove Content_Marketing: high VIF value
model_6 <- lm(formula = total_gmv ~ discount_per + earlyDelivery_orders + 
                onTimeDelivery_orders + Total_Investment +  
                Radio, data = train)

summary(model_6) # Multiple R-squared:  0.497,	Adjusted R-squared:  0.441 
sort(vif(model_6))

# remove discount_per: high P value
model_7 <- lm(formula = total_gmv ~ earlyDelivery_orders + 
                onTimeDelivery_orders + Total_Investment +  
                Radio, data = train)

summary(model_7) # Multiple R-squared:  0.485,	Adjusted R-squared:  0.44 
sort(vif(model_7))

# remove Radio: high P value
model_8 <- lm(formula = total_gmv ~ earlyDelivery_orders + 
                onTimeDelivery_orders + Total_Investment, data = train)

summary(model_8) # Multiple R-squared:  0.446,	Adjusted R-squared:  0.41
sort(vif(model_8))

final_model <- model_8
AIC(final_model) # 124

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)           -3.08e-17   1.08e-01    0.00   1.0000    
#earlyDelivery_orders   6.95e-01   1.72e-01    4.04   0.0002 ***
#  onTimeDelivery_orders -5.73e-01   1.70e-01   -3.36   0.0016 ** 
#  Total_Investment       5.53e-01   1.15e-01    4.81  1.6e-05 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 0.768 on 47 degrees of freedom
#Multiple R-squared:  0.446,	Adjusted R-squared:  0.41 
#F-statistic: 12.6 on 3 and 47 DF,  p-value: 3.63e-06

########## Test accuracy #######
### Cross validation 

################################

train_control <- trainControl(method="cv", number=5)

train <- na.omit(train)
# Fit Naive Bayes Model
model <- train(total_gmv ~ earlyDelivery_orders + 
                 onTimeDelivery_orders + Total_Investment 
               , data=train, trControl=train_control, method="lm")
# Summarise Results
print(model) 
# RMSE       Rsquared   MAE      
# 0.793  0.45    0.593

DAAG::cv.lm(data = train, form.lm = formula(total_gmv ~ earlyDelivery_orders + 
                                              onTimeDelivery_orders + Total_Investment),
            m = 5, dots = FALSE, seed = 29, plotit = c("Observed","Residual"),
            main="Small symbols show cross-validation predicted values",
            legend.pos="topleft", printit = TRUE)
# overall ms 0.75

############################ Elasticity ###############################
elast <- calcElasticity(final_model,master_ha_final.data,1)
elast

# variable elasticity direction
# 1  earlyDelivery_orders      0.695  Positive
# 2 onTimeDelivery_orders     -0.573  Negative
# 3      Total_Investment      0.553  Positive


##plot the elasticity
plot2 <- plotElasticity(elast,"HOme Audio - Multiplicative Model")
plot2

###########################################################################################
################################Koyck Model ###############################################
###########################################################################################

train <- master_ha_final_with_ma.data[,-which(names(master_ha_final_with_ma.data) %in% c("Week","tot_productmrp","norders","nunits","repeated_orders","value_per_visitor"))]

#take the lag of the total gmv
train <- slide(train, Var = "total_gmv", slideBy = -1)
train <- as.data.frame(scale(train))

model_1 <- lm(total_gmv ~ ., data=train)
summary(model_1) #Multiple R-squared:  0.91,	Adjusted R-squared:  0.765

step <- stepAIC(model_1, direction = "both")
step

model_2 <- lm(formula = total_gmv ~ avg_sla + avg_productmrp + avg_prod_proc_sla + 
                discount_per + cod_orders + earlyDelivery_orders + onTimeDelivery_orders + 
                Total_Investment + TV + Digital + Sponsorship + Online_Marketing + 
                Affiliates + SEM + Radio + NPS + npromotion_in_week + week_has_promotion + 
                inc_discount_per_MA2 + inc_discount_per_MA4 + inc_avg_productmrp_MA2 + 
                inc_avg_productmrp_MA4 + `total_gmv-1`, data = train)
summary(model_2) #Multiple R-squared:   0.9,	Adjusted R-squared:  0.804 
sort(vif(model_2))

#discount_per: high P value
model_3 <- lm(formula = total_gmv ~ avg_sla + avg_productmrp + avg_prod_proc_sla + 
                cod_orders + earlyDelivery_orders + onTimeDelivery_orders + 
                Total_Investment + TV + Digital + Sponsorship + Online_Marketing + 
                Affiliates + SEM + Radio + NPS + npromotion_in_week + week_has_promotion + 
                inc_discount_per_MA2 + inc_discount_per_MA4 + inc_avg_productmrp_MA2 + 
                inc_avg_productmrp_MA4 + `total_gmv-1`, data = train)
summary(model_3) #Multiple R-squared:  0.894,	Adjusted R-squared:  0.802 
sort(vif(model_3))

#cod_orders: High P value
model_4 <- lm(formula = total_gmv ~ avg_sla + avg_productmrp + avg_prod_proc_sla + 
                earlyDelivery_orders + onTimeDelivery_orders + 
                Total_Investment + TV + Digital + Sponsorship + Online_Marketing + 
                Affiliates + SEM + Radio + NPS + npromotion_in_week + week_has_promotion + 
                inc_discount_per_MA2 + inc_discount_per_MA4 + inc_avg_productmrp_MA2 + 
                inc_avg_productmrp_MA4 + `total_gmv-1`, data = train)
summary(model_4) #Multiple R-squared:  0.889,	Adjusted R-squared:   0.8  
sort(vif(model_4))

#NPS : High P value and VIF
model_5 <- lm(formula = total_gmv ~ avg_sla + avg_productmrp + avg_prod_proc_sla + 
                earlyDelivery_orders + onTimeDelivery_orders + 
                Total_Investment + TV + Digital + Sponsorship + Online_Marketing + 
                Affiliates + SEM + Radio + npromotion_in_week + week_has_promotion + 
                inc_discount_per_MA2 + inc_discount_per_MA4 + inc_avg_productmrp_MA2 + 
                inc_avg_productmrp_MA4 + `total_gmv-1`, data = train)
summary(model_5) #Multiple R-squared:  0.887,	Adjusted R-squared:  0.803  
sort(vif(model_5))

#onTimeDelivery_orders: high p value
model_6 <- lm(formula = total_gmv ~ avg_sla + avg_productmrp + avg_prod_proc_sla + 
                earlyDelivery_orders + 
                Total_Investment + TV + Digital + Sponsorship + Online_Marketing + 
                Affiliates + SEM + Radio + npromotion_in_week + week_has_promotion + 
                inc_discount_per_MA2 + inc_discount_per_MA4 + inc_avg_productmrp_MA2 + 
                inc_avg_productmrp_MA4 + `total_gmv-1`, data = train)
summary(model_6) #Multiple R-squared:  0.88,	Adjusted R-squared:  0.798  
sort(vif(model_6))

#week_has_promotion: high p value
model_7 <- lm(formula = total_gmv ~ avg_sla + avg_productmrp + avg_prod_proc_sla + 
                earlyDelivery_orders + 
                Total_Investment + TV + Digital + Sponsorship + Online_Marketing + 
                Affiliates + SEM + Radio + npromotion_in_week +  
                inc_discount_per_MA2 + inc_discount_per_MA4 + inc_avg_productmrp_MA2 + 
                inc_avg_productmrp_MA4 + `total_gmv-1`, data = train)
summary(model_7) #Multiple R-squared:  0.871,	Adjusted R-squared:  0.79  
sort(vif(model_7))

#npromotion_in_week: high p value
model_8 <- lm(formula = total_gmv ~ avg_sla + avg_productmrp + avg_prod_proc_sla + 
                earlyDelivery_orders + 
                Total_Investment + TV + Digital + Sponsorship + Online_Marketing + 
                Affiliates + SEM + Radio +   
                inc_discount_per_MA2 + inc_discount_per_MA4 + inc_avg_productmrp_MA2 + 
                inc_avg_productmrp_MA4 + `total_gmv-1`, data = train)
summary(model_8) #Multiple R-squared:  0.862,	Adjusted R-squared:  0.784  
sort(vif(model_8))

#earlyDelivery_orders: high p value
model_9 <- lm(formula = total_gmv ~ avg_sla + avg_productmrp + avg_prod_proc_sla + 
                Total_Investment + TV + Digital + Sponsorship + Online_Marketing + 
                Affiliates + SEM + Radio +   
                inc_discount_per_MA2 + inc_discount_per_MA4 + inc_avg_productmrp_MA2 + 
                inc_avg_productmrp_MA4 + `total_gmv-1`, data = train)
summary(model_9) #Multiple R-squared:  0.845,	Adjusted R-squared:  0.765   
sort(vif(model_9))

#inc_discount_per_MA2: high VIF,p value
model_10 <- lm(formula = total_gmv ~ avg_sla + avg_productmrp + avg_prod_proc_sla + 
                 Total_Investment + TV + Digital + Sponsorship + Online_Marketing + 
                 Affiliates + SEM + Radio +   
                 inc_discount_per_MA4 + inc_avg_productmrp_MA2 + 
                 inc_avg_productmrp_MA4 + `total_gmv-1`, data = train)
summary(model_10) #Multiple R-squared:  0.812,	Adjusted R-squared:  0.724    
sort(vif(model_10))


#inc_avg_productmrp_MA2: high VIF,p value
model_11 <- lm(formula = total_gmv ~ avg_sla + avg_productmrp + avg_prod_proc_sla + 
                 Total_Investment + TV + Digital + Sponsorship + Online_Marketing + 
                 Affiliates + SEM + Radio +   
                 inc_discount_per_MA4 +  
                 inc_avg_productmrp_MA4 + `total_gmv-1`, data = train)
summary(model_11) #Multiple R-squared:  0.799,	Adjusted R-squared:  0.713    
sort(vif(model_11))

#Online_Marketing: high VIF, P value
model_12 <- lm(formula = total_gmv ~ avg_sla + avg_productmrp + avg_prod_proc_sla + 
                 Total_Investment + TV + Digital + Sponsorship +  
                 Affiliates + SEM + Radio +   
                 inc_discount_per_MA4 +  
                 inc_avg_productmrp_MA4 + `total_gmv-1`, data = train)
summary(model_12) #Multiple R-squared:  0.758,	Adjusted R-squared:  0.665
sort(vif(model_12))

#TV: high P value
model_13 <- lm(formula = total_gmv ~ avg_sla + avg_productmrp + avg_prod_proc_sla + 
                 Total_Investment + Digital + Sponsorship +  
                 Affiliates + SEM + Radio +   
                 inc_discount_per_MA4 +  
                 inc_avg_productmrp_MA4 + `total_gmv-1`, data = train)
summary(model_13) #Multiple R-squared:  0.735,	Adjusted R-squared:  0.644 
sort(vif(model_13))

#inc_avg_productmrp_MA4: high VIF and P value
model_14 <- lm(formula = total_gmv ~ avg_sla + avg_productmrp + avg_prod_proc_sla + 
                 Total_Investment + Digital + Sponsorship +  
                 Affiliates + SEM + Radio +   
                 inc_discount_per_MA4 +  
                 `total_gmv-1`, data = train)
summary(model_14) #Multiple R-squared:  0.722,	Adjusted R-squared:  0.638 
sort(vif(model_14))

#inc_discount_per_MA4: high P value
model_15 <- lm(formula = total_gmv ~ avg_sla + avg_productmrp + avg_prod_proc_sla + 
                 Total_Investment + Digital + Sponsorship +  
                 Affiliates + SEM + Radio +   
                 `total_gmv-1`, data = train)
summary(model_15) #Multiple R-squared:  0.661,	Adjusted R-squared:  0.575 
sort(vif(model_15))

#Total_Investment: High VIF and p value
model_16 <- lm(formula = total_gmv ~ avg_sla + avg_productmrp + avg_prod_proc_sla + 
                 Digital + Sponsorship +  
                 Affiliates + SEM + Radio +   
                 `total_gmv-1`, data = train)
summary(model_16) #Multiple R-squared:  0.598,	Adjusted R-squared:  0.508 
sort(vif(model_16))

#Sponsorship: High VIF and p value
model_17 <- lm(formula = total_gmv ~ avg_sla + avg_productmrp + avg_prod_proc_sla + 
                 Digital +   
                 Affiliates + SEM + Radio +   
                 `total_gmv-1`, data = train)
summary(model_17) #Multiple R-squared:  0.578,	Adjusted R-squared:  0.496  
sort(vif(model_17))

#SEM: High VIF and p value
model_18 <- lm(formula = total_gmv ~ avg_sla + avg_productmrp + avg_prod_proc_sla + 
                 Digital +   
                 Affiliates + Radio +   
                 `total_gmv-1`, data = train)
summary(model_18) #Multiple R-squared:  0.556,	Adjusted R-squared:  0.482  
sort(vif(model_18))

#Affiliates: High VIF and P value
model_19 <- lm(formula = total_gmv ~ avg_sla + avg_productmrp + avg_prod_proc_sla + 
                 Digital +   
                 Radio +   
                 `total_gmv-1`, data = train)
summary(model_19) #Multiple R-squared:  0.539,	Adjusted R-squared:  0.475
sort(vif(model_19))

#avg_sla: high P value
model_20 <- lm(formula = total_gmv ~ avg_productmrp + avg_prod_proc_sla + 
                 Digital +   
                 Radio +   
                 `total_gmv-1`, data = train)
summary(model_20) #Multiple R-squared:  0.53,	Adjusted R-squared:  0.476
sort(vif(model_20))

#avg_prod_proc_sla: high P value
model_21 <- lm(formula = total_gmv ~ avg_productmrp +  
                 Digital +   
                 Radio +   
                 `total_gmv-1`, data = train)
summary(model_21) #Multiple R-squared:  0.517,	Adjusted R-squared:  0.474 
sort(vif(model_21))

#total_gmv-1 : high P value
model_22 <- lm(formula = total_gmv ~ avg_productmrp +  
                 Digital +   
                 Radio, data = train)
summary(model_22) #Multiple R-squared:  0.482,	Adjusted R-squared:  0.449  
sort(vif(model_22))

#Radio: high P value
model_23 <- lm(formula = total_gmv ~ avg_productmrp +  
                 Digital, data = train)
summary(model_23) #Multiple R-squared:  0.45,	Adjusted R-squared:  0.427   
sort(vif(model_23))

final_model <- model_23
AIC(final_model) #121

####################################cross validation ######################################

train_control <- trainControl(method="cv", number=5)
train <- na.omit(train)
# Fit Naive Bayes Model
model <- train(total_gmv ~  avg_productmrp + Digital, data=train, trControl=train_control, method="lm")
# Summarise Results
print(model) 
#RMSE   Rsquared  MAE
#0.773  0.504     0.549

DAAG::cv.lm(data = train, form.lm = formula(total_gmv ~  avg_productmrp + Digital),
            m = 5, dots = FALSE, seed = 29, plotit = c("Observed","Residual"),
            main="Small symbols show cross-validation predicted values",
            legend.pos="topleft", printit = TRUE)

# overall ms 0.73

############################ Elasticity ###############################
elast <- calcElasticity(final_model,train,0)
elast

#variable elasticity direction
#1 avg_productmrp      0.275  Positive
#2        Digital      0.587  Positive

##plot the elasticity
plot3 <- plotElasticity(elast,"HOme Audio - Koyck Model")
plot3

##########################################################################################
###############################4.Distributed Lag Model#######################################
##########################################################################################

train = master_ha_final_with_ma.data[,-which(names(master_ha_final_with_ma.data) %in% c("Week","tot_productmrp","norders","nunits","repeated_orders","value_per_visitor"))]
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


model_1 <- lm(total_gmv ~.,data=train)
summary(model_1) #Multiple R-squared:  0.969,	Adjusted R-squared:  0.855

step <- stepAIC(model_1,direction="both")
step

model_2 <- lm(formula = total_gmv ~ avg_sla + avg_productmrp + avg_prod_proc_sla + 
                discount_per + earlyDelivery_orders + onTimeDelivery_orders + 
                Total_Investment + TV + Sponsorship + Content_Marketing + 
                Online_Marketing + Affiliates + SEM + Radio + Other + NPS + 
                npromotion_in_week + week_has_promotion + inc_discount_per_MA2 + 
                inc_discount_per_MA4 + inc_avg_productmrp_MA2 + inc_avg_productmrp_MA4 + 
                total_gmv_1 + total_gmv_2 + total_gmv_3 + avg_productmrp_1 + 
                avg_productmrp_2 + avg_productmrp_3 + discount_per_1 + discount_per_2 + 
                discount_per_3, data = train)


summary(model_2) #Multiple R-squared:  0.966,	Adjusted R-squared:  0.901
sort(vif(model_2))

#`Content_Marketing` : high VIF and less significant
model_3 <- lm(formula = total_gmv ~ avg_sla + avg_productmrp + avg_prod_proc_sla + 
                discount_per + earlyDelivery_orders + onTimeDelivery_orders + 
                Total_Investment + TV + Sponsorship +  
                Online_Marketing + Affiliates + SEM + Radio + Other + NPS + 
                npromotion_in_week + week_has_promotion + inc_discount_per_MA2 + 
                inc_discount_per_MA4 + inc_avg_productmrp_MA2 + inc_avg_productmrp_MA4 + 
                total_gmv_1 + total_gmv_2 + total_gmv_3 + avg_productmrp_1 + 
                avg_productmrp_2 + avg_productmrp_3 + discount_per_1 + discount_per_2 + 
                discount_per_3, data = train)


summary(model_3) #Multiple R-squared:  0.947,	Adjusted R-squared:  0.853 
sort(vif(model_3))

#Affiliates: high VIF,P value
model_4 <- lm(formula = total_gmv ~ avg_sla + avg_productmrp + avg_prod_proc_sla + 
                discount_per + earlyDelivery_orders + onTimeDelivery_orders + 
                Total_Investment + TV + Sponsorship +  
                Online_Marketing + SEM + Radio + Other + NPS + 
                npromotion_in_week + week_has_promotion + inc_discount_per_MA2 + 
                inc_discount_per_MA4 + inc_avg_productmrp_MA2 + inc_avg_productmrp_MA4 + 
                total_gmv_1 + total_gmv_2 + total_gmv_3 + avg_productmrp_1 + 
                avg_productmrp_2 + avg_productmrp_3 + discount_per_1 + discount_per_2 + 
                discount_per_3, data = train)

summary(model_4) #Multiple R-squared:  0.927,	Adjusted R-squared:  0.811  
sort(vif(model_4))

#NPS : high  P value
model_5 <- lm(formula = total_gmv ~ avg_sla + avg_productmrp + avg_prod_proc_sla + 
                discount_per + earlyDelivery_orders + onTimeDelivery_orders + 
                Total_Investment + TV + Sponsorship +  
                Online_Marketing + SEM + Radio + Other +  
                npromotion_in_week + week_has_promotion + inc_discount_per_MA2 + 
                inc_discount_per_MA4 + inc_avg_productmrp_MA2 + inc_avg_productmrp_MA4 + 
                total_gmv_1 + total_gmv_2 + total_gmv_3 + avg_productmrp_1 + 
                avg_productmrp_2 + avg_productmrp_3 + discount_per_1 + discount_per_2 + 
                discount_per_3, data = train)

summary(model_5) #Multiple R-squared:  0.927,	Adjusted R-squared:  0.821  
sort(vif(model_5))

#Radio: high VIF P value
model_6 <- lm(formula = total_gmv ~ avg_sla + avg_productmrp + avg_prod_proc_sla + 
                discount_per + earlyDelivery_orders + onTimeDelivery_orders + 
                Total_Investment + TV + Sponsorship +  
                Online_Marketing + SEM + Other +  
                npromotion_in_week + week_has_promotion + inc_discount_per_MA2 + 
                inc_discount_per_MA4 + inc_avg_productmrp_MA2 + inc_avg_productmrp_MA4 + 
                total_gmv_1 + total_gmv_2 + total_gmv_3 + avg_productmrp_1 + 
                avg_productmrp_2 + avg_productmrp_3 + discount_per_1 + discount_per_2 + 
                discount_per_3, data = train)

summary(model_6) #Multiple R-squared:  0.879,	Adjusted R-squared:  0.715 
sort(vif(model_6))

#`total_gmv-3: high P value
model_7 <- lm(formula = total_gmv ~ avg_sla + avg_productmrp + avg_prod_proc_sla + 
                discount_per + earlyDelivery_orders + onTimeDelivery_orders + 
                Total_Investment + TV + Sponsorship +  
                Online_Marketing + SEM + Other +  
                npromotion_in_week + week_has_promotion + inc_discount_per_MA2 + 
                inc_discount_per_MA4 + inc_avg_productmrp_MA2 + inc_avg_productmrp_MA4 + 
                total_gmv_1 + total_gmv_2 +  avg_productmrp_1 + 
                avg_productmrp_2 + avg_productmrp_3 + discount_per_1 + discount_per_2 + 
                discount_per_3, data = train)

summary(model_7) #Multiple R-squared:  0.879,	Adjusted R-squared:  0.728
sort(vif(model_7))

#discount_per: high  P value
model_8 <- lm(formula = total_gmv ~ avg_sla + avg_productmrp + avg_prod_proc_sla + 
                earlyDelivery_orders + onTimeDelivery_orders + 
                Total_Investment + TV + Sponsorship +  
                Online_Marketing + SEM + Other +  
                npromotion_in_week + week_has_promotion + inc_discount_per_MA2 + 
                inc_discount_per_MA4 + inc_avg_productmrp_MA2 + inc_avg_productmrp_MA4 + 
                total_gmv_1 + total_gmv_2 +  avg_productmrp_1 + 
                avg_productmrp_2 + avg_productmrp_3 + discount_per_1 + discount_per_2 + 
                discount_per_3, data = train)



summary(model_8) #Multiple R-squared:  0.878,	Adjusted R-squared:  0.74  
sort(vif(model_8))

#avg_productmrp high P value
model_9 <- lm(formula = total_gmv ~ avg_sla + avg_prod_proc_sla + 
                earlyDelivery_orders + onTimeDelivery_orders + 
                Total_Investment + TV + Sponsorship +  
                Online_Marketing + SEM + Other +  
                npromotion_in_week + week_has_promotion + inc_discount_per_MA2 + 
                inc_discount_per_MA4 + inc_avg_productmrp_MA2 + inc_avg_productmrp_MA4 + 
                total_gmv_1 + total_gmv_2 +  avg_productmrp_1 + 
                avg_productmrp_2 + avg_productmrp_3 + discount_per_1 + discount_per_2 + 
                discount_per_3, data = train)

summary(model_9) #Multiple R-squared:  0.877,	Adjusted R-squared:  0.748  
sort(vif(model_9))

#`avg_sla` high P value
model_10 <- lm(formula = total_gmv ~ avg_prod_proc_sla + 
                 earlyDelivery_orders + onTimeDelivery_orders + 
                 Total_Investment + TV + Sponsorship +  
                 Online_Marketing + SEM + Other +  
                 npromotion_in_week + week_has_promotion + inc_discount_per_MA2 + 
                 inc_discount_per_MA4 + inc_avg_productmrp_MA2 + inc_avg_productmrp_MA4 + 
                 total_gmv_1 + total_gmv_2 +  avg_productmrp_1 + 
                 avg_productmrp_2 + avg_productmrp_3 + discount_per_1 + discount_per_2 + 
                 discount_per_3, data = train)


summary(model_10) #Multiple R-squared:  0.873,	Adjusted R-squared:  0.752  
sort(vif(model_10))


#avg_productmrp-2 high P value
model_11 <- lm(formula = total_gmv ~ avg_prod_proc_sla + 
                 earlyDelivery_orders + onTimeDelivery_orders + 
                 Total_Investment + TV + Sponsorship +  
                 Online_Marketing + SEM + Other +  
                 npromotion_in_week + week_has_promotion + inc_discount_per_MA2 + 
                 inc_discount_per_MA4 + inc_avg_productmrp_MA2 + inc_avg_productmrp_MA4 + 
                 total_gmv_1 + total_gmv_2 +  avg_productmrp_1 + 
                 avg_productmrp_3 + discount_per_1 + discount_per_2 + 
                 discount_per_3, data = train)

summary(model_11) #Multiple R-squared:  0.861,	Adjusted R-squared:  0.739 
sort(vif(model_11))

#discount_per-2 high P value
model_12 <- lm(formula = total_gmv ~ avg_prod_proc_sla + 
                 earlyDelivery_orders + onTimeDelivery_orders + 
                 Total_Investment + TV + Sponsorship +  
                 Online_Marketing + SEM + Other +  
                 npromotion_in_week + week_has_promotion + inc_discount_per_MA2 + 
                 inc_discount_per_MA4 + inc_avg_productmrp_MA2 + inc_avg_productmrp_MA4 + 
                 total_gmv_1 + total_gmv_2 +  avg_productmrp_1 + 
                 avg_productmrp_3 + discount_per_1 +  
                 discount_per_3, data = train)

summary(model_12) #Multiple R-squared:  0.85,	Adjusted R-squared:  0.728  
sort(vif(model_12))

#discount_per-3 high P value
model_13 <- lm(formula = total_gmv ~ avg_prod_proc_sla + 
                 earlyDelivery_orders + onTimeDelivery_orders + 
                 Total_Investment + TV + Sponsorship +  
                 Online_Marketing + SEM + Other +  
                 npromotion_in_week + week_has_promotion + inc_discount_per_MA2 + 
                 inc_discount_per_MA4 + inc_avg_productmrp_MA2 + inc_avg_productmrp_MA4 + 
                 total_gmv_1 + total_gmv_2 +  avg_productmrp_1 + 
                 avg_productmrp_3 + discount_per_1, data = train)

summary(model_13) #Multiple R-squared:  0.847,	Adjusted R-squared:  0.734    
sort(vif(model_13))

#avg_productmrp-3 high P value
model_14 <- lm(formula = total_gmv ~ avg_prod_proc_sla + 
                 earlyDelivery_orders + onTimeDelivery_orders + 
                 Total_Investment + TV + Sponsorship +  
                 Online_Marketing + SEM + Other +  
                 npromotion_in_week + week_has_promotion + inc_discount_per_MA2 + 
                 inc_discount_per_MA4 + inc_avg_productmrp_MA2 + inc_avg_productmrp_MA4 + 
                 total_gmv_1 + total_gmv_2 +  avg_productmrp_1 + 
                 discount_per_1, data = train)



summary(model_14) #Multiple R-squared:  0.842,	Adjusted R-squared:  0.736    
sort(vif(model_14))

#week_has_promotion high P value
model_15 <- lm(formula = total_gmv ~ avg_prod_proc_sla + 
                 earlyDelivery_orders + onTimeDelivery_orders + 
                 Total_Investment + TV + Sponsorship +  
                 Online_Marketing + SEM + Other +  
                 npromotion_in_week + inc_discount_per_MA2 + 
                 inc_discount_per_MA4 + inc_avg_productmrp_MA2 + inc_avg_productmrp_MA4 + 
                 total_gmv_1 + total_gmv_2 +  avg_productmrp_1 + 
                 discount_per_1, data = train)

summary(model_15) #Multiple R-squared:  0.831,	Adjusted R-squared:  0.726   
sort(vif(model_15))

#npromotion_in_week   high P value
model_16 <- lm(formula = total_gmv ~ avg_prod_proc_sla + 
                 earlyDelivery_orders + onTimeDelivery_orders + 
                 Total_Investment + TV + Sponsorship +  
                 Online_Marketing + SEM + Other +  
                 inc_discount_per_MA2 + 
                 inc_discount_per_MA4 + inc_avg_productmrp_MA2 + inc_avg_productmrp_MA4 + 
                 total_gmv_1 + total_gmv_2 +  avg_productmrp_1 + 
                 discount_per_1, data = train)


summary(model_16) #Multiple R-squared:  0.815,	Adjusted R-squared:  0.709     
sort(vif(model_16))

#total_gmv-2  high P value
model_17 <- lm(formula = total_gmv ~ avg_prod_proc_sla + 
                 earlyDelivery_orders + onTimeDelivery_orders + 
                 Total_Investment + TV + Sponsorship +  
                 Online_Marketing + SEM + Other +  
                 inc_discount_per_MA2 + 
                 inc_discount_per_MA4 + inc_avg_productmrp_MA2 + inc_avg_productmrp_MA4 + 
                 total_gmv_1 + avg_productmrp_1 + 
                 discount_per_1, data = train)

summary(model_17) #Multiple R-squared:  0.79,	Adjusted R-squared:  0.682     
sort(vif(model_17))

#Online Marketing high VIF, and the fall in adj R sq is least
model_18 <- lm(formula = total_gmv ~ avg_prod_proc_sla + 
                 earlyDelivery_orders + onTimeDelivery_orders + 
                 Total_Investment + TV + Sponsorship +  
                 SEM + Other +  
                 inc_discount_per_MA2 + 
                 inc_discount_per_MA4 + inc_avg_productmrp_MA2 + inc_avg_productmrp_MA4 + 
                 total_gmv_1 + avg_productmrp_1 + 
                 discount_per_1, data = train)

summary(model_18) #Multiple R-squared:  0.691,	Adjusted R-squared:  0.546     
sort(vif(model_18))

#SEM high VIF, P value
model_19 <- lm(formula = total_gmv ~ avg_prod_proc_sla + 
                 earlyDelivery_orders + onTimeDelivery_orders + 
                 Total_Investment + TV + Sponsorship +  
                 Other +  
                 inc_discount_per_MA2 + 
                 inc_discount_per_MA4 + inc_avg_productmrp_MA2 + inc_avg_productmrp_MA4 + 
                 total_gmv_1 + avg_productmrp_1 + 
                 discount_per_1, data = train)

summary(model_19) #Multiple R-squared:  0.689,	Adjusted R-squared:  0.557     
sort(vif(model_19))

#TV high VIF,P value
model_20 <- lm(formula = total_gmv ~ avg_prod_proc_sla + 
                 earlyDelivery_orders + onTimeDelivery_orders + 
                 Total_Investment + Sponsorship +  
                 Other +  
                 inc_discount_per_MA2 + 
                 inc_discount_per_MA4 + inc_avg_productmrp_MA2 + inc_avg_productmrp_MA4 + 
                 total_gmv_1 + avg_productmrp_1 + 
                 discount_per_1, data = train)

summary(model_20) #Multiple R-squared:  0.687,	Adjusted R-squared:  0.567    
sort(vif(model_20))

#`Other` high VIF P value
model_21 <- lm(formula = total_gmv ~ avg_prod_proc_sla + 
                 earlyDelivery_orders + onTimeDelivery_orders + 
                 Total_Investment + Sponsorship +  
                 inc_discount_per_MA2 + 
                 inc_discount_per_MA4 + inc_avg_productmrp_MA2 + inc_avg_productmrp_MA4 + 
                 total_gmv_1 + avg_productmrp_1 + 
                 discount_per_1, data = train)

summary(model_21) #Multiple R-squared:  0.684,	Adjusted R-squared:  0.576       
sort(vif(model_21))

#discount_per-1 high  P value
model_22 <- lm(formula = total_gmv ~ avg_prod_proc_sla + 
                 earlyDelivery_orders + onTimeDelivery_orders + 
                 Total_Investment + Sponsorship +  
                 inc_discount_per_MA2 + 
                 inc_discount_per_MA4 + inc_avg_productmrp_MA2 + inc_avg_productmrp_MA4 + 
                 total_gmv_1 + avg_productmrp_1 
                 , data = train)

summary(model_22) #Multiple R-squared:  0.676,	Adjusted R-squared:  0.577       
sort(vif(model_22))

#inc_avg_productmrp_MA4 high P value
model_23 <- lm(formula = total_gmv ~ avg_prod_proc_sla + 
                 earlyDelivery_orders + onTimeDelivery_orders + 
                 Total_Investment + Sponsorship +  
                 inc_discount_per_MA2 + 
                 inc_discount_per_MA4 + inc_avg_productmrp_MA2 +  
                 total_gmv_1 + avg_productmrp_1 
               , data = train)

summary(model_23) #Multiple R-squared:  0.67,	Adjusted R-squared:  0.581        
sort(vif(model_23))

#inc_discount_per_MA2 high VIF and P value
model_24 <- lm(formula = total_gmv ~ avg_prod_proc_sla + 
                 earlyDelivery_orders + onTimeDelivery_orders + 
                 Total_Investment + Sponsorship +  
                 inc_discount_per_MA4 + inc_avg_productmrp_MA2 +  
                 total_gmv_1 + avg_productmrp_1 
               , data = train)

summary(model_24) #Multiple R-squared:  0.668,	Adjusted R-squared:  0.59       
sort(vif(model_24))

#inc_discount_per_MA4 high P value
model_25 <- lm(formula = total_gmv ~ avg_prod_proc_sla + 
                 earlyDelivery_orders + onTimeDelivery_orders + 
                 Total_Investment + Sponsorship +  
                 inc_avg_productmrp_MA2 +  
                 total_gmv_1 + avg_productmrp_1 
               , data = train)


summary(model_25) #Multiple R-squared:  0.638,	Adjusted R-squared:  0.567        
sort(vif(model_25))

#Sponsorship high VIF,P value
model_26 <- lm(formula = total_gmv ~ avg_prod_proc_sla + 
                 earlyDelivery_orders + onTimeDelivery_orders + 
                 Total_Investment +   
                 inc_avg_productmrp_MA2 +  
                 total_gmv_1 + avg_productmrp_1 
               , data = train)


summary(model_26) #Multiple R-squared:  0.598,	Adjusted R-squared:  0.531        
sort(vif(model_26))

#total_gmv-1 high P value
model_27 <- lm(formula = total_gmv ~ avg_prod_proc_sla + 
                 earlyDelivery_orders + onTimeDelivery_orders + 
                 Total_Investment +   
                 inc_avg_productmrp_MA2 +  
                 avg_productmrp_1 
               , data = train)
summary(model_27) #Multiple R-squared:  0.585,	Adjusted R-squared:  0.527        
sort(vif(model_27))

#avg_prod_proc_sla high P value
model_28 <- lm(formula = total_gmv ~  
                 earlyDelivery_orders + onTimeDelivery_orders + 
                 Total_Investment +   
                 inc_avg_productmrp_MA2 +  
                 avg_productmrp_1 
               , data = train)


summary(model_28) #Multiple R-squared:  0.569,	Adjusted R-squared:  0.52     
sort(vif(model_28))

# onTimeDelivery_orders less significant
model_29 <- lm(formula = total_gmv ~  
                 earlyDelivery_orders +  
                 Total_Investment +   
                 inc_avg_productmrp_MA2 +  
                 avg_productmrp_1 
               , data = train)


summary(model_29) #Multiple R-squared:  0.516,	Adjusted R-squared:  0.473      
sort(vif(model_29))

final_model <- model_29
AIC(final_model) #115

#final model
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)              0.0223     0.1012    0.22  0.82648    
#earlyDelivery_orders     0.2768     0.1073    2.58  0.01328 *  
#  Total_Investment         0.4761     0.1072    4.44  5.8e-05 ***
#  inc_avg_productmrp_MA2   0.5989     0.1227    4.88  1.4e-05 ***
#  `avg_productmrp-1`       0.4343     0.1212    3.58  0.00083 ***

####################################cross validation ######################################

train_control <- trainControl(method="cv", number=5)
train <- na.omit(train)
# Fit Naive Bayes Model
model <- train(total_gmv ~  earlyDelivery_orders + Total_Investment +   inc_avg_productmrp_MA2 +  
                 avg_productmrp_1, data=train, trControl=train_control, method="lm")
# Summarise Results
print(model) 
# RMSE   Rsquared  MAE  
# 0.798  0.542     0.578

DAAG::cv.lm(data = train, form.lm = formula(total_gmv ~  earlyDelivery_orders +  
                                              Total_Investment +   
                                              inc_avg_productmrp_MA2 +  
                                              avg_productmrp_1),
            m = 5, dots = FALSE, seed = 29, plotit = c("Observed","Residual"),
            main="Small symbols show cross-validation predicted values",
            legend.pos="topleft", printit = TRUE)

# overall ms 0.62

############################ Elasticity ###############################
elast <- calcElasticity(final_model,train,0)
elast

#variable elasticity direction
#1   earlyDelivery_orders     -1.781  Negative
#2       Total_Investment      2.641  Positive
#3 inc_avg_productmrp_MA2      0.334  Positive
#4       avg_productmrp_1      0.153  Positive

##plot the elasticity
plot4 <- plotElasticity(elast,"HOme Audio - Distributive Model")
plot4

###########################################################################################
###########################Distributive and Multiplicative Model ##########################
###########################################################################################

train = master_ha_final.data[,-which(names(master_ha_final.data) %in% c("Week","tot_productmrp","norders","nunits","repeated_orders","value_per_visitor"))]
train <- slide(train, Var = "total_gmv", slideBy = -1)
train <- slide(train, Var = "total_gmv", slideBy = -2)
train <- slide(train, Var = "total_gmv", slideBy = -3)

#tagging the lag for avg productmrp and discount_per as well
train <- slide(train, Var = "avg_productmrp", slideBy = -1)
train <- slide(train, Var = "avg_productmrp", slideBy = -2)
train <- slide(train, Var = "avg_productmrp", slideBy = -3)

train <- slide(train, Var = "discount_per", slideBy = -1)
train <- slide(train, Var = "discount_per", slideBy = -2)
train <- slide(train, Var = "discount_per", slideBy = -3)

train <- as.data.frame(sapply(train,function(x) ifelse(x==0,0.0001,x)))
train <- as.data.frame(sapply(train,function(x) log(x)))
train <- as.data.frame(scale(train))

model_1 <- lm(total_gmv ~., data = train)
summary(model_1) #Multiple R-squared:  0.904,	Adjusted R-squared:  0.718 

step <- stepAIC(model_1, direction = "both")
step

model_2 <- lm(formula = total_gmv ~ avg_productmrp + avg_prod_proc_sla + 
                cod_orders + earlyDelivery_orders + onTimeDelivery_orders + 
                Total_Investment + Sponsorship + Content_Marketing + Online_Marketing + 
                SEM + NPS + `total_gmv-1` + `total_gmv-2` + `total_gmv-3` + 
                `avg_productmrp-3` + `discount_per-3`, data = train)
summary(model_2) #Multiple R-squared:  0.889,	Adjusted R-squared:  0.832
sort(vif(model_2))

#Online Marketing: high VIF, P value
model_3 <- lm(formula = total_gmv ~ avg_productmrp + avg_prod_proc_sla + 
                cod_orders + earlyDelivery_orders + onTimeDelivery_orders + 
                Total_Investment + Sponsorship + Content_Marketing +  
                SEM + NPS + `total_gmv-1` + `total_gmv-2` + `total_gmv-3` + 
                `avg_productmrp-3` + `discount_per-3`, data = train)
summary(model_3) #Multiple R-squared:  0.875,	Adjusted R-squared:  0.816
sort(vif(model_3))

#`discount_per-3: high VIF, p Value
model_4 <- lm(formula = total_gmv ~ avg_productmrp + avg_prod_proc_sla + 
                cod_orders + earlyDelivery_orders + onTimeDelivery_orders + 
                Total_Investment + Sponsorship + Content_Marketing +  
                SEM + NPS + `total_gmv-1` + `total_gmv-2` + `total_gmv-3` + 
                `avg_productmrp-3` , data = train)
summary(model_4) #Multiple R-squared:  0.867,	Adjusted R-squared:  0.811
sort(vif(model_4))

#avg_productmrp-3 : high p value
model_5 <- lm(formula = total_gmv ~ avg_productmrp + avg_prod_proc_sla + 
                cod_orders + earlyDelivery_orders + onTimeDelivery_orders + 
                Total_Investment + Sponsorship + Content_Marketing +  
                SEM + NPS + `total_gmv-1` + `total_gmv-2` + `total_gmv-3`, data = train)
summary(model_5) #Multiple R-squared:  0.854,	Adjusted R-squared:  0.799 
sort(vif(model_5))

#total_gmv-3 high p value
model_6 <- lm(formula = total_gmv ~ avg_productmrp + avg_prod_proc_sla + 
                cod_orders + earlyDelivery_orders + onTimeDelivery_orders + 
                Total_Investment + Sponsorship + Content_Marketing +  
                SEM + NPS + `total_gmv-1` + `total_gmv-2`, data = train)
summary(model_6) #Multiple R-squared:  0.838,	Adjusted R-squared:  0.784 
sort(vif(model_6))

#SEM high P value
model_7 <- lm(formula = total_gmv ~ avg_productmrp + avg_prod_proc_sla + 
                cod_orders + earlyDelivery_orders + onTimeDelivery_orders + 
                Total_Investment + Sponsorship + Content_Marketing +
                NPS + `total_gmv-1` + `total_gmv-2`, data = train)
summary(model_7) #Multiple R-squared:  0.826,	Adjusted R-squared:  0.774  
sort(vif(model_7))

#avg_prod_proc_sla high P value
model_8 <- lm(formula = total_gmv ~ avg_productmrp +  
                cod_orders + earlyDelivery_orders + onTimeDelivery_orders + 
                Total_Investment + Sponsorship + Content_Marketing +
                NPS + `total_gmv-1` + `total_gmv-2`, data = train)
summary(model_8) #Multiple R-squared:   0.8,	Adjusted R-squared:  0.747
sort(vif(model_8))

#NPS high VIF value
model_9 <- lm(formula = total_gmv ~ avg_productmrp +  
                cod_orders + earlyDelivery_orders + onTimeDelivery_orders + 
                Total_Investment + Sponsorship + Content_Marketing +
                `total_gmv-1` + `total_gmv-2`, data = train)
summary(model_9) #Multiple R-squared:  0.766,	Adjusted R-squared:  0.712
sort(vif(model_9))

#cod_orders high P value
model_10 <- lm(formula = total_gmv ~ avg_productmrp +  
                 earlyDelivery_orders + onTimeDelivery_orders + 
                 Total_Investment + Sponsorship + Content_Marketing +
                 `total_gmv-1` + `total_gmv-2`, data = train)
summary(model_10) #Multiple R-squared:  0.746,	Adjusted R-squared:  0.696
sort(vif(model_10))

#`total_gmv-1` high P value
model_11 <- lm(formula = total_gmv ~ avg_productmrp +  
                 earlyDelivery_orders + onTimeDelivery_orders + 
                 Total_Investment + Sponsorship + Content_Marketing +
                 `total_gmv-2`, data = train)
summary(model_11) #Multiple R-squared:  0.731,	Adjusted R-squared:  0.685 
sort(vif(model_11))

#Sponsorship high VIF, mod P value
model_12 <- lm(formula = total_gmv ~ avg_productmrp +  
                 earlyDelivery_orders + onTimeDelivery_orders + 
                 Total_Investment + Content_Marketing +
                 `total_gmv-2`, data = train)
summary(model_12) #Multiple R-squared:  0.673,	Adjusted R-squared:  0.626
sort(vif(model_12))

#`total_gmv-2  high P value
model_13 <- lm(formula = total_gmv ~ avg_productmrp +  
                 earlyDelivery_orders + onTimeDelivery_orders + 
                 Total_Investment + Content_Marketing, data = train)
summary(model_13) #Multiple R-squared:  0.631,	Adjusted R-squared:  0.59 
sort(vif(model_13))

#Content Marketing high VIF
model_14 <- lm(formula = total_gmv ~ avg_productmrp +  
                 earlyDelivery_orders + onTimeDelivery_orders + 
                 Total_Investment, data = train)
summary(model_14) #Multiple R-squared:  0.478,	Adjusted R-squared:  0.432   
sort(vif(model_14))

#avg_productmrp high P value
model_15 <- lm(formula = total_gmv ~   
                 earlyDelivery_orders + onTimeDelivery_orders + 
                 Total_Investment, data = train)
summary(model_15) #Multiple R-squared:  0.446,	Adjusted R-squared:  0.41   
sort(vif(model_15))

final_model <- model_15
AIC(final_model) #124

#final_model
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)           -3.08e-17   1.08e-01    0.00   1.0000    
#earlyDelivery_orders   6.95e-01   1.72e-01    4.04   0.0002 ***
#  onTimeDelivery_orders -5.73e-01   1.70e-01   -3.36   0.0016 ** 
#  Total_Investment       5.53e-01   1.15e-01    4.81  1.6e-05 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 0.768 on 47 degrees of freedom
#Multiple R-squared:  0.446,	Adjusted R-squared:  0.41 
#F-statistic: 12.6 on 3 and 47 DF,  p-value: 3.63e-06

####################################cross validation ######################################

train_control <- trainControl(method="cv", number=5)
train <- na.omit(train)
# Fit Naive Bayes Model
model <- train(total_gmv ~   earlyDelivery_orders + onTimeDelivery_orders + Total_Investment, data=train, trControl=train_control, method="lm")
# Summarise Results
print(model) 
#RMSE   Rsquared  MAE  
#0.731  0.454     0.579

DAAG::cv.lm(data = train, form.lm = formula(total_gmv ~  avg_productmrp + SEM),
            m = 5, dots = FALSE, seed = 29, plotit = c("Observed","Residual"),
            main="Small symbols show cross-validation predicted values",
            legend.pos="topleft", printit = TRUE)

# overall ms 0.729

############################ Elasticity ###############################
elast <- calcElasticity(final_model,train,1)
elast

# variable elasticity direction
# 1  earlyDelivery_orders      0.695  Positive
# 2 onTimeDelivery_orders     -0.573  Negative
# 3     Total_Investment      0.553  Positive

##plot the elasticity
plot5 <- plotElasticity(elast,"HOme Audio - Distributive + Multiplicative")
plot5

plotElasticity_grid(plot1,plot2,plot3,plot4,plot5)
