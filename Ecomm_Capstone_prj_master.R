
######## ECOMMERCE CAPSTONE PROJECT R MASTER CODE #########################

### Objective: 
# To develop a market mix model to observe the actual impact of different 
# marketing variables from July 2015 to June 2016 for 
# three product sub-categories - camera accessory, home audio and gaming accessory

### Steps
## 1. Load packages, utilities
## 2. Source data, prepare data and perform EDA
## 3. Create master data frames with KPIs and dump into csv files for model generation for 3 categories
## 4. Model generation for each category in separate R code. Five models generated for each category.
##     a. Linear model
##     b. Multiplicative model
##     c. Koyck model
##     d. Distributive Lag model
##     e. Distributive Lag + Multiplicative model
## 5. Select best model based on mulitple criteria. 
## 6. Recommendation for business of each category based on selected models

## Directory structure
## Inside working directory, following sub-directories expected
####### <code> : to contain all .R files
####### <data> : to contain all input .csv, .xls, and intermediate .csv files.

################ Code starts here ###############################

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

################### Set Working Directory ##############################
setwd("D:\\Devendra\\PGDDS\\Elective\\capstone_prj\\Ecommerce Capstone\\final_submission")
#list.dirs()
#list.files()

# Source custom functions used in code 
source("code\\UtilityFunctions.R")

# Source Data preparation, EDA R files
source("code\\Ecomm_Capstone_prj_data_preparation_EDA.R")

# Camera Accessory Model generation
source("code\\Ecomm_Capstone_prj_CA_modeling.R")

# Home Audio Model generation
source("code\\Ecomm_Capstone_prj_HA_modelling.R")

# Game Accessory Model generation
source("code\\Ecomm_Capstone_prj_GA_modeling.R")

###################### CONCLUSION ###################################
# Gaming Accessory :
#  Based on different evaluation criteria and elasticity of variables, distributive lag + multiplicative model is selected.
# It is recommended to increase advertisement spent on Radio and Sponsorship channels for Gaming Accessory.
# Ontime early delivery is very important for increase in sales for Gaming Accessory. 

# Camera Accessory :
#  Based on different evaluation criteria and elasticity of variables, distributive lag + multiplicative model is selected.
# As per elasticity of variables, it is recommended that advertisement spent be increased in Sponsorship and Affiliates channels for increase in sales in Camera Accessory.
# Early delivery of orders is key to increase in sales.

# Home Audio :
#  Based on different evaluation criteria and elasticity of variables, distributive lag model is selected.
# Overall advertisement spent can be increased to with more focus on digital channel for increase in sales in Home Audio
# Early Delivery of products is key to increase in sales.
############################## END ###################################
