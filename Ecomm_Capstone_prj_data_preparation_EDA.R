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
#####################################################################################
#############################load the data ##########################################
#####################################################################################
#load the data, convert the "\\N" string to NA while loading 

elecKart.data <- read.csv('data\\ConsumerElectronics.csv', header = T, stringsAsFactors = F,na.strings = c("\\N"))

#check the structure of the data
describe(elecKart.data)

View(elecKart.data)

#no of unige products
# rename first column to simplify
colnames(elecKart.data)[1] <- "fsn_id"
length(unique(elecKart.data$fsn_id)) #21219 unique skus are there


###################################################################################
#####################Data Preperation and Clean Up ################################
###################################################################################

#convert the order date into date format
elecKart.data$order_date <- as.POSIXct(elecKart.data$order_date, format= "%Y-%m-%d %H:%M:%S")

#we need to consider the data from July 2015 to June 2016
elecKart.data <- elecKart.data %>% subset(elecKart.data$order_date >= '2015-07-01 00:00:00' & 
                                            elecKart.data$order_date <= '2016-06-30 23:59:59')

#glimpse(elecKart.data)  #1648215 observations of 20 variables
str(elecKart.data)

#We need to do the analysis for three product sub category - camera accessory, home audio
#and gaming accessory
subCategories <- c('CameraAccessory','HomeAudio','GamingAccessory')

elecKart.data <- elecKart.data %>%
  subset(product_analytic_sub_category %in% subCategories)

#check the structure of the data
str(elecKart.data)

#check for duplicate entries
#considering the order_id and the fsn_id as the unique key
sum(duplicated(elecKart.data)) #there are 37625 duplicate records

#remove the duplicate records
elecKart.data <- distinct(elecKart.data,.keep_all = T) 

#order the data by date in asscending order
elecKart.data <- elecKart.data[order(elecKart.data$order_date),]

#as analysis has to be done on a weekly basis, extract the week from the 
#order date and storing it as a separate column
elecKart.data$Week <- week(ymd_hms(elecKart.data$order_date))#adding a week column

#--------------------------------------------------------------------------------
############################Treating Missing values ###########################
#--------------------------------------------------------------------------------

#check for na values
sum(is.na(elecKart.data)) #826826 NA values are there

#check which column has na values 
plot_missing(elecKart.data) #pincode, gmv and cust_id field has na values
# deliverybdays, deliverycdays also has NA values

###1. around 0.26% of the records have missing value in customer id, pincodde and gmv
sapply(elecKart.data,function(x) sum(is.na(x))) 
#1362 observations have missing values in cust id, pincode and gmv

#since the percentage of records is very small, we can drop these observations
sum(is.na(elecKart.data$cust_id) & is.na(elecKart.data$pincode) & is.na(elecKart.data$gmv))
# cust_id, pincode and gmv have NAs in same rows.
elecKart.data <- elecKart.data[!is.na(elecKart.data$cust_id),]

###2. check if month and year column values matches with the order date
sum(month(elecKart.data$order_date) != elecKart.data$Month & year(elecKart.data$order_date) != elecKart.data$Year)
#the month and column value matches with the order date.

###3. Negative Customer Id check
sum(elecKart.data$cust_id < 0) #there are 264028 records

#assuming that the customer id can never be negative, converting the negative 
#value to positive value
elecKart.data$cust_id <- ifelse(elecKart.data$cust_id < 0, -1 * elecKart.data$cust_id, elecKart.data$cust_id)

#check the number of uniqe customers, that visit the site frequently
length(unique(elecKart.data$cust_id)) #there are 440174 unique customers

#cross validate the missing values
sum(is.na(elecKart.data)) #820018 observations have missing values

###3. units should not be less than or equal to 0
sum(elecKart.data$units <= 0) #there are no orders with negative or 0 units

###4. product_mrp should never be negative
sum(elecKart.data$product_mrp < 0) #there are no negative observations
#can product mrp be 0?
sum(elecKart.data$product_mrp == 0) #there are 3236 observations

#replace the product mrp with 0 value,with the value of the same product if
#it exists
#elecKart.data <- elecKart.data %>% group_by(ï..fsn_id) %>%
#  mutate(product_mrp = replace(product_mrp, product_mrp == 0,mean(product_mrp)))
elecKart.data <- elecKart.data %>% group_by(fsn_id) %>%
  mutate(product_mrp = replace(product_mrp, product_mrp == 0,mean(product_mrp)))

sum(elecKart.data$product_mrp == 0) #there are 3236 observations, the mrp is not 
#available in the data set

#replace the observations with product mrp as 0 with the gmv value per unit, since
#gmv can never be greater tha mrp, we will set the mrp for these procuts with the
#gmv per unit value
sum(elecKart.data$product_mrp == 0 & (elecKart.data$gmv/elecKart.data$units) > 0) 


#there are 2895 observations for which mrp is 0 but gmv is greater than 0
#replace the mrp with gmv per unit for these records
elecKart.data$product_mrp <- ifelse(elecKart.data$product_mrp == 0 & (elecKart.data$gmv/elecKart.data$units) > 0, 
                                    round((elecKart.data$gmv/elecKart.data$units)), elecKart.data$product_mrp)
sum(elecKart.data$product_mrp == 0) #341 observations with mrp as 0

###5. gmv should not be negative or equal to zero
sum(elecKart.data$gmv < 0) #there are no negative values
sum(elecKart.data$gmv == 0) #there are 602 observations with a value of 0
#check the MRP for the observations where gmv is 0
sum(elecKart.data$gmv == 0 & elecKart.data$product_mrp == 0) #341 observations have both MRP and gmv as 0

#replace the observations with gmv values as 0 and product mrp not equal to 0
elecKart.data$gmv <- ifelse(elecKart.data$gmv == 0 & elecKart.data$product_mrp > 0,
                            elecKart.data$product_mrp * elecKart.data$units, elecKart.data$gmv)

###6. gmv should not be grater than product_mrp multiplied by units
sum(elecKart.data$gmv > (elecKart.data$units * elecKart.data$product_mrp)) #there are 12824 records
#assuming that the gmv should not be greater than product mrp multiplied by units, replace these
#observations gmv value with the product_mrp * units

elecKart.data$gmv <- ifelse(elecKart.data$gmv > (elecKart.data$units * elecKart.data$product_mrp), 
                            elecKart.data$units * elecKart.data$product_mrp,elecKart.data$gmv)
#counter check the gmv values after replacing the values
sum(elecKart.data$gmv > (elecKart.data$units * elecKart.data$product_mrp)) #there are no records

#there are 341 observations with both gmv and product_mrp as 0

###7. handling deliverbdays issues
#handling negative value 
sum(elecKart.data$deliverybdays <0,na.rm = T) 
#dropping these records as there are only 10 records with negative value
elecKart.data <- elecKart.data[!(!is.na(elecKart.data$deliverybdays) & elecKart.data$deliverybdays < 0),]

#replace the na value with the mean value by product
#elecKart.data <- elecKart.data %>% group_by(ï..fsn_id) %>% 
#  mutate(deliverybdays = replace_na(deliverybdays,round(mean(deliverybdays,na.rm = T))))
elecKart.data <- elecKart.data %>% group_by(fsn_id) %>% 
  mutate(deliverybdays = replace_na(deliverybdays,round(mean(deliverybdays,na.rm = T))))
#after replacing the missing values, we observe that there are few NAN
nrow(elecKart.data[is.nan(elecKart.data$deliverybdays),]) #31246 observations are there
#replacing the nan values with 1, as the delivery delays can not be negative or 0, assuming that the 
#default delay is 1
elecKart.data$deliverybdays <- ifelse(is.nan(elecKart.data$deliverybdays), 1, elecKart.data$deliverybdays)

###8. handling delivercdays issues
sum(elecKart.data$deliverycdays <0,na.rm = T)  #there are no observations

#replace the na value with the mean value by product
#elecKart.data <- elecKart.data %>% group_by(ï..fsn_id) %>% 
#  mutate(deliverycdays = replace_na(deliverycdays,round(mean(deliverycdays,na.rm = T))))
elecKart.data <- elecKart.data %>% group_by(fsn_id) %>% 
  mutate(deliverycdays = replace_na(deliverycdays,round(mean(deliverycdays,na.rm = T))))

#after replacing the missing values, we observe that there are few NAN
nrow(elecKart.data[is.nan(elecKart.data$deliverycdays),]) #31246 observations are there
#replacing the nan values with 1, assuming that the delivery delay can not be less than or equal to 0,
#replacing the NAN value with 1
elecKart.data$deliverycdays <- ifelse(is.nan(elecKart.data$deliverycdays), 1, elecKart.data$deliverycdays)

###9. sla should not be negative
sum(elecKart.data$sla < 0) #there are no negative values

###10. product procurement sla should not be negative
sum(elecKart.data$product_procurement_sla < 0) #there are 14138 records with negative values
sum(elecKart.data$product_procurement_sla == 0) #there are 11877 records with 0 value

#assuming that the procurement sla can not be less than or equal to 0,replacing the vales
#with a minimum value of 1
elecKart.data$product_procurement_sla <- ifelse(elecKart.data$product_procurement_sla <= 0, 1, 
                                                elecKart.data$product_procurement_sla)
sum(elecKart.data$product_procurement_sla <= 0) #there are 0 records

###11. calculating the percentage discount using the below formula
# ((product_mrp * units - gmv)/product_mrp) * 100
elecKart.data$discountPer <- round((((elecKart.data$product_mrp * elecKart.data$units) - elecKart.data$gmv)/ elecKart.data$product_mrp) * 100,2)
 sum(is.nan(elecKart.data$discountPer)) #there are 341 observations
#replace the nan with 0 
elecKart.data$discountPer <- ifelse(is.nan(elecKart.data$discountPer), 0,elecKart.data$discountPer)

###12. adding a variable to indicate whether there was a delay in order delivery or not
elecKart.data$delivery_on_time <- elecKart.data$sla - (elecKart.data$deliverybdays+elecKart.data$deliverycdays+elecKart.data$product_procurement_sla)
elecKart.data$delivery_status[elecKart.data$delivery_on_time < 0] <- 'Delayed'
elecKart.data$delivery_status[elecKart.data$delivery_on_time == 0] <- 'On time'
elecKart.data$delivery_status[elecKart.data$delivery_on_time > 0] <- 'Early'

#convert the categorical variables into factors
elecKart.data$s1_fact.order_payment_type_COD <- as.factor(elecKart.data$s1_fact.order_payment_type)
levels(elecKart.data$s1_fact.order_payment_type_COD)<-c(1,0) # 1- COD, 0- Prepaid
elecKart.data$s1_fact.order_payment_type_COD <- as.numeric(levels(elecKart.data$s1_fact.order_payment_type_COD))[elecKart.data$s1_fact.order_payment_type_COD]

elecKart.data <- data.frame(elecKart.data)
dummy <- data.frame(model.matrix(~delivery_status, data=elecKart.data))[,-1]
elecKart.data <- cbind(data.frame(elecKart.data[,-c(23,24)]),dummy)
  
# remove unwanted columns: fsn_id, order_id, order_item_id, cust_id, pincode, super category, category
elecKart.data %>% group_by(product_analytic_super_category,product_analytic_category,product_analytic_sub_category) %>% summarise(n=n())
# there is one to one relation, hence super category, category can be removed

# convert weeknum to continuous from 2015 to 2016
elecKart.data$Week <- ifelse(elecKart.data$Year==2016,elecKart.data$Week+53,elecKart.data$Week)

drop.columns.names <- c("Year","Month","s1_fact.order_payment_type","order_date","fsn_id","order_id","order_item_id","pincode","product_analytic_super_category","product_analytic_category","product_analytic_vertical")
elecKart.data  <- data.frame(drop_columns(elecKart.data,drop.columns.names))

############### create separate df for each sub category ####################### 
elecKart_ca.data <- filter(elecKart.data,product_analytic_sub_category=="CameraAccessory")
elecKart_ha.data <- filter(elecKart.data,product_analytic_sub_category=="HomeAudio")
elecKart_ga.data <- filter(elecKart.data,product_analytic_sub_category=="GamingAccessory")

##################### outlier treatment ########################################

box_theme<- theme(axis.line=element_blank(),axis.title=element_blank(), 
                  axis.ticks=element_blank(), axis.text=element_blank())

box_theme_y<- theme(axis.line.y=element_blank(),axis.title.y=element_blank(), 
                    axis.ticks.y=element_blank(), axis.text.y=element_blank(),
                    legend.position="none")

# check sla
p <- plot_grid(ggplot(elecKart_ca.data, aes(sla))+ geom_histogram(fill="blue"),
          ggplot(elecKart_ca.data, aes(x="",y=sla))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)
title <- ggdraw() + draw_label("SLA distribution for CameraAccessory", fontface='bold')
plot_grid(title, p, ncol=1, rel_heights=c(0.1, 1))

p <- plot_grid(ggplot(elecKart_ha.data, aes(sla))+ geom_histogram(fill="blue"),
               ggplot(elecKart_ha.data, aes(x="",y=sla))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
               align = "v",ncol = 1)
title <- ggdraw() + draw_label("SLA distribution for HomeAudio", fontface='bold')
plot_grid(title, p, ncol=1, rel_heights=c(0.1, 1))

p <- plot_grid(ggplot(elecKart_ga.data, aes(sla))+ geom_histogram(fill="blue"),
               ggplot(elecKart_ga.data, aes(x="",y=sla))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
               align = "v",ncol = 1)
title <- ggdraw() + draw_label("SLA distribution for GamingAccessory", fontface='bold')
plot_grid(title, p, ncol=1, rel_heights=c(0.1, 1))

quantile(elecKart_ca.data$sla,c(0.25,0.5,0.75,0.95,0.99))
quantile(elecKart_ha.data$sla,c(0.25,0.5,0.75,0.95,0.99))
quantile(elecKart_ga.data$sla,c(0.25,0.5,0.75,0.95,0.99))

# outliers present; limit them
elecKart_ca.data$sla <- func_limit_outliers(elecKart_ca.data$sla)
elecKart_ha.data$sla <- func_limit_outliers(elecKart_ha.data$sla)
elecKart_ga.data$sla <- func_limit_outliers(elecKart_ga.data$sla)

# check product procurement sla
p <- plot_grid(ggplot(elecKart_ca.data, aes(product_procurement_sla))+ geom_histogram(fill="blue"),
               ggplot(elecKart_ca.data, aes(x="",y=product_procurement_sla))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
               align = "v",ncol = 1)
title <- ggdraw() + draw_label("product_procurement_sla distribution for CameraAccessory", fontface='bold')
plot_grid(title, p, ncol=1, rel_heights=c(0.1, 1))

p <- plot_grid(ggplot(elecKart_ha.data, aes(product_procurement_sla))+ geom_histogram(fill="blue"),
               ggplot(elecKart_ha.data, aes(x="",y=product_procurement_sla))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
               align = "v",ncol = 1)
title <- ggdraw() + draw_label("product_procurement_sla distribution for HomeAudio", fontface='bold')
plot_grid(title, p, ncol=1, rel_heights=c(0.1, 1))

p <- plot_grid(ggplot(elecKart_ga.data, aes(product_procurement_sla))+ geom_histogram(fill="blue"),
               ggplot(elecKart_ga.data, aes(x="",y=product_procurement_sla))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
               align = "v",ncol = 1)
title <- ggdraw() + draw_label("product_procurement_sla distribution for GamingAccessory", fontface='bold')
plot_grid(title, p, ncol=1, rel_heights=c(0.1, 1))

quantile(elecKart_ca.data$product_procurement_sla,c(0.25,0.5,0.75,0.95,0.99))
quantile(elecKart_ha.data$product_procurement_sla,c(0.25,0.5,0.75,0.95,0.99))
quantile(elecKart_ga.data$product_procurement_sla,c(0.25,0.5,0.75,0.95,0.99))
# Outliers present; limit them
elecKart_ca.data$product_procurement_sla <- func_limit_outliers(elecKart_ca.data$product_procurement_sla)
elecKart_ha.data$product_procurement_sla <- func_limit_outliers(elecKart_ha.data$product_procurement_sla)
elecKart_ga.data$product_procurement_sla <- func_limit_outliers(elecKart_ga.data$product_procurement_sla)

# check units
p <- plot_grid(ggplot(elecKart_ca.data, aes(units))+ geom_histogram(fill="blue"),
               ggplot(elecKart_ca.data, aes(x="",y=units))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
               align = "v",ncol = 1)
title <- ggdraw() + draw_label("units distribution for CameraAccessory", fontface='bold')
plot_grid(title, p, ncol=1, rel_heights=c(0.1, 1))

p <- plot_grid(ggplot(elecKart_ha.data, aes(units))+ geom_histogram(fill="blue"),
               ggplot(elecKart_ha.data, aes(x="",y=units))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
               align = "v",ncol = 1)
title <- ggdraw() + draw_label("units distribution for HomeAudio", fontface='bold')
plot_grid(title, p, ncol=1, rel_heights=c(0.1, 1))

p <- plot_grid(ggplot(elecKart_ga.data, aes(units))+ geom_histogram(fill="blue"),
               ggplot(elecKart_ga.data, aes(x="",y=units))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
               align = "v",ncol = 1)
title <- ggdraw() + draw_label("units distribution for GamingAccessory", fontface='bold')
plot_grid(title, p, ncol=1, rel_heights=c(0.1, 1))

quantile(elecKart_ca.data$units,c(0.25,0.5,0.75,0.95,0.99))
quantile(elecKart_ha.data$units,c(0.25,0.5,0.75,0.95,0.99))
quantile(elecKart_ga.data$units,c(0.25,0.5,0.75,0.95,0.99))

sum(elecKart.data$units>2)
# 1702 entries with value more than 2
# since MRP, GMV calc depend on unit, hence removing them instead of limiting
elecKart_ca.data <- filter(elecKart_ca.data,units <= 2)
elecKart_ha.data <- filter(elecKart_ha.data,units <= 2)
elecKart_ga.data <- filter(elecKart_ga.data,units <= 2)

# check mrp
p <- plot_grid(ggplot(elecKart_ca.data, aes(product_mrp))+ geom_histogram(fill="blue"),
               ggplot(elecKart_ca.data, aes(x="",y=product_mrp))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
               align = "v",ncol = 1)
title <- ggdraw() + draw_label("product_mrp distribution for CameraAccessory", fontface='bold')
plot_grid(title, p, ncol=1, rel_heights=c(0.1, 1))

p <- plot_grid(ggplot(elecKart_ha.data, aes(product_mrp))+ geom_histogram(fill="blue"),
               ggplot(elecKart_ha.data, aes(x="",y=product_mrp))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
               align = "v",ncol = 1)
title <- ggdraw() + draw_label("product_mrp distribution for HomeAudio", fontface='bold')
plot_grid(title, p, ncol=1, rel_heights=c(0.1, 1))

p <- plot_grid(ggplot(elecKart_ga.data, aes(product_mrp))+ geom_histogram(fill="blue"),
               ggplot(elecKart_ga.data, aes(x="",y=product_mrp))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
               align = "v",ncol = 1)
title <- ggdraw() + draw_label("product_mrp distribution for GamingAccessory", fontface='bold')
plot_grid(title, p, ncol=1, rel_heights=c(0.1, 1))

quantile(elecKart_ca.data$product_mrp,c(0.25,0.5,0.75,0.95,0.99))
quantile(elecKart_ha.data$product_mrp,c(0.25,0.5,0.75,0.95,0.99))
quantile(elecKart_ga.data$product_mrp,c(0.25,0.5,0.75,0.95,0.99))

summary(elecKart.data$product_mrp)
# remove outliers
sum(elecKart_ca.data$product_mrp>19500)
sum(elecKart_ha.data$product_mrp>10500)
sum(elecKart_ga.data$product_mrp>50000)

elecKart_ca.data <- filter(elecKart_ca.data,product_mrp <= 19500)
elecKart_ha.data <- filter(elecKart_ha.data,product_mrp <= 10500)
elecKart_ga.data <- filter(elecKart_ga.data,product_mrp <= 50000)

####################################################################################################
#################Merging the data by week and creating KPI's #######################################
####################################################################################################

#holiday - calculating the number of promotions in a week
promotion_vector <- c(seq.dates("07/18/2015", "07/19/2015", by = "days"),
              (seq.dates("08/15/2015", "08/17/2015", by = "days")),
              (seq.dates("08/28/2015", "08/30/2015", by = "days")),
              (seq.dates("10/07/2015", "10/15/2015", by = "days")),
              (seq.dates("11/07/2015", "11/14/2015", by = "days")),
              (seq.dates("12/25/2015", "01/03/2016", by = "days")),
              (seq.dates("01/20/2016", "01/22/2016", by = "days")),
              (seq.dates("02/01/2016", "02/02/2016", by = "days")),
              (seq.dates("02/20/2016", "02/21/2016", by = "days")),
              (seq.dates("02/14/2016", "02/15/2016", by = "days")),
              (seq.dates("03/07/2016", "03/09/2016", by = "days")),
              (seq.dates("05/25/2016", "05/27/2016", by = "days")))

days_vector <- seq(as.Date("2015-07-01"),as.Date("2016-06-30"),by="days")
week_df <- data.frame(Year=year(days_vector),Month=month(days_vector),Week=week(days_vector))
week_df <- week_df %>% group_by(Year,Month,Week) %>% summarise(ndays_in_week=n())

special_day_df <- data.frame(ifelse(days_vector %in% promotion_vector,1,0))
colnames(special_day_df) <- c("is_special_day")
special_day_df$Week <- week(days_vector)
special_day_df$Month <- month(days_vector)
special_day_df$Year <- year(days_vector)
# convert weeknum to continuous from 2015 to 2016
special_day_df$Week <- ifelse(special_day_df$Year==2016,special_day_df$Week+53,special_day_df$Week)

promotion_weekly_df <- special_day_df %>% group_by(Week) %>% summarise(npromotion_in_week=sum(is_special_day))

################################################################################
#aggregate the data by group and create the required KPI's
elecKart_ca.final <- elecKart_ca.data %>% group_by(Week) %>% 
  summarise(
    total_gmv = sum(gmv),
    norders = n(),
    nunits = sum(units),
    avg_deliverybdays = round(mean(deliverybdays)),
    avg_deliverycdays = round(mean(deliverycdays)),
    avg_sla = round(mean(sla)),
    tot_productmrp = sum(product_mrp),
    avg_productmrp = round(mean(product_mrp)),
    avg_prod_proc_sla = round(mean(product_procurement_sla)),
    discount_per = round(((tot_productmrp - total_gmv)/tot_productmrp)* 100,2),
    cod_orders = round((sum(s1_fact.order_payment_type_COD)/norders) * 100,2),
    earlyDelivery_orders = round((sum(delivery_statusEarly)/norders) * 100,2),
    onTimeDelivery_orders = round((sum(delivery_statusOn.time)/norders) * 100,2),
    repeated_orders = round(((n() - length(unique(cust_id)))/n()) * 100,2),
    value_per_visitor = sum(gmv)/length(unique(cust_id))
  )

elecKart_ha.final <- elecKart_ha.data %>% group_by(Week) %>% 
  summarise(
    total_gmv = sum(gmv),
    norders = n(),
    nunits = sum(units),
    avg_deliverybdays = round(mean(deliverybdays)),
    avg_deliverycdays = round(mean(deliverycdays)),
    avg_sla = round(mean(sla)),
    tot_productmrp = sum(product_mrp),
    avg_productmrp = round(mean(product_mrp)),
    avg_prod_proc_sla = round(mean(product_procurement_sla)),
    discount_per = round(((tot_productmrp - total_gmv)/tot_productmrp)* 100,2),
    cod_orders = round((sum(s1_fact.order_payment_type_COD)/norders) * 100,2),
    earlyDelivery_orders = round((sum(delivery_statusEarly)/norders) * 100,2),
    onTimeDelivery_orders = round((sum(delivery_statusOn.time)/norders) * 100,2),
    repeated_orders = round(((n() - length(unique(cust_id)))/n()) * 100,2),
    value_per_visitor = sum(gmv)/length(unique(cust_id))
  )

elecKart_ga.final <- elecKart_ga.data %>% group_by(Week) %>% 
  summarise(
    total_gmv = sum(gmv),
    norders = n(),
    nunits = sum(units),
    avg_deliverybdays = round(mean(deliverybdays)),
    avg_deliverycdays = round(mean(deliverycdays)),
    avg_sla = round(mean(sla)),
    tot_productmrp = sum(product_mrp),
    avg_productmrp = round(mean(product_mrp)),
    avg_prod_proc_sla = round(mean(product_procurement_sla)),
    discount_per = round(((tot_productmrp - total_gmv)/tot_productmrp)* 100,2),
    cod_orders = round((sum(s1_fact.order_payment_type_COD)/norders) * 100,2),
    earlyDelivery_orders = round((sum(delivery_statusEarly)/norders) * 100,2),
    onTimeDelivery_orders = round((sum(delivery_statusOn.time)/norders) * 100,2),
    repeated_orders = round(((n() - length(unique(cust_id)))/n()) * 100,2),
    value_per_visitor = sum(gmv)/length(unique(cust_id))
  )



######################################### Media Investment ###############################################
# Read media investment sheet 
media_df <- read_excel("data\\Media data and other information.xlsx", sheet = "Media Investment", range="B3:M15")

# Calculate number of days in month
media_df$ndays_in_month <- monthDays(as.Date(paste(media_df$Year,media_df$Month,"1",sep = "-")))
str(media_df)

# check is na present in media df
sapply(media_df, function(x) sum(is.na(x)))
# NAs present in Radio and Other columns

# replace NA with 0
media_df[is.na(media_df)] <- 0

# Merge media df and weekly df on month basis
media_weekly_df <- merge(week_df,media_df, by=c("Year","Month"), all.x = TRUE)

# Proportion Monthly investment at weekly level based on number of days in week / number of days in month
media_weekly_df[,5:14] <- data.frame(sapply(media_weekly_df[,5:14],function(x) round((x*media_weekly_df$ndays_in_week/media_weekly_df$ndays_in_month),2)))

# convert weeknum to continuous from 2015 to 2016
media_weekly_df$Week <- ifelse(media_weekly_df$Year==2016,media_weekly_df$Week+53,media_weekly_df$Week)

# weeks can spill over month, so aggregate
media_weekly_df <- data.frame(media_weekly_df %>% group_by(Week) %>%  summarise(Total_Investment = sum(`Total Investment`),
                                                                                   TV = sum(TV), Digital=sum(Digital),
                                                                                   Sponsorship = sum(Sponsorship), 
                                                                                   Content_Marketing = sum(`Content Marketing`),
                                                                                   Online_Marketing = sum(`Online marketing`), 
                                                                                   Affiliates = sum(Affiliates), SEM = sum(SEM), 
                                                                                   Radio = sum(Radio), Other = sum(Other)))


#################################### NPS ###########################################################
# Read media investment sheet 
nps_df <- read_excel("data\\Media data and other information.xlsx", sheet = "Monthly NPS Score", range="B1:M3")

# Convert Month'15/16 to Month-2015/16 format
nps_df[1,] <- gsub("([A-z]{1,3})([a-z]?)(')([1-9]{1,2})","20\\4-\\1",nps_df[1,])

# transpose data frame
t_nps_df <- transpose(nps_df)
colnames(t_nps_df) <- c("MonthYear","NPS")
str(t_nps_df)
# round NPS score to 2 digits after decimal place
t_nps_df$NPS <- round(as.numeric(t_nps_df$NPS),2)
# Remove whitespaces in MonthYear string; convert to lower case, convert to date time
t_nps_df$MonthYear <- str_trim(t_nps_df$MonthYear)
t_nps_df$MonthYear <- tolower(t_nps_df$MonthYear)
t_nps_df$MonthYear <- paste(t_nps_df$MonthYear,"-1",sep="")  
t_nps_df$MonthYear <- as.Date(t_nps_df$MonthYear,format="%Y-%b-%d")
# extract year, month from date
t_nps_df$Year <- year(t_nps_df$MonthYear)
t_nps_df$Month <- month(t_nps_df$MonthYear)
# remove Monthyear column
drop.columns.names <- c("MonthYear")
t_nps_df  <- data.frame(drop_columns(t_nps_df,drop.columns.names))

# Convert monthly NPS to weekly by replicating month's NPS to weeks
nps_weekly_df <- merge(week_df,t_nps_df,by=c("Year","Month"),all.x = TRUE)

# convert weeknum to continuous from 2015 to 2016
nps_weekly_df$Week <- ifelse(nps_weekly_df$Year==2016,nps_weekly_df$Week+53,nps_weekly_df$Week)


# Effectively for complete weeks in month, NPS is same as month's NPS
# For weekly which spill across months, NPS is average of the two months' NPS
nps_weekly_df <- data.frame(nps_weekly_df %>% group_by(Week) %>% summarise(NPS=round(mean(NPS),2)))


#################### Calculate Adstock ##################################
# Assuming adstock rate of 50% in first week and 10% in 2nd week
adstock_weekly_df <- media_weekly_df
adstock_weekly_df[,2:11] <- data.frame(stats::filter(x=media_weekly_df[,-c(1)], c(0.5,0.1), method="recursive"))
adstock_weekly_df[,2:11] <- sapply(adstock_weekly_df[,2:11],function(x) round(x,2))

#########################################################################

# Merging Adstock, NPS and main data frame per sub category
master_ca_final.data <- merge(elecKart_ca.final,adstock_weekly_df,by="Week")
master_ca_final.data <- merge(master_ca_final.data,nps_weekly_df,by="Week")
master_ca_final.data <- merge(master_ca_final.data,promotion_weekly_df,by="Week")

master_ha_final.data <- merge(elecKart_ha.final,adstock_weekly_df,by="Week")
master_ha_final.data <- merge(master_ha_final.data,nps_weekly_df,by="Week")
master_ha_final.data <- merge(master_ha_final.data,promotion_weekly_df,by="Week")

master_ga_final.data <- merge(elecKart_ga.final,adstock_weekly_df,by="Week")
master_ga_final.data <- merge(master_ga_final.data,nps_weekly_df,by="Week")
master_ga_final.data <- merge(master_ga_final.data,promotion_weekly_df,by="Week")

master_ca_final.data$week_has_promotion <- ifelse(master_ca_final.data$npromotion_in_week !=0,1,0)
master_ha_final.data$week_has_promotion <- ifelse(master_ha_final.data$npromotion_in_week !=0,1,0)
master_ga_final.data$week_has_promotion <- ifelse(master_ga_final.data$npromotion_in_week !=0,1,0)

############################################################################
### END of Data preparation ##########

#####################################################################################################
################################Exploratory Data Analysis ###########################################
#####################################################################################################

theme_set(theme_cowplot(font_size = 10))

bar_theme<- theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1), 
                   legend.position="none") 
title_theme <- theme(plot.title = element_text(hjust=0.5, face="bold"))


###1. sales by week

p1 <- ggplot(master_ca_final.data, aes(x=Week, y= total_gmv)) + geom_line() +
  labs(x="week", y="total sales", title = "Camera accessory") + title_theme

p2<- ggplot(master_ga_final.data, aes(x=Week, y= total_gmv)) + geom_line()+
  labs(x="week", y="total sales", title = "Gaming accessory") + title_theme

p3 <- ggplot(master_ha_final.data, aes(x=Week, y= total_gmv)) + geom_line() + 
  labs(x="Week", y="total sales",title = "Home audio") + title_theme

plot_grid(p1,p2,p3, ncol=2, align = 'h')

###2. orders by week

p1 <- ggplot(master_ca_final.data, aes(x=Week, y= nunits)) + geom_line() +
  labs(x="week", y="Units Sold", title = "Camera Accessories") + title_theme

p2<- ggplot(master_ga_final.data, aes(x=Week, y= nunits)) + geom_line()+
  labs(x="week", y="Units Sold", title = "Gaming accessory") + title_theme

p3 <- ggplot(master_ha_final.data, aes(x=Week, y= nunits)) + geom_line() + 
  labs(x="Week", y="Units Sold",title = "Home audio") + title_theme

plot_grid(p1,p2,p3, ncol=2, align = 'h')

###3. discount distribution

p1 <- ggplot(master_ca_final.data, aes(x=discount_per)) + geom_histogram() + labs(title="Camera Accessory") + title_theme
p2 <- ggplot(master_ga_final.data, aes(x=discount_per)) + geom_histogram() + labs(title="Gaming Accessory") + title_theme
p3 <- ggplot(master_ha_final.data, aes(x=discount_per)) + geom_histogram() + labs(title="Home Audio") + title_theme

plot_grid(p1,p2,p3, ncol=2, align = 'h')

###gmv vs weeks considering the special days
p1 <- ggplot(master_ca_final.data, aes(x=Week, y= total_gmv, fill=factor(week_has_promotion))) + geom_bar(stat = "identity", width=0.5) +
  labs(x= "week", y="total sales", title="Camera Accessory") + title_theme

p2 <- ggplot(master_ga_final.data, aes(x=Week, y= total_gmv, fill=factor(week_has_promotion))) + geom_bar(stat = "identity", width=0.5) +
  labs(x= "week", y="total sales", title="Gaming Accessory") + title_theme

p3 <- ggplot(master_ha_final.data, aes(x=Week, y= total_gmv, fill=factor(week_has_promotion))) + geom_bar(stat = "identity", width=0.5) +
  labs(x= "week", y="total sales", title="Home Audio") + title_theme

plot_grid(p1,p2,p3, ncol=2, align = 'h')

###4. discount vs sales
#binning the discount percent
discount_vs_sales <- master_ca_final.data[,c("total_gmv","discount_per")]
discount_vs_sales$discount_range <- ifelse (discount_vs_sales$discount_per <= 10 , 'up to 10', ifelse ( discount_vs_sales$discount_per > 10 & discount_vs_sales$discount_per <= 30 , 'up to 30', ifelse(discount_vs_sales$discount_per > 30 & discount_vs_sales$discount_per <= 50 , 'up to 50','>50') ))
ca_dis_summary <- discount_vs_sales %>% group_by(discount_range) %>% summarise( avg_sale = mean(total_gmv))
p1 <- ggplot(ca_dis_summary,aes( x=discount_range , y=avg_sale)) + geom_bar(stat = "identity",width=0.5,fill="blue") +
  geom_text(aes(label=round(avg_sale,2)), vjust=-0.5, size=4)+
  labs(x="discount range", y="Average Sales", title = "Camera Accessories") + title_theme


discount_vs_sales_ga <- master_ga_final.data[,c("total_gmv","discount_per")]
discount_vs_sales_ga$discount_range <- ifelse (discount_vs_sales_ga$discount_per <= 10 , 'up to 10', ifelse ( discount_vs_sales_ga$discount_per > 10 & discount_vs_sales_ga$discount_per <= 30 , 'up to 30', ifelse(discount_vs_sales_ga$discount_per > 30 & discount_vs_sales_ga$discount_per <= 50 , 'up to 50','>50') ))
ga_dis_summary <- discount_vs_sales_ga %>% group_by(discount_range) %>% summarise( avg_sale = mean(total_gmv))
p2 <- ggplot(ga_dis_summary,aes ( x=discount_range , y =avg_sale  )) + geom_bar(stat = "identity",width=0.5,fill="blue") +
  geom_text(aes(label=round(avg_sale,2)), vjust=-0.5, size=4)+
  labs(x="discount range", y="Average Sales", title = "Gaming Accessories") + title_theme


discount_vs_sales_ha <- master_ha_final.data[,c("total_gmv","discount_per")]
discount_vs_sales_ha$discount_range <- ifelse (discount_vs_sales_ha$discount_per <= 10 , 'up to 10', ifelse ( discount_vs_sales_ha$discount_per > 10 & discount_vs_sales_ha$discount_per <= 30 , 'up to 30', ifelse(discount_vs_sales_ha$discount_per > 30 & discount_vs_sales_ha$discount_per <= 50 , 'up to 50','>50') ))
ha_dis_summary <- discount_vs_sales_ha %>% group_by(discount_range) %>% summarise( avg_sale = mean(total_gmv))
p3 <- ggplot(ha_dis_summary,aes ( x=discount_range , y =avg_sale  )) + geom_bar(stat = "identity",width=0.5,fill="blue") +
  geom_text(aes(label=round(avg_sale,2)), vjust=-0.5, size=4)+
  labs(x="discount range", y="Average Sales", title = "Home Audio") + title_theme

plot_grid(p1,p2,p3, ncol=2, align = 'h')

###5. avg discount during the promotional and non prmotional week
discount_vs_holidays <- master_ca_final.data[,c("npromotion_in_week","discount_per")]
discount_vs_holidays$promotion <- ifelse(discount_vs_holidays$npromotion_in_week > 0, "Yes","No")
disc_vs_holiday_summary <- discount_vs_holidays %>% group_by(promotion) %>% summarise(avg_dis = mean(discount_per))
p1 <- ggplot(disc_vs_holiday_summary,aes( x=promotion , y=avg_dis)) + geom_bar(stat = "identity",width=0.5,fill="blue") +
  geom_text(aes(label=round(avg_dis,2)), vjust=-0.5, size=4)+
  labs(x="Discount Sale", y="Average discount", title = "Camera Accessory") + title_theme


discount_vs_holidays_ga <- master_ga_final.data[,c("npromotion_in_week","discount_per")]
discount_vs_holidays_ga$promotion <- ifelse(discount_vs_holidays_ga$npromotion_in_week > 0, "Yes","No")
disc_vs_holiday_summary_ga <- discount_vs_holidays_ga %>% group_by(promotion) %>% summarise(avg_dis = mean(discount_per))
p2 <- ggplot(disc_vs_holiday_summary_ga,aes( x=promotion , y=avg_dis)) + geom_bar(stat = "identity",width=0.5,fill="blue") +
  geom_text(aes(label=round(avg_dis,2)), vjust=-0.5, size=4)+
  labs(x="Discount Sale", y="Average discount", title = "Gaming Accessory") + title_theme

discount_vs_holidays_ha <- master_ha_final.data[,c("npromotion_in_week","discount_per")]
discount_vs_holidays_ha$promotion <- ifelse(discount_vs_holidays_ha$npromotion_in_week > 0, "Yes","No")
disc_vs_holiday_summary_ha <- discount_vs_holidays_ha %>% group_by(promotion) %>% summarise(avg_dis = mean(discount_per))
p3 <- ggplot(disc_vs_holiday_summary_ha,aes( x=promotion , y=avg_dis)) + geom_bar(stat = "identity",width=0.5,fill="blue") +
  geom_text(aes(label=round(avg_dis,2)), vjust=-0.5, size=4)+
  labs(x="Discount Sale", y="Average discount", title = "Home Audio") + title_theme

plot_grid(p1,p2,p3, ncol=2, align = 'h')

#percent COD orders by Week considering special days
p1 <- ggplot(master_ca_final.data, aes(x=Week, y= cod_orders, fill=factor(week_has_promotion))) + geom_bar(stat="identity", width=0.5) +
  labs(x="week", y= "percent cod orders", title = "Camera Accessory") + title_theme

p2 <- ggplot(master_ga_final.data, aes(x=Week, y= cod_orders, fill=factor(week_has_promotion))) + geom_bar(stat="identity", width=0.5) +
  labs(x="week", y= "percent cod orders", title = "Gaming Accessory") + title_theme

p3 <- ggplot(master_ha_final.data, aes(x=Week, y= cod_orders, fill=factor(week_has_promotion))) + geom_bar(stat="identity", width=0.5) +
  labs(x="week", y= "percent cod orders", title = "Gaming Accessory") + title_theme

plot_grid(p1,p2,p3, ncol=2, align = 'h')

#media spent vs sales

##TV spent
p1 <- ggplot(master_ca_final.data, aes(x=TV, y= total_gmv)) + geom_line() +
  labs(x="Ad Spent", y="total sales", title = "Camera accessory") + title_theme

p2<- ggplot(master_ga_final.data, aes(x=TV, y= total_gmv)) + geom_line()+
  labs(x="Ad Spent", y="total sales", title = "Gaming accessory") + title_theme

p3 <- ggplot(master_ha_final.data, aes(x=TV, y= total_gmv)) + geom_line() + 
  labs(x="Ad Spent", y="total sales",title = "Home audio") + title_theme

plot_grid(p1,p2,p3, ncol=2, align = 'h')

#Digital spent

p1 <- ggplot(master_ca_final.data, aes(x=Digital, y= total_gmv)) + geom_line() +
  labs(x="Ad Spent", y="total sales", title = "Camera accessory") + title_theme

p2<- ggplot(master_ga_final.data, aes(x=Digital, y= total_gmv)) + geom_line()+
  labs(x="Ad Spent", y="total sales", title = "Gaming accessory") + title_theme

p3 <- ggplot(master_ha_final.data, aes(x=Digital, y= total_gmv)) + geom_line() + 
  labs(x="Ad Spent", y="total sales",title = "Home audio") + title_theme

plot_grid(p1,p2,p3, ncol=2, align = 'h')

#Sponsorship Spent
p1 <- ggplot(master_ca_final.data, aes(x=Sponsorship, y= total_gmv)) + geom_line() +
  labs(x="Ad Spent", y="total sales", title = "Camera accessory") + title_theme

p2<- ggplot(master_ga_final.data, aes(x=Sponsorship, y= total_gmv)) + geom_line()+
  labs(x="Ad Spent", y="total sales", title = "Gaming accessory") + title_theme

p3 <- ggplot(master_ha_final.data, aes(x=Sponsorship, y= total_gmv)) + geom_line() + 
  labs(x="Ad Spent", y="total sales",title = "Home audio") + title_theme

plot_grid(p1,p2,p3, ncol=2, align = 'h')

#Content Marketing

p1 <- ggplot(master_ca_final.data, aes(x=Content_Marketing, y= total_gmv)) + geom_line() +
  labs(x="Ad Spent", y="total sales", title = "Camera accessory") + title_theme

p2<- ggplot(master_ga_final.data, aes(x=Content_Marketing, y= total_gmv)) + geom_line()+
  labs(x="Ad Spent", y="total sales", title = "Gaming accessory") + title_theme

p3 <- ggplot(master_ha_final.data, aes(x=Content_Marketing, y= total_gmv)) + geom_line() + 
  labs(x="Ad Spent", y="toatl sales",title = "Home audio") + title_theme

plot_grid(p1,p2,p3, ncol=2, align = 'h')

#online marketing vs Sales
p1 <- ggplot(master_ca_final.data, aes(x=Online_Marketing, y= total_gmv)) + geom_line() +
  labs(x="Ad Spent", y="total sales", title = "Camera accessory") + title_theme

p2<- ggplot(master_ga_final.data, aes(x=Online_Marketing, y= total_gmv)) + geom_line()+
  labs(x="Ad Spent", y="total sales", title = "Gaming accessory") + title_theme

p3 <- ggplot(master_ha_final.data, aes(x=Online_Marketing, y= total_gmv)) + geom_line() + 
  labs(x="Ad Spent", y="toatl sales",title = "Home audio") + title_theme

plot_grid(p1,p2,p3, ncol=2, align = 'h')

#affiliates vs Sales
p1 <- ggplot(master_ca_final.data, aes(x=Affiliates, y= total_gmv)) + geom_line() +
  labs(x="Ad Spent", y="total sales", title = "Camera accessory") + title_theme

p2<- ggplot(master_ga_final.data, aes(x=Affiliates, y= total_gmv)) + geom_line()+
  labs(x="Ad Spent", y="total sales", title = "Gaming accessory") + title_theme

p3 <- ggplot(master_ha_final.data, aes(x=Affiliates, y= total_gmv)) + geom_line() + 
  labs(x="Ad Spent", y="toatl sales",title = "Home audio") + title_theme

plot_grid(p1,p2,p3, ncol=2, align = 'h')

#SEM vs Sales

p1 <- ggplot(master_ca_final.data, aes(x=SEM, y= total_gmv)) + geom_line() +
  labs(x="Ad Spent", y="total sales", title = "Camera accessory") + title_theme

p2<- ggplot(master_ga_final.data, aes(x=SEM, y= total_gmv)) + geom_line()+
  labs(x="Ad Spent", y="total sales", title = "Gaming accessory") + title_theme

p3 <- ggplot(master_ha_final.data, aes(x=SEM, y= total_gmv)) + geom_line() + 
  labs(x="Ad Spent", y="toatl sales",title = "Home audio") + title_theme

plot_grid(p1,p2,p3, ncol=2, align = 'h')

#Radio vs Sales

p1 <- ggplot(master_ca_final.data, aes(x=Radio, y= total_gmv)) + geom_line() +
  labs(x="Ad Spent", y="total sales", title = "Camera accessory") + title_theme

p2<- ggplot(master_ga_final.data, aes(x=Radio, y= total_gmv)) + geom_line()+
  labs(x="Ad Spent", y="total sales", title = "Gaming accessory") + title_theme

p3 <- ggplot(master_ha_final.data, aes(x=Radio, y= total_gmv)) + geom_line() + 
  labs(x="Ad Spent", y="toatl sales",title = "Home audio") + title_theme

plot_grid(p1,p2,p3, ncol=2, align = 'h')

#other vs Sales

p1 <- ggplot(master_ca_final.data, aes(x=Other, y= total_gmv)) + geom_line() +
  labs(x="Ad Spent", y="total sales", title = "Camera accessory") + title_theme

p2<- ggplot(master_ga_final.data, aes(x=Other, y= total_gmv)) + geom_line()+
  labs(x="Ad Spent", y="total sales", title = "Gaming accessory") + title_theme

p3 <- ggplot(master_ha_final.data, aes(x=Other, y= total_gmv)) + geom_line() + 
  labs(x="Ad Spent", y="toatl sales",title = "Home audio") + title_theme

plot_grid(p1,p2,p3, ncol=2, align = 'h')


#NPS vs Sales

p1 <- ggplot(master_ca_final.data, aes(x=NPS, y= total_gmv)) + geom_line() +
  labs(x="NPS", y="total sales", title = "Camera accessory") + title_theme

p2<- ggplot(master_ga_final.data, aes(x=NPS, y= total_gmv)) + geom_line()+
  labs(x="NPS", y="total sales", title = "Gaming accessory") + title_theme

p3 <- ggplot(master_ha_final.data, aes(x=NPS, y= total_gmv)) + geom_line() + 
  labs(x="NPS", y="toatl sales",title = "Home audio") + title_theme

plot_grid(p1,p2,p3, ncol=2, align = 'h')

#NPS vs repeated orders

p1 <- ggplot(master_ca_final.data, aes(x=NPS, y= repeated_orders)) + geom_line() +
  labs(x="Ad Spent", y="Repeated Customers", title = "Camera accessory") + title_theme

p2<- ggplot(master_ga_final.data, aes(x=NPS, y= repeated_orders)) + geom_line()+
  labs(x="Ad Spent", y="Repeated Customers", title = "Gaming accessory") + title_theme

p3 <- ggplot(master_ha_final.data, aes(x=NPS, y= repeated_orders)) + geom_line() + 
  labs(x="Ad Spent", y="Repeated Customers",title = "Home audio") + title_theme

plot_grid(p1,p2,p3, ncol=2, align = 'h')

##### Quantitative Bivariate analysis ###############
#create a coorelation matrix for all numeric fields
cor_matrix <- cor(master_ca_final.data)
corrplot(cor_matrix, method="square")

cor_matrix <- cor(master_ha_final.data)
corrplot(cor_matrix, method="square")

cor_matrix <- cor(master_ga_final.data)
corrplot(cor_matrix, method="square")

################################################################################
# Write Master Data frames for individual categories to CSV files for Model generation
write.csv(master_ca_final.data,file="data\\master_ca_final.csv",row.names = FALSE)
write.csv(master_ha_final.data,file="data\\master_ha_final.csv",row.names = FALSE)
write.csv(master_ga_final.data,file="data\\master_ga_final.csv",row.names = FALSE)

################################################################################