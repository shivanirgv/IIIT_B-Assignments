#Business Understanding
#You work for a consumer finance company which specialises in lending various types of loans to urban customers.When the company receives 
#a loan application, the company has to make a decision for loan approval based on the applicant’s profile. Two types of risks are associated 
#with the bank’s decision:If the applicant is likely to repay the loan, then not approving the loan results in a loss of business to the 
#company
#If the applicant is not likely to repay the loan, i.e. he/she is likely to default, then approving the loan may lead to a financial loss 
#for the company
#
#The data given below contains the information about past loan applicants and whether they ‘defaulted’ or not. The aim is to identify
#patterns which indicate if a person is likely to default, which may be used for taking actions such as denying the loan, reducing the amount
#of loan, lending (to risky applicants) at a higher interest rate, etc.
#When a person applies for a loan, there are two types of decisions that could be taken by the company:
#Loan accepted: If the company approves the loan, there are 3 possible scenarios described below:
#Fully paid: Applicant has fully paid the loan (the principal and the interest rate)
#Current: Applicant is in the process of paying the instalments, i.e. the tenure of the loan is not yet completed. These candidates are not 
#labelled as 'defaulted'.
#Charged-off: Applicant has not paid the instalments in due time for a long period of time, i.e. he/she has defaulted on the loan 
#Loan rejected: The company had rejected the loan (because the candidate does not meet their requirements etc.). Since the loan was rejected, 
#there is no transactional history of those applicants with the company and so this data is not available with the company (and thus in this
#dataset)

# Business Objectives
#This company is the largest online loan marketplace, facilitating personal loans, business loans, and financing of medical procedures. 
#Borrowers can easily access lower interest rate loans through a fast online interface. 
#Like most other lending companies, lending loans to ‘risky’ applicants is the largest source of financial loss (called credit loss). 
#The credit loss is the amount of money lost by the lender when the borrower refuses to pay or runs away with the money owed. In other words, borrowers who default cause the largest amount of loss to the lenders. In this case, the customers labelled as 'charged-off' are the 'defaulters'. 
#If one is able to identify these risky loan applicants, then such loans can be reduced thereby cutting down the amount of credit loss.
#Identification of such applicants using EDA is the aim of this case study.
#In other words, the company wants to understand the driving factors (or driver variables) behind loan default, i.e. the variables which
#are strong indicators of default.  The company can utilise this knowledge for its portfolio and risk assessment. 
#To develop your understanding of the domain, you are advised to independently research a little about risk analytics (understanding the
#types of variables and their significance should be enough).


library(tidyr)
library(dplyr)

#Set working directory
#Commenting it out for submission
#setwd("C:\\Users\\Kishore\\Downloads\\Surya\\Program\\Investment Case Study\\Data")


#For all data imports: 
#1)All strings are loaded as character vectors and not factors
#2)Leading and Traling whitespaces are trimmed
#3)Blank values are replace with NA

##################Checkpoint 1: Data Cleaning 1##########################################

#Load the companies text file into companies dataframe. 
companies <- read.delim(file = "companies.txt", stringsAsFactors = FALSE,strip.white = TRUE,na.strings=c("","NA"))

#Load the rounds2 csv file into rounds2 dataframe. 
rounds2 <- read.csv(file="rounds2.csv", stringsAsFactors = FALSE,strip.white = TRUE,na.strings=c("","NA"))

#Length of unique companies in companies data frame
#permalink is the primary key in companies data frame
length(unique(companies$permalink))

#Length of unique companies in rounds2 data frame
#company_permalink is the foreign key in rounds2 data frame correspoding to permalink
#primary key in companies data frame
length(unique(rounds2$company_permalink))

#convert the keys to match to the same case so that merge operation works as expected
rounds2$company_permalink <- toupper(rounds2$company_permalink)
companies$permalink <- toupper(companies$permalink)

#Inner join the rounds2 and companies based on the company_permalink and permalink keys  
master_frame <- merge(rounds2,companies,by.x ="company_permalink",by.y = "permalink")

#since the merge was an inner join
#if number of records in the merged dataframe equals
#original record count in rounds dataframe
if(length(rounds2$company_permalink) == length(master_frame$company_permalink)){print("All records in rounds2 dataframe matched with records in companies table")
  }else
  { print("All records in rounds2 dataframe did not match in companies table")}

##################Checkpoint 2: Funding Type Analysis##########################################


#Calculate Average funding amount based on funding round type 
avgfunding_byfundingtype= aggregate(master_frame$raised_amount_usd, by=list(master_frame$funding_round_type),FUN = mean,na.rm=TRUE)

#Select the funding type that suits the investment style i.e.  >5 million and < 15million
avgfunding_byfundingtype[avgfunding_byfundingtype$x > 5000000 & avgfunding_byfundingtype$x <= 15000000, ]


##################Checkpoint 3: Country Analysis##########################################

#create a subset of master based on funding round type "venture"
datasubset_venture_ft <- subset(master_frame,master_frame$funding_round_type == "venture")
#Get funding grouped by country in the venture subset
sumfunding_bycountry <- aggregate(datasubset_venture_ft$raised_amount_usd,by=list(datasubset_venture_ft$country_code),FUN=sum, na.rm=TRUE)
sumfunding_bycountry <- sumfunding_bycountry[order(-sumfunding_bycountry$x),]

#top nine countries which have received the highest total funding for funding_round_type="venture" in descending order
top_9 <- head(sumfunding_bycountry,9)

########## countrycodes.csv INCLUDED IN SUBMISSION NEEDS TO PLACED IN WORKING DIRECTORY################################
#Load the countrycodes csv file into country_codes dataframe
#countrycodes.csv was created based on data obtained from the link - #https://www.worldatlas.com/aatlas/ctycodes.htm
#It contains 3 digit country code mapping against every country in the world
country_codes <- read.csv(file="countrycodes.csv", stringsAsFactors = FALSE, fill = TRUE,strip.white = TRUE,na.strings=c("","NA"))

########## english_spk_countries.csv INCLUDED IN SUBMISSION NEEDS TO PLACED IN WORKING DIRECTORY################################
#Load the english_spk_countries csv file into english_spk_countries dataframe
#english_spk_countries.csv was created based on information provided in the course content
eng_spk_countries <- read.csv(file="english_spk_countries.csv",stringsAsFactors = FALSE, fill = TRUE,strip.white = TRUE,na.strings=c("","NA"))

#merge the eng_spk_countries and the country_codes dataframes using country as a key
#country is converted to upper case in both dataframes for proper matching during merge
country_codes$country <- toupper(trimws(country_codes$country))
eng_spk_countries$country <- toupper(trimws(eng_spk_countries$country))
eng_spk_countries = merge(eng_spk_countries,country_codes,by="country" )

#eng_spk_countries$country[which(is.na(match(eng_spk_countries$country, country_codes$country)))]

#English speaking countries
english_speaking <-match(top_9$Group.1,eng_spk_countries$code)
eng_speaking_rows <- which(!is.na(english_speaking))
non_eng_speaking_rows <- which(is.na(english_speaking))
english_speaking[eng_speaking_rows] <- "English Speaking"
english_speaking[non_eng_speaking_rows] <- "Non English Speaking"

#Included additional column to identify if the country is english speaking or not
top_9 <- cbind(top_9,english_speaking)

##################Checkpoint 4: Sector Analysis 1##########################################

#getsplitpart function that will accept a string, characters to split the string with and the index of the split part 
#to be returned.
getsplitpart <- function(...,splitchars,splitpartindex)
{
  strsplit(...,splitchars)[[1]][splitpartindex]
}

#Using sapply create a new column primary_sector in master_frame 
#by applying getsplitpart function on category_list

master_frame$primary_sector <- sapply(master_frame$category_list,getsplitpart,splitchars="\\|",splitpartindex=1)

#Load the mapping csv file into mapping dataframe
mapping <- read.csv(file="mapping.csv",stringsAsFactors = FALSE, fill = TRUE,strip.white = TRUE,na.strings=c("","NA"))

#create a new column main_sector in the mapping data frame 
mapping <- gather(mapping,main_sector,my_value,Automotive...Sports:Social..Finance..Analytics..Advertising)
#Remove all the asymmetric values
mapping <- mapping[!(mapping$my_value ==0),]
#Remove the my_value column as it has all 1's and is redundant
mapping <- mapping[,-3]

#merge the main sector column with master_frame matching with category as key and left join (all.x=TRUE)
master_frame <- merge(master_frame,mapping,by.x = "primary_sector",by.y = "category_list",all.x = TRUE)
write.table(master_frame,"master_data.csv", row.names = FALSE)

#Quality Check(additional activty not requested in the assignment)
#Get the list of indices for which main_sector is NA
index_sectors_withoutmapping <-which(is.na(master_frame$main_sector))
#Extract the primary sector records for which main sector is NA
records_withoutsectormapping <- master_frame[index_sectors_withoutmapping,"primary_sector"]
#Get the length of records with out sector mapping
lengthrecords_withoutsectormapping <-length(records_withoutsectormapping)
#Get the list of unique primary sectors without main sector mapping 
uniquesectors_withoutmapping <- unique(records_withoutsectormapping)
#Write the information into a csv file for reporting purpose
write.table(uniquesectors_withoutmapping,"sectorswithnomapping.csv",row.names = FALSE)

#93 primary sectors didn't find any mapping in the mapping sheet. 
#main_sector for those records was marked as NA and excluded from analysis  

##################Checkpoint 5: Sector Analysis 2##########################################

#Create D1,D2,D3 subsets for top 3 countries(USA,UK and IND) and venture type funding
D1 <- subset(master_frame,master_frame$funding_round_type=="venture" & master_frame$country_code=="USA" & master_frame$raised_amount_usd >= 5000000 & master_frame$raised_amount_usd <= 15000000)
D2 <- subset(master_frame,master_frame$funding_round_type=="venture" & master_frame$country_code=="GBR" & master_frame$raised_amount_usd >= 5000000 & master_frame$raised_amount_usd <= 15000000)
D3 <- subset(master_frame,master_frame$funding_round_type=="venture" & master_frame$country_code=="IND" & master_frame$raised_amount_usd >= 5000000 & master_frame$raised_amount_usd <= 15000000)


# Add 2 new columns to D1 dataframe for count of investments and total amount invested per main sector
D1 <- group_by(D1,main_sector)
D1 <- mutate(D1,no_inv_main_sector=n(),amt_invested_main_sector=sum(raised_amount_usd,na.rm = TRUE))
D1 <- D1[order(-D1$no_inv_main_sector,-D1$amt_invested_main_sector),]

# Add 2 new columns to D2 dataframe for count of investments and total amount invested per main sector
D2 <- group_by(D2,main_sector)
D2 <- mutate(D2,no_inv_main_sector=n(),amt_invested_main_sector=sum(raised_amount_usd,na.rm = TRUE))
D2 <- D2[order(-D2$no_inv_main_sector,-D2$amt_invested_main_sector),]

# Add 2 new columns to D3 dataframe for count of investments and total amount invested per main sector
D3 <- group_by(D3,main_sector)
D3 <- mutate(D3,no_inv_main_sector=n(),amt_invested_main_sector=sum(raised_amount_usd,na.rm = TRUE))
D3 <- D3[order(-D3$no_inv_main_sector,-D3$amt_invested_main_sector),]

#Get the total number of investments in the top 3 countries USA,UK and IND
#Logic = no of records in the data frame is equal to the number(count) of investments
totalnoinv_USA <- nrow(D1)
totalnoinv_GBR <- nrow(D2)
totalnoinv_IND <- nrow(D3)

#Get the total amount raised in the top 3 countries USA,UK and IND
totalamtinv_USA <-sum(D1$raised_amount_usd,na.rm = TRUE)
totalamtinv_GBR <-sum(D2$raised_amount_usd,na.rm = TRUE)
totalamtinv_IND <-sum(D3$raised_amount_usd,na.rm = TRUE)

#Get the count of investments made for each sector in each of the top 3 countries
#conversion to factor is useful to get a summary
summary(as.factor(D1$main_sector))
summary(as.factor(D2$main_sector))
summary(as.factor(D3$main_sector))

#Convert main sector column into a table and then data frame for further processing
#converting to table also gives the record count for each main sector
investmentsinUSA_bymainsector <- data.frame(table(D1$main_sector))
#Order the data frame based on the record count desc
investmentsinUSA_bymainsector <-investmentsinUSA_bymainsector[order(-investmentsinUSA_bymainsector$Freq),]
#Pick the top 3 sectors based on the recrod count from the sorted list
usa_top3sectorsbyinvcount <- head(investmentsinUSA_bymainsector,3)


#Convert main sector column into a table and then data frame for further processing
#converting to table also gives the record count for each main sector
investmentsinGBR_bymainsector <- data.frame(table(D2$main_sector))
#Order the data frame based on the record count desc
investmentsinGBR_bymainsector <-investmentsinGBR_bymainsector[order(-investmentsinGBR_bymainsector$Freq),]
#Pick the top 3 sectors based on the recrod count from the sorted list
gbr_top3sectorsbyinvcount <- head(investmentsinGBR_bymainsector,3)

#Convert main sector column into a table and then data frame for further processing
#converting to table also gives the record count for each main sector
investmentsinIND_bymainsector <- data.frame(table(D3$main_sector))
#Order the data frame based on the record count desc
investmentsinIND_bymainsector <-investmentsinIND_bymainsector[order(-investmentsinIND_bymainsector$Freq),]
#Pick the top 3 sectors based on the recrod count from the sorted list
ind_top3sectorsbyinvcount <- head(investmentsinIND_bymainsector,3)


#create subset for top 3 main sectors in USA
#create an aggregate on the subset based on raised amount and group by company name
#order the resultant aggregate in descending order
D1_top3sectors <-D1[(which(D1$main_sector %in% usa_top3sectorsbyinvcount$Var1)),]
#Aggregate sum investments grouped by main sector and company name within each main sector
fundingbycompany_topsectorUSA <- aggregate(D1_top3sectors$raised_amount_usd, by=list(D1_top3sectors$main_sector,D1_top3sectors$name),FUN=sum, na.rm=TRUE)
#sort the data frame based on main sector and sum investments descending
fundingbycompany_topsectorUSA <- arrange(fundingbycompany_topsectorUSA,Group.1,desc(x))

#create subset for top 3 main sectors in GBR
#create an aggregate on the subset based on raised amount and group by company name
#order the resultant aggregate in descending order
D2_top3sectors <-D2[(which(D2$main_sector %in% gbr_top3sectorsbyinvcount$Var1)),]
#Aggregate sum investments grouped by main sector and company name within each main sector
fundingbycompany_topsectorGBR <- aggregate(D2_top3sectors$raised_amount_usd, by=list(D2_top3sectors$main_sector,D2_top3sectors$name),FUN=sum, na.rm=TRUE)
#sort the data frame based on main sector and sum investments descending
fundingbycompany_topsectorGBR <- arrange(fundingbycompany_topsectorGBR,Group.1,desc(x))

#create subset for top 3 main sectors in IND
#create an aggregate on the subset based on raised amount and group by company name
#order the resultant aggregate in descending order
D3_top3sectors <-D3[(which(D3$main_sector %in% ind_top3sectorsbyinvcount$Var1)),]
#Aggregate sum investments grouped by main sector and company name within each main sector
fundingbycompany_topsectorIND <- aggregate(D3_top3sectors$raised_amount_usd, by=list(D3_top3sectors$main_sector,D3_top3sectors$name),FUN=sum, na.rm=TRUE)
#sort the data frame based on main sector and sum investments descending
fundingbycompany_topsectorIND <- arrange(fundingbycompany_topsectorIND,Group.1,desc(x))
