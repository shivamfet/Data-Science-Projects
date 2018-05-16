library(dplyr)
library(ggplot2)
library(lubridate)
library(reshape2)

###################
#Data Understanding
###################

####################
#CUSTOMER ATTRIBUTES
####################

#1. emp_length
#2. emp_title
#3. address
#4. url
#5. zip_code
#6. home_ownership
#7. verification_status
#8. annual_inc
#9. addr_state
#10.dti
#11.revol_bal
#12. revol_util

################
#LOAN ATTRIBUTES
################

#1. loan_amnt
#2. funded_amnt
#3. funded_amnt_inv
#4. term
#5. int_rate
#6. installment
#7. status
#8. grade
#9. sub grade
#10 issue_dt


#######################
#read the loan.csv file
#######################

loan <- read.csv("loan.csv" , stringsAsFactors = FALSE) 

##############
#Data Cleaning
##############

##########################################################
#Check for duplicate records
##########################################################

sum(duplicated(loan$id)) #0
sum(duplicated(loan$member_id)) #0

###########################################################
#Replace the empty values of "charcter" type columns to NA
###########################################################

loan <- loan %>% mutate_if(is.character, function(x) gsub("^$|^ $", NA, x))

######################################################################
#emp_length column contains few values as "n/a" so changing them to NA
######################################################################

loan$emp_length <- gsub("n/a" , NA , loan$emp_length)

###############################################################################################
#converting the "int_rate" , "revol_util" column to numeric and replacing their percentage sign
################################################################################################

loan$int_rate <- as.numeric(sub("%" , "" , loan$int_rate))
loan$revol_util <- as.numeric(sub("%" , "" , loan$revol_util))

###########################################################
#convert "term" to "factor" and replacing months to empty
###########################################################

loan$term <- as.factor(sub("months" , "" , loan$term))
loan$verification_status <- as.factor(loan$verification_status)
loan$annual_inc <- as.integer(loan$annual_inc)

sum(is.na(loan))  #2263566

####################################
#Columns in which all are NA values:
####################################

cols_all_na <- sapply(loan , function(x)all(is.na(x)))
cols_all_na <- names(cols_all_na[cols_all_na > 0])
cols_all_na

#############################################
#Imputing the columns where all are NA values
#############################################

loan <- loan[ , !(names(loan) %in% cols_all_na)]
sum(is.na(loan))  #118848

#############################################
#Columns that have more than 60% values as NA
#############################################

cols_na_percentage <- sapply(loan, function(col)sum(is.na(col))*100/length(col))
cols_na_percentage
cols_na_percentage_gt_60 <- names(cols_na_percentage[cols_na_percentage > 60])
cols_na_percentage_gt_60  #mths_since_last_delinq" "mths_since_last_record" "next_pymnt_d"

##########################################################
#Imputing the columns that have more than 60% values as NA
##########################################################

loan <- loan[ , !(names(loan) %in% cols_na_percentage_gt_60)]
sum(is.na(loan))  #17658

cnt_uniq_val_per_column <- sapply(loan, function(x)length(unique(na.omit(x))))
col_uniq_val_eq_1 <- names(cnt_uniq_val_per_column[cnt_uniq_val_per_column == 1])
col_uniq_val_eq_1

###################################################
#Imputing the columns that have only 1 unique value
###################################################

loan <- loan[ , !(names(loan) %in% col_uniq_val_eq_1)]
sum(is.na(loan)) #17507

pub_rec <- loan %>% group_by(pub_rec) %>% summarise(count = n()) %>% mutate(perc = (count/sum(count))*100)
pub_rec_bankruptcies <- loan %>% group_by(pub_rec_bankruptcies) %>% summarise(count = n()) %>% mutate(perc = (count/sum(count))*100)
delinq_2yrs <- loan %>% group_by(delinq_2yrs) %>% summarise(count = n()) %>% mutate(perc = (count/sum(count))*100)
recoveries <- loan %>% group_by(recoveries) %>% summarise(count = n()) %>% mutate(perc = (count/sum(count))*100)
total_rec_late_fee <- loan %>% group_by(total_rec_late_fee) %>% summarise(count = n()) %>% mutate(perc = (count/sum(count))*100)

###########################################
#From the above 
# "pub_rec" - 95% values are 0
# "pub_rec_bankruptcies" - 95% values are 0
# "delinq_2yrs", 90% values are 0, 
#  "recoveries" , 90% values are 0.
#  "total_rec_late_fee , 95 % values are 0
# We can impute these cols
###########################################

#Also imputing the following columns

#out_prncp - high % of values are zero  -- 97% values are zero
#out_prncp_inv - high % of values are zero -- 97% values are zero

#Following cols provide info about the 

###########################
#"CUSTOMER PAYMENT BEHAVIOR"
###########################

#1: last_paymnt_amnt :
#2: last_pymnt_d :
#3: total_pymnt
#4: total_pymnt_inv
#5: total_rec_prncp
#6: total_rec_int



unreq_cols <- c("pub_rec" , "pub_rec_bankruptcies" , "delinq_2yrs" , "recoveries" , "collection_recovery_fee" , "out_prncp" , "out_prncp_inv" ,
                               "total_rec_late_fee" , "last_pymnt_amnt" , "last_pymnt_d" , "total_pymnt",
                               "total_pymnt_inv" , "total_rec_prncp" , "total_rec_int")


loan <- loan[ , !names(loan) %in% unreq_cols]
sum(is.na(loan)) #16739

#################################################################################
#Imputing the not required columns : desc , zip_code , addr_state, url , emp_title
# As these columns will not help in pur analysis
#################################################################################

unreq_cols <- c("desc" , "zip_code" , "addr_state" , "url" , "emp_title" , "id" , "member_id" , "title")
loan <- loan[ , !names(loan) %in% unreq_cols]

sum(is.na(loan)) #1127

###################################################################
## Impute total_acc as we are interested in accounts that are open:
###################################################################
loan$total_acc <- NULL
loan$earliest_cr_line <-NULL
loan$last_credit_pull_d <- NULL

#######################
#Cols that has NA value
#######################

NA_cols <- sapply(loan , function(x) sum(is.na(x)))
NA_cols[NA_cols > 0] 

# emp_length has 1075 NA , since it is nearly 3% of the dataset we can impute these rows:

loan <- na.omit(loan)
sum(is.na(loan)) #0

##############################
##Filtering out charged loans
#############################

charged_off <- subset(loan , loan_status == "Charged Off")

charged_off[sapply(charged_off, is.character)] <- lapply(charged_off[sapply(charged_off, is.character)], 
                                       as.factor)

####################################################
#Function to generate plot for categorical variable
####################################################

gen_univariate_plot_categorical <- function(df , var , title , xlab) {
  ggplot(data = df , aes_string(x = var)) + 
    geom_bar(aes(y = (..count..)/sum(..count..)) , col = "red" , fill = "green" , alpha = 0.6 ) +
    geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
    scale_y_continuous(labels = function(x){ paste0(x*100, "%") }) +
    labs(title = title  , x = xlab , y = "Defaulted Borrowers") 
}

###################################################
#Function to generate plot for continuous variable
###################################################

gen_univariate_plot_continuous <- function(df , var , title , xlab , start_point , end_point , diff , theme) {
  if (theme) {
    ggplot(data = df , aes_string(x = var)) + 
      stat_bin(binwidth = diff, breaks=seq(start_point, end_point , diff), col = "red" , fill = "green" , alpha = 0.5) +
      stat_bin(binwidth= diff, breaks=seq(start_point, end_point , diff) , geom="text", aes(label=scales::percent((..count..)/sum(..count..))) , vjust=-0.75 , size = 2.9) +
      scale_x_continuous(breaks=seq(start_point, end_point , diff)) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5)) +
      labs(title = title , x = xlab , y = "Num Of Defaulted Borrowers") 
  } 
  else {
    ggplot(data = df , aes_string(x = var)) + 
    stat_bin(binwidth = diff, breaks=seq(start_point, end_point , diff), col = "red" , fill = "green" , alpha = 0.5) +
    stat_bin(binwidth= diff, breaks=seq(start_point, end_point , diff) , geom="text", aes(label=scales::percent((..count..)/sum(..count..))) , vjust=-0.75 , size = 2.9) +
    scale_x_continuous(breaks=seq(start_point, end_point , diff)) +
    #geom_histogram(breaks = seq(start_point , end_point , diff) , col = "red" , fill = "green" , alpha = 0.5) +
    #geom_text(aes(label=(..count..)), size = 3 , vjust = 0.5) +
    labs(title = title , x = xlab , y = "Num Of Defaulted Borrowers") 
  }
  
}

#################################################################
#Function to generate plot for Continuous / Categorical variable
#################################################################

gen_multivariate_plot_cont_cat <- function(df , var1 , var2 , title , xlab , fill_title , start_point , end_point ,diff , theme) {
  if (theme) {
    ggplot(data = df , aes_string(x = var1 , fill = var2)) + 
      stat_bin(binwidth = diff, breaks=seq(start_point, end_point , diff), col = "red" , alpha = 0.5) +
      stat_bin(binwidth= diff, breaks=seq(start_point, end_point , diff) , geom="text", aes(label=scales::percent((..count..)/sum(..count..))) , position = position_stack(vjust = 0.6) , size = 2.3) +
      scale_x_continuous(breaks = seq(start_point , end_point, diff)) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5)) +
      labs(title = title , x = xlab , y = "Num Of Defaulted Borrowers" , fill = fill_title) 
  } else {
    ggplot(data = df , aes_string(x = var1 , fill = var2)) + 
      stat_bin(binwidth = diff, breaks=seq(start_point, end_point , diff), col = "red" , alpha = 0.5) +
      stat_bin(binwidth= diff, breaks=seq(start_point, end_point , diff) , geom="text", aes(label=scales::percent((..count..)/sum(..count..))) , position = position_stack(vjust = 0.6) , size = 2.3) +
      scale_x_continuous(breaks = seq(start_point , end_point, diff)) +
      labs(title = title , x = xlab , y = "Num Of Defaulted Borrowers" , fill = fill_title) 
  }
}

gen_multivariate_plot_cont_cat2 <- function(df , var1 , var2 , title , xlab , fill_title , start_point , end_point ,diff , theme) {
  if (theme) {
    ggplot(data = df , aes_string(x = var1 , fill = var2)) + 
      geom_histogram(breaks = seq(start_point , end_point, diff) , col = "red" , alpha = 0.5) + 
      scale_x_continuous(breaks = seq(start_point , end_point, diff)) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5)) +
      labs(title = title , x = xlab , y = "Num Of Defaulted Borrowers" , fill = fill_title) 
  } else {
    ggplot(data = df , aes_string(x = var1 , fill = var2)) + 
      geom_histogram(breaks = seq(start_point, end_point , diff) , col = "red" , alpha = 0.5) + 
      scale_x_continuous(breaks = seq(start_point , end_point, diff)) +
      labs(title = title , x = xlab , y = "Num Of Defaulted Borrowers" , fill = fill_title) 
  }
}
  
#################################################################
#Function to generate plot for Categorical / Categorical variable
##################################################################

gen_multivariate_plot_cat_cat <- function(df , var1 , var2 , title , fill_title , xlab) {
    ggplot( data = df , aes_string(x = var1 , fill = var2)) + 
      geom_bar(aes(y = (..count..)/sum(..count..)) ,col = "red" , alpha = 0.5) +
      geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", position = position_stack(vjust = 0.6) , size = 2.3) +
      scale_y_continuous(labels = function(x){ paste0(x*100, "%") }) +
      labs(title = title , x = xlab , y = "Num Of Defaulted Borrowers" , fill = fill_title) 
}

##############################
#Function to generate Box Plot
##############################

gen_boxplot <- function(df , x , y ,title, xlab , ylab) {
    ggplot(data = df , aes_string(x = x , y = y)) +
    geom_boxplot() +
    labs(title = title , x = xlab , y = ylab)
}


##################################################
#UNIVARIATE ANALYSIS FOR ALL CONTINUOUS VARIABLES
##################################################

##############
#For int_rate
##############

gen_univariate_plot_continuous(charged_off , "int_rate" , "Defaulted Loan - Interest Rate" , "Interest Rate" , 0 , 30, 5 , FALSE)

######################################################
#Creating a derived column - int_range from "int_rate"
######################################################

# < 10 % - "0 - 10"
# > 10 < = 20 - "10-20"
# > 20

charged_off$int_range <- ifelse(charged_off$int_rate >= 0 & charged_off$int_rate <= 10 , "0 - 10" ,
                                ifelse(charged_off$int_rate > 10 & charged_off$int_rate <= 20 , "10 - 20" , "20 - 25"))

##################
#For Annual Income
##################

gen_univariate_plot_continuous(charged_off , "annual_inc" , "Defaulted Loan - Annual Income" , "Annual Income" , 0 , 500000, 10000 , TRUE)

#########################################################
#Creating a derived column - income_level from annual_inc
#########################################################

# 0 - 20K ---- "LOW ANNUAL INCOME"
# 20K - 80K ---- "MEDIUM ANNUAL INCOME"
# > 80K ------- "HIGH ANNUAL INCOME"

charged_off$annual_inc_range <- ifelse(charged_off$annual_inc >= 0 & charged_off$annual_inc <= 20000 , "LOW" ,
                                       ifelse(charged_off$annual_inc > 20000 & charged_off$annual_inc <= 80000 , "MEDIUM" , "HIGH"))

##################
#For Funded Amount
##################

gen_univariate_plot_continuous(charged_off , "funded_amnt" , "Defaulted Loan - Funded Amount" , "Funded Amount" , 0 , 40000, 10000 , FALSE)

#################################################################
#Creating a derived column - funded_amnt_range from "funded_amnt"
#################################################################

# 0 - 10K - "LOW"
# 10K - 20K - "MEDIUM"
# 20K - 30K - "HIGH"
# > 30K - "VERY HIGH"

charged_off$funded_amnt_range <- ifelse(charged_off$funded_amnt >= 0 & charged_off$funded_amnt <= 10000 , "LOW" ,
                                        ifelse(charged_off$funded_amnt > 10000 & charged_off$funded_amnt <= 20000 , "MEDIUM" , 
                                               ifelse(charged_off$funded_amnt > 20000 & charged_off$funded_amnt <= 30000 , "HIGH" , "VERY HIGH")))
################
#For Installment
################

gen_univariate_plot_continuous(charged_off , "installment" , "Defaulted Loan - Installment" , "Installment" , 0 , 1350, 50 , FALSE)

###############################################################
#Creating a derived column - installment_level from installment
###############################################################

# - -0-150 -- "LOW INSTALLMENT"
# - -150- 350 - "MEDIUM INSTALLMENT"
# -- > 350 -- "HIGH INSTALLMENT"

charged_off$installment_range <- ifelse(charged_off$installment >= 0 & charged_off$installment <= 150 , "LOW" ,
                                        ifelse(charged_off$installment > 150 & charged_off$installment <= 400 , "MEDIUM" , "HIGH"))

#########
#For DTI
#########

gen_univariate_plot_continuous(charged_off , "dti" , "Defaulted Loan - DTI" , "Debt to income ratio" , 0 , 30 , 10 , FALSE)

##########################
#For Revolving Utilization
##########################

gen_univariate_plot_continuous(charged_off , "revol_util" , "Defaulted Loan - Revolving Utilization" , "Revolving Utilization" , 0 , 100 , 10 , FALSE)

#######################
#For Revolving Balance
#######################

gen_univariate_plot_continuous(charged_off , "revol_bal" , "Defaulted Loan - Revolving Balance" , "Revolving Balance" , 0 , 150000 , 10000 , FALSE)

######################
#For Open Credit Lines
######################

gen_univariate_plot_continuous(charged_off , "open_acc" , "Defaulted Loan - Open Credit Lines" , "Open Credit Lines"   , 0 , 40 , 2 , FALSE)

#########################################################
#Creating a derived column - open_acc_range from open_acc
#########################################################

charged_off$open_acc_range <- ifelse(charged_off$open_acc >= 0 & charged_off$open_acc <= 2 , "0-4" ,
                                        ifelse(charged_off$open_acc > 4 & charged_off$open_acc <= 12 , "4-12" , ">12"))


############################################################
#UNIVARIATE ANALYSIS FOR ALL CATEGORICAL / DERIVED VARIABLES
############################################################

###################
#For Home Ownership
###################

gen_univariate_plot_categorical(charged_off , "home_ownership" , "Defaulted loans - Home ownership" , "Home ownership")

#############
# For Purpose
#############

gen_univariate_plot_categorical(charged_off , "purpose" , "Defaulted loans - Purpose" , "Purpose")

########################
#For Verification Status
########################

gen_univariate_plot_categorical(charged_off , "verification_status" , "Defaulted loans - Verification Status" , "Verification Status")

###########
#For Grade
##########

gen_univariate_plot_categorical(charged_off , "grade" , "Defaulted loan - Grade" , "Grade")

#########################
#For Length of employment
#########################

gen_univariate_plot_categorical(charged_off , "emp_length" , "Defaulted loan - Length Of Employment" , "Length of employment")

###################
# For term of loan
###################

gen_univariate_plot_categorical(charged_off , "term" , "Defaulted loan - Term" , "Term")

#############
#For Interest
#############

gen_univariate_plot_categorical(charged_off , "int_range" , "Defaulted loan - Interest Range" , "Interest Range")

########################
#For funded Amount Range
########################

gen_univariate_plot_categorical(charged_off , "funded_amnt_range" , "Defaulted loan - Funded Amount" , "Funded Amount")

########################
#For Annual Income Range
########################

gen_univariate_plot_categorical(charged_off , "annual_inc_range" , "Defaulted loan - Annual Income Level" , "Annual Income Level")

######################
#For Installment Range
######################

gen_univariate_plot_categorical(charged_off , "installment_range" , "Defaulted loan - Installment Level" , "Installment")

###################
#For open_acc_range
###################

gen_univariate_plot_categorical(charged_off , "open_acc_range" , "Defaulted loan - Open Credit Lines" , "Open Credit Lines Range")

######################
#MULTIVARIATE ANALYSIS
######################

###############################################################################################
# Generating ggplot for all the continuous variables - with "int_range" as categorical variable
###############################################################################################

gen_multivariate_plot_cont_cat2(charged_off , "annual_inc" , "int_range" , "Defaulted Loan - Annual Income / Range Of Interest" , "Annual Income" , "Range Of Interest" , 0 , 500000, 10000 , TRUE)
gen_multivariate_plot_cont_cat(charged_off , "annual_inc" , "int_range" , "Defaulted Loan - Annual Income / Range Of Interest" , "Annual Income" , "Range Of Interest" , 0 , 500000, 10000 , TRUE)

gen_multivariate_plot_cont_cat2(charged_off , "funded_amnt" , "int_range" , "Defaulted Loan - Funded Amount / Range Of Interest" , "Funded Amount" , "Range Of Interest" , 0 , 40000, 10000 , FALSE)
gen_multivariate_plot_cont_cat(charged_off , "funded_amnt" , "int_range" , "Defaulted Loan - Funded Amount / Range Of Interest" , "Funded Amount" , "Range Of Interest" , 0 , 40000, 10000 , FALSE)

gen_multivariate_plot_cont_cat2(charged_off , "installment" , "int_range" , "Defaulted Loan - Installment / Range Of Interest" , "Installment" , "Range Of Interest" ,  0 , 1350, 50 , FALSE)
gen_multivariate_plot_cont_cat(charged_off , "installment" , "int_range" , "Defaulted Loan - Installment / Range Of Interest" , "Installment" , "Range Of Interest" ,  0 , 1350, 50 , FALSE)

gen_multivariate_plot_cont_cat(charged_off , "dti" , "int_range" , "Defaulted Loan - Debt To Income Ratio / Range Of Interest" , "Debt To Income Ratio" , "Range Of Interest" ,  0 , 30, 5 , FALSE)
gen_multivariate_plot_cont_cat(charged_off , "revol_bal" , "int_range" , "Defaulted Loan - Revolving Balance / Range Of Interest" , "Revolving Balance" , "Range Of Interest" , 0 , 150000, 5000 , FALSE)
gen_multivariate_plot_cont_cat(charged_off , "revol_util" , "int_range" , "Defaulted Loan - Revolving Utilization / Range Of Interest" , "Revolving Utilization" , "Range Of Interest" , 0 , 100, 10 , FALSE)
gen_multivariate_plot_cont_cat(charged_off , "open_acc" , "int_range" , "Defaulted Loan - Open Credit Lines / Range Of Interest" , "Open Credit Lines"   , "Range Of Interest",  0 , 40 , 2 , FALSE)

#######################################################################################
# Generating ggplot for all the continuous variables - with "term" categorical variable
#######################################################################################

gen_multivariate_plot_cont_cat(charged_off , "annual_inc" , "term" , "Defaulted Loan - Annual Income / Term" , "Annual Income" , "Term" , 0 , 500000, 10000 , TRUE)
gen_multivariate_plot_cont_cat(charged_off , "funded_amnt" , "term" , "Defaulted Loan - Funded Amount / Term" , "Funded Amount" ,"Term" , 0 , 40000, 10000 , FALSE)
gen_multivariate_plot_cont_cat(charged_off , "installment" , "term" , "Defaulted Loan - Installment / Term" , "Installment" , "Term" , 0 , 1350, 50 , FALSE)
gen_multivariate_plot_cont_cat(charged_off , "dti" , "term" , "Defaulted Loan - Debt To Income Ratio / Term" , "Debt To Income Ratio" , "Term" , 0 , 30, 5 , FALSE)
gen_multivariate_plot_cont_cat(charged_off , "revol_bal" , "term" , "Defaulted Loan - Revolving Balance / Term" , "Revolving Balance" , "Term"  ,0 , 150000, 5000 , FALSE)
gen_multivariate_plot_cont_cat(charged_off , "revol_util" , "term" , "Defaulted Loan - Revolving Utilization / Term" , "Revolving Utilization" , "Term" , 0 , 100, 10 , FALSE)

#################################################################################################
# Generating ggplot for all the continuous variables - with "home_ownership" categorical variable
#################################################################################################

gen_multivariate_plot_cont_cat(charged_off , "annual_inc" , "home_ownership" , "Defaulted Loan - Annual Income / Home Ownership" , "Annual Income" , "Home Ownership" , 0 , 500000, 10000 , TRUE)
gen_multivariate_plot_cont_cat(charged_off , "funded_amnt" , "home_ownership" , "Defaulted Loan - Funded Amount / Home Ownership" , "Funded Amount" , "Home Ownership" ,0 , 40000, 10000 , FALSE)
gen_multivariate_plot_cont_cat(charged_off , "installment" , "home_ownership" , "Defaulted Loan - Installment / Homw Ownership" , "Installment" , "Home Ownership" , 0 , 1350, 50 , FALSE)
gen_multivariate_plot_cont_cat(charged_off , "dti" , "home_ownership" , "Defaulted Loan - Debt To Income Ratio / Home Ownership" , "Debt To Income Ratio" , "Home Ownership" , 0 , 30, 5 , FALSE)
gen_multivariate_plot_cont_cat(charged_off , "revol_bal" , "home_ownership" , "Defaulted Loan - Revolving Balance / Home Ownership" , "Revolving Balance" , "Home Ownership" , 0 , 150000, 5000 , FALSE)
gen_multivariate_plot_cont_cat(charged_off , "revol_util" , "home_ownership" , "Defaulted Loan - Revolving Utilization / Homw Ownership" , "Revolving Utilization" , "Home Ownership" , 0 , 100, 10 , FALSE)

######################################################################################################
# Generating ggplot for all the continuous variables - with "verification_status" categorical variable
######################################################################################################

gen_multivariate_plot_cont_cat(charged_off , "annual_inc" , "verification_status" , "Defaulted Loan - Annual Income  / Verification Status" , "Annual Income" , "Verification Status" , 0 , 500000, 10000 , TRUE)
gen_multivariate_plot_cont_cat(charged_off , "funded_amnt" , "verification_status" , "Defaulted Loan - Funded Amount / Verification Status" , "Funded Amount" , "Verification Status" , 0 , 40000, 10000 , FALSE)
gen_multivariate_plot_cont_cat(charged_off , "installment" , "verification_status" , "Defaulted Loan - Installment / Verification Status" , "Installment" , "Verification Status" , 0 , 1350, 50 , FALSE)
gen_multivariate_plot_cont_cat(charged_off , "dti" , "verification_status" , "Defaulted Loan - Debt To Income Ratio / Verification Status" , "Debt To Income Ratio" , "Verification Status", 0 , 30, 5 , FALSE)
gen_multivariate_plot_cont_cat(charged_off , "revol_bal" , "verification_status" , "Defaulted Loan - Revolving Balance / Verification Status" , "Revolving Balance" , "Verification Status", 0 , 150000, 5000 , FALSE)
gen_multivariate_plot_cont_cat(charged_off , "revol_util" , "verification_status" , "Defaulted Loan - Revolving Utilization / Verification Status" , "Revolving Utilization" , "Verification Status" ,0 , 100, 10 , FALSE)

###############################################################################
# Generating ggplot to see the relation between following categrical variables
# Lenth of employment - Range of Interest
# Purpose - Range of Interest
# Home Ownership - Range of Interest
# Verification Status - Range of Interest
# Grade - Range of Interest
################################################################################

gen_multivariate_plot_cat_cat(charged_off , "funded_amnt_range" , "int_range" , "Defaulted Loan - Funded Amount / Range of Interest" , "Range Of Interest" , "Funded Amount Range")
gen_multivariate_plot_cat_cat(charged_off , "funded_amnt_range" , "term" , "Defaulted Loan - Funded Amount / Term" , "Term" , "Funded Amount Range")
gen_multivariate_plot_cat_cat(charged_off , "funded_amnt_range" , "home_ownership" , "Defaulted Loan - Funded Amount / Home Ownership" , "Home Ownership" , "Funded Amount Range")
gen_multivariate_plot_cat_cat(charged_off , "funded_amnt_range" , "annual_inc_range" , "Defaulted Loan - Funded Amount Range / Annual Income Level" , "Annual Income Level" , "Funded Amount Range")
gen_multivariate_plot_cat_cat(charged_off , "funded_amnt_range" , "installment_range" , "Defaulted Loan - Funded Amount Range / Installment" , "Installment Level" , "Funded Amount Range")
gen_multivariate_plot_cat_cat(charged_off , "funded_amnt_range" , "purpose" , "Defaulted Loan - Funded Amount Range / Purpose" , "Purpose" , "Funded Amount Range")
gen_multivariate_plot_cat_cat(charged_off , "funded_amnt_range" , "grade" , "Defaulted Loan - Funded Amount Range / Purpose" , "Purpose" , "Funded Amount Range")

gen_multivariate_plot_cat_cat(charged_off , "annual_inc_range" , "int_range" , "Defaulted Loan - Annual Income / Range of Interest" , "Range Of Interest" , "Annual Income Range")
gen_multivariate_plot_cat_cat(charged_off , "annual_inc_range" , "term" , "Defaulted Loan - Annual Income / Term" , "Term" , "Annual Income Range")
gen_multivariate_plot_cat_cat(charged_off , "annual_inc_range" , "home_ownership" , "Defaulted Loan - Annual Income / Home Ownership" , "Home Ownership", "Annual Income Range")
gen_multivariate_plot_cat_cat(charged_off , "annual_inc_range" , "installment_range" , "Defaulted Loan - Annual Income Range / Installment Range" , "Installment Level" , "Annual Income Range")
gen_multivariate_plot_cat_cat(charged_off , "annual_inc_range" , "purpose" , "Defaulted Loan - Annual Income Level / Purpose" , "Purpose" , "annual Income Range")

gen_multivariate_plot_cat_cat(charged_off , "installment_range" , "int_range" , "Defaulted Loan - Installment / Range of Interest" , "Range Of Interest" , "Installment Range")
gen_multivariate_plot_cat_cat(charged_off , "installment_range" , "term" , "Defaulted Loan - Installment / Term" , "Term" , "Installment Range")
gen_multivariate_plot_cat_cat(charged_off , "installment_range" , "home_ownership" , "Defaulted Loan - Installment / Term" , "Home Ownership" , "Installment Range")
gen_multivariate_plot_cat_cat(charged_off , "installment_range" , "purpose" , "Defaulted Loan - Installment / Purpose" , "Purpose" , "Installment Range")

gen_multivariate_plot_cat_cat(charged_off , "emp_length" , "int_range" , "Defaulted Loan - Lenth of employment / Range of interest" , "Range of Interest" , "Length Of Employment")
gen_multivariate_plot_cat_cat(charged_off , "purpose" , "int_range" , "Defaulted Loan - Purpose / Range of interest" , "Range of Interest" , "Purpose")
gen_multivariate_plot_cat_cat(charged_off , "home_ownership" , "int_range" , "Defaulted Loan - Home Ownership / Range of interest" , "Range of Interest" , "Home Ownership")
gen_multivariate_plot_cat_cat(charged_off , "grade" , "int_range" , "Defaulted Loan - Interest_range / Grade" , "Grade" , "Range of Interest")
gen_multivariate_plot_cat_cat(charged_off , "emp_length" , "purpose" , "Defaulted Loan - Employment Length / Purpose" , "Purpose" , "employment length")
gen_multivariate_plot_cat_cat(charged_off , "open_acc_range" , "term" , "Defaulted Loan - Open Credit Lines / Term" , "Term" , "Open Credit Lines")



#funded_amnt_range & int_range

d1 <- charged_off %>% group_by(funded_amnt_range , int_range) %>% summarise(count = n()) %>% mutate(perc = (count/sum(count))*100)
d2 <- charged_off %>% group_by(funded_amnt_range , term) %>% summarise(count = n()) %>% mutate(perc = (count/sum(count))*100)
d3 <- charged_off %>% group_by(funded_amnt_range , home_ownership) %>% summarise(count = n()) %>% mutate(perc = (count/sum(count))*100)
d4 <- charged_off %>% group_by(funded_amnt_range , annual_inc_range) %>% summarise(count = n()) %>% mutate(perc = (count/sum(count))*100)
d5 <- charged_off %>% group_by(funded_amnt_range , installment_range) %>% summarise(count = n()) %>% mutate(perc = (count/sum(count))*100)
d6 <- charged_off %>% group_by(funded_amnt_range , purpose) %>% summarise(count = n()) %>% mutate(perc = (count/sum(count))*100)
d7 <- charged_off %>% group_by(funded_amnt_range , grade) %>% summarise(count = n()) %>% mutate(perc = (count/sum(count))*100)

d8 <- charged_off %>% group_by(annual_inc_range , int_range) %>% summarise(count = n()) %>% mutate(perc = (count/sum(count))*100)
d9 <- charged_off %>% group_by(annual_inc_range , term) %>% summarise(count = n()) %>% mutate(perc = (count/sum(count))*100)
d10 <- charged_off %>% group_by(annual_inc_range , home_ownership) %>% summarise(count = n()) %>% mutate(perc = (count/sum(count))*100)
d11 <- charged_off %>% group_by(annual_inc_range , installment_range) %>% summarise(count = n()) %>% mutate(perc = (count/sum(count))*100)
d12 <- charged_off %>% group_by(annual_inc_range , purpose) %>% summarise(count = n()) %>% mutate(perc = (count/sum(count))*100)

d13 <- charged_off %>% group_by(installment_range , int_range) %>% summarise(count = n()) %>% mutate(perc = (count/sum(count))*100)
d14 <- charged_off %>% group_by(installment_range , term) %>% summarise(count = n()) %>% mutate(perc = (count/sum(count))*100)
d15 <- charged_off %>% group_by(installment_range , home_ownership) %>% summarise(count = n()) %>% mutate(perc = (count/sum(count))*100)
d16 <- charged_off %>% group_by(installment_range , purpose) %>% summarise(count = n()) %>% mutate(perc = (count/sum(count))*100)

d17 <- charged_off %>% group_by(emp_length , int_range) %>% summarise(count = n()) %>% mutate(perc = (count/sum(count))*100)
d18 <- charged_off %>% group_by(purpose , int_range) %>% summarise(count = n()) %>% mutate(perc = (count/sum(count))*100)
d19 <- charged_off %>% group_by(home_ownership , int_range) %>% summarise(count = n()) %>% mutate(perc = (count/sum(count))*100)
d20 <- charged_off %>% group_by(grade , int_range) %>% summarise(count = n()) %>% mutate(perc = (count/sum(count))*100)
d21 <- charged_off %>% group_by(emp_length , purpose) %>% summarise(count = n()) %>% mutate(perc = (count/sum(count))*100)
d22 <- charged_off %>% group_by(open_acc_range , term) %>% summarise(count = n()) %>% mutate(perc = (count/sum(count))*100)

gen_plot <- function(df , x , fill , title , xlab) {
  ggplot( data = df , aes_string(x = x, y = "perc" , fill = fill)) + 
  geom_col(position = "stack" , width = 0.3) +
  geom_text(aes(label=scales::percent(perc/100)), size = 3, position = position_stack(vjust = 0.6)) +
  labs(title = title , x = xlab , y = "Defaulted Borrowers" , fill = fill) +  
  scale_y_continuous(labels = function(x){ paste0(x, "%") }) 
}


gen_plot(d1 , "funded_amnt_range" , "int_range" , "Defaulted Loan - Funded Amount / Range of interest" , "Funded Amount")
gen_plot(d2 , "funded_amnt_range" , "term" , "Defaulted Loan - Funded Amount / Term" , "Funded Amount")
gen_plot(d3 , "funded_amnt_range" , "home_ownership" , "Defaulted Loan - Funded Amount / Home Ownership" , "Funded Amount")
gen_plot(d4 , "funded_amnt_range" , "annual_inc_range" , "Defaulted Loan - Funded Amount / Annual Income" , "Funded Amount")
gen_plot(d5 , "funded_amnt_range" , "installment_range" , "Defaulted Loan - Funded Amount / Installment" , "Funded Amount")
gen_plot(d6 , "funded_amnt_range" , "purpose" , "Defaulted Loan - Funded Amount / Purpose" , "Funded Amount")
gen_plot(d7 , "funded_amnt_range" , "grade" , "Defaulted Loan - Funded Amount / Grade" , "Funded Amount")

gen_plot(d8 , "annual_inc_range" , "int_range" , "Defaulted Loan - Annual Income / Interest Range" , "Annual Income")
gen_plot(d9 , "annual_inc_range" , "term" , "Defaulted Loan - Annual Income / Term" , "Annual Income")
gen_plot(d10 , "annual_inc_range" , "home_ownership" , "Defaulted Loan - Annual Income / Home Ownership" , "Annual Income")
gen_plot(d11 , "annual_inc_range" , "installment_range" , "Defaulted Loan - Annual Income / Installment Range" , "Annual Income")
gen_plot(d12 , "annual_inc_range" , "purpose" , "Defaulted Loan - Annual Income / Purpose" , "Annual Income")

gen_plot(d13 , "installment_range" , "int_range" , "Defaulted Loan - Installment / Interest Range" , "Installment")
gen_plot(d14 , "installment_range" , "term" , "Defaulted Loan - Installment / Term" , "Installment")
gen_plot(d15 , "installment_range" , "home_ownership" , "Defaulted Loan - Installment / Home ownership" , "Installment")
gen_plot(d16 , "installment_range" , "purpose" , "Defaulted Loan - Installment / Purpose" , "Installment")

gen_plot(d17 , "emp_length" , "int_range" , "Defaulted Loan - Emp_length / Interest Range" , "Employment Length")
gen_plot(d18 , "purpose" , "int_range" , "Defaulted Loan - Purpose / Interest Range" , "Purpose")
gen_plot(d19 , "home_ownership" , "int_range" , "Defaulted Loan - Home ownership / Interest Range" , "Home Ownership")
gen_plot(d20 , "grade" , "int_range" , "Defaulted Loan - Grade / Interest Range" , "Grade")
gen_plot(d21 , "emp_length" , "purpose" , "Defaulted Loan - Installment / Purpose" , "Employment Length")


################################################
#Correlation between continuous variables
#################################################

####################
# Correlation matrix
####################

charged_off_cont_vars <- sapply(charged_off , is.numeric)
charged_off_cont_vars <- charged_off[ , charged_off_cont_vars]
cormat <- round(cor(charged_off_cont_vars) ,2)
melted_cormat <- melt(cormat)

#########
#HEAT MAP
#########

ggplot(data = melted_cormat , aes(x = Var1 , y = Var2 , fill = value)) + 
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5)) 

#################################################################################################
#From the above plotted Heat Map it is clear, following variables are highly correlated
##################################################################################################

cor(charged_off$loan_amnt , charged_off$funded_amnt)  # - 0.97
cor(charged_off$loan_amnt , charged_off$funded_amnt_inv)  # - 0.90
cor(charged_off$loan_amnt , charged_off$installment)  # - 0.90
cor(charged_off$loan_amnt , charged_off$annual_inc)  # - 0.34 - 

cor(charged_off$funded_amnt , charged_off$annual_inc) #-- Very Low correlation # 0.34
cor(charged_off$funded_amnt , charged_off$installment) #-- High correlation # 0.95

ggplot(charged_off , aes(x = loan_amnt , y = funded_amnt)) + geom_point() + geom_smooth()
ggplot(charged_off , aes(x = installment , y = funded_amnt)) + geom_point() + geom_smooth()
ggplot(charged_off , aes(x = annual_inc , y = funded_amnt)) + geom_smooth()

ggplot(charged_off , aes(x = annual_inc , y = funded_amnt , col = int_range)) + geom_smooth()
ggplot(charged_off , aes(x = annual_inc , y = funded_amnt , col = term)) + geom_smooth()

#############################
# Box plot - to find outliers
#############################

gen_boxplot(charged_off , "int_range" , "funded_amnt" , "Funded Amount - Range Of Interest" ,"Range Of Interest" , "Funded Amount")
gen_boxplot(charged_off , "int_range" , "annual_inc" , "Annual Income - Range Of Interest" ,"Range Of Interest" , "Annual Income")
gen_boxplot(charged_off , "int_range" , "installment" , "Installment - Range Of Interest" ,"Range Of Interest" , "Installment")
gen_boxplot(charged_off , "int_range" , "dti" , "Length Of employment - Range Of Interest" ,"Range Of Interest" , "Length Of Employment")
gen_boxplot(charged_off , "int_range" , "revol_util" , "Revolving Utilization - Range Of Interest" ,"Range Of Interest" , "Revolving Utilization")

gen_boxplot(charged_off , "purpose" , "funded_amnt" , "Funded Amount - purpose" ,"purpose" , "Funded Amount")
gen_boxplot(charged_off , "purpose" , "annual_inc" , "Annual Income - purpose" ,"purpose" , "Annual Income")

######################################
#OVERALL INFERENCE
######################################

d22 <- charged_off %>% group_by(emp_length , purpose , int_range) %>% summarise(count = n()) %>% mutate(perc = (count/sum(count))*100)
d22[which(d22$count == max(d22$count)) ,]
 
#########################################################
# If the anuual_inc_range -- MEDIUM i.e 20k - 80k
#        installment_range -- MEDIUM i.e 150 - 400
#        funded_amnt --- LOW i.e 0 - 10K
#        int_rate ---- 10 - 20 %
#        emp_length --- 10 + years
#        purpose ---- debt consolidation
# then loan is highly likely to default
#########################################################