###################################################
#      HR Analytics Case Study Assignment         #
###################################################

# Group members 
# Shivam Kakkar (Facilitator) - Roll Number - DDA1730346
# Ashwin Suresh
# Manohar Shanmugasundaram
# Sundeep Gupta

library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(MASS)
library(car)
library(e1071)
library(caret)
library(cowplot)
library(caTools)
library(scales)
library(MASS)
library(corrplot)
library(StatMeasures)
library(Information)
library(woe)
library(ROCR)

#################################
# Reading the diferent datasets #
#################################

gen_data <- read.csv("general_data.csv" , stringsAsFactors = FALSE)
emp_data <- read.csv("employee_survey_data.csv" , stringsAsFactors = FALSE)
mgr_data <- read.csv("manager_survey_data.csv" , stringsAsFactors = FALSE)
in_time <- read.csv("in_time.csv" , stringsAsFactors = FALSE)
out_time <- read.csv("out_time.csv" , stringsAsFactors = FALSE)

######################
#Check number of NA's
######################

sum(is.na(gen_data)) #28  
sum(is.na(emp_data)) #83
sum(is.na(mgr_data)) #0
sum(is.na(in_time)) #109080
sum(is.na(out_time)) #109080

#############################
#Check for duplicated records
#############################

sum(duplicated(gen_data$EmployeeID)) #0
sum(duplicated(emp_data$EmployeeID)) #0
sum(duplicated(mgr_data$EmployeeID)) #0
sum(duplicated(in_time$X)) #0
sum(duplicated(out_time$X)) #0

#############################################################################
#Replace the empty values of "charcter" type columns to NA (If there are any)
#############################################################################

replace_emp_vals_to_NA <- function(df) {
  df <- df %>% mutate_if(is.character, function(x) gsub("^$|^ $", NA, x))
}

gen_data <- replace_emp_vals_to_NA(gen_data)
emp_data <- replace_emp_vals_to_NA(emp_data)
in_time <- replace_emp_vals_to_NA(in_time)
out_time <- replace_emp_vals_to_NA(out_time)

#######################
#Rechecking NA values 
#######################

sum(is.na(gen_data)) #28
sum(is.na(emp_data)) #83
sum(is.na(in_time)) #109080
sum(is.na(out_time)) #109080

cols_na_gen_data <- data.frame(sapply(gen_data , function(col)sum(is.na(col))))
cols_na_gen_data # Only 0.6%
gen_data <- na.omit(gen_data) # Very less percentage of na's we can omit those rows:

cols_na_emp_data <- data.frame(sapply(emp_data , function(col)sum(is.na(col))))
cols_na_emp_data # Only 1.8%
emp_data <- na.omit(emp_data) # we are omitting the rows.

cols_na_mgr_data <- data.frame(sapply(mgr_data , function(col)sum(is.na(col))))
cols_na_mgr_data #0

cols_na_in_time <- data.frame(sapply(in_time , function(col)sum(is.na(col))))
cols_na_out_time <- data.frame(sapply(out_time , function(col)sum(is.na(col))))

####################################################################################
#Check unique records for every character column to ensure data is spelled correctly
####################################################################################

check_unique_vals <- function(df){
  char_cols <- sapply(df , is.character)
  char_cols <- names(char_cols[char_cols])  
  df <- df[ , (names(df) %in% char_cols)]
  uniq_recs <- sapply(df , unique)
  return(uniq_recs)
}

uniq_recs_gen_data <- check_unique_vals(gen_data)
uniq_recs_gen_data # No mispelling

uniq_recs_emp_data <- check_unique_vals(emp_data)
uniq_recs_emp_data # No mispelling

uniq_recs_mgr_data <- check_unique_vals(mgr_data)
uniq_recs_mgr_data # No mispelling

######################################################
#Listing those columns that have only one unique value
######################################################

cnt_uniq_val_eq_1 <- function(df) {
  cnt_uniq_val_per_column <- sapply(df, function(x)length(unique(na.omit(x))))
  cols_uniq_val_eq_1 <- names(cnt_uniq_val_per_column[cnt_uniq_val_per_column == 1])
  return(cols_uniq_val_eq_1)
}

cols_uniq_vals_eq_1_emp_data <- cnt_uniq_val_eq_1(emp_data)
cols_uniq_vals_eq_1_emp_data  #0

cols_uniq_vals_eq_1_gen_data <- cnt_uniq_val_eq_1(gen_data)
cols_uniq_vals_eq_1_gen_data  #EmployeeCount , Over18  , StandardHours 

##############################################################################################################
#Hence we are imputing the cols : EmployeeCount , Over18  , StandardHours because they have one 1 unique value
##############################################################################################################

gen_data$EmployeeCount <- NULL
gen_data$Over18 <- NULL
gen_data$StandardHours <- NULL

cols_uniq_vals_eq_1_mgr_data <- cnt_uniq_val_eq_1(mgr_data)
cols_uniq_vals_eq_1_mgr_data #0

################################################
#Check the dates in in_time and out_time matches
################################################

col_intime <- colnames(in_time)
col_outtime <- colnames(out_time)

col_intime <- data.frame(col_intime[-1])
col_outtime <- data.frame(col_outtime[-1])

colnames(col_intime) <- "in_time_col"
colnames(col_outtime) <- "out_time_col"

col_result <- anti_join(col_intime,col_outtime, by=c("in_time_col" = "out_time_col"))
col_result # 0 rows - All columns are matching, i.e, dates in in_time and out_time are matching

#############################################################
#Now caluculating the difference between in_time and out_time
#############################################################

in_time_new <- data.frame(sapply(in_time[2:ncol(in_time)] , function(x)as.POSIXlt(x , origin="1970-01-01", format = "%Y-%m-%d %H:%M:%S")))
out_time_new  <- data.frame(sapply(out_time[2:ncol(out_time)] , function(x)as.POSIXlt(x ,origin="1970-01-01", format = "%Y-%m-%d %H:%M:%S")))

diff <- out_time_new - in_time_new
diff <- data.frame(sapply(diff , as.numeric))

##################################
#Impute the cols that have all NAs:
##################################

cols_all_na <- sapply(diff , function(x)all(is.na(x)))
cols_all_na <- names(cols_all_na[cols_all_na > 0])
cols_all_na #"X2015.01.01" "X2015.01.14" "X2015.01.26" "X2015.03.05" "X2015.05.01" "X2015.07.17" "X2015.09.17" "X2015.10.02" "X2015.11.09"
#"X2015.11.10" "X2015.11.11" "X2015.12.25"

#################################################
#Hence we will impute those cols that all have NA
#################################################

diff <- diff[ , !(names(diff) %in% cols_all_na)]
sum(is.na(diff))  #56160

############################################################################################
#Creating 2 derived metrics : "avg_working_hr_per_day" & "no_of_leaves" employee has taken
############################################################################################

diff$avg_working_hr_per_day <- apply(diff , 1 , mean , na.rm = TRUE)
diff$no_of_leaves <- apply(diff , 1, function(x)sum(is.na(x)))

diff <- diff[, (names(diff) %in% c("avg_working_hr_per_day" , "no_of_leaves"))]

diff <- cbind(in_time$X , diff)
colnames(diff)[1] <- "EmployeeID"

master_data <- merge(gen_data , emp_data , by = "EmployeeID")
master_data <- merge(master_data , mgr_data , by = "EmployeeID")
master_data <- merge(master_data , diff , by = "EmployeeID")

#####################################
#Verifying if any column has NA value
#####################################

cols_na_master_data <- data.frame(sapply(master_data , function(col)sum(is.na(col))))
cols_na_master_data  ##None of the column has NA value now.

##########################################################################################
# Following numeric variables are ordinal in nature so we will convert them to categorical
##########################################################################################

cat_var<-c("Education" , "EnvironmentSatisfaction", "JobInvolvement" , "JobSatisfaction" ,
           "PerformanceRating" , "WorkLifeBalance" ,"JobLevel" , "StockOptionLevel")


#########################################
#Converting the above variables to factor
#########################################

master_data[,cat_var] <- lapply(master_data[,cat_var] , as.factor)


###########################################
#Convert all character types to factor type
###########################################

master_data[sapply(master_data, is.character)] <- lapply(master_data[sapply(master_data, is.character)], 
                                                         as.factor)

#########################################################
#Convert all integer type to numeric to have a consistency
##########################################################

master_data[sapply(master_data, is.integer)] <- lapply(master_data[sapply(master_data, is.integer)], 
                                                       as.numeric)

#################################################################
#Verifying if TotalWorkingYears is not less than "YearsAtCompany"
#################################################################

length(which(master_data$TotalWorkingYears < master_data$YearsAtCompany))   #0  

# -- There is no entry where TotalWorkingYears < YearsAtCompany - This is expected.

###############################################################
#Subsetting the "master_data for employees which have "attrited"
###############################################################

master_data_attr <- subset(master_data , Attrition == "Yes")


##############################
#OUTLIER DETECTION & TREATMENT
##############################

############################################
#Function to find outliers in a given vector
############################################

get_outliers <- function(x) {
  outliers <- outliers(x)
  return(outliers)
}

##################################################
#No Outliers exists in following numeric variables
##################################################

#DistanceFromHome , PercentSalaryHike & Age

ggplot(data = master_data , aes(x = "DistanceFromHome" , y = DistanceFromHome)) + geom_boxplot()
ggplot(data = master_data , aes(x = "PercentSalaryHike" , y = PercentSalaryHike)) + geom_boxplot()
ggplot(data = master_data , aes(x = "Age" , y = Age)) + geom_boxplot()
ggplot(data = master_data , aes(x = "avg_working_hr_per_day" , y = avg_working_hr_per_day)) + geom_boxplot()

##################################
#Following variables have outliers
##################################

###############
#Monthly Income
###############

ggplot(data = master_data , aes(x = "MonthlyIncome" , y = MonthlyIncome)) + geom_boxplot()
outliers <- get_outliers(master_data$MonthlyIncome)
outliers$numOutliers  #331
sort(master_data$MonthlyIncome[outliers$idxOutliers]) # ranging from 165950 - 199990

test <- quantile(master_data$MonthlyIncome , probs = seq(0 , 1 , 0.01))
test
plot(test)

#Capping it to 90 percentile

master_data$MonthlyIncome[which(master_data$MonthlyIncome > 137756.0)] <- 137756.0

###########################################################################################################
#Following is the other way of treating MonthlyIncome Values
# Grouping the records by "Department , JobRole , JobLevel , Education , EducationField , YearsAtCompany"
# Calulating the mean of every group and checking the difference of the "MonthlyIncome" with the mean
# Whereever the difference is more than 10000 in absolute terms , we replace the Monhtly Income with Mean
###########################################################################################################

master_data_1 <- master_data %>% group_by(Department , JobRole , JobLevel , Education , EducationField , YearsAtCompany) %>% summarise(Mean = mean(MonthlyIncome))
master_data_1 <- merge(master_data , master_data_1 , by = c("Department" , "JobRole" , "JobLevel" , "Education" , "EducationField" , "YearsAtCompany"))

#master_data_1 <- df2[ , c("Department" , "JobRole" , "JobLevel" , "Education" , "EducationField" , "MonthlyIncome" , "YearsAtCompany" , "Mean")]

master_data_1$diff <- master_data_1$MonthlyIncome - master_data_1$Mean
master_data_1$ActualIncome <- master_data_1$MonthlyIncome

master_data_1$diff_new <- master_data_1$MonthlyIncome - master_data_1$Mean
master_data_1$MonthlyIncome = ifelse(master_data_1$diff > 10000 | master_data_1$diff < -10000 , master_data_1$Mean , master_data_1$MonthlyIncome)

###################################################################################################################################

####################
#NumCompanies Worked
####################

#Here is our understanding of this variable:
#-------------------------------------------

# NumOfCompaniesWorked - is equal to the num of companies the employee has worked before joining this company.
# If NumOfCompaniesWorked value is "0" , then it means 
# he/she is a fresher in this company and first year in that company is considered as "probation" 
# So thats where for all the records where "NumOfCompanies" worked is "0" the "TotalWorkingYears" is greater "YearsAtCompany" by 1 .

# If the "NumOfCompaniesWorked" is equal to 1 , them there is a possibility he could have worked in that company 
# for less than 1 year so they could have counted that experience as 0 only... 
# So if you filter the records for "NumOfCompaniesWorked" as "1" , then 
# you do the difference between "TotalWorkingYears" - "YearAtCompany" , you will get either "0 or "1".

ggplot(data = master_data , aes(x = "NumCompaniesWorked" , y = NumCompaniesWorked)) + geom_boxplot()
outliers <- get_outliers(master_data$NumCompaniesWorked)  
outliers$numOutliers #154
master_data$NumCompaniesWorked[outliers$idxOutliers] #-- 9 values is coming as outlier

#Since there is a possibilty that employee would have worked for 9 companies

#Verifying the Age of the employees who have worked for 9 companies:

sort(master_data$Age[which(master_data$NumCompaniesWorked == 9)]) # it is coming in the range of 24 - 59
test <- subset(master_data , NumCompaniesWorked == 9 & (Age >= 24 & Age <= 30))

# We can see that even at the Age of 24 , employee has changed 9 companies (which is not realistic)
# we will keep it as it is. Due to lack of info. We are not treating outliers.

####################
#Total Working Years
####################

outliers <- get_outliers(master_data$TotalWorkingYears)
outliers$numOutliers #186
sort(master_data$TotalWorkingYears[outliers$idxOutliers]) # The outliers ranges from 29 - 40.

length(which(master_data$TotalWorkingYears < master_data$YearsAtCompany))   #0  

# -- There is no entry where TotalWorkingYears < YearsAtCompany - This is expected.

#To verify the data is correct, we will verify the Age of the folks who have TotalWorkYears in the range of 29 - 40

outliers_tot_working_years <- subset(master_data , TotalWorkingYears >= 29 & TotalWorkingYears <= 40)
sort(outliers_tot_working_years$Age)   # Age range is 47 - 60 # This seems to be ok.

# Hence we conclude that data is correct and we will not treat the outliers.

#########################
#Training Times Last year
#########################

outliers <- get_outliers(master_data$TrainingTimesLastYear)
outliers$numOutliers  #697
sort(master_data$TrainingTimesLastYear[outliers$idxOutliers]) # 0, 5 and 6 values are outliers

#Since we don't have data where it says "how many times an employee should undergo training as per his experience or role
# We are not treating the outliers. We expect them to genuine values.

###############
#YearsAtCompany
###############

outliers <- get_outliers(master_data$YearsAtCompany) #306
outliers$numOutliers #306
sort(master_data$YearsAtCompany[outliers$idxOutliers])  # ranges from 19 -- 40

#It seems to be ok, we are not treating outliers

########################
#YearsSinceLastPromotion
########################

outliers <- get_outliers(master_data$YearsSinceLastPromotion) 
outliers$numOutliers #316
sort(master_data$YearsSinceLastPromotion[outliers$idxOutliers]) # 8,9,10,11,12,13,14,15

# Verifying the YearsSinceLastPromotion should be less than equal to "YearsAtCompany"

outliers_years_since_last_promotion <- subset(master_data , YearsSinceLastPromotion >= 8 & YearsSinceLastPromotion <= 15)
outliers_years_since_last_promotion$YearsAtCompany - outliers_years_since_last_promotion$YearsSinceLastPromotion

#####################
#YearsWithCurrManager
#####################

outliers <- get_outliers(master_data$YearsWithCurrManager)  
outliers$numOutliers #40
master_data$YearsSinceLastPromotion[outliers$idxOutliers]  # 12, 1 ,5


####################################################
#Function to generate plot for categorical variable
####################################################

gen_univariate_plot_categorical <- function(df , var , title , xlab) {
  ggplot( data = df , aes_string(x = var , fill = "Attrition")) + 
    geom_bar(aes(y = (..count..)/sum(..count..)) ,col = "red" , alpha = 0.5 , position = "dodge") +
    geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count" , position = position_dodge(width = 1) , vjust = -0.9 , size = 2.6) +
    scale_y_continuous(labels = function(x){ paste0(x*100, "%") }) +
    labs(title = title , x = xlab , y = "Employees" , fill = "Attrition") +
    theme(axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.6))
}

gen_univariate_plot_categorical_2 <- function(df , var , title , xlab) {
  ggplot(data = df , aes_string(x = var)) + 
    geom_bar(aes(y = (..count..)/sum(..count..)) , col = "red" , fill = "green" , alpha = 0.6 , width = 0.5 ) +
    geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
    scale_y_continuous(labels = function(x){ paste0(x*100, "%") }) +
    labs(title = title  , x = xlab , y = "Employees") +
    theme(axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.6))
}

t1 <- theme(axis.text.x = element_text(angle = 90))
t2 <- theme(axis.text.x = element_text(angle = 0))

##############################################
#UNIVARIATE ANALYSIS FOR CATEGORICAL VARIABLES
##############################################

################
#BUSINESS TRAVEL
################


plot_grid(gen_univariate_plot_categorical(master_data , "BusinessTravel" , "Employees - Business Travel" , "Business Travel") ,
          gen_univariate_plot_categorical_2(master_data_attr , "BusinessTravel" , "BusinessTravel wise attrition" , "BusinessTravel") , 
          align = "h")

#It is visible from the above plot that 10.7% of all the employee who travel rarely have attrited.
#Among the folks who have attrited , 66.2 % Travel Rarely.

###########
#DEPARTMENT
###########

plot_grid(gen_univariate_plot_categorical(master_data , "Department" , "Employees - Department" , "Department") + t1,
          gen_univariate_plot_categorical_2(master_data_attr , "Department" , "Department wise attrition" , "Department") + t1,
          align = "h")

#It is visible from the above plot that 10.3% of all the employee who work in R&D department have attrited.
#Among the folks who have attrited , 63.7 % work in R&D.

########
#GENDER
########


plot_grid(gen_univariate_plot_categorical(master_data , "Gender" , "Employees - Gender" , "Gender"),
          gen_univariate_plot_categorical_2(master_data_attr , "Gender" , "Percentage of male/female attrition" , "Attrition Status"),
          align = "h")

#It is visible from the above plot that 10 % & 6.2% of all the employees who have attrited are Male and Female respectively.
#Among the folks who have attrited, 61.9% are males.

###############
#MARITAL STATUS
###############


plot_grid(gen_univariate_plot_categorical(master_data , "MaritalStatus" , "Employees - Marital Status" , "Marital Status"),
          gen_univariate_plot_categorical_2(master_data_attr , "MaritalStatus" , "Marital Status wise attrition" , "Marital Status"),
          align = "h")

#It is visible from the above plot that 8.1% of all the employee who are Single have attrited.
#Among the folks who have attrited 50.4% are single.


#########
#JOB ROLE
#########


plot_grid(gen_univariate_plot_categorical(master_data , "JobRole" , "Employees - JobRole" , "Job Role") + t1,
          gen_univariate_plot_categorical_2(master_data_attr , "JobRole" , "JobRole wise attrition" , "Job Role") + t1,
          align = "h")

#It is visble from the above plot that 3.8 %  & 3.7 % of the folks who have attrited are sales executive and Research Scientist respectively
#Among the folks who have attrited 23.3% and 22.7% are sales executive and Research Scientist

##########
#EDUCATION
##########


plot_grid(gen_univariate_plot_categorical(master_data , "Education" , "Employees - Education" , "Education Level") + t1,
          gen_univariate_plot_categorical_2(master_data_attr , "Education" , "Education wise attrition" , "Education Level") + t1,
          align = "h")

#It is visible frpm the above plot that  6% of the folks who have EDUCATION LEVEL as 3 i.e. Bachelor have attrited.
#Among the folks who have attrited 37.3 % have education level as 3 i.e 'BACHELOR'

################
#EDUCATION FIELD
################


plot_grid(gen_univariate_plot_categorical(master_data , "EducationField" , "Employees - EducationField" , "EducationField") + t1,
          gen_univariate_plot_categorical_2(master_data_attr , "EducationField" , "EducationField wise attrition" , "EducationField") + t1,
          align = "h")

#Among all the employees - 6.9% have attrited from Life Sciences educated field
#Among the employees who have attrited - 42.4 % are from Life Sciences Education Field.


##########
#JOB LEVEL
##########

plot_grid(gen_univariate_plot_categorical(master_data , "JobLevel" , "Employees - Job Level" , "Job Level") + t2,
          gen_univariate_plot_categorical_2(master_data_attr , "JobLevel" , "JobLevel wise attrition" , "Job Level") + t2,
          align = "h")

#Among all the enployees - 6.2% have attrited from Job Level 2
#Among the employees who have attrited - 39.6% are from Job Level 2


###################
#PERFORMANCE RATING
###################

plot_grid(gen_univariate_plot_categorical(master_data , "PerformanceRating" , "Employees - Performance Rating" , "Performance Rating") + t2,
          gen_univariate_plot_categorical_2(master_data_attr , "PerformanceRating" , "Performance Rating wise attrition" , "Performance Rating") + t2,
          align = "h")

#Among all the employees - 13.3% have attrited who have performance rating 3 ("EXCELLENT")
#Among all the employee who have attrited - 82.4% are have Performance Rating 3 ("EXCELLENT")


#########################
#ENVIRONMENT SATISFACTION
#########################


plot_grid(gen_univariate_plot_categorical(master_data , "EnvironmentSatisfaction" , "Employees - Environment Satisfaction" , "Environment Satisfaction") + t2,
          gen_univariate_plot_categorical_2(master_data_attr , "EnvironmentSatisfaction" , "Environment Satisfaction wise attrition" , "Environment Satisfaction") + t2,
          align = "h")

#Among all the employees - 4.9% have atrrited , have environment satisfaction rating as 1 (LOW)
#Among all the employees who have attrited - 30.4% have environment satisfaction rating as 1 (LOW)

##################
#JOB SATISFACTION
##################


plot_grid(gen_univariate_plot_categorical(master_data , "JobSatisfaction" , "Employees - Job Satisfaction" , "Job Satisfaction") + t2,
          gen_univariate_plot_categorical_2(master_data_attr , "JobSatisfaction" , "Job Satisfaction wise attrition" , "Job Satisfaction") + t2,
          align = "h")

#Among all the employees - 5.0% have attrited - have Job Satisfaction as "High" (3)
#Among all the employee who have attrited - 30.8% have Job Satisfaction as "High" (3)

##################
#WORKLIFE BALANCE
##################


plot_grid(gen_univariate_plot_categorical(master_data , "WorkLifeBalance" , "Employees - Work Life Balance" , "Work Life Balance") + t2,
          gen_univariate_plot_categorical_2(master_data_attr , "WorkLifeBalance" , "WorkLifeBalance wise attrition" , "Work Life Balance") + t2,
          align = "h")

#Among all the employees - 8.7% have attrited - have WorkLifeBalance as 3 "Better"
#Among all the employees who have attrited - 54% have WorkLifeBalance as 3 "Better"


################
#JOB INVOLVEMENT
################

plot_grid(gen_univariate_plot_categorical(master_data , "JobInvolvement" , "Employees - Job Involvement" , "Job Involvement") + t2,
          gen_univariate_plot_categorical_2(master_data_attr , "JobInvolvement" , "Job Involvement wise attrition" , "job Involvement") + t2,
          align = "h")


#Among all the employees - 9% have attrited -Have job involvemnt level as "3" (High)
#Among all the employee who have attrited - 55.8% have job involvement as "3" (High)

###################
#STOCK OPTION LEVEL
###################


plot_grid(gen_univariate_plot_categorical(master_data , "StockOptionLevel" , "Employees - StockOption Level" , "StockOption Level") + t2,
          gen_univariate_plot_categorical_2(master_data_attr , "StockOptionLevel" , "StockOptionLevel wise attrition" , "StockOption Level") + t2,
          align = "h")

#Among all the employee - 7.2% have attrited - have stock option level as "0"
#Among all the employees whi have attrited - 44.7% have StockOption level as "0"


#############################################
#UNIVARIATE ANALYSIS FOR CONTINUOUS VARIABLES
#############################################

gen_univariate_plot_continuous <- function(df , var , title , xlab , start_point , end_point , diff) {
  ggplot(data = df , aes_string(x = var)) + 
    stat_bin(binwidth = diff, breaks=seq(start_point, end_point , diff), col = "red" , fill = "green" , alpha = 0.5) +
    stat_bin(binwidth= diff, breaks=seq(start_point, end_point , diff) , geom="text", aes(label=scales::percent((..count..)/sum(..count..))) , vjust=-0.75 , size = 2.9) +
    scale_x_continuous(breaks=seq(start_point, end_point , diff)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 0, vjust = .5)) +
    labs(title = title , x = xlab , y = "No of employees") 
} 

gen_univariate_plot_continuous(master_data , "MonthlyIncome" , "Distribution-Monthly Income" , "Monthly Income" , 10000 , 200000 , 10000)
gen_univariate_plot_continuous(master_data , "DistanceFromHome" , "Distribution-Distance from Home" , "Distance from Home" , 0 , 30 , 3) + t2
gen_univariate_plot_continuous(master_data , "PercentSalaryHike" , "Distribution-Salary Hike in %" , "Salary Hike" , 11 , 25 , 1) + t2
gen_univariate_plot_continuous(master_data , "Age" , "Distribution-Age" , "Age" , 18 , 58 , 5) + t2
gen_univariate_plot_continuous(master_data , "avg_working_hr_per_day" , "Distribution-Avg Working hour per day" , "Average Working Hr Per Day" , 5 , 12 , 1) + t2
gen_univariate_plot_continuous(master_data , "no_of_leaves" , "Distribution-Number of leaves" , "Total Number of Leaves" , 1 , 24 , 3) + t2
gen_univariate_plot_continuous(master_data , "YearsWithCurrManager" , "Distribution-Years With Current Manager" , "YearsWithCurrentManager" , 0 , 17 , 3) + t2
gen_univariate_plot_continuous(master_data , "TotalWorkingYears" , "Distribution-TotalWorkingYears" , "TotalWorkingYears" , 0 , 40 , 5) + t2
gen_univariate_plot_continuous(master_data , "YearsSinceLastPromotion" , "Distribution-Years Since Last Promotion" , "Years Since Last Promotion" , 0 , 15 , 3) + t2
gen_univariate_plot_continuous(master_data , "TrainingTimesLastYear" , "Distribution-Trianing Times Last Year" , "Training Times Last Year" , 0 , 6 , 1) + t2
gen_univariate_plot_continuous(master_data , "YearsAtCompany" , "Distribution-Years At company" , "Years At Company" , 0 , 40 , 4) + t2
gen_univariate_plot_continuous(master_data , "NumCompaniesWorked" , "Distribution-Num Of Companies Worked" , "Num of Companies Worked" , 0 , 10 , 2) + t2

###################
#BIVARIATE ANALYSIS
###################

############################################################################################
#Function to generate box plot for continuous variable VS categorical variable ("Attrition")
############################################################################################

gen_boxplot <- function(df , x , y ,title, xlab , ylab) {
  ggplot(data = df , aes_string(x = x , y = y , fill = x)) +
    geom_boxplot() +
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5)) + coord_flip() +
    labs(title = title , x = ylab , y = xlab)
}

gen_boxplot(master_data , "Attrition" , "PercentSalaryHike" , "Attrition - SalaryHike" , "SalaryHike" , "Attrition")
gen_boxplot(master_data , "Attrition" , "DistanceFromHome" , "Attrition - DistanceFromHome" , "DistanceFromHome" , "Attrition")
gen_boxplot(master_data , "Attrition" , "Age" , "Attrition - Age" , "Age" , "Attrition")
gen_boxplot(master_data , "Attrition" , "YearsAtCompany" , "Attrition - Years At Company" , "Years At Company" , "Attrition")
gen_boxplot(master_data , "Attrition" , "YearsWithCurrManager" , "Attrition - Years With Current Manager" , "Years with Current Manager" , "Attrition")
gen_boxplot(master_data , "Attrition" , "YearsSinceLastPromotion" , "Attrition - Year Since Last Promotion" , "Years Since Last Promotion" , "Attrition")
gen_boxplot(master_data , "Attrition" , "NumCompaniesWorked" , "Attrition - Num of companies worked" , "Num of Companies Worked" , "Attrition") 
gen_boxplot(master_data , "Attrition" , "MonthlyIncome" , "Attrition - Monthly Income" , "Monthly Income" , "Attrition")
gen_boxplot(master_data , "Attrition" , "TotalWorkingYears" , "Attrition - Total working years" , "Total working years" , "Attrition")
gen_boxplot(master_data , "Attrition" , "TrainingTimesLastYear" , "Attrition - No of Times Training Last Year" , "Training Last Year" , "Attrition")
gen_boxplot(master_data , "Attrition" , "avg_working_hr_per_day" , "Attrition - Average working hour per day" , "Average working hour per day" , "Attrition")
gen_boxplot(master_data , "Attrition" , "no_of_leaves" , "Attrition - No of Leaves" , "Number of Leaves" , "Attrition")

# converting target variable "Attrition from No/Yes character to factor with levels 0/1 

master_data$Attrition <- ifelse(master_data$Attrition=="Yes",1,0)

##############################################
# Correlation matrix for continuous variables
##############################################

master_data$EmployeeID <- NULL

continuous_var <- names(master_data)[sapply(master_data, class) == "numeric"]
continuous_data <- master_data[,(colnames(master_data) %in% continuous_var)]

corr <- cor(continuous_data)
corrplot(corr, method="color" , addCoef.col="grey", order = "AOE",number.cex=0.65)


#############################
#Scaling continuous features
#############################

continuous_var <- continuous_var[ continuous_var != "Attrition"]

for(var in continuous_var) {
  continuous_data[,var] <- scale(continuous_data[,var])
}

#################################
#Retrieving "categorical columns'
################################

cat_var <- names(master_data)[sapply(master_data, class) == "factor"]
cat_data <- master_data[ , colnames(master_data) %in% cat_var]

################################################
# creating dummy variables for factor attributes
################################################

dummies<- data.frame(sapply(master_data, 
                            function(x) data.frame(model.matrix(~x-1,data = master_data))[,-1]))


###############
# Final dataset
###############

attrition_final<- cbind(continuous_data,dummies) 


###########################################
# splitting the data between train and test
###########################################

set.seed(100)

indices = sample.split(attrition_final$Attrition, SplitRatio = 0.7)

train = attrition_final[indices,]

sum(train$Attrition)/nrow(train) #0.1614

test = attrition_final[!(indices),]

sum(test$Attrition)/nrow(test) #0.162

######################################
# Logistic Regression: #Initial model
######################################

model_1 = glm(Attrition ~ ., data = train, family = "binomial")
summary(model_1) 


model_2<- stepAIC(model_1, direction="both")
summary(model_2)
a <- data.frame(sort(vif(model_2)))

#########################################################
# HIGH VIF VARIABLES & SIGNIFICANCE
#----------------------------------
# EducationField.xLife.Sciences :     8.794816   - 3 star
# EducationField.xMedical :           8.074231   - 3 star
# EducationField.xMarketing :         4.235484   - 3 star
# BusinessTravel.xTravel_Frequently : 3.536408   - 3 star
# WorkLifeBalance.x3 :                3.535572   - 3 star
# BusinessTravel.xTravel_Rarely :     3.504056   - 2 star
# EducationField.xTechnical.Degree:   3.345456   - 3 star
# WorkLifeBalance.x2:                 3.097400   - 3 star
# EducationField.xOther:              2.655963   - 3 star
# TotalWorkingYears:                  2.580303   - 3 star
# MaritalStatus.xSingle:              2.197588   - 3 star
# MaritalStatus.xMarried:             2.121678   - "DOT"
# YearsSinceLastPromotion             2.074798   - 3 star
# WorkLifeBalance                     2.0000     - 3 star
# Rest variables have vif < 2


# HIgh VIF have 3 stars so can't remove them
# So removing "MaritalStatus.xMarried"  ("DOT" low significant and high vif - 2.12)

model_3 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 avg_working_hr_per_day + BusinessTravel.xTravel_Frequently + 
                 BusinessTravel.xTravel_Rarely + Education.x5 + EducationField.xLife.Sciences + 
                 EducationField.xMarketing + EducationField.xMedical + EducationField.xOther + 
                 EducationField.xTechnical.Degree + JobLevel.x2 + JobLevel.x5 + 
                 JobRole.xHuman.Resources + JobRole.xManager + JobRole.xManufacturing.Director + 
                 JobRole.xResearch.Director + JobRole.xSales.Executive + 
                 MaritalStatus.xSingle + StockOptionLevel.x1 + EnvironmentSatisfaction.x2 + 
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                 JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                 WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                 JobInvolvement.x3, family = "binomial", data = train)


summary(model_3)
a <- data.frame(sort(vif(model_3)))

#########################################################
# HIGH VIF VARIABLES & SIGNIFICANCE
#----------------------------------
# EducationField.xLife.Sciences :     8.878      - 3 star
# EducationField.xMedical :           8.16       - 3 star
# EducationField.xMarketing :         4.27       - 3 star
# WorkLifeBalance.x3 :                3.55       - 3 star
# BusinessTravel.xTravel_Frequently : 3.54       - 3 star
# BusinessTravel.xTravel_Rarely :     3.50       - 2 star
# EducationField.xTechnical.Degree:   3.37       - 3 star
# WorkLifeBalance.x2:                 3.10       - 3 star
# EducationField.xOther:              2.66       - 3 star
# TotalWorkingYears:                  2.58       - 3 star
# YearsSinceLastPromotion             2.074798   - 3 star
# WorkLifeBalance.x4                  2.0000     - 3 star
# Rest variables have vif < 2


# High VIF have 3 stars so can't remove them
# removing BusinessTravel.xTravel_Rarely - only high vif(3.50) but 2 star

model_4 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 avg_working_hr_per_day + BusinessTravel.xTravel_Frequently + 
                 Education.x5 + EducationField.xLife.Sciences + 
                 EducationField.xMarketing + EducationField.xMedical + EducationField.xOther + 
                 EducationField.xTechnical.Degree + JobLevel.x2 + JobLevel.x5 + 
                 JobRole.xHuman.Resources + JobRole.xManager + JobRole.xManufacturing.Director + 
                 JobRole.xResearch.Director + JobRole.xSales.Executive + 
                 MaritalStatus.xSingle + StockOptionLevel.x1 + EnvironmentSatisfaction.x2 + 
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                 JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                 WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                 JobInvolvement.x3, family = "binomial", data = train)


summary(model_4)
a <- data.frame(sort(vif(model_4)))

#########################################################
# HIGH VIF VARIABLES & SIGNIFICANCE
#----------------------------------
# EducationField.xLife.Sciences :     8.82       - 3 star
# EducationField.xMedical :           8.12       - 3 star
# EducationField.xMarketing :         4.26       - 3 star
# WorkLifeBalance.x3 :                3.56       - 3 star
# EducationField.xTechnical.Degree:   3.35       - 3 star
# WorkLifeBalance.x2:                 3.13       - 3 star
# EducationField.xOther:              2.66       - 3 star
# TotalWorkingYears:                  2.58       - 3 star
# YearsSinceLastPromotion             2.05       - 3 star
# WorkLifeBalance.x4                  2.01       - 3 star
# Rest variables have vif < 2


#All high vif have 3 stars - so can't remove them
#Will remove the least significant variable i.e "JobLevel.x5"


model_5 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 avg_working_hr_per_day + BusinessTravel.xTravel_Frequently + 
                 Education.x5 + EducationField.xLife.Sciences + 
                 EducationField.xMarketing + EducationField.xMedical + EducationField.xOther + 
                 EducationField.xTechnical.Degree + JobLevel.x2 +  
                 JobRole.xHuman.Resources + JobRole.xManager + JobRole.xManufacturing.Director + 
                 JobRole.xResearch.Director + JobRole.xSales.Executive + 
                 MaritalStatus.xSingle + StockOptionLevel.x1 + EnvironmentSatisfaction.x2 + 
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                 JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                 WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                 JobInvolvement.x3, family = "binomial", data = train)


summary(model_5)
a <- data.frame(sort(vif(model_5)))

#########################################################
# HIGH VIF VARIABLES & SIGNIFICANCE
#----------------------------------
# EducationField.xLife.Sciences :     8.90       - 3 star
# EducationField.xMedical :           8.20       - 3 star
# EducationField.xMarketing :         4.23       - 3 star
# WorkLifeBalance.x3 :                3.56       - 3 star
# EducationField.xTechnical.Degree:   3.38       - 3 star
# WorkLifeBalance.x2:                 3.13       - 3 star
# EducationField.xOther:              2.60       - 3 star
# TotalWorkingYears:                  2.56       - 3 star
# YearsSinceLastPromotion             2.05       - 3 star
# WorkLifeBalance.x4                  2.01       - 3 star
# Rest variables have vif < 2


#All high vif have 3 stars - so can't remove them
#removing JobRole.xHuman.Resources (DOT significance)

model_6 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 avg_working_hr_per_day + BusinessTravel.xTravel_Frequently + 
                 Education.x5 + EducationField.xLife.Sciences + 
                 EducationField.xMarketing + EducationField.xMedical + EducationField.xOther + 
                 EducationField.xTechnical.Degree + JobLevel.x2 +  
                 JobRole.xManager + JobRole.xManufacturing.Director + 
                 JobRole.xResearch.Director + JobRole.xSales.Executive + 
                 MaritalStatus.xSingle + StockOptionLevel.x1 + EnvironmentSatisfaction.x2 + 
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                 JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                 WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                 JobInvolvement.x3, family = "binomial", data = train)


summary(model_6)
a <- data.frame(sort(vif(model_6)))

#removing Education.x5 (DOT star)

model_7 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 avg_working_hr_per_day + BusinessTravel.xTravel_Frequently + 
                 EducationField.xLife.Sciences + 
                 EducationField.xMarketing + EducationField.xMedical + EducationField.xOther + 
                 EducationField.xTechnical.Degree + JobLevel.x2 +  
                 JobRole.xManager + JobRole.xManufacturing.Director + 
                 JobRole.xResearch.Director + JobRole.xSales.Executive + 
                 MaritalStatus.xSingle + StockOptionLevel.x1 + EnvironmentSatisfaction.x2 + 
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                 JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                 WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                 JobInvolvement.x3, family = "binomial", data = train)


summary(model_7)
a <- data.frame(sort(vif(model_7)))

#remove JobRole.xSales.Executive (Single Star)

model_8 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 avg_working_hr_per_day + BusinessTravel.xTravel_Frequently + 
                 EducationField.xLife.Sciences + 
                 EducationField.xMarketing + EducationField.xMedical + EducationField.xOther + 
                 EducationField.xTechnical.Degree + JobLevel.x2 +  
                 JobRole.xManager + JobRole.xManufacturing.Director + 
                 JobRole.xResearch.Director +  
                 MaritalStatus.xSingle + StockOptionLevel.x1 + EnvironmentSatisfaction.x2 + 
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                 JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                 WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                 JobInvolvement.x3, family = "binomial", data = train)

summary(model_8)
a <- data.frame(sort(vif(model_8)))

#removing JobInvolvement.x3

model_9 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 avg_working_hr_per_day + BusinessTravel.xTravel_Frequently + 
                 EducationField.xLife.Sciences + 
                 EducationField.xMarketing + EducationField.xMedical + EducationField.xOther + 
                 EducationField.xTechnical.Degree + JobLevel.x2 +  
                 JobRole.xManager + JobRole.xManufacturing.Director + 
                 JobRole.xResearch.Director +  
                 MaritalStatus.xSingle + StockOptionLevel.x1 + EnvironmentSatisfaction.x2 + 
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                 JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                 WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 , family = "binomial", data = train)

summary(model_9)
a <- data.frame(sort(vif(model_9)))

#removing JobRole.xResearch.Director

model_10 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  avg_working_hr_per_day + BusinessTravel.xTravel_Frequently + 
                  EducationField.xLife.Sciences + 
                  EducationField.xMarketing + EducationField.xMedical + EducationField.xOther + 
                  EducationField.xTechnical.Degree + JobLevel.x2 +  
                  JobRole.xManager + JobRole.xManufacturing.Director + 
                  MaritalStatus.xSingle + StockOptionLevel.x1 + EnvironmentSatisfaction.x2 + 
                  EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                  JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                  WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 , family = "binomial", data = train)

summary(model_10)
a <- data.frame(sort(vif(model_10)))

#removing JobLevel.x2

model_11 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  avg_working_hr_per_day + BusinessTravel.xTravel_Frequently + 
                  EducationField.xLife.Sciences + 
                  EducationField.xMarketing + EducationField.xMedical + EducationField.xOther + 
                  EducationField.xTechnical.Degree +   
                  JobRole.xManager + JobRole.xManufacturing.Director + 
                  MaritalStatus.xSingle + StockOptionLevel.x1 + EnvironmentSatisfaction.x2 + 
                  EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                  JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                  WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 , family = "binomial", data = train)

summary(model_11)
a <- data.frame(sort(vif(model_11)))


#remove StockOptionLevel.x1

model_12 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  avg_working_hr_per_day + BusinessTravel.xTravel_Frequently + 
                  EducationField.xLife.Sciences + 
                  EducationField.xMarketing + EducationField.xMedical + EducationField.xOther + 
                  EducationField.xTechnical.Degree +   
                  JobRole.xManager + JobRole.xManufacturing.Director + 
                  MaritalStatus.xSingle + EnvironmentSatisfaction.x2 + 
                  EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                  JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                  WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 , family = "binomial", data = train)


summary(model_12)
a <- data.frame(sort(vif(model_12)))


#remove JobRole.xManage

model_13 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  avg_working_hr_per_day + BusinessTravel.xTravel_Frequently + 
                  EducationField.xLife.Sciences + 
                  EducationField.xMarketing + EducationField.xMedical + EducationField.xOther + 
                  EducationField.xTechnical.Degree +   
                  JobRole.xManufacturing.Director + 
                  MaritalStatus.xSingle + EnvironmentSatisfaction.x2 + 
                  EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                  JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                  WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 , family = "binomial", data = train)

summary(model_13)
a <- data.frame(sort(vif(model_13)))


final_model <- model_13
test_pred = predict(final_model, type = "response", newdata = test)

#####################
#summary of test_pred
#####################

summary(test_pred)
test$prob <- test_pred

###########################
# probability cutoff of 50%.
###########################

test_pred_attr <- factor(ifelse(test_pred >= 0.50, "Yes", "No"))
test_actual_attr <- factor(ifelse(test$Attrition==1,"Yes","No"))

conf <- confusionMatrix(test_pred_attr, test_actual_attr, positive = "Yes")

conf$overall[1]  #-- Accuracy : 0.86
conf$byClass[1]  #-- Sensistivity : 0.32 --Very low
conf$byClass[2]  # -- Specificity : 0.96

# 0.5 cutoff is not correct , very low sensistivity

############################################
# To find out the optimal probalility cutoff
############################################

perform_fn <- function(cutoff) 
{
  predicted_attr <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_attr, test_actual_attr, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}


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


cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.015)]
cutoff # 0.1855556

##############################################################
#We are finalizing a cutoff value of 0.1855556 for final model
##############################################################

test_pred_attr <- factor(ifelse(test_pred >=0.1855556, "Yes", "No"))
test_actual_attr <- factor(ifelse(test$Attrition==1,"Yes","No"))

conf_final <- confusionMatrix(test_pred_attr, test_actual_attr, positive = "Yes")

acc <- conf_final$overall[1]
acc #0.7651163

sens <- conf_final$byClass[1]
sens #0.7751196

spec <- conf_final$byClass[2]
spec #0.7631822

###############################
# EDA based on Information gain 
###############################

IV <- create_infotables(data=master_data, y="Attrition", bins=5, parallel=FALSE)
IV
IV_Value = data.frame(IV$Summary)
IV_Value  

############################################################################################
#1.The above code shows the amount of information (Based on the subject"information Theory") 
#in each of the variables.
############################################################################################

#############################################################################################
#2.a) no_of_leaves,JobInvolvement,MonthlyIncome,JobLevel, PercentSalaryHike,Education,
#     StockOptionLevel,DistanceFromHome,PerformanceRating & Gender are Weak Predictors based
#     on their IV values
#  b) MaritalStatus,BusinessTravel,EnvironmentSatisfaction,JobSatisfaction,NumCompaniesWorked
#     WorkLifeBalance,EducationField,JobRole,Department,YearsSinceLastPromotion &
#     TrainingTimesLastYear are Moderate Predictors.
#  C) YearsAtCompany,TotalWorkingYears,avg_working_hr_per_day,YearsWithCurrManager &
#       Age are Strong predictors.
#############################################################################################

plot_infotables(IV, IV$Summary$Variable[1:5], same_scale=FALSE)
plot_infotables(IV, IV$Summary$Variable[6:10], same_scale=FALSE)
plot_infotables(IV, IV$Summary$Variable[11:14], same_scale=FALSE)
plot_infotables(IV, IV$Summary$Variable[15:19], same_scale=FALSE)
plot_infotables(IV, IV$Summary$Variable[20:25], same_scale=FALSE)

############################################################################################

#################################################################
# Woe Analysis to increase the stability,however information gain 
# decreases as there is a trade off between the two
#################################################################

#The following are the categorical variables that are monotonic 
#with the number of bins indicated in the code 

woe(Data=master_data,"BusinessTravel",FALSE,"Attrition",3,Bad=0,Good=1)

woe(Data=master_data,"EducationField",FALSE,"Attrition",6,Bad=0,Good=1)

#The following are the continuous variables that are monotonic 
#with the number of bins indicated in the code 

woe(Data = master_data,"avg_working_hr_per_day",TRUE,"Attrition",3,Bad=0,Good=1)

woe(Data = master_data,"TotalWorkingYears",TRUE,"Attrition",7,Bad=0,Good=1)

woe(Data = master_data,"YearsAtCompany",TRUE,"Attrition",5,Bad=0,Good=1)

###########################################################################################

#################
#MODEL EVALUATION
#################

## KS -statistic - Test Data

test_cutoff_attr <- test_pred_attr

test_cutoff_attr <- ifelse(test_cutoff_attr=="Yes",1,0)
test_actual_attr <- ifelse(test_actual_attr=="Yes",1,0)

pred_object <- prediction(test_cutoff_attr, test_actual_attr)

performance_measures_test<- performance(pred_object, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test) #0.5383019

#Hence KS statistic for our model is 0.538019

#Hence we can say that KS statistic for our mode is nearly 54%

#------------------------------------------------------------------

##################
#Lift & Gain Chart
##################

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

Attrition_decile <- lift(test_actual_attr, test_pred, groups = 10)

###########
#GAIN CHART
###########

ggplot(data = Attrition_decile, aes(x=bucket,y=Gain))+ 
  geom_line()+geom_point()+ 
  ggtitle("Gain Chart") + labs(y = "Gain%") + labs(x = "Decile") + scale_x_continuous(breaks = c(2, 4, 6 , 8 , 10)) +
  geom_hline(yintercept = 61.24) +
  geom_vline(xintercept = 2) +
  geom_hline(yintercept = 83.25359) +
  geom_vline(xintercept = 4) +
  box() + panel_border(colour = "black")

#From the above GAIN chart, we can infer that by focussing on the top 40% of the employees (after sorting them by probailities)
# We can focus on top 83% of the employees who are likely to leave the company.


###########
#LIFT CHART
###########

ggplot(data = Attrition_decile, aes(x=bucket,y=Cumlift))+ 
  geom_line()+geom_point()+ 
  ggtitle("Lift Chart") + labs(y = "Lift") + labs(x = "Decile") + scale_x_continuous(breaks = c(seq(1:10))) +
  box() + panel_border(colour = "black")

##############################################################
#From the above lift chart, we can see that lift at the end of
# 1st decile is 3.49
# 2nd decile is 3.06
# 3rd decile is 2.47
# 4th decile is 2.08
##############################################################
