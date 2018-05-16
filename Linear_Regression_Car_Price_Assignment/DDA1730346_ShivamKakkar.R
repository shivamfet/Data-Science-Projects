library(tidyr)
library(ggplot2)
library(corrplot)
library(scales)
library(MASS)
library(car)

####################
#Reading the dataset
####################

car_price <- read.csv("CarPrice_Assignment.csv")
str(car_price)

#####################
#check for NA values:
#####################

sum(is.na(car_price)) #0

#############################
#check for duplicate records.
#############################

sum(duplicated(car_price$car_ID)) #0

##########################################################
#Convert all integer type to numeric to have a consistency
##########################################################

car_price[sapply(car_price, is.integer)] <- lapply(car_price[sapply(car_price, is.integer)], 
                                                         as.numeric)

########################################################
#Splitting the car name into Company Name and Model Name
########################################################

car_price <-separate(car_price , CarName , into = c("CompanyName" , "ModelName") ,
                     sep = " " , extra = "merge" , fill = "right")

#########################################
#Converting the Company Name to lowercase
#########################################

car_price$CompanyName <- tolower(car_price$CompanyName)
unique(car_price$CompanyName)
length(unique(car_price$CompanyName))  #27

#####################################################
#Cleaning the data as following values are misspelled
#####################################################

#Change "toyouta" to "toyota"
#Change "porcshce" to "porsche"
#Change "vokswagen" to "volkswagen"
#Change "vw" to "volkswagen"
#Change "maxda" to "mazda"

car_price$CompanyName <- gsub("toyouta" , "toyota" ,car_price$CompanyName)
car_price$CompanyName <- gsub("porcshce" , "porsche" ,car_price$CompanyName)
car_price$CompanyName <- gsub("vokswagen|vw" , "volkswagen" ,car_price$CompanyName)
car_price$CompanyName <- gsub("maxda" , "mazda" ,car_price$CompanyName)

unique(car_price$CompanyName)
length(unique(car_price$CompanyName)) #22

###########################################
#Convert all character types to factor type
###########################################

car_price[sapply(car_price, is.character)] <- lapply(car_price[sapply(car_price, is.character)], 
                                                     as.factor)
str(car_price)

##############################################
#Dropping the model name as it is not required
##############################################

car_price$ModelName <- NULL

##############################
#OUTLIER DETECTION & TREATMENT
##############################

############################################
#Function to find outliers in a given vector
############################################

get_outliers <- function(x) {
  outliers <- outliers(x)
  outliers$numOutliers  
  x[outliers$idxOutliers] 
}

##################################################
#No Outliers exists in following numeric variables
##################################################

#carlength , carheight and boreratio don't have any outliers

ggplot(data = car_price , aes(x = "carlength", y = carheight)) + geom_boxplot()
ggplot(data = car_price , aes(x = "carheight", y = carheight)) + geom_boxplot()
ggplot(data = car_price , aes(x = "boreratio", y = boreratio)) + geom_boxplot()

###############################################
#Outliers exists in following numeric variables
###############################################

###########
#enginesize
###########

ggplot(data = car_price , aes(x = "enginesize", y = enginesize)) + geom_boxplot()
outliers <- get_outliers(car_price$enginesize)
outliers
length(outliers)  #Total Ouliers in enginesize are 10
quantile(car_price$enginesize , probs = seq(0 , 1 , 0.05))

#Capping the 10 outliers to the 95 percentile value:

car_price$enginesize[which(car_price$enginesize > 201)] <- 201

##########
#car width
##########

ggplot(data = car_price , aes(x = "carwidth", y = carwidth)) + geom_boxplot()
outliers <- get_outliers(car_price$carwidth)
outliers
length(outliers)  #8 outliers
quantile(car_price$carwidth , probs = seq(0 , 1 , 0.01))

#Capping the carwidth to the 96th percentile value:

car_price$carwidth[which(car_price$carwidth > 70.852)] <- 70.852

#######
#Stroke
#######

ggplot(data = car_price , aes(x = "stroke", y = stroke)) + geom_boxplot()

#Outliers present in both lower and upper region.

outliers <- get_outliers(car_price$stroke)
outliers
length(outliers)   #20 outliers
quantile(car_price$stroke , probs = seq(0 , 1 , 0.01))

#Capping the lower outliers to "8th percentile value"

car_price$stroke[which(car_price$stroke < 2.7056)] <- 2.70

#Capping the upper outliers to 98th percentile value"

car_price$stroke[which(car_price$stroke > 3.86)] <- 3.86

##################
#Compression ratio
##################

ggplot(data = car_price , aes(x = "compressionratio", y = compressionratio)) + geom_boxplot()

outliers <- get_outliers(car_price$compressionratio)
outliers   
length(outliers) #28 outliers
quantile(car_price$compressionratio , probs = seq(0 , 1 , 0.01))

#capping the lowe outliers to 6th percentile value

car_price$compressionratio[which(car_price$compressionratio < 7.5 )] <- 7.5

#Capping the upper outliers to 89th percentile value

car_price$compressionratio[which(car_price$compressionratio > 10)] <- 10.00

###########
#horsepower
###########

ggplot(data = car_price , aes(x = "horsepower", y = horsepower)) + geom_boxplot()
outliers <- get_outliers(car_price$horsepower)
outliers   
length(outliers) #6 outliers

quantile(car_price$horsepower , probs = seq(0 , 1 , 0.01))

#Capping the outliers to the 97th percentile value

car_price$horsepower[which(car_price$horsepower > 184)] <- 184

########
#peakrpm
########

ggplot(data = car_price , aes(x = "peakrpm", y = peakrpm)) + geom_boxplot()
outliers <- get_outliers(car_price$peakrpm)
outliers  #2
quantile(car_price$peakrpm , probs = seq(0 , 1 , 0.01))

#There are 2 outliers for peakrpm i.e. 6600:
#Capping it to the upper value i.e 6000

car_price$peakrpm[which(car_price$peakrpm > 6000)] <- 6000

########
#citympg
########

ggplot(data = car_price , aes(x = "citympg", y = citympg)) + geom_boxplot()
outliers <- get_outliers(car_price$citympg)
outliers  #2 
quantile(car_price$citympg , probs = seq(0 , 1 , 0.01))

#capping the outliers at 98th percentile

car_price$citympg[which(car_price$citympg > 38)] <- 38

###########
#highwaympg
###########

ggplot(data = car_price , aes(x = "highwaympg", y = highwaympg)) + geom_boxplot()
outliers <- get_outliers(car_price$highwaympg)
outliers  # 3 outliers
quantile(car_price$highwaympg , probs = seq(0 , 1 , 0.01))

#capping the outliers at 98th percentile i.e 46.92 ~ 47
car_price$highwaympg[which(car_price$highwaympg > 47)] <- 47

##########
#wheelbase
##########

ggplot(data = car_price , aes(x = "wheelbase", y = wheelbase)) + geom_boxplot()
outliers <- get_outliers(car_price$wheelbase)
outliers
quantile(car_price$wheelbase , probs = seq(0 , 1 , 0.01))

#capping it to 98th percentile

car_price$wheelbase[which(car_price$wheelbase > 114.2)] <- 114.2

###############################################################################

#####
#EDA
#####

####################################################
#Function to generate plot for categorical variable
####################################################

gen_univariate_plot_categorical <- function(df , var , title , xlab) {
  ggplot(data = df , aes_string(x = var)) + 
    geom_bar(aes(y = (..count..)/sum(..count..)) , col = "red" , fill = "green" , alpha = 0.6 ) +
    geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
    scale_y_continuous(labels = function(x){ paste0(x*100, "%") }) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = .5)) +
    labs(title = title  , x = xlab , y = "Cars") 
}

#############################################
#Univariate analysis of Categorical variables
#############################################

#toyota company has max number of cars sold

gen_univariate_plot_categorical(car_price , "CompanyName" , "Cars - Company" , "Company Name")

#90.2% of cars sold are running on gas

gen_univariate_plot_categorical(car_price , "fueltype" , "Cars - Fuel Type" , "Fuel Type")

#98.5% of cars have engine in front

gen_univariate_plot_categorical(car_price , "enginelocation" , "Cars - Engine location" , "Engine Location")

#Majority of cars have 4 doors : 56.1%

gen_univariate_plot_categorical(car_price , "doornumber" , "Cars - Door Number" , "Door Number")

#82% of cars have std aspiration

gen_univariate_plot_categorical(car_price , "aspiration" , "Cars - Aspiration" , "Aspiration")

#Majority of cars (72.2%) have ohc engine type

gen_univariate_plot_categorical(car_price , "enginetype" , "Cars - EngineType" , "Engine Type")

#Majority of cars (77.6%) have 4 cylinders

gen_univariate_plot_categorical(car_price , "cylindernumber" , "Cars - No. of Cylinders" , "No of Cylinders")

#46.8% of cars are sedan : 34.1 % of cars are hatchback : rest others

gen_univariate_plot_categorical(car_price , "carbody" , "Cars - Car Body" , "Car Body")

#58.5% of cars have drivewheel "58.5%" and 

gen_univariate_plot_categorical(car_price , "drivewheel" , "Cars - Drive Wheel" , "Drive wheel")

#45.9% of cars have fuel system "mpfi" and 32.2% of cars have fuel system of 2bbl

gen_univariate_plot_categorical(car_price , "fuelsystem" , "Cars - Fuel System" , "Fuel System")

################################################################################################

############################################
#UNIVARIATE ANALYSIS OF CONTINUOUS VARIABLES
############################################

gen_univariate_plot_continuous <- function(df , var , title , xlab , start_point , end_point , diff) {
  ggplot(data = df , aes_string(x = var)) + 
    stat_bin(binwidth = diff, breaks=seq(start_point, end_point , diff), col = "red" , fill = "green" , alpha = 0.5) +
    stat_bin(binwidth= diff, breaks=seq(start_point, end_point , diff) , geom="text", aes(label=scales::percent((..count..)/sum(..count..))) , vjust=-0.75 , size = 2.9) +
    scale_x_continuous(breaks=seq(start_point, end_point , diff)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5)) +
    labs(title = title , x = xlab , y = "No of Cars") 
} 


gen_univariate_plot_continuous(car_price, "price" , "Price" , "Price" , 5000 , 48000 , 3000)
gen_univariate_plot_continuous(car_price, "enginesize" , "Enginesize" , "Enginesize" , 61 , 201 , 30)
gen_univariate_plot_continuous(car_price, "carheight" , "Car Height" , "Car Height" , 47 , 60 , 1)
gen_univariate_plot_continuous(car_price, "carwidth" , "Car Width" , "Car Width" , 60 , 71 , 1)
gen_univariate_plot_continuous(car_price, "carlength" , "Car Length" , "Car Length" , 140 , 210 , 5)
gen_univariate_plot_continuous(car_price, "wheelbase" , "Wheel Base" , "Wheel Base" , 88 , 114 , 1)
gen_univariate_plot_continuous(car_price, "curbweight" , "Curb Weight" , "Curb Weight" , 1480 , 4200 , 200)
gen_univariate_plot_continuous(car_price, "citympg" , "CityMpg" , "CityMpg" , 13 , 39 , 4)
gen_univariate_plot_continuous(car_price, "highwaympg" ,"Highwaympg" , "HighwayMpg" , 16 , 48 , 4)
gen_univariate_plot_continuous(car_price, "boreratio" , "Boreratio" , "Boreratio" , 2.5 , 4 , 0.5)
gen_univariate_plot_continuous(car_price, "stroke" , "Stroke" , "Stroke" , 2.7 , 3.9 , 0.3)
gen_univariate_plot_continuous(car_price, "compressionratio" , "Compressionratio" , "Compressionratio" , 7.5 , 10 , 1)
gen_univariate_plot_continuous(car_price, "horsepower" , "Horsepower" , "Horsepower" , 48 , 192 , 12)
gen_univariate_plot_continuous(car_price, "peakrpm" , "Peakrpm" , "Peakrpm" , 4150 , 6100 , 150)


###################
#BIVARIATE ANALYSIS
###################

##############################################################################
#Function to generate box plot for continuous variable VS categorical variable
##############################################################################

gen_boxplot <- function(df , x , y ,title, xlab , ylab) {
  ggplot(data = df , aes_string(x = x , y = y , fill = x)) +
    geom_boxplot() +
    theme(axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5)) +
    labs(title = title , x = xlab , y = ylab)
}

############################################################################################
# It is clearly visible from the following plot that most of the highly priced cars are from
# the companies : "bmw" : "buick" : "jaguar" : "porsche"
############################################################################################

gen_boxplot(car_price , "CompanyName" , "price" , "Price vs CompanyName" , "Company" , "Price")

gen_boxplot(car_price , "fueltype" , "price" , "Price vs Fuel Type" , "Fuel Type" , "Price")

###############################################################################################
# It is clearly visible from the following plot that the cars who engine location is "rear" are
# highly priced
###############################################################################################

gen_boxplot(car_price , "enginelocation" , "price" , "Price vs Engine location" , "Engine Location" , "Price")

gen_boxplot(car_price , "doornumber" , "price" , "Price vs Door" , "Door Number" , "Price")
gen_boxplot(car_price , "aspiration" , "price" , "Price vs Aspiration" , "Aspiration" , "Price")

#################################################################################################
# It is clearly visible from the following plot that the cars whose enginetype is "ohcv" are more
# priced than the other
#################################################################################################

gen_boxplot(car_price , "enginetype" , "price" , "Price vs Engine Type" , "Engine Type" , "Price")

##############################################################################################
#It is clearly visible from following plot that "twelve" , "eight" , "six" , "five" cylinders 
#are more priced than the others
##############################################################################################

gen_boxplot(car_price , "cylindernumber" , "price" , "Price vs Cylinder Number" , "Cylinder Number" , "Price")

###################################################################################################
#In the following plot : Median price of the cars that have the body "convertible" and "hardtop" 
#is higher than the others
###################################################################################################

gen_boxplot(car_price , "carbody" , "price" , "Price vs Car Body" , "Car Body" , "Price")

######################################################################
#following plot shows "rwd" drive wheel is priced more than the others
######################################################################

gen_boxplot(car_price , "drivewheel" , "price" , "Price vs Drive Wheel" , "Drive wheel" , "Price")

gen_boxplot(car_price , "fuelsystem" , "price" , "Price vs Fuel System" , "Fuel System" , "Price")

gen_boxplot(car_price , "factor(symboling)" , "price" , "Price vs Symboling" , "Fuel System" , "Price")

##################################
#Continuous Vs Continuous Variable
##################################

ggplot(car_price , aes(x = horsepower , y = price)) + geom_point() + geom_smooth() #Price increase with horsepower
ggplot(car_price , aes(x = carheight , y = price)) + geom_point() + geom_smooth() #
ggplot(car_price , aes(x = carwidth , y = price)) + geom_point() + geom_smooth() #Price increases with carwidth
ggplot(car_price , aes(x = carlength , y = price)) + geom_point() + geom_smooth()#Price increases with carlength
ggplot(car_price , aes(x = curbweight , y = price)) + geom_point() + geom_smooth() #Price increases with curbweight
ggplot(car_price , aes(x = enginesize , y = price)) + geom_point() + geom_smooth() #Price increases with enginesize
ggplot(car_price , aes(x = citympg , y = price)) + geom_point() + geom_smooth() #Price decreases with citympg
ggplot(car_price , aes(x = highwaympg , y = price)) + geom_point() + geom_smooth()#Price decreases with highwaympg
ggplot(car_price , aes(x = stroke , y = price)) + geom_point() + geom_smooth()#
ggplot(car_price , aes(x = wheelbase , y = price)) + geom_point() + geom_smooth()
ggplot(car_price , aes(x = compressionratio , y = price)) + geom_point() + geom_smooth()
ggplot(car_price , aes(x = boreratio , y = price)) + geom_point() + geom_smooth()
ggplot(car_price , aes(x = peakrpm , y = price)) + geom_point() + geom_smooth()
ggplot(car_price , aes(x = symboling , y = price)) + geom_point() + geom_jitter(alpha(alpha = 0.5))


##############################################
# Correlation matrix for continuous variables
##############################################

#Setting the car_id to NULL as it is not required for modelling

continous_data$car_ID <- NULL
continous_var <- names(car_price)[sapply(car_price, class) == "numeric"]
continous_data <- car_price[,(colnames(car_price) %in% continous_var)]
corr <- cor(continous_data)
corrplot(corr, method="color" , addCoef.col="grey", order = "AOE",number.cex=0.65)

####################################################################
#From the above correlation plot it is clearly visible :
# Price is higly(positively) correlated with "carlength" , 
# "carwidth" , "curbweight" , "enginesize" , "horsepower"
# Price is highly(negatively) correlated with "citympg" & "highwaympg"
######################################################################


######################################################
#Converting categorical variable fuel type to numeric
######################################################

levels(car_price$fueltype) <- c(1,0)
car_price$fueltype <- as.numeric(levels(car_price$fueltype))[car_price$fueltype]

########################################################
#Converting categorical variable aspiration type to numeric
########################################################

levels(car_price$aspiration) <- c(1,0)
car_price$aspiration <- as.numeric(levels(car_price$aspiration))[car_price$aspiration]

########################################################
#Converting categorical variable engine location to numeric
########################################################

levels(car_price$enginelocation) <- c(1 ,0)
car_price$enginelocation <- as.numeric(levels(car_price$enginelocation))[car_price$enginelocation]

########################################################
#Converting categorical variable cylinder number to numeric
########################################################

levels(car_price$cylindernumber) <- c(8 , 5 ,4 ,6 ,3 ,12 ,2)
car_price$cylindernumber <- as.numeric(levels(car_price$cylindernumber))[car_price$cylindernumber]

########################################################
#Converting categorical variable door number to numeric
########################################################

levels(car_price$doornumber) <- c(4 , 2)
car_price$doornumber <- as.numeric(levels(car_price$doornumber))[car_price$doornumber]

##############################################
# Correlation matrix for continuous variables
##############################################

#Setting the car_id to NULL as it is not required for modelling

continous_data$car_ID <- NULL
continous_var <- names(car_price)[sapply(car_price, class) == "numeric"]
continous_data <- car_price[,(colnames(car_price) %in% continous_var)]
corr <- cor(continous_data)
corrplot(corr, method="color" , addCoef.col="grey", order = "AOE",number.cex=0.65)

summary(car_price$drivewheel)
summary(car_price$carbody)
summary(car_price$fuelsystem)

dummy_1 <- data.frame(model.matrix( ~carbody, data = car_price))
dummy_1 <- dummy_1[,-1]
car_price$carbody <- NULL
car_price <- cbind(car_price , dummy_1)

dummy_2 <- data.frame(model.matrix(~fuelsystem , data = car_price))
dummy_2 <- dummy_2[,-1]
car_price$fuelsystem <- NULL
car_price <- cbind(car_price , dummy_2)

dummy_3 <- data.frame(model.matrix(~drivewheel , data = car_price))
dummy_3 <- dummy_3[,-1]
car_price$drivewheel <- NULL
car_price <- cbind(car_price , dummy_3)

dummy_4 <- data.frame(model.matrix(~enginetype , data = car_price))
dummy_4 <- dummy_4[,-1]
car_price$enginetype <- NULL
car_price <- cbind(car_price , dummy_4)

dummy_5 <- data.frame(model.matrix(~CompanyName , data = car_price))
dummy_5 <- dummy_5[,-1]
car_price$CompanyName <- NULL
car_price <- cbind(car_price , dummy_5)


car_price$car_ID <- NULL

set.seed(100)

# randomly generate row indices for train dataset
trainindices= sample(1:nrow(car_price), 0.7*nrow(car_price))
# generate the train data set
train = car_price[trainindices,]

#Similarly store the rest of the observations into an object "test".
test = car_price[-trainindices,]


model_1 <-lm(price~.,data=train)
summary(model_1)  # Adjusted R2 = 0.9604

step <- stepAIC(model_1, direction="both")
step

model_2 <- lm(formula = price ~ symboling + aspiration + enginelocation + carlength + 
                carwidth + carheight + curbweight + cylindernumber + enginesize + 
                boreratio + horsepower + carbodyhardtop + carbodyhatchback + 
                carbodysedan + carbodywagon + fuelsystem4bbl + drivewheelrwd + 
                enginetypel + enginetypeohcf + CompanyNameaudi + CompanyNamebmw + 
                CompanyNamebuick + CompanyNamechevrolet + CompanyNamedodge + 
                CompanyNamehonda + CompanyNameisuzu + CompanyNamejaguar + 
                CompanyNamemazda + CompanyNamemercury + CompanyNamemitsubishi + 
                CompanyNamenissan + CompanyNameplymouth + CompanyNamerenault + 
                CompanyNamesaab + CompanyNametoyota + CompanyNamevolkswagen + 
                CompanyNamevolvo, data = train)

summary(model_2)  # Adjusted R2 = 0.9649
a <- data.frame(sort(vif(model_2)))

#######################################################################
# Least significant variables (empty)
#  CompanyNameVolvo
#  CompanyNameisuzu
#  carheight
#
#  Zero Star Variable - High VIF
#  -----------------------------
#  carheight : 5.633945
#  CompanyNameVolvo : 5.153490
#
#  Zero Star Variable - Low VIF
#  ----------------------------
#  CompanyNameisuzu : 1.690376
#
# Hence removing "carheight" first
#######################################################################

#Removed car height

model_3 <- lm(formula = price ~ symboling + aspiration + enginelocation + carlength + 
                carwidth + curbweight + cylindernumber + enginesize + 
                boreratio + horsepower + carbodyhardtop + carbodyhatchback + 
                carbodysedan + carbodywagon + fuelsystem4bbl + drivewheelrwd + 
                enginetypel + enginetypeohcf + CompanyNameaudi + CompanyNamebmw + 
                CompanyNamebuick + CompanyNamechevrolet + CompanyNamedodge + 
                CompanyNamehonda + CompanyNameisuzu + CompanyNamejaguar + 
                CompanyNamemazda + CompanyNamemercury + CompanyNamemitsubishi + 
                CompanyNamenissan + CompanyNameplymouth + CompanyNamerenault + 
                CompanyNamesaab + CompanyNametoyota + CompanyNamevolkswagen + 
                CompanyNamevolvo, data = train)

summary(model_3) # Adjusted R2 = 0.9647
a <- data.frame(sort(vif(model_3)))

##################################################################
# Least significant variables (empty)
#  CompanyNameisuzu
#  symboling
#  CompanyNamevolvo
#
#  Zero Star Variable - High VIF
#  -----------------------------
#  CompanyNameVolvo : 4.781163
#  symboling : 4.306225
#
#  Zero Star Variable - Low VIF
#  ----------------------------
#  Compnaynameisuzu : 1.689293
#
# Hence removing "CompanyNamevolvo" 
##################################################################

#Removed "CompanyNameVolvo"

model_4 <- lm(formula = price ~ symboling + aspiration + enginelocation + carlength + 
                carwidth + curbweight + cylindernumber + enginesize + 
                boreratio + horsepower + carbodyhardtop + carbodyhatchback + 
                carbodysedan + carbodywagon + fuelsystem4bbl + drivewheelrwd + 
                enginetypel + enginetypeohcf + CompanyNameaudi + CompanyNamebmw + 
                CompanyNamebuick + CompanyNamechevrolet + CompanyNamedodge + 
                CompanyNamehonda + CompanyNameisuzu + CompanyNamejaguar + 
                CompanyNamemazda + CompanyNamemercury + CompanyNamemitsubishi + 
                CompanyNamenissan + CompanyNameplymouth + CompanyNamerenault + 
                CompanyNamesaab + CompanyNametoyota + CompanyNamevolkswagen , data = train)

summary(model_4) # 0.9647
a <- data.frame(sort(vif(model_4)))

#################################################################
# Least significant variables (empty)
#  CompanyNameisuzu
#  symboling
#
#  Zero Star Variable - High VIF
#  -----------------------------
#  symboling : 4.230045
#
#  Zero Star Variable - Low VIF
#  ----------------------------
#  carNameisuzu : 1.446243
#
# Hence removing "Symboling" 
##################################################################

#Removed Symboling

model_5 <- lm(formula = price ~ aspiration + enginelocation + carlength + 
                carwidth + curbweight + cylindernumber + enginesize + 
                boreratio + horsepower + carbodyhardtop + carbodyhatchback + 
                carbodysedan + carbodywagon + fuelsystem4bbl + drivewheelrwd + 
                enginetypel + enginetypeohcf + CompanyNameaudi + CompanyNamebmw + 
                CompanyNamebuick + CompanyNamechevrolet + CompanyNamedodge + 
                CompanyNamehonda + CompanyNameisuzu + CompanyNamejaguar + 
                CompanyNamemazda + CompanyNamemercury + CompanyNamemitsubishi + 
                CompanyNamenissan + CompanyNameplymouth + CompanyNamerenault + 
                CompanyNamesaab + CompanyNametoyota + CompanyNamevolkswagen , data = train)

summary(model_5) #0.9643
a <- data.frame(sort(vif(model_5)))

##############################################################################
# Least significant variables (empty/dot)
#  CompanyNameaudi
#  CompanyNamemercury
#  CompanyNamehonda (dot)
#  CompanyNamesaab
#  CompanyNameisuzu 
#
#  Zero/(Dot) Star Variable - High VIF
#  -----------------------------
#  CompanyNamehonda:  4.684844
#  CompanyNamesaab :  2.749075
#  CompanyNameaudi :  2.421842
#
#  Zero Star Variable - Low VIF
#  ----------------------------
#  CompanyNameisuzu : 1.445572
#  CompanyNamemercury : 1.242325
#
# Hence removing "CompanyNamehonda" 
#############################################################################

#Removed CompanyNamehonda

model_6 <- lm(formula = price ~ aspiration + enginelocation + carlength + 
               carwidth + curbweight + cylindernumber + enginesize + 
               boreratio + horsepower + carbodyhardtop + carbodyhatchback + 
               carbodysedan + carbodywagon + fuelsystem4bbl + drivewheelrwd + 
               enginetypel + enginetypeohcf + CompanyNameaudi + CompanyNamebmw + 
               CompanyNamebuick + CompanyNamechevrolet + CompanyNamedodge + 
               CompanyNameisuzu + CompanyNamejaguar + 
               CompanyNamemazda + CompanyNamemercury + CompanyNamemitsubishi + 
               CompanyNamenissan + CompanyNameplymouth + CompanyNamerenault + 
               CompanyNamesaab + CompanyNametoyota + CompanyNamevolkswagen , data = train)

summary(model_6)  #0.9634
a <- data.frame(sort(vif(model_6)))

##############################################################################
# Least significant variables (empty)
# ----------------------------------
# "CompanyNameaudi"
# "CompanyNamemercury"
# "CompanyNameisuzu"
# "CompanyNamesaab"
# "drivewheelrwd"
# "horsepower"
# 
#  Zero star (empty) variables - VIF
#  ---------------------------------
#  CompanyNameaudi : 1.587785
#  CompanyNamemercury : 1.197198
#  CompanyNameisuzu : 1.379489
#  drivewheelrwd : 5.318238
#  horsepower : 11.416632
#  
#  
#  Now we will remove "horse power"
###################################################################################

#removing horsepower

model_7 <- lm(formula = price ~ aspiration + enginelocation + carlength + 
                carwidth + curbweight + cylindernumber + enginesize + 
                boreratio + carbodyhardtop + carbodyhatchback + 
                carbodysedan + carbodywagon + fuelsystem4bbl + drivewheelrwd + 
                enginetypel + enginetypeohcf + CompanyNameaudi + CompanyNamebmw + 
                CompanyNamebuick + CompanyNamechevrolet + CompanyNamedodge + 
                CompanyNameisuzu + CompanyNamejaguar + 
                CompanyNamemazda + CompanyNamemercury + CompanyNamemitsubishi + 
                CompanyNamenissan + CompanyNameplymouth + CompanyNamerenault + 
                CompanyNamesaab + CompanyNametoyota + CompanyNamevolkswagen , data = train)

summary(model_7) #0.9631
a <- data.frame(sort(vif(model_7)))

##########################################################
# Least significant variables (empty)
# ----------------------------------
# "CompanyNameaudi"
# "CompanyNamemercury"
# "CompanyNameisuzu"
# "CompanyNamesaab"
# "drivewheelrwd"
# 
#  Zero star (empty) variables - VIF
#  ---------------------------------
#  CompanyNameaudi : 1.573438
#  CompanyNamemercury : 1.148476
#  CompanyNameisuzu : 1.323907
#  CompanyNamesaab : 1.903723
#  drivewheelrwd : 4.799609
#  
#  
#  Now we will remove "drivewheelrwd"
###################################################################################

model_8 <- lm(formula = price ~ aspiration + enginelocation + carlength + 
                carwidth + curbweight + cylindernumber + enginesize + 
                boreratio + carbodyhardtop + carbodyhatchback + 
                carbodysedan + carbodywagon + fuelsystem4bbl + 
                enginetypel + enginetypeohcf + CompanyNameaudi + CompanyNamebmw + 
                CompanyNamebuick + CompanyNamechevrolet + CompanyNamedodge + 
                CompanyNameisuzu + CompanyNamejaguar + 
                CompanyNamemazda + CompanyNamemercury + CompanyNamemitsubishi + 
                CompanyNamenissan + CompanyNameplymouth + CompanyNamerenault + 
                CompanyNamesaab + CompanyNametoyota + CompanyNamevolkswagen , data = train)

summary(model_8) #0.9631
a <- data.frame(sort(vif(model_8)))

###########################################################################
# Least significant variables (empty)
# ----------------------------------
# "CompanyNameaudi"
# "CompanyNamemercury"
# "CompanyNameisuzu"
# "CompanyNamesaab"
#
# One Star variables : VIF
# ------------------------
#  carbodyhardtop : 3.236304
#  CompanyNamedodge : 1.328060
#  CompanyNamenissan : 1.773683
#  CompanyNameplymouth : 1.383847
#  CompanyNamerenault : 1.214250
#  
#  Zero star (empty) variables - VIF
#  ---------------------------------
#  CompanyNamesaab : 1.612952
#  CompanyNameaudi : 1.391919
#  CompanyNamemercury : 1.148476
#  CompanyNameisuzu : 1.286608
#  
# Since zero start variable have low vif less than 2 
# so we considered 1 start variables and we can carbodyhardtop has highest vif
##############################################################################

#removing carbodyhardtop

model_9 <- lm(formula = price ~ aspiration + enginelocation + carlength + 
                carwidth + curbweight + cylindernumber + enginesize + 
                boreratio + carbodyhatchback + 
                carbodysedan + carbodywagon + fuelsystem4bbl + 
                enginetypel + enginetypeohcf + CompanyNameaudi + CompanyNamebmw + 
                CompanyNamebuick + CompanyNamechevrolet + CompanyNamedodge + 
                CompanyNameisuzu + CompanyNamejaguar + 
                CompanyNamemazda + CompanyNamemercury + CompanyNamemitsubishi + 
                CompanyNamenissan + CompanyNameplymouth + CompanyNamerenault + 
                CompanyNamesaab + CompanyNametoyota + CompanyNamevolkswagen , data = train)

summary(model_9) #0.9615
a <- data.frame(sort(vif(model_9)))

###########################################################################
# Least significant variables (empty)
# ----------------------------------
# "CompanyNameaudi"
# "CompanyNamemercury"
# "CompanyNameisuzu"
# "CompanyNamesaab"
#
# One Star variables : VIF
# ------------------------
#  carbodyhatchback : 6.341660
#  carlength  : 11.802776
#  CompanyNamedodge : 1.327620
#  CompanyNameplymouth : 1.382924
#  CompanyNamerenault : 1.212674
#  
#  Zero star (empty) variables - VIF
#  ---------------------------------
#  CompanyNamesaab : 1.610528
#  CompanyNameaudi : 1.391820
#  CompanyNamemercury : 1.144097
#  CompanyNameisuzu : 1.282829
#  
# Since zero start variable have low vif less than 2 
# so we considered 1 start variables and we can carlength has highest vif
###########################################################################

#removing carlength

model_10 <- lm(formula = price ~ aspiration + enginelocation + 
                carwidth + curbweight + cylindernumber + enginesize + 
                boreratio + carbodyhatchback + 
                carbodysedan + carbodywagon + fuelsystem4bbl + 
                enginetypel + enginetypeohcf + CompanyNameaudi + CompanyNamebmw + 
                CompanyNamebuick + CompanyNamechevrolet + CompanyNamedodge + 
                CompanyNameisuzu + CompanyNamejaguar + 
                CompanyNamemazda + CompanyNamemercury + CompanyNamemitsubishi + 
                CompanyNamenissan + CompanyNameplymouth + CompanyNamerenault + 
                CompanyNamesaab + CompanyNametoyota + CompanyNamevolkswagen , data = train)

summary(model_10) #0.96
a <- data.frame(sort(vif(model_10)))

##########################################################################
# Least significant variables (empty) /(Dot)
# ----------------------------------
# "CompanyNameaudi"
# "CompanyNamemercury"
# "CompanyNameisuzu"
# "CompanyNamesaab"
# "CompanyNamerenault"
# "CompanyNameDodge"
#
# One Star variables : VIF
# ------------------------
#  carbodyhatchback : 6.327478
#  carbodysedan : 6.026321
#  aspiration : 2.040902
#  CompanyNamenissan : 1.697105
#  CompanyNameplymouth : 1.356295
#  CompanyNamerenault : 1.180599
#  
#  Zero star (empty) variables / (Dot) - VIF
#  ---------------------------------
#  CompanyNamesaab : 1.366050
#  CompanyNameaudi : 1.373145
#  CompanyNamemercury : 1.143600
#  CompanyNameisuzu : 1.263309
#  
# Since zero start variable have low vif less than 2 
# so we considered 1 start variables and we can carbodyhatchback has highest vif.
##########################################################################

#removing carbodyhatchback

model_11 <- lm(formula = price ~ aspiration + enginelocation + 
                 carwidth + curbweight + cylindernumber + enginesize + 
                 boreratio + 
                 carbodysedan + carbodywagon + fuelsystem4bbl + 
                 enginetypel + enginetypeohcf + CompanyNameaudi + CompanyNamebmw + 
                 CompanyNamebuick + CompanyNamechevrolet + CompanyNamedodge + 
                 CompanyNameisuzu + CompanyNamejaguar + 
                 CompanyNamemazda + CompanyNamemercury + CompanyNamemitsubishi + 
                 CompanyNamenissan + CompanyNameplymouth + CompanyNamerenault + 
                 CompanyNamesaab + CompanyNametoyota + CompanyNamevolkswagen , data = train)

summary(model_11) #0.9582
a <- data.frame(sort(vif(model_11)))

#####################################################
# Least significant variables (empty) /(Dot)
# ----------------------------------
# "CompanyNameaudi"
# "CompanyNamemercury"
# "CompanyNameisuzu"
# "CompanyNamesaab"
# "carbodysedan"
#
# One Star variables : VIF
# ------------------------
#  carbodywagon : 2.050445
#  aspiration : < 2
#  CompanyNamenissan : < 2
#  CompanyNameplymouth : < 2 
#  CompanyNamerenault : < 2
#  CompanyNamevolkswagen : < 2
#  
#  Zero star (empty) variables / (Dot) - VIF
#  ---------------------------------
#  CompanyNamesaab : < 2
#  CompanyNameaudi : < 2
#  CompanyNamemercury : < 2
#  CompanyNameisuzu : < 2
#  carbodysedan : < 2
# Since zero start variable have low vif less than 2 
# so we considered 1 start variables and we can carbodywagon has highest vif

####################################################

#removing carbodywagon

model_12 <- lm(formula = price ~ aspiration + enginelocation + 
                 carwidth + curbweight + cylindernumber + enginesize + 
                 boreratio + 
                 carbodysedan + fuelsystem4bbl + 
                 enginetypel + enginetypeohcf + CompanyNameaudi + CompanyNamebmw + 
                 CompanyNamebuick + CompanyNamechevrolet + CompanyNamedodge + 
                 CompanyNameisuzu + CompanyNamejaguar + 
                 CompanyNamemazda + CompanyNamemercury + CompanyNamemitsubishi + 
                 CompanyNamenissan + CompanyNameplymouth + CompanyNamerenault + 
                 CompanyNamesaab + CompanyNametoyota + CompanyNamevolkswagen , data = train)

summary(model_12) # 0.9565
a <- data.frame(sort(vif(model_12)))

#########################################################

# Following have high VIF:
#-------------------------
  
#curbweight -- 3 star
#enginesize -- 2 star
#carwidth -- 2 star
#cylindernumber -- 3 star
#boreration - 3 star
#enginetypeohcf - 3 star
#enginelocation - 3 star
#enginetypel - 3 star
#fuelsystem4bbl - 3 star
#CompanyNamebuick - 3 star
#CompanyNametoyota - 3 star

  
#Rest all variables are having VIF under 2
  
# WE Will now remove insignificant variables:

#CompanyNamesaab (Dot)

#Zero Star

#CompanyNamemercury
#CompanyNameisuzu
#CompanyNameaudi
#carbodysedan

#########################################################

model_13 <- lm(formula = price ~ aspiration + enginelocation + 
                 carwidth + curbweight + cylindernumber + enginesize + 
                 boreratio + 
                 fuelsystem4bbl + 
                 enginetypel + enginetypeohcf + CompanyNamebmw + 
                 CompanyNamebuick + CompanyNamechevrolet + CompanyNamedodge + 
                 CompanyNamejaguar + 
                 CompanyNamemazda + CompanyNamemitsubishi + 
                 CompanyNamenissan + CompanyNameplymouth + CompanyNamerenault + 
                 CompanyNamesaab + CompanyNametoyota + CompanyNamevolkswagen , data = train)

summary(model_13) # 0.9569
a <- data.frame(sort(vif(model_13)))

########################################################
#Least significant variables
#--------------------------

#CompanyNamesaab -- Dot but very low vif : 1.214915 - not removing it


# Enginesize High Vif : 15.325189 and Single star - so we will remove it.


##############################################################################

model_14 <- lm(formula = price ~ aspiration + enginelocation + 
                 carwidth + curbweight + cylindernumber + 
                 boreratio + 
                 fuelsystem4bbl + 
                 enginetypel + enginetypeohcf + CompanyNamebmw + 
                 CompanyNamebuick + CompanyNamechevrolet + CompanyNamedodge + 
                 CompanyNamejaguar + 
                 CompanyNamemazda + CompanyNamemitsubishi + 
                 CompanyNamenissan + CompanyNameplymouth + CompanyNamerenault + 
                 CompanyNamesaab + CompanyNametoyota + CompanyNamevolkswagen , data = train)

summary(model_14) #0.9549
a <- data.frame(sort(vif(model_14)))


########################################################
# HIGH VIF
#--------
# curbweight : 10.68 : 2 star
# carwidth : 6.895805 : 3 star
# cylindernumber : 4.154065 : 3 star
# engineohcf : 2.290611 : 3 star
# boreratio : 3.09 : 3 star
# they all have 3/2 stars so we cannot remove them

#Following have 1 star
#---------------------
#CompanyNamedodge
#CompanyNamesaab
#CompanyNamevolkswagen
##############################################################################

model_15 <-lm(formula = price ~ aspiration + enginelocation + 
                carwidth + curbweight + cylindernumber + 
                boreratio + 
                fuelsystem4bbl + 
                enginetypel + enginetypeohcf + CompanyNamebmw + 
                CompanyNamebuick + CompanyNamechevrolet + 
                CompanyNamejaguar + 
                CompanyNamemazda + CompanyNamemitsubishi + 
                CompanyNamenissan + CompanyNameplymouth + CompanyNamerenault + 
                CompanyNametoyota , data = train)

summary(model_15) #0.9495
a <- data.frame(sort(vif(model_15)))

############################################################################

#All variables that high vif:
#---------------------------
#curbweight - 3 star
#carwidth - 3 star
#cylindernumber - 3 star
#boreratio -3 star
#enginetypeohcf - 3 star

# So cannot remove them

#Will remove all single star variables now
#CompanyNamemazda
#CompanyNamenissan
#CompanyNameplymouth
#CompanyNamerenault
#aspiration

##############################################################################

model_16 <- lm(formula = price ~ enginelocation + 
                 carwidth + curbweight + cylindernumber + 
                 boreratio + 
                 fuelsystem4bbl + 
                 enginetypel + enginetypeohcf + CompanyNamebmw + 
                 CompanyNamebuick + CompanyNamechevrolet + 
                 CompanyNamejaguar + 
                 CompanyNamemitsubishi + 
                 CompanyNametoyota , data = train)


summary(model_16) #0.9425
a <- data.frame(sort(vif(model_16)))

############################################################################
#All variables that high vif:
#---------------------------
#curbweight - 3 star
#carwidth - 3 star
#cylindernumber - 3 star

# So cannot remove them

#boreratio -1 star (vif = 2.81)

# We will remove it

##############################################################################

model_17 <- lm(formula = price ~ enginelocation + 
                 carwidth + curbweight + cylindernumber + 
                 fuelsystem4bbl + 
                 enginetypel + enginetypeohcf + CompanyNamebmw + 
                 CompanyNamebuick + CompanyNamechevrolet + 
                 CompanyNamejaguar + 
                 CompanyNamemitsubishi + 
                 CompanyNametoyota , data = train)

summary(model_17) #0.9406
a <- data.frame(sort(vif(model_17)))

###############################
# Now removing enginetypeohcf
###############################

model_18 <- lm(formula = price ~ enginelocation + 
                 carwidth + curbweight + cylindernumber + 
                 fuelsystem4bbl + 
                 enginetypel + CompanyNamebmw + 
                 CompanyNamebuick + CompanyNamechevrolet + 
                 CompanyNamejaguar + 
                 CompanyNamemitsubishi + 
                 CompanyNametoyota , data = train)

summary(model_18) #0.94
a <- data.frame(sort(vif(model_18)))

#Now removing "CompanyNamemitsubishi"

model_19 <- lm(formula = price ~ enginelocation + 
                 carwidth + curbweight + cylindernumber + 
                 fuelsystem4bbl + 
                 enginetypel + CompanyNamebmw + 
                 CompanyNamebuick + CompanyNamechevrolet + 
                 CompanyNamejaguar + 
                 CompanyNametoyota , data = train)

summary(model_19) #0.939
a <- data.frame(sort(vif(model_19)))

#now removing enginetypel : 2 star

model_20 <- lm(formula = price ~ enginelocation + 
                 carwidth + curbweight + cylindernumber + 
                 fuelsystem4bbl + 
                 CompanyNamebmw + 
                 CompanyNamebuick + CompanyNamechevrolet + 
                 CompanyNamejaguar +
                 CompanyNametoyota ,
                 data = train)

summary(model_20) #0.935
a <- data.frame(sort(vif(model_20)))

#now removing CompanyNametoyota : 2 star

model_21 <- lm(formula = price ~ enginelocation + 
                 carwidth + curbweight + cylindernumber + 
                 fuelsystem4bbl + 
                 CompanyNamebmw + 
                 CompanyNamebuick + CompanyNamechevrolet + 
                 CompanyNamejaguar, 
               data = train)

summary(model_21) #0.933
a <- data.frame(sort(vif(model_21)))

#now removing CompanyNamechevrolet as it has 2 star

model_22 <- lm(formula = price ~ enginelocation + 
                 carwidth + curbweight + cylindernumber + 
                 fuelsystem4bbl + 
                 CompanyNamebmw + 
                 CompanyNamebuick + 
                 CompanyNamejaguar, 
               data = train)

summary(model_22) #0.9291
a <- data.frame(sort(vif(model_22)))


#############################
#model_22 is the final model
############################

#######################################
#predicting the results in test dataset
#######################################

Predict_1 <- predict(model_22,test[,-1])
test$test_price <- Predict_1

#############################################################
# Testing the the r square between actual and predicted price.
#############################################################

r <- cor(test$price,test$test_price)
rsquared <- cor(test$price,test$test_price)^2
rsquared  #0.8133468

