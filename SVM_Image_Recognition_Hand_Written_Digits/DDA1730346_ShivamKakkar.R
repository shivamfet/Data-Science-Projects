#------------------
#BUSINES OBJECTIVE
#------------------

#To develop a model that can correctly identify the digits written in an image.

#Loading the required libraries

library(caret)
library(ggplot2)
library(kernlab)
library(dplyr)
library(gridExtra)
library(corrplot)
library(doParallel)
library(caTools)

#---------------------------------------
#DATA UNDERSTANDING AND DATA PREPARATION
#---------------------------------------

####################################
#Loading the train and test datasets
####################################

train <- read.csv("mnist_train.csv" , header = FALSE , stringsAsFactors = FALSE)
test <- read.csv("mnist_test.csv" ,   header = FALSE , stringsAsFactors = FALSE)

###############################################################
#Checking the structure and dimension of train and test dataset
###############################################################

str(train)
str(test)

dim(train) # 60000 rows, 785 cols
dim(test)  # 10000 rows ,785 cols


#############################################
#Checking for missing values in train dataset
#############################################

sum(is.na(train)) #0

############################################
#Checking for missing values in test dataset
############################################

sum(is.na(test)) #0

########################################################################################################################
#Creating a combined dataset of train and test so that we can reduce the columns ins-ync, after dimensionality reduction
#we will split them back to train and test
########################################################################################################################

combined <- rbind(train , test)

####################################################################
#Checking for columns that have one unique value in combined dataset
####################################################################

cnt_uniq_val_eq_1 <- function(df) {
  cnt_uniq_val_per_column <- sapply(df, function(x)length(unique(na.omit(x))))
  cols_uniq_val_eq_1 <- names(cnt_uniq_val_per_column[cnt_uniq_val_per_column == 1])
  return(cols_uniq_val_eq_1)
}

cols_uniq_vals_eq_1 <- cnt_uniq_val_eq_1(combined)
cols_uniq_vals_eq_1  

#######################################################
#Imputing those columns that have only one unique value
########################################################

combined <- combined[ , !(names(combined) %in% cols_uniq_vals_eq_1)]

# Storing the combined data frame in combined_orig

combined_orig <- combined

#Removing the target/output variable before removing near zero variance predictors

combined <- combined[ , -1]

###########################################
#Removing the near zero variance predictors
###########################################

remove_cols <- nearZeroVar(combined , freqCut = 95/5, names = TRUE , uniqueCut = 5)
all_cols <- names(combined)

combined <- combined[ , setdiff(all_cols , remove_cols)]

##########################################################
#Dimensionality reduction using Priciple Component Analysis
############################################################

combined_2_pca <- prcomp(combined , scale. = TRUE)

screeplot(combined_2_pca , type="lines" , npcs = 70)

#---------------------------------------------------------------------------------------------------------------
# From the above screeplot it is quite eveident that nearly 97-98% of the variation is explained by 60 variables
# hence we will reduce our dimensions to 60
#---------------------------------------------------------------------------------------------------------------

combined_final <-as.matrix(combined) %*% combined_2_pca$rotation[,1:60]
combined_final <- as.data.frame(combined_final)

combined_final <- cbind(combined_orig[,1] , combined_final)
names(combined_final)[1] <- "target"

######################################################################
#Splitting the final (complete) train and test after Data Preparation
######################################################################

#######################################################################################################################
#First 60000 rows are of training dataset & last 10000 rows are of test dataset as we combined them initially via rbind
#######################################################################################################################

train_final <- combined_final[1:60000,]
test_final <- combined_final[60001:70000,]

###############################################
#Coverting the output/target variable to factor
###############################################

train_final$target <- as.factor(train_final$target)
test_final$target <- as.factor(test_final$target)

# Taking a sample of train dataset as 30%

set.seed(100)

train_indices_1 <- sample.split(train_final$target , SplitRatio = 0.3)
train_1 <- train_final[train_indices_1,]

#--------------
#Model Building
#--------------

################################################################################
# Building a Linear model - SVM  at Cost(C) = 1 using sample training data (30%)
################################################################################

#Model with sampled training data i.e.

nrow(train_1) #- #18000 - 30%

start_time <- Sys.time()  
model_1<- ksvm(target ~ ., data = train_1,scale = FALSE,C=1)
end_time <- Sys.time()

diff <- end_time - start_time #54.21 secs is the Time taken

###########################################
#Predicting the test results using model_1
###########################################

evaluate_1<- predict(model_1, test_final[,-1])
evaluate_1

# Confusion Matrix - 
# Finding accuracy, Sensitivity and specificity

confusionMatrix(evaluate_1, test_final$target)

#Acurracy is 0.964
#Sensitiviy [For Classes 0 - 9 respectively] is 0.9898 , 0.9877 , 0.9486 , 0.9604 , 0.9766 , 0.9630 , 0.9697 , 0.9504 , 0.9528 , 0.9395
#Average Sensitivity - 0.96385
#Specificity[For Classes 0 - 9 respectively] is 0.9967 , 0.9980 , 0.9945 , 0.9947 , 0.9950 , 0.9962 , 0.9973 , 0.9962 , 0.9953 , 0.9961
#Average Specificity - 0.996

###########################################################################
#BUilding a Linear model - SVM at Cost(C) = 1 using full training data set
###########################################################################

start_time <- Sys.time() 
model_2 <-ksvm(target ~ . , data = train_final , scale = FALSE , C = 1)
end_time <- Sys.time()

diff <- end_time - start_time # 6.20 mins

###########################################
#Predicting the test results using model_2
###########################################

evaluate_2<- predict(model_2, test_final[,-1])
evaluate_2

# Confusion Matrix - 
# Finding accuracy, Sensitivity and specificity

confusionMatrix(evaluate_2, test_final$target)

#Accuracy - 0.9756
#Sensistivty[For Classes 0 - 9 respectively] - 0.9918 , 0.9912 , 0.97 , 0.9752 , 0.9786 , 0.9709 , 0.9791 , 0.9650 , 0.9723 , 0.9604
#Average Sensitivity - 0.97545
#Specificity[For Classes 0 - 9 respectively] - 0.9970 , 0.9983 , 0.9961 ,0.9968 , 0.9974 , 0.9973 ,0.9982 , 0.9968 , 0.9973 , 0.9977
#Average Specificity - 0.99729

########################################################
# LINEAR Model SVM with C = 10 on sampled training data
########################################################

start_time <- Sys.time() 
model_3 <- ksvm(target ~ ., data = train_1,scale = FALSE,C=10)
end_time <- Sys.time()

diff <- end_time - start_time #49.26 secs
 
#Predicting the test results using model_3

evaluate_3<- predict(model_3, test_final[,-1])
evaluate_3

# Confusion Matrix - 
# Finding accuracy, Sensitivity and specificity

confusionMatrix(evaluate_3, test_final$target)

#Accuracy - 0.9716
#Sensitivity  - 0.9878 , 0.9885 , 0.9583 , 0.9663 , 0.9735 , 0.9608 , 0.9687 , 0.9650 , 0.9548 , 0.9465
#Average Sensitivity - 0.967
#Specificity -  0.9966 , 0.9980 , 0.9952 , 0.99544, 0.9967 , 0.9963 , 0.9962 , 0.9968 , 0.9958 , 0.9968
#Average Specificity - 0.9963

################################################
# LINEAR Model with C = 10 on full training data
################################################

start_time <- Sys.time() 
model_4 <- ksvm(target ~ ., data = train_final,scale = FALSE,C=10)
end_time <- Sys.time()

diff <- end_time - start_time  #4.92 mins

# Predicting the test results using model_4

evaluate_4<- predict(model_4, test_final[,-1])
evaluate_4

# Confusion Matrix - 
# Finding accuracy, Sensitivity and specificity

confusionMatrix(evaluate_4, test_final$target)

#Accuracy - 0.982
#Sensitivity  - 0.9898 , 0.9956 , 0.9797 , 0.9802 , 0.9837 , 0.9798 , 0.9802 , 0.9796 ,0.9805 , 0.9693
#Average Sensitivity - 0.98184
#Specificity -  0.9976 , 0.9989,  0.9972 , 0.9983 , 0.9983 , 0.9976 , 0.9982 , 0.9979 ,0.9982 , 0.9978
#Average Specificity - 0.998
#-----------------------------------------------------------------------------------------------------

#######################################################################################
# Building a non linear Model using RBF Kernel on the sample training dataset (i.e 30%)
#######################################################################################

start_time <- Sys.time()
model_RBF <- ksvm(target ~ ., data = train_1, scale = FALSE, kernel = "rbfdot")
end_time <- Sys.time()

diff <- end_time - start_time #model building took 52.87 secs

eval_RBF<- predict(model_RBF, test_final[,-1])

#confusion matrix - RBF Kernel

confusionMatrix(eval_RBF,test_final$target)

#Accuracy is 0.9639
#Sensitivity [Class 0 - Class 9 respectively] -  0.9898   0.9877   0.9486   0.9604   0.9766   0.9630   0.9697   0.9494   0.9528   0.9395
#Average Sensitivity - #0.96375
#Specificity [Class 0 - Class 9 respectively] -  0.9967   0.9980   0.9944   0.9947   0.9950   0.9962   0.9973   0.9962   0.9953   0.9961
#Average Specificity - #0.99599

###########################################################################
# Building a non linear Model using RBF Kernel on the full training dataset
###########################################################################

start_time <- Sys.time()
model_RBF_2 <- ksvm(target ~ ., data = train_final, scale = FALSE, kernel = "rbfdot")
end_time <- Sys.time()

diff <- end_time - start_time #Model building took 5.15 mins

eval_RBF_2 <- predict(model_RBF_2, test_final[,-1])

#confusion matrix - RBF Kernel

confusionMatrix(eval_RBF_2,test_final$target)

#Accuracy is # 0.9755
#Sensitivity [ Class 0 - Class 9 respectively ]-  0.9918   0.9912   0.9700   0.9752   0.9786   0.9709   0.9791   0.9650   0.9713   0.9604
#Average Sensitivity - 0.97535
#Specificity [Class 0 - Class 9 respectively ] -  0.9970   0.9983   0.9961   0.9967   0.9974   0.9973   0.9982   0.9968   0.9973   0.9977
#Average Specificity - 0.99728


#----------------------------------------
#CROSS VALIDATION & HYPERPARAMETER TUNING
#----------------------------------------

#########################################################
#Hyperparameter tuning and Cross Validation - LINEAR SVM
#########################################################

registerDoParallel(makeCluster(detectCores()))

# Using the train function from caret package to perform crossvalidation

# Number - Number of folds 
# Method - cross validation

set.seed(100)

trainControl <- trainControl(method="cv", number=5)
metric <- "Accuracy"

#############################
# making a grid of C values. 
#############################

grid <- expand.grid(C=seq(1, 10, by=1))

####################################
# Performing 5-fold cross validation
####################################

start_time <- Sys.time()
fit.svm <- train(target~., data=train_1, method="svmLinear", metric=metric, 
                 tuneGrid=grid, trControl=trainControl)
end_time <- Sys.time()
diff <- end_time - start_time #11.57 mins

#############################################
# Printing & plotting cross validation result
#############################################

print(fit.svm)
plot(fit.svm)

# Best tune at C = 1 
# Accuracy - 0.913  , Kappa - 0.9033

##########################################################
# Valdiating the model after cross validation on test data
##########################################################

evaluate_linear_test<- predict(fit.svm, test_final[,-1])
confusionMatrix(evaluate_linear_test, test_final$target) 

#Accuracy - #92%
#Sensitivity [Class 0 - Class 9 respectively]  -  0.9694   0.9850   0.9070   0.9208   0.9369   0.8643   0.9384   0.9222   0.8686   0.8821
#Average Sensitivity - #0.919
#Specificity [Class 0 - Class 9 respectively]  -  0.9941   0.9955   0.9886   0.9862   0.9901   0.9858   0.9945   0.9932   0.9917   0.9923
#Average Specificity -  #0.9912

fit.svm.linear <- fit.svm

#--------------------------------------------------------------
#Hyperparameter tuning and Cross Validation for NON-LINEAR SVM
#--------------------------------------------------------------

#registerDoParallel(makeCluster(detectCores()))

# Using the train function from caret package to perform Cross Validation. 

# traincontrol function Controls the computational nuances of the train function.
# --- method =  CV means  Cross Validation.
# --- Number = 5 implies Number of folds in CV.

trainControl <- trainControl(method="cv", number=5)

################################
# Evaluation metric is Accuracy
################################

metric <- "Accuracy"
set.seed(7)

#########################
#Setting hyperparameters
#########################

grid <- expand.grid(.sigma=seq(0.01, 0.05, by = 0.01), .C=seq(1 , 5 , by = 1) )

###################################################################
#train function takes Target ~ Prediction, Data, Method = Algorithm
#Metric = Type of metric, 
#tuneGrid = Grid of Parameters,
#trcontrol = Our traincontrol method.
###################################################################

start_time <- Sys.time()

fit.svm <- train(target ~., data=train_1, method="svmRadial", metric=metric, 
                 tuneGrid=grid, trControl=trainControl)

end_time <- Sys.time()

diff <- end_time - start_time # 2 hrs

print(fit.svm)
plot(fit.svm)

#Best tune at C = 3 and sigma = 0.02 , accuracy = 0.9685 , kappa = 0.9649

evaluate_non_linear_test<- predict(fit.svm, test_final[,-1])
confusionMatrix(evaluate_non_linear_test, test_final$target) # Accuracy 0.9705
 
#----------------------------------------------------------------------------------------------------------------------------------------
# Accuracy  comes as 0.9705 ,  Kappa : 0.9672 
# Sensitivity [Class 0 - Class 9 respectively] :  0.9867   0.9868   0.9603   0.9713   0.9745   0.9697   0.9739   0.9630   0.9692   0.9485
# Average Sensitivity - 0.97039
# Specificity [Class 0 - Class 9 respectivey] :  0.9975   0.9989   0.9953   0.9949   0.9969   0.9971   0.9978   0.9975   0.9942   0.9971
# Avergae Specificity - 0.99672
#----------------------------------------------------------------------------------------------------------------------------------------

fit.svm.non.linear <- fit.svm


#-----------------------------------------------------------------------------------------------------------------------------------------

#----------
# SUMMARY #
#----------

#LINEAR MODEL SVM WITH C=1 - (30 % TRAINING DATA SET - 18k observations)-ACCURACY = 96.4% : AVG SENSITIVITY = 96.4% : AVG SPECIFICITY = 99.6%
#LINEAR MODEL SVM WITH C=1 - (FULL TRAINING DATA SET - 60k onservations)-ACCURACY = 97.6% : AVG SENSITIVITY = 97.5% : AVG SPECIFICITY = 99.7%
#LINEAR MODEL SVM WITH C=10 -(30 % TRAINING DATA SET - 18k observations)-ACCURACY = 96.7% : AVG SENSITIVITY = 96.7% : AVG SPECIFICITY = 99.6%
#LINEAR MODEL SVM WITH C=10 -(FULL TRAINING DATA SET - 60k onservations)-ACCURACY = 98.2% : AVG SENSITIVITY = 98.2% : AVG SPECIFICITY = 99.8%
#--------------------------------------------------------------------------------------------------------------------------------------------

#NON LINEAR MODEL RBF KERNEL - (30% TRAINING DATA SET - 18k observations)-ACCURACY = 96.4% : AVG SENSITIVITY = 96.4% : AVG SPECIFICITY = 99.6%
#NON LINEAR MODEL RBF KERNEL - (FULL TRAINING DATA SET - 60k onservations)-ACCURACY = 97.6% : AVG SENSITIVITY = 97.5% : AVG SPECIFICITY = 99.7%

#_-------------------------------------------------------------------------------------------------------------------------------------------

#HYPERPARAMETER TUNING CROSS VALIDATION LINEAR SVM - # Best tune at C = 1 : Accuracy - 91.3% : Kappa - 0.9033 - 
# - ON TEST DATA - ACCURACY - 92.08% : 91.9% AVG SENSITIVITY = 99.1%

#--------------------------------------------------------------------------------------------------------------------------------------------

#HYPERPARAMETER TUNING CROSS VALIDATION NON LINEAR SVM - Best tune at C = 3 and sigma = 0.02 , ACCURACY - 96.99% , kappa = 0.9665
# - ON TEST DATA - ACCURACY - 97% : 97.04% AVG SENSITIVITY = 99.7%




