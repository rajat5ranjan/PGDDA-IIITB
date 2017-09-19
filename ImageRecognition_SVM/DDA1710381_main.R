# ------------------------------------------------------- #
# Code Description : PGDDA - SVM Assignment               #
# Author           : Anugraha Sinha                       #
# Roll Number      : DDA1710381                           #
# ------------------------------------------------------- #

# -- Setup -- #
required_packages <- c("kernlab","readr","caret","dplyr","caTools","ggplot2","gridExtra")
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if (length(new_packages)) install.packages(new_packages)

library(kernlab)
library(readr)
library(caret)
library(dplyr)
library(caTools)
library(ggplot2)
library(gridExtra)

setwd("C:/Users/anugraha.sinha/OneDrive/Documents/Post Graduate Data Sciences IIIT-Bangalore Upgrad/Main Program/4_Predictive_Analytics-2/Assignment-SVM/SVM Dataset")

train <- read.delim("mnist_train.csv",sep=",",stringsAsFactors = FALSE,header = FALSE)
test <- read.delim("mnist_test.csv",sep=",",stringsAsFactors = FALSE,header = FALSE)
# -- Setup Complete -- #

# -- Business Understanding -- #
# Problem Statement : The data set given is image data converted to gray scale pixel information
#                     having 784 pixels per image signifying numerical numbers.
#                     This is a multi-class classification problem, to learn from train data set
#                     (having 60000 image data) and test the model on test data (10000 image data)
# --                        -- #

# -- Data Understanding/Preparation -- #

# Lets change the column name of the first column for both train and test
# as it signifies the class (the numbers)
colnames(train)[1] <- "number"
colnames(test)[1] <- "number"

# Lets add a column signifying train/test data in each data.frame #
train$type <- "train"
test$type <- "test"

# Lets combine both test and train together for data undestanding #
combinedDataSet <- rbind(train,test)

# Lets see if there are any NAs in the combined Data Set #
which(sapply(combinedDataSet,function(x) sum(is.na(x))) != 0)
# No NAs

# Lets see if some features have same value in them
cols_with_same_value <- which(sapply(combinedDataSet,function(x) length(unique(x)) == 1))
length(cols_with_same_value) # 65 columns 
cols_with_same_value
# There are 65 column which have same value present in all data points (test & train)
# combined.
# If the value in each column is same then, we can say that axis of the dimension is a constant
# and will not suffice any variance in getting valuable information for prediction.
# Hence removing those columns

combinedDataSet_2 <- combinedDataSet %>% select(-cols_with_same_value)

# According to problem statement, each pixel provides the density of black in that pixel
# Therefore, values range from 0 to 255. Hence we should see if there are some block
# which has value more than 255.

which(combinedDataSet_2 %>% select(-type,-number) > 255)
# No value of pixel is greater than 255
which((combinedDataSet_2 %>% select(number)) > 9 | (combinedDataSet_2 %>% select(number)) < 0)
# No number given is more than 9 or less than zero.

# Lets get the data back in train and test sets now.

train_2 <- combinedDataSet_2 %>% filter(type == "train") %>% select(-type)
test_2 <- combinedDataSet_2 %>% filter(type == "test") %>% select(-type)

# Convert the number column to factors for both data set #
train_2$number <- as.factor(train_2$number)
test_2$number <- as.factor(test_2$number)

# Since the train data set is very large, we will have to sample some data out of actual
# for training of the model #
# Explanation of sampling :
# Case 1:
# If we go by the 80/20 rule of train and test set, then our test data is 10000 rows,
# which means that our train set should be 40000 rows. which is just 1/3rd less than the original
# 60000 rows.
# Case 2:
# Since we have 10000 rows for our test set, we can also do the training on double the data
# thereby making the train set as 20000 rows. This would reduce the train set by 66.67%,
# which has a higher chance of giving computationally better results.
# However, we should check the distribution of data for better understanding.

set.seed(100)

case_1_train_sampled_idx <- sample.split(train_2$number,SplitRatio = 0.6667)
case_1_train_set <- train_2[case_1_train_sampled_idx,]
# nrow(case_1_train_set) = 40003

case_2_train_sampled_idx <- sample.split(train_2$number,SplitRatio = 0.3333)
case_2_train_set <- train_2[case_2_train_sampled_idx,]
# nrow(case_2_train_set) = 19997


G01 <- ggplot(case_1_train_set) + 
  geom_bar(aes(x=as.factor(number)),fill="blue",col="black") +
  scale_y_continuous(breaks = seq(0,4700,500)) +
  labs(title="Distribution Case 1\nTrainSet = 40003 rows",x="Numbers",y="Count")

G02 <- ggplot(case_2_train_set) + 
  geom_bar(aes(x=as.factor(number)),fill="blue",col="black") +
  scale_y_continuous(breaks = seq(0,2400,200)) +
  labs(title="Distribution Case 2\nTrainSet = 19997 rows",x="Numbers",y="Count")

grid.arrange(G01,G02)

# Conclusion : The distribution in both case if pretty balanced, and within the numbers
#            : also, there seem to be balanced occurrances of each number.
#            : Therefore, in order to keep the computation time low, we will go with
#            : Case 2.
# Note       : We are assuming that in this sampling, we are able to take case of all
#            : possible cases in which each number (2,3,4 etc) can be written.
#            : We are assuming that we are not missing any surprise patterns of writing
#            : a specific number would be present in test data set, which we would have
#            : missed in this sampling.
# 

train_set <- case_2_train_set
test_set <- test_2
# -- Data Understanding/Preparation complete -- #


# -- Model Preparation -- #

# ------ LINEAR SVM and performance check (Default parameters) ------ #
# lets build a standard SVM model first and see the performance #
# Note : There will be some warning here, because of scaling issue. But we can ignore them.
SVM_linear_model_1 <- ksvm(number~.,
                           data=train_set,
                           scale=FALSE,
                           kernel="vanilladot")

SVM_linear_model_1
# Hyper Parameter : C = 1
# Support Vectors : 4388

# Lets check the training accuracy #
eval_train_linear_model_1 <- predict(SVM_linear_model_1,train_set)
confusionMatrix(eval_train_linear_model_1,train_set$number)
# Train Set Net Accuracy = 1.0

# Lets check test accuracy #
eval_test_linear_model_1 <- predict(SVM_linear_model_1,test_set)
confusionMatrix(eval_test_linear_model_1,test_set$number)
# Test Set Net Accuracy of model = 0.9132

# ------ NON-LINEAR (RBF - Kernel) SVM and performance check (Default Parameters) ------ #
# Lets build a RBF kernel model with default parameters and see if performance increases #
# Note : There will be some warning here, because of scaling issue. But we can ignore them.
SVM_non_linear_RBF_model_1 <- ksvm(number~.,
                                   data=train_set,
                                   scale=FALSE,
                                   kernel="rbfdot")
SVM_non_linear_RBF_model_1
# Hyper Parameter :     C = 1
#                 : sigma = 1.63691367429906e-07
# Support Vectors : 6018

# Lets check the training accuracy #
eval_train_non_linear_model_1 <- predict(SVM_non_linear_RBF_model_1,train_set)
confusionMatrix(eval_train_non_linear_model_1,train_set$number)
# Train Set Net Accuracy = 0.9838

# Lets Check test accuracy #
eval_test_non_linear_RBF_model_1 <- predict(SVM_non_linear_RBF_model_1,test_set)
confusionMatrix(eval_test_non_linear_RBF_model_1,test_set$number)
# Net Accuracy of model = 0.9673

# Conclusion from modelling exercise :
# It seems the RBF model is performing better (even with default kernel parameters) #
# This is because, there is larger gap between train accuracy and test accuracy with linear model.
# With non-linear model (default parameters), the gap is still shorter, and hence
# we can tune non-linear model through cross validation for better results.

# We should try building the cross validation based on this
# The Sigma built by default RBF model says , sigma = 1.63334422481776e-07
# As we saw that in linear model also we got an accuracy of 0.9132, sigma of non-linear
# model is of the order 1e-7, that means there is no much non-linearity in the data.
# However, having some non-linearity is definitely helping the accuracy.


# ---- Model Evaluation - Cross Validation ---- #

# We will doing cross validation for RBF Model, with following login
# Cross Validation folds = 3
# Range of sigma = 0.63e-7, 1.63e-7, 2.63e-7
# Range of C = 1 2 3
#
# Note : In order for faster computation, we are keep limited levels in grids and cross validation
# Below cross validation process can take upto 2.5 hrs on some systems
# In order to get granular analysis, we can consider increasing the level in sigma & C for more fine
# analysis.

trainControl <- trainControl(method = "cv", number = 3,verboseIter=TRUE)

metric <- "Accuracy"

set.seed(90)

grid <- expand.grid(.sigma = c(0.63e-7,1.63e-7,2.63e-7),.C=c(1,2,3))

# Note : There will be some warning here (for every iteration of cv), because of scaling issue. 
#        But we can ignore them.

SVM_non_linear_RBF.fit <- train(number~.,
                                data=train_set,
                                method="svmRadial",
                                metric=metric,
                                tuneGrid=grid,
                                trControl=trainControl)
SVM_non_linear_RBF.fit
#Support Vector Machines with Radial Basis Function Kernel 
#19997 samples
#719 predictor
#10 classes: '0', '1', '2', '3', '4', '5', '6', '7', '8', '9' 

#Resampling: Cross-Validated (3 fold) 
#Summary of sample sizes: 13332, 13330, 13332
#sigma     C  Accuracy   Kappa    
#6.30e-08  1  0.9429911  0.9366369
#6.30e-08  2  0.9508422  0.9453625
#6.30e-08  3  0.9541928  0.9490863
#1.63e-07  1  0.9619941  0.9577579
#1.63e-07  2  0.9682949  0.9647608
#1.63e-07  3  0.9689450  0.9654835
#2.63e-07  1  0.9689450  0.9654835
#2.63e-07  2  0.9724457  0.9693744
#2.63e-07  3  0.9732957  0.9703192

#Accuracy was used to select the optimal model using  the largest value.
#The final values used for the model were sigma = 2.63e-07 and C = 3.

plot(SVM_non_linear_RBF.fit)


# Lets build a model with C = 3 and sigma = 2.63e-07

SVM_non_linear_RBF_model_final <- ksvm(number~.,
                                       data=train_set,
                                       kernel="rbfdot",
                                       scale=FALSE,
                                       C=3,
                                       kpar=list(sigma=2.63e-7))
SVM_non_linear_RBF_model_final
# Hyper Parameters :     C = 3
#                  : sigma = 2.63e-7
# Support vectors  : 6318

# Lets check training accuracy #
eval_train_non_linear_RBF_model_final <- predict(SVM_non_linear_RBF_model_final,train_set)
confusionMatrix(eval_train_non_linear_RBF_model_final,train_set$number)
# Net Train Accuracy = 0.9989 #

# Lets check the test accuracy #
eval_test_non_linear_RBF_model_final <- predict(SVM_non_linear_RBF_model_final,test_set)
confusionMatrix(eval_test_non_linear_RBF_model_final,test_set$number)
# Net test Accuracy = 0.9777 #

# There is very less difference between train and test accuracy, hence we can say that over-fitting
# is not the case, our model is able to predict correct digits using Non-Linear SVM to a large extent.

# Final Conclusion #
# Non-linearity in the data set seems to be present to a very less extent as the value of hyper parameter
# sigma is of the order of 1e-7. However, it is also seen that having even this magnitude of non-lineariry
# is helping in increasing the performance (accuracy).
# Based on this input, after performing cross validation for rbfdot kernel, it has been seen that
# maximum accuracy can be seen with C=3 and sigma=2.63e-7, the train accuracy is very good.
# Also with these hyper parameters, the test accuracy is comparable with train accuracy, eliminating
# chances of over fitting as well. Also the specificity and sensitivity across different levels (1,2,3,etc.)
# is very good for both train and test.
#
# Looking
# Final Hyper Parameters :
# C=3
# sigma = 2.63e-7