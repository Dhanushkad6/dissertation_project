#importing packages for data analysis, classification and performance measures
library(randomForest)
library(caret)
library(tidyverse)
library(ROCR)

#importing the file that has all required function implementations
source("all_functions.R")

#importing the initially pre-processed dataset
software_project_data <- read.csv("pre_processed_software_project_data.csv")

#getting a summary of the dataset
str(software_project_data)
#converting all columns to factors
software_project_data <- as.data.frame(unclass(software_project_data),
                                       stringsAsFactors=TRUE)
str(software_project_data)

#conducting Hypothesis Testing using Pearson's Chi-squared test and filtering 
#critical success factors from p-value <= 0.05
project_cf_data <- get_significant_factors(software_project_data, 0.05)

#conducting Hypothesis Testing using Pearson's Chi-squared test and filtering 
#critical success factors from p-value <= 0.01
project_cf_data_most_significant <- get_significant_factors(software_project_data, 0.01)

#splitting the dataset to train (90%) and test (10%) sets
ind <- sample(2, nrow(project_cf_data), replace = TRUE, prob = c(0.9, 0.1))

project_data_train <- project_cf_data[ind==1,]
project_data_test <- project_cf_data[ind==2,]

#using 10 fold cross-validation to train
train_control <- trainControl(method = "cv",
                          number = 10,
                          search = "grid")

#building the Random forest model with default parameter values (by default ntree=500)
default_rf <- train(successful~., data = project_data_train, method = "rf", metric = "Accuracy",
                    trControl = train_control)
default_rf

#assigning the best 'mtry' value to tune using grid search method
best_mtry <- default_rf$bestTune$mtry
tune_grid <- expand.grid(.mtry=best_mtry)

#building the Random forest model with best 'mtry'
mtry_tuned_rf <- train(successful~.,
                    data = project_data_train,
                    method = "rf",
                    metric = "Accuracy",
                    trControl = train_control,
                    tuneGrid =tune_grid )
mtry_tuned_rf 

#calculating per-class Precision, Recall and F1 measure for Random Forest
rf_per_class_performance_measures <- get_performance_measures(mtry_tuned_rf, project_data_test)
rf_per_class_performance_measures

#getting the mean Precision, Recall and f-measure
rf_macro_performance_measures <- get_macro_performance_measures(
  rf_per_class_performance_measures$precision,
  rf_per_class_performance_measures$recall,
  rf_per_class_performance_measures$f_measure)
rf_macro_performance_measures

#tuning 'ntree' for best 'mtry' in random forest
models <- list()
for (ntree in c(10, 20, 30, 40, 50 ,60, 100, 200, 500, 1000, 1500, 2000, 2500)) {
  fit_model <- train(successful~., data=project_data_train, 
               method="rf", metric = "Accuracy", tuneGrid=tune_grid, 
               trControl=train_control, ntree=ntree)
  key <- toString(ntree)
  models[[key]] <- fit_model
}

model_results <- resamples(models) 
summary(model_results) 


#building the random forest model using best 'ntree' identified for best 'mtry'
mtry_ntree_tuned_rf <- train(successful~.,
                  project_data_train,
                  method = "rf",
                  metric = "Accuracy",
                  tuneGrid = tune_grid,
                  trControl = train_control,
                  importance = TRUE,
                  ntree = 500)
mtry_ntree_tuned_rf


#finding the best combination of 'mtry' and 'ntree'
custom_rf <- list(type = "Classification", library = "randomForest", loop = NULL)
custom_rf$parameters <- data.frame(parameter = c("mtry", "ntree"), 
                                  class = rep("numeric", 2), label = c("mtry", "ntree"))
custom_rf$grid <- function(x, y, len = NULL, search = "grid") {}
custom_rf$fit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) {
  randomForest(x, y, mtry = param$mtry, ntree=param$ntree, ...)
}
custom_rf$predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata)
custom_rf$prob <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata, type = "prob")
custom_rf$sort <- function(x) x[order(x[,1]),]
custom_rf$levels <- function(x) x$classes

#training the model
tune_grid_new <- expand.grid(.mtry=c(1:30), .ntree=c(10, 20, 30, 40, 50, 60, 100, 
                                                200, 500, 1000, 1500, 2000, 2500))

custom_fit <- train(successful~., data=project_data_train, 
                method=custom_rf, metric="Accuracy", tuneGrid=tune_grid_new, 
                trControl=train_control)
summary(custom_fit)
plot(custom_fit)
custom_fit

#building the model using best combination of 'mtry' and 'ntree'
tune_grid <- expand.grid(.mtry=custom_fit$bestTune$mtry)
custom_rf_fit <- train(successful~.,
                  project_data_train,
                  method = "rf",
                  metric = "Accuracy",
                  tuneGrid = tune_grid,
                  trControl = train_control,
                  importance = TRUE,
                  ntree = custom_fit$bestTune$ntree)
custom_rf_fit

#calculating per-class Precision, Recall and F-measure for Random Forest custom fit model
custom_rf_per_class_performance_measures <- get_performance_measures(custom_rf_fit, project_data_test)
custom_rf_per_class_performance_measures

#calculating mean Precision, Recall and F-measure for Random Forest custom fit model
custom_rf_macro_performance_measures <- get_macro_performance_measures(
  custom_rf_per_class_performance_measures$precision,
  custom_rf_per_class_performance_measures$recall,
  custom_rf_per_class_performance_measures$f_measure)
custom_rf_macro_performance_measures
                  
#identifying important critical success factors using Random Forest
rf <-randomForest(successful~.,
                  data=project_data_train, 
                  mtry=best_mtry,
                  importance=TRUE)
rf
#getting the important variables
imp <- data.frame(importance(rf))
#plotting the important variables
varImpPlot(rf)

#predicting results using Random Forest
predicted <-predict(mtry_tuned_rf, project_data_test)
#confusion matrix with TP, TN, FP, FN
cf <- confusionMatrix(as.factor(predicted), as.factor(project_data_test$successful))
predicted
cf












#################################################################





#LOGISTIC REGRESSION





##################################################################

#building the Logistic Regression model with only the related factors of p-value 0.05 threshold
logistic_reg_model <- train_logistic_regression(project_data_train, train_control)
logistic_reg_model

#calculating per-class Precision, Recall and F-measure for Logistic Regression
lr_per_class_performance_measures <- get_performance_measures(logistic_reg_model, project_data_test)
lr_per_class_performance_measures

#calculating mean Precision, Recall and F-measure
lr_macro_performance_measures <- get_macro_performance_measures(
  lr_per_class_performance_measures$precision,
  lr_per_class_performance_measures$recall,
  lr_per_class_performance_measures$f_measure)
lr_macro_performance_measures

#predicting results using Logistic Regression
predicted_lr <-predict(logistic_reg_model, project_data_test)
#confusion matrix with TP,TN,FP,FN
cf_lr <- confusionMatrix(as.factor(predicted_lr), as.factor(project_data_test$successful))
predicted_lr
cf_lr


#chi-squared test filtering critical success factors from p-value <= 0.1
project_cf_data <- get_significant_factors(software_project_data, 0.1)

#splitting the dataset to train (90%) and test (10%) sets
ind <- sample(2, nrow(project_cf_data), replace = TRUE, prob = c(0.9, 0.1))
project_data_train <- project_cf_data[ind==1,]
project_data_test <- project_cf_data[ind==2,]

#logistic regression model with only the related factors with p-value 0.1 threshold
logistic_reg_model <- train_logistic_regression(project_data_train, train_control)
logistic_reg_model

#calculating per-class Precision, recall and F1 measure for Logistic regression
lr_per_class_performance_measures <- get_performance_measures(logistic_reg_model, project_data_test)
lr_per_class_performance_measures

#calculating mean Precision, Recall and F-measure
lr_macro_performance_measures <- get_macro_performance_measures(
  lr_per_class_performance_measures$precision,
  lr_per_class_performance_measures$recall,
  lr_per_class_performance_measures$f_measure)
lr_macro_performance_measures

#chi-squared test filtering critical success factors from p-value <= 0.01
project_cf_data <- get_significant_factors(software_project_data, 0.01)

#splitting the dataset to train (90%) and test (10%) sets
ind <- sample(2, nrow(project_cf_data), replace = TRUE, prob = c(0.9, 0.1))
project_data_train <- project_cf_data[ind==1,]
project_data_test <- project_cf_data[ind==2,]

#logistic regression model with only the related factors with p-value 0.01 threshold
logistic_reg_model <- train_logistic_regression(project_data_train, train_control)
logistic_reg_model

#calculating per-class Precision, Recall and F-measure for Logistic regression
lr_per_class_performance_measures <- get_performance_measures(logistic_reg_model, project_data_test)
lr_per_class_performance_measures

#calculating mean Precision, Recall and F-measure
lr_macro_performance_measures <- get_macro_performance_measures(
  lr_per_class_performance_measures$precision,
  lr_per_class_performance_measures$recall,
  lr_per_class_performance_measures$f_measure)
lr_macro_performance_measures
