library(randomForest)
library(caret)
library(tidyverse)
library(ROCR)

source("all_functions.R")

#importing the initially pre-processed dataset
software_project_data <- read.csv("pre_processed_software_project_data.csv")

#converting all columns to factors
str(software_project_data)
software_project_data <- as.data.frame(unclass(software_project_data),
                                       stringsAsFactors=TRUE)
str(software_project_data)

#conducting Hypothesis Testing using Pearson's Chi-squared test filtering 
#and critical success factors from p-value <= 0.05
project_cf_data <- get_significant_factors(software_project_data, 0.05)

#conducting Hypothesis Testing using Pearson's Chi-squared test filtering 
#and critical success factors from p-value <= 0.01
project_cf_data_most_significant <- get_significant_factors(software_project_data, 0.01)

#Splitting the dataset to train (90%) and test (10%)
ind <- sample(2, nrow(project_cf_data), replace = TRUE, prob = c(0.9, 0.1))

project_data_train <- project_cf_data[ind==1,]
project_data_test <- project_cf_data[ind==2,]

#creating reproducible results
set.seed(1234)

#using 10 fold cross-validation
train_control <- trainControl(method = "cv",
                          number = 10,
                          search = "grid")

#building the Random forest model with default parameter values (by default ntree=500)
default_rf <- train(successful~., data = project_data_train, method = "rf", metric = "Accuracy",
                    trControl = train_control)
default_rf

#assigning the best mtry value to tune using grid search
best_mtry <- default_rf$bestTune$mtry
tune_grid <- expand.grid(.mtry=best_mtry)

set.seed(1234)
#building the Random forest model with bestmtry
mtry_tuned_rf <- train(successful~.,
                    data = project_data_train,
                    method = "rf",
                    metric = "Accuracy",
                    trControl = train_control,
                    tuneGrid =tune_grid )
mtry_tuned_rf 

#Tunning ntrees for bestmtry in random forest
models <- list()
for (ntree in c(10, 20, 30, 40, 50 ,60, 100, 200, 500, 1000, 1500, 2000, 2500)) {
  set.seed(1234)
  fit_model <- train(successful~., data=project_data_train, 
               method="rf", metric = "Accuracy", tuneGrid=tune_grid, 
               trControl=train_control, ntree=ntree)
  key <- toString(ntree)
  models[[key]] <- fit_model
}

model_results <- resamples(models) 
summary(model_results) 

set.seed(1234)
#building the random forest model using bestntree identified for bestmtry
mtry_ntree_tuned_rf <- train(successful~.,
                  project_data_train,
                  method = "rf",
                  metric = "Accuracy",
                  tuneGrid = tune_grid,
                  trControl = train_control,
                  importance = TRUE,
                  ntree = 500)
mtry_ntree_tuned_rf


#finding the best combination of mtry and ntree
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

# training the model
tune_grid_new <- expand.grid(.mtry=c(1:30), .ntree=c(10, 20, 30, 40, 50, 60, 100, 
                                                200, 500, 1000, 1500, 2000, 2500))
set.seed(1234)
custom_fit <- train(successful~., data=project_data_train, 
                method=custom_rf, metric="Accuracy", tuneGrid=tune_grid_new, 
                trControl=train_control)
summary(custom_fit)
plot(custom_fit)
custom_fit

#building the model using best combination of mtry and ntree
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

#Calculating per-class Precision, recall and F1 measure for Random Forest custom fit model
custom_rf_per_class_performance_measures <- get_performance_measures(custom_rf_fit, project_data_train)
custom_rf_per_class_performance_measures

#mean precision, recall and f-measure for Random Forest custom fit model
custom_rf_macro_performance_measures <- get_macro_performance_measures(
  custom_rf_per_class_performance_measures$precision,
  custom_rf_per_class_performance_measures$recall,
  custom_rf_per_class_performance_measures$f_measure)
custom_rf_macro_performance_measures
                  
#Identifying important critical success factors
rf <-randomForest(successful~.,
                  data=project_data_train, 
                  mtry=best_mtry,
                  importance=TRUE)
rf
#getting the important variables
imp <- data.frame(importance(rf))
#plotting the important variables
varImpPlot(rf)

#Predicting results using Random Forest
predicted <-predict(mtry_tuned_rf, project_data_test)
#confusion metrix with TP,TN,FP,FN
cf <- confusionMatrix(as.factor(predicted), as.factor(project_data_test$successful))
predicted
cf

#Calculating per-class Precision, recall and F1 measure for Random Forest
rf_per_class_performance_measures <- get_performance_measures(mtry_tuned_rf, project_data_train)
rf_per_class_performance_measures

#mean precision, recall and f-measure
rf_macro_performance_measures <- get_macro_performance_measures(
  rf_per_class_performance_measures$precision,
  rf_per_class_performance_measures$recall,
  rf_per_class_performance_measures$f_measure)
rf_macro_performance_measures

#################################################################





#LOGISTIC REGRESSION





##################################################################

#With only the related factors with chi-squared test 0.05 threshold
logistic_reg_model <- train_logistic_regression(project_data_train, train_control)
logistic_reg_model

#per-class Precision, recall and F1 measure for Logistic regression
lr_per_class_performance_measures <- get_performance_measures(logistic_reg_model, project_data_train)
lr_per_class_performance_measures

#mean precision, recall and f-measure
lr_macro_performance_measures <- get_macro_performance_measures(
  lr_per_class_performance_measures$precision,
  lr_per_class_performance_measures$recall,
  lr_per_class_performance_measures$f_measure)
lr_macro_performance_measures

#Chi-squared test filtering crirical success factors from p-value <= 0.1
project_cf_data <- get_significant_factors(software_project_data, 0.1)

#Splitting the dataset to train (90%) and test (10%)
ind <- sample(2, nrow(project_cf_data), replace = TRUE, prob = c(0.9, 0.1))
project_data_train <- project_cf_data[ind==1,]
project_data_test <- project_cf_data[ind==2,]

#logistic regression model with only the related factors with chi-squared test 0.1 threshold
logistic_reg_model <- train_logistic_regression(project_data_train, train_control)
logistic_reg_model

#per-class Precision, recall and F1 measure for Logistic regression
lr_per_class_performance_measures <- get_performance_measures(logistic_reg_model, project_data_train)
lr_per_class_performance_measures

#mean precision, recall and f-measure
lr_macro_performance_measures <- get_macro_performance_measures(
  lr_per_class_performance_measures$precision,
  lr_per_class_performance_measures$recall,
  lr_per_class_performance_measures$f_measure)
lr_macro_performance_measures

#Chi-squared test filtering crirical success factors from p-value <= 0.01
project_cf_data <- get_significant_factors(software_project_data, 0.01)

#Splitting the dataset to train (90%) and test (10%)
ind <- sample(2, nrow(project_cf_data), replace = TRUE, prob = c(0.9, 0.1))
project_data_train <- project_cf_data[ind==1,]
project_data_test <- project_cf_data[ind==2,]

#logistic regression model with only the related factors with chi-squared test 0.01 threshold
logistic_reg_model <- train_logistic_regression(project_data_train, train_control)
logistic_reg_model

#per-class Precision, recall and F1 measure for Logistic regression
lr_per_class_performance_measures <- get_performance_measures(logistic_reg_model, project_data_train)
lr_per_class_performance_measures

#mean precision, recall and f-measure
lr_macro_performance_measures <- get_macro_performance_measures(
  lr_per_class_performance_measures$precision,
  lr_per_class_performance_measures$recall,
  lr_per_class_performance_measures$f_measure)
lr_macro_performance_measures


#https://stackoverflow.com/questions/46776944/how-to-calculate-randomforest-training-auc-in-r
#https://stackoverflow.com/questions/8499361/easy-way-of-counting-precision-recall-and-f1-score-in-r
# rf_predict_train <- predict(mtry_tuned_rf, type="prob")[,2]
# #make predictions for each instanc in train set
# rf_predicted_train <- prediction(rf_predict_train, project_data_train$successful)
# #calculate AUC
# rf_auc_train <- performance(rf_predicted_train, measure = "auc")@y.values[[1]] 
# rf_auc_train #0.8786765
# 
# #gettingthe optimum cut-off
# opt.cut = function(perf, pred){
#   cut.ind = mapply(FUN=function(x, y, p){
#     d = (x - 0)^2 + (y-1)^2
#     ind = which(d == min(d))
#     c(sensitivity = y[[ind]], specificity = 1-x[[ind]], 
#       cutoff = p[[ind]])
#   }, perf@x.values, perf@y.values, pred@cutoffs)
# }
# 
# #random forest precision
# rf_precision <- performance(rf_predicted_train, "prec");
# plot (rf_precision);
# 
# rf_recall <- performance(rf_predicted_train, "rec");
# plot (rf_recall);
# 
# print(opt.cut(rf_recall, rf_predicted_train))
# 
# 
# rf_tpr <- performance(rf_predicted_train, "tpr");
# plot (rf_tpr);
# 
# print(opt.cut(rf_tpr, rf_predicted_train))
# 
# rf_fpr <- performance(rf_predicted_train, "fpr");
# plot (rf_fpr);
# 
# rf_f1 <- performance(rf_predicted_train, "f");
# plot (rf_f1);
# 
# 
# lr_predict_train <- predict(logistic_reg__model, type="prob")[,2]
# #make predictions for each instanc in train set
# lr_predicted_train <- prediction(lr_predict_train, project_data_train$successful)
# #calculate AUC
# lr_auc_train <- performance(lr_predicted_train, measure = "auc")@y.values[[1]] 
# lr_auc_train # 0.9301471
# 
# lr_precision <- performance(lr_predicted_train, "prec");
# plot (lr_precision);
# 
# lr_recall <- performance(lr_predicted_train, "rec");
# plot (lr_recall);
# 
# lr_tpr <- performance(lr_predicted_train, "tpr");
# plot (lr_tpr);
# 
# lr_fpr <- performance(lr_predicted_train, "fpr");
# plot (lr_fpr);
# 
# #f1-score
# lr_f1 <- performance(lr_predicted_train, "f");
# plot (lr_f1);



---------------------------------------------------------

#Accuracy
#mean(predicted.classes == project_data_test$successful)

#leanear regression with loocv
#http://www.sthda.com/english/articles/38-regression-model-validation/157-cross-validation-essentials-in-r/#leave-one-out-cross-validation---loocv


------------------------------------------------------------
  # library(readr)
  # library(dplyr)
  # library(tidyr)
  # library(stringr)
  #library(e1071)

# Installing the package 
# For Logistic regression 
#install.packages("caTools")    
#library(caTools)

# For generating random forest model
#install.packages('randomForest') 

# classification and regression training : The library caret has a function to make prediction.
#install.packages('caret')                   
#install.packages('e1071', dependencies=TRUE)


#Byasin optimizatio n- hyper parameter optimization


# install.packages("devtools") - use below to install
#devtools::install_github("ymattu/MlBayesOpt") 

#library(MlBayesOpt)

#project_data_copy <- project_data

#project_data_c <- project_data[ -c(1,32:34) ]
#head(project_data_c)

# a<-predict(mtry_tuned_rf, project_data_train, type="prob")[,2]
# pred <- prediction(a, project_data_train$successful)
# pred
# pred$tp/max(pred$tp)

p = rf_row_sums / rf_n #distribution of instances over the actual classes
q = rf_col_sums / rf_n #distribution of instances over the predicted classes





plot_values <- function(values, cols, x_lab, y_lab, title) {
  barplot(values,
          main = title,
          xlab = x_lab,
          ylab = y_lab,
          names.arg = cols,
          col = "darkred",
          space=c(0,2),
          legend.text=TRUE,
          beside=TRUE,
          horiz=TRUE,
          axes=TRUE,
          cex.names=0.5, las=1 )
}
plot_values(imp$MeanDecreaseAccuracy, row.names(imp), 'a','b','t')

x <- barplot(imp$MeanDecreaseAccuracy, 
             beside=TRUE, 
             space=0.1, 
             ylim = c(-2, 3),
             xlab = 'Critical Success Factors',
             ylab = 'Mean Decrease Accuracy',
             main='Success Factors by its importance',
             reorder(row.names(imp)))
labs <- paste(row.names(imp))
text(cex=0.5, x=x-.25, y=-2, labs, xpd=TRUE, srt=90)

mylist <- list(a= prediction, b = imp)


#Higher the value of mean decrease accuracy or mean decrease gini score , 
#higher the importance of the variable in the model. In the plot shown above- most important
#is resource monitoring



cf$overall['Accuracy']

lr_predict_train <- predict(mtry_tuned_rf, type="prob")[,2]
#100% accuracy achieved
