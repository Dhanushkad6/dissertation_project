#importing packages for data analysis, classification and performance measures
library(dplyr)
library(tidyr)
library(stringr)

library(randomForest)
library(caret)

#check for missing values in each column in a dataset
get_na_count_all_cols <- function(data_set){
  return(colSums(is.na(data_set)))
}

#detect string 'N/A' values in each column in a dataset
get_na_count_by_str_detect_all_cols <- function(data_set){
  for(i in 1:ncol(data_set)) {    
    print(colnames(data_set)[i])
    print(sum(str_detect(data_set[ , i] , 'N/A')))
  }
}

#check data type of each column is of the correct type
check_col_data_type <- function(data_set, data_type){
  return(unlist(lapply(data_set, data_type)))
}

#get count of all invalid values (incorrect data type)  in each column of a dataset
column_invaild_count <- function(data_set, column, data_type){
  value_count <- !unlist(lapply(data_set[column], data_type))

  return(sum(value_count, na.rm = TRUE) ) #returning false values
}

#get number of invalid values (incorrect data type) in each column in a dataset
get_invalid_value_count_all_cols <- function(data_set){
  
  #check data type of each column
  numeric_cols <- check_col_data_type(data_set, is.numeric)
  character_cols <- check_col_data_type(data_set, is.character)
  
  for (col in  colnames(data_set) ) {
    
    #identify the correct data type of a column
    if (numeric_cols[col]){
      type <- is.numeric
    }else if(character_cols[col]){
      type <- is.character
    }else{
      type <- 'date'
    }
    #get invalid value count for a column
    if(character_cols[col] || numeric_cols[col]){
      print(col)
      print( column_invaild_count(data_set, col, type))
    }
  }
}

#Hypothesis Testing - Returns the dependent factors withing the given threshold 
#and plots the significance of success factors 
get_significant_factors <- function(data_set, threshold){
  
  columns <- colnames(data_set) 
  #create a new list for dependent factors
  dependent_list <- list()
  
  #create an empty data frame to add dependent factors and their p-values
  dependent_columns = c("critical_success_factor","p_value") 
  factor_significance = data.frame(matrix(nrow = 0, ncol = length(dependent_columns))) 
  
  for (col in columns) {
    
    #conduct Pearson's Chi-Squared test
    chisq<- chisq.test(software_project_data[col], software_project_data$successful,
                       simulate.p.value = TRUE)
    
    print(col)
    print(chisq)
    
    #check if the success factor is significant or not under/on the give threshold
    if(chisq$p.value <= threshold){
      print('Reject H0 - Depenedent factor')
      
      #if significant add the factor to the dependent factors list
      dependent_list[[col]] <- software_project_data[[col]]
      
      #eliminate adding the success(factor column) to the dependent factors data frame
      if(col != 'successful')
        factor_significance[nrow(factor_significance) + 1,] <- c(col, chisq$p.value)
      
    }else{
      print('Fails to reject H0 - Indepenedent factor')
    }
    
    print('--------------------------------------------------------------------')
  }
  
  
  colnames(factor_significance) <-dependent_columns
  #arrange critical success factors in the decreasing order of p-value
  factor_significance <- factor_significance[order(factor_significance$p_value, decreasing = TRUE), ]
  
  #initialize x and y labels for the plot
  y<- factor_significance$p_value
  x<- factor_significance$critical_success_factor

  #set margins in the plot
  par(mar = c(4, 14, 4, 0.5))   
  #plot the graph
  plot(factor_significance$p_value, 
       seq_along(factor_significance$critical_success_factor), 
       yaxt = "n", xlab='P-Value',
       ylab = '')
  axis(2, las=2, 
       at = seq_along(factor_significance$critical_success_factor), 
       labels = factor_significance$critical_success_factor)
  
  #returns the dependent factors 
  return(data.frame(dependent_list))
  
}

#train Logistic Regression Model
train_logistic_regression <- function(project_data_train, train_control){
  
  logistic_reg_model <- train(successful~.,
                              data = project_data_train,
                              method = "glm",
                              family=binomial(link = "logit"),
                              metric = "Accuracy",
                              trControl = train_control)
  return(logistic_reg_model)
}

#calculate per-class Precision, Recall and F-measure 
get_performance_measures <- function(model, data){
  
  predicted_result <-predict(model, data)
  confusion_matrix <- as.matrix( table(Actual = data$successful, 
                                          Predicted = predicted_result))
  
  n = sum(confusion_matrix) #number of instances
  nc = nrow(confusion_matrix) #number of classes
  nccic = diag(confusion_matrix) #number of correctly classified instances per class (TP and TN)
  row_sums = apply(confusion_matrix, 1, sum) #number of instances per class (TP + FN)
  col_sums = apply(confusion_matrix, 2, sum) #number of predictions per class (TP + FP)
  
  #per-class Precision, Recall and F-measure
  precision = nccic / col_sums 
  recall = nccic / row_sums 
  f_measure = 2 * precision * recall / (precision + recall) 
  
  return (data.frame(precision, recall, f_measure)) 
  
}

#calculate mean (macro) Precision, Recall and F-measure 
get_macro_performance_measures<- function(precision, recall, f_measure){
  
  macro_precision = mean(precision)
  macro_recall = mean(recall)
  macro_f_measure = mean(f_measure)
  
  return(data.frame(macro_precision, macro_recall, macro_f_measure))
  
}

