library(dplyr)
library(tidyr)
library(stringr)

library(randomForest)
library(caret)

#Check for missing values in each column in a dataset
get_na_count_all_cols <- function(data_set){
  return(colSums(is.na(data_set)))
}

#Detect string 'N/A' values in each column in a dataset
get_na_count_by_str_detect_all_cols <- function(data_set){
  for(i in 1:ncol(data_set)) {    
    print(colnames(data_set)[i])
    print(sum(str_detect(data_set[ , i] , 'N/A')))
  }
}

#get data type of each column 
get_col_count_by_type <- function(data_set, data_type){
  return(unlist(lapply(data_set, data_type)))
}

#Check if all values in the columns are of correct data type
column_invaild_count <- function(data_set, column, data_type){
  value_count <- !unlist(lapply(data_set[column], data_type))

  return(sum(value_count, na.rm = TRUE) ) # returning false values
}

#get number of invalid values in each column in the data set
get_invalid_value_count_all_cols <- function(data_set){
  
  numeric_cols <- get_col_count_by_type(data_set, is.numeric)
  character_cols <- get_col_count_by_type(data_set, is.character)
  
  for (col in  colnames(data_set) ) {
    
    if (numeric_cols[col]){
      type <- is.numeric
    }else if(character_cols[col]){
      type <- is.character
    }else{
      type <- 'date'
    }
    if(character_cols[col] || numeric_cols[col]){
      print(col)
      print( column_invaild_count(data_set, col, type))
    }
  }
}

#Hypothesis Testing - Returns the dependent factors withing the given threshold
get_significant_factors <- function(data_set, threshold){
  
  columns <- colnames(data_set) 
  dependent_list <- list()
  
  dependent_columns = c("critical_success_factor","p_value") 
  factor_significance = data.frame(matrix(nrow = 0, ncol = length(dependent_columns))) 
  
  for (col in columns) {
    
    chisq<- chisq.test(software_project_data[col], software_project_data$successful,
                       simulate.p.value = TRUE)
    
    print(col)
    print(chisq)
    
    if(chisq$p.value <= threshold){
      print('Reject H0 - Depenedent factor')
      dependent_list[[col]] <- software_project_data[[col]]
      
      factor_significance[nrow(factor_significance) + 1,] <- c(col, chisq$p.value)
      
    }else{
      print('Fails to reject H0 - Indepenedent factor')
    }
    
    print('--------------------------------------------------------------------')
  }
  
  
  colnames(factor_significance) <-dependent_columns
  factor_significance <- factor_significance[order(factor_significance$p_value, decreasing = TRUE), ]
  #Critical success factors in decreasing significance level
  print(factor_significance)
  y<- factor_significance$p_value
  x<- factor_significance$critical_success_factor

  
  par(mar = c(4, 14, 4, 0.5))   
  plot(factor_significance$p_value, 
       seq_along(factor_significance$critical_success_factor), 
       yaxt = "n", xlab='a',
       ylab = '')
  axis(2, las=2, 
       at = seq_along(factor_significance$critical_success_factor), 
       labels = factor_significance$critical_success_factor)
  
  #Return the dependent factors 
  return(data.frame(dependent_list))
  
}

#Train Logistic Regression Model
train_logistic_regression <- function(project_data_train, train_control){
  
  logistic_reg_model <- train(successful~.,
                              data = project_data_train,
                              method = "glm",
                              family=binomial(link = "logit"),
                              metric = "Accuracy",
                              trControl = train_control)
  return(logistic_reg_model)
}

#Calculate per-class precision, recall and f-measure 
get_performance_measures <- function(model, data){
  
  predicted_train <-predict(model, data)
  confusion_matrix <- as.matrix( table(Actual = data$successful, 
                                          Predicted = predicted_train))
  
  n = sum(confusion_matrix) #number of instances
  nc = nrow(confusion_matrix) #number of classes
  nccic = diag(confusion_matrix) #number of correctly classified instances per class (TP and TN)
  row_sums = apply(confusion_matrix, 1, sum) #number of instances per class (TP + FN)
  col_sums = apply(confusion_matrix, 2, sum) #number of predictions per class (TP + FP)
  
  #per-class precision, recall and f-measure
  precision = nccic / col_sums 
  recall = nccic / row_sums 
  f_measure = 2 * precision * recall / (precision + recall) 
  
  return (data.frame(precision, recall, f_measure)) 
  
}

#Calculate mean precision, recall and f-measure 
get_macro_performance_measures<- function(precision, recall, f_measure){
  
  macro_precision = mean(precision)
  macro_recall = mean(recall)
  macro_f_measure = mean(f_measure)
  
  return(data.frame(macro_precision, macro_recall, macro_f_measure))
  
}

#################################################
#Plot bar plot
plot_values <- function(values, cols, x_lab, y_lab, title) {
  barplot(values,
          main = title,
          xlab = x_lab,
          ylab = y_lab,
          names.arg = cols,
          col = "darkred",
          horiz = FALSE)
}

plot_missing_or_invalids_or_outliers <- function(data, dataset_name, option){
  
  columns <- colnames(data) 
  numeric_cols <- get_col_count_by_type(data, is.numeric)
  character_cols <- get_col_count_by_type(data, is.character)
  count = 1;
  
  if (option == 1) {
    na_count_list <- c()
    
    #Identifying the missing values for each column
    for (col in columns) {
      #creating a dynamic variable name
      col_na <- paste("col_na", count, sep = "_") 
      #assigning value to the variable created
      col_na_count <- assign(col_na, get_na_count(data,col))
      na_count_list <- c(na_count_list, col_na_count)  
      
      count <- count + 1
    } 
    #na_count_list
    plot_values(na_count_list, columns, "Columns", "NA count", paste(dataset_name, " missing values"))
    get_na_count_all_cols(data)
  }else if(option == 2){ # format checking for each 
    
    invalid_count_list <- c()
    num_char_cols <- c()
    
    for (col in columns) {
      
      #creating dynamic variable
      col_invalid <- paste("col_invalid", count, sep = "_") 
      
      if (numeric_cols[col]){
        type <- is.numeric
      }else if(character_cols[col]){
        type <- is.character
      }else{
        type <- 'date'
      }
      if(character_cols[col] || numeric_cols[col]){
        col_invalid_count <- assign(col_invalid, column_invaild_count(data, col, type))
        invalid_count_list <- c(invalid_count_list, col_invalid_count) 
        #to exclude the date column - getting only numeric and character type cols
        num_char_cols <- c(num_char_cols, col)
        
        count <- count + 1
      }
    }
    print("Numeric columns:")
    print(numeric_cols)
    print("Character columns:")
    print(character_cols)
    
    plot_values(invalid_count_list, num_char_cols, "Columns", "Invalid count",
                paste(dataset_name, " invalid values"))  #paste - to concatenate string values
    
  }else{ #check if values are in in correct range
    outlier_count_list <- c()
    num_cols <- c()
    for (col in columns) {
      #check if column is numeric
      if (numeric_cols[col]){
        #creating a dynamic variable
        col_outlier <- paste("col_outlier", count, sep = "_") 
        col_outliers_count <- assign(col_outlier, get_outliers(data, col))
        outlier_count_list <- c(outlier_count_list, col_outliers_count)  
        #getting only the numeric columns
        num_cols <- c(num_cols, col)
        count <- count + 1
      }
    }
    plot_values(outlier_count_list, num_cols, "Columns", "Outlier count",
                paste(dataset_name, " outliers"))
    
  }
}

get_frequency <- function(data, column) {
  frequency <- data %>% 
               select(column) %>% 
               count(column)
  return(frequency)
}

plot_bar_chart <- function(data, column, n, x_lab, y_lab){
  bar_plot <- ggplot(data,aes(x=factor(column),y=n))+
  geom_col(color='black',fill='cyan3')+
  xlab(x_lab)+
  ylab(y_lab)
  
  return(bar_plot)
}

get_percentage <- function(column){
  return(round(100*column/sum(column)))
}

plot_pie_chart <- function(n, column, percentage, title){
  pie(n,
      labels = paste(column, sep = " ", percentage, "%"), 
      col = rainbow(length(column)), 
      main = title)
}

get_duplicates <- function(data, columns, col_start, col_end) {
  subset <- data %>% select(all_of(columns))
  duplicates <- duplicated(subset[,col_start:col_end])
  #counting the duplicates
  return(sum(duplicates, na.rm = TRUE))
}


#Check for missing values in a column
# get_na_count <- function(data_set, column){
#   return(sum(is.na(data_set[column]),  na.rm = TRUE))
# }
