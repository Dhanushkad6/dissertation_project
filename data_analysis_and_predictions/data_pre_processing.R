#importing pcakages required for data pr-processing
library(readr)
library(dplyr)
library(tidyr)
library(stringr)

#importing the file that has all required function implementations and file that has all configurations
source("all_functions.R")
source("configurations.R")

#extracting the dataset
software_projects_raw_data <- read.csv(file = software_project_dataset_file)
nrow(software_projects_raw_data)

#removing the unnecessary columns (gender,age, highest level of education,
#job category, primary employee status, total years of work experience, submit button )
software_project_data <- software_projects_raw_data[ -c(1:6,41) ]

#renaming column names
colnames(software_project_data) <- software_project_col_names
head(software_project_data)

#checking for missing value count for each column
get_na_count_all_cols(software_project_data)
get_na_count_by_str_detect_all_cols(software_project_data)
#experienced_project_manager - has 6 N/A values

#replacing N/A's in column experienced_project_manager to value = No
software_project_data$experienced_project_manager[software_project_data$experienced_project_manager
                                                  == "N/A (If no Project Manager was involved)"] <- 'No'
#checking for duplicate projects
software_project_data %>%
  add_count(project_type, 
            application_domain, production_type, software_and_product, project_size,
            development_staff_size, sufficient_development_staff, overall_staff_size, sufficient_overall_staff, 
            staff_experience, staff_qualified, project_manager,experienced_project_manager, reuse_assets_introduced, 
            reuse_asset_qualification, top_management_support, domain_analysis, risks_identification, risk_management, 
            project_management_methodologies, communication, repository,
            time_line_monitoring, performance_monitoring, budget_monitoring, risk_monitoring, change_monitoring, 
            quality_monitoring, resource_monitoring, sufficient_budget_allocation, human_factors, 
            completed_on_time, completed_on_budget, requirement_satisfaction) %>%
  filter(n>1) %>%
  distinct()
#No duplicates

#removing non-critical factors (project_type, application_domain, 
#production_type, software_and_product, project_size)
software_project_data <- software_project_data[ -c(1,1:4) ]

#checking for columns with invalid values
get_invalid_value_count_all_cols(software_project_data)
#no invalid columns

#adding a new column for project success and assigning the default value to 'Yes'
software_project_data['successful'] <- 'Yes'

#combining columns completed_on_time, completed_on_budget and requirement_satisfaction into one column (successful)
for (row in 1:nrow(software_project_data)) {
  if((software_project_data[row,28] == 'Yes') && (software_project_data[row,29] == 'Yes')  
     && (software_project_data[row,30] == 'Yes'))
    software_project_data[row,31] <- 'Yes'
  else
    software_project_data[row,31] <- 'No'
}

#removing columns completed_on_time, completed_on_budget and requirement_satisfaction
software_project_data <- software_project_data[ -c(0,28:30) ]

#exporting the cleansed and transformed file 
write.csv(software_project_data, pre_processed_dataset_file, row.names=FALSE)
