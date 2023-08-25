
#raw dataset of the completed software projects
software_project_dataset_file <- "predicting_software_project_success_survey.csv"

#column labels for the software project dataset
software_project_col_names <- c('project_type', 
  'application_domain',
  'production_type', 'software_and_product', 'project_size',
  'development_staff_size', 'sufficient_development_staff',
  'overall_staff_size', 'sufficient_overall_staff', 
  'staff_experience', 'staff_qualified', 'project_manager',
  'experienced_project_manager', 'reuse_assets_introduced', 
  'reuse_asset_qualification', 'top_management_support',
  'domain_analysis', 'risks_identification', 'risk_management', 
  'project_management_methodologies', 'communication', 'repository',
  'time_line_monitoring', 'performance_monitoring',
  'budget_monitoring', 'risk_monitoring', 'change_monitoring', 
  'quality_monitoring', 'resource_monitoring', 
  'sufficient_budget_allocation', 'human_factors', 
  'completed_on_time', 'completed_on_budget',
  'requirement_satisfaction')

#pre-processed file name
pre_processed_dataset_file <- 'pre_processed_software_project_data.csv'