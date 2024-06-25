library(tidyverse)
library(dplyr)
library(lavaan)
library(polycor)
library(xtable)
library(semTable)
library(lavaanPlot)
library(DiagrammeRsvg)
library(rsvg)
library(semTable)
library(psych)
library(semTools)

#data <- read_csv("/Users/de51/Library/CloudStorage/OneDrive-UniversityofSussex/Funding proposals/Wins/Success in PM/rawData/Dynamic Conditions for Project Success_April 22, 2022_19.52.csv")
data <- read.csv("C:\\Users\\David Eggleton\\OneDrive - University of Sussex\\Funding proposals\\Wins\\Success in PM\\rawData\\Dynamic Conditions for Project Success_April 22, 2022_19.52.csv")
temporaryDataIndustry = data %>% 
  dplyr::select(ResponseId, Q1, Q17) %>% #Just selecting data related to 'what is project success question
  filter(!is.na(Q1)) %>% #Removing all unanswered obs
  filter(!is.na(Q17)) %>% 
  slice(-1) %>% #removing first two rows which are ireelevant
  slice(-1) %>%   #removing first two rows which are ireelevant  %>% 
  tidyr::separate_rows(Q1, sep = ",") %>% #separating out the data contained in each observation 
  filter(Q1 != "") %>% #Removing any empty values
  filter(Q17 != "") %>%
  mutate(value = 1) %>% #converting values into dummies
  tidyr::pivot_wider(    
    names_from = Q1,
    values_from = value,
    values_fill = 0 #sorting everything out
  ) %>%
  rename(
    "CostPerformance" = 3,
    "SafetyPerformance" = 4,
    "TimePerformance" = 5,
    "ScopePerformance" = 6,
    "otherToBeDeleted" = 7,
    "UserSatisfaction" = 8,
    "StakeholderSatisfaction" = 9,
    "SupplierSatisfaction" = 10,
    "TeamSatisfaction" = 11,
    "PublicSatisfaction" = 12,
    "otherToBeDeleted2" = 13,
    "UserBenefits" = 14,
    "ShortTermOrganizationalBenefits" = 15,
    "LongTermOrganizationalBenefits" = 16,
    "EmployeeRetention" = 17,
    "SocietalBenefits" = 18,
    "otherToBeDeleted3" = 19,
    "industry" = 2
  )
cleanDataIndustry <- temporaryDataIndustry[,c("industry",
                                            "CostPerformance",
                                            "TimePerformance",
                                            "ScopePerformance",
                                            "SafetyPerformance",
                                            "UserSatisfaction",
                                            "StakeholderSatisfaction",
                                            "SupplierSatisfaction",
                                            "TeamSatisfaction",
                                            "PublicSatisfaction",
                                            "UserBenefits",
                                            "ShortTermOrganizationalBenefits",
                                            "LongTermOrganizationalBenefits",
                                            "EmployeeRetention",
                                            "SocietalBenefits")]#moving data into a cleanDataset using a more logical listing format  

dfTemp1 = cleanDataIndustry
df_industrytemp1 <- subset(dfTemp1, select = -c(industry))
df.2 <- as.data.frame(lapply(df_industrytemp1, factor))
het.mat <- hetcor(df.2)$cor #creating a correlation matrix - everything looks fairly ok
KMO(df_industrytemp1) #produces value of 0.83 which is regarded as 'meritorous'
print(het.mat)
bartlett.test(df_industrytemp1) #statistically significant  with chi(square) of 1268.2 and p value much much less than 0.01
det(het.mat) 

industry_groups <- unique(cleanDataIndustry$`industry`)
list_of_dataframes <- lapply(industry_groups, function(group) {
  your_data_subset <- cleanDataIndustry[cleanDataIndustry$`industry` == group, ]
  return(your_data_subset)
})

# Assign each dataframe to a variable
for (i in seq_along(list_of_dataframes)) {
  assign(paste0("df_industry", industry_groups[i]), list_of_dataframes[[i]])
}

# Assuming cleanData is a list of dataframes
list_of_dataframes <- list(df_industry9 = df_industry9, 
                           df_industry8 = df_industry8,
                           df_industry7 = df_industry7, 
                           df_industry6 = df_industry6,
                           df_industry5 = df_industry5,
                           df_industry4 = df_industry4,
                           df_industry3 = df_industry3,
                           df_industry21 = df_industry21,
                           df_industry20 = df_industry20,
                           df_industry2 = df_industry2,
                           df_industry19 = df_industry19,
                           df_industry18 = df_industry18,
                           df_industry17 = df_industry17,
                           df_industry16 = df_industry16,
                           df_industry15 = df_industry15,
                           df_industry14 = df_industry14,
                           df_industry13 = df_industry13,
                           df_industry12 = df_industry12,
                           df_industry11 = df_industry11,
                           df_industry10 = df_industry10)  # List of your dataframes

for (df_industryname in names(list_of_dataframes)) {
  # Get the data frame
  df_industrytemp <- get(df_industryname)
  
  # Remove the column "industry"
  df_industrytemp <- subset(df_industrytemp, select = -c(industry))
  
  # Assign the modified data frame back to its original name
  assign(df_industryname, df_industrytemp)
}


# Define a function to perform the analysis and save results
run_analysis <- function(df) {
  # Convert non-numeric columns to factors
  df.3 <- as.data.frame(lapply(df, factor))
  het.mat <- hetcor(df.3)$cor #creating a correlation matrix - everything looks fairly ok
  print(het.mat)
  print("KMO test result: ")
  print(KMO(df))
  cat("\n") #produces value of 0.83 which is regarded as 'meritorous'
  print("Bartlet test result: ")
  print(bartlett.test(df))
  cat("\n") #statistically significant  with chi(square) of 1268.2 and p value much much less than 0.01
  print("Determinant of matrix: ")
  print(det(het.mat))
  cat("\n")
  #Fit models
  ironTriangle.model <- 'Iron Triangle =~ TimePerformance + CostPerformance + ScopePerformance'
  multidimensional.model <- 'Project Efficiency =~ TimePerformance + CostPerformance
                            Impact on the Customer =~ ScopePerformance + UserSatisfaction + UserBenefits 
                            Business Success =~ ShortTermOrganizationalBenefits
                            Preparing for the future =~ LongTermOrganizationalBenefits'
  tesseract.model <- 'Project Plan Success =~ CostPerformance + TimePerformance + ScopePerformance
                    Green Efficacy =~ SocietalBenefits
                    Business Case Success =~ ShortTermOrganizationalBenefits + LongTermOrganizationalBenefits + UserSatisfaction + UserBenefits
                    Shared Stakeholder Point of View =~ SupplierSatisfaction + PublicSatisfaction + EmployeeRetention + StakeholderSatisfaction'

  fitIron <- cfa(ironTriangle.model, data = df)
  fitMulti <- cfa(multidimensional.model, data = df)
  fitTesseract <- cfa(tesseract.model, data = df)
  
  # Summarize the fits
  s1 <- summary(fitIron, fit.measures = TRUE)
  s2 <- summary(fitMulti, fit.measures = TRUE)
  s3 <- summary(fitTesseract, fit.measures = TRUE)
  
  paste0(substitute(df), "_comparison_output") <- compareFit(fitIron, fitMulti, fitTesseract)
  
  # print(s1)
  # print(s2)
  # print(s3)
  
  # Define the subfolder
  subfolder <- "output/images/industry"
  
  # Check if the subfolder exists, and create it if it doesn't
  if (!dir.exists(subfolder)) {
    dir.create(subfolder, recursive = TRUE)
  }
  
  # Define the filenames for the diagrams
  ironTriangleFilename <- paste0(subfolder, "/",substitute(df), "_ironTrianglePathDiagram.png")
  multidimensionalFilename <- paste0(subfolder, "/", substitute(df), "_multidimensionalPathDiagram.png")
  #tesseractFilename <- paste0(subfolder, "/", substitute(df), "_tesseractPathDiagram.png")
  
  # Generate the diagrams with the correct file paths
  ironTrianglePathDiagram <- semPlot::semPaths(fitIron, 
                                               what = "path", 
                                               whatLabels = "std", 
                                               style = "lisrel", 
                                               edge.label.cex = .9, 
                                               rotation = 1, 
                                               curve = 2, 
                                               layoutSplit = FALSE, 
                                               normalize = FALSE, 
                                               filetype = "png", 
                                               filename = ironTriangleFilename, 
                                               height = 9, 
                                               width = 6.5, 
                                               residScale = 10)
  
  multidimensionalPathDiagram <- semPlot::semPaths(fitMulti, 
                                                   what = "path", 
                                                   whatLabels = "std", 
                                                   style = "lisrel", 
                                                   edge.label.cex = .9, 
                                                   rotation = 1, 
                                                   curve = 2, 
                                                   layoutSplit = FALSE, 
                                                   normalize = FALSE, 
                                                   filetype = "png", 
                                                   filename = multidimensionalFilename, 
                                                   height = 9, 
                                                   width = 6.5, 
                                                   residScale = 10)
  
  tesseractPathDiagram <- semPlot::semPaths(fitTesseract,
                                            what = "path",
                                            whatLabels = "std",
                                            style = "lisrel",
                                            edge.label.cex = .9,
                                            rotation = 1,
                                            curve = 2,
                                            layoutSplit = FALSE,
                                            normalize = FALSE,
                                            filetype = "png",
                                            filename = tesseractFilename,
                                            height = 9,
                                            width = 6.5,
                                            residScale = 10)
  # 
  # Return the results
  models <- list(fitIron, fitMulti, fitTesseract)
  results <- compareLavaan(models)
  return(results, paste0(substitute(df), "_comparison_output"))

}

education = df_industry10
healthcare = df_industry12
transport_warehousing = df_industry15
accomodation_or_food_service = df_industry16
information = df_industry17
other_services = df_industry18
finance_or_insurance = df_industry19
real_estate = df_industry2
unclassified = df_industry20
government = df_industry21
mining = df_industry3
prof_science_technical_services = df_industry4
utilities = df_industry5
management = df_industry6
construction = df_industry7
admin_support_waste_mgmt = df_industry8
manufacturing = df_industry9
wholesale_trade = df_industry11
retail = df_industry13
arts_and_entertainment = df_industry14

education_results <- run_analysis(education)
#wholesale_trade_results <- run_analysis(wholesale_trade)
healthcare_results <- run_analysis(healthcare)
#retail_results <- run_analysis(retail)
#arts_and_entertainment_results <- run_analysis(arts_and_entertainment)
transport_warehousing_results <- run_analysis(transport_warehousing)
accomodation_or_food_service_results <- run_analysis(accomodation_or_food_service)
information_results <- run_analysis(information)
other_services_results <- run_analysis(other_services)
finance_or_insurance_results <- run_analysis(finance_or_insurance)
#real_estate_results <- run_analysis(real_estate)
#unclassified_results <- run_analysis(unclassified)
government_results <- run_analysis(government)
#mining_results <- run_analysis(mining)
prof_science_technical_services_results <- run_analysis(prof_science_technical_services)
utilities_results <- run_analysis(utilities)
management_results <- run_analysis(management)
construction_results <- run_analysis(construction)
#admin_support_waste_mgmt_results <- run_analysis(admin_support_waste_mgmt)
manufacturing_results <- run_analysis(manufacturing)

# Initialize an empty data frame to store the results
# Initialize an empty data frame to store the results
comparison_results <- data.frame()

# List of all your data subset names
data_names <- c("education", "healthcare", "information", "government", 
                "prof_science_technical_services", "utilities", 
                "management", "construction", "manufacturing")

# Loop over each result
for (data_name in data_names) {
  # Get the result for the current data subset
  result <- run_analysis(get(paste0(data_name)))
  
  # Extract the fit measures for the best model
  best_model <- result$fit.measures[which.min(result$fit.measures$AIC), ]
  
  # Add the data name to the best model result
  best_model$data <- data_name
  
  # Append the best model result to comparison_results
  comparison_results <- rbind(comparison_results, best_model)
}

# Print the comparison results
print(comparison_results)

run_analysis <- function(df) {
  # Convert non-numeric columns to factors
  df.3 <- as.data.frame(lapply(df, factor))
  het.mat <- hetcor(df.3)$cor #creating a correlation matrix - everything looks fairly ok
  print(het.mat)
  print("KMO test result: ")
  print(KMO(df))
  cat("\n") #produces value of 0.83 which is regarded as 'meritorous'
  print("Bartlet test result: ")
  print(bartlett.test(df))
  cat("\n") #statistically significant with chi(square) of 1268.2 and p value much much less than 0.01
  print("Determinant of matrix: ")
  print(det(het.mat))
  cat("\n")
  
  # Fit models
  ironTriangle.model <- 'Iron Triangle =~ TimePerformance + CostPerformance + ScopePerformance'
  multidimensional.model <- 'Project Efficiency =~ TimePerformance + CostPerformance
                            Impact on the Customer =~ ScopePerformance + UserSatisfaction + UserBenefits 
                            Business Success =~ ShortTermOrganizationalBenefits
                            Preparing for the future =~ LongTermOrganizationalBenefits'
  tesseract.model <- 'Project Plan Success =~ CostPerformance + TimePerformance + ScopePerformance
                    Green Efficacy =~ SocietalBenefits
                    Business Case Success =~ ShortTermOrganizationalBenefits + LongTermOrganizationalBenefits + UserSatisfaction + UserBenefits
                    Shared Stakeholder Point of View =~ SupplierSatisfaction + PublicSatisfaction + EmployeeRetention + StakeholderSatisfaction'
  
  fitIron <- cfa(ironTriangle.model, data = df)
  fitMulti <- cfa(multidimensional.model, data = df)
  fitTesseract <- cfa(tesseract.model, data = df)
  
  # Summarize the fits
  s1 <- summary(fitIron, fit.measures = TRUE)
  s2 <- summary(fitMulti, fit.measures = TRUE)
  s3 <- summary(fitTesseract, fit.measures = TRUE)
  
  comparison_output <- compareFit(fitIron, fitMulti, fitTesseract)
  
  # Define the subfolder
  subfolder <- "output/images/industry"
  
  # Check if the subfolder exists, and create it if it doesn't
  if (!dir.exists(subfolder)) {
    dir.create(subfolder, recursive = TRUE)
  }
  
  # Define the filenames for the diagrams
  ironTriangleFilename <- paste0(subfolder, "/", substitute(df), "_ironTrianglePathDiagram.png")
  multidimensionalFilename <- paste0(subfolder, "/", substitute(df), "_multidimensionalPathDiagram.png")
  tesseractFilename <- paste0(subfolder, "/", substitute(df), "_tesseractPathDiagram.png")
  
  # Generate the diagrams with the correct file paths
  ironTrianglePathDiagram <- semPlot::semPaths(fitIron, 
                                               what = "path", 
                                               whatLabels = "std", 
                                               style = "lisrel", 
                                               edge.label.cex = .9, 
                                               rotation = 1, 
                                               curve = 2, 
                                               layoutSplit = FALSE, 
                                               normalize = FALSE, 
                                               filetype = "png", 
                                               filename = ironTriangleFilename, 
                                               height = 9, 
                                               width = 6.5, 
                                               residScale = 10)
  
  multidimensionalPathDiagram <- semPlot::semPaths(fitMulti, 
                                                   what = "path", 
                                                   whatLabels = "std", 
                                                   style = "lisrel", 
                                                   edge.label.cex = .9, 
                                                   rotation = 1, 
                                                   curve = 2, 
                                                   layoutSplit = FALSE, 
                                                   normalize = FALSE, 
                                                   filetype = "png", 
                                                   filename = multidimensionalFilename, 
                                                   height = 9, 
                                                   width = 6.5, 
                                                   residScale = 10)
  
  tesseractPathDiagram <- semPlot::semPaths(fitTesseract,
                                            what = "path",
                                            whatLabels = "std",
                                            style = "lisrel",
                                            edge.label.cex = .9,
                                            rotation = 1,
                                            curve = 2,
                                            layoutSplit = FALSE,
                                            normalize = FALSE,
                                            filetype = "png",
                                            filename = tesseractFilename,
                                            height = 9,
                                            width = 6.5,
                                            residScale = 10)
  
  # Return the results
  models <- list(fitIron, fitMulti, fitTesseract)
  results <- compareLavaan(models)
  
  return(list(results = results, comparison_output = comparison_output))
}

# Initialize an empty list to store the results
comparison_results <- list()

# List of all your data subset names
data_names <- c("education", "healthcare", "information", "government", 
                "prof_science_technical_services", "utilities", 
                "management", "construction", "manufacturing")

# Loop over each result
for (data_name in data_names) {
  # Get the result for the current data subset
  df <- get(paste0(data_name))
  analysis_result <- run_analysis(df)
  
  # Extract the fit measures for the best model
  best_model <- analysis_result$results$fit.measures[which.min(analysis_result$results$fit.measures$AIC), ]
  
  # Add the data name to the best model result
  best_model$data <- data_name
  
  # Append the best model result to comparison_results
  comparison_results[[data_name]] <- best_model
  
  # Print the comparison output
  print(paste0(data_name, "_comparison_output"))
  print(analysis_result$comparison_output)
}

# Convert the list to a data frame
comparison_results_df <- do.call(rbind, lapply(comparison_results, as.data.frame))

# Print the comparison results
print(comparison_results_df)
 
