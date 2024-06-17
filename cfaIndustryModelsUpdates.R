library(tidyverse)
library(dplyr)
library(lavaan)
library(polycor)
library(xtable)
library(semTable)
library(lavaanPlot)
library(DiagrammeRsvg)
library(rsvg)
library(psych)
library(semTools)

# Load data
data <- read.csv("C:\\Users\\David Eggleton\\OneDrive - University of Sussex\\Funding proposals\\Wins\\Success in PM\\rawData\\Dynamic Conditions for Project Success_April 22, 2022_19.52.csv")

# Prepare temporary data for Industry
temporaryDataIndustry <- data %>% 
  select(ResponseId, Q1, Q17) %>% 
  filter(!is.na(Q1) & !is.na(Q17)) %>% 
  slice(-c(1, 2)) %>% 
  separate_rows(Q1, sep = ",") %>% 
  filter(Q1 != "", Q17 != "") %>% 
  mutate(value = 1) %>% 
  pivot_wider(names_from = Q1, values_from = value, values_fill = list(value = 0)) %>% 
  rename(
    "CostPerformance" = 3,
    "SafetyPerformance" = 4,
    "TimePerformance" = 5,
    "ScopePerformance" = 6,
    "UserSatisfaction" = 8,
    "StakeholderSatisfaction" = 9,
    "SupplierSatisfaction" = 10,
    "TeamSatisfaction" = 11,
    "PublicSatisfaction" = 12,
    "UserBenefits" = 14,
    "ShortTermOrganizationalBenefits" = 15,
    "LongTermOrganizationalBenefits" = 16,
    "EmployeeRetention" = 17,
    "SocietalBenefits" = 18,
    "industry" = 2
  )

# Clean data industry
cleanDataIndustry <- temporaryDataIndustry %>% 
  select(
    industry,
    CostPerformance,
    TimePerformance,
    ScopePerformance,
    SafetyPerformance,
    UserSatisfaction,
    StakeholderSatisfaction,
    SupplierSatisfaction,
    TeamSatisfaction,
    PublicSatisfaction,
    UserBenefits,
    ShortTermOrganizationalBenefits,
    LongTermOrganizationalBenefits,
    EmployeeRetention,
    SocietalBenefits
  )

# Remove 'industry' column and convert to factors
df_industrytemp1 <- cleanDataIndustry %>% select(-industry) %>% mutate(across(everything(), as.factor))

# Create correlation matrix
het.mat <- hetcor(df_industrytemp1)$cor
print(het.mat)

# KMO test
print("KMO test result: ")
print(KMO(df_industrytemp1))

# Bartlett's test
print("Bartlett test result: ")
print(bartlett.test(df_industrytemp1))

# Determinant of the correlation matrix
print("Determinant of matrix: ")
print(det(het.mat))

# Split data by industry
industry_groups <- unique(cleanDataIndustry$industry)
list_of_dataframes <- lapply(industry_groups, function(group) {
  subset(cleanDataIndustry, industry == group) %>% select(-industry)
})

# Create a named list of data frames
names(list_of_dataframes) <- c(
  "df_manufacturing", "df_admin_support_waste_mgmt", "df_construction", 
  "df_management", "df_utilities", "df_prof_science_technical_services", 
  "df_mining", "df_government", "df_unclassified", "df_real_estate", 
  "df_finance_or_insurance", "df_other_services", "df_information", 
  "df_accomodation_or_food_service", "df_transport_warehousing", 
  "df_arts_and_entertainment", "df_retail", "df_healthcare", 
  "df_wholesale_trade", "df_education"
)

run_analysis <- function(df) {
  # Convert non-numeric columns to factors
  df.3 <- as.data.frame(lapply(df, factor))
  het.mat <- hetcor(df.3)$cor
  print(het.mat)
  
  # Print KMO test result
  print("KMO test result: ")
  print(KMO(df))
  cat("\n")
  
  # Print Bartlett's test result
  print("Bartlett test result: ")
  print(bartlett.test(df))
  cat("\n")
  
  # Print determinant of matrix
  print("Determinant of matrix: ")
  print(det(het.mat))
  cat("\n")
  
  # Define models
  ironTriangle.model <- 'Iron Triangle =~ TimePerformance + CostPerformance + ScopePerformance'
  multidimensional.model <- 'Project Efficiency =~ TimePerformance + CostPerformance
                            Impact on the Customer =~ ScopePerformance + UserSatisfaction + UserBenefits 
                            Business Success =~ ShortTermOrganizationalBenefits
                            Preparing for the future =~ LongTermOrganizationalBenefits'
  tesseract.model <- 'Project Plan Success =~ CostPerformance + TimePerformance + ScopePerformance
                    Green Efficacy =~ SocietalBenefits
                    Business Case Success =~ ShortTermOrganizationalBenefits + LongTermOrganizationalBenefits + UserSatisfaction + UserBenefits
                    Shared Stakeholder Point of View =~ SupplierSatisfaction + PublicSatisfaction + EmployeeRetention + StakeholderSatisfaction'
  
  # Fit models
  fitIron <- cfa(ironTriangle.model, data = df)
  fitMulti <- cfa(multidimensional.model, data = df)
  fitTesseract <- cfa(tesseract.model, data = df)
  
  # Summarize the fits
  s1 <- summary(fitIron, fit.measures = TRUE)
  s2 <- summary(fitMulti, fit.measures = TRUE)
  s3 <- summary(fitTesseract, fit.measures = TRUE)
  
  # Generate path diagrams and save them
  semPlot::semPaths(fitIron, what = "path", whatLabels = "std", style = "lisrel", edge.label.cex=.9, rotation = 1, curve = 2, layoutSplit = FALSE, normalize = FALSE, filetype = "png", filename = paste0(substitute(df), "_ironTrianglePathDiagram"), height = 9, width = 6.5, residScale = 10)
  semPlot::semPaths(fitMulti, what = "path", whatLabels = "std", style = "lisrel", edge.label.cex=.9, rotation = 1, curve = 2, layoutSplit = FALSE, normalize = FALSE, filetype = "png", filename = paste0(substitute(df), "_multidimensionalPathDiagram"), height = 9, width = 6.5, residScale = 10)
  semPlot::semPaths(fitTesseract, what = "path", whatLabels = "std", style = "lisrel", edge.label.cex=.9, rotation = 1, curve = 2, layoutSplit = FALSE, normalize = FALSE, filetype = "png", filename = paste0(substitute(df), "_tesseractPathDiagram"), height = 9, width = 6.5, residScale = 10)
  
  # Return the results
  models <- list(fitIron, fitMulti, fitTesseract)
  results <- compareLavaan(models)
  return(results)
}

# Apply the analysis function to each data frame in the list
results_list <- lapply(list_of_dataframes, run_analysis)

# Print or save results as needed
print(results_list)

