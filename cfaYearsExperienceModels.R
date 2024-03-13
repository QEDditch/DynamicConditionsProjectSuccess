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

#data <- read_csv("/Users/de51/Library/CloudStorage/OneDrive-UniversityofSussex/Funding proposals/Wins/Success in PM/rawData/Dynamic Conditions for Project Success_April 22, 2022_19.52.csv")
data <- read.csv("C:\\Users\\David Eggleton\\OneDrive - University of Sussex\\Funding proposals\\Wins\\Success in PM\\rawData\\Dynamic Conditions for Project Success_April 22, 2022_19.52.csv")
temporaryDataExperience = data %>% 
  dplyr::select(ResponseId, Q1, Q15) %>% #Just selecting data related to 'what is project success question
  filter(!is.na(Q1)) %>% #Removing all unanswered obs
  filter(!is.na(Q15)) %>% 
  slice(-1) %>% #removing first two rows which are ireelevant
  slice(-1) %>%   #removing first two rows which are ireelevant  %>% 
  tidyr::separate_rows(Q1, sep = ",") %>% #separating out the data contained in each observation 
  filter(Q1 != "") %>% #Removing any empty values
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
    "Years of experience" = 2
  )
cleanDataExperience <- temporaryDataExperience[,c("Years of experience",
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

demographic_groups <- unique(cleanDataExperience$`Years of experience`)
list_of_dataframes <- lapply(demographic_groups, function(group) {
  your_data_subset <- cleanDataExperience[cleanDataExperience$`Years of experience` == group, ]
  return(your_data_subset)
})
cleanDataExperience <- cleanDataExperience %>%
  mutate_all(as.numeric)

# Assign each dataframe to a variable
for (i in seq_along(list_of_dataframes)) {
  assign(paste0("df_", demographic_groups[i]), list_of_dataframes[[i]])
}

df_names <- ls(pattern = "^df_")

column_to_remove <- "Years of experience"  # Replace with your actual column name

# Loop through each data frame
for (df_name in df_names) {
  # Get the data frame
  current_df <- get(df_name)
  current_df <- current_df %>% select(-column_to_remove)
  
  # Update the data frame in the environment
  assign(df_name, current_df)
} 
  # Remove the specified column

# Loop through each dataframe
your_functions <- function(df) {
  # Convert columns to factors using sapply
  dfTemp <- sapply(df_1, as.factor)
  
  # Create a correlation matrix
  het.mat <- hetcor(dfTemp)$cor
  
  # Print the KMO value
  cat("KMO:", KMO(df), "\n")
  
  # Perform the Bartlett test
  bartlett_result <- bartlett.test(df)
  cat("Bartlett test - Chi-square:", bartlett_result$statistic, "p-value:", bartlett_result$p.value, "\n")
  
  # Print the determinant of the correlation matrix
  cat("Determinant of the correlation matrix:", det(het.mat), "\n")
  
  sem_model <- switch(
    model_name,
    "iron_triangle" = 'Iron Triangle =~ TimePerformance + CostPerformance + ScopePerformance',
    "multidimensional" = 'Project Efficiency =~ TimePerformance + CostPerformance
                            Impact on the Customer =~ ScopePerformance + UserSatisfaction + UserBenefits 
                            Business Success =~ ShortTermOrganizationalBenefits
                            Preparing for the future =~ LongTermOrganizationalBenefits',
    "tesseract" = 'Project Plan Success =~ CostPerformance + TimePerformance + ScopePerformance
                        Green Efficacy =~ SocietalBenefits
                        Business Case Success =~ ShortTermOrganizationalBenefits + LongTermOrganizationalBenefits + UserSatisfaction + UserBenefits
                        Shared Stakeholder Point of View =~ SupplierSatisfaction + PublicSatisfaction + EmployeeRetention + StakeholderSatisfaction'
  )
  
  # Fit the SEM model
  fit <- cfa(sem_model, data = df)
  s <- summary(fit, fit.measures = TRUE)
  
  # Create the path diagram
  path_diagram <- semPlot::semPaths(fit, what = "path", whatLabels = "std", style = "lisrel",
                                    edge.label.cex = 0.9, rotation = 1, curve = 2,
                                    layoutSplit = FALSE, normalize = FALSE,
                                    height = 9, width = 6.5, residScale = 10)
  
  # Save the path diagram with a unique filename
  save_png(path_diagram, paste0(model_name,"_", df, "_PathDiagram.png"))
  
  # Return the summary object
  return(df)
}

list_of_dataframes <- lapply(df_names, your_functions)
