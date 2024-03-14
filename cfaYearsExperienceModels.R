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

data <- read_csv("/Users/de51/Library/CloudStorage/OneDrive-UniversityofSussex/Funding proposals/Wins/Success in PM/rawData/Dynamic Conditions for Project Success_April 22, 2022_19.52.csv")
# data <- read.csv("C:\\Users\\David Eggleton\\OneDrive - University of Sussex\\Funding proposals\\Wins\\Success in PM\\rawData\\Dynamic Conditions for Project Success_April 22, 2022_19.52.csv")
temporaryDataExperience = data %>% 
  dplyr::select(ResponseId, Q1, Q15) %>% #Just selecting data related to 'what is project success question
  filter(!is.na(Q1)) %>% #Removing all unanswered obs
  filter(!is.na(Q15)) %>% 
  slice(-1) %>% #removing first two rows which are ireelevant
  slice(-1) %>%   #removing first two rows which are ireelevant  %>% 
  tidyr::separate_rows(Q1, sep = ",") %>% #separating out the data contained in each observation 
  filter(Q1 != "") %>% #Removing any empty values
  filter(Q15 != "") %>%
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
    "Years_of_experience" = 2
  )
cleanDataExperience <- temporaryDataExperience[,c("Years_of_experience",
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

sapply(cleanDataExperience, class)
dfTemp1 = cleanDataExperience
sapply(dfTemp1, class)
df_temp1 <- subset(dfTemp1, select = -c(Years_of_experience))
sapply(df_temp1, class)
df.2 <- sapply(df_temp1, as.factor)
het.mat <- hetcor(df_temp1)$cor #creating a correlation matrix - everything looks fairly ok
KMO(df_temp1) #produces value of 0.83 which is regarded as 'meritorous'

bartlett.test(df_temp1) #statistically significant  with chi(square) of 1268.2 and p value much much less than 0.01
det(het.mat) 

demographic_groups <- unique(cleanDataExperience$`Years_of_experience`)
list_of_dataframes <- lapply(demographic_groups, function(group) {
  your_data_subset <- cleanDataExperience[cleanDataExperience$`Years_of_experience` == group, ]
  return(your_data_subset)
})
cleanDataExperience <- cleanDataExperience %>%
  mutate_all(as.numeric)

# Assign each dataframe to a variable
for (i in seq_along(list_of_dataframes)) {
  assign(paste0("df_", demographic_groups[i]), list_of_dataframes[[i]])
}

sapply(df_1, class)
dfTemp3 = df_1
sapply(dfTemp3, class)
df.2 <- sapply(dfTemp3, as.factor)
het.mat <- hetcor(dfTemp3)$cor #creating a correlation matrix - everything looks fairly ok
KMO(dfTemp3) #produces value of 0.83 which is regarded as 'meritorous'

bartlett.test(dfTemp3) #statistically significant  with chi(square) of 1268.2 and p value much much less than 0.01
det(het.mat) 


# Assuming cleanData is a list of dataframes


for (df_name in names(list_of_dataframes)) {
  # Get the data frame
  df_temp <- get(df_name)
  
  # Remove the column "Years_of_experience"
  df_temp <- subset(df_temp, select = -c(Years_of_experience))
  
  # Assign the modified data frame back to its original name
  assign(df_name, df_temp)
}

list_of_dataframes <- list(df_1 = df_1, 
                           df_2 = df_2,
                           df_3 = df_3, 
                           df_4 = df_4,
                           df_5 = df_5,
                           df_6 = df_6)  # List of your dataframes
# Define the models
ironTriangle.model <- 'Iron Triangle =~ TimePerformance + CostPerformance + ScopePerformance'
multidimensional.model <- 'Project Efficiency =~ TimePerformance + CostPerformance
                            Impact on the Customer =~ ScopePerformance + UserSatisfaction + UserBenefits 
                            Business Success =~ ShortTermOrganizationalBenefits
                            Preparing for the future =~ LongTermOrganizationalBenefits'
tesseract.model <- 'Project Plan Success =~ CostPerformance + TimePerformance + ScopePerformance
                    Green Efficacy =~ SocietalBenefits
                    Business Case Success =~ ShortTermOrganizationalBenefits + LongTermOrganizationalBenefits + UserSatisfaction + UserBenefits
                    Shared Stakeholder Point of View =~ SupplierSatisfaction + PublicSatisfaction + EmployeeRetention + StakeholderSatisfaction'

# Define a function to perform the analysis and save results
run_analysis <- function(df_name, df) {
  # Convert non-numeric columns to factors
  for (col in names(df)) {
    if (!is.numeric(df[[col]]) && !is.factor(df[[col]])) {
      print(paste("Column", col, "has unexpected data type:", class(df[[col]])))
      print(unique(df[[col]]))
      df[[col]] <- as.factor(df[[col]])
    }
  }
  
  # Filter out non-numeric columns
  numeric_cols <- sapply(df, is.numeric)
  df_numeric <- df[, numeric_cols]
  
  # Calculate KMO
  kmo <- KMO(df_numeric)
  
  # Bartlett's Test
  bartlett <- bartlett.test(df_numeric)
  
  # Create the correlation matrix
  het.mat <- hetcor(df_numeric)$cor
  
  # Check determinant of the correlation matrix
  det_value <- det(het.mat)
  
  # Fit models
  fitIron <- cfa(ironTriangle.model, data = df_numeric)
  fitMulti <- cfa(multidimensional.model, data = df_numeric)
  fitTesseract <- cfa(tesseract.model, data = df_numeric)
  
  # Summarize the fits
  s1 <- summary(fitIron, fit.measures = TRUE)
  s2 <- summary(fitMulti, fit.measures = TRUE)
  s3 <- summary(fitTesseract, fit.measures = TRUE)
  
  # Generate path diagrams and save them
  ironTrianglePathDiagram <- semPlot::semPaths(fitIron, what = "path", whatLabels = "std", style = "lisrel", edge.label.cex=.9, rotation = 2, curve = 2, layoutSplit = FALSE, normalize = FALSE, height = 9, width = 6.5, residScale = 10)
  multidimensionalPathDiagram <- semPlot::semPaths(fitMulti, what = "path", whatLabels = "std", style = "lisrel", edge.label.cex=.9, rotation = 1, curve = 2, layoutSplit = FALSE, normalize = FALSE, height = 9, width = 6.5, residScale = 10)
  tesseractPathDiagram <- semPlot::semPaths(fitTesseract, what = "path", whatLabels = "std", style = "lisrel", edge.label.cex=.9, rotation = 1, curve = 2, layoutSplit = FALSE, normalize = FALSE, height = 9, width = 12, residScale = 10)
  
  save_png(ironTrianglePathDiagram, paste0(df_name, "_ironTrianglePathDiagram.png"))
  save_png(multidimensionalPathDiagram, paste0(df_name, "_multidimensionalPathDiagram.png"))
  save_png(tesseractPathDiagram, paste0(df_name, "_tesseractPathDiagram.png"))
  
  # Return the results
  return(list(kmo = kmo, bartlett = bartlett, det_value = det_value, s1 = s1, s2 = s2, s3 = s3))
}


# Apply the analysis function to each dataframe in the list
results <- list()
for (name in names(list_of_dataframes)) {
  results[[name]] <- run_analysis(name, list_of_dataframes[[name]])
}

# You can access results for each dataframe using indexing
# For example, results[["cleanData1"]] will give you results for the dataframe cleanData1, results[["cleanData2"]] for the cleanData2, and so on.

