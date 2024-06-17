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
temporaryDataOrgSize = data %>% 
  dplyr::select(ResponseId, Q1, Q16) %>% #Just selecting data related to 'what is project success question
  filter(!is.na(Q1)) %>% #Removing all unanswered obs
  filter(!is.na(Q16)) %>% 
  slice(-1) %>% #removing first two rows which are ireelevant
  slice(-1) %>%   #removing first two rows which are ireelevant  %>% 
  tidyr::separate_rows(Q1, sep = ",") %>% #separating out the data contained in each observation 
  filter(Q1 != "") %>% #Removing any empty values
  filter(Q16 != "") %>%
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
    "Organization_size" = 2
  )
cleanDataOrgSize <- temporaryDataOrgSize[,c("Organization_size",
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

dfTemp1 = cleanDataOrgSize
df_org_sizetemp1 <- subset(dfTemp1, select = -c(Organization_size))
df.2 <- as.data.frame(lapply(df_org_sizetemp1, factor))
het.mat <- hetcor(df.2)$cor #creating a correlation matrix - everything looks fairly ok
KMO(df_org_sizetemp1) #produces value of 0.83 which is regarded as 'meritorous'
print(het.mat)
bartlett.test(df_org_sizetemp1) #statistically significant  with chi(square) of 1268.2 and p value much much less than 0.01
det(het.mat) 

org_groups <- unique(cleanDataOrgSize$`Organization_size`)
list_of_dataframes <- lapply(org_groups, function(group) {
  your_data_subset <- cleanDataOrgSize[cleanDataOrgSize$`Organization_size` == group, ]
  return(your_data_subset)
})

# Assign each dataframe to a variable
for (i in seq_along(list_of_dataframes)) {
  assign(paste0("df_org_size", demographic_groups[i]), list_of_dataframes[[i]])
}

# Assuming cleanData is a list of dataframes
list_of_dataframes <- list(df_org_size1 = df_org_size1, 
                           df_org_size2 = df_org_size2,
                           df_org_size3 = df_org_size3, 
                           df_org_size4 = df_org_size4,
                           df_org_size5 = df_org_size5)  # List of your dataframes

for (df_org_sizename in names(list_of_dataframes)) {
  # Get the data frame
  df_org_sizetemp <- get(df_org_sizename)
  
  # Remove the column "Organization_size"
  df_org_sizetemp <- subset(df_org_sizetemp, select = -c(Organization_size))
  
  # Assign the modified data frame back to its original name
  assign(df_org_sizename, df_org_sizetemp)
}

# Define the models
#num_nans <- sum(is.na(df_org_size6))

# Print the result
print(num_nans)
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
  
  # print(s1)
  # print(s2)
  # print(s3)
  
  # # Generate path diagrams and save them
  ironTrianglePathDiagram <- semPlot::semPaths(fitIron, what = "path", whatLabels = "std", style = "lisrel", edge.label.cex=.9, rotation = 1, curve = 2, layoutSplit = FALSE, normalize = FALSE, filetype = "png", filename = paste0(substitute(df), "_ironTrianglePathDiagram"), height = 9, width = 6.5, residScale = 10)
  multidimensionalPathDiagram <- semPlot::semPaths(fitMulti, what = "path", whatLabels = "std", style = "lisrel", edge.label.cex=.9, rotation = 1, curve = 2, layoutSplit = FALSE, normalize = FALSE, filetype = "png", filename = paste0(substitute(df), "_multidimensionalPathDiagram"), height = 9, width = 6.5, residScale = 10)
  tesseractPathDiagram <- semPlot::semPaths(fitTesseract, what = "path", whatLabels = "std", style = "lisrel", edge.label.cex=.9, rotation = 1, curve = 2, layoutSplit = FALSE, normalize = FALSE, filetype = "png",  filename = paste0(substitute(df), "_tesseractPathDiagram"),height = 9, width = 6.5, residScale = 10)
  # 
  # save_png(ironTrianglePathDiagram, filename = paste0(substitute(df), "_ironTrianglePathDiagram.png"))
  # save_png(multidimensionalPathDiagram, filename = paste0(substitute(df), "multidimensionalPathDiagram.png"))
  # save_png(tesseractPathDiagram, filename = paste0(substitute(df), "tesseractPathDiagram.png"))
  
  # Return the results
  models <- list(fitIron, fitMulti, fitTesseract)
  results <- compareLavaan(models)
  return(results)#, s1 = s1, s2 = s2, s3 = s3))
}

lessThan30 = df_org_size1
thirtyTo100 = df_org_size2
hundredTo500 = df_org_size3
fiveHundredTo1000 = df_org_size4
moreThan1000 = df_org_size5

less_than_30_results <- run_analysis(lessThan30)
thirtyTo100_results <- run_analysis(thirtyTo100)
hundredTo500_results <- run_analysis(hundredTo500)
fiveHundredTo1000_results <- run_analysis(fiveHundredTo1000)
moreThan1000_results <- run_analysis(moreThan1000)


