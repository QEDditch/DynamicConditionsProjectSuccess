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

# dfTemp1 = cleanDataOrgSize
# df_org_sizetemp1 <- subset(dfTemp1, select = -c(Organization_size))
# df.2 <- as.data.frame(lapply(df_org_sizetemp1, factor))
# het.mat <- hetcor(df.2)$cor #creating a correlation matrix - everything looks fairly ok
# KMO(df_org_sizetemp1) #produces value of 0.83 which is regarded as 'meritorous'
# print(het.mat)
# bartlett.test(df_org_sizetemp1) #statistically significant  with chi(square) of 1268.2 and p value much much less than 0.01
# det(het.mat) 

org_groups <- unique(cleanDataOrgSize$`Organization_size`)
list_of_dataframes <- lapply(org_groups, function(group) {
  your_data_subset <- cleanDataOrgSize[cleanDataOrgSize$`Organization_size` == group, ]
  return(your_data_subset)
})

# Assign each dataframe to a variable
for (i in seq_along(list_of_dataframes)) {
  assign(paste0("df_org_size", org_groups[i]), list_of_dataframes[[i]])
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

# Define a function to perform the analysis and save results
run_analysis <- function(df, org_size) {
  # Convert non-numeric columns to factors
  df.3 <- as.data.frame(lapply(df, factor))
  het.mat <- hetcor(df.3)$cor
  print(het.mat)
  print("KMO test result: ")
  print(KMO(df))
  cat("\n")
  print("Bartlett test result: ")
  print(bartlett.test(df))
  cat("\n")
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
  
  # Extract fit measures
  get_fit_measures <- function(fit) {
    summary_fit <- summary(fit, fit.measures = TRUE)
    fit_measures <- list(
      chisq = ifelse(!is.null(summary_fit$chisq), summary_fit$chisq, NA),
      df = ifelse(!is.null(summary_fit$df), summary_fit$df, NA),
      pvalue = ifelse(!is.null(summary_fit$pvalue), summary_fit$pvalue, NA),
      rmsea = ifelse(!is.null(summary_fit$rmsea), summary_fit$rmsea, NA),
      cfi = ifelse(!is.null(summary_fit$cfi), summary_fit$cfi, NA),
      tli = ifelse(!is.null(summary_fit$tli), summary_fit$tli, NA),
      srmr = ifelse(!is.null(summary_fit$srmr), summary_fit$srmr, NA),
      aic = ifelse(!is.null(summary_fit$aic), summary_fit$aic, NA),
      bic = ifelse(!is.null(summary_fit$bic), summary_fit$bic, NA)
    )
    return(fit_measures)
  }
  
  fit_measures_iron <- get_fit_measures(fitIron)
  fit_measures_multi <- get_fit_measures(fitMulti)
  fit_measures_tesseract <- get_fit_measures(fitTesseract)
  
  # Print extracted fit measures for debugging
  print("Fit measures for Iron Triangle model:")
  print(fit_measures_iron)
  print("Fit measures for Multidimensional model:")
  print(fit_measures_multi)
  print("Fit measures for Tesseract model:")
  print(fit_measures_tesseract)
  
  # Ensure all fit measures are available
  fit_measures <- data.frame(
    Model = c("Model1", "Model2", "Model3"),
    chisq = c(fit_measures_iron$chisq, fit_measures_multi$chisq, fit_measures_tesseract$chisq),
    df = c(fit_measures_iron$df, fit_measures_multi$df, fit_measures_tesseract$df),
    pvalue = c(fit_measures_iron$pvalue, fit_measures_multi$pvalue, fit_measures_tesseract$pvalue),
    rmsea = c(fit_measures_iron$rmsea, fit_measures_multi$rmsea, fit_measures_tesseract$rmsea),
    cfi = c(fit_measures_iron$cfi, fit_measures_multi$cfi, fit_measures_tesseract$cfi),
    tli = c(fit_measures_iron$tli, fit_measures_multi$tli, fit_measures_tesseract$tli),
    srmr = c(fit_measures_iron$srmr, fit_measures_multi$srmr, fit_measures_tesseract$srmr),
    aic = c(fit_measures_iron$aic, fit_measures_multi$aic, fit_measures_tesseract$aic),
    bic = c(fit_measures_iron$bic, fit_measures_multi$bic, fit_measures_tesseract$bic),
    dchi = c(NA, fit_measures_multi$chisq - fit_measures_iron$chisq, fit_measures_tesseract$chisq - fit_measures_multi$chisq),
    ddf = c(NA, fit_measures_multi$df - fit_measures_iron$df, fit_measures_tesseract$df - fit_measures_multi$df),
    npval = c(NA, pchisq(fit_measures_multi$chisq - fit_measures_iron$chisq, fit_measures_multi$df - fit_measures_iron$df, lower.tail = FALSE), pchisq(fit_measures_tesseract$chisq - fit_measures_multi$chisq, fit_measures_tesseract$df - fit_measures_multi$df, lower.tail = FALSE))
  )
  
  # Print fit measures data frame for debugging
  print(fit_measures)
  
  # Check if the output directory exists, if not create it
  output_dir <- "output"
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Save the fit measures as a LaTeX table
  latex_table <- xtable(fit_measures)
  latex_filename <- paste0(output_dir, "/fit_measures_", org_size, ".tex")
  
  print(paste("Saving LaTeX table to:", latex_filename))  # Debugging statement
  
  # Try writing to the file and check for errors
  tryCatch({
    print(latex_table, type = "latex", file = latex_filename)
    print("File saved successfully!")  # Debugging statement
  }, error = function(e) {
    print(paste("Error saving file:", e))
  })
  
  # Return the results
  models <- list(fitIron, fitMulti, fitTesseract)
  results <- compareLavaan(models)
  return(results)
}

# Run the analysis for each organizational size
less_than_30_results <- run_analysis(df_org_size1, "less_than_30")
thirtyTo100_results <- run_analysis(df_org_size2, "thirtyTo100")
hundredTo500_results <- run_analysis(df_org_size3, "hundredTo500")
fiveHundredTo1000_results <- run_analysis(df_org_size4, "fiveHundredTo1000")
moreThan1000_results <- run_analysis(df_org_size5, "moreThan1000")



