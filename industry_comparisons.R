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

# Define the CFA model
cfa_model_iron_triangle <- '
  Iron Triangle =~ TimePerformance + CostPerformance + ScopePerformance
'
  

# Fit the model separately for each industry
fit_education <- cfa(cfa_model_iron_triangle, data = education)
fit_healthcare <- cfa(cfa_model_iron_triangle, data = healthcare)
#fit_transport <- cfa(cfa_model_iron_triangle, data = transport_warehousing)
# Add similar lines for other industries...

# Combine the data into a single dataframe with an industry column
combined_data <- rbind(
  cbind(education, industry = "Education"),
  cbind(healthcare, industry = "Healthcare"),
  #cbind(transport_warehousing, industry = "Transport"),
  # Add similar lines for other industries...
)

# Fit the multi-group CFA model
multi_group_fit <- cfa(cfa_model, data = combined_data, group = "industry")

# Test for measurement invariance
invariance_results <- measurementInvariance(multi_group_fit, 
                                            data = combined_data, 
                                            group = "industry", 
                                            std.lv = TRUE)
print(invariance_results)

# Check fit measures for each group (industry)
summary(multi_group_fit, fit.measures = TRUE)

# Extract and compare fit indices
fit_measures <- fitMeasures(multi_group_fit, c("cfi", "tli", "rmsea", "srmr"))
print(fit_measures)

# Compare factor loadings across industries
loadings <- parameterEstimates(multi_group_fit, standardized = TRUE)
loadings
