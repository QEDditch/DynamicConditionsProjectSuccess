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

#data <- read_csv("/Users/de51/Library/CloudStorage/OneDrive-UniversityofSussex/Funding proposals/Wins/Success in PM/rawData/Dynamic Conditions for Project Success_April 22, 2022_19.52.csv")
data <- read.csv("C:\\Users\\David Eggleton\\OneDrive - University of Sussex\\Funding proposals\\Wins\\Success in PM\\rawData\\Dynamic Conditions for Project Success_April 22, 2022_19.52.csv")
temporaryData = data %>% 
  select(ResponseId, Q1) %>% #Just selecting data related to 'what is project success question
  filter(!is.na(Q1)) %>% #Removing all unanswered obs 
  slice(n=-1) %>% #removing first two rows which are ireelevant
  slice(n=-1) %>%   #removing first two rows which are ireelevant  %>% 
  tidyr::separate_rows(Q1, sep = ",") %>% #separating out the data contained in each observation 
  filter(Q1 != "") %>% #Removing any empty values
  mutate(value = 1) %>% #converting values into dummies
  tidyr::pivot_wider(    
    names_from = Q1,
    values_from = value,
    values_fill = 0 #sorting everything out
  ) %>%
  rename(
    "CostPerformance" = 2,
    "SafetyPerformance" = 3,
    "TimePerformance" = 4,
    "ScopePerformance" = 5,
    "otherToBeDeleted" = 6,
    "UserSatisfaction" = 7,
    "StakeholderSatisfaction" = 8,
    "SupplierSatisfaction" = 9,
    "TeamSatisfaction" = 10,
    "PublicSatisfaction" = 11,
    "otherToBeDeleted2" = 12,
    "UserBenefits" = 13,
    "ShortTermOrganizationalBenefits" = 14,
    "LongTermOrganizationalBenefits" = 15,
    "EmployeeRetention" = 16,
    "SocietalBenefits" = 17,
    "otherToBeDeleted3" = 18
  )
cleanData <- temporaryData[,c("CostPerformance",
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
df.2 <- sapply(cleanData, as.factor)
het.mat <- hetcor(df.2)$cor #creating a correlation matrix - everything looks fairly ok
KMO(cleanData) #produces value of 0.83 which is regarded as 'meritorous'
bartlett.test(cleanData) #statistically significant  with chi(square) of 1268.2 and p value much much less than 0.01
det(het.mat) #checking determinent of c