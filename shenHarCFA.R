library(tidyverse)
library(dplyr)
library(psych)
library(GPArotation)
library(corpcor)
library(polycor)
library(EFA.dimensions)
library(xtable)
library(paran)
library(lavaan)

data <- read_csv("/Users/de51/Library/CloudStorage/OneDrive-UniversityofSussex/Funding proposals/Wins/Success in PM/rawData/Dynamic Conditions for Project Success_April 22, 2022_19.52.csv")
#data <- read.csv("C:\\Users\\David Eggleton\\OneDrive - University of Sussex\\Funding proposals\\Wins\\Success in PM\\rawData\\Dynamic Conditions for Project Success_April 22, 2022_19.52.csv")
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
    costPerf = 2,
    safetyPerf = 3,
    timePerf = 4,
    scopePerf = 5,
    otherToBeDeleted = 6,
    userSat = 7,
    stakeholderSat = 8,
    supplierSat = 9,
    teamSat = 10,
    publicSat = 11,
    otherToBeDeleted2 = 12,
    userBene = 13,
    orgShortBene = 14,
    orgLongBene = 15,
    empRet = 16,
    SocBene = 17,
    otherToBeDeleted3 = 18
  )
cleanData <- temporaryData[,c("costPerf",
                              "timePerf",
                              "scopePerf",
                              "safetyPerf",
                              "userSat",
                              "stakeholderSat",
                              "supplierSat",
                              "teamSat",
                              "publicSat",
                              "userBene",
                              "orgShortBene",
                              "orgLongBene",
                              "empRet",
                              "socBene")] #moving data into a cleanDataset using a more logical listing format  

#given that we've got binary values (either 0 or 1) we follow the instructions provided here - https://www.google.com/url?sa=t&rct=j&q=&esrc=s&source=web&cd=&ved=2ahUKEwiYyfqo4JT7AhVVoVwKHefJBXcQFnoECBMQAQ&url=https%3A%2F%2Fit.unt.edu%2Fsites%2Fdefault%2Ffiles%2Fbinaryfa_l_jds_sep2014_0.pdf&usg=AOvVaw3FDWJhR1m9ZqwRGoDHh3CE which proved very useful

df.2 <- sapply(cleanData, as.factor) #turning into factors

het.mat <- hetcor(df.2)$cor 

#Now lets do that with the classic Shenhar and Dvir model
cleanData.model <- '
projectEfficiency =~ costPerf + timePerf + scopePerf + safetyPerf
impactOnCustomer =~ userSat + stakeholderSat + userBene
businessSuccess =~ orgShortBene + orgLongbene
preparationForFutures =~ publicSat + socBene'

cleanData.fit <- cfa(cleanData.model, data = cleanData)
summary(cleanData.fit, standardized = TRUE)
semPlot::semPaths(cleanData.fit)
semPlot::semPaths(cleanData.fit, "std")
