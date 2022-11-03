library(tidyverse)
library(dplyr)
library(psych)
library(GPArotation)
library(corpcor)

data <- read_csv("/Users/de51/Library/CloudStorage/OneDrive-UniversityofSussex/Funding proposals/Wins/Success in PM/rawData/Dynamic Conditions for Project Success_April 22, 2022_19.52.csv")
temporaryData = data %>% 
  select(ResponseId, Q1) %>% #Just selecting data related to 'what is project success question
  filter(!is.na(Q1)) %>% #Removing all unanswered obs 
  slice(n=-1) %>% #removing first two rows which are ireelevant
  slice(n=-1) %>%   #removing first two rows which are ireelevant  %>% 
  tidyr::separate_rows(Q1, sep = ",")%>% #separating out the data contained in each observation 
  filter(Q1 != "") %>% #Removing any empty values
  mutate(value = 1) %>% #converting values into dummies
  tidyr::pivot_wider(    
    names_from = Q1,
    values_from = value,
    values_fill = 0 #sorting everything out
  )  
cleanData <- temporaryData[,c(1,5,2,3,4,17,6,7,8,9,12,16,10,14,11,15,18,13)] %>% 
  subset(select =-c(1,6,12,18)) %>% 
  rename(
      timePerf = 1,
      scopePerf = 2,
      budgetPerf = 3,
      safetyPerf = 4,
      custSat = 5,
      stakeSat = 6,
      supplierSat = 7,
      teamSat = 8,
      publicSat = 9,
      userBene = 10,
      orgShortBene = 11,
      orgLongBene = 12,
      employeeRet = 13,
      SocBene = 14
  ) #could be some confusion here double check things in some detail

S <- cov(cleanData)
S.eigen <- eigen(S)
plot(S.eigen$values, xlab = 'Eigenvalue Number', ylab = 'Eigenvalue Size', main = 'Scree Graph', type = 'b', xaxt = 'n')
axis(1, at = seq(1, 15, by = 1))

corMatrix <- cor(cleanData)
roundCorMatrix <- round(corMatrix, 2)

cortest.bartlett(cleanData)
KMO(cleanData) #0.83 - 'good'

det(corMatrix) #0.091679 - much higher than critical value of 0.00001

pc1 <- principal(cleanData, nfactors=4, rotate="varimax")
pc1

plot(pc1$values, type="b")

factor.model(pc1$loadings)

residuals <- factor.residuals(corMatrix, pc1$loadings) %>% 
  as.matrix(residuals[upper.tri(residuals)]) 
large.resid <- abs(residuals) > 0.5
sum(large.resid)
sum(large.resid/nrow(residuals))
