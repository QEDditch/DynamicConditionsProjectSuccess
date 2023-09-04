library(tidyverse)
library(dplyr)
library(psych)
library(GPArotation)
library(corpcor)
library(polycor)
library(EFA.dimensions)
library(xtable)
library(paran)
#exploratory factor analysis

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

#given that we've got binary values (either 0 or 1) we follow the isntructions provided here - https://www.google.com/url?sa=t&rct=j&q=&esrc=s&source=web&cd=&ved=2ahUKEwiYyfqo4JT7AhVVoVwKHefJBXcQFnoECBMQAQ&url=https%3A%2F%2Fit.unt.edu%2Fsites%2Fdefault%2Ffiles%2Fbinaryfa_l_jds_sep2014_0.pdf&usg=AOvVaw3FDWJhR1m9ZqwRGoDHh3CE which proved very useful

df.2 <- sapply(cleanData, as.factor) #turning into factors

het.mat <- hetcor(df.2)$cor #creating a correlation matrix - everything looks fairly ok
KMO(cleanData) #produces value of 0.83 which is regarded as 'meritorous'
bartlett.test(cleanData) #statistically significant  with chi(square) of 1268.2 and p value much much less than 0.01
det(het.mat) #checking determinent of correlation matrix produces value of 0.00042... which is greater than critical value of 0.00001 which is good

#Q1 - how many factors to extract? We have a few methods we can use which are considered in turn:
#They are:
#1 Parallel analysis where we use MC methods. Regarded as quite accurate overview here: https://en.wikipedia.org/wiki/Parallel_analysis
#2 Velicer's (1976) MAP test
#3 Screeplot where we look for the 'elbow' in the plot - bit hand wavey
#4 Kaiser Criteria where we only use factors with eigenvalues greater than 1

#1 Parallel analysis
paran(cleanData, graph = TRUE) #Suggests two!

core_parr <- fa.parallel(cleanData,
                         cor='cor', ## type of correlation to use
                         fm="fa", ## method of factor estimation
                         n.iter=20, ## number of simulated iterations
                         fa='both', ## calculate components, factors, or both
                         SMC=TRUE, 
                         quant = .95, ## percentile cut off
                         nfactors=14) ## number of factors to extract

#2 Velicer's (1976) MAP test
png('Valicer')
Valicer <- MAP(cleanData, Ncases = 14) #output suggests only two factors!
dev.off()
#3 - Scree plot
pilot <- principal(cleanData, nfactors = 14, rotate = "none")

png('scree_plot.png')
plot(pilot$values, type="b", xlab="Number", ylab="Eigenvalue") #produces a scree plot ;looks like about 3 or four factors is about right
dev.off() #Saved for later
#4 - Kaiser criteria
pilot$values #Based on the values only 3 are above 1 so three is right using the Kaiser criterion

corMatrixTable <- print(het.mat)
print(xtable(corMatrixTable, type = "latex"), file = "corMatrixTable.tex")
eigenvalueTable <- print(pilot$values)
print(xtable(eigenvalueTable, type = "latex"), file = "eigenvalueTable.tex")

fa.1 <- factanal(covmat = het.mat, factors = 1, rotation = "varimax")
fa.2 <- factanal(covmat = het.mat, factors = 2, rotation = "varimax")
fa.3 <- factanal(covmat = het.mat, factors = 3, rotation = "varimax")
fa.4 <- factanal(covmat = het.mat, factors = 4, rotation = "varimax")
fa.5 <- factanal(covmat = het.mat, factors = 5, rotation = "varimax")

#creating a series of factor based models based on number provided using varimax rotation as used mostly in the literature - three is probably best but we'll run it with a few for fun

print(fa.3, digits = 2, cutoff=0.3, sort = TRUE) #Using Kaiser rule of only selecting factors with eigenvalues >= 1
print(fa.2, digits = 2, cutoff=0.44, sort = TRUE) #Based on Valicer and Parallel analysis findings



#TODO 
#Save eigenvalue table as Latex
#save all other outputs as latex


