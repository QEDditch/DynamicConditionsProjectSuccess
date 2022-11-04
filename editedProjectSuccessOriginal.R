library(hetcor)
library(polycor)
library(psych)


df.2 <- sapply(cleanData, as.factor)

het.mat <- hetcor(df.2)$cor

fa.1 <- factanal(covmat = het.mat, factors = 4, rotation = "varimax")
fa.2 <- factanal(covmat = het.mat, factors = 2, rotation = "varimax")
fa.3 <- factanal(covmat = het.mat, factors = 3, rotation = "varimax")
fa.4 <- factanal(covmat = het.mat, factors = 4, rotation = "varimax")
fa.5 <- factanal(covmat = het.mat, factors = 5, rotation = "varimax")

print(df.4, digits = 2, cutoff=0.3, sort = TRUE)
print(fa.4, digits = 2, cutoff=0.3, sort = TRUE)
print(fa.4, digits = 2, cutoff=0.45, sort = TRUE)
print(fa.4, digits = 2, cutoff=0.4, sort = TRUE)

f1 <- data[, c("stakeSat", "supplierSat", "teamSat", "employeeRet", "scopePerf", "orgLongBene")]
f2 <- data[, c("ï..timePerf", "budgetPerf", "safetyPerf")]
f3 <- data[, c("publicSat", "socBene")]
f4 <- data[, c("custSat", "userBene")]

alpha(f1)
alpha(f1)
alpha(f1)
alpha(f2)
alpha(f3)
alpha(f4)

df.2 <- sapply(cleanedData, as.factor)
head(df2)
df.2
lowerCor(df.2)
fa.parallel(df.2)

het.mat <- hetcor(df.2)$cor
het.mat
fa.4 <- factanal(covmat = het.mat, factors = 4, rotation = "varimax")
fa.4
print(fa.4, digits = 3, cutoff=0.4, sort = TRUE)
het2.mat <- hetcor(data)$cor
het2.mat
fa.4 <- factanal(covmat = het.mat, factors = 4, rotation = "varimax")
fa.4
print(fa.4, digits = 3, cutoff=0.4, sort = TRUE)
fa2.4 <- factanal(covmat = het2.mat, factors = 4, rotation = "varimax")
fa2.4
print(fa2.4, digits = 3, cutoff=0.4, sort = TRUE)
fa.parallel(data)
corPlot(r)
corPlot(data)
vss(data)
fa(data)
alpha(data)
head(data)
fa2.4
print(fa2.4, digits = 3, cutoff=0.4, sort = TRUE)
print(fa2.4, digits = 3, cutoff=0.3, sort = TRUE)
print(fa2.4, digits = 3, cutoff=0.35, sort = TRUE)
install.packages("corpcor")
install.packages("GPArotation")
library(GPArotation)
library(corpcor)
raqMatrix <- cor(data)
round(raqMatrix, 2)
cortest.bartlett(data)
KMO(data)
$overall
det(raqMatrix)
pc4 <- principal(data, nfactors = 4, rotate = "varimax")
pc4
plot(pc4$values, type = "b")
plot(pc4$values, type = "p")
plot(pc4$values, type = "p")
plot(pc4$values, type = "b")
plot(pc4$values, type = "b")
plot(pc4$values, type = "b")
plot(pc4$values, type = "p")
plot(pc4$values, type = "b")
plot(pc4$values, type = "l")
factor.model(pc4$loadings)
residuals <- factor.residuals(raqMatrix, pc4$loadings)
residuals <- as.matrix(residuals[upper.tri(residuals)])
large.resid <- abs(residuals) > 0.05
sum(large.resid)
sum(large.resid/nrow(residuals))
sqrt(mean(residuals^2))
hist(residuals)
print.psych(pc4, cut = 0.4, sort = TRUE)
print.psych(pc4, cut = 0.41, sort = TRUE)
pc4Oblique <- principal(data, nfactors = 4, rotate = "oblimin")
print.psych(pc4Oblique, cut = 0.3, sort = TRUE)
print.psych(pc4Oblique, cut = 0.41, sort = TRUE)
pc4 <- principal(data, nfactors = 4, rotate = "varimax", scores = TRUE)
head(pc4$scores, 10)
print.psych(pc4, cut = 0.4, sort = TRUE)
print.psych(pc4, cut = 0.41, sort = TRUE)
fa1 <- data[, c("budgetPerf", "ï..timePerf", "stakeSat", "safetyPerf", "scopePerf")]
fa2 <- data[, c("orgShortBene", "teamSat", "employeeRet", "orgLongBene", "supplierSat")]
fa3 <- data[, c("publicSat", "socBene")]
fa4 <- data[, c("custSat", "userBene")]
alpha(fa1)
alpha(fa2)
alpha(fa3)
alpha(fa4)
pc5 <- principal(data, nfactors = 5, rotate = "varimax", scores = TRUE)
print.psych(pc5, cut = 0.41, sort = TRUE)
pc3 <- principal(data, nfactors = 3, rotate = "varimax", scores = TRUE)
print.psych(pc3, cut = 0.41, sort = TRUE)
print.psych(pc4, cut = 0.40, sort = TRUE)
save.image("~/Bibliometric training/projectSuccessDefinition.RData")
load(tidyr)
install.packages("tidyr")
KMO(data)
load(psych)
load("psych")
library(psych)
KMO(data)
bartlett.test(data)
cortest.bartlett(data)
View(het.mat)
cortest.bartlett(het.mat)
cortest.bartlett(het.mat, n= nrow(data))
cortest.bartlett(het.mat, n= 14)
cortest.bartlett(het.mat, n= 13)
cortest.bartlett(het.mat, n= nrow(data))
cor_matrix <- cor(data)
cortest.bartlett(cor_matrix, n= nrow(cor_matrix))
cortest.bartlett(cor_matrix, n= nrow(data))
cortest.bartlett(cor_matrix, n = nrow(data))
cortest.bartlett(cor_matrix, n = nrow(cor_matrix))
cortest.bartlett(cor_matrix, n = nrow(data))
plot(pc3$values, type="b")
scree_plot = plot(pc3$values, type="b")
png('scree_plot.png')
dev.copy(scree_plot, 'scree_plot.png')
scree_plot = plot(pc3$values, type="b", xlab="Number", ylab="Eigenvalue")
View(scree_plot)
plot(pc3$values, type="b", xlab="Number", ylab="Eigenvalue")
pc3
pc4
plot(pc4$values, type="b", xlab="Number", ylab="Eigenvalue")
pc4$values
print.psych(pc4, cut=0.4, sort = TRUE)
print.psych(pc4, cut=0.41, sort = TRUE)
factor_results <- print.psych(pc4, cut=0.41, sort = TRUE)
cor_matrix
corMatrixTable <- print(cor_matrix)
print(xtable(corMatrixTable, type = "latex"), file = "corMatrixTable.tex")
install.packages("xtable")
library(xtable)
print(xtable(corMatrixTable, type = "latex"), file = "corMatrixTable.tex")