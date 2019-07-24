#Regression for variable influence

library(readr)
library(car)
library(ggplot2)
library(mice)
library(corrplot)
library(RColorBrewer)
library(purrr)
library(tidyr)
library(dplyr)
library(glmnet)
library(MASS)
library(gvlma)
library(DAAG)
library(bootstrap)
library(relaimpo)
library(devtools)
install_github("vqv/ggbiplot")
library(ggbiplot)


dat <- read_csv("data_regression.csv")
finaltable <- read_csv("scoretable.csv")
dat <- merge(dat, finaltable, by="MSA")
dim(dat)
#We have varying amounts of missing data
nacount <- colSums(is.na(dat))
percentna <- nacount/356
percentna <- as.data.frame(percentna)
#Only impute for variables that have greater than a 50% fill rate
data <-  dat[-c(59:78, 99, 120, 22, 19, 18)]  #not using CBRE data, qualityoflife, cpi, purchasingpower
#regraph
nacount <- colSums(is.na(data))
percentna <- nacount/356
percentna <- as.data.frame(percentna)

################IMPUTATION#####################
imputed_dat <- mice::mice(data, m=1, maxit = 10, method = 'cart', seed = 500) 
#extract the first imputed dataset from the methood
dat_c <- mice::complete(imputed_dat,1)
#put a step here to export this data so we can use it in tableau, add back the removed columns without imputation
export <- cbind(dat_c, dat[c(59:78, 99, 120, 22, 19, 18)]) #add back in the variables that were not imputed
write.table(export, "imputeddatasubset.txt", sep="\t")
#check for na
nacount <- colSums(is.na(dat_c))
sum(nacount)

#######Clean up data#########
#Remove the dollars by industry b\c that's just a linear combo of target, keep percentage (also remove perofrev0 b\c that's just 1.0 for all)
model_dat <-  dat_c[-c(57:75)]
#Remove categorical variables (MSA, State, Lat, Long), keep numerical and factors
model_dat <- model_dat[5:96]
model_dat <- model_dat[-c(84:90)]
model_dat <- model_dat[-c(84)]
#check class of the variables 
lapply(model_dat,class)
#convert Tech_Hub to a factor, keep num airlines as integer and all else numeric
model_dat[1:74] <- lapply(model_dat[1:74], as.numeric)
model_dat$Tech_Hub <- as.factor(model_dat$Tech_Hub)
model_dat$Amazon <- as.factor(model_dat$Amazon)
model_dat$TopGrad <- as.factor(model_dat$TopGrad)
lapply(model_dat,class)

###############################
#Mult Linear Model - ALL VARIABLES AND SEE WHAT'S UP
sink <- lm(finalwithuni ~ ., data = model_dat)
summary(sink)

#log transformation
nontransdata <- model_dat[-c(1,2,6,7,8,18,21,22,29,33,35:47,49,52,53:71,72,73,74,79)] 
transdata <- model_dat[c(1,2,6,7,8,18,21,22,29,33,35:47,49,52,53:71,72,73,74,79)]
transdata[transdata == 0] <- .0001
transdata <- log10(transdata)
model_data <- cbind(transdata, nontransdata )
#PCA
#Drop PerofRev0 b\c that's just the sum
PerofRev <- model_data[c(27:45)]
pca1 = prcomp(PerofRev, scale. = TRUE)
ggbiplot(pca1)
# sqrt of eigenvalues
pca1$sdev
# loadings
head(pca1$rotation)
# PCs (aka scores)
head(pca1$x) #take the first 3
pca <- pca1$x[,1:3]
notRev <- model_data[-c(27:45)] 
model_data <- cbind(pca, notRev)

#rerun full model
#Let's do ridge regression again for variable selection
X <- model_data[-c(71)]
X <- as.matrix(X)  
fit <- glmnet(X, model_data$finalscore) 
plot(fit, xvar = "lambda", label = TRUE)
print(fit)
[25,]  7 0.7350 1.29e-02 - select a bit above
coef(fit, s=1.28e-02)
#....
model <- lm(finalscore ~  PC1 + PC2 + PovertyRate + GDP5Year, data = model_data)
summary(model)
#Multiple R-squared:  0.373,	Adjusted R-squared:  0.365 


#RELATIVE IMPORTANCE
calc.relimp(model, type=c("lmg","last","first","pratt"), rela=TRUE)


