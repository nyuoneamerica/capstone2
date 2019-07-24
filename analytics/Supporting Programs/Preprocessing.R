setwd("H:/NYU BACKUP/Capstone/Regression")
library(readr)
library(car)
library(ggplot2)
library(mice)
library(RColorBrewer)
library(purrr)
library(tidyr)
library(dplyr)

#LOAD DATA
dat <- read_csv("data_regression.csv")
dim(dat)

#We have varying amounts of missing data
#Let's count the number of NAs per columns
nacount <- colSums(is.na(dat))
percentna <- nacount/356
percentna <- as.data.frame(percentna)

#plot dist of missing data
png("initialmissing.png")
initialmissing <- ggplot(data=percentna, aes(percentna)) + 
  geom_histogram() +
  labs(title="PERCENT MISSING VALUES IN FULL DATASET", 
       x = "Percent Missing",
       y="Proportion") +
  theme_bw() +
  theme(text=element_text(size=15, family="RobotoBold")) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(color = "gray"), axis.ticks.x = element_blank(), axis.ticks.y = element_blank()) 

print(initialmissing)
dev.off()

#Only impute for variables that have greater than a 50% fill rate
data <-  dat[-c(59:78, 99, 120, 22, 19, 18)]  #not using CBRE data, qualityoflife, cpi, purchasingpower
#regraph
nacount <- colSums(is.na(data))
percentna <- nacount/356
percentna <- as.data.frame(percentna)

png("aftermissing.png")
aftermissing <- ggplot(data=percentna, aes(percentna)) + 
  geom_histogram() +
  labs(title="PERCENT MISSING VALUES IN SUBSETTED DATASET", 
       x = "Percent Missing",
       y="Proportion") +
  theme_bw() +
  theme(text=element_text(size=15, family="RobotoBold")) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(color = "gray"), axis.ticks.x = element_blank(), axis.ticks.y = element_blank()) 
print(aftermissing)
dev.off()

################IMPUTATION#####################
#impute data using MICE, cannot use pmm method  https://www.kaggle.com/c/house-prices-advanced-regression-techniques/discussion/24586
#other documentation: https://datascienceplus.com/handling-missing-data-with-mice-package-a-simple-approach/
#multiple imputation...m=5, we'll make 5 datasets and select the best
imputed_dat <- mice::mice(data, m=5, maxit = 20, method = 'cart', seed = 500) #can increase maxit for final run
#let's see results
png("imputedensity.png")
imputedensity < -densityplot(imputed_dat) #first imputation is the best
print(imputedensity)
dev.off()


imputed_dat <- mice::mice(data, m=1, maxit = 10, method = 'cart', seed = 500)
densityplot(imputed_dat)
#extract the first imputed dataset from the methood
dat_c <- mice::complete(imputed_dat,1)
#put a step here to export this data so we can use it in tableau, add back the removed columns without imputation
export <- cbind(dat_c, dat[c(59:78, 99, 120, 22, 19, 18)]) #add back in the variables that were not imputed
write.table(export, "imputeddatasubset.txt", sep="\t")

#check for na
nacount <- colSums(is.na(dat_c))
sum(nacount)