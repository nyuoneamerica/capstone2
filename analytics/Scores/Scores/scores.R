#Creating Scores
#Impute all variables
#Explore, Modify and Normalize the variables 
#Seperate into the 4 categories
#Run regressions on 
#    a) RevPerCap0 b) RevPerCap51+52+55 = TechRev
#    Choose the most predictive target variable -> looks like TechRev
#    Then figure out which variables are most important to each regression 
#    Use the importance with some tweaking if necessary to make the final scores
#    Scale the scores 
#    Regression to figure out the importance of all 4 categories in aggregate regression 
#    Use these weights to develop Final Scores and then rescale
#    Quick plots to see distributions, reality check 

setwd("H:/NYU BACKUP/Capstone/Scores")
library(mice)
library(readr)
library(corrplot)
library(glmnet)
library(gdata)
library(dplyr)
library(ggplot2)
library(tidyr)
library(gvlma)
library(reshape2)
library(plotly)
library(relaimpo)
library(bestNormalize) #https://cran.r-project.org/web/packages/bestNormalize/vignettes/bestNormalize.html
library(KernSmooth)
library(gtools)
install_github("vqv/ggbiplot")
library(ggbiplot)
library(randomForest)

#SAME AS REGRESSION PREPROCESSING
#LOAD DATA
dat <- read_csv("data_regression.csv")
dim(dat)

#Only impute for variables that have greater than a 50% fill rate
data <-  dat[-c(59:78, 99, 120, 22, 19, 18)]  #not using CBRE data, qualityoflife, cpi, purchasingpower
imputed_dat <- mice::mice(data, m=1, maxit = 1, method = 'cart', seed = 500)
dat_c <- mice::complete(imputed_dat,1)
#Engineer a Tech Revenue Category
TecRev <- dat_c$RevPerCap51 + dat_c$RevPerCap52 + dat_c$RevPerCap54 + dat_c$RevPerCap55
dat_c <- cbind(dat_c, TecRev)
#Remove the dollars by industry b\c that's just a linear combo of target, keep percentage (also remove perofrev0 b\c that's just 1.0 for all)
model_dat <-  dat_c[-c(57:76)]
#Create a Region Variable
region <- read_csv("stateregion.csv")
data <- merge(model_dat, region, by="State")
#Remove categorical variables (Lat, Long), keep numerical and factors
model_dat <- data[c(1:2,5:89)]
#convert Tech_Hub to a factor, keep num airlines as integer and all else numeric
model_dat[3:85] <- lapply(model_dat[3:85], as.numeric)
model_dat$Tech_Hub <- as.factor(model_dat$Tech_Hub)
model_dat$Amazon <- as.factor(model_dat$Amazon)
model_dat$TopGrad <- as.factor(model_dat$TopGrad)

#CLEANING UP THE DATA FOR SCORES
#Remove the dollars by industry b\c that's just a linear combo of target
model_dat <-  model_dat[-c(25)] #remove electricity which didn't impute well

#check class of the variables 
lapply(model_dat,class)
#convert Tech_Hub, Amazon, & TopGrad to a factor, keep num airlines as integer and all else numeric
model_dat$Region <- as.factor(model_dat$Region)
model_dat$SubRegion <- as.factor(model_dat$SubRegion)
lapply(model_dat,class)

region <- model_dat[c(85:86)] 
model_dat <- model_dat[-c(85:86)] #get rid of region stuff for now

#Normlize the variables 
   # function 
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }
  # apply function 
norm1 <- as.data.frame(lapply(model_dat[,3:4], normalize))
norm2 <- as.data.frame(lapply(model_dat[,6:75], normalize))
norm3 <- as.data.frame(lapply(model_dat[,78:84], normalize))

#combo <- cbind(model_dat[,1:4], model_dat[,111:112], model_dat[,7],model_dat[,105:106],norm1, norm2, norm3)
combo <- cbind(model_dat[,1:2], model_dat[,5], model_dat[,76:77],norm1, norm2, norm3, region)
combo <- rename.vars(combo, from = "model_dat[, 5]", to = "TechHub")



#PCA on distribution of ages
age <- combo[c(38:50)]
pca_age <- prcomp(age, scale. = TRUE)
  # sqrt of eigenvalues
pca_age$sdev
  # loadings
head(pca_age$rotation)
  # PCs (aka scores)
summary(pca_age) #just need first

png("pcaage1.png")
pcaage1 <- ggbiplot(pca_age)
print(pcaage1)
dev.off()

#let's group these MSAs up into tech-hub or not
TH <- model_dat[c(5)]
png("pcaage_techhub.png")
pcaage_techhub <- ggbiplot(pca_age, ellipse=TRUE, groups=TH[,1])
print(pcaage_techhub)
dev.off()

#let's group these MSAs up into Amazon pitch or not
AM <- model_dat[c(76)]
png("pcaage_amazon.png")
pcaage_amazon <- ggbiplot(pca_age, ellipse=TRUE, groups=AM[,1])
print(pcaage_amazon)
dev.off()


head(pca_age$x) #take the first 1
pca_age <- pca_age$x[,1]
combo <- combo[-c(38:50)] 
combo <- cbind(combo, pca_age)
#combo <- rename.vars(combo, from = "PC1", to = "PC1_age")


#PCA on Percent of Rev Spent 
rev <- combo[c(43:61)]
pca_rev <- prcomp(rev, scale. = TRUE)
  # sqrt of eigenvalues
pca_rev$sdev
  # loadings
head(pca_rev$rotation)
summary(pca_age)

head(pca_rev$x) #take the first 3
pca_rev <- pca_rev$x[,1:3]
combo <- combo[-c(43:61)] 
combo <- cbind(combo, pca_rev)
combo <- rename.vars(combo, from = "PC1", to = "PC1_rev")
combo <- rename.vars(combo, from = "PC2", to = "PC2_rev")
combo <- rename.vars(combo, from = "PC3", to = "PC3_rev")


#Seperate into 4 categories
#PevPerCap0 and TecRev will be tried for targets

talent <- combo[c(1,2,3,4,5,11,12,14,23,24,25,26,36,37,38,39,40,42,43,44,45,52,53,54,55)]
connect <- combo[c(1,2,3,4,6,11,16,17,18,19,20,22,23,42,43,44,45,46,47,48,49,52,53,54,55,56,57,58)]
cost <- combo[c(1,2,3,4,5,6,7,8,9,11,13,15,16,20,21,25,26,27,28,29,30,31,32,33,34,35,41,42,48,49,50,51,52,53,54,55,56,57,58)]
quality <- combo[c(1,2,3,4,5,6,11,13,15,16,17,18,19,20,21,22,30,37,38,42,46,47,49,50,51,52,53,54,55,56,57,58)]

              
##################Correlation Matrix#################
##############TALENT#####################
#just numberical values for cor matrix
model_dat_num <- talent[-c(1:5,23:24)]
png("talentcormatrix.png")
talentcormatrix <- cor(model_dat_num)
corrplot(M, type="upper", order="hclust", tl.col="black", tl.srt=45, tl.cex = .5)
print(talentcormatrix)
dev.off()


##############CONNECT#####################
model_dat_num <- connect[-c(1:4,23:24)]  
png("connectcormatrix.png")
connectcormatrix <- cor(model_dat_num)
corrplot(M, type="upper", order="hclust", tl.col="black", tl.srt=45, tl.cex = .5)
print(connectcormatrix)
dev.off()


##############COST#####################
model_dat_num <- cost[-c(1:5,34:35)]  
png("costcormatrix.png")
costcormatrix <- cor(model_dat_num)
corrplot(M, type="upper", order="hclust", tl.col="black", tl.srt=45, tl.cex = .5)
print(costcormatrix)
dev.off()

##############QUALITY#####################
model_dat_num <- quality[-c(1:5,27:28)]  
png("qualitycormatrix.png")
qualitycormatrix <- cor(model_dat_num)
corrplot(M, type="upper", order="hclust", tl.col="black", tl.srt=45, tl.cex = .5)
print(qualitycormatrix)
dev.off()

#########MODELING PRINT FUNCTION##############
#supply fxn with title of model (in quotes) and model name
writeOutput.F <- function(output.title, modelname) {
  sink(file="modeloutputs.txt", append=TRUE) #creates an empty file; if TRUE, it will append to the file
  print("##########################", row.names=FALSE)
  print(output.title, row.names=FALSE)
  print("##########################",  row.names=FALSE)
  summary(modelname) #write summary of the model to text file 
}


#########################REGRESSION##############################################################################
########### REGRESSION TALENT ##############
#on RevPerCap0
talent1 <- talent[-c(1,2,22,23,24)] 
talent1_fit<- lm(RevPerCap0 ~ ., data = talent1)
summary(talent1_fit)
#Multiple R-squared:  0.1898,	Adjusted R-squared:  0.144 

#Let's do ridge regression for variable selection
X <- talent1[-c(16)]
X <- as.matrix(X)  
fit <- glmnet(X, talent1$RevPerCap0) 
plot(fit, xvar = "lambda", label = TRUE)
print(fit)
#[29,] 10 0.15550 2.937e-03-> select a bit above 
coef(fit, s=2.938e-03 )
talent1_fit <- lm(RevPerCap0 ~ Amazon + TopGrad + PercentBach + AvgSchoolRep + AvgTuition + MaleFemaleRatio + Labor + LaborForceAnnGrowth + Patents + VCFirms, data = talent1)
summary(talent1_fit)
#Multiple R-squared:  0.1633,	Adjusted R-squared:  0.139

########on TecRev -> BETTER
talent2 <- talent[-c(1,2,23,24,18)] 
talent2_fit<- lm(TecRev~ ., data = talent2)
summary(talent2_fit)
#Multiple R-squared:  0.5961,	Adjusted R-squared:  0.5733 


#Let's do ridge regression for variable selection
X <- talent2[-c(19)]
X <- as.matrix(X)  
fit <- glmnet(X, talent2$TecRev) 
plot(fit, xvar = "lambda", label = TRUE)
print(fit)
#[29,]  9 0.55520 6.368e-03 -> select a bit above 
coef(fit, s=6.369e-03)
talent2_fit <- lm(TecRev ~ TechHub + Amazon + TopGrad + PopDensity + PercentBach + AvgTuition + MaleFemaleRatio + Labor + VCFirms, data = talent2)
summary(talent2_fit)
#Multiple R-squared:  0.5662,	Adjusted R-squared:  0.5549 
writeOutput.F("Multiple Linear Regression on Tech Revenue - Talent", talent2_fit)
closeAllConnections() #close all connections and restore all sink diversi

#TRY RF MODEL
fit_T <- randomForest(TecRev ~ TechHub + Amazon + TopGrad + PopDensity + PercentBach + AvgTuition + MaleFemaleRatio + Labor + VCFirms, data = talent2)
print(fit_T) # view results 
#% Var explained: 58.38

importance(fit_T) # importance of each predictor
varImpPlot(fit_L)



########### REGRESSION CONNECT #############################################################
#on RevPerCap0
connect1 <- connect[-c(1,2,23,24,22)] 
connect1_fit<- lm(RevPerCap0 ~ ., data = connect1)
summary(connect1_fit)
#Multiple R-squared:  0.5636,	Adjusted R-squared:  0.5347 

###########on TecRev -> BETTER
connect2 <- connect[-c(1,2,23,24,14)] 
connect2_fit<- lm(TecRev~ ., data = connect2)
summary(connect2_fit)
#Multiple R-squared:  0.7609,	Adjusted R-squared:  0.7451
#Let's do ridge regression for variable selection
X <- connect2[-c(19)]
X <- as.matrix(X)  
fit <- glmnet(X, connect2$TecRev) 
plot(fit, xvar = "lambda", label = TRUE)
print(fit)
#[37,]  8 0.7347 4.556e-03 -> select a bit above 
coef(fit, s=4.557e-03  )
connect2_fit <- lm(TecRev ~ Amazon + PopDensity + VCFirms + asai + densityscore + PC1_rev + PC2_rev + PC3_rev, data = connect2)
summary(connect2_fit)
#Multiple R-squared:  0.7386,	Adjusted R-squared:  0.7326 
writeOutput.F("Multiple Linear Regression on Tech Revenue - Connectivity", connect2_fit)
closeAllConnections() #close all connections and restore all sink diversi

#TRY RF MODEL
fit_CN <- randomForest(TecRev ~ Amazon + PopDensity + VCFirms + asai + densityscore + PC1_rev + PC2_rev + PC3_rev, data = connect2)
print(fit_CN) # view results 
#% Var explained: 73.37

importance(fit_CN) # importance of each predictor
varImpPlot(fit_CN)


########### REGRESSION COST ##############
#on RevPerCap0
cost1 <- cost[-c(1,2,34,35,33)] 
cost1_fit<- lm(RevPerCap0 ~ ., data = cost1)
summary(cost1_fit)
#Multiple R-squared:  0.6862,	Adjusted R-squared:  0.654 


###########on TecRev -> BETTER
cost2 <- cost[-c(1,2,34,35,28)] 
cost2_fit<- lm(TecRev~ ., data = cost2)
summary(cost2_fit)
#Multiple R-squared:  0.7947,	Adjusted R-squared:  0.7736 
#Let's do ridge regression for variable selection
X <- cost2[-c(30)]
X <- as.matrix(X)  
fit <- glmnet(X, cost2$TecRev) 
plot(fit, xvar = "lambda", label = TRUE)
print(fit)
#[35,] 15 0.7638 5.488e-03-> select a bit above 
coef(fit, s=5.489e-03 )
#deviating
cost2_fit <- lm(TecRev ~ Amazon + GDP + AvgPropTaxPerCap + PopDensity + MedianIncome + PropPricetoIncomeRatio + AvgSchoolRep + CorpTaxMax + 
                  GDP5Year + GDPperCap + PC1_rev + PC2_rev + PC3_rev, data = cost2)
summary(cost2_fit)
#Multiple R-squared:  0.7772,	Adjusted R-squared:  0.7687
#print
writeOutput.F("Multiple Linear Regression on Tech Revenue - Cost", cost2_fit)
closeAllConnections() #close all connections and restore all sink diversi

#TRY RF MODEL
fit_C <- randomForest(TecRev ~ Amazon + GDP + AvgPropTaxPerCap + PopDensity + MedianIncome + PropPricetoIncomeRatio + AvgSchoolRep + CorpTaxMax + 
                        GDP5Year + GDPperCap + PC1_rev + PC2_rev + PC3_rev, data = cost2)
print(fit_C) # view results 
#% Var explained: 78.77

importance(fit_C) # importance of each predictor
varImpPlot(fit_C)


########### REGRESSION QUALITY ##############
#on RevPerCap0
quality1 <- quality[-c(1,2,26,27,28)] 
quality1_fit<- lm(RevPerCap0 ~ ., data = quality1)
summary(quality1_fit)
#Multiple R-squared:  0.6138,	Adjusted R-squared:  0.5833 


######on TecRev -> BETTER
quality2 <- quality[-c(1,2,27,28,20)] 
quality2_fit<- lm(TecRev~ ., data = quality2)
summary(quality2_fit)
#Multiple R-squared:  0.767,	Adjusted R-squared:  0.7486 
#Let's do ridge regression for variable selection
X <- quality2[-c(23)]
X <- as.matrix(X)  
fit <- glmnet(X, quality2$TechRev) 
plot(fit, xvar = "lambda", label = TRUE)
print(fit)
#[32,]  [35,] 15 0.7638 5.488e-03 -> select a bit above, edit/add obviuosly quality of life factors
coef(fit, s=5.489e-03)
quality2_fit <- lm(TecRev ~ Amazon + PopDensity + MedianIncome +PropPricetoIncomeRatio + PovertyRate + HealthCareIndex + CostLivingComposit 
                    + landusescore + activityscore + streetscore + PC1_rev,
                     data = quality2)
summary(quality2_fit)
#Multiple R-squared:  0.5616,	Adjusted R-squared:  0.5476
writeOutput.F("Multiple Linear Regression on Tech Revenue - Quality", quality2_fit)
closeAllConnections() #close all connections and restore all sink diversi

#TRY RF MODEL
fit_Q <- randomForest(TecRev ~ Amazon + PopDensity + MedianIncome +PropPricetoIncomeRatio + PovertyRate + HealthCareIndex + CostLivingComposit 
                      + landusescore + activityscore + streetscore + PC1_rev,
                      data = quality2)
print(fit_Q) # view results 
#% Var explained: 51.98

importance(fit_Q) # importance of each predictor
varImpPlot(fit_Q)




#########VAR IMPORTANCE#######################################################

#####VARIABLE IMPORTANCE - TALENT ##############
# Bootstrap Measures of Relative Importance (100 samples) 
boot <- boot.relimp(talent2_fit, b = 100, type = c("lmg", "last", "first", "pratt"), rank = TRUE, diff = TRUE, rela = TRUE)
booteval.relimp(boot) # print result
plot(booteval.relimp(boot,sort=TRUE), cex.axis = .1) # plot result
##########TALENT SCORE############
booteval.relimp(boot,sort=TRUE)

#Relative importance metrics: (use lmg)
#lmg       last      first       pratt
#TechHub         0.05994658 0.00667570 0.08804249  0.02780908
#Amazon          0.21552670 0.31652326 0.18214936  0.29486856
#TopGrad         0.02633075 0.03957891 0.03381096 -0.04523188
#PopDensity      0.08083261 0.02444924 0.09431997  0.05982863
#PercentBach     0.22263237 0.38285539 0.16839361  0.29908976
#AvgTuition      0.15724536 0.04690005 0.18971364  0.14215600
#MaleFemaleRatio 0.01942483 0.02375732 0.01784810  0.02048252
#Labor           0.13003717 0.04107486 0.15412476  0.10135478
#VCFirms         0.08802362 0.11818527 0.07159710  0.09964255

importance(fit_T) # importance of each predictor
varImpPlot(fit_T)
#IncNodePurity
#TechHub             0.1303249
#Amazon              0.7610379
#TopGrad             0.1407618
#PopDensity          1.6492541
#PercentBach         1.5764875
#AvgTuition          0.9268272
#MaleFemaleRatio     0.4396805
#Labor               1.7885763
#VCFirms             0.5581274


combo$TechHub <- as.numeric(combo$TechHub) -1
combo$TopGrad <- as.numeric(combo$TopGrad) -1
combo$Amazon <- as.numeric(combo$Amazon) -1
#unscaled_talent_score <- 0.085041607*combo$TechHub + 0.019206753*combo$TopGrad + 0.017551347*combo$Age2534 + 0.184487864*combo$PercentBach + 
#  0.074054465*combo$TotEnrollment + 0.170142322*combo$AvgTuition + -0.057845876*combo$AvgSchoolRep + -0.020068509*combo$MaleFemaleRatio + 
#  0.091434420*combo$Labor + -0.004399969*combo$MedianAge + 0.001428946*combo$LaborForceAnnGrowth + 0.045167181*combo$CBRE_Tech_Talent +
#  0.046374675*combo$CBRE_Concentration_17 + 0.087486679*combo$CBRE_17_Tech_Degrees + 0.002826917*combo$CBRE_TalentWage_5Yr_Growth + 
#  0.087691603*combo$Patents + -0.004790866*combo$PC2_age

unscaled_talent_score <- combo$TechHub*0.0309840920743382+combo$Amazon*0.173870938967328+combo$TopGrad*0.0156406264139591+
  combo$PopDensity*0.135881198935275+combo$PercentBach*0.233009369708521+combo$AvgTuition*0.125138763520472+
  combo$MaleFemaleRatio*0.0377688365350526+combo$Labor*0.165515571382341+combo$VCFirms*0.0821905999627124
hist(unscaled_talent_score)

#make normal
norm_talent_score <- bestNormalize(unscaled_talent_score)
norm_talent_score <- predict(norm_talent_score)
hist(norm_talent_score)

#rescale this between .05 and .95
talent_score <- (.95-.05)/(max(norm_talent_score)-min(norm_talent_score))*(norm_talent_score-max(norm_talent_score))+.95
talent_score <- talent_score*100
hist(talent_score)


#####VARIABLE IMPORTANCE - CONNECT ##############
boot <- boot.relimp(connect2_fit, b = 100, type = c("lmg", "last", "first", "pratt"), rank = TRUE, diff = TRUE, rela = TRUE)
booteval.relimp(boot) # print result
plot(booteval.relimp(boot,sort=TRUE), cex.axis = .1) # plot result
booteval.relimp(boot,sort=TRUE)
#Relative importance metrics: (use lmg)
#lmg        last       first       pratt
#Amazon       0.134575111 0.023865355 0.164772722 0.075944441
#PopDensity   0.060011149 0.018239915 0.085322060 0.043937001
#VCFirms      0.053960858 0.027296573 0.064766900 0.045061423
#asai         0.139425694 0.003827773 0.195557288 0.041556760
#densityscore 0.051887998 0.001297729 0.089579456 0.013261919
#PC1_rev      0.007454205 0.017414145 0.001567298 0.004954509
#PC2_rev      0.530956227 0.854133584 0.389316501 0.754248158
#PC3_rev      0.021728759 0.053924925 0.009117773 0.021035790

importance(fit_CN) # importance of each predictor
varImpPlot(fit_CN)

#IncNodePurity
#Amazon           0.5354205
#PopDensity       1.0991625
#VCFirms          0.5568651
#asai             1.4291437
#densityscore     0.6009568
#PC1_rev          0.5070526
#PC2_rev          3.2369408
#PC3_rev          0.5043176


#unscaled_connect_score <- 0.057922912*combo$TechHub + 0.205354036*combo$Amazon + 0.050465602*combo$PopDensity + 0.006893477*combo$PollutionIndex +
#  0.009627496*combo$TrafficIndex + 0.085379363*combo$Labor + 0.002722620*combo$LaborForceAnnGrowth + 0.082586424*combo$CBRE_TechScore +
#  0.008213789*combo$CBRE_Vacancy_Rate + 0.086012042*combo$CBRE_Office_Vacancy_Rate_13 + 0.033391318*combo$VCInvPer + 
#  0.068289190*combo$VCFirms + 0.050058256*combo$numairport + 0.128385800*combo$asai + 0.063676985*combo$landusescore + 
#  0.013142098*combo$activityscore + 0.047878593*combo$streetscore
#hist(unscaled_connect_score)

unscaled_connect_score <- combo$Amazon*0.146811771652994+combo$PopDensity*0.156135133819905+combo$VCFirms*0.122068723256437+combo$asai*0.197222040150124+
  combo$densityscore*0.120289250722363+combo$NumAirline*0.128736540199088+combo$numairport*0.0791534404469243
hist(unscaled_connect_score)

#make normal
norm_connect_score <- bestNormalize(unscaled_connect_score)
norm_connect_score <- predict(norm_connect_score)
hist(norm_connect_score)

#rescale this between .05 and .95
connect_score <- (.95-.05)/(max(norm_connect_score)-min(norm_connect_score))*(norm_connect_score-max(norm_connect_score))+.95
connect_score <- connect_score*100
hist(connect_score)





#####VARIABLE IMPORTANCE - COST ##############
# Bootstrap Measures of Relative Importance (100 samples) 
boot <- boot.relimp(cost2_fit, b = 100, type = c("lmg", "last", "first", "pratt"), rank = TRUE, diff = TRUE, rela = TRUE)
booteval.relimp(boot) # print result
plot(booteval.relimp(boot,sort=TRUE), cex.axis = .1) # plot result
booteval.relimp(boot,sort=TRUE)
#Relative importance metrics: 
#lmg         last        first        pratt
#Amazon                 0.093078426 0.0192570202 1.188068e-01  0.055036971
#GDP                    0.087328081 0.0137333387 1.180470e-01  0.060022933
#AvgPropTaxPerCap       0.019232677 0.0007520753 3.052086e-02  0.005065299
#PopDensity             0.044688712 0.0053785073 6.152015e-02  0.024199816
#MedianIncome           0.133871280 0.0197319901 1.504099e-01  0.081086127
#PropPricetoIncomeRatio 0.005745766 0.0121771471 3.647148e-03 -0.006880776
#AvgSchoolRep           0.045309513 0.0245358265 6.600968e-02 -0.045189647
#CorpTaxMax             0.003080147 0.0083420679 7.603394e-05  0.000757036
#GDP5Year               0.006542667 0.0084129374 1.047154e-02 -0.009510707
#GDPperCap              0.140110018 0.0890010272 1.520759e-01  0.153660865
#PC1_rev                0.006623449 0.0039562536 1.130076e-03  0.002125719
#PC2_rev                0.385784434 0.6691380262 2.807106e-01  0.651561239
#PC3_rev                0.028604830 0.1255837825 6.574229e-03  0.028065125

importance(fit_C) # importance of each predictor
varImpPlot(fit_C)

#IncNodePurity
#Amazon                     0.3193505
#GDP                        1.1923913
#AvgPropTaxPerCap           0.1790770
#PopDensity                 0.5275938
#MedianIncome               0.5510627
#PropPricetoIncomeRatio     0.1490439
#AvgSchoolRep               0.1740310
#CorpTaxMax                 0.1325217
#GDP5Year                   0.1434509
#GDPperCap                  1.5770558
#PC1_rev                    0.2406962
#PC2_rev                    2.9902329
#PC3_rev                    0.2989060

#negative b\c high cost should get lower score
#unscaled_cost_score <-  -0.0012212756*combo$AvgStateTax + -0.0627961272*combo$AvgPropTaxPerCap + -0.3140204253*combo$MedianIncome + 
#  0.0988799276*combo$PovertyRate + -0.0166692394*combo$HealthCareIndex + 0.0296597584*combo$PurchasingPower + -0.0067434924*combo$CPIIndex +
#  -0.1863926246*combo$RentIndex + -0.0072617969*combo$PropPricetoIncomeRatio + -0.0161252065*combo$WCIndex + -0.0181598674*combo$CorpTaxMax +
#  -0.0004284582*combo$CostLivingComposit + -0.0170793878*combo$SingEmpCost + -0.0167659913*combo$CBRE_Costofliving + 
#  -0.0480421580*combo$CBRE_Rent_to_Tech_WageRatio + -0.0127206338*combo$CBRE_WageRelativetoUSAvg + 0.1185163858*combo$CBRE_Office_Vacancy_Rate_13 + 
#  -0.0285172437*combo$CBRE_AvgAnn_TechWage

#negative b\c high cost should get lower score
unscaled_cost_score <- -1*combo$Amazon*0.0879120879120879+-1*combo$GDP*0.021978021978022+-1*combo$AvgPropTaxPerCap*0.164835164835165+-1*combo$PopDensity*0.131868131868132+-1*combo$MedianIncome*0.0879120879120879+-1*combo$PropPricetoIncomeRatio*0.21978021978022+-1*combo$AvgSchoolRep*0.010989010989011+-1*combo$CorpTaxMax*0.164835164835165+-1*combo$GDP5Year*0.0549450549450549+-1*combo$GDPperCap*0.0549450549450549
hist(unscaled_cost_score)

#make normal
norm_cost_score <- bestNormalize(unscaled_cost_score)
norm_cost_score <- predict(norm_cost_score)
hist(norm_cost_score)

#rescale this between .05 and .95
cost_score <- (.95-.05)/(max(norm_cost_score)-min(norm_cost_score))*(norm_cost_score-max(norm_cost_score))+.95
cost_score <- cost_score*100
hist(cost_score)


#####VARIABLE IMPORTANCE - QUALITY ##############
# Bootstrap Measures of Relative Importance (100 samples) 
boot <- boot.relimp(quality2_fit, b = 100, type = c("lmg", "last", "first", "pratt"), rank = TRUE, diff = TRUE, rela = TRUE)
booteval.relimp(boot) # print result
plot(booteval.relimp(boot,sort=TRUE), cex.axis = .1) # plot result
booteval.relimp(boot,sort=TRUE)

#give 25 percent to quality of life index, 10% to activity, 10% to Pollution, 5% to traffic, and 5% to price to income
#lmg         last        first        pratt
#Amazon                 0.242121759 0.2203505480 0.2375144456  0.257413906
#PopDensity             0.158447983 0.2379703316 0.1229889360  0.175250739
#MedianIncome           0.346397300 0.4170127111 0.3006942724  0.527643062
#PropPricetoIncomeRatio 0.012196318 0.0330288595 0.0072912510 -0.016400681
#PovertyRate            0.090832777 0.0011366247 0.1110100888  0.014286536
#HealthCareIndex        0.014303779 0.0004450530 0.0244165189  0.003242028
#CostLivingComposit     0.001723664 0.0010770248 0.0006513316  0.000812986
#landusescore           0.039710236 0.0004024768 0.0737166176 -0.006382351
#activityscore          0.022033849 0.0129110360 0.0345692197  0.020982528
#streetscore            0.047239269 0.0008268614 0.0848881100  0.009982298
#PC1_rev                0.024993066 0.0748384732 0.0022592086  0.013168949

importance(fit_Q) # importance of each predictor
varImpPlot(fit_Q)

#IncNodePurity
#Amazon                     1.1204930
#PopDensity                 1.5256072
#MedianIncome               1.8078116
#PropPricetoIncomeRatio     0.3171439
#PovertyRate                0.9526470
#HealthCareIndex            0.3609183
#CostLivingComposit         0.2341104
#landusescore               0.5014222
#activityscore              0.3940277
#streetscore                0.4835868
#PC1_rev                    0.5355910


#unscaled_quality_score <- 0.1685269613*combo$PovertyRate + 0.0325338054*combo$HealthCareIndex + 0.0050658113*combo$CrimeIndex + 0.0385033805*combo$PurchasingPower +
#  0.1101468110*combo$PollutionIndex + 0.0545183038*combo$TrafficIndex + 0.2601026410*combo$QualityofLifeIndex + 
#  0.0696343548*combo$PropPricetoIncomeRatio+ 0.0021069460*combo$CO2Index+ 0.0007701137*combo$CostLivingComposit +
#  0.0564075905*combo$MaleFemaleRatio + 0.0521098376*combo$GDPperCap + 0.0047993471*combo$PC2_age+ 0.0083087862*combo$PC3_age +
#  0.1364653098*combo$activityscore

unscaled_quality_score <- combo$Amazon*0.109392755155441+combo$PopDensity*0.109392755155441+combo$MedianIncome*0.109392755155441+
  -1*combo$PropPricetoIncomeRatio*0.0801613363828234+-1*combo$PovertyRate*0.141398306146358+
  -1*combo$HealthCareIndex*0.0836061463608224+-1*combo$CostLivingComposit*0.0711374892832017+
  combo$landusescore*0.100268695226161+combo$activityscore*0.091627989100963+combo$streetscore*0.103621772033348+
hist(unscaled_quality_score)

#make normal
norm_quality_score <- bestNormalize(unscaled_quality_score)
norm_quality_score <- predict(norm_quality_score)
hist(norm_quality_score)

#rescale this between .05 and .95
quality_score <- (.95-.05)/(max(norm_quality_score)-min(norm_quality_score))*(norm_quality_score-max(norm_quality_score))+.95
quality_score <- quality_score*100
hist(quality_score)


#######################FINAL STEPS####################

##Make the Final table#####
scoretable <- cbind.data.frame(combo[,2], talent_score, connect_score, cost_score, quality_score)
scoretable <- rename.vars(scoretable, from = "combo[, 2]", to = "MSA")

#######Now use all 4 scores of the 4 categories to get the overall weight which will be default that the user can play with 
model <- lm(combo$TecRev ~ talent_score + connect_score + cost_score + quality_score, data = scoretable)
summary(model)
#Multiple R-squared:  0.6672,	Adjusted R-squared:  0.6634

boot <- boot.relimp(model, b = 100, type = c("lmg", "last", "first", "pratt"), rank = TRUE, diff = TRUE, rela = TRUE)
booteval.relimp(boot) # print result
plot(booteval.relimp(boot,sort=TRUE), cex.axis = .1) # plot result
booteval.relimp(boot,sort=TRUE)

#lmg        last      first      pratt
#talent_score  0.22960705 0.007838178 0.28495332 0.04855220
#connect_score 0.40208847 0.408510403 0.40170845 0.45472952
#cost_score    0.31200085 0.459907729 0.28659413 0.43737672
#quality_score 0.05630363 0.123743689 0.02674411 0.05934156

#TRY RF MODEL
fit <- randomForest(combo$TecRev ~ talent_score + connect_score + cost_score + quality_score, data = scoretable)
print(fit) # view results 
#% Var explained: 79.13
importance(fit) # importance of each predictor
varImpPlot(fit)

#IncNodePurity
#talent_score      2.0810199
#connect_score     2.7032600
#cost_score        2.8247967
#quality_score     0.8091955

#these can be changed by the user of the dashboard if possible
wtalent <- .30
wconnect <- .25
wcost <- .30
wquality <- .15 

#finalscore <- wtalent*scoretable$talent_score + wconnect*scoretable$connect_score + wcost*scoretable$cost_score + wquality*scoretable$quality_score
unscaled_finalscore <- scoretable$talent_score*0.30+scoretable$connect_score*0.25+scoretable$cost_score*0.30+scoretable$quality_score*0.15
hist(unscaled_finalscore)

#rescale this between .05 and .95
finalscore <- (.95-.05)/(max(unscaled_finalscore)-min(unscaled_finalscore))*(unscaled_finalscore-max(unscaled_finalscore))+.95
scoretable$finalscore <- finalscore*100


#FINAL OUTPUT
write.table(scoretable, "scoretable.csv", sep=",")

##########VISUALS##############
png("talentscore.png")
par(mfrow=c(1,3))
talent <- hist(unscaled_talent_score, main = paste("Talent Unscaled"))
talent <- hist(norm_talent_score, main = paste("Talent Normalized"))
talent <- hist(talent_score, main = paste("Talent Norm & Scaled"))
print(talent)
dev.off()

png("connectscore.png")
par(mfrow=c(1,3))
connect <- hist(unscaled_connect_score, main = paste("Connectivity Unscaled"))
connect <- hist(norm_connect_score, main = paste("Connectivity Normalized"))
connect <- hist(connect_score, main = paste("Connectivity Norm & Scaled"))
print(connect)
dev.off()

png("costscore.png")
par(mfrow=c(1,3))
cost <- hist(unscaled_cost_score, main = paste("Cost Unscaled"))
cost <- hist(norm_cost_score, main = paste("Cost Normalized"))
cost <- hist(cost_score, main = paste("Cost Norm & Scaled"))
print(cost)
dev.off()

png("qualityscore.png")
par(mfrow=c(1,3))
quality <- hist(unscaled_quality_score, main = paste("Quality of Life Unscaled"))
quality <- hist(norm_quality_score, main = paste("Quality of Life Normalized"))
quality <- hist(quality_score, main = paste("Quality of Life Norm & Scaled"))
print(quality)
dev.off()

#bringing it all into one table
png("finalscore.png")
par(mfrow=c(1,1))
final <- hist(scoretable$finalscore, main = paste("Final Scores with Default Weights"))
print(final)
dev.off()



###VALIDATEION OF SCORES AGAINST CBRE
validation <- merge(dat, scoretable, by="MSA")
validation <- validation[-c(2:58,79:131)]
validation <- validation[complete.cases(validation), ]


png("CRBEcor_talent.png")
val_talent <- validation[c(3,7,9,17,22)]
M <- cor(val_talent)
CRBEcor_talent <- corrplot(M, type="upper", order="hclust", tl.col="black", tl.srt=45, tl.cex = .8)
print(CRBEcor_talent)
dev.off()

png("CRBEcor_connect.png")
val_connect <- validation[c(2,6,12,20,23)]
M <- cor(val_connect)
CRBEcor_connect <- corrplot(M, type="upper", order="hclust", tl.col="black", tl.srt=45, tl.cex = .8)
print(CRBEcor_connect)
dev.off()

png("CRBEcor_cost.png")
val_cost <- validation[c(2,10,11,13,14,15,16,18,19,21,24)]
M <- cor(val_cost)
CRBEcor_cost <- corrplot(M, type="upper", order="hclust", tl.col="black", tl.srt=45, tl.cex = .7)
print(CRBEcor_cost)
dev.off()

png("CRBEcor_quality.png")
val_quality <- validation[c(2,14,16,18,25)]
M <- cor(val_quality)
CRBEcor_quality <- corrplot(M, type="upper", order="hclust", tl.col="black", tl.srt=45, tl.cex = .8)
print(CRBEcor_quality)
dev.off()
