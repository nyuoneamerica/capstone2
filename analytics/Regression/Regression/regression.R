#Regression for variable influence
setwd("H:/NYU BACKUP/Capstone/Regression")
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
library("xlsx")
####Note: sensativity testing done using finalscore as target.  Many of the same variables proved to be predicitve, especially the PC1 and PC2 components, that's good!

#Note this is a repeat of preprocessing stuff but included here for convienence:
#LOAD DATA
dat <- read_csv("data_regression.csv")
data <-  dat[-c(59:78, 99, 120, 22, 19, 18)]  #not using CBRE data, qualityoflife, cpi, purchasingpower
imputed_dat <- mice::mice(data, m=1, maxit = 1, method = 'cart', seed = 500) #did more for preprocessing but selected 1 anyway so do this to same computational time
dat_c <- mice::complete(imputed_dat,1)
#########END PREPROCESSING ###########

#Remove the dollars by industry b\c that's just a linear combo of target, keep percentage (also remove perofrev0 b\c that's just 1.0 for all)
model_dat <-  dat_c[-c(57:75)]
#Remove categorical variables (MSA, State, Lat, Long), keep numerical and factors
model_dat <- model_dat[5:87]

##################Correlation Matrix#################
#just numerical values for cor matrix
png("cormatrix.png")
numerical <- model_dat[-c(3,75,76)]
M <- cor(numerical)

corrplot(M, type="upper", order="hclust", tl.col="black", tl.srt=45, tl.cex = .28)
print(M)
dev.off()


M <- as.data.frame(M)
write.xlsx(M, "cormatix.xlsx")

###########################################################
#check class of the variables 
lapply(model_dat,class)
#convert Tech_Hub to a factor, keep num airlines as integer and all else numeric
model_dat[1:74] <- lapply(model_dat[1:74], as.numeric)
model_dat$Tech_Hub <- as.factor(model_dat$Tech_Hub)
model_dat$Amazon <- as.factor(model_dat$Amazon)
model_dat$TopGrad <- as.factor(model_dat$TopGrad)
lapply(model_dat,class)

#########MODELING PRINT FUNCTION##############
#supply fxn with title of model (in quotes) and model name
writeOutput.F <- function(output.title, modelname) {
  sink(file="modeloutputs.txt", append=TRUE) #creates an empty file; if TRUE, it will append to the file
  print("##########################", row.names=FALSE)
  print(output.title, row.names=FALSE)
  print("##########################",  row.names=FALSE)
  summary(modelname) #write summary of the model to text file 
}

###############################
#Mult Linear Model - ALL VARIABLES AND SEE WHAT'S UP
#https://www.statmethods.net/advstats/cart.html

sink <- lm(RevPerCap0 ~ ., data = model_dat)
summary(sink)
writeOutput.F("Multiple Linear Regression Kitchen Sink", sink)
closeAllConnections() #close all connections and restore all sink diversi
#Multiple R-squared:  0.7781,	Adjusted R-squared:  0.7115

#Look at residuals vs fitted values
png("fitres_sink.png")
reg.res=resid(sink)
reg.fit=fitted(sink)
fitres_sink <- plot(reg.fit,reg.res, main = "Residuals vs. Fitted Values for Kitchen Sink Model") #looks like there are outliers
print(fitres_sink)
dev.off()

#nonlinear pattern -> take log of target
hist(model_dat$RevPerCap0) #yea, this is skewed
#look at all variables
png("variabledistribution.png")
variabledistribution <- model_dat %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  theme(text=element_text(size=5, family="RobotoBold")) +
  geom_histogram()
print(variabledistribution)
dev.off()

#log transformation
nontransdata <- model_dat[-c(1,2,6,7,8,18,21,22,29,33,35:47,49,52,53:71,72,73,74,79)] 
transdata <- model_dat[c(1,2,6,7,8,18,21,22,29,33,35:47,49,52,53:71,72,73,74,79)]
transdata[transdata == 0] <- .0001
transdata <- log10(transdata)
model_data <- cbind(transdata, nontransdata )


#now let's look at the histograms
png("variabledistributionafter.png")
variabledistributionafter <- model_data %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  theme(text=element_text(size=5, family="RobotoBold")) +
  geom_histogram()
print(variabledistributionafter)
dev.off()

#sink 2
sink <- lm(RevPerCap0 ~ ., data = model_data)
summary(sink)
writeOutput.F("Multiple Linear Regression Kitchen Sink After Log-Transform", sink)
closeAllConnections() 
#Multiple R-squared:  0.8853,	Adjusted R-squared:  0.8509

#Variable inflation investigation: want below 10 (or 5 if stricter)
vif(sink) #def some problems here, mostly with age dist and pop,popdensity,landarea
#Better...
png("fitres_sink_aftertransform.png")
reg.res=resid(sink)
reg.fit=fitted(sink)
fitres_sink_aftertransform <- plot(reg.fit,reg.res, main = "Residuals vs. Fitted Values for Kitchen Sink After Log-Trans")
print(fitres_sink_aftertransform)
dev.off()
#Let's do ridge regression for variable selection
X <- model_data[-c(25)] #removing target to make predictor matrix
X <- as.matrix(X)  

fit <- glmnet(X, model_data$RevPerCap0) 
png("ridgeregsink.png")
ridgeregsink <- plot(fit, xvar = "lambda", label = TRUE)
print(ridgeregsink)
dev.off()
#The picture shows that the larger the alpha the more of the coefficients become zero. 
#On the top horizontal axis R shows the number of non-zero coefficients. 
#On the bottom horizontal axis the logarithm of alpha is shown. 
#The colored curves show the value of the different coefficients.
print(fit)
#[31,] 25 0.78820 7.377e-03 -> select a bit above 
coef(fit, s=7.378e-03 )
#Results -> Looks like all the revenue stuff if important!  Let's PCA that
#(Intercept)             1.488220e+00
#NumAirline              9.122145e-03
#TaxableWageBase         6.379546e-03
#PerofRev11             -6.324011e-03
#PerofRev22              2.123504e-03
#PerofRev44_45          -9.156522e-02
#PerofRev51             -1.712036e-02
#PerofRev52              7.025785e-03
#PerofRev53             -3.626712e-02
#PerofRev54             -2.050024e-02
#PerofRev62             -2.849507e-01
#PerofRev71              5.354693e-03
#PerofRev72             -1.748010e-01
#PerofRev55              4.302406e-03
#AvgPropTaxPerCap        1.058509e-05
#PovertyRate            -4.464812e-01
#Electricity            -5.800223e-05
#CorpTaxMax              1.838152e-01
#WCIndex                -4.472801e-03
#GDP5Year               -1.233853e-01
#MaleFemaleRatio        -4.099508e-03
#MedianAge               3.995853e-04
#GDPperCap               3.686909e+00
#TopGrad                 1.553890e-03
#landusescore            5.046025e-05
#activityscore           6.910573e-05

#PerofRev -> there is a lot here, let's PCA these components
#Drop PerofRev0 b\c that's just the sum
PerofRev <- model_data[c(27:45)]
pca1 = prcomp(PerofRev, scale. = TRUE)
summary(pca1)
png("pcarev1.png")
pcarev1 <- ggbiplot(pca1)
print(pcarev1)
dev.off()

#let's group these MSAs up into tech-hub or not
TH <- model_data[c(49)]
png("pcarev_techhub.png")
pcarev_techhub <- ggbiplot(pca1, ellipse=TRUE, groups=TH[,1])
print(pcarev_techhub)
dev.off()

#let's group these MSAs up into Amazon pitch or not
AM <- model_data[c(77)]
png("pcarev_amazon.png")
pcarev_amazon <- ggbiplot(pca1, ellipse=TRUE, groups=AM[,1])
print(pcarev_amazon)
dev.off()

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
model <- lm(RevPerCap0 ~ ., data = model_data)
summary(model)

#Multiple R-squared:  0.849,	Adjusted R-squared:  0.8145
#Let's do ridge regression again for variable selection
X <- model_data[-c(28)]
X <- as.matrix(X)  
fit <- glmnet(X, model_data$RevPerCap0) 
png("ridgeregafterpca.png")
ridgeregafterpca <- plot(fit, xvar = "lambda", label = TRUE)
print(ridgeregafterpca)
dev.off()

#[30,] 12 0.7558 8.077e-03 - select a bit above
coef(fit, s=8.078e-03)
#(Intercept)             2.141026e+00
#PC1                    -3.830211e-02
#PC2                     4.064659e-02
#NumAirline              6.524268e-03
#GDP                     1.459783e-02
#PovertyRate            -5.761459e-01
#CorpTaxMax              1.532226e-01
#GDP5Year               -3.110169e-02
#MaleFemaleRatio        -3.809430e-03
#GDPperCap               4.046149e+00


model <- lm(RevPerCap0 ~ PC1 + PC2  + NumAirline + GDP + PovertyRate + CorpTaxMax + GDP5Year + 
              MaleFemaleRatio + GDPperCap , data = model_data)
summary(model)
writeOutput.F("Multiple Linear Regression Final", model)
closeAllConnections()
#Multiple R-squared:   0.77,	Adjusted R-squared:  0.764


# Assessing Outliers
outlierTest(model) # Bonferonni p-value for most extreme obs
qqPlot(model, main="QQ Plot") #qq plot for studentized resid 
leveragePlots(model) # leverage plots
#183, 211, 141, 27
influencePlot(model, id.method="identify", main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )
#Yup REMOVE 183, 211, 141, 27, and 155


#Remove outliers and take transformation of y
#y power transformation
nontransdata <- model_data[-c(28)]
transdata <- model_data[c(28)]
transdata <- transdata^1.332857
model_data <- cbind(transdata, nontransdata)
#remove outlier rows 
MSAlookup <- cbind(dat_c[,1], model_data)
#removed 183: Lebanan, PA | 211: Midland, TX | 141: Hinesville, GA | 27: Bay City, MI | 155: Jacksonville, NC
#midland rev is driven by Mining, Quarrying, and Oil and Gas Extraction.
#lebanan rev is driven by Wholesale Trade 
#hinesville rev is driven by Manufacturing
#bay city rev is driven by  Wholesale Trade & Manufacturing
#Jacksonville NC is driven by Retail Trade

model_data2 <- model_data[-c(183, 211, 141, 27, 155), ] 
#test to note remove anything
#model_data2 <- model_data
model <- lm(RevPerCap0 ~ PC1 + PC2  + NumAirline + GDP + PovertyRate +CorpTaxMax + GDP5Year + 
              MaleFemaleRatio + GDPperCap, data = model_data2)
summary(model)
writeOutput.F("Multiple Linear Regression Final", model)
closeAllConnections()
#Multiple R-squared:  0.828,	Adjusted R-squared:  0.8235
#not removed: Multiple R-squared:  0.7724,	Adjusted R-squared:  0.7651

#Remove non predictive 
model <- lm(RevPerCap0 ~ PC1 + PC2  + NumAirline +  PovertyRate + CorpTaxMax + GDP5Year + 
              MaleFemaleRatio + MedianAge + GDPperCap, data = model_data2)
summary(model)
#Multiple R-squared:  0.8307,	Adjusted R-squared:  0.8263 
#not remoevd: Multiple R-squared:  0.7681,	Adjusted R-squared:  0.7627 & median age is non predictive now

#NORMALITY
png("normalqq.png")
normalqq <- qqPlot(model, main="QQ Plot")
print(normalqq)
dev.off()
# distribution of studentized residuals
png("normalresiduals.png")
sresid <- studres(model) 
normalresiduals <- hist(sresid, freq=FALSE, 
                        main="Distribution of Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=40) 
yfit<-dnorm(xfit) 
lines(xfit, yfit)
print(normalresiduals)
dev.off()

# Evaluate Nonlinearity
# component + residual plot 
png("linear.png")
# component + residual plot 
linear <- crPlots(model)
# Ceres plots 
#ceresPlots(model)
print(linear)
dev.off()

# Test for Autocorrelated Errors
durbinWatsonTest(model)
#The Durbin Watson statistic is always between 1 and 4, values close to 2 indicate that there is no autocorrelation.
#1.921184 

# Evaluate homoscedasticity
# non-constant error variance test
ncvTest(model)
# plot studentized residuals vs. fitted values 
png("homoscedasticity.png")
homoscedasticity <- spreadLevelPlot(model)
print(homoscedasticity)
dev.off()


###################Try another model form...glm with gamma#################  Not better
#model <- glm(RevPerCap0 ~ PC1 + PC2  + NumAirline +  PovertyRate + CorpTaxMax + GDP5Year + 
#              MaleFemaleRatio + MedianAge + GDPperCap, data = model_data2, family = Gamma)
#summary(model)
#model <- glm(RevPerCap0 ~ PC1 + PC2  + NumAirline +  PovertyRate + GDP5Year + 
#               MaleFemaleRatio  + GDPperCap, data = model_data2, family = Gamma)
#summary(model)
#model <- glm(RevPerCap0 ~ PC1 + PC2  + NumAirline +  PovertyRate + GDP5Year + 
#               MaleFemaleRatio  + GDPperCap, data = model_data2, family = inverse.gaussian)
#summary(model)
#######################


#################VALIDATION##################

#MODEL VALIDATION
#Cross Validation
# K-fold cross-validation
cv.lm(model_data2, model, m=10) # 10 fold cross-validation
#Sum the MSE for each fold, divide by the number of observations, and take the square root to get the cross-validated standard error of estimate.


#RELATIVE IMPORTANCE
#Relative Importance - Calculate Relative Importance for Each Predictor
calc.relimp(model, type=c("lmg","last","first","pratt"), rela=TRUE)

# Bootstrap Measures of Relative Importance (1000 samples) 
boot <- boot.relimp(model, b = 1000, type = c("lmg", "last", "first", "pratt"), rank = TRUE, diff = TRUE, rela = TRUE)
booteval.relimp(boot) # print result
plot(booteval.relimp(boot,sort=TRUE), cex.axis = .1, cex = .5) # plot result


#Finally, fitted vs actual

plot(predict(model), model_data2$RevPerCap0, xlab="predicted",ylab="actual")
abline(a=0,b=1)

#untransformed
y_pred <- predict(model)^(1/2.221164)
y_pred <- 10^y_pred
y_act <- model_data2$RevPerCap0^(1/2.221164)
y_act <- 10^y_act
png("fitactual.png")
fitactual <- plot(y_pred, y_act, xlab="predicted",ylab="actual", main = "Actual vs. Fitted")
abline(a=0,b=1)
print(fitactual)
dev.off()

# Sector	Description
#11	Agriculture, Forestry, Fishing and Hunting
#21	Mining, Quarrying, and Oil and Gas Extraction
#22	Utilities
#23	Construction
#31-33	Manufacturing
#42	Wholesale Trade
#44-45	Retail Trade
#48-49	Transportation and Warehousing
#51	Information
#52	Finance and Insurance
#53	Real Estate and Rental and Leasing
#54	Professional, Scientific, and Technical Services
#55	Management of Companies and Enterprises
#56	Administrative and Support and Waste Management and Remediation Services
#61	Educational Services
#62	Health Care and Social Assistance
#71	Arts, Entertainment, and Recreation
#72	Accommodation and Food Services
#81	Other Services (except Public Administration)
#92	Public Administration


#Moral -> the breakdown of what industry is in your MSA is very important.  Attract the right industry.
#So we'll do a comparison of the most profitable breakdown vs. what the city has
#Also reduce poverty rates, crime, and try to get your city to be more connected. 
