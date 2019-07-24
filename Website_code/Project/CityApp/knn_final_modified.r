#KNN algorithm for nearest neighbor state given states specific information
args = commandArgs(trailingOnly = TRUE)

outputfile="output.json"
# test if there is at least one argument: if not, return an error
if (length(args)==0) {
  print("arguments 0")
} else if (length(args)==1) {
  print("arguments 1")
  msa_selected = args[1]
} else   {
  msa_selected = args[1] 
  
  var_Age2034 = as.numeric(args[2])
  
  var_AvgPropTaxPerCap=as.numeric(args[3])
  
  var_Bet10and14=as.numeric(args[4])
  
  var_Bet5and9=as.numeric(args[5])
  
  var_Bet65and74=as.numeric(args[6])
  
  var_LandArea=as.numeric(args[7])
  
  var_Bet25and34=as.numeric(args[8])
  
  var_Under5=as.numeric(args[9])
  
  var_CorpTaxMin=as.numeric(args[10])
  
  var_CorpTaxMax=as.numeric(args[11])
  
  var_CO2Index=as.numeric(args[12])
  
  var_TotEnrollment=as.numeric(args[13])
  
  var_PercentBach=as.numeric(args[14])
  
  var_LaborForceAnnGrowth=as.numeric(args[15])
  
  var_PropPricetoIncomeRatio=as.numeric(args[16])
  
  var_CostLivingComposit= as.numeric(args[17])
  
  var_NumAirline  = as.numeric(args[18])
  
  var_TrafficIndex  = as.numeric(args[19])
  
  var_GDPperCap  = as.numeric(args[20])
  
  var_GDP5Year  = as.numeric(args[21])
  
  var_MedianIncome  = as.numeric(args[22])
  
  var_CPIIndex  = as.numeric(args[23])
  
  print("All 23 arguments taken ")
}

print("msa_selected is as below")

print("Arg 1") 

print(msa_selected)

print("Arg 2")

print(var_Age2034)

print("Arg 3")

print(var_AvgPropTaxPerCap)

print("Arg 4")

print(var_Bet10and14)

print("Arg 5")

print(var_Bet5and9)

print("Arg 6")

print(var_Bet65and74)

print("Arg 7")

print(var_LandArea)

print("Arg 8")

print(var_Bet25and34)

print("Arg 9")

print(var_Under5)

print("Arg 10")

print(var_CorpTaxMin)

print("Arg 11")

print(var_CorpTaxMax)

print("Arg 12")

print(var_CO2Index)

print("Arg 13")

print(var_TotEnrollment)

print("Arg 14")

print(var_PercentBach)

print("Arg 15")

print(var_LaborForceAnnGrowth)

print("Arg 16")

print(var_PropPricetoIncomeRatio)

print("Arg 17")

print(var_CostLivingComposit)

print("Arg 18")

print(var_NumAirline)

print("Arg 19")

print(var_TrafficIndex)

print("Arg 20")

print(var_GDPperCap)

print("Arg 21")

print(var_GDP5Year)

print("Arg 22")

print(var_MedianIncome)

print("Arg 23")

print(var_CPIIndex)


#install.packages('shiny', repos='https://cran.rstudio.com/')
#install.packages(c("Rcpp", "readr"), repos='https://cran.rstudio.com/')
#install.packages("readr",repos="https://github.com/tidyverse/readr")
#Normalize 
#install.packages("remotes",repos='https://cran.rstudio.com/')
#library(remotes)
#install_github("https://github.com/tidyverse/readr")
#install.packages("tidyverse")
#sudo su - -c "R -e \"install.packages('http://s3-us-west-2.amazonaws.com/10x.files/code/cellrangerRkit-1.1.0.tar.gz', repos=NULL)\""
#sudo su - -c "R -e \"install.packages('http://stefvanbuuren.github.io/mice/', repos=NULL)\""
#install.packages('mice',lib='/home/bitnami/Project/CityApp/R',repo = "https://cran.revolutionanalytics.com/")
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }

#library(shiny)
#library(readr)
#library(devtools)
# load package w/o installing
#load_all('/home/bitnami/Project/CityApp/R')
#library(mice,lib='/home/bitnami/Project/CityApp/R/')
#library(VIM,lib='/home/bitnami/Project/CityApp/R/')
#library(ggmap,lib='/home/bitnami/Project/CityApp/R/')
#library(ggplot2,lib='/home/bitnami/Project/CityApp/R/')
#library(tidyverse,lib='/home/bitnami/Project/CityApp/R/')
#library(rpart,lib='/home/bitnami/Project/CityApp/R/')
#library(DMwR,lib='/home/bitnami/Project/CityApp/R/')
#library(caret,lib='/home/bitnami/Project/CityApp/R/')
#library(rjson,lib='/home/bitnami/Project/CityApp/R/')
#library(caret,lib='/home/bitnami/Project/CityApp/R/')
#install.packages("png",repos="https://cran.wu.ac.at")
#install.packages("RgoogleMaps",repos="http://rgooglemaps.r-forge.r-project.org/QuickTutorial.html")
#install.packages("ggmap",repos="https://cran.wu.ac.at")
#install.packages("tidyverse",repos='http://lib.stat.cmu.edu/R/CRAN/',dependencies = TRUE)
#install.packages("readr",repos="https://cran.wu.ac.at")
library(readr)
library(mice)
library(VIM)
#library(ggmap)
library(ggplot2)
#library(tidyverse)
library(rpart)
library(DMwR)
library(caret)
library(rjson)
library(caret)

dat <- read_csv("Data_final_knn.csv")
dat <- dat[1:352,]
# check 
#dim(dat)
#replace zeros with NAs for average school reputation and average school tuition?  Do not impute these variables?  Not sure what to do here...


#visualize missing values for written report
#mice_plot <- aggr(dat, col=c('navyblue','yellow'), numbers=TRUE, sortVars=TRUE, labels=names(dat), cex.axis=.7, gap=3, ylab=c("Missing data","Pattern"))

#dim(dat)

#IMPUTATION

#HealthCareIndex missing 135
#CrimeIndex missing 143
#PurchasingPower missing 259
#CPIIndex missing 259
#PollutionIndex missing 143
#TrafficIndex missing 159  
#QualityofLifeIndex missing 269
#RentIndex missing 138
#PropPricetoIncomeRatio missing 163
#Co2Index missing 160
#Electricity missing 6
#NatGas missing 118
#CorTaxMin missing 44
#CostLivingComposit missing 122
#AnnGrowthLabor needs help

#impute data using MICE, cannot use pmm method  https://www.kaggle.com/c/house-prices-advanced-regression-techniques/discussion/24586
#m  - Refers to number of imputed data sets
#maxit - Refers to no. of iterations taken to impute missing values
#method - Refers to method used in imputation. we used 'cart' (classification and regression trees)
imputed_dat <- mice(dat, m=1, maxit = 10, method = 'cart', seed = 500) #can increase maxit for final run
#extract the imputed dataset from the methood
dat_c <- mice::complete(imputed_dat,1)


#Decision Tree
set.seed(1234)
#https://www.statmethods.net/advstats/cart.html
dt <- rpart(Tech_Hub ~ NumAirline + GDP + AvgStateTax + AvgPropTaxPerCap + LandArea + PopDensity + 
              Age2034 + MedianIncome + PercentBach + PovertyRate + HealthCareIndex + CrimeIndex + PurchasingPower + CPIIndex
            + PollutionIndex + TrafficIndex + QualityofLifeIndex + RentIndex + PropPricetoIncomeRatio + CO2Index + NumTechConf
            + TotEnrollment + AvgSchoolRep + AvgTuition + Electricity + NatGas + CorpTaxMin + CorpTaxMax + CostLivingComposit
            + WCIndex + TaxableWageBase + SingEmpCost + FamilyEmpCost + GDP5Year + Poplutation + MaleFemaleRatio	+ Under5+ Bet5and9	
            + Bet10and14 + Bet15and19 + Bet20and24 + Bet25and34 + Bet35and44 + Bet45and54 + Bet55and59 + Bet60and64 + Bet65and74 + Bet75and84
            + AtorOver85 + MedianAge + Labor + LaborForceAnnGrowth + GDPperCap, 
            data = dat_c, method = "class")

#PRUNE TREE
#Use printcp( ) to examine the cross-validated error results, select the complexity parameter associated with minimum error, 
#and place it into the prune( ) function.  Here, I automatically select the complexity parameter associated with the smallest cross-validated error
dt$cptable[which.min(dt$cptable[,"xerror"]),"CP"]

#FOR REPORT, # create attractive postscript plot of tree 
#post(dt, file = "prunetree.ps", title = "Classification Tree for Tech Hub Prediction")



#variable importance

varImp(dt)
#Overall Results
#Age2034                 3.714881 - 12
#AvgPropTaxPerCap        2.286081 - 9
#Bet10and14             15.234578 - 44
#Bet25and34             16.100002 - 47
#Bet5and9               15.657222 - 43
#Bet65and74             16.096229 - 52
#CO2Index                2.314286 - 25
#CorpTaxMax              2.880952 - 33
#CorpTaxMin              7.058431 - 32
#CPIIndex                3.825108 - 19
#LaborForceAnnGrowth     3.143557 - 57
#LandArea                4.023810 - 10
#PurchasingPower         1.928571 - 18
#Under5                 15.657222 - 42
#NumAirline              5.000000 - 5
#MedianIncome            5.000000 - 13
#PercentBach             5.000000 - 14
#TrafficIndex            5.000000 - 21
#PropPricetoIncomeRatio  5.000000 - 24
#TotEnrollment           5.000000 - 27
#CostLivingComposit      5.000000 - 34
#GDP5Year                5.000000 - 39
#GDPperCap               5.000000 - 58
#Tech_Hub                         - 7



#PLUS WHAT JAMES WANTS - Give them all a 5
#Office Lease -> TBD
#% population with higher education -> Age2034 and PercentBach
#labor force growth rate -> in there
#Cost of Living -> add PropPricetoIncomeRatio, CostLivingComposit
# of colleges and universities within 50 miles -> add enrollment
# of daily connecting flights to big four hubs -> add NumAirline
#avg commute time -> TrafficIndex
#gdp per capita -> we have both GDP and Population, engineer this variable to display in final table
#gdp growth rate -> Add GDP5Year
#Market value by industry -> handle later
#Median Home price -> handled with cost of living
#median household income -> MedianIncome
#age distribution -> handled above
#Inflation -> CPIIndex
#property tax -> add avgproptaxpercap
#Corporate income tax -> add corp tax min and max

#use to help get column numbers 
#write.csv(dat_c, "dat_c.csv")

#create a subset with just the data variables we want:
data <- dat_c[c(7, 12, 9, 44, 47, 43, 52, 25, 33, 32, 19, 57, 10, 18, 42, 5, 13, 14, 21, 24, 27, 34, 39, 58)]  


#table(data$Tech_Hub)
#summary(data) 



data_n <- as.data.frame(lapply(data[3:ncol(data)], normalize))
data_model <- cbind(data[,1:2],data_n)

# print("model   1 ")
# #Varying the weight. To put more weight on one feature, you can multiply the given feature by a certain value.
# data_model$Age2034 <- data_model$Age2034*3.714881 
# print("model   2 ")
# data_model$AvgPropTaxPerCap <-data_model$AvgPropTaxPerCap*2.286081  
# print("model   3 ")
# data_model$Bet10and14 <- data_model$Bet10and14*15.234578 
# print("model  4 ")
# data_model$Bet5and9 <- data_model$Bet5and9*15.657222 
# print("model   5 ")
# data_model$Bet65and74 <- data_model$Bet65and74*16.096229 
# print("model   6 ")
# data_model$LandArea <- data_model$LandArea*4.023810 
# print("model   7 ")
# data_model$Net25and34 <- data_model$Bet25and34*16.100002 
# print("model   8 ")
# data_model$Under5 <- data_model$Under5*15.657222 
# print("model   9 ")
# data_model$CorpTaxMin <- data_model$CorpTaxMin*7.058431  
# print("model   10 ")
# data_model$CorpTaxMax <- data_model$CorpTaxMax*2.880952 
# print("model   11 ")
# data_model$CO2Index <- data_model$CO2Index*2.314286 
# print("model   12 ")
# data_model$TotEnrollment <- data_model$TotEnrollment*5
# print("model   13 ")
# data_model$PercentBach <- data_model$PercentBach*5 
# print("model   14 ")
# data_model$LaborForceAnnGrowth <- data_model$LaborForceAnnGrowth*3.143557  
# print("model   15 ")
# data_model$PropPricetoIncomeRatio <- data_model$PropPricetoIncomeRatio*5  
# print("model   16 ")
# data_model$CostLivingComposit <- data_model$CostLivingComposit*5  
# print("model   17 ")
# data_model$NumAirline <- data_model$NumAirline*5
# print("model   18 ")
# data_model$TrafficIndex <- data_model$TrafficIndex*5  
# print("model   19 ")
# data_model$GDPperCap <- data_model$GDPperCap*5
# print("model   20 ")
# data_model$GDP5Year <- data_model$GDP5Year*5  
# print("model   21 ")
# data_model$MedianIncome <- data_model$MedianIncome*5 
# print("model   22 ")
# data_model$CPIIndex <- data_model$CPIIndex*3.825108 
# print("model   23 ")
# #data_model$office <- data_model$office*5
# print("model is assigned")

print("model   1 ")
#Varying the weight. To put more weight on one feature, you can multiply the given feature by a certain value.
data_model$Age2034 <- data_model$Age2034*var_Age2034 
print("model   2 ")
data_model$AvgPropTaxPerCap <-data_model$AvgPropTaxPerCap*var_AvgPropTaxPerCap  
print("model   3 ")
data_model$Bet10and14 <- data_model$Bet10and14*var_Bet10and14 
print("model   4 ")
data_model$Bet5and9 <- data_model$Bet5and9*var_Bet5and9 
print("model   5 ")
data_model$Bet65and74 <- data_model$Bet65and74*var_Bet65and74 
print("model   6 ")
data_model$LandArea <- data_model$LandArea*var_LandArea 
print("model   7 ")
data_model$Net25and34 <- data_model$Bet25and34*var_Bet25and34
print("model   8 ")
data_model$Under5 <- data_model$Under5*var_Under5 
print("model   9 ")
data_model$CorpTaxMin <- data_model$CorpTaxMin*var_CorpTaxMin 
print("model   10 ")
data_model$CorpTaxMax <- data_model$CorpTaxMax*var_CorpTaxMax 
print("model   11 ")
data_model$CO2Index <- data_model$CO2Index*var_CO2Index
print("model   12 ")
data_model$TotEnrollment <- data_model$TotEnrollment*var_TotEnrollment
print("model   13 ")
data_model$PercentBach <- data_model$PercentBach*var_PercentBach 
print("model   14 ")
data_model$LaborForceAnnGrowth <- data_model$LaborForceAnnGrowth*var_LaborForceAnnGrowth
print("model   15 ")
data_model$PropPricetoIncomeRatio <- data_model$PropPricetoIncomeRatio*var_PropPricetoIncomeRatio  
print("model   16 ")
data_model$CostLivingComposit <- data_model$CostLivingComposit*var_CostLivingComposit  
print("model   17 ")
data_model$NumAirline <- data_model$NumAirline*var_NumAirline
print("model   18 ")
data_model$TrafficIndex <- data_model$TrafficIndex*var_TrafficIndex  
print("model   19 ")
data_model$GDPperCap <- data_model$GDPperCap*var_GDPperCap
print("model   20 ")
data_model$GDP5Year <- data_model$GDP5Year*var_GDP5Year  
print("model   21 ")
data_model$MedianIncome <- data_model$MedianIncome*var_MedianIncome 
print("model   22 ")
data_model$CPIIndex <- data_model$CPIIndex*var_CPIIndex 
print("model   23 ")
#data_model$office <- data_model$office*5
print("model is assigned")

#Creating training and test data set

set.seed(1234)
splitIndex <- createDataPartition(data_model$Tech_Hub, p = .70, 
                                  list = FALSE,
                                  times = 1)
trainSplit <- data_model[ splitIndex,]
testSplit <- data_model[-splitIndex,]

#SMOTE
trainSplit$Tech_Hub <- as.factor(trainSplit$Tech_Hub)
trainSplit <- SMOTE(Tech_Hub ~ ., trainSplit, perc.over = 100, perc.under=200)
trainSplit$Tech_Hub <- as.numeric(trainSplit$Tech_Hub)

prop.table(table(trainSplit$Tech_Hub))


#Implement knn
library(class)

sqrt(nrow(data_model)) #start with k = 19

knn_predict <- knn(train = trainSplit[,3:ncol(trainSplit)], test = testSplit[,3:ncol(trainSplit)], cl = trainSplit[,1], k=19)

table(knn_predict, testSplit[,1])
prop.table(table(knn_predict, testSplit[,1]))


#not very senative to k at all...
knn_predict4 <- knn(train = trainSplit[,3:ncol(trainSplit)], test = testSplit[,3:ncol(trainSplit)], cl = trainSplit[,1], k=4)
prop.table(table(knn_predict4, testSplit[,1]))
knn_predict8 <- knn(train = trainSplit[,3:ncol(trainSplit)], test = testSplit[,3:ncol(trainSplit)], cl = trainSplit[,1], k=8)
prop.table(table(knn_predict8, testSplit[,1]))
#k = 10 is what James wanted so we'll go with that
knn_predict12 <- knn(train = trainSplit[,3:ncol(trainSplit)], test = testSplit[,3:ncol(trainSplit)], cl = trainSplit[,1], k=12)
prop.table(table(knn_predict12, testSplit[,1]))
knn_predict16 <- knn(train = trainSplit[,3:ncol(trainSplit)], test = testSplit[,3:ncol(trainSplit)], cl = trainSplit[,1], k=16)
prop.table(table(knn_predict16, testSplit[,1]))
#install.packages("FNN",repos="https://cran.wu.ac.at")
library(FNN)
#Refit with all data; choose k = 10
data_model$Tech_Hub <- as.factor(data_model$Tech_Hub)
datamodelfinal <- SMOTE(Tech_Hub ~ ., data_model, perc.over = 100, perc.under=200)
#check class
# lapply(datamodelfinal,class)

#kvalue <- as.numeric(kvalue)
data_model$Tech_Hub <- as.numeric(data_model$Tech_Hub)
#training on smote data
knn_model_final <- knn(train = datamodelfinal[,3:ncol(trainSplit)], test = data_model[,3:ncol(trainSplit)], cl = datamodelfinal[,1], k=10, prob=TRUE)

#envoking
prop.table(table(knn_model_final, data_model[,1]))
# print(knn_model_final) - quick check

#Finding the nearest neighbors
indices <- attr(knn_model_final, "nn.index")

#append MSA
near <- cbind(dat_c[,1], indices) #MSA name and row number of the nearest neighbor
colnames(near)=c("MSA", "First", "Second", "Third", "Fourth","Fifth", "Sixth", "Seventh", "Eighth", "Ninth", "Tenth")
#temp dataset of the row number and MSA name
temp <- cbind(seq(1, nrow(data), 1), dat_c[,1]) 
colnames(temp)=c("Row", "MSA")

#First
near <- merge(near, temp, by.x = "First", by.y = "Row")
near <- near[, -1]   
colnames(near)[colnames(near)=="MSA.y"] <- "First"
colnames(near)[colnames(near)=="MSA.x"] <- "MSA"
#Second
near <- merge(near, temp, by.x = "Second", by.y = "Row")
near <- near[, -1]  
colnames(near)[colnames(near)=="MSA.y"] <- "Second"
colnames(near)[colnames(near)=="MSA.x"] <- "MSA"
#Third
near <- merge(near, temp, by.x = "Third", by.y = "Row")
near <- near[, -1] 
colnames(near)[colnames(near)=="MSA.y"] <- "Third"
colnames(near)[colnames(near)=="MSA.x"] <- "MSA"
#Fourth
near <- merge(near, temp, by.x = "Fourth", by.y = "Row")
near <- near[, -1] 
colnames(near)[colnames(near)=="MSA.y"] <- "Fourth"
colnames(near)[colnames(near)=="MSA.x"] <- "MSA"
#Fifth
near <- merge(near, temp, by.x = "Fifth", by.y = "Row")
near <- near[, -1] 
colnames(near)[colnames(near)=="MSA.y"] <- "Fifth"
colnames(near)[colnames(near)=="MSA.x"] <- "MSA"
#Sixth
near <- merge(near, temp, by.x = "Sixth", by.y = "Row")
near <- near[, -1] 
colnames(near)[colnames(near)=="MSA.y"] <- "Sixth"
colnames(near)[colnames(near)=="MSA.x"] <- "MSA"
#Seventh
near <- merge(near, temp, by.x = "Seventh", by.y = "Row")
near <- near[, -1] 
colnames(near)[colnames(near)=="MSA.y"] <- "Seventh"
colnames(near)[colnames(near)=="MSA.x"] <- "MSA"
#Eighth
near <- merge(near, temp, by.x = "Eighth", by.y = "Row")
near <- near[, -1] 
colnames(near)[colnames(near)=="MSA.y"] <- "Eighth"
colnames(near)[colnames(near)=="MSA.x"] <- "MSA"
#Ninth
near <- merge(near, temp, by.x = "Ninth", by.y = "Row")
near <- near[, -1] 
colnames(near)[colnames(near)=="MSA.y"] <- "Ninth"
colnames(near)[colnames(near)=="MSA.x"] <- "MSA"
#Tenth
near <- merge(near, temp, by.x = "Tenth", by.y = "Row")
near <- near[, -1] 
colnames(near)[colnames(near)=="MSA.y"] <- "Tenth"
colnames(near)[colnames(near)=="MSA.x"] <- "MSA"

selectedmsa <- near[which(near$MSA == msa_selected ), ]
#print(selectedmsa)


selectedmsa$MSA = as.character(selectedmsa$MSA)
selectedmsa$First = as.character(selectedmsa$First)
selectedmsa$Second = as.character(selectedmsa$Second)
selectedmsa$Third = as.character(selectedmsa$Third)
selectedmsa$Fourth = as.character(selectedmsa$Fourth)
selectedmsa$Fifth = as.character(selectedmsa$Fifth)
selectedmsa$Sixth = as.character(selectedmsa$Sixth)
selectedmsa$Seventh = as.character(selectedmsa$Seventh)
selectedmsa$Eighth = as.character(selectedmsa$Eighth)
selectedmsa$Ninth = as.character(selectedmsa$Ninth)

x <- toJSON(unname(split(selectedmsa, 1:nrow(selectedmsa))))
#Check its existence
if (file.exists(outputfile)) 
  #Delete file if it exists
  file.remove(outputfile)
write(x,outputfile)
x 
