#KNN algorithm for nearest neighbor state given states specific information

library(shiny)
library(readr)
library(mice)
#library(VIM)
library(ggmap)
library(ggplot2)
library(tidyverse)
library(rpart)
library(class)
library(gmodels)
library(FNN)
library(rjson)

################# ADD 4 parameters like earlier script . previously we have 16 now we have only 4
args = commandArgs(trailingOnly = TRUE)

outputfile="/home/ubuntu/cityamerica_aws/Project/CityApp/output.json"
# test if there is at least one argument: if not, return an error
if (length(args)<5) {
  print("arguments 0")
}   else   {
  #msa_selected <- 'Abilene-TX'
  msa_selected = args[1]
  
  wtalent = args[2]
  
  wconnect = args[3]
  
  wcost = args[4]
  
  wquality = args [5]
  
  print("All 5 arguments taken ")
}

wtalent <- as.numeric( wtalent)
wconnect <- as.numeric( wconnect)
wcost <- as.numeric( wcost)
wquality <- as.numeric( wquality)


print("msa_selected is as below")

print("Arg 1")

print(msa_selected)

print("Arg 2")

print(wtalent)

print("Arg 3")

print(wconnect)

print("Arg 4")

print(wcost)

print("Arg 5")

print(wquality)

#setwd("H:/NYU BACKUP/Capstone/Knn")


#setwd("H:/NYU BACKUP/Capstone/Knn")


data_model <- read_csv("CityApp/scoretable_final_modified.csv")
#dim(dat)
tc <- data <- read_csv("CityApp/data_score_modified.csv")
data_model <- merge(tc, data_model, by = 'MSA')
data_model <- data_model[-c(1:6,8:131)]
data_model <- data_model[-c(2)]

  
data_model$talent_score <- data_model$talent_score*wtalent
data_model$connect_score <- data_model$connect_score*wconnect
data_model$cost_score <- data_model$cost_score*wcost
data_model$quality_score <- data_model$quality_score*wquality

prc_train <- data_model[1:356,]
prc_test <- data_model[5:356,]
prc_train_labels <- data_model[1:356, 1]
prc_test_labels <- data_model[5:356, 1]
#model2 <- knn(train = prc_train, test = prc_test, cl = prc_train_labels, k=10)

#CrossTable(x=prc_test_labels, y=model2, prop.chisq = FALSE)
#kdist <- knn.dist(model2)
#view(kdist)

#refit
model2 <- knn(train = prc_train, test = prc_train, cl = prc_train_labels, k=11)



#Finding the nearest neighbors
indices <- attr(model2, "nn.index")

#append MSA
near <- cbind(tc[,1], indices) #MSA name and row number of the nearest neighbor
colnames(near)=c("MSA", "First", "Second", "Third", "Fourth","Fifth", "Sixth", "Seventh", "Eighth", "Ninth", "Tenth", "Eleventh")
#temp dataset of the row number and MSA name
temp <- cbind(seq(1, nrow(data), 1), tc[,1]) 
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
#Eleventh
near <- merge(near, temp, by.x = "Eleventh", by.y = "Row")
near <- near[, -1] 
colnames(near)[colnames(near)=="MSA.y"] <- "Eleventh"
colnames(near)[colnames(near)=="MSA.x"] <- "MSA"

#dim(near)
#view(near)
near <- near[-c(2)]
#view(near)
#write.table(near, "knn_yp_noSMOTE.csv", sep=",")


selectedmsa <- near[which(near$MSA == msa_selected ), ]
selectedmsa

selectedmsa$MSA = as.character(selectedmsa$MSA)
#selectedmsa$First = as.character(selectedmsa$First)
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



