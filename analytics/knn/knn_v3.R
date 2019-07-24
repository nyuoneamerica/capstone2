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
#library(dissUtils) # NEW LIBRARY
#library(dplyr) # NEW LIBRARY

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


data_model <- read_csv("scoretable_final_modified.csv")
#dim(dat)
tc <- data <- read_csv("data_score_modified.csv")
data_model <- merge(tc, data_model, by = 'MSA')
#view(data_model)

data_model <- data_model[-c(2:6, 8:39, 41:131)] #keeping estab tech hub and population
data_model <- data_model[-c(4,9)]


#Normlize the variables 
# function 
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }
# apply function 
norm1 <- as.data.frame(lapply(data_model[,3:7], normalize))
#view(norm1)


df1<- data_model[,1:2]
#view(df1)
combo <- cbind(df1, norm1)
#view(combo)
names(combo)[names(combo)=="df1"] <- "TechHub" #strange, this guy didn't work but whatever
names(combo)[names(combo)=="Poplutation"] <- "Population"
data_model <- combo

#these are paramters
wtalent <- .1
wconnect <- .1
wcost <- .7
wquality <- .1

#these are fixed
wpop <- .3
wtech <- .2


data_model$Tech_Hub <- as.numeric(data_model$Tech_Hub)
data_model$talent_score <- data_model$talent_score*wtalent
data_model$connect_score <- data_model$connect_score*wconnect
data_model$cost_score <- data_model$cost_score*wcost
data_model$quality_score <- data_model$quality_score*wquality
data_model$Population <- data_model$Population*wpop
data_model$Tech_Hub <- data_model$Tech_Hub*wtech


#########################################

xnew <- data_model[-c(1)]
y <- data_model[c(7)]
x <- xnew


library(FNN)
indices<-get.knn(xnew, k=10, algorithm=c("brute"))

#The next step is to merge the data into one table.
#In order to do that we need to create a new table with the neighbours indexes

index_table <- as.data.frame(indices$nn.index,colnames(c("n1","n2","n3","n4","n5", "n6", "n7", "n8", "n9", "n10")))

#After that we just need to replace the index for the MSA name so we know which
data_model$First <- data_model$MSA[index_table$V1]
data_model$Second <- data_model$MSA[index_table$V2]
data_model$Third <- data_model$MSA[index_table$V3]
data_model$Fourth <- data_model$MSA[index_table$V4]
data_model$Fifth <- data_model$MSA[index_table$V5]
data_model$Sixth <- data_model$MSA[index_table$V6]
data_model$Seventh <- data_model$MSA[index_table$V7]
data_model$Eighth <- data_model$MSA[index_table$V8]
data_model$Ninth <- data_model$MSA[index_table$V9]
data_model$Tenth <- data_model$MSA[index_table$V10]

#can view data_model output for reasonability
#drop extras and call near
near <- data_model[-c(2,3,4,5,6,7)]
                 
                 

###export knn results out to a csv

#dim(near)
#view(near)
#write.table(near, "knn_default_wt.csv", sep=",")
#write.table(near, "knn_cost_wt_7.csv", sep=",")

#######

######## print igraph

#links2 <- read.csv("cost_edges.csv", header=F)
#nodes2 <- read.csv("cost_nodes.csv", header=F, as.is=T)

#library("igraph")
#net <- graph_from_data_frame(d=links2, vertices=nodes2, directed=T) 
#net <- simplify(net, remove.multiple = F, remove.loops = T) 

#inc.edges <- incident(net,  V(net)[net$from=="Denver Aurora Lakewood, CO"], mode="all")

#graph_attr(net, "layout") <- layout.random
#plot(net, edge.arrow.size=.3, edge.color="orange",
#     vertex.color="orange", vertex.frame.color="#ffffff",
#     vertex.label=V(net)$to, vertex.label.color="black") 

###########

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



