# For those new to the topic of Polynomial Regression, here's a wonderful video of Dr. Andrew Ng from Stanford, explaining the topic. It might be helpful to watch this video and any other resources of your choice before proceeding with the problem.
# 
# Polynomial Regression
# 
# The Problem
# 
# Charlie wants to purchase office-space. He does a detailed survey of the offices and corporate complexes in the area, and tries to quantify a lot of factors, such as the distance of the offices from residential and other commercial areas, schools and workplaces; the reputation of the construction companies and builders involved in constructing the apartments; the distance of the offices from highways, freeways and important roads; the facilities around the office space and so on.
# 
# Each of these factors are quantified, normalized and mapped to values on a scale of 0 to 1. Charlie then makes a table. Each row in the table corresponds to Charlie's observations for a particular house. If Charlie has observed and noted F features, the row contains F values separated by a single space, followed by the office-space price in dollars/square-foot. If Charlie makes observations for H houses, his observation table has (F+1) columns and H rows, and a total of (F+1) * H entries.
# 
# Charlie does several such surveys and provides you with the tabulated data. At the end of these tables are some rows which have just F columns (the price per square foot is missing). Your task is to predict these prices. F can be any integer number between 1 and 5, both inclusive.
# 
# There is one important observation which Charlie has made.
# 
# The prices per square foot, are (approximately) a polynomial function of the features in the observation table. This polynomial always has an order less than 4
# 
# Input Format
# 
# The first line contains two space separated integers, F and N. Over here, F is the number of observed features. N is the number of rows for which features as well as price per square-foot have been noted.
# This is followed by a table having F+1 columns and N rows with each row in a new line and each column separated by a single space. The last column is the price per square foot.
# 
# The table is immediately followed by integer T followed by T rows containing F columns.
# 
# Constraints
# 
# 1 <= F <= 5
# 5 <= N <= 100
# 1 <= T <= 100
# 0 <= Price Per Square Foot <= 10^6 0 <= Factor Values <= 1
# 
# Output Format
# 
# T lines. Each line 'i' contains the predicted price for the 'i'th test case. 

#tonote: took me some 6 hours. :/ transformation of raw data into polynomial was a big pain.
#todo: understand diff between poly and polym
#todo: try to improve score a little bit more
library(data.table)
library(data.table)
input <- readLines("/home/nirzareevadgama/Downloads/predicting-office-space-price-testcases/input/input01.txt")
# input <- readLines(file("stdin"))
input <- strsplit(input," ")

nfeatures <- as.numeric(input[[1]][1])
nlines <- as.numeric(input[[1]][2])

dtData <- list()
for (ind in 2:(2+nlines-1)) {
  dtData <- append(dtData,input[ind])
}

#testdata
ntestlines <- as.numeric(input[nlines+2])

dtTest <- list()
for (ind in (nlines+3):(nlines+3+ntestlines-1)) {
  dtTest <- append(dtTest,input[ind])
}

#model 
dtData <- lapply(dtData,as.numeric)
dtData <- do.call(rbind,dtData)
dtData <- as.data.table(dtData)
# names(dtData) <- c("V1","V2","Output")

#plot to check
# plot(x = dtData[,V1],y = dtData[,Output])
# plot(x = dtData[,V2],y = dtData[,Output])

#transform inputs 
dtTrainFeatures <- dtData[,1:(ncol(dtData) - 1)]
dtTrainLabels <- as.data.table(dtData[,ncol(dtData),with=FALSE])

dtTest <- lapply(dtTest,as.numeric)
dtTest <- do.call(rbind,dtTest)
dtTest <- as.data.table(dtTest)

#will try it for order 2 & 3
acc <- vector()
powertermlist <- seq(from = 2,to = 3,by = 1)
for (powertermindex in powertermlist) {
  dtTrainFeaturesTransformed <- poly(as.matrix(dtTrainFeatures),degree = powertermindex,raw = T)
  dtTrainFeaturesTransformed <- as.data.table(dtTrainFeaturesTransformed)
  names(dtTrainFeaturesTransformed) <- make.names(names(dtTrainFeaturesTransformed))
  
  lformula <- paste0(names(dtTrainLabels),"~",paste(names(dtTrainFeaturesTransformed),collapse = "+"))
  polyregmodel <- lm(lformula,cbind(dtTrainFeaturesTransformed,dtTrainLabels))
  
  acc[powertermindex] <- summary(polyregmodel)[9]
}
accdiff <- max(unlist(acc)) - min(unlist(acc))/max(unlist(acc))
if (abs(accdiff) > 2) {
  finalpowerterm = powertermlist[which.max(unlist(acc))]
} else {
  finalpowerterm = powertermlist[which.min(unlist(acc))]
}
finalpowerterm <- powertermlist[which.max(unlist(acc))]
dtTrainFeaturesTransformed <- poly(as.matrix(dtTrainFeatures),degree = finalpowerterm,raw = T)
dtTrainFeaturesTransformed <- as.data.table(dtTrainFeaturesTransformed)
names(dtTrainFeaturesTransformed) <- make.names(names(dtTrainFeaturesTransformed))

lformula <- paste0(names(dtTrainLabels),"~",paste(names(dtTrainFeaturesTransformed),collapse = "+"))
polyregmodel <- lm(lformula,cbind(dtTrainFeaturesTransformed,dtTrainLabels))

dtTestFeaturesTransformed <- poly(as.matrix(dtTest),degree = finalpowerterm,raw = T)
dtTestFeaturesTransformed <- as.data.table(dtTestFeaturesTransformed)
names(dtTestFeaturesTransformed) <- make.names(names(dtTestFeaturesTransformed))

dtTestFeaturesTransformed[,"pred"] <- predict(polyregmodel,dtTestFeaturesTransformed)
# cat(round(dtTestFeaturesTransformed[,pred],2))

for (ind in 1:ntestlines) {
  write(round(dtTestFeaturesTransformed[ind,pred],2),stdout())
}

##logic2: 
#one equation with all terms
# paste0("V",(nfeatures+1),"~","(",paste(lapply(seq(1:nfeatures),function(x) {return(paste0("V",x))}),collapse = "+"),")^",PowerTermIndex)
# lFormula <- "V3~V1+V2+V1^2+V2^2+V1^3+V2^3"
# PolyRegModel <- lm(lFormula,data = dtTrain) #train on whole data
# dtTest[,"pred"] <-  predict(PolyRegModel,dtTest)
# 
# for (ind in 1:testingsamples) {
#   write(round(dtTest[ind,pred],2),stdout())
# }

#logic1: either one power or the other: 9.85 points

# lFormula <- paste0("V",nfeatures,"~",paste(lapply(seq(1:nfeatures),function(x) {return(paste0("V",x))}),collapse = "+"))

#get correlation between features
# cor(dtTrain[,V2],dtTrain[,V1]) #not a lot
# cor(dtTrain[,V1],dtTrain[,V3])
# cor(dtTrain[,V1]^2,dtTrain[,V3])
# cor(dtTrain[,V1]^3,dtTrain[,V3])
# cor(dtTrain[,V2],dtTrain[,V3])
# cor(dtTrain[,V2]^2,dtTrain[,V3])
# cor(dtTrain[,V2]^3,dtTrain[,V3])
# cor((dtTrain[,V1]+dtTrain[,V2])^2,dtTrain[,V3]) #best
# cor((dtTrain[,V1]+dtTrain[,V2])^3,dtTrain[,V3])

# PolyRegModel  <- loess(lFormula,data = dtTrain)

#split dtTrain in train and test
# trainingindices <- sample(seq(1:nrow(dtTrain)),size = 0.8*nrow(dtTrain))
# dtTrainingData <- dtTrain[trainingindices,]
# dtTestingData <- dtTrain[-trainingindices,]
# 
# Error <- vector()
# PowerTerm <- seq(from =2,to =3)
# for (PowerTermIndex in PowerTerm) {
#   lFormula <- paste0("V",(nfeatures+1),"~","(",paste(lapply(seq(1:nfeatures),function(x) {return(paste0("(V",x,"^",PowerTermIndex,")"))}),collapse = "+"),")")
#   PolyRegModel <- lm(lFormula,data = dtTrainingData)
#   dtTestingData[,paste0("pred",PowerTermIndex)] <-  predict(PolyRegModel,dtTestingData)
#   Error[PowerTermIndex-1] <- sum(abs(dtTestingData[,paste0("pred",PowerTermIndex),with=FALSE] - dtTestingData[,paste0("V",(nfeatures+1)),with=FALSE])/dtTestingData[,paste0("V",(nfeatures+1)),with=FALSE])
# }
# 
# finalPowerTerm <- PowerTerm[min(which(Error == min(Error)))]
# lFormula <- paste0("V",(nfeatures+1),"~","(",paste(lapply(seq(1:nfeatures),function(x) {return(paste0("V",x))}),collapse = "+"),")^",finalPowerTerm)
# PolyRegModel <- lm(lFormula,data = dtTrain) #train on whole data
# dtTest[,"pred"] <-  predict(PolyRegModel,dtTest)
# for (ind in 1:testingsamples) {
#   write(round(dtTest[ind,pred],2),stdout())
# }

#todo: acche se stepwise polynomial regression : find best way to handle this.


