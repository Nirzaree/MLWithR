# 2 7
# 0.18 0.89 109.85
# 1.0 0.26 155.72
# 0.92 0.11 137.66
# 0.07 0.37 76.17
# 0.85 0.16 139.75
# 0.99 0.41 162.6
# 0.87 0.47 151.77
# 4
# 0.49 0.18
# 0.57 0.83
# 0.56 0.64
# 0.76 0.18

#stdin and out

library(data.table)

# input <- readLines("/home/nirzareevadgama/Downloads/predicting-house-prices-testcases/input/input00.txt")
input <- readLines(file("stdin"))
input <- strsplit(input," ")

nfeatures <- as.numeric(input[[1]][1])
trainingsamples <- as.numeric(input[[1]][2]) 

# dtTrain <- as.data.table(input[2:(2 + trainingsamples-1)])
dtTrain <- as.data.table(matrix(unlist(lapply(input[2:(trainingsamples+2-1)],as.numeric)),byrow = T,nrow = trainingsamples)) #this is to remember : 

testsamples <- as.numeric(input[[2 + trainingsamples]])

dtTest <- as.data.table(matrix(unlist(lapply(input[(trainingsamples+3):(trainingsamples+3+testsamples-1)],as.numeric)),byrow = T,nrow = testsamples))

lformula <- paste0("V",(nfeatures+1),"~",paste0(unlist(lapply(seq(1:nfeatures),function(x) {return(paste0("V",x))})),collapse = "+"))
LinModel <- lm(lformula,data = dtTrain)

dtTest[,"predPrices"] <- predict(LinModel,dtTest)

# print(round(dtTest[,predPrices],2))

for(ind in 1:testsamples) {
  write(round(dtTest[ind,predPrices],2),stdout())
} #for hackerrank format :|