#finally working. phew.
input <- readLines(file("stdin"))
# input <- readLines("/home/nirzareevadgama/Downloads/saveprincess-testcases/input/input00.txt")
input <- strsplit(input," ")
gridsize <- as.numeric(input[[1]])

grid <- list()
for (lineindex in seq(1:gridsize)) {
  rowvalue <- strsplit(input[lineindex+1],"")
  grid <- append(grid,rowvalue)
}

grid <- do.call(rbind,grid)

#get position of princess
PrincessCell <- which(grid == "p",arr.ind = T)

#get position of bot
BotCell <- which(grid == "m",arr.ind = T)

#Calculate trajectory
#match row
stepsDiffRow = PrincessCell[,1] - BotCell[,1]
if (stepsDiffRow > 0) {
  OutputSequence <- rep("DOWN\n",stepsDiffRow)
} else if (stepsDiffRow < 0) {
  OutputSequence <- rep("UP\n",-stepsDiffRow)
}

#match column
stepsDiffColumn = PrincessCell[,2] - BotCell[,2]
if (stepsDiffColumn > 0) {
  OutputSequence <- append(rep("RIGHT\n",stepsDiffColumn),OutputSequence)
} else if (stepsDiffColumn < 0) {
  OutputSequence <- append(rep("LEFT\n",-stepsDiffColumn),OutputSequence)
}

# write(paste(OutputSequence,collapse = ""),stdout())
cat(paste(OutputSequence,collapse = ""))

# ##Only if reading line by line from stupid stdin was not so tedious
# 
# # input <- readLines(file("stdin"))
# # input <- strsplit(input," ")
# 
# # gridsize <- as.numeric(input[[1]])
# # grid <- input[[2]]
# 
# # gridsize = 3
# # grid <- list(list("-","-","-"),
# #              list("-","m","-"),
# #              list("p","-","-"))
# 
# # grid <- do.call(rbind,grid)
# 
# #get position of princess
# PrincessCell <- which(grid == "p",arr.ind = T)
# 
# #get position of bot
# BotCell <- which(grid == "m",arr.ind = T)
# 
# #Calculate trajectory 
# #match row 
# stepsDiffRow = PrincessCell[,1] - BotCell[,1]
# if (stepsDiffRow > 0) {
#   OutputSequence <- rep("DOWN\n",stepsDiffRow)
# } else if (stepsDiffRow < 0) {
#   OutputSequence <- rep("UP\n",-stepsDiffRow)
# }
# 
# #match column 
# stepsDiffColumn = PrincessCell[,2] - BotCell[,2]
# if (stepsDiffColumn > 0) {
#   OutputSequence <- append(rep("RIGHT\n",stepsDiffColumn),OutputSequence)
# } else if (stepsDiffColumn < 0) {
#   OutputSequence <- append(rep("LEFT\n",-stepsDiffColumn),OutputSequence)
# }
# 
  # write(paste(OutputSequence,collapse = ""),stdout())