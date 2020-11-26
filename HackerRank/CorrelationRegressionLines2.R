# Here are the test scores of 10 students in physics and history:
#   
#   Physics Scores  15  12  8   8   7   7   7   6   5   3
# History Scores  10  25  17  11  13  17  20  13  9   15
# 
# Compute the slope of the line of regression obtained while treating Physics as the independent variable. Compute the answer correct to three decimal places.
# 
# Output Format
# 
# In the text box, enter the floating point/decimal value required. Do not leave any leading or trailing spaces. Your answer may look like: 0.255
# 
# This is NOT the actual answer - just the format in which you should provide your answer.

# input <- readLines(file("stdin"))
# input <- strsplit(input," ")

# dtData <- data.table(
  # PhysicsScores = as.numeric(input[[1]]),
  # HistoryScores = as.numeric(input[[2]])
# )
library(data.table)
dtData <- data.table(
  PhysicsScores = c(15,12,8,8,7,7,7,6,5,3),
  HistoryScores = c(10,25,17,11,13,17,20,13,9,15)
)

lformula <- "HistoryScores~PhysicsScores"
linModel <- lm(lformula,data = dtData)

write(round(linModel$coefficients[2],3),stdout())
# 0.208 is the answer but the code doesnt succeed. only when printing only output it works. :( :  todo: understand why  