# # Correlation and Regression Lines - A quick recap #3
# 
# Here are the test scores of 10 students in physics and history:
#   
# Physics Scores  15  12  8   8   7   7   7   6   5   3
# History Scores  10  25  17  11  13  17  20  13  9   15
# 
# When a student scores 10 in Physics, what is his probable score in History? Compute the answer correct to one decimal place.
# 
# Output Format
# 
# In the text box, enter the floating point/decimal value required. Do not leave any leading or trailing spaces. Your answer may look like: 5.5
# 
# This is NOT the actual answer - just the format in which you should provide your answer.

input <- readLines(file("stdin"))
input <- strsplit(input," ")
PhysicsScores <- as.numeric(input[[1]])
HistoryScores <- as.numeric(input[[2]])

PhysicsScores <- c(15 , 12,  8  , 8 ,  7,   7 ,  7  , 6 ,  5  , 3)
HistoryScores <- c(10 , 25,  17,  11,  13,  17,  20,  13,  9,   15)

cor(PhysicsScores,HistoryScores)
#really low correlation
linmodel <- lm("HistoryScores~PhysicsScores")

HistoryScore <- linmodel$coefficients[1] + linmodel$coefficients[2]*10

#todo: but hardly any correlation. .this is stupid.
#figure through stdin :|

