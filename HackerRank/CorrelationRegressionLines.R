# Here are the test scores of 10 students in physics and history:
#   
#   Physics Scores  15  12  8   8   7   7   7   6   5   3
# History Scores  10  25  17  11  13  17  20  13  9   15
# 
# Compute Karl Pearson’s coefficient of correlation between these scores.
# Compute the answer correct to three decimal places.
# 
# Output Format
# 
# In the text box, using the language of your choice, print the floating point/decimal value required. Do not leave any leading or trailing spaces.
# 
# For example, if your answer is 0.255. In python you can print using
# 
# print("0.255")
# 
# This is NOT the actual answer - just the format in which you should provide your answer.
inputs <- readlines(file(stdin))
inputs <- strsplit(inputs," ")
V1 <- inputs[[1]]
V2 <- inputs[[2]]
cor(V1,V2)

V1 <- c(15,12,8,8,7,7,7,6,5,3)
V2 <- c(10,25,17,11,13,17,20,13,9,15)
cat(round(cor(V1,V2),3))
