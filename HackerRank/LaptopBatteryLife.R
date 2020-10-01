# Fred is a very predictable man. For instance, when he uses his laptop, all he does is watch TV shows. He keeps on watching TV shows until his battery dies. Also, he is a very meticulous man, i.e. he pays great attention to minute details. He has been keeping logs of every time he charged his laptop, which includes how long he charged his laptop for and after that how long was he able to watch the TV. Now, Fred wants to use this log to predict how long will he be able to watch TV for when he starts so that he can plan his activities after watching his TV shows accordingly.
# 
# Challenge
# 
# You are given access to Fred’s laptop charging log by reading from the file “trainingdata.txt”. The training data file will consist of 100 lines, each with 2 comma-separated numbers.
# 
# The first number denotes the amount of time the laptop was charged.
# The second number denotes the amount of time the battery lasted.
# 
# The training data file can be downloaded here (this will be the same training data used when your program is run). The input for each of the test cases will consist of exactly 1 number rounded to 2 decimal places. For each input, output 1 number: the amount of time you predict his battery will last.
# 
# Sample Input
# 
# 1.50
# 
# Sample Output
# 
# 3.00
# 
# Scoring
# 
# Your score will be 10-X, where X is the sum of the distances you are from expected answer of each test case. For instance if there are 2 test cases with expected answer 4 and you print 3 for the first one and 6 for the second one your score will be 10-(1+2) = 7.

# input <- readLines("/home/nirzareevadgama/Downloads/battery-testcases/input/input01.txt")
# input <- strsplit(input,",")
# input <- lapply(input,as.numeric)
# input <- do.call(rbind,input)
# input <- data.table(input)

# names(input) <- c("laptopchargetime","batterylastedtime")
# plot(x = input[,laptopchargetime],y = input[,batterylastedtime])
# #saturated linear function (x < 4, linear, x > 4 constant)
# 
# dtTrain <- input[laptopchargetime < 4,]
# linModel <- lm("batterylastedtime~laptopchargetime",dtTrain)

#now the input only contains the charging time, so read that. 
input <- readLines(file("stdin"))
input <- as.numeric(input[1])

BatteryLastingTime <- ifelse(input > 4,8,round(5.238191e-16 +  2.000000e+00*input,2))
write(BatteryLastingTime,stdout())
