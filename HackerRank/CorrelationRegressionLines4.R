# The two regression lines of a bivariate distribution are:
#   4x – 5y + 33 = 0 (line of y on x) 20x – 9y – 107 = 0 (line of x on y).
#   Estimate the value of  x when
#   
# y  = 7. Compute the correct answer to one decimal place.
#   
#   Output Format
#   In the text box, enter the floating point/decimal value required. Do not lead any leading or trailing spaces. Your answer may look like: 7.2
#   
#   This is NOT the actual answer - just the format in which you should provide your answer.

#dont know what bivariate distribution means. 
#but assuming that since y is given we use the second equation.
y = 7 
# x = (107+9*y)/20
x = (5*y-33)/4
write(x,stdout())