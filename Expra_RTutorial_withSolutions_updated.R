#############Introduction to R Tutorial#################

# In this first part of the data analysis intro, we have defined our own small artificial
# data set and got to know some basic functionalities of R.
# As explained in the lesson, in the following lines of code we define a value for the 
# standard stimulus of our artificial experiment and values for test intervals that correspond to 
# the percentages w.r.t our standard that you see in the first task. 
# When you go through the code again by yourself, try to play around with the values that we
# are working with and follow the logic of what happens. 

standard = 500
test1 = 500 * 0.95
test1

# Task1: Please create variables for test intervals that are -10%, -20%, -40%, 
#        +5%, +10%, +20% and +40% of the standard interval

#Solution:
test2 = 500 * 0.90
test3 = 500 * 0.80
test4 = 500 * 0.60
# as discussed, the smarter way in most cases is to reference directly the corresponding variable
# on which our calculations should be based, instead of its value -> less prone to mistakes
test5 = standard * 1.05 
test6 = standard * 1.10
test7 = standard * 1.2
test8 = standard * 1.4

# Now we want to collect our test intervals in a vector.
testIntervals = c(test1,test2,test3,test4,test5,test6,test7,test8)

# These are our first examples of using inbuild functions that are available in R
# This one sorts our test intervals values in ascending order
testIntervals = sort(testIntervals)

# Here, we create a vector that contains the amount of trials for each stimulus by
# using the replicate function. 
n1 = rep(20,8)

# Here, we create a vector that reflects possible responses of our artificial participant for each 
# test interval -> it tells us how many times this participant perceived the corresponding 
#test interval as longer than the standard
r1 = c(2,4,7,9,10,13,17,19)

# Just to show another useful function which returns the number of values/elements that are contained
# in the vector/matrix/data frame or list of interest
nStim = length(testIntervals)

# Here we use vector calculation to get the response proportions for each test interval.
# These are the proportions with which our artificial participant judged the test interval 
# as longer than the standard
rprop1 = r1/n1

# Task2: Create artificial n, r and rprop for a second person

#Solution:
n2 = rep(20,8)

r2 = c(1,2,5,8,11,13,17,18)

rprop2 = r2/n2

# Here we combine our vectors that each contain the data for one participant into
# a matrix that contains the data for both participants
N = rbind(n1, n2)
R = rbind(r1, r2)
RProp = rbind(rprop1, rprop2)


# When it comes to indexing values, remember that the first value in square brackets
# indexes rows and the second value, separated by a comma indexes the columns. 
# If we want to index through a range of values, we use a colon. 
# Play with these examples and try the tasks one more time by yourself. 
testIntervals[1]
r1[5:8]
RProp[1,5]
RProp[,1:4]

# Task: print the following values through indexing
#       a) the testinterval of 450 in the testIntervals vector
#       b) the number of times the first participant judged the test interval of 475 as longer than the standard 
#       c) the proportions with which the second participant judged the two test intervals surrounding 
#          the standard interval as longer than the standard
#       d) all response proprtions but the last of both participants

#Solution:
testIntervals[3]
R[1,4]
RProp[2,4:5]
RProp[,1:7]


# When it comes to functions, you should remember that the arguments that
# we put in the brackets are placeholders for the values that you will use 
# as inputs later on. Everything in between the "function" and the "return" statement
# are the operations that we want to perform on our input arguments. 
ownSum = function(a,b) {
  result = a+b 
  return(result)
}

# When you use your own functions, you usually have to give those functions
# as many input values as you have defined the function with.
ownSum(6,8)

ownSum2 = function(a,b,c,d,e,f,g,h) {
  result = a+b+c+d+e+f+g+h
  return(result)
}

testSum = ownSum2(test1, test2, test3, test4, test5, test6, test7, test8)

ownMean = function(sum, n) {
  ownMean = sum / n
  return(ownMean)
}
# what is the problem of the function above?

# Remember that in the lecture, we have discussed that the function above might 
# be appropriate in case we have calculated the sum of all our data entries of interest
# beforehand but it doesn't work in case of vectors as input.

ownMean(testSum, 8)
ownMean(testIntervals,8)


# Here, we have seen that we can define functions in a way that they perform 
# multiple operations with our input values of interest and then return the result
# that we are interested in. 

trySomething = function(c,d) {
  firstResult = c*d
  secondResult = firstResult**2
  thirdResult = secondResult/10
  finalResult = sqrt(thirdResult)
  print('we have done some calculations on our input values')
  return(finalResult)
}

a = trySomething(10, 20)

# Here is our own mean function in a more appropriate manner. 
# We first calculate the sum of all the values of our input vector.
# Than we calculate the number of values in this vector.  
# In the end we calculate the mean just by using the basic formula for that purpose 
# and print the sentence that we define in our print statement, together with the result. 
ownMean2 = function (testIntervals){
  Sum = sum(testIntervals)
  N = length(testIntervals)
  mean = Sum/N
  print('the mean of our input data is')
  return (mean)
}

ownMean2(testIntervals)

differentVector = seq(1,10,1)
ownMean2(differentVector)

# The cool thing is that R provides hundreds or thousands of in-build functions
# and in many cases a quick google search provides us with the functions that we are
# searching for. We just have to use them the right way. Here, we use the in-build mean
# function to compare the result to the one that we get with our previously defined own mean function. 
mean(differentVector)

# Task: define your own function to calculate the variance. 
#       Hint: use google to get the formula for the variance if you need to refresh it

# Solution:
ownVariance = function(x) {
  m = mean(x)
  n = length(x)
  var = (1/(n - 1)) * sum((x - m)^2)
  return(var)
}

ownVariance(testIntervals)

var(testIntervals)

# This is another quite useful function for descriptive statistics. Try it out. 
summary(testIntervals)

#If you want to see the documentation for a certain in-build function, you can just type help
# and use the function that you want to know more about as the argument. 
help(var)


# We came until this point during our exercise. 
# The rest that follows will be part of our next lesson. 
# If you like, you can already go through these examples to get a first view on
# what happens. Otherwise just leave it for our next session but try to understand the examples above.
# If you have any further questions don't hesitate to post them in the forum. 



#################################
# Part 2:
#setting working directory 
# You can (and always should!) set your working either by using the following 
# command and typing in the path to your folder where the files you want to use 
# are stored. Or you can go to Session -> Set Working Directory -> Choose Directory
# (press control, shift and H as keyboard shortcut) to do it via the menu.
setwd("your working directory")

#importing data
# Remember to first take a look at the file you want to import to get sure that 
# you use the right command to get your data in an appropriate way into R.
# Look for the format of your file and for how values are separated by each other.
df <-read.table("testFileTabSeparated", header = TRUE)
df <-read.csv("testFileCommaSeparated")

# As discussed, often it is necessary to load specific libraries to access 
# certain functionalities that will make your life much easier.
library(jsonlite)
psyphysdata <-fromJSON('experiment_data_3ox3hac0f_undefined.json')

summary(df)

# accessing specific data and logical operators
# Remember that logical operators return a "True" or "False". 
# You can think of them for example as following:
# Return all rows of the data frame where it is true that the value in 
# in the gender column equals 'm'
df$gender
df$gender == 'm'

# or like here: 
# create a subset of our data frame where it is true that the value 
# in the gender column equals female and at the same time the value in 
# the psychStudent columns equals 'TRUE'
femalePsychStudents = subset(df, subset = df$gender == 'w' & df$psychStudent == TRUE)
femalePsychStudents

# the vertical bar works as an 'OR' operator 
subset(df, subset = df$age < 20 | df$age > 25)

any(testIntervals == 500)
any(testIntervals != 500)


# Task 6: a) create a variable that contains all non-psychology students by accessing the right values
#       b) print all male participants that are 25 or older (use >=)
#       c) print all female non-psychology students that are younger than 20 or older than 25 

#Solution:

nonPsychStud = subset(df, subset = df$psychStudent == FALSE)

subset(df, subset = df$gender == 'm' & df$age >= 25)

subset(df, subset = df$gender == 'w' & df$psychStudent == FALSE & (df$age < 20 | df$age > 25)) #note that you have to use additional parantheses




# if conditionals
# if statements are used very often when operations should be done based on
# certain conditions. You can think of them as: if 'a' is true do 'x', otherwise do 'y'
# So, for example here: if it is true that the first value in our test intervals 
# vector is less than 500, print 'testintervall less than standard', otherwise
# print 'testintervall higher than standard'
if (testIntervals[1] < 500) {
  print('testintervall less than standard')
} else print('testintervall higher than standard')

psychStudent = df$psychStudent

if (psychStudent[1]==TRUE) {
  print('this participant is a psychology student')
} else print('this is not a psychology student')


#combining functions and if statements
# It is always possible to combine all the different functionalities in very 
# flexible ways. We can for example create functions that use if statements. 
getPsychStud = function(psychStudent) {
  if (psychStudent == TRUE) {
    print('this participant is a psychology student')
  } else print('this is not a psychology student')
}

getPsychStud(psychStudent[6])


# Task 7: a) write a function that contains an if condition that prints: 'our senses should be able to discriminate between this test value 
#          and the standard pretty well' in case the value is 40% less or 40% higher than the standard.
#          Otherwise print: 'more difficult to discriminate'

#Solution:

checkHighDisc = function(value, standard) {
  if (value == standard * 0.6 | value == standard * 1.4){
    print('our senses should be able to discriminate between this test value and the standard pretty well')
  } else print('more difficult to discriminate')
}

checkHighDisc(testIntervals[1], standard)



# for loops
# for loops are very useful when we want to perform the same operations on 
# different values (or inputs in general). 
for (i in 1:5){
  squaredValue = i**2
  print(squaredValue)
}


for (i in 1:8) {
  print(paste0('the age of the',' ',i, 'th participant is',' ', df$age[i]))
}


# combined with if-conditionals
# As we have seen in the examples for if statements, for loops can also 
# be flexibly combined with other functionalities. 
# In general, we can combine as many for loops, if statements and all the other
# concepts as we like and build functions which do these things in the ways 
# we want them to do
# 
for (i in df$psychStudent){
  if (i == TRUE) {
    print('this is a psychology student ')
  } else if (i == FALSE) {
    print('this is not a psychology student')
  }
}


#nested loops
for (i in 1:5) {
  for (j in 1:3) {
    print(i*j)
  }
}

#Task 8: loop through the testinterval vector and print for every value less than the standard
# 'this interval is less than the standard', otherwise print 'this interval is higher than the standard'

#Solution:

for (i in testIntervals) {
  if (i < standard) {
    print('this interval is less than the standard')
  } else print('this interval is higher than the standard')
}



# plotting
# As discussed in the lecture, when it comes to plots there are hundreds and thousands
# of ways to construct and modify them with lots of parameters. 
# When you create your own plots, look for appropriate plotting
# libraries online, study their functionalities and try to experiment with them.

# here, as an example are the plots which represent the response proportions
# of our two artificial participants for all our test intervals. 
plot(testIntervals, RProp[1,])
plot(testIntervals, RProp[2,])

# or both plots in one window, e.g. as follows:
# define area for the first plot and plot it
par(fig=c(0,0.5,0.2,0.9))
plot(testIntervals, RProp[1,])
#define area for the second plot and plot it
par(fig=c(0.5,0.99,0.2,0.9), new=TRUE)
plot(testIntervals, RProp[2,])

