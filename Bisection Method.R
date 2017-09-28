
#Bisection Method

#Remove List
rm(list=ls())

#Defining minimization function

f = function(x)
{
  exp(-x) + (x)^2
}

f(-3)

#Set Input here

LowerBound= -3
UpperBound=3
Limit=10^-8

#Initial Values
MidPoint=(UpperBound+LowerBound)/2

#Plot the graph

plot(seq(LowerBound,UpperBound,0.01),f(seq(LowerBound,UpperBound,0.01)))

#Reset Iteration Counter
Iterations=1

while ((UpperBound-LowerBound)> Limit)
{
  if ((f(MidPoint)-f(LowerBound))/(MidPoint-LowerBound) <0)
  {
    LowerBound=MidPoint
    UpperBound= UpperBound
    MidPoint=(UpperBound+LowerBound)/2
  } else
  {
    LowerBound=LowerBound
    UpperBound=MidPoint
    MidPoint=(UpperBound+LowerBound)/2
  }
  
  Iterations=Iterations+1
  
}

#Final estimate
EstimatedMinimum=(LowerBound+UpperBound)/2

#Prints out the results

cat("The minimum will be found at",EstimatedMinimum, "after",Iterations, "iterations.")

#Solution: The minimum will be found at 0.375 after 31 iterations.

