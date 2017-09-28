
#Golden Section Method

rm(list=ls())

#Defining minimization function

f = function(x)
{
  exp(-x) + (x)^2
}

#Set Input here

LowerBound=-3
UpperBound=3
Limit=10^-8

#Initial Values

golden.ratio=(3-sqrt(5))/2
Lambda=(golden.ratio*(UpperBound-LowerBound)) + LowerBound
Mu=((1-golden.ratio)*(UpperBound-LowerBound)) + LowerBound

#Plot the graph for refernce
plot(seq(LowerBound,UpperBound,0.01),f(seq(LowerBound,UpperBound,0.01)))

#Reset Iteration Counter
Iterations=1


while (UpperBound-LowerBound > Limit)
{
if (f(Lambda) <= f(Mu))
{
  LowerBound=LowerBound
  UpperBound=Mu
  Mu=Lambda
  Lambda=(golden.ratio*(UpperBound-LowerBound)) + LowerBound
} else
{
  LowerBound=Lambda
  UpperBound=UpperBound
  Lambda=Mu
  Mu=((1-golden.ratio)*(UpperBound-LowerBound)) + LowerBound
}
  Iterations=Iterations+1

}



#Final estimate
EstimatedMinimum=(LowerBound+UpperBound)/2

#Prints out the results

cat("The minimum will be found at",EstimatedMinimum, "after",Iterations, "iterations.")

#Solution: The minimum will be found at 0.3517337 after 44 iterations.
