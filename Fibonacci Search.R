
#Fibonacci Search

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

#Creating Fibonacci Sequence

n=round(1 + (1/log(0.618))*log(Limit/(UpperBound-LowerBound)), digits=0)
Fibonacci = numeric(n)
Fibonacci[0] = 1
Fibonacci[1] = 1
for (i in 3:n) { 
  Fibonacci[i] = Fibonacci[i-1] + Fibonacci[i-2]
} 
Fibonacci

#Initial Lambda and Mu

Lambda=LowerBound+((Fibonacci[n-2]/Fibonacci[n])*(UpperBound-LowerBound))
Mu = LowerBound+((Fibonacci[n-1]/Fibonacci[n])*(UpperBound-LowerBound))


#Plot the graph for reference

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
    Lambda=LowerBound+((Fibonacci[n-2]/Fibonacci[n])*(UpperBound-LowerBound))
  } else
  {
    LowerBound=Lambda
    UpperBound=UpperBound
    Lambda=Mu
    Mu=LowerBound+((Fibonacci[n-1]/Fibonacci[n])*(UpperBound-LowerBound))

  }
  Iterations=Iterations+1
  
}

#Final estimate
EstimatedMinimum=(LowerBound+UpperBound)/2

#Prints out the results

cat("The minimum will be found at",EstimatedMinimum, "after",Iterations, "iterations.")

#Solution: The minimum will be found at 0.3517337 after 44 iterations.
