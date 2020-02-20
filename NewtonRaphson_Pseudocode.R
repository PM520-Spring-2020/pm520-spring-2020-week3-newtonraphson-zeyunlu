# a function we will work with
F1<-function(x){
  return(c(x^2,2*x)) # note that the function returns two numbers. The first is f(x); the second is the derivative, f'(x)
}

# Define your Newton-Raphson function  
NewtonRaphson<-function(func,StartingValue,Tolerance,MaxNumberOfIterations){
  #initialize a variable, Deviation (say), to record |f(x)| so that you know how far away you are from 0. 
  #(So initialize it to some arbitrary large number)
  #Set up a counter, i, to record how many iterations you have performed. Set it equal to 0	
  # Initialize the values of x and f(x)
  
  #Set up a while loop until we hit the required target accuracy or the max. number of steps
  
  Deviation <- func(StartingValue)[1]
  i <- 0
  newX <- StartingValue
  while ((i < MaxNumberOfIterations) && (Deviation > Tolerance)) {
    tmp <- func(newX)
    if ((tmp[1]=="NaN") || (tmp[2]=="NaN")){
      cat("Function or derivative not defined error.")
      cat("\n", newX, tmp)
      break
    }
    newX <- newX - tmp[1]/tmp[2]
    newVal <- func(newX)
    Deviation <- abs(newVal[1])
    i <- i + 1
    
    # if you like, have the program write out how it is getting on
    cat(paste("\nIteration ",i,":   X=",newX,"  Y=",newVal))
    
    # If you are feeling fancy, add some line segments to the screen to show where it just went
    # See the 'fixed points' code for a reminder of how to do that.
  }
  
  # output the result
  if (Deviation<Tolerance){
    cat(paste("\nFound the root point: ",newX, " after ", i, "iterations"))
  }else{
    cat(paste("\nConvergence failure. Deviation: ",Deviation, "after ", i, 	"iterations"))}    
  
  # have the function return the answer
  return(newX)
}


pdf("Fig6.pdf")
curve(x^2,-1,11,main="y=x^2")
NewtonRaphson(F1,10,1e-3,40)
abline(h=0)
dev.off()