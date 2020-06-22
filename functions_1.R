add2 <- function(a,b) {
  a+b
  ##The function returns whatever the last expression was
  ##After you finished the function run 
  ##the file and now you can use it
}

above_n <-function(x,n = 0){
  ##function that return the elements of the vector
  ##that a bigger than n
  ##if you specify n in the function initialization then
  ##R looks at it as the default value
  use <- x>n
  x[use]
}

column_mean <-function(y, removeNA=TRUE){
  nc<-ncol(y)
  means <- numeric(nc)
  for (i in 1: nc){
    means[i] <- mean(y[,i], na.rm = removeNA)
  }
  means
}
