#fibo
#a classic
#takes n, the number of fibonacci numbers to print, and assembles a list of them

#soln1: (non-tail) recursive
fibo <- function(n){
  if(n ==1) return(0)
  if(n ==2) return(0:1)
  prev <- fibo(n-1)
  c(prev, tail(prev, 1) + tail(fibo(n-2),1))
}

#soln2: iteratively in R
fibo <- function(n){
  if(n==1) return(0)
  if(n==2) return(0:1)
  ls <- 1:0
  for(i in 2:n){
    ls <- c(ls[1] + ls[2], ls)
  }
  return(rev(ls))
}





