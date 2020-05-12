#sum

#Write three functions that compute the sum of the numbers in a given list using a for-loop, a while-loop, and recursion.

sum <- function(ls){
  tot <- 0
  for(x in ls){
    tot <- x + tot
  }
  return(tot)
}

sum <- function(ls){
  tot <- 0
  while(length(ls) > 0){
    tot <- ls[1] + tot
    ls <- ls[-1]
  }
  return(tot)
}

sum <- function(ls){
  if(length(ls) == 0) return(0)
  ls[1] + sum(ls[-1])
}
