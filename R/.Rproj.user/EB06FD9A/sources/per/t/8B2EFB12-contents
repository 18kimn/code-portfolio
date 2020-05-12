#split 
# Given an array of ints, is it possible to divide the ints into two groups, so that the sums of the two groups are the same. Every int must be in one group or the other. Write a recursive helper method that takes whatever arguments you like, and make the initial call to your recursive helper from splitArray(). (No loops needed.)

split_vec <- function(vec, l1 = NULL, l2 = NULL){
  if(!length(vec)) return(sum(l1) == sum(l2))
  any(split_vec(vec[-1], l1, c(vec[1], l2)), 
      split_vec(vec[-1], c(vec[1], l1), l2))
}

#split, version 2
#Given a non-empty array, return true if there is a place to split the array so that the sum of the numbers on one side is equal to the sum of the numbers on the other side.

#solution 1: sapply()
balance <- function(vec){
  any(sapply(1:(length(vec)-1), function(i){
    sum(vec[1:i]) == sum(vec[(i+1):length(vec)])
  }))
}

#Solution 2: recursive with carry-through argument. Faster. 
balance <- function(vec, rest = NULL){
  if(!length(vec)) return(FALSE)
  any(c(rest, vec) == sum(vec[-1]), 
      balance(vec[-1], c(rest,vec)))
}


#span
#Consider the leftmost and righmost appearances of some value in an array. We'll say that the "span" is the number of elements between the two inclusive. A single value has a span of 1. Returns the largest span found in the given array. (Efficiency is not a priority.)

#Solution 1: sapply()
span <- function(vec){
  1 + max(sapply(unique(vec), function(x){
    w <- which(x == vec)
    max(w) - min(w)
  }))
}


#Say that a "clump" in an array is a series of 2 or more adjacent elements of the same value. Return the number of clumps in the given array.
#countClumps([1, 2, 2, 3, 4, 4]) → 2
#countClumps([1, 1, 2, 1, 1]) → 2
#countClumps([1, 1, 1, 1, 1]) → 1
#a clunky carry-over recursive solution

#the rle "best" version. Kind of cheating, but whatever. 
clumps1 <- function(vec){
  sum(rle(vec)$lengths>1)  
}

#A recursive solution. Looks good, runs slow. 
clumps2 <- function(vec, in_clump = F){
  if(length(vec) == 1) return(0)
  ifelse(vec[1] == vec[2], 
         as.numeric(!in_clump) + clumps2(vec[-1], T), 
         clumps2(vec[-1], F))
}

#For-loop version. Looks bad, runs okay. 
clumps3 <- function(vec){
  in_clump <- F
  val <- 0
  for(i in 1:(length(vec)-1)){
    if((vec[i] == vec[i+1]) & !in_clump){
      val <- val+1; in_clump <- T
    } else if(vec[i] != vec[i+1]){
      in_clump <- F
    }
  }
  return(val)
}

#so clunky

vec <- sample(1:100, 1000, replace = T)
microbenchmark(
  clumps1(vec), 
  clumps2(vec), 
  clumps3(vec)
)
