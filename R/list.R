#list
#Write a function that combines two lists by alternatingly taking elements. For example: given the two lists [a, b, c] and [1, 2, 3], the function should return [a, 1, b, 2, c, 3].

my_list <- function(ls1, ls2){
  tot <- c()
  while(length(ls1)){
    tot <- c(tot, ls1[1], ls2[1])
    ls1 <- ls1[-1]
    ls2 <- ls2[-1]
  }
  return(tot)
}

#recursive soln
my_list <- function(ls1, ls2){
  if(!length(ls1)) return(c())
  c(ls1[1], ls2[1], my_list(ls1[-1], ls2[-1]))
}
