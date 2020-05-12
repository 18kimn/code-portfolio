#overlap_merge 
# Given a collection of intervals, write a function that merges all overlapping intervals and prints them out.
# 
# For example, given [1, 3], [2, 6], [8, 10], and [7, 11], the function should print [1, 6], [7, 11]. Or given [5, 12], and [8, 10] the function should print [5, 12].
# 
# You can assume that the first element of each interval is always less or equal than the second element of the interval.

# if the first range overlaps with the second range, 
# replace the first one with the new overlap, run the function on the new list
# if it does not, make a list:
#list(first_element, union_merge(rest_of_list))
#recursive two-function soln
merge_help <- function(vecs){
  if(length(vecs) == 1) return(vecs)
  vec1 <- vecs[[1]]
  vec2 <- vecs[[2]]
  overlap <- max(vec1) > min(vec2) & min(vec1) < max(vec2) |
    max(vec2) > min(vec1) & min(vec2) < max(vec1) 
  if(overlap){ 
    union_merge(c(list(c(min(vec1, vec2), max(vec1,vec2))), 
                  vecs[-c(1:2)]))
  }else{
    c(list(vec1), union_merge(vecs[-1]))
  }
}
union_merge <- function(vecs){
  vecs <- vecs[order(unlist(lapply(vecs, function(x) x[1])))]
  merge_help(vecs)
}


#explicit/non-recursive solution
union_merge <- function(vecs){
  vecs <- vecs[order(unlist(lapply(vecs, function(x) x[1])))]
  i <- 1
  while(i<length(vecs)){
    vec1 <- vecs[[i]]
    vec2 <- vecs[[i+1]]
    overlap <- (max(vec1) > min(vec2) & min(vec1) < max(vec2)) |
      (max(vec2) > min(vec1) & min(vec2) < max(vec1))
    if(overlap){
      
      vecs <- c(vecs[0:(i-1)], 
                list(c(min(vec1, vec2), max(vec1,vec2))), 
                vecs[-c(1:(i+1))])
    }else{
      i <- i+1 
    }
  }
  return(vecs)
}
