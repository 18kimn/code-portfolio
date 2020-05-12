#Write a function that given a list of non negative integers, arranges them such that they form the largest possible number. For example, given [50, 2, 1, 9], the largest formed number is 95021.

#the tie case is incredibly annoying but a fantastic one to think about
#input: numeric vector
#recursively
largest_num <- function(vec){
 if(!length(vec)) return("")

 l <- which.max(substr(vec, 1,1))
 
 #annoying tie case -- starting with i=1, compare the ith placements for max. 
 vec_ties <- vec
 for(i in 1:max(nchar(vec))){
   ith_dig <- substr(vec_ties,i,i) 
   max_dig <- max(ith_dig)
   # replace "" with the maximum digit in the series of ties
   ith_dig[!nzchar(ith_dig)] <- max(substr(vec_ties, 1,1))
   #if there is a tie at the ith digit
   if(sum(ith_dig == max_dig) > 1){
   #locate and isolate the ties
   ith_dig[!nzchar(ith_dig)] <- max_dig
   vec_ties <- vec_ties[which(ith_dig == max_dig)]
   next #then check out ith+1 digit
   }
   #if there is no tie at the ith digit then use that 
   l <- which.max(ith_dig)
   break
 }
 paste0(vec[l], largest_num(vec[-l]))
}
