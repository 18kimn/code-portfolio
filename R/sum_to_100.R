#Write a program that outputs all possibilities to put + or - or nothing between the numbers 1, 2, ..., 9 (in this order) such that the result is always 100. For example: 1 + 2 + 34 – 5 + 67 – 8 + 9 = 100.

#sum to 100
#another problem with a kind of elegant solution
#recursive solution
sum_100 <- function(vec = 2:9, exprs = "1"){
  if(!length(vec)){
    walk(exprs, ~ifelse(eval(parse(text=.))==100, print(.), ""))
    return(invisible(NULL))
  }
  exprs <- unlist(map(exprs, ~paste0(., c("+", "","-"), vec[1])))
  return(sum_100(vec[-1], exprs))
}

#explicit/iterative solution
sum_100 <- function(){
  exprs <- "1"
  for(i in 2:9){
    exprs <- unlist(map(exprs, ~paste0(., c("+", "","-"), i)))
  }
  walk(exprs, ~ifelse(eval(parse(text=.))==100, print(.), ""))
  return(invisible(NULL))
}
