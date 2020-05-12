#Write a function to rotate an NxN matrix by 90 degrees. You should rotate it in place, meaning you can't use another matrix to perform the rotation, but instead you have to use the same given structure.

#not really sure what "you can't use another matrix to perform the rotation" means
#a solution that comes very quickly in R
rotate <- function(mat){
  newmat <- matrix(NA,nrow(mat), ncol(mat))
  for(i in 1:ncol(mat)) newmat[i,] <- rev(mat[,i])
  return(newmat)
}

#another interesting solution
rotate <- function(mat){
  newmat <- matrix(NA, nrow(mat), ncol(mat))
  for(i in 1:nrow(mat)) newmat[,5-i] <- t(mat)[,i]
  return(newmat)
}


