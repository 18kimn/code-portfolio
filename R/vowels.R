#counts vowels (or other strings)
vowels <- function(stri, v = c("a","e","i","o","u"), val = 0){
  ifelse(length(v)>0,   
         vowels(stri, v= v[-1], 
                val = val + sum(v[1] == strsplit(stri, "")[[1]])
                ),
         val)
}

#recursive
#simple syntax