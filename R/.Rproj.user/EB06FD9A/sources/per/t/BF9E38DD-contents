#R is not made for strings in the same way that JS or Python are.
#These exerises are meant to show how R can do it anyhow, through some neat vectorized functions R comes with and those that we can write on-the-fly. 
# A demonstration of base R's looping functions, function syntax exploited to make general-case statements, and recursive statements written in R. 

#Problem 1: palindrome. Write a function that returns TRUE if a string is a palindrome, that is if that string written in reverse order is that string. 

#Solution 1.1: A single statement through R's strsplit() and rev() commands. The "idiomatic" version, and by far the fastest of the three 
palindrome <- function(stri){
  paste0(rev(strsplit(stri, split = "")[[1]]),
         collapse ="") == stri
}

#Solution 1.2: A two-function recursiion, one function creating the reversed string and another evaluating if it is equal to the original.  
#Not tail-recursive to cut down on the number of arguments needed. 
reverse <- function(stri){
  ifelse(stri != "",
         paste0(reverse(substr(stri, 2, nchar(stri))), substr(stri,1,1)),
         "")
}

palindrome2 <- function(stri){
  stri == reverse(stri)
}


#Solution 1.3: A one-function tail-recursive solution. Requires some extra arguments to "carry" information through recursion. 
palindrome3 <- function(stri, sb= "", og_stri = stri){
  ifelse(stri != "",
         palindrome3(substr(stri, 2, nchar(stri)),
                    paste0(substr(stri,1,1), sb), 
                    og_stri),
         og_stri == sb)
}

#Problem 2: anagram. Write a function that evaluates an arbitrary number of strings to test if they are all anagrams of each other. 

#Solution 2.1: Using base R's loop function lapply() to split strings. Near-idiomatic.
anagram <- function(...){
  strings <- lapply(c(...), function(x) sort(strsplit(tolower(x),"")[[1]]))
  length(unique(strings)) == 1
}


#Problem 3: vowels. Write a function that returns the number of vowels in a string. Note that base R's string functions are very slow. 

#Solution 3.1: A non-recursive sapply() solution. The fastest. 

vowels1 <- function(..., v = c("a","e","i","o","u")){
  stri <- unlist(strsplit(c(...), ""))
  sum(sapply(v, function(x) sum(x == stri)))
}


#Solution 3.2: A recursive solution through the list of "vowels".  
vowels2 <- function(..., v = c("a","e","i","o","u")){
  ifelse(length(v),   
         sum(v[1] == unlist(strsplit(c(...), ""))) + 
           vowels(c(...), v = v[-1]),
         0)
}

#Solution 3.3: Using prevectorized functions from base R.  
vowels3 <- function(..., v =  c("a","e","i","o","u")){
  length(unlist(regmatches(c(...), 
                        gregexpr(paste(v, collapse = "|"),c(...))
                        )))
}


#Problem 4: permute Create all permutations of a given string. 

#Solution 4.1: A once-recursive lapply() solution.  

permute <- function(stri){
  if(!nchar(stri)) return("")
  unlist(lapply(1:nchar(stri), function(i){
    paste0(substr(stri, i, i), 
          permute(paste0(substr(stri, 0, i-1), 
                         substr(stri, i+1, nchar(stri)))))
  }))
  
}

#Solution 4.2: Non-recursive expand.grid solution. Somehow much slower?

permute2 <- function(stri){
  stri <- strsplit(stri, "")[[1]]
  stri <- expand.grid(rep(list(stri), length(stri)))
  stri <- apply(stri, 1, function(x) ifelse(length(unique(x)) == ncol(stri), 
                                    paste0(x, collapse = ""), 
                                    NA_character_))
  stri[!is.na(stri)]
}


