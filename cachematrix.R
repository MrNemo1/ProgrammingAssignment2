
## R Programming Assignment 2: Lexical Scoping--caching the inverse of a matrix 
  
## The makeCacheMatrix function generates a "special" matrix. 
## A "special" matrix is essentially a list,
## containing a function to carry out the following procedure: 
## a) set the value of the matrix 
## b) get the value of the matrix 
## c) set the value of the inverse of the matrix 
## d) get the value of the inverse of the matrix  
 
 makeCacheMatrix <- function(x = matrix()) { 
     j <- NULL 
     set <- function(y) { 
         x <<- y 
         j <<- NULL 
     } 
     get <- function() x 
     set_inverse <- function(inverse) j <<- inverse 
     get_inverse <- function() j 
     list( 
         set = set, 
         get = get, 
         set_inverse = set_inverse, 
         get_inverse = get_inverse) 
 } 
 
 
 ## The cacheSolve function calculates the inverse of this "matrix".  
 ## First, we check if the inverse has already been calculated. 
 ## If so, we get the inverse from the cache, and we are done. 
 ## Otherwise, we calculate the inverse of the matrix and set the 
 ## value of the inverse in the cache. 
 
 cacheSolve <- function(x = matrix(), ...) { 
     ## Return a matrix that is the inverse of 'x' 
     j <- x$get_inverse() 
     if(!is.null(j)) { 
         message("getting cached data") 
         return(j) 
     } 
     data <- x$get() 
     j <- solve(data, ...) 
     x$set_inverse(j) 
     j 
 } 