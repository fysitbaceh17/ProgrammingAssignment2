## Put comments here that give an overall description of what your
## functions do

## There are two function makeCacheMatrix and cacheSolve
## Library(MASS) is used to calculate inverse for non squared as well as square matrices

library(MASS)
makeCacheMatrix <- function(x = matrix()) {
  j <- NULL                                  ##initializing inverse as NULL
  set <- function(y){                        ##set the value of matrix
    x <<- y
    j <<- NULL
  }
  get <- function()x                        ##function to get matrix x
  setInverse <- function(inverse) j <<- inverse
  getInverse <- function() j 
  list(set = set, get = get, 
       setInverse = setInverse,             ##set the value of inverse
       getInverse = getInverse)             ##function to obtain inverse of matrix
}

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  j <- x$getInverse()                     ##return inverse value
  if(!is.null(j)){
    message("getting cached data")
    return(j)                            ##return the inverse
  }
  mat <- x$get()
  j <- solve(mat,...)                     ##calculate inverse value
  x$setInverse(j)
  j                                       ##returns the inverse
}
