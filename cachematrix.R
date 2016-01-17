## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix is a function that creates a special matrix object that 
## can cache its inverse 
## Define the variable as a function with "x" as its assignment. 
## "x" begins as an empty matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## Write a short comment describing this function
## This function cachseSolve computes the inverse of the special matrix
## returned above. If the inverse is already calcultaed (and the matirx
##has not been changed), then the cached value will be retrieved and used.

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if (!is.null(inv)){
          message("getting cached data")
          return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv
}
