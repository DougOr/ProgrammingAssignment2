## This file contains two functions
## makeCacheMatrix and cacheSolve

## makeCacheMatrix creates a special "matrix" object
## that can cache its inverse
makeCacheMatrix <- function(x = numeric()) {
  # initialize
  cache <- NULL
  
  # set
  setMatrix <- function(newValue) {
    x <<- newValue
    cache <<- NULL
  }
  
  # get
  getMatrix <- function() {
    x
  }
  
  # cache
  cacheInverse <- function(solve) {
    cache <<- solve
  }
  
  # get cached
  getInverse <- function() {
    cache
  }
  
  list(setMatrix = setMatrix, getMatrix = getMatrix, cacheInverse = cacheInverse, getInverse = getInverse)
}


## Computes the inverse of the special "matrix" returned by
## makeCacheMatrix
cacheSolve <- function(y, ...) {
  # get cache
  inverse <- y$getInverse()

  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }

  data <- y$getMatrix()
  inverse <- solve(data)
  y$cacheInverse(inverse)
  
  # return
  inverse
}