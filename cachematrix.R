## Matrix inversion is usually a costly computation and there may be some benefit to caching the 
## inverse of a matrix rather than computing it repeatedly.  These functions will allow caching of
## the inverse of a matrix. makeCacheMatrix should be called before cacheSolve, as makeCacheMatrix
## sets up the cache and then cacheSolve utilizes the cache (or populates it).

## This function creates a special "matrix" object that can cache its inverse.  The available functions
## for this special "matrix" object are:
## get - returns the matrix
## set - sets the matrix
## getinverse - returns the inverse of the matrix
## setinverse - sets the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  
  get <- function() x
  setinverse <- function(inv) i <<- inv
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)){
    message("Getting cached data")
  } else {
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
  }
  i
}
