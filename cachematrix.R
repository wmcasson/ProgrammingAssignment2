## This script demonstrates how to cache a matrix inversion result for later reference.
## This process may be useful when needing to repeat a computationally intensive process on data
## that may not have changed.


## When passed a square invertible matrix, makeCacheMatrix makes a special object that can 
## cache the matrix inverse once calculated.

makeCacheMatrix <- function(x = matrix()) {
  ## Set inverse to null
  inverse <- NULL
  
  ## Subfunction to special matrix object accessible outside current function.
  ## Nullify any existing inverse
  set <- function(y) {
    x <<- y
    inverse <-- NULL
  }
  
  ## Subfunction to retrieve stored matrix
  get <- function() x
  
  ## Subfunction to set cached inverse
  setinverse <- function(inv) inverse <<- inv
  
  #Subfunciton to retrieve cached inverse
  getinverse <- function() inverse
  
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## When passed a special matrix object generated with makeCacheMatrix, cacheSolve first checks to see if
## the matrix inverse has been previously cached.  If so, it returns the cached value.  If not, it generates
## the matrix inverse, caches it, and returns it.

cacheSolve <- function(x, ...) {
  ## Attempts to retrieve cached inverse
  inverse <- x$getinverse()
  
  ## If matrix value is already cached, retrieve and return it
  if(!is.null(inverse)) {
    message("Retrieving cached matrix inverse")
    return(inverse)
  }
  
  ## If matrix value was not already cached, retrieve matrix, calculate inverse, and cache it
  matrix <- x$get()
  inverse <- solve(matrix)
  x$setinverse(inverse)
  inverse
}