## Introduction
#  Matrix inversion is usually a costly computation, so we exploit 
#  the possibility of caching the inverse of a matrix which may be
#  of benefit, specially in a scenario where repeated calculations
#  are performed. By doing this we can eventually save some valuable
#  computing time...


## function: makeCacheMAatrix
#  this function creates a cache to store the contents of the matrix
#  and its inverse.


makeCacheMatrix <- function(x = matrix()) {
    m<-NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


##  function: cacheSolve 
#   This function returns the inverse of a given matrix but
#   first check's whether if it has already been calculated.
#   If a cache for the inverse exists then it is retuned.
#   Otherwise, it is computed as usual, stored and returned.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
