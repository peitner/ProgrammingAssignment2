## Functions cache and retrieve an input matrix, also compute and cache or retrieve
## the inverse of the input matrix

## Function makeCacheMatrix
## Creates a list consisting of four functions:
##  'set' caches the value of the input matrix
##  'get' retrieves the cached matrix
##  'setInverse' caches the matrix inverse
##  'getInverse' retrieves the cached inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Function cacheSolve
## Finds inverse of matrix using built-in R function 'solve'
## Solves for inverse & caches it if inverse has not been previously found
## Otherwise retrieves it from cache

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
    data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
