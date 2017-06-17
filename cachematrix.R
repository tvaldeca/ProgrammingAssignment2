
## The functions makeCacheMatrix and cacheSolve when paired together will calculate the inverse of a matrix
## and cache the result. If the inverse has already been cached, the cached result will be returned 
## instead of recalculating. The input matrix must be a square invertible.

## makeCacheMatrix accepts a matrix argument and returns a list of functions
## set will replace the input matrix
## get will retreive the input matrix
## setinverse will replace the inverse matrix
## getinverse will retrieve the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
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


## cacheSolve accepts an object from makeCacheMatrix, checks the cache for a pre-calculated result
## and returns the result if it exists; otherwise, the inverse of the input matrix will be calculated and cached.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  ## Return a matrix that is the inverse of 'x'
  m
}
