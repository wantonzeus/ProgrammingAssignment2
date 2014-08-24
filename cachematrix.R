## wantonzeus 8/24/2014

## This pair of functions help cache the result of matrix inverse calculation.
## The point is to reduce the amount of computing by caching the result of an
## inverse calculation

## This function creates the caching of the matrix and the inverse; it also
## accesses the cached matrix.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() {x}
  setinverse <- function(inverse) {i <<- inverse}
  getinverse <- function() {i}
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function actually calculates the inverse of the matrix that was set in
## the previous function. If there is an inverse already cached, then this function
## will return the cached inverse.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

