## Functions to create an inverse matrix
## and cache the result
##
## Usage example:
## l <- makeCacheMatrix(matrixToInvert)
## cacheSolve(l)
## cacheSolve(l)

## makeCacheMatrix provides the caching mechanism
## for inverse matrix computation

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve computes the inverse matrix
## It is assumed that the matrix is always invertible

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
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
