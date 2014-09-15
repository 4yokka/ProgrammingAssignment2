## Put comments here that give an overall description of what your
## functions do

## This function creates a "special" matrix object which can cache its inverse

makeCacheMatrix <- function(matrx = matrix()) {
  inv <- NULL
  set <- function(y) {
    matrx <<- y
    inv <<- NULL
  }
  get <- function() {
    matrx
  }
  setinverse <- function(inverse) {
    inv <<- inverse
  }
  getinverse <- function(){
    inv
  }
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Function computes the inverse of the special "matrix" returned by makeCacheMatrix() function
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
