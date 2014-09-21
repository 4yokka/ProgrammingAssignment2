## Code below can calculate and cache the inverse of invertible matrix.


## This function creates a "special" matrix object which can cache its inverse

makeCacheMatrix <- function(matrx = matrix()) {
  inv <- NULL
  
  # Saves the input matrix in enclosing environment and flushes its inverse
  set <- function(y) {
    matrx <<- y
    inv <<- NULL
  }
  
  # Returns original matrix
  get <- function() {
    matrx
  }
  
  # Saves matrix inverse in the enclosing environment
  setinverse <- function(inverse) {
    inv <<- inverse
  }
  
  # Returns matrix inverse
  getinverse <- function() {
    inv
  }
  
  # Returns "special" matrix object in a form of list of the functions defined above
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Function computes the inverse of the special "matrix" returned by makeCacheMatrix() function.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the inverse gets retrieved from the cache.

cacheSolve <- function(x, ...) {
  # Tries to get cached inverse of the "special" matrix x
  inv <- x$getinverse()
  
  # If the inverse of the matrix has been cached before (woot!), returns the inverse from cache
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # Otherwise gets original matrix...
  data <- x$get()
  
  # Calculates its inverse...
  inv <- solve(data, ...)
  
  # Caches the inverse for future use...
  x$setinverse(inv)
  
  # And returns the inverse. All done!
  inv
}
