## Caching the inverse of a matrix.

##This function creates a matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Computes the inverse of the matrix created by the above function 'makeCacheMatrix'.
##If the inverse is already calculated,then it will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting data which is cached")
    return(inv)
  }
## Return a matrix that is the inverse of 'x'.        
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
        


