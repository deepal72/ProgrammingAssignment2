## This script involves the use of functions and cache operator (<<-) to compute, cache and return the inverse of a matrix; with the purpose of reusabilty of cached data to save computation time for lengthy operations. 

## This function primarily has a set function which defines the variables used for the matrix and its inverse in a cached format. 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(Inverse) inv <<- Inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function computes the inverse of the matrix using the previous function to return the cached inverse of the specified matrix

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
