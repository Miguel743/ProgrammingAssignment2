## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  set_inverse <- function(inverse) inv <<- inverse
  get_inverse <- function() inv
  list(set=set, get=get, set_inverse=set_inverse, get_inverse=get_inverse)
}


# The following function returns the inverse of the matrix. It first checks if the inverse has already been computed. 
# If so, it gets the result and skips the computation. If not, it computes the inverse, sets the value in the cache via
# set_inverse function.

# This function assumes that the matrix is always invertible.
cacheSolve <- function(x, ...) {
  inv <- x$get_inverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$set_inverse(inv)
  inv
}