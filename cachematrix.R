makeCacheMatrix <- function(x = matrix()) {
  ## initializing objects x and inv
  inv <- NULL
  ## defining the "behaviors"of functions for objects of type makeCacheMatrix, namely set, get,
  ## setInverse, and getInverse
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(Inverse) inv <<- Inverse
  getInverse <- function() inv
  ## create a new object by returning a list()
  list(set = set, get =  get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## call the getInverse() function on the input object
  inv <- x$getInverse()
  ## check whether the result is NULL
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  ## if !is.null(inv) is FALSE, get the vector from the input object, calculate the inverse, 
  ## and set the inverse in the input object. afterwards, return inv
  mat.data <- x$get()
  inv <- solve(mat.data, ...)
  x$setInverse(inv)
  inv
}