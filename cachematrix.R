## Put comments here that give an overall description of what your
## functions do

## This function will take a matrix as an input during construct and make it as a special matrix 
## This special matrix will have the set, he get, the setinverse and the getinverse functions

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(  set = set
       , get = get
       , setInverse = setInverse
       , getInverse = getInverse)
}


## The following function calculates the Inverse of the special "vector" 
## it first checks to see if the Inverse has already been calculated. If so, it gets the mean from the cache and skips the computation. 
## Otherwise, it calculates the Inverse of the data and sets the value of the Inverse in the cache via the setInverse function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  m <- x$get()
  inv <- solve(m, ...)
  x$setInverse(inv)
  inv
}
