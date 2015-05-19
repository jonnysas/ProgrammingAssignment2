## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix - This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  ## set - will set the new value (matrix) and reset the "inv" arg (clear the cahce when the matrix is changed)
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x ## retrive the value of the matrix
  setInv <- function(solve) inv <<- solve
  getInv <- function () inv
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}

## cacheSolve - This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInv(inv)
  inv
}