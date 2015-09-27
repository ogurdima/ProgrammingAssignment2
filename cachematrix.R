## Functions that add objects with cacheable inversable matrices support

##  This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(newMatrix) {
    x <<- newMatrix
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(newInv)  {
    inv <<- newInv
  }
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

##  This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if (!is.null(inv))
  {
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
