## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## xi is the variable that should contain the inverse of matrix x.
makeCacheMatrix <- function(x = matrix()) {
  xi <- NULL
  set <- function(y) {
          x <<- y
          xi <<- NULL
  }
  get <- function() x
  setInverse <- function(x_inverse) xi <<- x_inverse
  getInverse <- function() xi
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function
## xi is the varible that should contain the matrix inverse.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
   xi <- x$getInverse()
  if(!is.null(xi)) {
          message("getting cached data")
          return(xi)
  }
  data <- x$get()
  xi <- solve(data, ...)
  x$setInverse(xi)
  xi
}
