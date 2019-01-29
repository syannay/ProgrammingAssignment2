## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function gets a matrix and sets the matrix in x
## The function sets the inv (inverse) value to NULL
## The function defines the internal functions of get, setinv (set inverse) and getinv (get inverse)

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(i) inv <<- i
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function
## This function implements a single tone for inverse.
## if inverse already exists it returns its value otherwise it calculates the inverse matrix
## set it to the single tone and return its value

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting inverse matrix from cache")
    return(i)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setinv(inv)
  inv
}
