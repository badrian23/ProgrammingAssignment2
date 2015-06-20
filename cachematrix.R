## makeCacheMatrix: This function creates a special "matrix" object 
## that can cache its inverse.

## Creates a matrix object : "matrice"

makeCacheMatrix <- function(x = matrix()) {
  inversed <- NULL
  set <- function(y) {
    x <<- y
    inversed <<- NULL
  }
  get <- function() return(x)
  set_inversed <- function(solve) inverse <<- solve
  get_inversed <- function() inverse
  return(list(set = set, 
              get = get, 
              set_inversed = set_inversed, 
              get_inversed = get_inversed))
}

## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already
## been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inversed <- x$get_inversed()
  if(!is.null(inversed)) {
    message("getting cached data")
    return(inversed)
  }
  data <- x$get()
  inversed <- solve(data, ...)
  x$set_inversed(inversed)
  inversed
}