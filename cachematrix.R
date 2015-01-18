## Functions and data structures to calculate the 
## inverse of a matrix and cache it.
##
## First call makeCacheMatrix using a matrix as 
## parameter to create the data structures.
##
## Then use cacheSolve on the result of the 
## makeCacheMatrix call.

## makeCacheMatrix creates a list of functions to
## - set a new matrix (set)
## - get the current matrix (get)
## - set a new inverse matrix (set_inv)
## - get the current inverse matrix (get_inv)
## 
## The result is to be used as parameter to cacheSolve.

makeCacheMatrix <- function(x = matrix()) {
  ## initialise cached inverse
  inv <- NULL
  ## set new matrix
  set <- function(y) {
    x <<- y
    ## initialise cached inverse for new matrix
    inv <<- NULL
  }
  ## get matrix
  get <- function() x
  set_inv <- function(new_inv) inv <<- new_inv
  get_inv <- function() inv
  list(set = set, get = get,
       set_inv = set_inv,
       get_inv = get_inv)
}


## cacheSolve does the actual calculation of the matrix using
## a cached result if already available. The parameter is
## a return value of makeCacheMatrix.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$get_inv()
  
  ## check if the inverse is already cached
  if(!is.null(inv)) {
    ## then just return it and exit the function
    message("getting cached data")
    return(inv)
  }
  else {
    ## otherwise get the matrix
    matrix <- x$get()
    ## and calculate the inverse
    inv <- solve(matrix, ...)
    ## and cache it
    x$set_inv(inv)
    ## and return it
    return(inv)
  }
}
