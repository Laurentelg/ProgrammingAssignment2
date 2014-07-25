## Put comments here that give an overall description of what your
## functions do

## This function will cmpute the Matrix Inverve
## If the Matrix is already in cahe it will not compute the inverse again but 
## use the cahe instead
## The trick is to use the operator <<- to set a variable in the the global env to be reused accross functions
## Testing if the object is already around means the invert has already been computed thus using the cache
## To run this assignment your first need to "Instantiate the object by calling makeCacheMatrix and then call CacheSolve
## on the result

makeCacheMatrix <- function(x = matrix()) {

  xt <- NULL
  ## The set function sets the private variables matrix x and invert matrix xt
  ## x: Origninal matrix
  ## xt: Invert Matrix
  set <- function(y) {
    x <<- y
    xt <<- NULL
  }
  ## Just return the original matrix
  get <- function() x 
  setSolve <- function(solve) {
    xt <<- solve
  }
  getSolve <- function() xt
  ## return a list of function to act on the "class Member"
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    xt <- x$getSolve()
    if(!is.null(xt)) {
      message("getting cached data")
      return(xt)
    }
    data <- x$get()
    xt <- solve(data,...)
    x$setSolve(xt)
    xt
}
