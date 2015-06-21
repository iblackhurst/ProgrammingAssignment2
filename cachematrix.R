## These functions use R's scoping rules to eliminate unnecessary
## recalculation of a matrix inverse. Once the inverse has been 
## calculated, it will pull the inverse from a cache instead of
## recalculating it every time.

## makeCacheMatrix creates a list of four functions in a new environment.
## This list can be created and assigned to a name, like 'y',
## using y <- makeCacheMatrix().
## For this to be meaningful, a matrix z that has an inverse, 
## such as can be created
## with a command like z <- matrix(1:4, ncol = 2), can be used.
## You can call the set function from y on matrix z by a command like
## y$set(z).
## The original matrix can be inspected with y$get().
## setinv and getinv are called when using cacheSolve.

makeCacheMatrix <- function(x = matrix()) {
  a <- NULL
  set <- function(b) {
    m <<- b
    a <<- NULL
  }
  get <- function() m
  setinv <- function(inv) a <<- inv
  getinv <- function() a
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function takes as its first argument the output of
## makeCacheMatrix (a list). For this to be meaningful, 
## the set function in makeCacheMatrix should have been
## called on a matrix with an inverse. Once it has been, cacheSolve
## will calculate the inverse of the matrix if it is being called
## for the first time, or it will pull the answer from the cache
## if it has already been calculated.
## If it is being pulled from the cache, you will see
## "getting cached data" printed before the inverse is.
## The "..." arguments are used to pass more arguments to the solve
## function, if that is necessary.
## In this example, cacheSolve(y) is what might be run.

cacheSolve <- function(x, ...) {
  a <- x$getinv()
  if(!is.null(a)) {
    message("getting cached data")
    return(a)
  }
  data <- x$get()
  a <- solve(data, ...)
  x$setinv(a)
  a
}

