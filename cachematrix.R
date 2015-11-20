## This function creates a special "matrix" object, calculate its inverse, and cache it

makeCacheMatrix <- function(x = matrix()) {
    # m is an empty variable
    m <- NULL
    # set changes the matrix stored in the main function
    set <- function(y) {
      # x <<- y substitutes matrix x with matrix y in the main fn, not in set
      x <<- y
      # m <<- NULL restores the null value of the inverse, m, b/c the old inverse is not needed, new inverse is calced w/ cacheSolve
      m <<- NULL
    }
    # get returns the matrix x, does not require input
    get <- function() x
    # setinverse & getinverse store the value of the input in m into the main fn, then return it (getmean)
    setinverse <- function(inversemat) 
    m <<- inversemat
    getinverse <- function() m
    # stores the 4 fns in makeVector
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix
## If the inverse has already been calculated, the this fn will retrieve the inverse from the cache
## Input of cacheSolve is the object where makeCacheMatrix is stored

cacheSolve <- function(x, ...) {
  # verifies that the value of getinverse exists in memory then returns it
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  # gets the matrix stored w/ makeCacheMatrix
  data <- x$get()
  # calcualtes the inverse of the vector
  m <- solve(data, ...)
  # stores it in m
  x$setinverse(m)
  m
}