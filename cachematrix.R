## Functions that cache the inverse of a matrix
##
## Usage example: 
##
## > x <- makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))
## > cacheSolve(x)
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

## Create a special "matrix," which is a list containing a function to
## - set the value of the matrix
## - get the value of the matrix
## - set the value of the inverse matrix
## - get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set=set, get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}


## Calculate the inverse of 'x,' the special "matrix" created with the above function. Checks cache first to see if inverse has already been calculated and, if so, retrieves inverse and skips compuation. If inverse is not already in cache, calculates the inverse and stores the value of the 
## inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m = x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
