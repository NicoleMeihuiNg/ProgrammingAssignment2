## two funcations with the purpose of cache the inverse of a matrix

## The first function - makeCacheMatrix function creates a special "matrix" 
## which can be inverse using the second function - cacheSolve

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list (set = set, 
        get = get, 
        setinv = setinv, 
        getinv = getinv)
}



## The cacheSolve function solve the inverse of the special matrix in the first
## function makeCacheMatrix. It will first check if the inverse has been solved
## before and the matrix has not changed, if so, it retrieve the earlier
## calculated inverse without computing again.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data,...)
  x$setinv(inv)
  inv
}
