## Put comments here that give an overall description of what your
## functions do

## Initialise a special matrix object from an existing matrix

makeCacheMatrix <- function(x = matrix()) {

  cachedInverse <- NULL
  set <- function(y){
    x <<- y
    cachedInverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) cachedInverse <<- inverse
  getinverse <- function() cachedInverse
  list (set = set, get = get, 
        setinverse = setinverse,
        getinverse = getinverse)
}


## Accepts the special matrix object and returns its inverse. Does not perform the inverse if it was already calculated.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inverse <- x$getinverse()
  if (!is.null(inverse)){
    message("getting cached data")
    return(inverse)
  }
  
  data <- x$get()
  message("Performing a solve")
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
