## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

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
