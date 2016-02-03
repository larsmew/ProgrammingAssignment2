## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# This function caches the result of an (inverse) matrix, 
# potentially reduces the number of times this expensive computation is made.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list (set = set, get = get,
        setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function

# This functions returns the cached inverse matrix if it exists
# Otherwise, the inverse matrix is computed and cached.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}