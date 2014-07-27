## Create special matrices that can cache their inverse

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  ## new CacheMatrix, inverse is not calculated yet, so set to NULL
  i <- NULL
  
  ## store a new matrix, and reset the inverse to NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  ## return the stored matrix
  get <- function() x
  
  ## store the inverse matrix
  setinverse <- function(inverse) i <<- inverse
  
  ## return the stored inverse matrix
  getinverse <- function() i
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and
## the matrix has not changed), then cacheSolve should retrieve the inverse
## from the cache.

cacheSolve <- function(x, ...) {
  ## get the stored inverse matrix
  i <- x$getinverse()
  
  if (!is.null(i)) {
    ## we found the cached inverse matrix, so we return that
    message("getting cached data")
    return(i)
  }
  
  ## we don't have a cached inverse yet, so we calculate, store and return
  ## the inverse matrix
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}
