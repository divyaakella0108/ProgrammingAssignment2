
## functions does the Caching the Inverse of Matrix 

## This function creates a "matrix" that can cache its inverse.

makeCacheMatrix <- function(x = matrix())
  {
  inv <- NULL
  # set the value of matrix here
  set <- function(y) 
    {
      x <<- y
      inv <<- NULL
  }
  #get the value of matrix
  get <- function() x
  #set the value of Inverse matrix
  setInverse <- function(inverse) inv <<- inverse
  #get the value of Inverse matrix
  getInverse <- function() inv
  
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## returns the inverse matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
