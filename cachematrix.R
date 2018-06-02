## These functions create a matrix and the methods to cache its inverse

## The makeCacheMatrix function creates a matrix object 
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
      invX <- NULL
      set <- function(y) {
        x <<- y
        invX <<- NULL
      }
      get <- function() x
      setInv <- function(iX) invX <<- iX
      getInv <- function() invX
      list(set = set, get = get,
           setInv = setInv,
           getInv = getInv)
}


## The cacheSolve function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache 

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      xInv <- x$getInv()
      if(!is.null(xInv)) {
        message("getting cached data")
        return(xInv)
      }
      data <- x$get()
      xInv <- solve(data, ...)
      x$setInv(xInv)
      xInv
}
