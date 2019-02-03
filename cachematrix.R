## This scripts contains two functions that cache the inverse of a given matrix

## The next function, makeCacheMatrix returns a list of the functions: set the
# value of a matrix, get the value of a matrix, set the value of its inverse
# and get the value of its inverse

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x 
      setsolve <- function(solve) m <<- solve
      getsolve <- function() m
      list(set = set, get = get,
           setsolve = setsolve,
           getsolve = getsolve)
      
}


## The function cacheSolve computes the inverse of the special "matrix" returned by
## makeCacheMatrix, assuming that the the matrix is always invertible

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      m <- x$getsolve()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setsolve(m)
      m
}

myma1 <- matrix(1:4,nrow = 2, ncol = 2)
cacheSolve(makeCacheMatrix(myma1))