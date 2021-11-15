## In order to avoid doing unnecessary multiple computations of matrix inversions. These functions check if an inversion has already been computed 
# and cached and returns the cached matrix if so. If done for the first time, the inverse is computed and chached. 

## This function creates an object that stores a matrix and cache's its inverse. The object is in fact a list that contains functions 
# to perform the following tasks: set the matrix, get the matrix, set the inverse of the matrix, get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() {x}
      setInverse <- function(inverse) inv <<- inverse
      getInverse <- function() {inv}
      list(set = set, get = get,
           setInverse = setInverse,
           getInverse = getInverse)
}


## This function calculates the inverse of the matrix, assuming that the matrix is invertible. Before it calculates the inverse it checks if 
## the inverse hasl already been computed. If so, it gets the inverse from the cache and skips the calculation itself. If no cached version of the
#matrix is available it calculates the inverse and set the iverxe in the cache using the setInverse function

cacheSolve <- function (x, ...) {
      ## Return a matrix that is the inverse of 'x'
      inv <- x$getInverse()
      if(!is.null(inv)){
            message("getting cached data")
            return(inv)
      }
      mat <- x$get()
      inv <- solve(mat, ...)
      x$setInverse(inv)
      inv
}