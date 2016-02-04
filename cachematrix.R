## Matrix inversion is usually a costly computation and there may be some benefit 
# to caching the inverse of a matrix rather than compute it repeatedly. 

## This function creates a special "matrix" object 
# that can cache its inverse.
# List of functions:
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse matrix
# 4. get the value of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
    invMatrix <- NULL
    set <- function(y) {
        x <<- y
        invMatrix <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) invMatrix <<- inverse
    getInverse <- function() invMatrix
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


# This function computes the inverse of the special "matrix" 
# returned by makeCacheMatrix above.
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    invMatrix <- x$getInverse()
    if(!is.null(invMatrix)) {
        message("getting cached data")
        return(invMatrix)
    }
    data <- x$get()
    invMatrix <- solve(data, ...)
    x$setInverse(invMatrix)
    invMatrix
}
