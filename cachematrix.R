## This function is used for Caching the Inverse of a Matrix

## This function is a special matrix that is invertible

makeCacheMatrix <- function(x = matrix()) {
    z <- NULL
    set <- function(y) {
        x <<- y
        z <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) z <<- inverse
    getinverse <- function() z
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}  

## This function returns the inverse of the main matrix as the output.

cacheSolve <- function(x, ...) {
    z <- x$getinverse()
    if (!is.null(z)) {
        message("getting cached data")
        return(z)
    }
    data <- x$get()
    z <- solve(data, ...)
    x$setinverse(z)
    z
        ##  z Returns a matrix that is the inverse of 'x'
}
