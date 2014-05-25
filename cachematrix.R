## Cache the inverse of a matrix
## 
## Calculate the inverse of a matrix, and save the inverse
## in a cached matrix object
## If the inverse of the matrix was calculated before, don't
## calculate it again after subsequent calls to the function,
## but return the cached object to prevent unnecessary
## calculations.

##
## function: makeCacheMatrix 
##
## purpose: Return a list of functions to accommodate the caching of inverted matrices
##
## Included functions:
##
## set: store matrix for which the inverse matrix should be calculated
## get: retrieve matrix for which the inverse matrix should be calculated
## setinverse: store the result of the inversion of a matrix
## getinverse: retrieve the result of the inversion of a matrix
##
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    # return list of helper functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


##
## function: cacheSolve 
##
## purpose: Return a matrix that is the inverse of 'x'
## If the inverse of the matrix was calculated before, don't
## calculate it again after subsequent calls to the function,
## but return the cached object to prevent unnecessary
## calculations.
##
cacheSolve <- function(x, ...) {
    # Check if inversion was calculated before.
    m <- x$getinverse()
    # If yes: return cached inversion data
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    # If no: calculate the inversion, store in cache, and return inversion
    data <- x$get()
    # Calculate inverse matrix
    m <- solve(data, ...)
    # Cache inverse matrix
    x$setinverse(m)
    # Return inverse matrix
    m
}
