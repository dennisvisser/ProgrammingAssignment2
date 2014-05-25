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
makeCacheMatrix <- function(x = matrix()) {
    # initial (empty) value for inverse calculation result
    m <- NULL
    
    # store matrix for which the inverse matrix should be calculated
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    # retrieve matrix for which the inverse matrix should be calculated
    get <- function() x
    
    # store the result of the inversion of a matrix
    setinverse <- function(solve) m <<- solve
    
    # retrieve the result of the inversion of a matrix
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
