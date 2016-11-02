## This script contains two functions. The first takes a matrix input
## and creates a list object which contains the matrix, while providing
## the capability for caching its inverse. If the matrix component of the
## list is reset, the inverse cache is cleared.

## The second function takes this list as an input and attempts to evaluate
## the inverse of the matrix that it contains. First, the function checks
## the object's cache for the inverse matrix (in the event that it has been
## previously calculated) and returns it if present. Otherwise, the inverse
## is calculated and returned.


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    ## Return a list object containing the input matrix and a cache of its
    ## inverse (where it has been called to calculate previously)
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(x_inv) inv <<- x_inv
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by the
## makeCacheMatrix function. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should retrieve
## the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
