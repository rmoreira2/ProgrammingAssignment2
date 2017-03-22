## Those functions creates a matrix with cache functions to store its inverse
## So evaluating the inverse will first check if its inverse already was stored on the cache


## Receives an invertible matrix
## returns a list with functions to get / set the matrix and get/set the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
    invCache <- NULL
    set <- function(y) {
        x <<- y
        invCache <<- NULL
    }
    get <- function() x
    setInverse <- function(inv) invCache <<- inv
    getInverse <- function() invCache
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Receives a matrix with list of functions created with makeCacheMatrix
## Returns the matrix inverse using the solve function. 
## If the inverse was already evaluated it is cached.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv
}
