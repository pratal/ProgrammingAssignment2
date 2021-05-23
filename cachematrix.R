## The two functions makeCacheMatrix and cacheSolve are used to cache inverse
## of a matrix.

## The function makeCacheMatrix creates a special matrix object that can cache 
## the value of its inverse & returns a list of functions to - 
## set : set the value of the matrix
## get : get the value of the matrix
## setinverse : set the value of the inverse
## getinverse : get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() {
        x
    }
    setinverse <- function(inv) {
        inverse <<- inv
    }
    getinverse <- function() {
        inverse
    }
    list(
        set = set,
        get= get,
        setinverse = setinverse,
        getinverse = getinverse
    )
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix function. If the inverse has already been calculated (and  
## the matrix has not changed), the cacheSolve function retrieves the inverse 
## from the cache. Else, it computes the inverse and sets the value in the 
## cache of the matrix.

cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
    if (!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse
}
