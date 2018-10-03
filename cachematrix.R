## The functions defined below are used to create a special object that stores
## an invertible matrix and cache its inverse.

## This function creates a special "matrix" object that can cache its inverse. 
## It returns a list containing a function to
##
## -- set the value of the matrix
## -- get the value of the matrix
## -- set the value of the inverse
## -- get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    minv <- NULL
    set <- function(y) {
        x <<- y
        minv <<- NULL
    }
    get <- function() x
    setinv <- function(solve) minv <<- solve
    getinv <- function() minv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and
## the matrix has not changed), then the cachesolve retrieves the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    minv <- x$getinv()
    if(!is.null(minv)) {
        message("getting cached data")
        return(minv)
    }
    ma <- x$get()
    minv <- solve(ma)
    x$setinv(minv)
    minv
}
