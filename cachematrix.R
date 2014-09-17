## This profile contains two major funtions:  
## the first one, makeCacheMatrix, creates matrix object,
## and the second one, cacheSolve, inverse the matrix.  
## The functions cache the value of the matrix.  
## When we need the matrix again, it can be looked up in the caches rather than recomputed.


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
  

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    inverse <- x$get()
    m <- solve(inverse, ...)
    x$setinverse(m)
    m
}
