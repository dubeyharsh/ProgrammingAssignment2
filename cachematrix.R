## The first funcstion creates a special "matrix" that is really just
## a list containing to set and get the value and inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## The second funcstion calculates the inverse of the said special "matrix"
## by first checking to see if it is stored in the cache or not; if it is stored
## the computation is skipped and the inverse directly returned, otherwise the
## cache is computed and then returned

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setinv(inv)
    inv
}
