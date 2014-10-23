## Matrix inversion is ususally a time-comsuming computation. Caching
## the inverse of a matrix will improve the speed.

## makeCacheMatrix creats a special matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
               free <- NULL
               set <- function(y) {
                      x <<- y
                      free <<- NULL
               }
               get <- function() {x}
               setinv <- function(inv) free <<- inv
               getinv <- function() {free}
               list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## cacheSolve computes the inverse of the special matrix returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        free <- x$getinv()
        if(!is.null(free)) {
                message("getting cached matrix")
                return(free)
        }
        data <- x$get()
        free <- solve(data, ...)
        x$setinv(free)
        return(free)
}
