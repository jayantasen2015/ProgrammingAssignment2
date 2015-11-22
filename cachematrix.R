## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        mtr <- NULL
        set <- function(y) {
                x <<- y
                mtr <<- NULL
        }
        get <- function() x
        setinv <- function(invvalue) mtr <<- invvalue
        getinv <- function() mtr
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        mtr <- x$inv()
        if(!is.null(mtr)) {
                message("getting cached data")
                return(mtr)
        }
        data <- x$get()
        mtr <- solve(data, ...)
        x$setinv(mtr)
        mtr
}