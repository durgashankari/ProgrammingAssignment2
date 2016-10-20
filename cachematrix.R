## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        invers <- NULL
        set <- function(y) {
                x <<- y
                invers <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) invers <<- inverse
        getInverse <- function() invers
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invers <- x$getInverse()
        if (!is.null(invers)) {
                message("Cached data")
                return(invers)
        }
        matx <- x$get()
        invers <- solve(matx, ...)
        x$setInverse(invers)
        invers 
}
