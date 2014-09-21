## These functions take the inverse of a matrix and cache
## the solution for future use if the same matrix is used

## This function caches the inverse of a matrix object

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list(set = set, get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)
}


## This function solves the inverse of the matrix from makeCacheMatrix
## unless the matrix is unchanged, in which case the solution is
## retrieved from the cache

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
        ## Return a matrix that is the inverse of 'x'
}