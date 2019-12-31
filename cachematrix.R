## These functions take advantage of lexical scoping to avoid unnecessary computation 
## by caching the inverse of the matrix created in makeCacheMatrix with cacheSolve

## This function creates a matrix and stores the inverse of that matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


## This function makes use of the makeCacheMatrix to retrieve the inverse of the matrix from the cache

cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        matrix <- x$get()
        m <- solve(matrix, ...)
        x$setinv(m)
        m 
        ## Return a matrix that is the inverse of 'x'
}
