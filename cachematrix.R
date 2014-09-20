## Put comments here that give an overall description of what your
## functions do

# makeCacheMatrix: This function creates a special "matrix" object that
# can cache its inverse.

# object$setinverse() invokes the R matrix "solve" function
# object$getinverse() retrieves a cached inverse if available,
#   otherwise, it computes the inverse and caches it

# To see the original matrix use object$get()

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

# cacheSolve: This function computes the inverse of the special "matrix"
# returned by makeCacheMatrix above. If the inverse has already been
# calculated (and the matrix has not changed), then the cachesolve should
# retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        flush.console()
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    return(i)
}

# Example matrix
m <- matrix(data = c(2,0,0,2), nrow = 2, ncol = 2)

# Convert to cache object
M <- makeCacheMatrix(m)

# Show original matrix
M$get()

# Show inverse; uncached
cacheSolve(M)

# Show inverse; cached
cacheSolve(M)
