# ----------------------------------------------------
# Matrix inversion
# ----------------------------------------------------
# Matrix inversion is usually a costly computation and there may be some benefit to 
# caching the inverse of a matrix rather than compute it repeatedly.
# Here assignment is to write a pair of functions that cache the inverse of a matrix.

# For this assignment, assumed that the matrix supplied is always invertible.

# The function 'makeCacheMatrix' has below functions
#   1) set the value of the matrix
#   2) get the value of the matrix
#   3) set the value of inverse of the matrix
#   4) get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    invt <- NULL
    set <- function(y) {
        x <<- y
        invt <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) invt <<- inverse
    getinverse <- function() invt
    list(set = set, 
         get = get, 
         setinverse = setinverse, 
         getinverse = getinverse)
}

# The following function calculates the inverse of the special "matrix" 
# created with the above function. However, it first checks to see if the inverse has 
# already been calculated. If so, it gets the inverse from the cache and skips the computation. 
# Otherwise, it calculates the inverse of the data and sets the value of the inverse
# in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
    invt <- x$getinverse()
    if(!is.null(invt)) {
        message("getting cached data.")
        return(invt)
    }
    data <- x$get()
    invt <- solve(data)
    x$setinverse(invt)
    invt
}
