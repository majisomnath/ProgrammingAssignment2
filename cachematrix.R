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
    ## initialize values
    invt <- NULL
    set <- function(y) {
        ## set the values of parent environment
        x <<- y
        invt <<- NULL
    }
    
    get <- function() x
    ## set the inverse in parent environment
    setInverse <- function(inverse) invt <<- inverse
    getInverse <- function() invt
    ## prepare a list
    list(set = set, 
         get = get, 
         setInverse = setInverse, 
         getInverse = getInverse)
}

# The following function calculates the inverse of the special "matrix" 
# created with the above function. However, it first checks to see if the inverse has 
# already been calculated. If so, it gets the inverse from the cache and skips the computation. 
# Otherwise, it calculates the inverse of the data and sets the value of the inverse
# in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
    ## return a matrix which is inverse of x
    invt <- x$getInverse()
    
    ## check if value already in cache, else make calculation
    if(!is.null(invt)) {
        message("getting cached data.")
        return(invt)
    }
    data <- x$get()
    ## inverse using 'solve' function
    invt <- solve(data)
    x$setInverse(invt)
    invt
}
