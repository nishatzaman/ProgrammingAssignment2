##This function creates a list containing the functions set(), get(), setInverse() and getInverse(). The set() function takes a matrix 'y' as a parameter and the value of 'x' is redefined in the parent environment. The get() function returns the matrix 'x'. setInverse() sets the 'inverse' variable in the parent environment to a new inverse value. getInverse() returns the 'inverse' variable from the parent environment.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setInverse <- function(newInverse) inverse <<- newInverse
    getInverse <- function() inverse
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## This function checks whether the inverse of the matrix has been cached. If it has, then the cached inverse is returned. Otherwise, the matrix is retrieved from the list using the get() function and its inverse is calculated. The calculated inverse is returned.
cacheSolve <- function(x, ...) {
        inverse <- x$getInverse()
        if(!is.null(inverse)) {
            return(inverse)
        }
        matrix <- x$get()
        inverse <- solve(matrix)
        x$setInverse(inverse)
        inverse
}
