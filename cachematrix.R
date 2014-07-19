# cachematrix.R:  A matrix that can cache its inverse.
# Implements a CacheMatrix--a matrix that uses R's lexical
# scoping rules to cache its inverse.

# makeCacheMatrix:  Convert a matrix into a CacheMatrix that can
#                   cache its inverse.
#
# Input:  matrixValue:  The matrix to be converted into a CacheMatrix.
#                       matrixValue is assumed to be invertible.
#
# Return:  A list containing four functions.
#          setValue:  A function that stores the value of the CacheMatrix
#                     and clears the cache.
#          getValue:  A function that retrieves the previously stored
#                     value of the CacheMatrix.
#          setInverse:  A function that stores the inverse of the
#                       stored matrix, calculated elsewhere.
#          setInverse:  A function that retrieves the previously stored
#                       value of the inverse of the stored matrix.
makeCacheMatrix <- function(matrixValue = matrix()) {
    matrixInverse <- NULL
    setValue <- function(matrixValueProvided) {
        matrixValue <<- matrixValueProvided
        matrixInverse <<- NULL
    }
    getValue <- function() matrixValue
    setInverse <- function(inverse) matrixInverse <<- inverse
    getInverse <- function() matrixInverse
    list(setValue = setValue, getValue = getValue,
          setInverse = setInverse, getInverse = getInverse)
}

# cacheSolve:  Calculate or retrieve the inverse of a CacheMatrix
#
# Input:  x:  A CacheMatrix created with makeCacheMatrix
#         ...:  A variable number of other arguments supplied to 
#               the solve function.
#
# Return:  The inverse of the value of the supplied CacheMatrix
# Side Effects:  The diagnostic  message 'Returning cached data.' 
#                is emitted for debugging purposes when a cached
#                inverse is returned.
# Errors:  Stop with 'must be square' when the CacheMatrix is not
#          square.
#          Stop with 'system is exactly singular' when the CacheMatrix
#          is not invertible.
#          Other errors indicated by the solve() function
cacheSolve <- function(x, ...) {
    result <- x[['getInverse']]()
    if (!is.null(result)) {
        message('Returning cached data.')
        return(result)
    }
    theMatrix <- x[['getValue']]()
    result <- solve(theMatrix)
    x[['setInverse']](result)
    result
}
