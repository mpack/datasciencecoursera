## makeCacheMatrix and cacheSolve can be used to solve and cache the inverse
## of a matrix. The purpose of these functions is to increase the performance
## of repeated matrix inversion like in loops.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(matrix = matrix()) {
    cachedInverse <- NULL
    
    set <- function(newMatrix) {
        matrix <<- newMatrix
        cachedInverse <<- NULL
    }
    
    get <- function() matrix
    
    setInverse <- function(inverse) cachedInverse <<- inverse
    
    getInverse <- function() cachedInverse
    
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix
## has not changed), then it retrieves the inverse from the cache.

cacheSolve <- function(matrix, ...) {
    ## Return a matrix that is the inverse of 'matrix'
    inverse <- matrix$getInverse()
    
    if(!is.null(inverse)) {
        return(inverse)
    }
    
    data <- matrix$get()
    
    inverse <- solve(data)
    
    matrix$setInverse(inverse)
    
    inverse
}
