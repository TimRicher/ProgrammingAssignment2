## This function allows you to calculate the inverse of a matrix
## and to cache the values of the matrix so that they can be 
## reused.  The values can be retrieved from cache instead of being 
##recalculated.

## The makeCacheMatrix function creates a special "matrix" that can
## cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    local_matrix <- NULL
    set <- function(y) {
        cache_x <<- y
        local_matrix <<- NULL
    }
    get <- function() cache_x
    setInverse <- function(cache_matrix) local_matrix <<- cache_matrix
    getInverse <- function() local_matrix
    list(
        set = set,
        get = get,
        setInverse = setInverse,
        getInverse = getInverse
    )
}

## The cacheSolve function computes the inverse of the special "matrix"
## returned by makeCacheMatrix.  If the inverse has already been 
## calculated (ant the matrix has not changed) then cacheSolve retrieves
## the inverse from the cache.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    local_matrix <- x$getInverse()
    if(!is.null(local_matrix)) {
        message("getting cached data")
        return(local_matrix)
    }
    matrix_start <- x$get()
    matrix_end <- solve(matrix_start, ...)
    x$setInverse(matrix_end)
    matrix_end
}
# Test Results:
# 
# > mtrx <- makeCacheMatrix()
# > mtrx$set(matrix(c(3,2,2,3),2,2))
# > mtrx$get()
#      [,1] [,2]
# [1,]    3    2
# [2,]    2    3
# > cacheSolve(mtrx)
#      [,1] [,2]
# [1,]  0.6 -0.4
# [2,] -0.4  0.6
# > cacheSolve(mtrx)
# getting cached data
#      [,1] [,2]
# [1,]  0.6 -0.4
# [2,] -0.4  0.6
# 
# > mtrx <- makeCacheMatrix()
# > mtrx$set(matrix(c(30,20,20,30),2,2))
# > mtrx$get()
#      [,1] [,2]
# [1,]   30   20
# [2,]   20   30
# > cacheSolve(mtrx)
#        [,1] [,2]
# [1,]  0.06 -0.04
# [2,] -0.04  0.06
# > cacheSolve(mtrx)
# getting cached data
#        [,1] [,2]
# [1,]  0.06 -0.04
# [2,] -0.04  0.06