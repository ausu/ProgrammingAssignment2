## makeCacheMatrix() and cacheSolve() functions can be used to cache the inverse
## of a matrix (a potentially costly computation) instead of recomputing it 

## makeCacheMatrix() function creates a special "matrix" object that can cache 
## its inverse.  makeCacheMatrix() takes an invertible square matrix
## solve() returns inverse of square matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y){
        x <<- y
        i <<- NULL
    }
    
    get <- function() x 
    setInverse <- function(solve) i <<- solve
    getInverse <- function() i
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## cacheSolve() function computes inverse of special "matrix" object returned by
## makeCacheMatrix(). If the inverse has already been computed (and matrix has
## not changed), cacheSolve() gets inverse from cache. Otherwise, inverse is
## computed and set in cache via setInverse()

cacheSolve <- function(x, ...) {
    i <- x$getInverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setInverse(i)
    i
}
