## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a special matrix, which is a list containing the functions to
## set, get, setInverse, getInverse
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y = matrix()) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setInverse <- function(inv) inverse <<- inv
    getInverse <- function() inverse
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Checks to see if the inverse of the matrix x is cached.
## If so, returns the cached value. If not, calculates the inverse and then returns it
cacheSolve <- function(x, ...) {
    inverse <- x$inverse
    if (!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    matrix <- x$get
    inverse <- solve(x, ...)
    x$setInverse(inverse)
    inverse
}