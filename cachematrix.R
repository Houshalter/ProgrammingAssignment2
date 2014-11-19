## These functions create an object which can store
## a matrix and the result of inverting it.

## Creates a CacheMatrix object from a matrix. Allows
## the inverse matrix to be stored after it is computed
## once.

makeCacheMatrix <- function(x = matrix()) {
    ## Initialize a variable for storing the inverse matrix.
    inv <- NULL
    ## Function to change the matrix stored and delete the cache.
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    ## Function to get the value of the matrix stored.
    get <- function() x
    ## Internal function for setting the cached inverse value.
    setInverse <- function(inverse) inv <<- inverse
    ## Internal function for retreiving the cached inverse value.
    getInverse <- function() inv
    ## Returns a list of all the functions defined above.
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)   
}


## Computes the inverse of a CacheMatrix object. Caches
## the value and uses it the next time it is called.

cacheSolve <- function(x, ...) {
    ## Returns the cached value if it has been computed before.
    inv <- x$getInverse()
    if(!is.null(inv)){
        return(inv)
    }
    ## Otherwise computes it and then stores it in the cache.
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setInverse(inv)
    inv
}
