## Functions to create a special matrix object and cache its matrix inverse.

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    # Set the value of the matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    # Get the value of the matrix
    get <- function() x
    
    # Set the value of the inverse matrix
    setInverse <- function(inverse) inv <<- inverse
    
    # Get the value of the inverse matrix
    getInverse <- function() inv
    
    # Return a list of the four getter/setter functions
    list(set = set, 
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated, it retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    
    # Check if the inverse is already cached
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    # Retrieve the matrix, compute the inverse, and set it in the cache
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv
}
