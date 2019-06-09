## Pair of functions that help reduce the time to repeatedly calculate the 
## inverse of a matrix by using a cache

## Creates a list with functions to cache a matrix and its inverse

makeCacheMatrix <- function(matrix = matrix()) {
	cachedInverse <- NULL
	
        set <- function(y) {
                matrix <<- y
                cachedInverse <<- NULL
        }
        get <- function() matrix

        setInverse <- function(inverse) cachedInverse <<- inverse

        getInverse <- function() cachedInverse
        
	  list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Function that checks if the inverse for a matrix is already calculated
## and cached. If cached, return the cached inverse else, calculate the inverse
## for the matrix and cache the result. 

cacheSolve <- function(matrix, ...) {
        inverse <- matrix$getInverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- matrix$get()
        inverse <- solve(data)
        matrix$setInverse(inverse)
        inverse
}
