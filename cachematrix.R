
## Caching the Inverse of a Matrix

## Function makeCacheMatrix creates a "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL
        
        ## Set the matrix value
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        ## Get the value of the matrix
        get <- function() x
        
        ## Set the value of the inverse matrix
        setinv <- function(solve) inv <<- solve
        
        ## Get the inverse matrix
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Function cacheSolve computes the inverse of a "matrix" returned by makeCacheMatrix
## If the inverse has already been calculated 
## the cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        
        ## Get the inverse of the matrix
        inv <- x$getinv()
        
        ## Return the cached data if the inverse is already cached
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        ## Get initial matrix
        data <- x$get()
        
        ## Calculate the inverse of the matrix
        inv <- solve(data, ...)
        
        ## Cache the inverse
        x$setinv(inv)
        
        ## Return the (calculated) inverse 
        inv
}