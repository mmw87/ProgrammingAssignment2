#These functions take a matrix (x), calculate its inverse, cache the inverse, 
#and then retrieve the inverse from the cache.

#This function creates a matrix and can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

# The cachesolve function can calculate the inverse of the matrix created in the previous makeCacheMatrix function.
# cachesolve can retieve the the inverse of the matrix calculated in the previous function and stored in the cache. 


cachesolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}