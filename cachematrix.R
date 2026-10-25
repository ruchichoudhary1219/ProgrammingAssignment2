#Matrix inversion can be computationally expensive, especially when the
#inverse needs to be calculated multiple times. To improve efficiency,
#these functions use a caching mechanism to store the matrix and its inverse.
#makeCacheMatrix creates a list of functions to:
#- Set the matrix value
#- Get the matrix value
#- Set the cached inverse of the matrix
#- Get the cached inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
i <- NULL
set <- function(y) {
x <<- y
i <<- NULL
}
get <- function() x
setinverse <- function(inverse) i <<- inverse
getinverse <- function() i
list(set = set,
get = get,
setinverse = setinverse,
getinverse = getinverse)
}



#This function returns the inverse of the matrix.
#It first checks if the inverse is already stored in the cache.
#If the inverse is available, it returns the cached value
#without performing the calculation again.
#If the inverse is not cached, it calculates the inverse,
#stores the result using the setinverse function, and returns it.
#This function assumes that the matrix is invertible.

cacheSolve <- function(x, ...) {
i <- x$getinverse()
if (!is.null(i)) {
message("getting cached data")
return(i)
}
data <- x$get()
i <- solve(data, ...)
x$setinverse(i)
i
}
