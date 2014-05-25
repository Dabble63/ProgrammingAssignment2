## These are a pair of functions which cache the inverse of a matrix.
## The reason for these functions is to obviate the need for a recalculation of the inverse
## should the inverse be needed a second time for the same matrix

## To use these functions:
##     1) Create your "matrix" using makeCacheMatrix
##     2) Calculate the inverse of your matrix by calling cacheSolve and passing
##        the "matrix" created by the previous function as a parameter.

## This function creates a special "matrix" object that caches its inverse.

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
     	i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}

