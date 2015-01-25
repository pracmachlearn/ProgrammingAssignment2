##The aim of these functions is to calculate
##the inverse of a matrix. The result then will be
##cached so that it has not to be re-calculated in case
##of repeated calls to cacheSolve function.

## This function creates a special matrix

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


## This functions returns the inverse of a matrix. If the result was cached
## it just returns the value otherwise the result will be calculatede and cached.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
