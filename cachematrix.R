## The following two functions, makeCacheMatrix and cacheSolve,
## allow for the possibility of caching a matrix inverse to avoid 
## the possibility of repeatedly making a computationally expensive
## function call (solve()) when not necessary. 


## makeCacheMatrix created a matrix object, not unlike a class,
## in that it includes getters and setters for a matrix inverse
## for that object.
makeCacheMatrix <- function(X = matrix()) {
        M <- NULL
        set <- function(Y) {
                X <<- Y
                M <<- NULL
        }
        get <- function() X
        setinverse <- function(matrix_inverse) M <<- matrix_inverse
        getinverse <- function() M
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## The cacheSolve function checks the cacheMatrix object to determine
## if the inverse matrix value has already been set. If so (i.e., if the
## value is not NULL), then that value is returned without further 
## computation. Otherwise, the inverse has not yet been computed. So,
## we use the solve() function to compute the inverse, store the value
## in the cacheMatrix object (for future use) and return the computed
## inverse.

cacheSolve <- function(X, ...) {
        ## Return a matrix that is the inverse of 'X'
        M <- X$getinverse()
        if(!is.null(M)) {
                message("getting cached data")
                return(M)
        }
        data <- X$get()
        M <- solve(data, ...)
        X$setinverse(M)
        M
}


