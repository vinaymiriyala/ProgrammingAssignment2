## The following functions can be used to solve the inverse of a matrix. The inverse of a matrix will be 
## returned readily if it has already been cached before, instead of being computed again.
## This saves time when doing time consuming computations.

## The first function, makeCacheMatrix creates a special matrix, 
## which is really a list containing a function to

## set the value of the matrix
## get the value of the matrix
## set the inverse of the matrix
## get the inverse of the matrix

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


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
## above. If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache. If the inverse has not yet been calculated
## it computes the inverse and caches the value with the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
