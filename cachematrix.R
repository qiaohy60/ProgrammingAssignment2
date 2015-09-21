## Put comments here that give an overall description of what your
## functions do

## Create a special matrix object which can cache its reverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setreverse <- function(r) m <<- r
        getreverse <- function() m
        list(set = set, get = get,
             setreverse = setreverse,
             getreverse = getreverse)
}


## Compute the reverse of the matrix returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getreverse()
        if(!is.null(m)) {
                message("getting cached reverse")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setreverse(m)
        m
}
