## The purpose of these functions is to cache a matrix and 
## the inverse of a matrix if already calculated.

## The makeCacheMatrix stores the matrix you pass to it
## and stores the inverse matrix if it has already been calculated

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) m <<- solve
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Returns either the stored inverse matrix or calculates the inverse

cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        
        ## Return a matrix that is the inverse of 'x'
		m
}
