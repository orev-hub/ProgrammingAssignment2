## The functions below cache the inverse of a matrix.

## The function makeCacheMatrix creates a special "matrix" object 
## that can cache its inverse.

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
                   setcache = setcache,
                   getcache = getcache)
}


## The function cacheSolve computes the inverse of the special 
## "matrix" returned by the makeCacheMatrix function above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
                i <- x$getinverse()
                if (!is.null(i)) {
                            message ("getting cached data")
                            return(i)
                }
                data <- x$get()
                i <- solve(data, ...)
                x$setinverse(i)
                i
}
