## These functions make an object which stores a matrix
## and caches its inverse

## The first function returns a list of functions which:
## set a matrix; get a matrix; 
## set its inverse; get the cached inverse
## the inverse is NULL until set

makeCacheMatrix <- function(x = matrix()) {
            inv <- NULL
            set <- function(y) {
                    x <<- y
                    inv <<- NULL
            }
            get <- function() x
            setinv <- function(s) inv <<- s
            getinv <- function() inv
            list(set = set, get = get,
                 setinv = setinv,
                 getinv = getinv)
 
}


## The next function retrieves the cached inverse from 
## the makeCacheMatrix object. If the inverse is not yet cached,
## the function gets the matrix from the makeCacheMatrix object, 
## computes the inverse inv, and sets the value of inv in the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        	inv <- x$getinv()
            if(!is.null(inv)) {
                    message("getting cached data")
                    return(inv)
            }
            mat <- x$get()
            inv <- solve(mat, ...)
            x$setinv(inv)
            inv

}
