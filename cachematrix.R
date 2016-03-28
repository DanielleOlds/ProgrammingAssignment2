## Below is the R code to do matrix inversion. It will cache the the inverse of a matrix 
## rather than computing it repeatedly. To do this, it uses a pair of functions. The first
## creates the matrix that can cache its inverse. The second function computes the inverse of 
## the matrix and will retrieve the inverse from the cache if the inverse has already been
## calculated.

## This is the first function (makeCacheMatrix) which creates 
## a matrix that will cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse 
        getinv <- function() inv
        list(set=set, get=get, 
        	setinv=setinv, 
        	getinv=getinv)
}

## This is the second function (cacheSolve) which computes the inverse of 
## the matrix from the first function above (makeCacheMatrix). If the inverse has
## already been calculated, then this function will retrieve this 
## inverse from the cache. 

cacheSolve <- function(x, ...) {
        inv = x$getinv()
        if (!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
