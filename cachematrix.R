## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a function object.  The object contains 2 variables the matrix and it's
## inverse and provides functions to set and get the matrix or it's inverse.
##
## note - no error checking is performed. I don't know if this is within the scope of
##        the assignmant.
##
## test with  a<-makeCacheMatrix(matrix(1:4,2,2))
##            cacheSolve(a)
##            a$get() %*% a$getinverse()
##            result is the Identity matrix

makeCacheMatrix <- function(x = numeric()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## cacheSolve calculates the inverse of a makeCacheMatrix object matrix if it has not
## been previously calculated and caches the result in the makeCacheMatric object.
## The inverse matrix of the makeCacheMatrix object is always returned as a result of the call.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached inverse matrix")
        return(inv)
    }
    inv <- solve(x$get())
    x$setinverse(inv)
    inv
}