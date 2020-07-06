## The functions "makeCacheMatrix" and "cacheSolve" were writen to cache the 
## inverse of a matrix

## The function "makeCacheMatrix" creates a special "matrix" object that can cache
##its inverse

makeCacheMatrix <- function(x = matrix()) {
    invm <- NULL
    set <- function(y){
            x <<- y
            invm <<- NULL
    }
    get <- function() x
    setinvm <- function(inverse) invm <<- inverse  
    getinvm <- function() invm
    list (set = set, get = get, setinvm = setinvm, getinvm = getinvm)
}


## The function "cacheSolve" computes the inverse of the special "matrix" returned
## by "makeCacheMatrix". If the inverse has already been calculated 
##(and the matrix has not changed), then "cacheSolve" retrieves the inverse from
## the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    invm <- x$getinvm()
    if(!is.null(invm)) {
        message("getting cached data")
        return(invm)
    }
    data <- x$get()
    invm <- solve(data, ...)
    x$setinvm(invm)
    invm
}
