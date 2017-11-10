## The following two functions can be used to cache the inverse of a matrix

## makeCacheMatrix reads a matrix and returns a special matrix (a list actually)
## One can perform get, set, getinv (get the inverse), and setinv of the special matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## The cachSolve function reads the special matrix returned by makeCacheMatrix
## Then it check whether the inverse of the matrix exists
## If the inverse exists already, it returns the inverse matrix
## Otherwise it calculate the inverse by Solve(), set it, and return the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
