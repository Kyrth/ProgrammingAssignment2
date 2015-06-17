## makeCacheMatrix - builds and caches a matrix
## cacheSolve - takes built/cached matrix and returns inverse

## makeCacheMatrix
## expects: 'x' an invertible square matrix
## returns: a list of functions
##      1. set the matrix
##      2. get the matrix
##      3. set the inverse
##      4. get the inverse

makeCacheMatrix <- function(x = matrix()) {

    inverse <- NULL
    
    # Set the matrix
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    # Get the matrix
    get <- function() x
    
    # Set the inverse
    setinv <- function(inv) inverse <<- inv
    
    # Get the inverse
    getinv <- function() inverse
    
    # Build the function list
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## cacheSolve
## expects: 'x' output of makeCacheMatrix()
## returns: inverse of the original matrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv
    
    # Matrix inverse previously found
    if (!is.null(inv)) {
        message("getting cached inverse")
        return(inv)
    }
    
    # Calculate the inverse matrix
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
