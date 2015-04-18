## The R script cachemataix.R provides functions useful in working with cache inverse matrices.
## The script provides two functions, makeCacheMatrix() for initializing the Cache Matrix and
## cacheSolve() for calculating the inverse matrix, accessing a cache version if the inverse
## has been computed already. It is required that you initialize with makeCacheMatrix() prior
## to using cacheSolve().


## makeCacheMatrix intializes a cache matrix providing functionality for setting and getting 
## the input matrix. In addition to, methods for getting and setting the inverse matrix.
## Input: Non Singular matrix 
## Output: list containing getter, setter for input matrix and getter, setter for inverse.
## Usage: 
##          y <- makeCacheMatrix( matrix(rnorm(3*3, mean=2, sd=0.5), nrow = 3, ncol = 3) )
##          y$get()
##          y$set(matrix)
##          y$getinv()
##          y$setinv(matrix)

makeCacheMatrix <- function(x = matrix()) {
    inv  <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(matinv) inv <<- matinv
    getinv <- function() inv
    list(set = set, get = get, 
         setinv = setinv, 
         getinv = getinv)    
}


## CacheSolve calculates the inverse of a non singular matrix.
## CacheSolve makes use of the cache matrix, if it exists otherwise
## the inverse is calculated cached and returned from the function.
## Input: list initialized via makeCacheMatrix()
## Output: Inverse matrix
## Usage:
##          cacheSolve(y)
## Test:
##          y$get() %*% cacheSolve(y) == I

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
