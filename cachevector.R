
makeVector <- function(x = numeric()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmean <- function(mean) m <<- mean
    getmean <- function() m
    list(set = set, get = get,
         setmean = setmean,
         getmean = getmean)
}

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


cachemean <- function(x, ...) {
    m <- x$getmean()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- mean(data, ...)
    x$setmean(m)
    m
}

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

newmat <- function(n = 1){
    matrix(rnorm(n*n, mean=2, sd=0.5), nrow = n, ncol = n)
    #matrix
}

x <- makeVector(c(1:10))
cachemean(x)

is.singular.matrix(newmat(3))

y <- makeCacheMatrix(newmat(3))
is.singular.matrix(y$get())
cacheSolve(y)
