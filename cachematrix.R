
## The inverse of the matrix "x" is called "s", the
## makeCacheMatrix function will assign the value of
## the matrix with he function set and then the get
## function will return the matrix value "x".
## "setinv" calculates the inv fo the matrix
## and "getinv" returns the inverse matrix.

## Finally the list retuned by the makeCacheMatrix
## function sets the properties of the new cache matrix.

## The "<<-" operator will store the value of the
## matrix and it's inverse in a superassignment way
## so that the state of the current matrix is stored.

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setinv <- function(solve) s <<- solve
    getinv <- function() s
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve will take a matrix only if it is already a
## makeCacheMatrix which means it is an object with the
## properties from the list ('get', 'set', 'getinv', 'setinv')
## it will check if 's' (the inverse) is different form NULL
## if it is then it doesn't calcuate it, it returns s
## and the message 'getting cached data'.
## If it doesn't exist then the inverse 's' is calculated
## and stored and returned.

cacheSolve <- function(x, ...) {
    s <- x$getinv()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setinv(s)
    s
}

## I used the examples from the forum to try my code if 
## if you want to do so:
## > m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
## > m1
## [,1]  [,2]
## [1,]  0.50 -1.00
## [2,] -0.25  0.75
## > myMatrix_object <- makeCacheMatrix(m1)
## > cacheSolve(myMatrix_object)
## [,1] [,2]
## [1,]    6    8
## [2,]    2    4
## > cacheSolve(myMatrix_object)
## getting cached data
## [,1] [,2]
## [1,]    6    8
## [2,]    2    4
## > n2 <- matrix(c(5/8, -1/8, -7/8, 3/8), nrow = 2, ncol = 2)
## > myMatrix_object$set(n2)
## > cacheSolve(myMatrix_object)
## [,1] [,2]
## [1,]    3    7
## [2,]    1    5
## > cacheSolve(myMatrix_object)
## getting cached data
## [,1] [,2]
## [1,]    3    7
## [2,]    1    5