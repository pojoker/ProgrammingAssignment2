## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix())  {
        matrixinversion <- NULL
        set <- function(y) {
                x <<- y
                matrixinversion <<- NULL
        }
        get <- function() x
        setinversion <- function(inversion) matrixinversion <<- inversion
        getinversion <- function() matrixinversion
        list(set = set, get = get,
             setinversion = setinversion,
             getinversion = getinversion)
}

## Write a short comment describing this function
#return the inversion of the matrix if it exists, if it doesn't exsit, calculates it and update
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinversion()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinversion(inv)
    inv
}

