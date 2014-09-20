## A solution to Programming Assignment 2 of the R Programming course.
## See https://www.coursera.org/course/rprog for further details.
##
## The functions `makeCacheMatrix` creates an object containing a matrix,
## which caches the inverse matrix when the function `cacheSolve` is called on it.
##
## E.g.
## m <- matrix( ... )
## cm <- makeCacheMatrix(m)
## inverseM <- cacheSolve(cm)
##
## Future calls of cacheSolve will be much faster, since they now return a cached value.



## Creates a new cache matrix object, which can be used in the cacheSolve function.
##
## This object is actually a list of functions as follows:
##   * get()          Gets the contained matrix.
##   * set(y)         Replaces the contained matrix with the specified matrix y.
##   * getInverse()   Gets the cached inverse matrix, if any.
##   * setInverse(i)  Caches the inverse matrix i.
##
## The optional argument x can be used to specify the matrix contained in the object.
## If no value is specified, then an empty 1x1 matrix is used.
## The matrix contained within this object may be replaced using the set function.
##
makeCacheMatrix <- function(x = matrix()) {
        
        # The cached inverse value
        inverse <- NULL

        # Gets the contained matrix
        get <- function() {
                x
        }
        
        # Sets a new contained matrix, and invalidates the cached inverse.
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        
        # Gets the cached inverse matrix value
        getInverse <- function() {
                inverse
        }
        
        # Sets the cached inverse matrix value
        setInverse <- function(i) {
                inverse <<- i
        }
        
        # Return our cacheMatrix object
        list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}



## Finds the inverse matrix of the specified cacheMatrix object
## (i.e. as returned by the makeCacheMatrix function above).
##
## This inverse is cached, so future calls to cacheSolve
## for the same cacheMatrix object return immediately.
##
##
## Argument x must specify the cacheMatrix object to solve.
##
## Any further arguments are passed to the solve function,
## which is used to compute the inverse matrix.
cacheSolve <- function(x, ...) {
    
        # Get and return the cached inverse matrix if we have it
        inverse <- x$getInverse()
        if (!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        
        # Otherwise, compute the inverse and store it in the cache
        matrix <- x$get()
        inverse <- solve(matrix, ...)
        x$setInverse(inverse)
        inverse
}
