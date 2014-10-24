## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# makeCacheMatrix creates a special "list", which is really a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
 inve <- NULL
    set <- function(y) {
        x <<- y
        inve <<- NULL
    }                                                    # set the value of the matrix
    get <- function() x                                  # get the value of the matrix
    setinverse <- function(inverse) inve <<- inverse     # set the value of inverse of the matrix
    getinverse <- function() inve                        # get the value of inverse of the matrix
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}


## Write a short comment describing this function
# The following function calculates the inverse of the matrix.
# It first checks to see if the inverse has already been calculated. 
# If so, it gets the inverse from the cache and skips the computation. 
# Otherwise, it calculates the inverse of the matrix and sets the value of 
# the inverse in the cache via the setinverse function.
cacheSolve <- function(x, ...) {
	  inve <- x$getinverse()
    if(!is.null(inve)) {
        message("getting cached data.")
        return(inve)
    }                                                      # Check to see if the inverse has already been calculated and get the inverse from the cache

    data <- x$get()
    inve <- solve(data)
    x$setinverse(inve)                                     # Calculates the inverse of the matrix and sets the value of the inverse in the cache
    inve

## Return a matrix 'inve' that is the inverse of 'x'
}
