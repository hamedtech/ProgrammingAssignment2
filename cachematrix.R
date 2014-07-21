## The functions in this file compute the inverse
## of a matrix and cache that result.  The code 
## assumes that the matrix in question has an 
## inverse.  A typical use case may look like:
##
## x <- matrix(cbind(1, 2, -5, -8), nrow = 2, ncol = 2)
## xw <- makeCacheMatrix(x)
## s <- cacheSolve(xw$get())
## xw$setSolution(s)
## print solution
## print(xw$getSolution())

## Create an "object" that wraps a matrix object.
## The "matrix wrapper" object provides functions
## get and set to get/set the underlying matrix 
## object.  Functions getSolution and setSolution 
## are used to get/set the solution found by using
## the solve() function.

makeCacheMatrix <- function(x = matrix()) {
    solution <- NULL
    set <- function(y) {
        x <<- y
        solution <<- NULL
    }
    get <- function() x
    setSolution <- function(solved) solution <<- solved
    getSolution <- function() solution
    list(set = set, get = get,
         setSolution = setSolution,
         getSolution = getSolution)
}



## Return the cached solution if one exists.  
## Otherwise, compute the solution by calling
## solve() and store it in the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    solution <- x$getSolution()
    if(!is.null(solution)) {
        message("getting cached data")
        return(solution)
    }
    data <- x$get()
    solution <- solve(data, ...)
    x$setSolution(solution)
    solution
}
