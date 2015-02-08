## The functions in this module compute the inverse of matrix
## only once for a given matrix.
##
## Usage:
##   . create a special matrix object
##       m <- makeCacheMatrix(matrix(1:4, nrow=2, ncol=2))
##   . compute the inverse of the matrix
##       cacheSolve(m)
##
## Note:
##   There will be a message of 'cached inverse' if the
##   function cacheSolve(m) is called from the second time
##   onward.


## This function creates a special "matrix" object to store
## the original matrix as well as the inverse of that matrix
## for caching purposes.
makeCacheMatrix <- function(m = matrix()) {
    i <- NULL
    set <- function(x) {
        m <<- x         ## store the new matrix
        i <<- NULL      ## reset the inverse of matrix
    }
    get <- function() m

    ## store the inverse of matrix for caching
    setInverse <- function(inverse) i <<- inverse

    getInverse <- function() i
    list(set=set, get=get,
         setInverse=setInverse,
         getInverse=getInverse)
}


## This function returns the inverse of matrix from a
## cache stored in the given "matrix" object o.  If no
## cache is found, the inverse of matrix is computed and
## then stored in the cache for future re-use.
cacheSolve <- function(o, ...) {
    i <- o$getInverse()
    if(!is.null(i)) {
        message("cached inverse")
        return(i)       ## return the cached inverse
    }
    m <- o$get()        ## get the original matrix
    i <- solve(m)       ## compute the inversed matrix
    o$setInverse(i)     ## cache the computed inverse
    i
}

