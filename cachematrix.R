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
##
## Example:
## > A <- makeCacheMatrix(matrix(c(1,-1/4,-1/4,1), nrow=2))
## > cacheSolve(A)
## [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667
## > cacheSolve(A)
## cached inverse
## [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667
## > A$setMatrix(matrix(c(-1/4,1,1,-1/4), nrow=2))
## > cacheSolve(A)
## [,1]      [,2]
## [1,] 0.2666667 1.0666667
## [2,] 1.0666667 0.2666667
## >

## This function creates a special "matrix" object to store
## the original matrix as well as the inverse of that matrix
## for caching purposes.
makeCacheMatrix <- function(m = matrix()) {
    i <- NULL           ## initialize a variable in the lexical scope, aka function closure

    setMatrix <- function(x) {
        m <<- x         ## store the new matrix in the function closure scope
        i <<- NULL      ## reset the inverse of matrix in the function closure scope
    }

    ## return the matrix 'm' stored in the function closure scope
    getMatrix <- function() m

    ## store the inverse of matrix for caching in the function closure scope
    setInverse <- function(inverse) i <<- inverse

    ## return the inverse matrix stored in the function closure scope
    getInverse <- function() i

    ## return a list with custom methods to support matrix inversion
    list(setMatrix=setMatrix,
         getMatrix=getMatrix,
         setInverse=setInverse,
         getInverse=getInverse)
}


## This function returns the inverse of matrix from a
## cache stored in the given "matrix" object o.  If no
## cache is found, the inverse of matrix is computed and
## then stored in the cache for future re-use.
cacheSolve <- function(o, ...) {
    i <- o$getInverse()
    if(!is.null(i)) {   ## validate if an inverse matrix already exists in cache
        message("cached inverse")
        return(i)       ## return the cached inverse
    }
    m <- o$getMatrix()  ## get the original matrix from the function closure scope
    i <- solve(m)       ## compute the inverse matrix
    o$setInverse(i)     ## store the computed inverse to the function closure scope
    i
}

