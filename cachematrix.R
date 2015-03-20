## cachematrix.R
##
## Cursera : R programming : assignment 2
##
## Functions that calculates inverse matrix or get inverse matrix
## from cache if it is already calculated before.

## Function that emulating "class-list" for manipulation of
## cached inversed matrixes. Must be called first.

makeCacheMatrix <- function(x = matrix()) {

    inv_mat <- NULL
    set <- function(y) {
        x <<- y
        inv_mat <<- NULL
    }
    get <- function() x
    setinvmat <- function(im) inv_mat <<- im
    getinvmat <- function() inv_mat
    list(set = set,
         get = get,
         setinvmat = setinvmat,
         getinvmat = getinvmat)
}

## Fuction that calculate inverse martix or get result from cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'

    inv_mat <- x$getinvmat()
    if(!is.null(inv_mat)) {
        message("Getting cached data.")
        return(inv_mat)
    }
    data <- x$get()
    inv_mat <- solve(data, ...)
    x$setinvmat(inv_mat)
    inv_mat
}

## end of cachematrix.R