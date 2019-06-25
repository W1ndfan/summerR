
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    get <- function() x
    get_inv <- function() inv
    set_inv <- function(result) inv <<- result 
    list(get = get, get_inv = get_inv, set_inv = set_inv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$get_inv()
    if (is.null(inv)) {
        y <- x$get()
        result <- solve(y)
        x$set_inv(result)
        return(result)
    } else {
        message('retrive from cache')
        return(x$get_inv())
    }
}
