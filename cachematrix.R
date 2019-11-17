## The following functions enable the caching of inverse calculation for
## matrices.

#' `makeCacheMatrix` adds inverse caching capability to R matrices.
#'
#'  It returns an object with methods to retrieve and set the underlying matrix
#'  or the inverse. *Note*, it does not have methods to calculate the inverse
#'  itself. To calculate the inverse please use the `cacheSolve` function.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function()
        x
    
    setinverse <- function(inv)
        inverse <<- inv
    
    getinverse <- function()
        inverse
    
    list(
        set = set,
        get = get,
        setinverse = setinverse,
        getinverse = getinverse
    )
}


#' Calculate the inverse of a matrix
#'
#' `cacheSolve` is variation of the R `solve` function, that uses the CacheMatrix
#' to speed up operations in case of repeated inverse calculations.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    
    if (!is.null()) {
        message("getting cached data")
        return(m)
    }
    
    data <- x$get()
    m <- solve(x, ...)
    x$setinverse(m)
    m
}
