## Specific functions to cache the inverse of a matrix 
## by caching the first inverse computation
##
## Comment on implementation difference between provided sample.
## It makes no sense to make inner working of makeCacheMatrix depended
## of specific call order in cacheSolve. The actual interface of  
## makeCacheMatrix should hide that and limit the interface to the 
## actual concern it solves


## Function sets a variable given in the constructor and caches the result of a function call
## after first call to cacheCall the function will always return the first result independent 
## of further cacheCall calls with different fun types

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
            x <<- y
            m <<- NULL
    }
    get <- function() x
    cacheCall <- function(fun,...){
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        m <<- fun(x,...)
        m
    } 
    list(set = set, get = get,
            cacheCall = cacheCall)
}


## using the solve function to cache the result 

cacheSolve <- function(x, ...) {
        x$cacheCall(solve,...)
}
