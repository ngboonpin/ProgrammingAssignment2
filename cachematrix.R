## These functions receive special matrix, calculate and cache its inverse.

##  makeCacheMatrix creates a list containing a function to
##  1. set the value of the matrix
##  2. get the value of the matrix
##  3. set the value of inverse of the matrix
##  4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    
    #   Define function to set value of the matrix
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    #   Define function to get value of the matrix
    get <- function() x
    
    #   Define function to set inverse
    setInverse <- function(inverse) m <<- inverse
    
    #   Define function to get inverse
    getInverse <- function() m
    
    #   Return a list with the above four functions
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

#   This function computes the inverse matrix returned by the makeCacheMatrix.
#   If the inverse is calculated and no changed in value, this function will 
#   return the cache value.

cacheSolve <- function(x, ...) {
    #   Fetches the cache value for inverse
    m <- x$getInverse()
    
    #   If there is value in cache, it will give message "getting cache data" 
    #   and return the inverse value
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    #   If no value return, it will compute the value and cache it
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}