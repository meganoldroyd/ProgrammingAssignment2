## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
      ## set the value of the matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
      ## get the value of the matrix
    get <- function() x
    
      ## set the value of the inverse
    setinv <- function(inverse) inv <<- inverse
    
      ## get the value of the inverse
    getinv <- function() inv
    
      ##special object to return
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}



## This function computes the inverse of the special "matrix" returned by 
## the above function. If the inverse has already been calculated (and the
## matrix has not changed), the function retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$setinv()
    
      ## If inverse is already set, use cached data
    if(!is.null(inv)) {
        message("getting cached data")
        return inv
    }
    
      ## Otherwise, need to get the data and solve for the inverse of x
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
    
}

