## Put comments here that give an overall description of what your
## functions do

## a function that makes a list with a cache matrix, the inverse of
## the matrix, and subroutines that retreave and store/cache the 
## matrix inverse. 

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    get <- function() x
    setinv <- function(minverse) inv <<- minverse
    getinv <- function() inv
    
    list(set = set, get = get, setinv = setinv, getinv = getinv)
    
}


## a function that computes the inverse of a matrix but first checks to see
## if the inverse is already cached. If the inverse is not cached it 
## stores the computed inverse.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data<-x$get()
    inv <- solve(data)
    x$setinv(inv)
    inv
}

