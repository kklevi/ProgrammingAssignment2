## Put comments here that give an overall description of what your
## functions do

## This function is used to create a special matrix that
## can bet set using $set() and when passed into cacheSolve
## the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        im <- NULL
        m <- NULL
        set <- function(y){
                m <<- y 
                im <<- NULL
        }
        get <- function() m
        setinverse <- function(inv) im <<- inv
        getinverse <- function() im
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function is passed in a special matrix and will
## return its inverse, and set the inverse, if it is not
## already set

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        im <- x$getinverse()
        if(!is.null(im)){
                message("getting cached data")
                return(im)
        }
        data <- x$get()
        im <- solve(data)
        x$setinverse(im)
        im
}
