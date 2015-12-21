## Put comments here that give an overall description of what your
## functions do

## Creation of the object makeCacheMatrix, it is used to solve 
## the inverse of the matrix and calculates the inverse one time.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    
    #set the value of the matrix
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    #get the value of the matrix
    get <- function() x
    
    #set the value of the inverse of the matrix
    setsolve <- function(solve) m <<- solve
    
    #get the value of the inverse of the matrix
    getsolve <- function() m
    
    #return our list
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## cacheSolve return the inverse of the cacheMatrix object

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    # retrieve from cache
    m <- x$getsolve() 
    
    if(!is.null(m)) { 
        message("getting cached data")
        return(m)
    }
    
    # get data
    data <- x$get()  
    # solve data
    m <- solve(data, ...) 
    # set data
    x$setsolve(m)	      
    # return data
    m		     
}
