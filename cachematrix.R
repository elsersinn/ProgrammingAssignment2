## The aim is to be able to cache costly calculations in orden to avoid repeating them. 
## This is achieved with two functions: makeCacheMatrix creates a especial matrix that can store its
## inverse; cacheSolve calculates the inverse of a special matrix only if there is no cached data.




## makeCacheMatrix creates a special "matrix" capable of storing its inverse. It has the following methods:
## set: Store the matrix
## get: Retrieve the matrix
## setinverse: Store inverse of the matrix
## getinverse: Retrieve inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        
        ## Initial parameters
        
        inv <- NULL
        
        ## Define methods
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        
        ## Return list
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}


## cacheSolve has a makeCacheMatrix "matrix" as input. If the input has a stored inverse, it returns it.
## Else, the function will calculate the inverse and store it in the makeCacheMatrix "matrix".

cacheSolve <- function(x, ...) {
        
        ## If there is an stored inverse return it.
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }        
        
        
        ## Else, calculate the inverse and store it.
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        
        ## Return the inverse.
        inv
        
}
