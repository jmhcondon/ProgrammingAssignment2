
## This R function is an example of how to cache the 
## results of a time and resource expensive computation in R.

## As an example, the inverse of an input matrix is cached
## after the first time it is solved.  Subsequent calls retrieve 
## the cached inverse matrix result, a much faster and economical approach.

## makeCacheMatrix accepts a square invertibble matrix x. 
        ## It creates a list, containing the functions to:
        ## 1. set the value of the matrix
        ## 2. get the value of the matrix
        ## 3. set the value of the inverse matrix
        ## 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        
        ## 1. set the matrix
        
        set <- function(y) {
                ## the <<- super assignment operator is used to assign 
                ## to a cache in the global environment
                
                x <<- y
                inv <<- NULL
        }
        
        ## 2. get the matrix
        
        get <- function() x
        
        ## 3. sset the inverse matrix value
        setinverse <- function(inverse) inv <<- inverse
        
        ## 4. get the inverse matrix value
        getinverse <- function() inv
        
        ## return the 4 functions in a list
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## The cacheSolve function solves the inverse of the matrix
        ## using the input from makeCacheMatrix
        ## It returns the inverse of the original matrix from cache if
        ## it has already been calculated, or uses R's solve function ic not

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inv <- x$getinverse()
        
        ## see if the inverse matrix is already cached - if so 
        ## return the cached inverse matrix with an appropriate message
        
        if(!is.null(inv)) {
                message("getting cached data.")
                return(inv)
        }
        ## the matrix inverse hazsn't been solved.  Get the input matrix, solve it, 
        ## call set to cache the inverse result
        
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        
        ## return the inverse matrix
        
        inv  
        
        
}
