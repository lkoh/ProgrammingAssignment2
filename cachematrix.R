##  The two functions below are used to create a special object
## that stores a Matrix and cache's its inverse

## This function, "makeCacheMatrix", creates a list and 
## accomplishes the following:
##      1. Sets the value of the matrix
##      2. Gets the value of the matrix
##      3. Sets the value of the inverse
##      4. Gets the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL             ## Initializing variable for inverse
        set <- function(y) {
                x <<- y
                inv <<- NULL
                
        }
        
        get <- function() x
        setinverse <- function(invsol) inv <<- invsol
        getinverse <- function() inv
        list (set=set,get=get,setinverse=setinverse,getinverse=getinverse)
        
        
}


## The function below calculates the inverse of the special "matrix" 
## created with the above function.
## First, the function checks if the inverse has already been calculated.
## If so, it retrieves the inverse from the cache and skips the computations.
## If not, it calculates the inverse of the matrix and and sets the value of
## the inverse in the cache using the setinverse function.

## Make sure to assign a variable name to the output of the above function,
## makeCacheMatrix.  This output is a list.  Then run cacheSolve on this 
## variable (and NOT on the original matrix)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inv <- x$getinverse()   ## Assigns inverse from makeCacheMatrix
        if (!is.null(inv)) {
                message("Getting cached data")
                return(inv)                     ## uses cached value and exits
        }
        data <- x$get()                ## If no inv in Cache, inv matrix is calculated
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv       
}
