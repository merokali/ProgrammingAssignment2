## Put comments here that give an overall description of what your
## functions do

## this function creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        ## initiate inv as NULL, placeholder for matrix inverse
        inv <- NULL
        
        ## define set function to assign the new inv value
        set <- function(y) {            
                x <<- y
                inv <<- NULL  ## reset inv to NULL, if there is a new matrix
        }
        
        ## define the get function to return the value of the matrix argument
        get <- function() x
        
        ## assign value of inv in parent environment
        setinverse <- function(inverse) inv <<- inverse
        
        ## get the value of inv where called
        getinverse <- function() inv
        
        ## put all functions in list that allows to call functions with th $ operator
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

##  ## this function computes inverse of matrix returned by makeCacheMatrix function
## if inverse has already been calculated (and matrix has not changed)
## then the function will retrieve the inverse from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()         ## getting the new matrix and storing in data
        inv <- solve(data, ...) ## inverse the matrix stored in data using the solve function and storing in new inv
        x$setinverse(inv)       ## passing the inversed matrix to parent function/environment ?      
        inv
}