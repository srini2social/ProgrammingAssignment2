## The objective is to cache the inverse of a matrix and return it 
## if one is available versus calculating it from scratch
## This is more of an efficiecny play

## This function creates a special "matrix" object that can 
## cache its inverse
## The first function, makeCacheMatrix creates a special "matrix", 
## which is really a list containing a function to

## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

#######################################################################
makeCacheMatrix <- function(x = matrix()) {
        
        #initialize the cached inverse matrix to NULL
        m <- NULL
        
        # set the function
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        #get the function
        get <- function() x
        
        # set the inverse matrix
        setMatrix <- function(matrix) m <<- matrix
        
        #get the inverse matrix
        getMatrix <- function() m
        
        #create a list with these functions and return it
        list(set = set, get = get,
             setMatrix = setMatrix,
             getMatrix = getMatrix)
}
########################################################################

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache. 

cacheSolve <- function(x, ...) {
        
        # Get the cached inverse matrix
        m <- x$getMatrix()
        
        #if not null, return it and write out a message indicating so
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        # if null, then we have to compute the inverse first
        data <- x$get()
        m <- solve(data, ...)
        x$setMatrix(m)
        
        # return the inverse matrix
        m
}
##########################################################################
# Test the functions out

# matrixData <- matrix(data = c(1,2,3,4), nrow = 2, ncol = 2)
# cached_Matrix <- makeCacheMatrix(matrixData)
# cacheSolve(cached_Matrix)

# check with just the solve function
# solve(matrixData)