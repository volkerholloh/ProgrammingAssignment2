## makeCacheMatrix is a function which supports matrix operations by providing
## functions to set and get matrices and their inverted matrices.

## cacheSolve is a function to calculate the inverse of a matrix and to store
## the invert into the matrix structure.


## makeCacheMatrix is a function which provides four methods to set and get a
## a matrix and its inverse matrix.
## set - assigns the value of the original matrix to the matrix of the structure
## get - reads the value of the matrix out of the matrix of the structure
## setinverse - assigns the value of the inverse matrix of the structure
## getinverse - reads the value of the inverted matrix of the structure


makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL    ## the matrix is changed

        set <- function(y) {
                x <<- y  		## the given matrix is stored in x
                inv <<- NULL   	## the matrix is changed
        }
        get <- function() x 	## gets the given matrix from structure
        setinverse <- function(invert) inv <<- invert 	## assigns the inverse matrix to the structure
        getinverse <- function() inv 	## reads inverse matrix from structure
        list(set = set, get = get,		## returns the list of the provided functions
             setinverse = setinverse,
             getinverse = getinverse)
}



## cacheSolve calculates the inverse matrix of the given matrix, if it is not
## calculated yet. The inverse matrix is then stored into the structure 
## makeCacheMatrix has provided. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        invert <- x$getinverse()	## reads the inverse matrix

        if(!is.null(invert)) {		## tests if inverse matrix is existing
                message("getting cached data")  ## if existing, then message is displayed
                return(invert)					## and inverted matrix is returned 
                								## and function is ended.
        }
        data <- x$get()				## the original matrix is read
        invert <- solve(data, ...)	## the inverted matrix is calculated
        x$setinverse(x)				## the inverted matrix is saved into structure
        invert  					## the inverted matrix is returned
}


