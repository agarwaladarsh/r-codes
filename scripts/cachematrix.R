## Put comments here that give an overall description of what your
## functions do
## These functions are a part of the Assignment for Week 3 of the R programming course
## as part of the JHU Data Science Specialization

## Write a short comment describing this function

## function to create a special matrix object that would cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        
        inverseMatrix <- NULL ## Setting the inverse matrix to Null initially
        
        setMatrix <- function(y) { ## Function to reassign values if matrix is new
                x <<- y
                inverseMatrix <<- NULL
        }
        
        getMatrix <- function() x ## Get the value of the matrix
        
        setInverse <- function(inverse) inverseMatrix <<- inverse ## Set value of inverse matrix
        
        getInverse <- function() inverseMatrix ## Get inverse matrix
        
        list(setMatrix=setMatrix, getMatrix=getMatrix, setInverse=setInverse, getInverse=getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverseMatrix <- x$getInverse() ## Get Inverse MAtrix from makeCacheMatrix Function
        
        if(!is.null(inverseMatrix)) {
                message("Getting Cached Inverse Matrix")
                return(inverseMatrix)
        }
        
        MatrixData <- x$getMatrix() ## Fetch original matrix data
        inverseMatrix <- solve(MatrixData,...) ## invert original matrix data
        x$setInverse(inverseMatrix) ## Set Inverse MAtrix data 
        
        return(inverseMatrix) ## return inverse matrix 
        
        
        
}
