## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    cachedInverseMatrix <- NULL
    
    # Retrieves the original matrix
    getData <- function() x
    
    # Retrieves the inverse matrix
    getInverseMatrix <- function() cachedInverseMatrix
    
    # Sets the inverse matrix
    setInverseMatrix <- function(inversedMatrix) {
        cachedInverseMatrix <<- inversedMatrix
    }
    
    list(getData=getData, 
         getInverseMatrix=getInverseMatrix, 
         setInverseMatrix=setInverseMatrix)
}


## Write a short comment describing this function
cacheSolve <- function(matrixToInvert, ...) {
    inverseMatrix <- matrixToInvert$getInverseMatri()
    if(!is.null(inverseMatrix)) {
        message("Returning cached inverse matrix")
        return(inverseMatrix)
    }
    # if there is no cached inverse matrix, calculate and store it
    matrixData <- matrixToInvert$getData()
    inverseMatrix <- solve(matrixData, ...)
    matrixToInvert$setInverseMatrix(inverseMatrix)
    inverseMatrix
}
