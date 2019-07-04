## Creates a matrix and caches the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
    if(!is.matrix(x)) {
        message("Error: argument must be a matrix")
        return(NULL)
    }
    
    if(ncol(x) != nrow(x)) {
        message(paste("Error: argument must be a square matrix. The one you passed has", nrow(x), "rows and", ncol(x), "columns"))
        return(NULL)
    }
    
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

## The first time it calculates the inverse matrix. In subsequent calls it returns the cached inverse matrix
cacheSolve <- function(matrixToInvert, ...) {
    inverseMatrix <- matrixToInvert$getInverseMatrix()
    if(!is.null(inverseMatrix)) {
        message("Returning cached inverse matrix")
        message(inverseMatrix)
        return(inverseMatrix)
    }
    # if there is no cached inverse matrix, calculate and store it
    matrixData <- matrixToInvert$getData()
    inverseMatrix <- solve(matrixData, ...)
    matrixToInvert$setInverseMatrix(inverseMatrix)
    message("Calculating inverse matrix and caching it...")
    message("Inverse matrix: ")
    message(inverseMatrix)
    inverseMatrix
}

####################################################################
## TESTING 
####################################################################

## Calls the 3 unit tests
test <- function() {
    testInvertMatrix()
    testRectangularMatrix()
    testNotAMatrix()
}

# Create a matrix, calculate and cache inverse matrix
testInvertMatrix <- function() {
    message("Test: create a matrix, calculate and cache inverse matrix")
    plainMatrix <- matrix(c(3,1,4,2), ncol = 2, nrow = 2)
    newMatrix = makeCacheMatrix(x=plainMatrix)
    cacheSolve(newMatrix)
    cacheSolve(newMatrix)
    cacheSolve(newMatrix)
}

# Create a rectangular matrix and get an error (a matrix must be square to be inverted)
testRectangularMatrix <- function() {
    message("Test: create a rectangular matrix and get an error")
    plainMatrix <- matrix(1:6, nrow = 3, ncol = 2)
    newMatrix = makeCacheMatrix(x=plainMatrix)
}

# Pass an object that is not a matrix and get an error
testNotAMatrix <- function() {
    message("Test: pass a parameter that is not a matrix. Must return an error")
    newMatrix = makeCacheMatrix(x="This is a text and not a matrix!")
}
