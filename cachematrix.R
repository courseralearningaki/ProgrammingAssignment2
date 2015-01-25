## Overall description
## There are two functions.
##      The first one, makeCacheMatrix is to create and update a matrix,
##      and to provide set/get/setsolve/getsolve functions inside.
##      The second one, cacheSolve is to calculate inverted matrix.


## Test procedure:
##      1: create a matrix
##      x <- matrix(data = 1:4, nrow = 2, ncol = 2)
##      2: create a matrix under makeCacheMatrix
##      y <- makeCacheMatrix(x)
##      3: see the cached matrix
##      y$get()
##      4: get inverted matrix
##      cacheSolve(y)
##      5: get cached inverted value
##      cacheSolve(y)
##      6: create another matrix
##      z <- matrix(data = 4:7, nrow = 2, ncol = 2)
##      7: set matrix
##      y$set(z)
##      8: get inverted matrix
##      cacheSolve(y)

## makeCacheMatrix: This function creates a special "matrix" object
##      that can cache its inverse.

makeCacheMatrix <- function(matrixObj = matrix()) {
        ## cache of inverted matrix
        inversedMatrixObj <- NULL
        
        ## set method
        set <- function(inObj) {
                matrixObj <<- inObj
                inversedMatrixObj <<- NULL
        }
        
        ## get method
        get <- function(){
                matrixObj
        }
        
        ## set the value of solve(matrix)
        setsolve <- function(solve){
                inversedMatrixObj <<- solve      
        }
        
        ## get the value of inversedMatrix
        getsolve <- function() inversedMatrixObj
        
        ## provide reference to internal methods
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## cacheSolve: This function computes the inverse of the special "matrix"
##      returned by makeCacheMatrix above. If the inverse has already been
##      calculated (and the matrix has not changed), then cacheSolve should
##      retrieve the inverse from the cache.

cacheSolve <- function(matrixObj) {
        ## Return a matrix that is the inverse of 'matrixObj'
        inversedMatrixObj <- matrixObj$getsolve()
        
        if(!is.null(inversedMatrixObj)) {
                message("getting cached data")
                return(inversedMatrixObj)
        }
        
        ## continue processing for is.null(inversedMatrixObj)
        data <- matrixObj$get()
        inversedMatrixObj <- solve(data)
        matrixObj$setsolve(inversedMatrixObj)
        inversedMatrixObj        
}