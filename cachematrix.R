## Put comments here that give an overall description of what your
## functions do

## This function creates a matrix with caching properties
## basicMatrix = matrix to base the CacheMatrix upon; defaults to empty matrix
makeCacheMatrix <- function(basicMatrix = matrix()) {
    inverseMatrix <- NULL
    
    # sets matrix data
    set <- function(matrix) {
      basicMatrix <<- matrix
      inverseMatrix <<- NULL
    }
    
    # gets matrix data
    get <- function() basicMatrix
    
    # sets the inverse matri x
    setinverse <- function(inverse) inverseMatrix <<- inverse
    
    # returns inverseMatrix
    getinverse <- function() inverseMatrix
    
    # set of functions within scope
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
  
}


## This function calculates the inverse of the matrix x or returns the cached
## value if already calculated.
cacheSolve <- function(enhancedMatrix, ...) {
  # first find out if it has been cached
  inverseMatrix <- enhancedMatrix$getinverse()
  if(!is.null(inverseMatrix)) {
    message("getting cached data")
    return(inverseMatrix)
  }
  
  # so we need a calculation
  # extract the base matrix and take its inverse
  matrixData <- enhancedMatrix$get()
  inverseMatrix <- solve(matrixData,...)
  
  # sets the value into cache
  enhancedMatrix$setinverse(inverseMatrix)
  
  #return the obtained value
  inverseMatrix
}

