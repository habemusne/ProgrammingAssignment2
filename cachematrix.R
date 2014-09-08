## These two functions caches the Inverse of a Matrix



## This function represents a matrix which stores its inverese
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL # This is the inverse matric

  # Set the ordinary matrix and clear the inverse matrix
  setMatrix <- function(y) {
    x <<- y
    inverse <<- NULL
  }

  # Get the ordinary matrix
  getMatrix <- function() {
    x
  }

  # Set the inverse matrix
  setInverseMatrix <- function(inverseMatrix) {
    inverse <<- inverseMatrix
  }

  # Get the inverse matrix
  getInverseMatrix <- function() {
    inverse
  }

  list(setMatrix = setMatrix, getMatrix = getMatrix, 
       setInverseMatrix = setInverseMatrix, 
       getInverseMatrix = getInverseMatrix)
}


## This function returns the makeCacheMatrix's inverse matrix
cacheSolve <- function(x, ...) {
  inverse = x$getInverseMatrix()
  if (!is.null(inverse)) {
    message("Getting cached data")
    return(inverse)
  }

  matrix <- x$getMatrix()
  inverse <- solve(matrix)
  x$setInverseMatrix(inverse)
  inverse
}
