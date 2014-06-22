# Function Description - makeCacheMatrix
# * Creates a special "matrix" and returns the following functions 
# * set - to set or replace the existing matrix with a new one
# * get - to get the existing matrix 
# * setInverse - to set the inverse of the matrix
# * getInverse - to retrieve the inverse of the matrix (default NULL)
#
# Args:
#   myMatrix: accepts a matrix as argument when called but defaults to an empty matrix if none is provided
#
# Returns:
#   List containing functions for set, get, setInverse and getInverse

makeCacheMatrix <- function(myMatrix = matrix()) {
  
  # Initialize the inverse variable (default value NULL)
  inverseMatrix <- NULL
  
  # Set matrix to 'myMatrix' variable
  set <- function( matrix ) {
    myMatrix <<- matrix
    inverseMatrix <<- NULL # sets 'inverseMatrix' to NULL (removes the "cache")
  }
  
  # Gets the matrix when called
  get <- function() {
    myMatrix
  }
  
  # Sets the inverse of the matrix
  setInverse <- function(inverse) {
    inverseMatrix <<- inverse
  }
  
  # Gets the inverse of the matrix 
  getInverse <- function() {
    inverseMatrix
  }
  
  # Returns a list containing function for set, get, setInverse and getInverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


# Function Description - cacheSolve
# * The following function will return an inverse of a special matrix
# * The special matrix can be made using 'makeCacheMatrix' and should be passed to this function as argument
# *     The function assumes argument x is the special matrix
#
# Args:
#   x: needs a valid special matrix with set, get, setInverse and getInverse functions
#
# Returns:
#   The inverse of the matrix

cacheSolve <- function(x, ...) {
  
  # Calls getInverse function from the special matrix and sets it to variable 'inverseMatrix'
  inverseMatrix <- x$getInverse()
  
  # If variable 'inverseMatrix' is not null return the inverse and message that it was retrieved from "cache"
  if( !is.null(inverseMatrix) ) {
    message("getting cached data")
    return(inverseMatrix)
  }
  
  # Calls get function from the special matrix and sets the matrix to the variable 'myMatrix'
  myMatrix <- x$get()
  
  # Calculates the inverse and sets it to variable 'inverseMatrix'
  inverseMatrix <- solve(myMatrix)
  
  # Set the inverse of the special matrix using its setInverse function
  x$setInverse(inverseMatrix)
  
  # Return the inverse of the matrix
  inverseMatrix
}

# Sample output:
# > source("cacheMatrix.R")
# > myFunctionList <- makeCacheMatrix(matrix(c(1,1,1,3,4,3,3,3,4), nrow=3, ncol=3))
# > myFunctionList[["get"]]()
#      [,1] [,2] [,3]
# [1,]    1    3    3
# [2,]    1    4    3
# [3,]    1    3    4
# > cacheSolve(myFunctionList)
#      [,1] [,2] [,3]
# [1,]    7   -3   -3
# [2,]   -1    1    0
# [3,]   -1    0    1
# > cacheSolve(myFunctionList)
# getting cached data
#      [,1] [,2] [,3]
# [1,]    7   -3   -3
# [2,]   -1    1    0
# [3,]   -1    0    1
# > 