## Put comments here that give an overall description of what your
## functions do
# These functions below provide a way of 'caching' the inverse of a matrix.
# It holds the value in memory without it being available in the global environment.



# Write a short comment describing this function
# This function allows creation of a list variable that serves as a way to
# get and set the matrix and get and set the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  inverse_matrix <- NULL
  
  # Getter to get cached matrix
  getMatrix <- function() {
    return(x)
  }
  
  # Setter to set matrix
  setMatrix <- function(y) {
    x <<- y
    inverse_matrix <<- NULL
  }
  
  setInverse <- function(mat) {
    m <<- mat
  }
  
  getInverse <- function() {
    return(m)
  }
  
  return (list(setInverse = setInverse, getInverse = getInverse,
       setMatrix = setMatrix,
       getMatrix = getMatrix))
}


## Write a short comment describing this function
# This function takes as input the list created by the first and checks if
# the variable in the list for holding the solved matrix has been set. If so, it
# returns that variable. If not, it solves the matrix,sets the variable, and returns it.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  # Check if inverse is cached
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  # If it is not cached
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  return(m)
}
