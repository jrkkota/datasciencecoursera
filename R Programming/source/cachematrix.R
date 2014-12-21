# The following two functions work together to calculate the inverse of a invertible 
# square matrix, if a matrix supplied has been cached before, the inverse of a matrix
# is returned from cache else the inverse of a given matrix is computed and returned

# Creates a special "matrix" object that can cache its inverse
#
# Args:
#   x: An invertible square matrix 
#   verbose: Throws an error if it is not an invertible square matrix
#
# Returns:
#   . Sets a value for the given matrix
#   . Gets a value for the given matrix
#   . Sets inverse of a given matrix
#   . Sets inverse of a given matrix

makeCacheMatrix <- function( x = matrix()) {
  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL				
  }
  
  get <- function() x
  setmatrix <- function(solve) m <<- solve
  getmatrix <- function() m
  
  list(set = set, get = get,  setmatrix = setmatrix, getmatrix = getmatrix) 
  
}

# Computes inverse of the special “matrix” returned by makeCacheMatrix function
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache
#
# Args:
#         x: An invertible square matrix 
#   verbose: Throws an error if it is not an invertible square matrix
#
#   Returns:
#       The inverse of a given matrix

cacheSolve <- function(x=matrix(), ...) {
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  matval <- x$get()
  m <- solve(matval, ...)
  x$setmatrix(m)
  m
}
