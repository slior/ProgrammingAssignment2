## The following two functions follow the same pattern as given in the assignment instructions.
## makeCacheMatrix serves as a 'constructor' function that constructs a new cached matrix object.
## cacheSolve runs base.solve for a given cached matrix created by 'makeCacheMatrix'. 
# If solve was previously run on that object, the cached result is retrieved. Otherwise the value is computed and cached


## makeCacheMatrix
# Create and returns a new cached matrix object.
# Input: x a simple numeric (or complex) square matrix, as expected by base.solve.
# Output: a new object supporting 4 functions:
#     get: retrieves the matrix object
#     set: sets a new matrix object. Will also clear the cache
#     getInv: retrieves the cached inverse matrix, if available; NULL otherwise
#     setInv: sets the inverted matrix, as cached value.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  get <- function() { x }
  
  set <- function (m2)
  {
    x <<- m2
    inv <<- NULL
  }
  
  getInv <- function() { inv }
  
  setInv <- function (i) { inv <<- i }
  
  list(set = set, get = get, getInv = getInv, setInv = setInv)

}


## 
# Run base.solve on the given cached matrix.
# If a cached value is available, no computation is done and the cached value is returned.
# If no cached inverted matrix is available, compute the inverse matrix (call solve), cache the result and return it.
# Diagnostic messages will also be printed in each case - whether the cache was used or not.
# 
# Input: x - a cached matrix value, as created makeCacheMatrix.
#   Assumption: the underlying matrix of x is assumed to be invertible.
# Output: The inverse matrix, of the underlying matrix in x.

cacheSolve <- function(x, ...) 
{
  i <- x$getInv()
  
  if (is.null(i))
  {
    message("No cache, computing inverse...")
    i <- solve(x$get(),...)
    x$setInv(i)
  }
  else message("Retrieving cached inverse matrix")
  
  return (i)
}
