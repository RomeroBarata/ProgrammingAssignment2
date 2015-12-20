# makeCacheMatrix: It receives a matrix as an argument and returns a list containing 
#                  the functions to get and set a matrix and also to get and set its
#                  inverse.
# cacheSolve: It receives the list argument returned by makeCacheMatrix and returns
#             the inverse of the matrix contained in the given list. If the inverse
#             has already been computed, it returns the cached value, if not it 
#             computes it.

makeCacheMatrix <- function(x = matrix()) {
  x_inv <- NULL
  set <- function(y){
    x <<- y
    x_inv <<- NULL
  }
  get <- function() x
  setinv <- function(inv) x_inv <<- inv
  getinv <- function() x_inv
  list(set = set, get = get, 
       setinv = setinv, 
       getinv = getinv)

}

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  x_inv <- x$getinv()
  if(!is.null(x_inv)){
    message("getting cached data")
    return(x_inv)
  }
  data <- x$get()
  x_inv <- solve(data, ...)
  x$setinv(x_inv)
  x_inv
}
