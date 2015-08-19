## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  ## input statement checking that x is a matrix
  
  ## first the inverse matrix is set to NULL
  mat_inv <- NULL
  ## The function to change the matrix
  set <- function(y) {
    x <<- y
    mat_inv <<- NULL
  }
  ## The function to return the matrix
  get <- function() x
  ## The function to set the inverse matrix
  setinverse <- function(inversed_mat) mat_inv <<- inversed_mat
  ## The function to return the inverse matrix
  getinverse <- function() mat_inv
  ## A list of the 4 functions are returned
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function returns an inversed matrix of the special "matrix" returned 
## by makeCacheMatrix. If the inverse matrix has already been calculated
##  (and the matrix has not changed), then the program does not recompute
## the matrix, but retrieves it from the cache.

cacheSolve <- function(x, ...) {
  ## retrieve the inversed matrix from makeCacheMatrix     
  mat_inv <- x$getinverse()
  ## If it is not NULL, return the inversed mtrix, and a message
  if(!is.null(mat_inv)) {
    message("getting cached data")
    return(mat_inv)
  }
  ## Otherwise the original matrix is retrived and, the inverse mtrix is calculated
  data <- x$get()
  mat_inv <- solve(data, ...)
  ## the inverse matrix is passed to it "place" in the special "Matrix"
  x$setinverse(mat_inv)
  ## The inverse matrix is returned
  mat_inv  
}
