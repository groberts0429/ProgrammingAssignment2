## Put comments here that give an overall description of what your
## functions do

## Function makeCacheMatrix returns a matrix-like object which can cache its own inverse.
#  - Create the CacheMatrix object by calling the makeVector accessory function.
#  - Check to see of the InverseMatrix object has been created yet
#    (*)If not, create the invMatrix object by calling cacheSolve

makeCacheMatrix <- function(x = matrix()) {
  #nrow = nrow(x)
  #ncol = ncol(x) # x is an invertible matrix. thus, nrow should equal ncol
  cacheMatrix <- makeVector(x)
  im <- x$getInverse
  if (!is.null(im)) {
    message("Getting the cached inverseMatrix")
    return(im)
  } else {
    inverseMatrix <- cacheSolve(x)
  }
  setinverse <- function(cacheMatrix) cacheSolve(cacheMatrix) #"setter"
  getInverse <- function(cacheMatrix) cacheMatrix #"getter"
  list(set = set, get = get, # create storage for resulting S3 objects
  setInverse = setInverse,
  getInverse = getInverse)
  return(cacheMatrix)
}


## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## the cachesolve should retrieve the inverse from the cache.
## cachseSolve retruns an inverted matrix using the built-in R function solve()

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
        inverseMatrix <- solve(x) #solve returns the inverse of a matrix when given an invertible matrix as input (we assume that the input is an invertible matrix in this assignment)
        return(inverseMatrix)
}
