## Put comments here that give an overall description of what your
## functions do
# makeCacheMatrix creates matrix that can cache its inverse
# cacheSolve computes inverse of matrix created by makeCacheMatrix either by picking it from cache if  
# has been calculated else calculates it 

## Write a short comment describing this function
#makeCacheMatrix creates a list with functions for
#Setting value of matrix
#Getting value of matrix
#Setting value of inverse of matrix
#Getting value of inverse of matrix

makeCacheMatrix <- function(x = matrix()) {

  invmat <- NULL
  set <- function(y) {
    x <<- y
    invmat <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) invmat <<- inverse
  getinverse <- function() invmat
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}

## Write a short comment describing this function
#Make sure that the matrix passed is invertible matrix
## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  
  
  invmat <- x$getinverse()
  
  #following checks if inverse is already calculated. If yes, it gets value from cache else computes it
  if(!is.null(invmat)) {
    message("getting cached data")
    return(invmat)
  }
  
  datamat <- x$get()
  invmat <- solve(datamat, ...)
  x$setinverse(invmat)
  invmat
}
