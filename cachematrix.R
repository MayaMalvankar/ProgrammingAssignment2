## This file contains following two functions
## makeCacheMatrix creates matrix that can cache its inverse
## cacheSolve computes inverse of matrix created by makeCacheMatrix either by picking it from cache if  
## has been calculated else calculates it 


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


## cacheSolve return a matrix that is the inverse of 'x' from cache or calcuulates it if there's nothing in cache
#Make sure that the matrix passed is invertible matrix

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

## Tested function execution using following 
## > mat <- matrix(c(1,-1,1,-1,2,1,-1,3,4), 3,3) ## Create a matrix that can produce an inverse
## > chkcache = makeCacheMatrix(mat) 
## > cacheSolve(chkcache) ## First execution does not have cached inverse
## [,1] [,2] [,3]
## [1,]    5    3   -1
## [2,]    7    5   -2
## [3,]   -3   -2    1
## > cacheSolve(chkcache) ## Second execution provides cached inverse 
## getting cached data
## [,1] [,2] [,3]
## [1,]    5    3   -1
## [2,]    7    5   -2
## [3,]   -3   -2    1


