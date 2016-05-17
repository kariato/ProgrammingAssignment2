## Author Mark Davey, Date: 2016-05-17
## Coursera R Programming Course Week3 assignment
## Two functions to create an inverse cache of a matrix in R
## 1. makeCacheMatrix is a list of functions to maintain the cache
## 2. cacheSolve to calculate the cached solution

## Matrix inversion is usually a costly computation and there may be some benefit to caching the 
## inverse of a matrix rather than computing it repeatedly

makeCacheMatrix <- function(x = matrix())  {
  ##initialze cache
  specialMatrixInv <- NULL
  ##setter (set item)
  set <- function(y) {
    x <<- y
    specialMatrixInv <<- NULL
  }
  ##getter (get the value from the stored data )
  get <- function() x
  ## calcuate the inverse matrix
  setinverse <- function(solve) specialMatrixInv <<- solve
  ##  return the value
  getinverse <- function() specialMatrixInv
  ## return the functions as a list 
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  specialMatrixInv <- x$getinverse()
  ##check if a cached version exist if it does return it
  if(!is.null(specialMatrixInv)) {
    message("getting cached data")
    return(specialMatrixInv)
  }
  ##since no cache caclulate it, store it and return it
  specialMatrixInv <- solve(x$get(), ...)
  x$setinverse(specialMatrixInv)
  specialMatrixInv
}

##Test Case
##To test this function perform the following steps: 
##> test1 <-replicate(10, rnorm(10)) 
##> t1<-makeCacheMatrix(test1)
##> cacheSolve(t1)
##> cacheSolve(t1)  ##this time it should retrieve cache
