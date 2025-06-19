## Programming Assignment 2: Caching the Inverse of a Matrix
##
## Author: Ali Aleem
## Course: R Programming - Johns Hopkins University (Coursera)
##
## This script defines two functions:

## 1. makeCacheMatrix():
##    - Creates a special "matrix" object that can cache its inverse.
##    - Returns a list of functions to set/get the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL  # clear the cache
  }
  
  get <- function() x
  
  setinverse <- function(inverse) inv <<- inverse
  
  getinverse <- function() inv
  
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse
  )
}


## 2. cacheSolve():
##    - Computes the inverse of the special matrix returned by makeCacheMatrix().
##    - If the inverse has already been calculated (and the matrix hasn't changed),
##      then it retrieves the inverse from the cache instead of recomputing it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  
  if (!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setinverse(inv)
  
  inv
}


## These functions use R's lexical scoping and the `<<-` operator to maintain a
## cached value across function calls, improving performance for repeated inverse
## calculations on the same matrix.


## How to Use

# Create a matrix
mat <- matrix(c(2, 1, 1, 2), nrow = 2)

# Create special "matrix" object
cmat <- makeCacheMatrix(mat)

# First time: calculates and caches inverse
inv1 <- cacheSolve(cmat)

# Second time: retrieves cached inverse
inv2 <- cacheSolve(cmat)

# You can verify that the inverse is cached
identical(inv1, inv2)  # should be TRUE