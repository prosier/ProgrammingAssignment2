
## Functions that calculate/cache the inverse of a matrix
##
## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## cacheSolve computes the inverse of the special "matrix". If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

## Example:
##
## > source('cachematrix.R')
## > m <- makeCacheMatrix(matrix(1:4, 2, 2))
## > m$get()
##     [,1] [,2]
## [1,]  1    3
## [2,]  2    4
## 
## > cacheSolve(m)
##     [,1] [,2]
## [1,] -2  1.5
## [2,]  1 -0.5


## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  # cache initialised to NULL
  inverse_x <- NULL
  # setter function of the matrix
  set <- function(y) {
    x <<- y
    inverse_x <<- NULL
  }
  # getter function of the matrix
  get <- function() x
  # setter function of the inverse matrix
  setinverse<- function(inverse) inverse_x <<- inverse
  # getter function of the inverse matrix
  getinverse <- function() inverse_x
  # return the created functions
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the         
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  # see if the matrix is in cache
  inverse_x <- x$getinverse()
  # if in cache, return the inverted matrix from cache and display message.
  if (! is.null(inverse_x)) {
    message("getting cached inverse matrix")
    return(inverse_x)
  }
  # if not in cache, calculate the inverse using the solve function.
  inverse_x <- solve(x$get())
  # put the calculated inverse matrix in the cache.
  x$setinverse(inverse_x)
  # return the calculated inverse matrix.
  return(inverse_x)
}
