## Programming Assignmend 2
## 
## Since the calculation of the inverse of a matrix is a time consuming step, 
## the inverse of a matrix shall be cached in the memory until the input matrix changes.
## Hence the inverese of the matrix can be used in other operations until the input changes without being recalculated every time it is used.

## makeCacheMatrix creates a matrix object that can cache the inverse of the matrix argument.
## it contains setters and getters that either retrieve the cached data or process the inputs if they are changed or nothing is stored.
## subsequent code can access the values of x and inverse through the use of getters and setters.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  # cache the input matrix
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  # call the input matrix
  get <- function() x
  # save the inverse to the cache
  setinverse <- function(inv) inverse <<- inv
  # call the cached inverse
  getinverse <- function() inverse
  # create a list of functions
  list(set = set, 
       get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)

}

## cacheSolve is able to calculate and store the inverse for the input argument of type makeCacheMatrix
## because the list elements in makeCacheMatrix we can access these functions using $

cacheSolve <- function(x, ...) {
  # read the inverse from cache
  inverse <- x$getinverse()
  # if the inverse is not NULL return value from cache
  if(!is.null(inverse)){
    message("getting cached data")
    return(inverse)
  }
  # else load input matrix
  dat <- x$get()
  # and calculate the inverse
  inverse <- solve(dat,...)
  # save the inverse to cache
  x$setinverse(inverse)
  # return the inverse
  inverse
}
