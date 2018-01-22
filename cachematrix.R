## These two functions work together to enable caching of matrix inversion. 
## makeCacheMatrix has two matrix objects, "x" and "i", and four functions that operate on these objects.
## When setting a new value for "x", "i" is set to null. This resets the cache.
## cacheSolve only accepts a list object returned from makeCacheMatrix. 
## cacheSolve interacts with the matrx and inverse via the four methods in the makeCacheMatrix list.
## If the object passed to cacheSolve already has an inverse, it will be returned from the cache.
## Otherwise a new inverse is created via the solve method and saved to the cache.

## Returns a list of four functions that operate on a matrix and it's inverse
## Function 1: "set", sets the matrix to equal the argument and sets the inverse to null
## Function 2: "get", gets the matrix 
## Function 3: "setinverse", sets the inverse to equal the argument
## Function 4: "getinverse", gets the inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) i <<- inv 
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Returns a matrix that is the inverse of 'x'
## 'x' must be the output of a makeCacheMatrix function call 
## If 'x' has already cached an inverse return the cached version
## Otherwise apply the solve function to the matrix returned by x$get and return the resulting inverse

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
