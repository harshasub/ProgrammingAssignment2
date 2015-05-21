## R Programming Assignment 2: Lexical Scoping--caching the inverse of a matrix

## The makeCacheMatrix function creates a special "matrix",
## which is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
  ## create a matrix object x and some associated sub-functions/methods
  
  ## define the cache h
  h <- NULL
  set <- function(y) {
    x <<- y 
    h <<- NULL 
  }
  get <- function() x 
  setinverse <- function(inverse) h <<- inverse ## set the cache h equal
  ## to the inverse of the matrix x
  getinverse <- function() h ## return the cached inverse of x
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the matrix returned by makeCacheMatrix
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  h <- x$getinverse()
  if(!is.null(h)) {
    message("getting cached data")
    return(h)
  }
  data <- x$get()
  h <- solve(data, ...)
  x$setinverse(h)
  h
}