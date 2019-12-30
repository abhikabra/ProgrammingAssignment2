## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve 
##the inverse from the cache.

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  # 1. initialize the cache Matrix 'cacheMatrix' by NULL Value
  
  cacheMatrix <- NULL
  
  # 2. define the method named 'setMatrix'

  setMatrix <- function(y) {
    x <<- y
    cacheMatrix <<- NULL
  }
  
  # 3. define the method named 'getMatrix'
  # return the matrix 'x'
  
  getMatrix <- function() x
  
  # 4. define the method named 'setCache'
  
  setCache <- function(inverse) cacheMatrix <<- inverse
  
  # 5. define the method named 'getCache'
  # that will return the cached inverse of 'x'
  
  getCache <- function() cacheMatrix
  
  # 6. list the names of all methods that will be known to the outside world  
  list(setMatrix = setMatrix,
       getMatrix = getMatrix,
       setCache = setCache,
       getCache = getCache)
  
  #---------------------------------------------------------
}



cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        # 1. check the content of cache matrix

  cacheMatrix <- x$getCache()
  
  # 2. if the content is not null then: return the result 
  
  if (!is.null(cacheMatrix)) {
    message("loading cache matrix...")
    return(cacheMatrix)
  }
  
  # 3. if the content is empty then: 
  # get the matrix, create, set, update and return the cache matrix
  else {
    dMatrix <- x$getMatrix()
    cacheMatrix <- solve(dMatrix, ...)
    x$setCache(cacheMatrix)
    return(cacheMatrix)
  }
  #---------------------------------------------------------
}
