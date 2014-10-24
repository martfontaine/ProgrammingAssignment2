## This function creates a special "matrix" object 
## that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  ## initialize the atribute and functions for this matrix to 
  ## store, get and set the inverse and set and get the matrix
  i <- NULL
  set <- function(y){
    x <<-y
    i <<-NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  
  ## set the value of the matrix x to the matrix 
  ## given as an argument
  set(x)
  
  ## list the functin calls defined above
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## try to get the inverse from the cache  
  i <- x$getinverse()
  
  ## if the inverse was stored in cache return it
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  ## since the inverse was not stored in cache calculate it
  ## first get the matrix and then use the function solve to 
  ## calculate the inverse
  data <- x$get()
  i <- solve(data)
  
  ## before returning it stor it on the special matrix in the cache
  x$setinverse(i)
  
  ## Return a matrix that is the inverse of 'x'
  return(i)
}
