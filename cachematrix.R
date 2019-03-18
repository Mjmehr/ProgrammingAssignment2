
##first: creates a special “matrix” object that can cache its inverse.

## This function  is a special “matrix” object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  
  set <- function(y) {
    
    x <<- y
    
    inv <<- NULL
    
  }
  
  get <- function() x
  
  ## Return the matrix
  
  setInverse <- function(inverse) inv <<- inverse
  
  getInverse <- function() inv
  
  list(set = set,
       
       get = get,
       
       setInverse = setInverse,
       
       getInverse = getInverse)
  
}



## second: computes the inverse of the “matrix” returned by makeCacheMatrix().

## This function measures the inverse of the unique "matrix" returned.

cacheSolve <- function(x, ...) {
  
  ## This is the inverse of 'x'
  
  In <- x$getInverse()
  
  if(!is.null(In)){
    
    message("getting cached data")
    
    return(In)
    
  }
  
  data <- x$get()
  
  In <- solve(data)
  
  x$setInverse(In)
  
  In     
  
