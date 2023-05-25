## The makeCacheMatrix function creates a special "matrix" object that can cache its 
## inverse, while cacheSolve computes the inverse of that special "matrix".

## MakeCacheMatrix initializes x (the function argument) and m (used within the 
## function environment).  After initializing the objects, setters and getters 
## are defined for x and m.  List is used to send the functions to the parent 
## environment.

makeCacheMatrix <- function(x = matrix()) {
 # This function creates a special "matrix" object that can cache its inverse.
  
  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  
  #adapted code from makecachevector 
  setinverse <- function(inverse) m <<- inverse
  
  getinverse <- function() m
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

  # The cacheSolve function computes the inverse of the special "matrix" returned by 
  # makeCacheMatrix above. If the inverse has already been calculated (and the 
  # matrix has not changed), then cacheSolve will retrieve the inverse from 
  # the cache.

cacheSolve <- function(x, ...) {
        
  if(!is.null(m)) {
    message("getting cached matrix")
    return(m)
  }
  
  ## Return a matrix that is the inverse of 'x'
  # this only executes if the "if" part is false
  
  data <- x$get()
  
  m <- solve(data, ...)
  
  x$setinverse(m)
  m
}
