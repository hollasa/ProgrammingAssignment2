## creates a matrix object that can cache its inverse
## Written by Sarah Holland, January 24, 2015




makeCacheMatrix <- function(x = matrix()) {
  
  
  m <- NULL
  set <- function(y) {
    x <<- y
    
    # m is set to null when a new matrix is defined
    m <<- NULL
  }
  
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  
  #list gives object type commands
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinverse()
  if(!is.null(m)) {
    # if m is not a null, then show the cached data
    # m will be a null after the first time that a matrix is defined
    message("Getting cached data")
    
    #and leave the function. Could also use a else if
    return(m)
    
  }
  # tell it to solve, and then grab the inverse
  
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
