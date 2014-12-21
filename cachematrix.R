## makeCacheMatrix returns a list of functions that: 
## Set, get the values of a matrix 
## Set, get the values of the inverse matrix

makeCacheMatrix <- function(x = matrix()) { 
  
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
  
}


## cacheSolve matrix checks if the inverse matrix has already 
## computed. If so its returns directly the value otherwise
## the inverse is computed.
## functions do

cacheSolve <- function(x, ...) {
  
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
  
}
