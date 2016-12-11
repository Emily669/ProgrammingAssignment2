## Caching the reverse of a matrix  

## makeCacheMatrix： Cache the reverse
## cacheSolve: Compute the reverse if there is no cached data, 
## and read from cache if it's cached.

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y ## read from global environment rather than current environment
    s <<- NULL
  } #set() is included so that once an object of type 
  # makeCacheMatrix() is created, its value can be changed 
  #without initializaing another instance of the object. 
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get, 
       setsolve = setsolve,
       getsolve = getsolve)
  ## gives the name 'set' to the set() function defined above,etc.
} # the result of this is a list, not an atomic vector.


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  s <- x$getsolve() #read from cache
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get() #read from input
  s <- solve(data, ...) # get the inverse matrix
  x$setsolve(s)# load into cache
  s
}
