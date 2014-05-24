## Put comments here that give an overall description of what your
## functions do
# makeCacheMatrix and cacheSolve work together to create a matrix (makeCacheMatrix), solve for the inverse of that matrix that is then stored in memory (cacheSolve)
# and then provide convenient getter and setter helper methods on the matrix object

## Write a short comment describing this function
# makeCacheMatrix takes a matrix as an argument and adds typical OOP (object oriented programming) getter and setter methods on the matrix object
# it additionally creates getter and setter methods for the matrix inverse that can be cached


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
# cacheSolve takes a matrix object (e.g. the object created by makeCacheMatrix) as an argument, returns the inverse of that matrix, 
# and stores the inverse in memory so that it does not need to be recomputed every time it is needed

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
