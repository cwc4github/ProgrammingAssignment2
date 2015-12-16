## 1. makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## 2. cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
##    above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve 
##    should retrieve the inverse from the cache.

## makeCacheMatrix: input a matrix; output a list, which is a special "matrix" object

makeCacheMatrix <- function(x = matrix()) {
  m_inv <- NULL
  set <- function(y) {
    x <<- y
    m_inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inversed) m_inv <<- inversed
  getinverse <- function() m_inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve: input a list from above; ouput the inversed matrix inputed in the above function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m_inv <- x$getinverse()
  if(!is.null(m_inv)) {
    message("getting cached data")
    return(m_inv)
  }
  data <- x$get()
  m_inv <- solve(data, ...)
  x$setinverse(m_inv)
  m_inv
}
