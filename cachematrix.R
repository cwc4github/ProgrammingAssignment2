## Programming Assignment 2 - R Programming
##
##
## The two functions shown here help in caching the inverse of a matrix.  
## Matrix inversion is usually very computationally intensive - especially for large size matrices.
## To avoid recomputing the inverse and generating the same result repeatedly, this two functions 
## can simply compute the result once.  If you try to recompute the inverse again, 
## they just return the pre-computed result. 
## 
##
## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## input: a matrix
## output: a list, which is a special "matrix" object
##
## Usage example: 
## x <- matrix(1:4, 2, 2) 
## m <- makeCacheMatrix(x) 

makeCacheMatrix <- function(x = matrix()) {
  # Following the same format as the assignment example
  # makeCacheMatrix creates a list containing a function to 
  # 1. set: set the value of the matrix 
  # 2. get: get the value of the matrix 
  # 3. setinverse: set the value of inverse of the matrix 
  # 4. getinverse: get the value of inverse of the matrix
  
  # Initially set to NULL when the matrix is set
  m_inv <- NULL
  
  # set function 
  # Set the matrix itself but not the inverse 
  set <- function(y) {
    x <<- y
    m_inv <<- NULL
  }
  
  # get function 
  # Get the matrix itself but not the inverse 
  get <- function() x
  
  # setinverse function
  # Set the inverse matrix
  setinverse <- function(inversed) m_inv <<- inversed
  
  # getinverse function
  # Get the inverse matrix
  getinverse <- function() m_inv
  
  # Generate the ouput list
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix 
## above. If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.
## input: a list from above
## ouput: the inversed matrix inputed in the above function
##
## Noted: This function assumes that the matrix is always invertible.
##
## Usage example: 
## x <- matrix(1:4, 2, 2) 
## m <- makeCacheMatrix(x) 
## s <- cacheSolve(m) 
## print(s) 
## s should return: 
##     [,1] [,2] 
##[1,]   -2  1.5 
##[2,]    1 -0.5 
## 
## s2 <- cacheSolve(m) 
## This should display a "Getting cached matrix" message 
## print(s2) 
## s2 should return 
##     [,1] [,2] 
##[1,]   -2  1.5 
##[2,]    1 -0.5 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  # Get the current state of the inverse and check whether it has been computed 
  m_inv <- x$getinverse()
  
  # If it has been computed
  if(!is.null(m_inv)) {
    # Simply return the computed inverse with a message
    message("getting cached data")
    return(m_inv)
  }
  
  # If not
  # Get the matrix
  data <- x$get()
  
  # Solve its inverse
  m_inv <- solve(data, ...)
  
  # Cache the inverse in the special "matrix" object
  x$setinverse(m_inv)
  
  # Return the inverse matrix
  m_inv
}
