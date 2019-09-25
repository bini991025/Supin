# This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  
  # start setting the inverse property
  m <- NULL
  
  # make function to set the matrix
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  
  # make function to get the matrix
  get <- function() x
  
  # make function to set the inverse of the matrix
  setInverse <- function(inverse) m <<- inverse
  
  # make function to get the inverse of the matrix
  getInverse <- function() m
  
  # return the list
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}

# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
# If the inverse has already been calculated (and the matrix has not changed),
# then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  
  ## return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  
  # return the inverse if already set
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  
  # get matrix
  data <- x$get()
  
  # calculate the inverse
  m <- solve(data,...)
  
  # set the inverse of the object
  x$setInverse(m)
  
  # return the matrix
  m
}