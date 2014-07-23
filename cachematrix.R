#makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
#cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
#
# The puspose of makeCacheMatrix is to store a martix and a cached value of the inverse of the 
# matrix. Contains the following functions:
# ->setM      set value of a matrix
# ->getM      get the value of a matrix
# ->cacheI   set the cached value (inverse)
# ->getI     get the cached value (inverse)


makeCacheMatrix <- function(x = numeric()) {
  
  # holds the cached value
  # initialise the cache with NULL
  cache <- NULL
  
  # store a new  matrix whose inverse is to be calculated
  setM <- function(newValue) {
    x <<- newValue
    # since the matrix is assigned a new value, flush the cache
    cache <<- NULL
  }
  
  # returns the stored matrix
  getM <- function() {
    x
  }
  
  # cache the given argument using solve
  cacheI <- function(solve) {
    cache <<- solve
  }
  
  # get the cached value
  getI <- function() {
    cache
  }
  
  # return a list. Each named element of the list is a function
  list(setM = setM, getM = getM, cacheI = cacheI, getI = getI)
}


# The following function calculates the inverse of a special matrix created with 
# the above function
# purpose:
#if the cached value exist then it is used if not then new value is calculated and stored in the cache
cacheSolve <- function(y, ...) {
  # get the cached value
  inv <- y$getI()
  # if a cached value exists return it
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  # otherwise get the matrix, caclulate the inverse and store it in
  # the cache
  data <- y$getM()
  inv <- solve(data)
  y$cacheI(inv)
  
  # return the inverse
  inv
}
