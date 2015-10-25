## Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.

## The function is designed to create a matrix object, that is able to cache the inverse. Used to speed up the operations that are done on the matrix.

makeCacheMatrix <- function(x = matrix()) {
  # Setting some initial variables
  set_inverse <- NULL
  
  # Defening function Y that will help us with the <<- function
  set_variable <- function(y) {
    x <<- y
    set_inverse <<- NULL
  }
  set_function <- function() x
  setInverse <- function(inverse) set_inverse <<- inverse
  getInverse <- function() set_inverse
  
  # Creating the list that we'll use later on when defining the right cache_solve function.
  list(set_variable = set_variable,
       set_function = set_function,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  set_inverse <- x$getInverse()
  if (!is.null(set_inverse)) {
    ## Return cached value
    return(set_inverse)
  }
  
  # Going to solve for the variable we just set.
  solve_for <- x$get()
  
  # We're now setting the inveser variable by solving the solve_for variable
  set_inverse <- solve(solve_for, ...)
  
  
  x$setInverse(set_inverse)
  
  # Not really required (as the last output value would be posted anyhow)
  set_inverse
}