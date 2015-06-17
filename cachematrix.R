## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(mat = matrix()) {
  # function that contains 4 functions inside which simply replace the old matrix/inverse with new ones 
  # and display them
  inv <- NULL # sets default inverse value to NULL
  
  # function 1. reset the matrix stored
  setMatrix = function(a){
    mat <<- a # reset the old matrix x stored in the makeCacheMatrix() with a new matrix y
    inv <<- NULL # reset the inverse inv of old matrix. The new inverse will be calculated in cacheSolve
  }
  
  # function 2. return the new matrix
  getMatrix = function(){mat} # returns the matrix x that overide old matrix
  
  # function 3. reset inverse of a matrix inv with a new value b
  setInverse <- function(b){inv <<- b}
  
  # function 4. return the new inverse
  getInverse <- function(){inv}
  
  # create a list of functions
  list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(a, ...) {
  ## Return a matrix that is the inverse of 'x'
  # assign inverse value if a's inverse has already been calculated
  inv <- a$getInverse()
  
  # check to see if it indeed exists, then return the inverse value
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  
  # otherwise, pull matrix data
  mat <- a$getMatrix()
  
  # calculate inverse here
  inv <- solve(mat,...)
  
  # reset the inverse value
  a$setInverse(inv)
  
  # and display the result
  inv
}
