## Put comments here that give an overall description of what your
## functions do
## Creates a object that cashes the inverse of a matrix once it is solved
## After matrix is solved once, it should no longer need to call solve() again.

## Write a short comment describing this function

## Creates matrix object from x and has methods to 1)set matrix, this will reset
## the inverse back to null. 2) Get the inverse, this will return null if it has
## not been calculated yet. 3) Set the inverse, this is used by the casheSolve()
## function once the matrix is solved. 4) Get the matrix, for getting the
## underlying data.
makeCacheMatrix <- function(x = matrix()) {
  ##Sets Inverse to null, is changed at first calculation
  matrixInverse <- NULL

  ##Changes the vector, also resets inverse to null
  set <- function(mat){
    x <<- mat
    matrixInverse <<- NULL
  }
  ##retrieves inverse, or null if it hasn't been calculated.
  getInverse <- function(){
    matrixInverse
  }
  ##Sets the inverse once it has been calculated.
  setInverse <- function(mat){
    matrixInverse <<- mat
  }
  ##returns vector
  get <- function(){
    x
  }
  ##Actual list to return
  list(set = set, getInverse = getInverse, setInverse = setInverse, get = get)
}


## Solves the matrix, unless it has already been solved, it would then return
## the previously solved inverse.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  matInverse <- x$getInverse()

  ##if the calculation has already been done, return inverse
  if(!is.null(matInverse)){
    return(matInverse)
  }
  ##Otherwise calculate it.
  mat <- x$get()
  matInverse <- solve(mat, ...)
  x$setInverse(matInverse)
  return(matInverse)
}
