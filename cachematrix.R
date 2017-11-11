## Creates a object that cashes the inverse of a matrix once it is solved
## After matrix is solved once, it should no longer need to call solve() again.


## Creates matrix object from x and has methods to 1)set matrix, this will reset
## the inverse back to null. 2) Get the inverse, this will return null if it has
## not been calculated yet. 3) Set the inverse, this is used by the casheSolve()
## function once the matrix is solved. 4) Get the matrix, for getting the
## underlying data.
makeCacheMatrix <- function(x = matrix()) {
  matrixInverse <- NULL

  set <- function(mat){
    x <<- mat
    matrixInverse <<- NULL
  }
  getInverse <- function(){
    matrixInverse
  }
  setInverse <- function(mat){
    matrixInverse <<- mat
  }
  get <- function(){
    x
  }
}


## Solves the matrix, unless it has already been solved, it would then return
## the previously solved inverse.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        matInverse <- x$getInverse()

        if(!is.null(matInverse)){
          return(matInverse)
        }

        mat <- x$get()
        matInverse <- solve(mat, ...)
        x$setInverse(matInverse)
        return(mat)
}
