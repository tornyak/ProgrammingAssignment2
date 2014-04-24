## makeCacheMatrix is a wrapper function around a matrix x
## in its scope it contains variables:
## x - argument of the function storing a matrix, it is assuemd
##     that x is invertible
## inv - inverse of the matrix x
##
## it defines four functions inside its scope:
## set - sets value of matrix "x" and its inverse "inv" ( inv is initally NULL)
## get - returns a matrix "x"
## getInverse - returns value previously stored into "inv" by setInverse()
## setInverse - sets "inv" variable. It is supposed to contain inverse of matrix "x"
##
## returns a list containing "references" to the internal functions

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Returns inverse of matrix wrapped by the list "x" given as a function argument
## it will first try to fetch already calculated inverse if it exists. If not
## it will use function solve to calculate inverse and store its value into "x"
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  matrix <- x$get()
  inv <- solve(matrix)
  x$setInverse(inv)
  inv
}