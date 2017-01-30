## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Creates Matrix that includes a list of functions to get and set the Matrix, and get and set the inverse of that matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <-function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function
## Using the list created by the function above, this function returns the inverse of a matrix. If the matrix inverse has
## not been calculated previously, function calculates using solve(matrix) and caches the result before returning the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return (inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv
}
