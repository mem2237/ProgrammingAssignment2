## Put comments here that give an overall description of what your
## functions do

## Creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Checks to see if the inverse of a matrix has already been calculated and, if so, returns its value and, if not, calculates it.
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("---getting cached data---")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}

## Checking code to see if it is working properly.
matrix_number<-matrix(1, 2, 3, 4, 2, 1, 5, 4, 2)
matrix_structured<-matrix(matrix_number,3,3)
test_matrix <- makeCacheMatrix(matrix_structured)
test_matrix$get()
test_matrix$getInverse()
## Checking to see if cached results are returned
cacheSolve(test_matrix)
cacheSolve(test_matrix)
## Checked results for accuracy by comparing them to https://www.easycalculation.com/matrix/matrix-inverse.php.

