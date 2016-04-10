## this function creates a list that contains 4 member functions: set, get,
## setInverse and getInverse. It uses the <<- operator so that these variables
## are not exposed to the outside environment.
##
makeCacheMatrix <- function(x = matrix()) {
          xinv <- NULL  ## store the results of the inverse matrix
##          
##  function sets the value of the vector, gets the value of the vector
##  also sets the value of the inverse of the vector and gets the value of
##  the inverse of the vector
##          
          set <- function(y) {
                  x <<- y
                  xinv <<- NULL
          }
##
##  return the input matrix
##
          get <- function() x
##
##  sets and returns the inverse of the matrix
##          
          setInverse <- function(inverse) xinv <<- inverse
          getInverse <- function() xinv
##
##  create a list that contains these functions  
##
          list(set = set,
              get = get,
              setInverse = setInverse,
              getInverse = getInverse)
}
##
##
## Return a matrix that is the inverse of 'x'
##

cacheSolve <- function(x, ...) {
          xinv <- x$getInverse()
##
##  if the inverse is there display "getting cached data"
##
          if(!is.null(xinv)) {
                message("getting cached data")
                return(xinv)
          }
##
##  if matrix is not there do x$get to get matrix object,
##  solve it, set and return result
##
          mat <- x$get()
          xinv <- solve(mat, ...)
          x$setInverse(xinv)
          xinv
}