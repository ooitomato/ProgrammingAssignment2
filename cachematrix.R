## Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below are a pair of functions that are used to create a special object that stores a matrix and caches its inverse
## This function creates a special "matrix" object that can cache its inverse.
##
## Sample on how to call the functions below:
## First use 
##     test_run <- makeCacheMatrix(matrix(1:4, 2, 2)) to create a matrix
## Then
##     test_run$get() to look at what is created successfully
## Then 
##     test_run$get_inverse() to check whether it is in the cache or already being solved before
## Then 
##     cacheSolve(test_run) to inverse the matrix
## Finally run
##     cacheSolve(test_run) again to prove that it will get from cache data when it is already cache from previous run
## and final check with test_run$get_inverse to confirm the answer.

makeCacheMatrix <- function(x = matrix()) {
        inv_matrix <- NULL
        set <- function(y) {
          x <<- y
          inv_matrix <<- NULL
        }
        get <- function() x
        set_inverse <- function(inv) inv_matrix <<- inv
        get_inverse <- function() inv_matrix
        list(set=set, get=get,set_inverse =set_inverse, get_inverse=get_inverse)
          
}


## This function computes the inverse of the special "matrix" created by makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv_matrix <- x$get_inverse()
  if(!is.null(inv_matrix)) {
    message("getting cached data")
    return(inv_matrix)
  }
  matr <- x$get()
  inv_matrix <- solve(matr,...)
  x$set_inverse(inv_matrix)
  inv_matrix
}
