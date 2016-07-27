## makeCacheMatrix creates an object with matrix x in its environment, and associated functions
## solveMatrix gets the inverse matrix of matrix x from the cache, or when it's not there calculates it.

## Makes an object with setters and getters. Has matrix x in its environment.

makeCacheMatrix <- function(x = matrix()) {
  cacheInverse <- NULL
  set <- function(y) {
    x <<- y
    cacheInverse <<- NULL
  }
  get <- function() x
  set_inverse <- function(inverse_matrix) cacheInverse <<- inverse_matrix
  get_inverse <- function() cacheInverse
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}

# Reads an object created with makeCacheMatrix and returns a matrix that is the inverse matrix of matrix x as put in in makeCacheMatrix. 
# Gets inverse matrix of matrix x from cache if there, otherwise calculates it.

cacheSolve <- function(y, ...) {
  cacheInverse <- y$get_inverse()
  if(!is.null(cacheInverse)) {
    message("getting cached inverse matrix")
    print(cacheInverse)
  }
  if(is.null(cacheInverse)) {
    message("calculating inverse matrix")
    data <- y$get()
    cacheInverse <- solve(data)
    y$set_inverse(cacheInverse)
    print(cacheInverse)
  }
}
