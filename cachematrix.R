## Function creates matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      set_inverse <- function(inverse) m <<- inverse
      get_inverse <- function() m
      list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
}


## Function computes the inverse of matrix returned by makeCacheMatrix
## If inverse has already been calculated, function retrieves the prior result

cacheSolve <- function(x, ...) {
      m <- x$get_inverse()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$set_inverse(m)
      m
}
