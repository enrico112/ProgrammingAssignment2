## This function creates and store a matrix, also caching its inverse

makeCacheMatrix <- function(x = matrix()) {
  z <- NULL
  set <- function(y) {
    x <<- y
    z <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) z <<- inverse
  getinverse <- function() z
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function checks whether the inverse of the matrix 
## created by makeCacheMatrix already exist. If it does, it 
## retrieves it, otherwise it calculates it. 

cacheSolve <- function(x, ...) {
  z <- x$getinverse()
  if (!is.null(z)) {
    message("getting cached data")
    return(z)
  }
  mat <- x$get()
  z <- solve(mat, ...)
  x$setinverse(z)
  z
}