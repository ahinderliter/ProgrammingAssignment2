## makeCacheMatrix makes an object that has a matrix, and possibly its inverse.
## cacheSolve takes an object created by makeCacheMatrix and returns its inverse.

## makeCacheMatrix accepts a matrix, and returns an object which has room for a cached inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <<- NULL;
  
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  get <- function() x
  setinv <- function(invmat) i <<- invmat
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve accepts an object created by makeCacheMatrix. If the cached inverse already exists, it returns that.
## Otherwise it computes the inverse, caches it, and then returns it.

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if (! is.null(m)) {
    return(m)
  }
  data <- x$get()
  x$setinv(solve(data))
  x$getinv()
}
