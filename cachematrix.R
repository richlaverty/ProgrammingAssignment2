## These two functions work together to create matix-like objects and invert
## them.

## makeCacheMatrix creates a matrix-like object that can be inverted by
## the cacheSolve function.  The idea is that once one of these 
## matrix-like objects has been inverted, its inverse is cached so that
## subsequent calls to cacheSolve won't invert the matrix-like object,
## but recall the cached inverse

makeCacheMatrix <- function(x = matrix())
{
  m <- NULL
  set <- function(y)
  {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function inverts a matrix-like object, created by the makeCacheMatrix
## function.  If the matrix-like object is new, its inverse is calculated
## then stored.  If the matrix-like object has previously been inverted then
## its inverse is called from the cache, rather than computed.

cacheSolve <- function(x, ...)
{
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m))
  {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
